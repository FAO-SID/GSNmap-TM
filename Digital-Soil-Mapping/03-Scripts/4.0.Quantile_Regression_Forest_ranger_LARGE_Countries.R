#_______________________________________________________________________________
#
# Quantile Regression Forest
# Soil Property Mapping
#
# GSP-Secretariat
# Contributors: Isabel Luotto (GSP-FAO)
#               Marcos E. Angelini (GSP-FAO)
#               Luis Rodríguez Lado (GSP-FAO)
#               Stephen Roecker (NRCS-USDA)
#_______________________________________________________________________________

#Empty environment and cache 
rm(list = ls())
gc()

# Content of this script =======================================================
# 0 - Set working directory, soil attribute, and packages
# 1 - Merge soil data with environmental covariates 
# 2 - Covariate selection
# 3 - Model calibration
# 4 - Uncertainty assessment
# 5 - Prediction
# 6 - Export final maps
#_______________________________________________________________________________


# 0 - Set working directory, soil attribute, and packages ======================

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Define country of interes throuhg 3-digit ISO code
ISO ='ISO'

# Load Area of interest (shp)
shp <- "aoi.shp"

# Target soil attribute (Mandatory 10)
target_properties<- c("ph_0_30", "k_0_30" , "soc_0_30" ,"bd_0_30", "cec_0_30","p_0_30",   
                      "n_0_30","clay_0_30", "sand_0_30" ,"silt_0_30")

# Function for Uncertainty Assessment
load(file = "03-Scripts/eval.RData")

#load packages
library(tidyverse)
library(caret)
library(terra)
library(Boruta)
library(ranger)

# 1 - Merge soil data with environmental covariates ============================
# Run step 1 only once, then comment the code until section 2 ==================
# 
## 1.1 - Load covariates files (list of GEE tiles of covariates) ---------------
f <- list.files("01-Data/covs", ".tif$", full.names = TRUE)
covs <- rast(f[1])
ncovs <- names(covs)

# 1.2 - Load the soil data (Script 2) -----------------------------------------
dat <- read_csv("02-Outputs/harmonized_soil_data.csv")
X <- dat
# Convert soil data into a spatial object (check https://epsg.io/6204)
dat <- vect(dat, geom=c("x", "y"), crs = crs(covs))

## 1.3 - Extract values from covariates to the soil points ---------------------
X$ID <- 1:nrow(X)

for (i in seq_along(f)) {
  covs <- rast(f[i])
  pv <- terra::extract(x = covs, y = dat, xy=FALSE, ID=TRUE)
  ids <- pv$ID[complete.cases(pv)]
  if (i==1) {
    X <- left_join(X,pv, by = "ID")
  } else {
    if (length(ids)>0) {
      ID.idx <- which(names(X)=="ID")
      X.ncol <- ncol(X)
      X[,ID.idx:X.ncol] <- map2_df(X[,ID.idx:X.ncol], pv, ~ coalesce(.x, .y))
    }
  }
  print(i)
}

summary(X)

X%>%
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% # convert to spatial object
  mapview::mapview(zcol = "dtm_twi_500m", cex = 3, lwd = 0.1)

write_csv(X, "02-Outputs/regression_matrix.csv")

# 

acc <- tibble(
  Soil_Property = character(),
  ME = numeric(),
  MAE = numeric(),
  RMSE = numeric(),
  r = numeric(),
  r2 = numeric(),
  MEC = numeric(),
  rhoC = numeric(),
  Cb = numeric()
)

for (k in seq_along(target_properties)) {
  soilatt <- target_properties[k]

  ## 1.4 - Target soil attribute + covariates ------------------------------------
  d <- dplyr::select(X, all_of(soilatt), all_of(ncovs))
  d <- na.omit(d)
  d <- as.data.frame(d)
  # 2 - Covariate selection with Boruta package ==================================
  # Wrapper feature selection algorithm
  ## 2.1 - Run the Boruta algorithm ----------------------------------------------
  fs_bor <- Boruta(y = d[,soilatt], x = d[-1], maxRuns = 100, doTrace = 1)
  
  ## 2.2 - Plot variable importance and selected features ------------------------
  png(filename = paste0("02-Outputs/Boruta_FS_",soilatt,".png"), 
      width = 15, height = 25, units = "cm", res = 600)
  par(las = 2, mar = c(4, 10, 4, 2) + 0.1)
  Boruta:::plot.Boruta(fs_bor, horizontal = TRUE, ylab = "",
                       xlab = "Importance", cex.axis=0.60)
  dev.off()
  ## 2.3 - Extract the selected feature variables --------------------------------
  (fs_vars <- getSelectedAttributes(fs_bor, withTentative = TRUE))
  
  # 3 - QRF Model calibration with ranger ========================================
  ## 3.1 - Set training parameters -----------------------------------------------
  fitControl <- trainControl(method = "repeatedcv",
                             number = 10,         ## 10 -fold CV
                             repeats = 10,        ## repeated 10 times
                             savePredictions = TRUE)
  
  ## 3.2 - Tune hyperparameters --------------------------------------------------
  mtry <- round(length(fs_vars)/3)
  tuneGrid <-  expand.grid(
    mtry = abs(c(mtry-round(mtry/2), 
                 mtry, 
                 mtry+round(mtry/2))),
    min.node.size = 5,
    splitrule = c("variance", "extratrees", "maxstat")
  )
  
  ## 3.3 - Calibrate the ranger model --------------------------------------------
  print(soilatt)
  print("training the model...")
  model_rn <- caret::train(
    y = d[, soilatt], x = d[,fs_vars],
    method = "ranger",
    quantreg = TRUE,
    importance = "permutation",
    trControl = fitControl,
    verbose = TRUE,
    tuneGrid = tuneGrid
  )
  print(model_rn)
  print(model_rn$bestTune)
  
  ## 3.4 - Extract covariate importance ------------------------------------------
  # png(filename = paste0("02-Outputs/Model_varImp_",soilatt,".png"), 
  #     width = 15, height = 25, units = "cm", res = 600)
  plot(varImp(model_rn))
  # dev.off()
  
  ## 3.5 - Save the ranger model -------------------------------------------------
  saveRDS(model_rn, file = paste0("02-Outputs/models/ranger_model_",soilatt,".rds"))
  # model_rn <- readRDS(paste0("02-Outputs/models/ranger_model_",soilatt,".rds"))
  
  
  # 4 - Accuracy assessment ======================================================
  ## 4.1 - extract observed and predicted values ---------------------------------
  o <- model_rn$pred %>% 
    filter(mtry == model_rn$bestTune$mtry, 
           splitrule==model_rn$bestTune$splitrule, 
           min.node.size==model_rn$bestTune$min.node.size) %>% 
    select(obs) %>% as.vector() %>% unlist()
  p <- model_rn$pred %>% 
    filter(mtry == model_rn$bestTune$mtry, 
           splitrule==model_rn$bestTune$splitrule, 
           min.node.size==model_rn$bestTune$min.node.size) %>% 
    select(pred) %>% as.vector() %>% unlist()
  df <- data.frame(o,p)
  
  ## 4.2 - Plot and save scatterplot --------------------------------------------- 
  (g1 <- ggplot(df, aes(x = o, y = p)) + 
     geom_point(alpha = 0.3) + 
     geom_abline(slope = 1, intercept = 0, color = "red")+
     ylim(c(min(o), max(o))) + theme(aspect.ratio=1)+ 
     labs(title = soilatt) + 
     xlab("Observed") + ylab("Predicted"))
  ggsave(g1, filename = paste0("02-Outputs/residuals_",soilatt,".png"), scale = 1,
         units = "cm", width = 12, height = 12)
  
  ## 4.3 - Print accuracy coeficients --------------------------------------------
  # https://github.com/AlexandreWadoux/MapQualityEvaluation
  print(eval(p,o))
  acc[k,1] <- soilatt
  acc[k,2:9] <- eval(p,o)
  write_csv(acc, "02-Outputs/accuracy.csv")
}

## Load covariates -------------------------------------------------------
terra::terraOptions(memfrac = 0.9)
covs <- list()
# read the GEE tiles 
f <- list.files("01-Data/covs", "covs_", full.names = TRUE)
for (i in seq_along(f)) {
  covs[[i]] <- rast(f[i])
}
ncovs <- names(covs[[i]])
# set_to_0 <- c(which(str_detect(ncovs, pattern = c("fpar"))),9)
# for (i in seq_along(covs)) {
#   for (j in set_to_0) {
#     covs[[i]][[j]][is.na(covs[[i]][[j]])] <- 0
#   }
#   print(i)
# }
# f <- str_replace(f, "GEE", "GEE_Marcos")
# for (i in seq_along(f)) {
#   writeRaster(covs[[i]], f[i], overwrite = TRUE,gdal=c("COMPRESS=DEFLATE"))  
# }

# Prediction ===============================================================

## 5.2 - Predict soil attributes per tiles -------------------------------------
# country borders
v <- vect(shp)

for (i in seq_along(target_properties)) {
  (soilatt <- target_properties[i])
  #load model
  model_rn <- read_rds(paste0("02-Outputs/models/ranger_model_",soilatt,".rds"))

  for (j in seq_along(covs)) {
    
    gc()
    
    # select covariates with the tile j
    r <- covs[[j]][[1]]
    # split the GEE tiles into smaller tiles
    t <- rast(nrows = 4, ncols = 1, extent = ext(r), crs = crs(r))
    tile <- makeTiles(r, t,overwrite=TRUE, filename= "02-Outputs/tiles/tiles.tif")
    
    for (n in seq_along(tile)) {
      t <- rast(tile[n])
      covst <- crop(covs[[j]], t)
      
      # create a function to extract the predited values from ranger::predict.ranger()
      pfun <- \(...) { predict(...)$predictions |> t() }
      
      # predict conditional standard deviation
      terra::interpolate(covst, 
                         model = model_rn$finalModel, 
                         fun=pfun, 
                         na.rm=TRUE, 
                         type = "quantiles", 
                         what=sd,
                         filename = paste0("02-Outputs/tiles/soilatt_tiles/sd_",
                                           soilatt,"_tile_", j,"_",n, ".tif"), 
                         overwrite = TRUE, gdal=c("COMPRESS=DEFLATE"))
      gc()
      # predict conditional mean
      terra::interpolate(covst, 
                         model = model_rn$finalModel, 
                         fun=pfun, 
                         na.rm=TRUE, 
                         type = "quantiles", 
                         what=mean,
                         filename = paste0("02-Outputs/tiles/soilatt_tiles/mean_",
                                           soilatt,"_tile_", j,"_",n, ".tif"), 
                         overwrite = TRUE, gdal=c("COMPRESS=DEFLATE"))
      gc()
      print(paste("tile", j,"_", n, "of", length(covs)))
    }
    
  }        

  
  ## 5.3 - Merge tiles both prediction and st.Dev --------------------------------
  f_mean <- list.files(path = "02-Outputs/tiles/soilatt_tiles/", 
                     pattern = paste0("mean_", soilatt), full.names = TRUE)
  f_sd <- list.files(path = "02-Outputs/tiles/soilatt_tiles/", 
                     pattern =  paste0("sd_", soilatt), full.names = TRUE)
  r_mean_l <- list()
  r_sd_l <- list()
  
  for (g in 1:length(f_mean)){
    r <- rast(f_mean[g])
    r_mean_l[[g]] <-r
    rm(r)
  }

  for (g in 1:length(f_sd)){
    r <- rast(f_sd[g])
    r_sd_l[[g]] <-r
    rm(r)
  }
  r_mean <-sprc(r_mean_l)
  r_sd <-sprc(r_sd_l)
  
  pred_mean <- mosaic(r_mean)
  pred_sd <- mosaic(r_sd)
  
  # Crop and mask the maps for each country
  for (h in seq_along(cty)) {
    pred_mean <- pred_mean %>% 
      crop(v[v$ISO3CD==cty[h]]) %>% 
      mask(v[v$ISO3CD==cty[h]])
    pred_sd <- pred_sd %>% 
      crop(v[v$ISO3CD==cty[h]]) %>% 
      mask(v[v$ISO3CD==cty[h]])
    writeRaster(pred_mean, 
                paste0("02-Outputs/noMaskedMaps/mean_",soilatt, ".tif"),
                overwrite=TRUE, gdal=c("COMPRESS=DEFLATE"))
    writeRaster(pred_sd, 
                paste0("02-Outputs/noMaskedMaps/sd_",soilatt, ".tif"),
                overwrite=TRUE, gdal=c("COMPRESS=DEFLATE"))
    ## 6.1 - Mask croplands --------------------------------------------------------
    msk <- rast("01-Data/covs/mask.tif")
    msk <- terra::project(msk, pred_mean)
    pred_mean <- mask(pred_mean, msk)
    pred_sd <- mask(pred_sd, msk)
    
    ## 6.2 - Save results ----------------------------------------------------------
    writeRaster(pred_mean, 
                paste0("02-Outputs/maps/",ISO,"_mean_",soilatt, ".tif"),
                overwrite=TRUE)
    writeRaster(pred_sd, 
                paste0("02-Outputs/maps/",ISO,"_sd_",soilatt, ".tif"),
                overwrite=TRUE)
  }
  print(paste("prediction of", soilatt, "ready"))
  rm(pred_sd)
  rm(pred_mean)
  unlink(f_mean)
  unlink(f_sd)
  gc()
}

