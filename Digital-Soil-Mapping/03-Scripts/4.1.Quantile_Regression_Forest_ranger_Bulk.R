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
AOI <- '01-Data/AOI.shp'

# Terget soil attribute (Mandatory 10)
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

## 1.1 - Load covariates -------------------------------------------------------
covs <- rast("01-Data/covs/Covariates.tif") # match case of the file name
ncovs <- names(covs)


## 1.2 - Load the soil data (Script 2) -----------------------------------------
dat <- read_csv("02-Outputs/harmonized_soil_data.csv")

# Convert soil data into a spatial object (check https://epsg.io/6204)
dat <- vect(dat, geom=c("x", "y"), crs = crs(covs))

# Reproject point coordinates to match coordinate system of covariates
dat <- terra::project(dat, covs)
names(dat)

## 1.3 - Extract values from covariates to the soil points ---------------------
pv <- terra::extract(x = covs, y = dat, xy=F)
dat <- cbind(dat,pv)
dat <- as.data.frame(dat)

summary(dat)

# create tiles for step 5
r <-covs[[1]]
t <- rast(nrows = 5, ncols = 5, extent = ext(r), crs = crs(r))
tile <- makeTiles(r, t,overwrite=TRUE,filename="02-Outputs/tiles/tiles.tif")

for(soilatt in unique(target_properties)){
  
  ## 1.4 - Target soil attribute + covariates ------------------------------------
  d <- dplyr::select(dat, all_of(soilatt), all_of(ncovs))
  d <- na.omit(d)
  
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
                 mtry-round(mtry/3), 
                 mtry, 
                 mtry+round(mtry/3),
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
  
  
  # 5 - Prediction ===============================================================
  # Generation of maps (prediction of soil attributes) 
  ## 5.1 - Produce tiles ---------------------------------------------------------
  # r <-covs[[1]]
  # t <- rast(nrows = 5, ncols = 5, extent = ext(r), crs = crs(r))
  # tile <- makeTiles(r, t,overwrite=TRUE,filename="02-Outputs/tiles/tiles.tif")
  
  ## 5.2 - Predict soil attributes per tiles -------------------------------------
  # loop to predict soilatt on each tile
  
  for (j in seq_along(tile)) {
    gc()
    # read the tile
    t <- rast(tile[j])
    # crop the selected covariates with the tile j
    covst <- crop(covs[[fs_vars]], t)
    
    # create a function to extract the predited values from ranger::predict.ranger()
    pfun <- \(...) { predict(...)$predictions |> t() }
    
    # predict conditional standard deviation
    terra::interpolate(covst, 
                       model = model_rn$finalModel, 
                       fun=pfun, 
                       na.rm=TRUE, 
                       type = "quantiles", 
                       what=sd,
                       filename = paste0("02-Outputs/tiles/soilatt_tiles/",
                                         soilatt,"_tileSD_", j, ".tif"), 
                       overwrite = TRUE)
    
    # predict conditional mean
    terra::interpolate(covst, 
                       model = model_rn$finalModel, 
                       fun=pfun, 
                       na.rm=TRUE, 
                       type = "quantiles", 
                       what=mean,
                       filename = paste0("02-Outputs/tiles/soilatt_tiles/",
                                         soilatt,"_tile_", j, ".tif"), 
                       overwrite = TRUE)
    
    print(paste("tile", j, "of", length(tile)))
  }
  
  
  ## 5.3 - Merge tiles both prediction and st.Dev --------------------------------
  f_mean <- list.files(path = "02-Outputs/tiles/soilatt_tiles/", 
                       pattern = paste0(soilatt,"_tile_"), full.names = TRUE)
  f_sd <- list.files(path = "02-Outputs/tiles/soilatt_tiles/", 
                     pattern =  paste0(soilatt,"_tileSD_"), full.names = TRUE)
  r_mean_l <- list()
  r_sd_l <- list()
  
  for (g in 1:length(f_mean)){
    r <- rast(f_mean[g])
    r_mean_l[g] <-r
    rm(r)
  }
  
  for (g in 1:length(f_sd)){
    
    r <- rast(f_sd[g])
    r_sd_l[g] <-r
    rm(r)
  }
  r_mean <-sprc(r_mean_l)
  r_sd <-sprc(r_sd_l)
  
  pred_mean <- mosaic(r_mean)
  pred_sd <- mosaic(r_sd)
  
  aoi <- vect(AOI)
  pred_mean <- mask(pred_mean,aoi)
  pred_sd <- mask(pred_sd,aoi)
  
  writeRaster(pred_mean, 
              paste0("02-Outputs/noMaskedMaps/mean_",soilatt, ".tif"),
              overwrite=TRUE)
  writeRaster(pred_sd, 
              paste0("02-Outputs/noMaskedMaps/sd_",soilatt, ".tif"),
              overwrite=TRUE)
  
  plot(c(pred_mean, pred_sd), main = paste(c("mean","sd"), soilatt), 
       col = hcl.colors(100, "Viridis"))
  
  # 6 - Export final maps ========================================================
  ## 6.1 - Mask croplands --------------------------------------------------------
  msk <- rast("01-Data/covs/mask.tif")
  # plot(msk)
  msk <- terra::project(msk, pred_mean)
  pred_mean <- mask(pred_mean, msk)
  # plot(pred_mean)
  pred_sd <- mask(pred_sd, msk)
  # plot(pred_sd)
  plot(pred_sd/pred_mean*100, main = paste("Coeficient of variation", soilatt), 
       col = hcl.colors(100, "Viridis"))
  
  ## 6.2 - Save results ----------------------------------------------------------
  writeRaster(pred_mean, 
              paste0("02-Outputs/maps/",ISO,"_mean_",soilatt, ".tif"),
              overwrite=TRUE)
  writeRaster(pred_sd, 
              paste0("02-Outputs/maps/",ISO,"_sd_",soilatt, ".tif"),
              overwrite=TRUE)
}


