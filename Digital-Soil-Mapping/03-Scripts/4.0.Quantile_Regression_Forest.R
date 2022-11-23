#_______________________________________________________________________________
#
# Quantile Regression Forest
# Soil Property Mapping
#
# GSP-Secretariat
# Contact: Isabel.Luotto@fao.org
#          Marcos.Angelini@fao.org
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
wd <- 'C:/Users/luottoi/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping'
setwd(wd)

# Define country of interes throuhg 3-digit ISO code
ISO ='demo'

# Load Area of interest (shp)
AOI <- '01-Data/AOI.shp'

# Terget soil attribute
target_properties<- c("ph_0_30","k_0_30","soc_0_30", "clay_0_30", "bd_0_30","cec_0_30","p_0_30" , "n_0_30" )

# Function for Uncertainty Assessment
load(file = "03-Scripts/eval.RData")

#load packages
library(tidyverse)
library(data.table)
library(caret)
library(quantregForest)
library(terra)
library(sf)
library(doParallel)


# 1 - Merge soil data with environmental covariates ============================

## 1.1 - Load covariates -------------------------------------------------------
files <- list.files(path= '01-Data/covs/', pattern = '.tif$', full.names = T)

ref <- rast(files[1])
covs <- list()
for (i in seq_along(files)) {
  r <- rast(files[i])
  r <- project(r, ref)
  covs[[i]] <- r
}
covs <- rast(covs)


ncovs <- str_remove(files, "01-Data/covs/")
ncovs <- str_remove(ncovs, ".tif")
ncovs <- str_replace(ncovs, "-", "_")
names(covs) <- ncovs
names(covs)[19] <- "dtm_pos_openness2_250m" 
names(covs)[17] <- "dtm_pos_openness3_250m"
# covs <- rast("01-Data/covs/covs.tif")
# ncovs <- names(covs)

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

for(soilatt in unique(target_properties)){


## 1.4 - Target soil attribute + covariates ------------------------------------
d <- dplyr::select(dat, soilatt, names(covs))
d <- na.omit(d)

# 2 - Covariate selection with RFE =============================================
## 2.1 - Setting parameters ----------------------------------------------------
# Repeatedcv = 3-times repeated 10-fold cross-validation
fitControl <- rfeControl(functions = rfFuncs,
                         method = "repeatedcv",
                         number = 10,         ## 10 -fold CV
                         repeats = 3,        ## repeated 3 times
                         verbose = TRUE,
                         saveDetails = TRUE, 
                         returnResamp = "all")

# Set the regression function
fm = as.formula(paste(soilatt," ~", paste0(ncovs,
                                             collapse = "+")))

# Calibrate the model using multiple cores
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)


## 2.2 - Calibrate a RFE model to select covariates ----------------------------
covsel <- rfe(fm,
              data = d,  
              sizes = seq(from=10, to=length(ncovs)-1, by = 5),
              rfeControl = fitControl,
              verbose = TRUE,
              keep.inbag = T)
stopCluster(cl)
saveRDS(covsel, "02-Outputs/models/covsel.rda")

## 2.3 - Plot selection of covariates ------------------------------------------
trellis.par.set(caretTheme())
plot(covsel, type = c("g", "o"))

# Extract selection of covariates and subset covs
opt_covs <- predictors(covsel)

# 3 - QRF Model calibration ====================================================
## 3.1 - Update formula with the selected covariates ---------------------------
fm <- as.formula(paste(soilatt," ~", paste0(opt_covs, collapse = "+")))

# parallel processing
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

## 3.2 - Set training parameters -----------------------------------------------
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,         ## 10 -fold CV
                           repeats = 3,        ## repeated 3 times
                           savePredictions = TRUE)

# Tune mtry hyperparameters
mtry <- round(length(opt_covs)/3)
tuneGrid <-  expand.grid(mtry = c(mtry-5, mtry, mtry+5))

## 3.3 - Calibrate the QRF model -----------------------------------------------
model <- caret::train(fm,
                      data = d,
                      method = "qrf",
                      trControl = fitControl,
                      verbose = TRUE,
                      tuneGrid = tuneGrid,
                      keep.inbag = T,
                      importance = TRUE)
stopCluster(cl)
gc()


## 3.4 - Extract predictor importance as relative values (%)
x <- randomForest::importance(model$finalModel)
model$importance <- x
## 3.5 - Print and save model --------------------------------------------------
print(model)
saveRDS(model, file = paste0("02-Outputs/models/model_",soilatt,".rds"))

# 4 - Uncertainty assessment ===================================================
# extract observed and predicted values
o <- model$pred$obs
p <- model$pred$pred
df <- data.frame(o,p)

## 4.1 - Plot and save scatterplot --------------------------------------------- 
(g1 <- ggplot(df, aes(x = o, y = p)) + 
  geom_point(alpha = 0.1) + 
   geom_abline(slope = 1, intercept = 0, color = "red")+
  ylim(c(min(o), max(o))) + theme(aspect.ratio=1)+ 
  labs(title = soilatt) + 
  xlab("Observed") + ylab("Predicted"))
# ggsave(g1, filename = paste0("02-Outputs/residuals_",soilatt,".png"), scale = 1, 
#        units = "cm", width = 12, height = 12)

## 4.2 - Print accuracy coeficients --------------------------------------------
# https://github.com/AlexandreWadoux/MapQualityEvaluation
eval(p,o)

## 4.3 - Plot Covariate importance ---------------------------------------------
(g2 <- varImpPlot(model$finalModel, main = soilatt, type = 1))

# png(filename = paste0("02-Outputs/importance_",soilatt,".png"), 
#     width = 15, height = 15, units = "cm", res = 600)
# g2
# dev.off()

# 5 - Prediction ===============================================================
# Generation of maps (prediction of soil attributes) 
## 5.1 - Produce tiles ---------------------------------------------------------
r <-covs[[1]]
t <- rast(nrows = 5, ncols = 5, extent = ext(r), crs = crs(r))
tile <- makeTiles(r, t,overwrite=TRUE,filename="02-Outputs/tiles/tiles.tif")

## 5.2 - Predict soil attributes per tiles -------------------------------------
# loop to predict on each tile

for (j in seq_along(tile)) {
  gc()
  t <- rast(tile[j])
  covst <- crop(covs, t)
  
  
  # plot(r)# 
  pred_mean <- terra::predict(covst, model = model$finalModel, na.rm=TRUE,  
                              cpkgs="quantregForest", what=mean)
  pred_sd <- terra::predict(covst, model = model$finalModel, na.rm=TRUE,  
                            cpkgs="quantregForest", what=sd)  
  
  
  
  # ###### Raster package solution (in case terra results in many NA pixels)
  # library(raster)
  # covst <- stack(covst)
  # class(final_mod$finalModel) <-"quantregForest"
  # # Estimate model uncertainty
  # pred_sd <- predict(covst,model=final_mod$finalModel,type=sd)
  # # OCSKGMlog prediction based in all available data
  # pred_mean <- predict(covst,model=final_mod)
  # 
  # 
  # ##################################  
  
  writeRaster(pred_mean, 
              filename = paste0("02-Outputs/tiles/soilatt_tiles/",
                                soilatt,"_tile_", j, ".tif"), 
              overwrite = TRUE)
  writeRaster(pred_sd, 
              filename = paste0("02-Outputs/tiles/soilatt_tiles/",
                                soilatt,"_tileSD_", j, ".tif"), 
              overwrite = TRUE)
  
  rm(pred_mean)
  rm(pred_sd)
  
  
  print(paste("tile",tile[j]))
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


plot(pred_mean)
plot(pred_sd)


# 6 - Export final maps ========================================================
## 6.1 - Mask croplands --------------------------------------------------------
msk <- rast("01-Data/mask.tif")
plot(msk)
pred_mean <- mask(pred_mean, msk)
plot(pred_mean)
pred_sd <- mask(pred_sd, msk)
plot(pred_sd)
plot(pred_sd/pred_mean*100, main = paste("CV",soilatt))

## 6.2 - Save results ----------------------------------------------------------

# Harmonized naming 
if (soilatt == 'ph_0_30'){
  name <-'_GSNmap_pH_Map030.tiff'
}else if (soilatt == 'k_0_30'){
  name <-'_GSNmap_Ktot_Map030.tiff'
}else if (soilatt == 'soc_0_30'){
  name <-'_GSNmap_SOC_Map030.tiff'
}else if (soilatt == 'clay_0_30'){
  name <-']_GSNmap_Clay_Map030.tiff'
}else if (soilatt == 'bd_0_30'){
  name <-'_GSNmap_BD_Map030.tiff'
}else if (soilatt == 'cec_0_30'){
  name <-'_GSNmap_CEC_Map030.tiff'
}else if (soilatt == 'p_0_30'){
  name <-'_GSNmap_Pav_Map030.tiff'
}else if (soilatt == 'n_0_30'){
  name <-'_GSNmap_Ntot_Map030.tiff'
}

writeRaster(pred_mean, 
            paste0("02-Outputs/maps/",ISO,name),
            overwrite=TRUE)
writeRaster(pred_sd, 
            paste0("02-Outputs/maps/",ISO, '_SD',name),
            overwrite=TRUE)



}


