#
# Digital Soil Mapping
# Soil Profile Data
# Cleaning and Processing
#
# GSP-Secretariat
# Contact: Isabel.Luotto@fao.org
#          Marcos.Angelini@fao.org
#_______________________________________________________________________________

#Empty environment and cache 
rm(list = ls())
gc()

# Content of this script =======================================================
# The goal of this script is to organise the soil data for mapping, including:
# 
# 0 - User-defined variables 
# 1 - Set working directory and load necessary packages
# 2 - Import national data 
# 3 - select useful columns
# 4 - Quality check
# 5 - Estimate BD using pedotransfer function
# 6 - Estimate Organic Carbon Stock (ocs) 
# 7 - Harmonize soil layers
# 8 - Plot and save results
#_______________________________________________________________________________

# 0 - User-defined variables ===================================================
wd <- 'C:/Users/hp/Documents/GitHub/Digital-Soil-Mapping'
#wd <- "C:/GIT/Digital-Soil-Mapping"

# 1 - Set working directory and load necessary packages ========================
setwd(wd) # change the path accordingly

library(tidyverse) # for data management and reshaping
library(readxl) # for importing excel files
library(mapview) # for seeing the profiles in a map
library(sf) # to manage spatial data (shp vectors) 
# install.packages("devtools") 
# devtools::install_bitbucket("brendo1001/ithir/pkg") #install ithir package


# 2 - Import national data =====================================================
# Save your national soil dataset in the data folder /01-Data as a .csv file or 
# as a .xlsx file

## 2.1 - for .xlsx files -------------------------------------------------------
# Import data with coordinates
dxy <- read_excel("01-Data/data_with_coord.xlsx", sheet = 1)
dxy <- select(dxy,LabID, x=Long, y=Lat, p=P_ppm)
dxy <- dxy %>% na.omit()
# Import data without coordinates
darea <- readxl::read_excel("01-Data/Buenos_Aires_sur_P_Bray_MA.xls")

# 3 - select useful columns ====================================================
# 4 - Quality check ============================================================

## 4.1 - Check locations -------------------------------------------------------
# https://epsg.io/6204
dxy %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% # convert to spatial object
  mapview(zcol = "p", cex = 2, lwd = 0.1) # visualise in an interactive map

# profile P5810 is wrongly located, so let's remove it
dxy <- filter(dxy, p < 100)

hist(dxy$p)
summary(dxy)

write_csv(dxy, "01-Data/data_with_coord.csv")
## data without coordinates

# library(splines2)
# estimate <- function(x){
#   new_x <- predict(fit_spline, data.frame(cum_probs = c(x)))
#   return(new_x)
# }
# 
# quants_df <- select(darea, x0:x1)
# district <- darea$code
# x <- NULL
# cum_probs <- seq(0.0, 1, 0.25)
# for (i in 1:nrow(quants_df)) {
#   quants <- t(quants_df[i,])
#   row.names(quants) <- NULL
#   df_quants <- data.frame(cum_probs, (quants))
#   names(df_quants)[2] <- "quants"
#   fit_spline <- lm(quants ~ bSpline(cum_probs, df = 5), df_quants)
#   df_quants$fit_spline <- predict(fit_spline, df_quants)
#   y <-(estimate(runif(200, 0, 1)))
#   x <- rbind(x, data.frame(district[i], p=y))
# }
# names(x) <- c("district", "p")


library(terra)

aoi <- vect("01-Data/AOI_Arg.shp")

darea <- darea %>% filter(stratum %in% aoi$stratum)
write_csv(darea, "01-Data/sim_data_no_coord.csv")
# crop1 <- rast("01-Data/land cover/mnc-invierno-2020.tif")
# crop2 <- rast("01-Data/land cover/mnc-verano-2021.tif")
# covs <- rast("01-Data/covs/bio1.tif")
# 
# crop1 <- project(crop1, covs)
# crop2 <- project(crop2, covs)
# 
# crop1[crop1$`mnc-invierno-2020`==20] <- 0
# crop1[crop1$`mnc-invierno-2020`!= 0] <- 1
# plot(crop1)
# 
# crop2[crop2$`mnc-verano-2021`==22] <- 0
# crop2[crop2$`mnc-verano-2021`!=0] <- 1
# plot(crop2)
# crops <- crop1+crop2
# crops[crops$`mnc-invierno-2020`==2] <- 1
# names(crops) <- "crops"
# plot(crops)
# plot(aoi, add=T)

# writeRaster(crops, "01-Data/land cover/croplands.tif")
names(aoi)[5] <- "stratum"

crops <- rast("01-Data/land cover/croplands.tif")
aoir <- rasterize(aoi, crops, field = "stratum")
plot(aoir)
aoir_crop <- aoir*crops
aoir_crop[aoir_crop$stratum==0] <- NaN
plot(aoir_crop)
aoir_crop <- terra::trim(aoir_crop)
crops <- crop(crops, aoir_crop)
writeRaster(aoir_crop, "01-Data/land cover/SE_districts_croplands.tif", overwrite=TRUE)
writeRaster(crops, "01-Data/land cover/SE_croplands.tif")
# 

