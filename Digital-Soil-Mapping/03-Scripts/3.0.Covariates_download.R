#Empty environment and cache
rm(list = ls());
gc()

# Content of this script =======================================================
# The goal of this script is to organise to clip and dowload the covariates and 
# it includes the following steps:
# 
# 0 -User-defined variables 
# 1 - Set working directory and load necessary packages
# 2 - Import shapefile
# 3 - Overview of covariates 
# 4 - Initialize GEE
# 5 - Upload shapefile to GEE OR use uploaded UN borders
# 6 - Clip and download the covariates
# 7 - Clip and download cropland mask (Copernicus Global Land Service)
#_______________________________________________________________________________


# 0 - User-defined variables ===================================================
# Working directory
wd <- 'C:/Users/luottoi/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping'
#wd <- "C:/GIT/GSNmap-TM/Digital-Soil-Mapping"

# Output covariate folder
#output_dir <-''
output_dir <-'01-Data/covs/'

# Area of interest: either own shapefile or 3-digit ISO code to extract from 
# UN 2020 boundaries
aoi <- '01-Data/AOI.shp'
# AOI <- 'MKD'
# Resolution and projection
res = 250
crs = "EPSG:4326"
#_______________________________________________________________________________

#  1 - Set working directory and load necessary packages ======================= 
# Set working directory
setwd(wd)
#load libraries
library(raster)
library(terra)
library(tidyverse)
library(sf)
library(rgee)
library(googledrive)


# 2 - Import shapefile =========================================================
AOI <- read_sf(aoi)
# convert AOI to a box polygon
#AOI <- st_as_sfc(st_bbox(AOI))
#AOI <- st_as_sf(AOI)


# 3 - Overview of covariates ===================================================
# CLIMATIC VARIABLES from CHELSA
# VEGETATION INDICES, FPAR and LAND SURFACE TEMPERATURE from MODIS
# LAND COVER LAYERS from Dynamic World 10m near-real-time (NRT) 
# TERRAINE attributes from OpenLandMap

# for more information about the single covariates: open covariates.xslx in the 
# training material folder

# 4 - Initialize GEE ===========================================================
ee_Initialize()

# 5 - Upload shapefile to GEE OR use uploaded UN borders =======================
## 5.1 Convert shp to gee geometry ---------------------------------------------
region <- sf_as_ee(AOI)
region = region$geometry()

## 5.2 Extract from UN 2020 map using ISO code ---------------------------------
# region <-ee$FeatureCollection("projects/digital-soil-mapping-gsp-fao/assets/UN_BORDERS/BNDA_CTY")%>%
#   ee$FeatureCollection$filterMetadata('ISO3CD', 'equals', AOI)
# region = region$geometry()
# AOI_shp <-ee_as_sf(region)
# AOI_shp <- st_collection_extract(AOI_shp, "POLYGON")
# write_sf(AOI_shp, paste0('01-Data/',AOI,'.shp'))
# aoi <- vect(AOI_shp)

# 6 - Clip and download covariates =============================================
assetname <- read_csv("01-Data/covs/covs_rgee.csv")
assetname$num <- rownames(assetname)

# Loop over the names of assets to clip and dowload the covariates
for (i in unique(assetname$ID)){
  
  #Extract filename 
  filename <- sub('.*\\/', '', i)
  
  #Clip image to the extent of the AOI
  image <- ee$Image(i) %>%
    ee$Image$clip(region)%>%
    ee$Image$toFloat()
  
  # Resample to target resolution
  image = image$resample('bilinear')$reproject(
    crs= crs,
    scale= res)
  
  
  #Export clipped covariate as raster
  raster <- ee_as_raster(
    image = image,
    scale= res,
    region = region,
    via = "drive",
    maxPixels = 1e+12
  )
  
  plot(raster)
  
  num <- assetname[assetname$ID == i, 'num']
  
  raster <- mask(raster, AOI)
  writeRaster(raster, paste0(output_dir,filename, '.tif'), overwrite=T)
  print(paste(filename, 'exported successfully - Covariate',num, 'out of 68'))
}

# 7 - Clip and download cropland mask (Copernicus Global Land Service) =========
image1 <- ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$select("discrete_classification")%>%
  ee$ImageCollection$filterBounds(region)%>%
  ee$ImageCollection$toBands()

# default resampling = nearest neighbor
image1 = image1$resample()$reproject(
  crs= crs,
  scale= res)


#Reclassify 
# for more info on the single land cover classes: https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_Landcover_100m_Proba-V-C3_Global

inList <- ee$List(c(0  ,20  ,30  ,40  ,50  ,60  ,70  ,80  ,90 ,100 ,111 ,112 ,113 ,114 ,115 ,116 ,121 ,122 ,123 ,124 ,125 ,126, 200))
outList <- ee$List(c(0,  0,  0,  1,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0))

# Extract points for target classes (works for medium/small countries) Solution for large countries below
FAO_lu<- image1$remap(inList, outList)
FAO_lu <-FAO_lu$toDouble()
FAO_lu =FAO_lu$clip(region)
#Convert 0 to NA
mask <- FAO_lu$neq(0)
FAO_lu <- FAO_lu$updateMask(mask)

#Obtain points
FAO_lu <- ee_as_raster(
  image = FAO_lu,
  scale= res,
  region = region,
  via = "drive"
)

AOI <- read_sf(aoi)
FAO_lu <- mask(FAO_lu,AOI)

cov  <- rast('01-Data/covs/bio1.tif')
FAO_lu <- project(rast(FAO_lu),cov)

writeRaster(FAO_lu, ("01-Data/mask.tif"), overwrite= T)
