##########################
#                        #
# GSOCseq Stats          #
# Stats per different    #
# categories             #
##########################

##############################################
# readme file

# Zonal statistics for GSOCmap TR v1.6

#Input files
# GSOCmap v1.6_WGS84 (raster)
# UN country borders (shapefile, better raster (Isa))
# IPCC climate regions (raster)
# Land cover (Globcover raster)
# Soil type (HWSD raster)

# Output files
# data frame with sum statistics for 
# SOC stocks 
    # per Country
    # per Climate region
    # per Land cover
    # per soil type
# UNIT: Pg/km^2 ????
###############################################
#empty global environment
rm(list = ls())
#set working directory
wd <-"C:/Users/Mainka/OneDrive - Food and Agriculture Organization/Documents/GSOCmap_update"
setwd(wd)
#Load packages
library(raster)
library(rgdal)
library(data.table)
library(tidyverse)
library(maptools)
library(spatialEco)
library(exactextractr)
##############################################
# for any testing purposes: ECUADOR
#UNmap <- readOGR("spatial data/UNmap_shp/BNDA_CTY.shp")
#UNmap <- UNmap[73,]
##############################################
# load GSOC map
#GSOC <- raster("spatial data/GSOCmap_1.6.1_WGS84_.tif")
#correct pixel area and ha to km2
#GSOC <- GSOC*area(GSOC)*100
# save raster
#writeRaster(GSOC, 'spatial data/GSOC_areacorrected_km2.tif', overwrite=TRUE)

#load corrected raster file
GSOC <- raster("spatial data/GSOC_areacorrected_km2.tif")

#when testing script for functionality
#GSOC <- crop(GSOC, example)
############################################################
# calculate output zonal statistics
# per country
#load input files
#UNmap <- raster("spatial data/UN_2020_worldmap.tif")
UNmap <- shapefile("spatial data/UNmap_shp/BNDA_CTY.shp")
# get attribute table
UNmap.shp <- as.data.frame(UNmap)
# ISOADM = non-independent islands are counted to the country they belong (e.g. Aland Island --> Finland)
UNmap.shp <- rename(UNmap.shp, "ISO" = "ISOADM")

#zonal stats
stats_CTY <- exact_extract(GSOC, UNmap, fun = "sum")
stats_CTY <- data.frame(stats_CTY)
# add mean stocks per country to stats_CTY
stats_CTY$mean <- exact_extract(GSOC, UNmap, fun = 'mean')

cty_soc <- cbind(UNmap.shp, stats_CTY)
cty_soc <- cty_soc %>% 
  rename(sum_SOC = "stats_CTY", mean_SOC = "mean") %>%
  mutate(sum_SOC = sum_SOC/1000000000,
         mean_SOC = mean_SOC/100) %>%
  select(c("ISO3CD","ROMNAM","MAPLAB","CONTCD","ISO","sum_SOC","mean_SOC")) 

cty_soc <- cty_soc %>% group_by(ISO) %>% 
  summarise(sum_SOC = sum(sum_SOC), mean_SOC = mean(mean_SOC, na.rm = T)) %>%
  filter(sum_SOC != 0)

#Annex national submissions
#an <- read.csv('metadata/Annex_National_submissions.csv', sep = ';')
#x <- merge(cty_soc,an, by = "ISO")
#y <- aggregate(stats_CTY ~ ISO + Country, FUN = "sum" , data = x)
#z <- aggregate(mean ~ ISO + Country, FUN = "mean" , data = x)
#UN_zonal <- merge(y,z)
#save file
write.csv(cty_soc, "outputs/zonal_stats_CTY_1.6.csv")
############################################################
# per IPCC climate region
# load IPCC_Climate regions raster file
#IPCC <- raster("spatial data/IPCC_CLIMATE_2919.tif")
#1 ) crop extent
#IPCC <- crop(IPCC, GSOC)
# 2) same resolution
#IPCC <- resample(IPCC, GSOC, method = "ngb")
# save raster
#writeRaster(IPCC,"spatial data/IPCC_CLIMATE_cropped_resampled.tif", overwrite = T)

# load re-calculated raster
IPCC <- raster("spatial data/IPCC_CLIMATE_cropped_resampled.tif")
# 3) zonal statistics
stats_IPCC <- zonal(GSOC, IPCC, fun = 'sum', digits = 0, na.rm = T)
stats_IPCC_mean <- zonal(GSOC, IPCC, fun = 'mean', digits = 0, na.rm = T)
# merge zonal stats with metadata
stats_IPCC <- data.frame(stats_IPCC)
stats_IPCC$mean <- stats_IPCC_mean[,2]
colnames(stats_IPCC)[1] <- "zones"
# load metadata
metadata_IPCC <- read.csv("spatial data/metadata_ipcc.csv", header = T, sep = ",")
stats_IPCC <- merge(metadata_IPCC, stats_IPCC, by = "zones", all= T)
write.csv(stats_IPCC, "outputs/zonal_stats_IPCC_1.6.csv")

#############################################################
# per land cover
# metadata for land cover available in spatial data/landcover_classes.txt
# load land cover raster
cover <- raster("spatial data/ESA_Land_Cover_12clases_FAO_World_2015_1km.tif")
#1 ) crop extent
cover <- crop(cover, GSOC)

# 2) zonal statistics
stats_cover <- zonal(GSOC, cover, fun = 'sum', digits = 0, na.rm = T)
stats_cover_mean <- zonal(GSOC, cover, fun = 'mean', digits = 0, na.rm = T)
# merge zonal stats with metadata
stats_cover <- data.frame(stats_cover)
stats_cover$mean <- stats_cover_mean[,2]

# define vector with zone no. = land cover
stats_cover$land <- c("No Data","Artificial", "Croplands", "Grassland", "Tree covered",
          "Shrubs covered","Herbaceous vegetation flooded","Mangroves",
          "Sparse vegetation","Bare soil","Snow and glaciers","Waterbodies",
          "Treecrops","Paddy fields (rice/flooded crops)")
write.csv(stats_cover, "outputs/zonal_stats_LandCover_1.6.csv")

#################################
# based on GlobCover
#load raster file
globcover <- raster('spatial data/GlobCover_cropped_resampled.tif')

# run zonal statistics
stats_globcover <- zonal(GSOC, globcover, fun = 'sum', digits = 0, na.rm = T)
stats_globcover_mean <- zonal(GSOC, globcover, fun = 'mean', digits = 0, na.rm = T)
# merge zonal stats with metadata
stats_globcover <- data.frame(stats_globcover)
stats_globcover$mean <- stats_globcover_mean[,2]
#load metadata
recl_globcover <- data.frame("zone" = 1:6, "Land_cover" = c("Croplands","Forests","Grasslands",
                                        "Wetlands","Others","Waterbodies")) 
stats_globcover2 <- merge(recl_globcover, stats_globcover, by = "zone", all = T)
#save output
write.csv(stats_globcover2, "outputs/zonal_stats_GlobCover_1.6.csv")

#############################################################
# raster preparation
# per soil type

#Create Soil Unit (SU) raster out of HWSD
# load soil type file
#hwsd <- raster("spatial data/HWSD.tif")

# load HWSD attribute table
#smu <- fread("spatial data/smu.csv", select=c("MU_GLOBAL","SU_CODE"))
#colnames(smu)[1] <- "ID"
#smu$SU_SYMBOL <- factor(smu$SU_SYMBOL)
#hwsd <- as.factor(hwsd)
#recmat <- levels(hwsd)
#recmat <- merge(recmat, smu, by = "ID",all = TRUE)
#recmat <- recmat[,1:2]
#recmat <- recmat[complete.cases(recmat),]
#hwsd <- reclassify(hwsd, recmat)
# save reclassified raster layer
#writeRaster(hwsd, "spatial data/reclassified_HWSD.tif", overwrite = T)

#load reclassified raster file
hwsd <- raster("spatial data/reclassified_HWSD.tif")
#1 ) crop extent
hwsd <- crop(hwsd, GSOC)

# 2) zonal statistics
stats_soiltype <- zonal(GSOC, hwsd, fun = 'sum', digits = 0, na.rm = T)
stats_soiltype_mean <- zonal(GSOC, hwsd, fun = 'mean', digits = 0, na.rm = T)
stats_soiltype <- data.frame(stats_soiltype)
stats_soiltype$mean <- stats_soiltype_mean[,2]
# merge zonal stats with metadata
metadata_HWSD <- read.csv("spatial data/HWSD_withsoiltype.csv", header = T, sep = ",")
soils <- data.frame("SU_CODE" = unique(metadata_HWSD$SU_CODE),"Soil type" = unique(metadata_HWSD$Soil_type))
soils <- arrange(soils,SU_CODE)
stats_soiltype <- rename(stats_soiltype, SU_CODE = zone)
stats_soiltype <- merge(stats_soiltype,soils, by = "SU_CODE", all = T)
write.csv(stats_soiltype, "outputs/zonal_stats_soiltype_1.6.csv")
####################################################################