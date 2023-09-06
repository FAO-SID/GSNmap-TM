#_______________________________________________________________________________
#
# QA/QC
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
# 0 - Set working directory and packages
# 1 - Step 1: Completeness of layers 
# 2 - Step 2: Check the projection and resolution of all data products
# 3 - Step 3: Check the extent
# 4 - Step 4: Check the units, ranges, and outliers
#
# 5 - Export QA/QC report
#_______________________________________________________________________________


# 0 - Set working directory, soil attribute, and packages ======================

# Working directory
wd <- 'C:/Users/hp/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping'
#wd <- 'C:/Users/luottoi/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping'
setwd(wd)

# Define country of interes throuhg 3-digit ISO code
ISO ='ISO'

#load packages
library(terra)
library(readxl)

# Load reference values
dt <- read_xlsx("C:/Users/luottoi/Documents/GitHub/GSNmap-TM/tables/wosis_dist.xlsx")
dt <- dt[!(dt$`Soil property` %in%c( "P Bray I","P Olsen" )),]

# In case old naming system was used
dt$old_prop_ids <- c('Ntot', 'Pav', 'CEC','pH', 'Clay', 'Silt', 'Sand', 'SOC', 'BD', 'Kav')


## Set potential ranges for Available K in ppm

dt[dt$property_id=='k_0_30','Min'] <- 0
dt[dt$property_id=='k_0_30','Max'] <- 150
# 1 - Step 1: Completeness of layers -------------------------------------------

#Check number of layers

## Specify number of soil property maps generated (not including the uncertainty layers)

## Check if all layers were correctly generated (including uncertainty layers)
# and if the correct ISO code and soil property ids were included in the files names
files <- list.files(pattern= '.tif', full.names = T)
names <- list.files( pattern= '.tif', full.names = F)
names <- sub('.tif', '', names)


# Switch depending on the naming system (i.e. files have e.g. Pav instead of p_0_30)
#Step1 <-data.frame(property_id =dt$property_id)
Step1 <-data.frame(property_id =dt$old_prop_ids) #old naming system

Step1$Names <- 'Rename layer'
Step1$Uncertainty <- 'Missing'

for (i in unique(Step1$property_id)){
  
  t11 <- TRUE %in% grepl(paste0('SD_GSNmap_',i), files)|grepl(paste0('sd_',i), files)
  t12 <- TRUE %in% grepl(paste0(ISO,'_GSNmap_',i), files)|grepl(paste0('mean_',i), files)
  
  t13 <- TRUE %in% grepl(ISO, files)
  
  Step1[Step1$property_id ==i, 'Names'] <- ifelse(t12[[1]] ==T & t13[[1]] ==T, 'Correctly named', 'Rename layer')
  Step1[Step1$property_id ==i, 'Uncertainty'] <- ifelse(t11[[1]] ==T , 'Generated', 'Missing')
  
}


# 2 - Step 2: Check the projection and resolution of all data products ---------
r <- rast(files)
names(r) <- names
# Check projection (WGS 84)
(Step21=crs(r, describe=TRUE)$name =='WGS 84')

# Check resolution (250 m)
(Step22=round(res(r)[[1]], 5) == 0.00225)

# 3 - Step 3: Check the extent -------------------------------------------------
# Check if the layers were masked with a cropland mask

mask <- rast('mask/mask.tif')
mask <- project(mask, r[[1]])

t <- r[[1]]
t <- ifel(!is.na(t),1, NA)

t3 <- sum(values(t, na.rm=T))-sum(values(mask, na.rm=T))

(Step3= t3 <=10)



# 4 - Step 4: Check the units, ranges, and outliers ----------------------------
# 4 - Step 4: Check the units, ranges, and outliers ----------------------------
Step4 <- data.frame()

for (i in unique(dt$property_id)){
  
  if (TRUE %in% grepl(i, names(r))){
    
    t41 <-min(values(r[[grepl(paste0('mean_',i), names(r))|grepl(paste0(ISO,'_GSNmap_',i), names(r))]],na.rm=T)) >=dt[dt$property_id == i, 'Min']
    t42 <-max(values(r[[grepl(paste0('mean_',i), names(r))|grepl(paste0(ISO,'_GSNmap_',i), names(r))]],na.rm=T)) <=dt[dt$property_id == i, 'Max']
    
    step<-data.frame(property_id =i,range= ifelse(t41[[1]] ==T & t42[[1]] ==T, 'Values in range', 'Values not in range'))
    Step4 <- rbind(Step4,step)
    
  }else if( !(TRUE%in% grepl(i, names(r)))){
    step<-data.frame(property_id =i,range= 'Layer not submitted')
    Step4 <- rbind(Step4,step)
    
    
  }
}


# 5 - Export QA/QC report ------------------------------------------------------
report <- merge(Step4, Step1, by=c('property_id'))

report$projection <- ifelse(Step21, 'WGS 84', 'Reproject layer')
report$resolution <- ifelse(Step21, '250 m', 'Resample layer')
report$extent <- ifelse(Step3, 'Croplands', 'Mask out layer')

report

write.csv(report, paste0('QA_QC_', ISO, '.csv'))
