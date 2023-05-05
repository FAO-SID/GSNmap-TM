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
# 3 - Select useful columns
# 4 - Quality check
# 5 - Estimate BD using pedotransfer function
# 6 - Harmonize soil layers
# 7 - Add chemical properties from additional dataset
# 8 - Plot and save results
#_______________________________________________________________________________

# 0 - User-defined variables ===================================================
#wd <- 'C:/Users/luottoi/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping'
#wd <- "C:/GIT/GSNmap-TM/Digital-Soil-Mapping"
wd <- 'C:/Users/hp/Documents/GitHub/GSNmap-TM/Digital-Soil-Mapping'

# 1 - Set working directory and load necessary packages ========================
setwd(wd) # change the path accordingly

library(tidyverse) # for data management and reshaping
library(readxl) # for importing excel files
library(mapview) # for seeing the profiles in a map
library(sf) # to manage spatial data (shp vectors) 
library(aqp) # for soil profile data
#install.packages("devtools") 
#devtools::install_bitbucket("brendo1001/ithir/pkg") #install ithir package
library(mpspline2) # for horizon harmonization
library(data.table)

# 2 - Import national data =====================================================
# Save your national soil dataset in the data folder /01-Data as a .csv file or 
# as a .xlsx file

## 2.1 - for .xlsx files -------------------------------------------------------
# Import horizon data 
# hor <- read_excel("01-Data/soil_data.xlsx", sheet = 2)
# # Import site-level data
# site <- read_excel("01-Data/soil_data.xlsx", sheet = 1)
# chem <- read_excel("01-Data/soil_data.xlsx", sheet = 2)
# phys <- read_excel("01-Data/soil_data.xlsx", sheet = 3)


## 2.2 - for .csv files --------------------------------------------------------
# Import horizon data 
hor <- read_csv(file = "01-Data/soil_profile_data.csv")
chem <- read_csv(file = "01-Data/soil_chem_data030.csv")
phys <- read_csv(file = "01-Data/soil_phys_data030.csv")


site <- select(hor, id_prof, x, y) %>% unique()


hor <- select(hor, id_prof, id_hor, top:cec)

# change names of key columns
names(site)
names(site)[1] <- "ProfID"
names(hor)
names(hor)[1] <- "ProfID"
names(hor)[2] <- "HorID"
# scan the data
summary(site)
summary(hor)

# 3 - select useful columns ====================================================
## 3.1 - select columns --------------------------------------------------------
hor <- select(hor, ProfID, HorID, top, bottom, ph=ph_h2o, k, soc, bd, cec)

# 4 - Quality check ============================================================

## 4.1 - Check locations -------------------------------------------------------
# https://epsg.io/6204
site %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% # convert to spatial object
  mapview(zcol = "ProfID", cex = 3, lwd = 0.1) # visualise in an interactive map

site_sub <- site[site$ProfID=='6524',]

site_sub %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% # convert to spatial object
  mapview(zcol = "ProfID", cex = 3, lwd = 0.1) # visualise in an interactive map
# profile 2823 is wrongly located, so let's remove it
site <- filter(site, ProfID != 2823)


## 4.2 - Convert data into a Soil Profile Collection ---------------------------
depths(hor) <- ProfID ~ top + bottom
hor@site$ProfID <- as.numeric(hor@site$ProfID)
site(hor) <- left_join(site(hor), site)
profiles <- hor

profiles

## 4.3 - plot first 20 profiles using pH as color ------------------------------
plotSPC(x = profiles[1:20], name = "pH", color = "ph",
        name.style = "center-center")


## 4.4 - check data integrity --------------------------------------------------
# A valid profile is TRUE if all of the following criteria are false:
#    + depthLogic : boolean, errors related to depth logic
#    + sameDepth : boolean, errors related to same top/bottom depths
#    + missingDepth : boolean, NA in top / bottom depths
#    + overlapOrGap : boolean, gaps or overlap in adjacent horizons
aqp::checkHzDepthLogic(profiles)

# visualize some of these profiles by the pid
subset(profiles, grepl(6566, ProfID, ignore.case = TRUE))
subset(profiles, grepl(7410 , ProfID, ignore.case = TRUE))
subset(profiles, grepl(8002, ProfID, ignore.case = TRUE))


## 4.5 - keep only valid profiles ----------------------------------------------
clean_prof <- HzDepthLogicSubset(profiles)
metadata(clean_prof)$removed.profiles
# write_rds(clean_prof, "01-Data/soilProfileCollection.rds")

## 4.6 convert soilProfileCollection to a table --------------------------------
dat <- left_join(clean_prof@site, clean_prof@horizons)
dat <- select(dat, ProfID, HorID, x, y, top, bottom, ph:cec )


# 5 - Estimate BD using pedotransfer functions =================================

# create the function with all PTF
estimateBD <- function(SOC=NULL, method=NULL) {
  OM <- SOC * 1.724
  BD <- switch(method,
               "Saini1996" = 1.62 - 0.06 * OM,
               "Drew1973" = 1 / (0.6268 + 0.0361 * OM),
               "Jeffrey1979" = 1.482 - 0.6786 * (log(OM)),
               "Grigal1989" = 0.669 + 0.941 * exp(1)^(-0.06 * OM),
               "Adams1973" = 100 / (OM / 0.244 + (100 - OM) / 2.65),
               "Honeyset_Ratkowsky1989" = 1 / (0.564 + 0.0556 * OM),
               stop("Invalid method specified.")
  )
  return(BD)
}

# 5.1 - Select a pedotransfer function ----------------------------------------
method_names <- c("Saini1996", "Drew1973", "Jeffrey1979", "Grigal1989", 
                  "Adams1973", "Honeyset_Ratkowsky1989")

# Create a test dataset with BD and SOC data
BD_test <- data.frame(SOC = dat$soc, BD_observed = dat$bd)

# Remove missing values
BD_test <- BD_test[complete.cases(BD_test),]

# 5.2 - Estimate BLD for a subset using the pedotransfer functions ------------
for (i in method_names) {
  BD_test[[i]] <- estimateBD(BD_test$SOC, method = i)
}

# Print the resulting data frame
BD_test


## 5.3 Compare results ---------------------------------------------------------
# Observed values:
summary(BD_test)

# Compare data distributions for observed and predicted BLD
BD_test %>% 
  select(-SOC) %>% 
  pivot_longer(cols = c("BD_observed", "Saini1996", "Drew1973", "Jeffrey1979",
                        "Grigal1989", "Adams1973", "Honeyset_Ratkowsky1989"), 
               names_to = "Method", values_to = "BD") %>% 
  ggplot(aes(x = BD, color = Method)) + 
  geom_density()


# Plot the Selected function again
BD_test %>% 
  select(-SOC) %>% 
  pivot_longer(cols = c("BD_observed", "Honeyset_Ratkowsky1989"), 
               names_to = "Method", values_to = "BD") %>% 
  ggplot(aes(x = BD, color = Method)) + 
  geom_density() + xlim(c(0,2.5))


## 5.4 Estimate BD for the missing horizons ------------------------------------
dat$bd[is.na(dat$bd)] <-
  estimateBD(dat[is.na(dat$bd),]$soc, method="Honeyset_Ratkowsky1989")

# Explore the results
summary(dat$bd)

g <- BD_test %>% 
  select(-SOC) %>% 
  pivot_longer(cols = c("BD_observed"), 
               names_to = "Method", values_to = "BD") %>% 
  ggplot(aes(x = BD, color = Method)) + 
  geom_density() +
  xlim(c(0,2.5))
g + geom_density(data = dat, aes(x=bd, color = "Predicted"))


## 5.5 - Explore outliers ------------------------------------------------------
# Outliers should be carefully explored and compared with literature values.
# Only if it is clear that outliers represent impossible or highly unlikely 
# values, they should be removed as errors.
# 
# Carbon content higher than 15% is only typical for organic soil (histosols)
# We will remove all atypically high SOC as outliers
summary(dat$soc)
na.omit(dat$ProfID[dat$soc > 10])
dat <- dat[dat$ProfID != 6915,]
dat <- dat[dat$ProfID != 7726,]

# Explore bulk density data, identify outliers
# remove layers with Bulk Density < 1 g/cm^3
low_bd_profiles <- na.omit(dat$ProfID[dat$bd<1])
dat <- dat[!(dat$ProfID %in% low_bd_profiles),]

# Explore data, identify outliers
x <- pivot_longer(dat, cols = ph:cec, values_to = "value",
                  names_to = "soil_property")
x <- na.omit(x)
ggplot(x, aes(x = soil_property, y = value, fill = soil_property)) +
  geom_boxplot() + 
  facet_wrap(~soil_property, scales = "free")


# 6 - Harmonize soil layers ====================================================
# Function to apply mpspline to a single soil property and depth range
apply_mpspline <- function(df, property, depth_range) {
  # Prepare input data for mpspline function
  input_data <- df[, c("ProfID", "top", "bottom", property)]
  colnames(input_data) <- c("SID", "UD", "LD", "VAL")
  
  # Apply mpspline function
  mpspline_result <- mpspline(obj = input_data, 
                              var_name = "VAL", 
                              d = depth_range,
                              vlow = min(input_data$VAL), 
                              vhigh = max(input_data$VAL))
  
  # Extract predicted values for the depth range
  predicted_values <- t(unique(as.data.frame(sapply(mpspline_result, function(x) x$est_dcm))))
  predicted_values <- as.data.frame(predicted_values)
  predicted_values$ProfID <- sub(x = rownames(predicted_values), pattern = "X", replacement = "")
  class(predicted_values$ProfID) <- class(df$ProfID)
  
  # Create a new dataframe with the interpolated values
  result_df <- unique(data.frame(ProfID = df$ProfID))
  result_df <- left_join(result_df, predicted_values)
  n <- length(depth_range)
  coln <- NULL
  for(layer in 2:n){
    coln <- append(coln, paste0(property,"_", depth_range[layer-1], "_", depth_range[layer]))
  }
  names(result_df)[-1] <- coln
  return(result_df)
}

# Function to apply mpspline to all soil properties and depth ranges
apply_mpspline_all <- function(df, properties, depth_range) {
  final_df <- data.frame(ProfID = df$ProfID)
  for (property in properties) {
    final_df <- left_join(final_df, apply_mpspline(df, property, depth_range))
  }
  final_df <- unique(final_df)
  return(final_df)
}

## 6.1 - Set target soil properties and depths ---------------------------------
names(dat)
dat <- select(dat, ProfID, HorID, x, y, top, bottom, ph, k, soc, bd, cec)

target <- c("ph", "k", "soc",  "bd", "cec")
depths <- c(0,30,60)

## 6.2 - Create standard layers ------------------------------------------------
d <- unique(select(dat, ProfID, x, y))


splines <- apply_mpspline_all(df = dat, 
                                   properties = target,
                                   depth_range = depths)
summary(splines)

# merge splines with x and y
d <- left_join(d, splines)



# 7 - Harmonize units ==========================================================
#Harmonize units if different from target units
# Mandatory Soil Propertes and corresponing units:
# Total N - ppm
# Available P - ppm
# Available K - ppm
# Cation exchange capacity cmolc/kg
# pH
# SOC - %
# Bulk density g/cm3
# Soil fractions (clay, silt and sand) - 

# Units soil profile data (dataframe d)
# 
head(d) # pH; K cmolc/kg; SOC %; BD g/cm3; CEC  cmolc/kg

# K => convert cmolc/kg to ppm (K *10 * 39.096)
d$k_0_30 <- d$k_0_30*10 * 39.096

head(chem)# P ppm; N %; K ppm
# N => convert % to ppm (N * 10000)
chem$tn <-chem$tn*10000

head(phys)# clay, sand, silt g/kg
# convert g/kg to % (/10)
phys$clay_0_30 <-phys$clay_0_30/10
phys$sand_0_30  <-phys$sand_0_30 /10
phys$silt_0_30 <-phys$silt_0_30/10


# Add chemical and physical properties from additional datasets ==========================
  

# Rename columns to match the main data set
names(d)
names(chem)[1] <- 'ProfID'
names(chem)[4] <- 'p_0_30'
names(chem)[5] <- 'k_0_30' 
names(chem)[6] <- 'n_0_30'


#The chem dataframe comes from and independent dataset we need to create new unique ProfIDs 
#Create unique ProfID 
chem$ProfID <- seq(max(d$ProfID)+1,max(d$ProfID)+1+nrow(chem)-1)

# Add the new data as new rows using data.table we can add empty rows
# automatically for the not measured properties in the chem dataset
d <- rbind(setDT(d),setDT(chem), fill = TRUE)

#The phys dataframe with the texture instead shares the same ProfIDs (we can directly merge)
d <- merge(d, phys, by=c('ProfID', 'x', 'y'), all.x =T)

# 8 - Plot  and save results ===================================================
names(d)
x <- pivot_longer(d, cols = ph_0_30:silt_0_30, values_to = "value",
                  names_sep = "_", 
                  names_to = c("soil_property", "top", "bottom"))
x <- mutate(x, depth = paste(top, "-" , bottom))
#x <- na.omit(x)
ggplot(x, aes(x = depth, y = value, fill = soil_property)) +
  geom_boxplot() + 
  facet_wrap(~soil_property, scales = "free")

# save the data
write_csv(d, "02-Outputs/harmonized_soil_data.csv")
