# Introduction to R

# 0. Playground ================================================================

# learn important keyboard shortcuts
# Ctrl + enter for running code
# tab after writing the first three characters of the function name
# F1 to access the help

# explore the use of <-, $, [], ==, !=, c(), :, data.frame(), list(), as.factor()



# 1. Set working directory ===================================================== 
setwd()

# 2. Install and load packages =================================================
# readxl, tidyverse, and data.table packages using the functions
install.packages("tidyverse")

library(tidyverse)

# 3. Import an spreadsheet =====================================================
## 3.1 Read the MS Excel file --------------------------------------------------
#Read the soil_data.xlsx file, spreadsheet 2, using read_excel() 
read_excel(path = , sheet = )

## 3.2 Read the csv file with the native function ------------------------------
# 01-Data/horizon.csv
read.csv()

## 3.3 Read the csv file with the tidyverse function ---------------------------
read_

## 3.4 Read the csv file with the data.table package ---------------------------


## 3.5 Assign the dataframe to an object called dat ----------------------------
dat <- 

# 4. Tidyverse functions =======================================================
## 4.1 Select variables --------------------------------------------------------
# pid, hip, top, bottom, ph_h2o, clay from dat 
# save the result as dat_1



## 4.2 Filter: pick observations by their values -------------------------------
# filter observations with clay > 50%



## 4.3 Mutate: create a new variable -------------------------------------------
# thickness = top - bottom


## 4.4 Group_by and summarise --------------------------------------------------
# group by variable pid
# summarise taking the mean of pH and clay



## 4.5 Reshape the table using pivot_longer ------------------------------------
# use dat_3
# put the names of the variables ph_h2o, clay and thickness in the column 
# variable and keep the rest of the table. Save in dat_5 


## 4.6 Join the table sites.csv with dat_3 -------------------------------------
# Load site.csv (in 01-Data folder) 
# Join its columns with dat_3 keeping all the rows of dat_3
# save the result as dat_6



# 5. Data visualization with ggplot2 ===========================================
## 5.1 1D plot: histograms -----------------------------------------------------
# histograms of clay and ph_h2o



## 5.2 2D plot: scatterplot ----------------------------------------------------
# Scatterplot bottom vs. ph_h2o



# add a fitting line


## 5.3 3D plot: scatterplot ----------------------------------------------------
# Scatterplot bottom vs. ph_h2o, add clay as color and size inside the 
# function aes()


## 5.4 2D plot + facets --------------------------------------------------------
# Make a histogram of pH for each year
# use dat_6


# 6. Geospatial data with terra ================================================
## Load packages (install them if needed)


## 6.1 Load a raster and a vector layer ----------------------------------------
# Load 01-Data/covs/grass.tif using rast() function, then plot it
# Load 01-Data/soil map/SoilTypes.shp using vect() function and plot it 
# explore the attributes of these layers



## 6.2 Load a raster and a vector layer ----------------------------------------
# Check the current CRS (EPSG) of the raster and the vector. 
# Find a *projected* CRS in http://epsg.io for Macedonia and copy the number
# Check the Arguments of function project (?project) that need to be defined
# Save the new object as r_proj and v_proj
# plot both objects



## 6.3 Cropping and masking a raster -------------------------------------------
# Compute the area of the polygons in v_proj (search for a function) and
# assign the values to a new column named area
# select the largest polygon using [], $, == and max() func. and save it as pol
# crop the raster with pol using the crop() function and save it as r_pol
# mask the raster r_pol with the polygon pol and save it with the same name
# plot each result






## 6.4 Replace values in a raster by filtering their cells ---------------------
# Explore the following link to understand how terra manage cell values
# https://rspatial.org/terra/pkg/4-algebra.html 
# Replace values lower than 5 in r+pol by 0




## 6.5 Rasterize a vector layer ------------------------------------------------
# Use rasterize() function to convert v_proj to raster
# Use r_proj as reference raster
# Use field Symbol to assign cell values, and plot the new map





## 6.6 Extracting raster values using points -----------------------------------
# Covert dat_6 to spatial points using vect() function (check help of vect())
# Note that the EPSG number is 6204
# Save the points as s
# Plot s and r_proj together in the same map (Argument add=TRUE)
# Extract the values of the raster using extract() function (check the help)
# Remove the ID column of the extracted values
# merge the extracted data with s using cbind() function
# Convert s as a dataframe








## 6.7 Zonal statistics using polygons and rasters -----------------------------
# Use the extract() func. to estimate the mean value of r_proj at each polygon
# Use the fun= argument (check the help)
# Use the cbind() func. to merge v_proj and the extracted values
# convert v_proj to a dataframe
# Create a ggplot boxplot (geom_boxplot) with x=Symbol and y=grass 






## 6.8 Bonus track: use zonal() function ---------------------------------------


