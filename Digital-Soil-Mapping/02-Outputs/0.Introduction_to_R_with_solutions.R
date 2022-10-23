# Introduction to R

# 0. Playground ================================================================

# learn important keyboard shortcuts
# Ctrl + enter for running code
# tab after writing the first three characters of the function name
# F1 to access the help

# explore the use of <-, $, [], ==, !=, c(), :, data.frame(), list(), as.factor()

a <- 10:15
a[2]
a[2:3]
b <- c("1", "a", a )
length(b)
df <- data.frame(column_a = 1:8, column_b = b)

df[,1]
df$column_b
as.numeric(df$column_b)
plot(df)

df[1:3,]
df[,1]

as.factor(b)

d <- list(a, b, df)
d
names(d)
names(d) <- c("numeric_vector", "character_vector", "dataframe")
d
d[[1]]
d$numeric_vector

a == b
a != b

# 1. Set working directory ===================================================== 
setwd("C:/GIT/Digital-Soil-Mapping/")

# 2. Install and load packages =================================================
# readxl, tidyverse, and data.table packages using the functions
install.packages("tidyverse")
install.packages("readxl")
install.packages("data.table")
library(tidyverse)
library(readxl)
library(data.table)

# 3. Import an spreadsheet =====================================================
## 3.1 Read the MS Excel file --------------------------------------------------
#Read the soil_data.xlsx file, spreadsheet 2, using read_excel 
read_excel(path = "01-Data/soil_data.xlsx", sheet = 2)

## 3.2 Read the csv file with the native function ------------------------------
# 01-Data/horizon.csv
read.csv("01-Data/horizon.csv")

## 3.3 Read the csv file with the tidyverse function ---------------------------
read_csv("01-Data/horizon.csv")

## 3.4 Read the csv file with the data.table function --------------------------
fread("01-Data/horizon.csv")

## 3.5 Assign the dataframe to an object called dat ----------------------------
dat <- read_csv("01-Data/horizon.csv")

# 4. Tidyverse functions =======================================================
## 4.1 Select pid, hip, top, bottom, ph_h2o, clay from dat ---------------------
dat_1 <- dat %>% 
  select(pid, hid, top, bottom, ph_h2o, clay)

## 4.2 Filter: pick observations by their values -------------------------------
# filter observations with clay > 50%

dat_2 <- dat_1 %>% 
  filter(clay > 50)

dat_2
## 4.3 Mutate: create a new variable -------------------------------------------
# thickness = top - bottom

dat_3 <- dat_2 %>% 
  mutate(thickness = bottom - top)

## 4.4 Group_by and summarise --------------------------------------------------
# group by variable pid
# summarise taking the mean of pH and clay

dat_4 <- dat_3 %>% 
  group_by(pid) %>% 
  summarise(mean_ph = mean(ph_h2o),
            mean_clay = mean(clay))

## 4.5 Reshape the table using pivot_longer ------------------------------------
# use dat_3
# put the names of the variables ph_h2o, clay and thickness in the column 
# variable and keep the rest of the table. Save in dat_5 

dat_5 <- dat_3 %>% 
  pivot_longer(ph_h2o:thickness, names_to = "soil_property", values_to = "value")

## 4.6 Join the table sites.csv with dat_3 -------------------------------------
# Load site.csv (in 01-Data folder) 
# Join its columns with dat_3 keeping all the rows of dat_3
# save the result as dat_6

sites <- read_csv("01-Data/site.csv") 
dat_6 <- dat_3 %>% 
  left_join(sites)
# or
dat_6 <- sites %>% 
  right_join(dat_3)

# 5. Data visualization with ggplot2 ===========================================
## 5.1 1D plot: histograms -----------------------------------------------------
# histograms of clay and ph_h2o

ggplot(dat_3, aes(x=clay)) + geom_histogram()

## 5.2 2D plot: scatterplot ----------------------------------------------------
# Scatterplot bottom vs. ph_h2o

ggplot(dat_3, aes(x = bottom, y = ph_h2o)) + 
  geom_point() 

# add a fitting line

ggplot(dat_3, aes(x = bottom, y = ph_h2o)) + 
  geom_point() +
  geom_smooth(method = "lm" )

## 5.3 3D plot: scatterplot ----------------------------------------------------
# Scatterplot bottom vs. ph_h2o, add clay as color and size inside the 
# function aes()

ggplot(dat_3, aes(x = bottom, y = ph_h2o, color = clay, size = clay)) + 
  geom_point() 

## 5.4 2D plot + facets --------------------------------------------------------
# Make a histogram of pH for each year
# use dat_6

ggplot(dat_6, aes(x = ph_h2o, fill = year)) +
  geom_histogram()+
  facet_wrap(~year)

# 6. Geospatial data with terra ================================================
## Load packages (install them if needed)
library(terra)
## 6.1 Load a raster and a vector layer ----------------------------------------
# Load 01-Data/covs/grass.tif using rast() function, then plot it
# Load 01-Data/soil map/SoilTypes.shp using vect() function and plot it 
# explore the attributes of these layers

r <- rast("01-Data/covs/grass.tif")
plot(r)

v <- vect("01-Data/soil map/SoilTypes.shp")
plot(v)

## 6.2 Load a raster and a vector layer ----------------------------------------
# Check the current CRS (EPSG) of the raster and the vector. 
# Find a *projected* CRS in http://epsg.io for Macedonia and copy the number
# Check the Arguments of function project (?project) that need to be defined
# Save the new object as r_proj and v_proj
# plot both objects

r_proj <- project(x = r, y = "epsg:6204", method = "bilinear", res = 250)
plot(r_proj)
v_proj <- project(x = v, y = "epsg:6204")
plot(v_proj, add = TRUE)

## 6.3 Cropping and masking a raster -------------------------------------------
# Compute the area of the polygons in v_proj (search for a function) and
# assign the values to a new column named area
# select the largest polygon using [], $, == and max() func. and save it as pol
# crop the raster with pol using the crop() function and save it as r_pol
# mask the raster r_pol with the polygon pol and save it with the same name
# plot each result

v_proj$area <- expanse(v_proj, unit = "ha")
pol <- v_proj[v_proj$area == max(v_proj$area)]
plot(pol)
r_pol <- crop(r_proj, pol)
plot(r_pol)
plot(pol, add = TRUE)
r_pol <- mask(r_pol, pol)
plot(r_pol)

## 6.4 Replace values in a raster by filtering their cells ---------------------
# Explore the following link to understand how terra manage cell values
# https://rspatial.org/terra/pkg/4-algebra.html 
# Replace values lower than 5 in r+pol by 0

r_pol[r_pol$grass < 5] <- 0
plot(r_pol)

## 6.5 Rasterize a vector layer ------------------------------------------------
# Use rasterize() function to convert v_proj to raster
# Use r_proj as reference raster
# Use field Symbol to assign cell values, and plot the new map

v_class <- rasterize(x = v_proj, y = r_proj, field = "Symbol" )
plot(v_class)
v_class
activeCat(v_class) <- 1

## 6.6 Extracting raster values using points -----------------------------------
# Covert dat_6 to spatial points using vect() function (check help of vect())
# Note that the EPSG number is 6204
# Save the points as s
# Plot s and r_proj together in the same map (Argument add=TRUE)
# Extract the values of the raster using extract() function (check the help)
# Remove the ID column of the extracted values
# merge the extracted data with s using cbind() function
# Convert s as a dataframe

s <- vect(dat_6, geom=c("x", "y"), crs = "epsg:6204")
plot(r_proj)
plot(s, add=TRUE)
x <- extract(r_proj,s, ID=FALSE)
s <- cbind(s,x)
d <- as.data.frame(s)
d
GGally::ggscatmat(d)

## 6.7 Zonal statistics using polygons and rasters -----------------------------
# Use the extract() func. to estimate the mean value of r_proj at each polygon
# Use the fun= argument (check the help)
# Use the cbind() func. to merge v_proj and the extracted values
# convert v_proj to a dataframe
# Create a ggplot boxplot (geom_boxplot) with x=Symbol and y=grass 

x <- extract(r_proj, v_proj, fun = mean, ID=FALSE)
v_proj <- cbind(v_proj, x)

d <- as_tibble(v_proj)

d %>% 
  ggplot(aes(x =Symbol, y = grass, fill = Symbol)) +
  geom_boxplot() +
  ylab("Grass probability")

## 6.8 Bonus track: use zonal() function ---------------------------------------

zonal(r_proj, v_class, na.rm=T)