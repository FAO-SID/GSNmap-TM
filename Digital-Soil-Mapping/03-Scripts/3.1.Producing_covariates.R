# Create terrain derivatives from MERIT-DEM and 
# harmonise their format with the rest of the covariates 
# Marcos Angelini, Luis Rodríguez Lado & Isabel Luotto
# Global Soil Partnership - FAO
rm(list = ls())
setwd("C:/GIT/GSNmap-TM/Digital-Soil-Mapping/")

# Load libraries
  library(terra)
  library(raster)

# import high resolution raster  
dem <- rast("01-Data/create_covariates/MERIT_DEM.tif")

# Plot DEM
plot(dem)

# Obtain DEM derived maps
derived_vars <- terrain(dem, 
                        c("slope", "roughness", "aspect", "flowdir", 
                          "aspect","TPI", "TRI", "TRIriley", "TRIrmsd"), 
                        unit = "degrees")

plot(derived_vars, nc=3, nr=3)

# load rster of reference
ref <- rast("01-Data/covs/Covariates.tif")[[1]]

derived_vars
ref
# transform the variable's coordinates, resolution and extent 
resampled_vars <- project(derived_vars, ref)
# mask variables
resampled_vars <- mask(resampled_vars, ref)
plot(resampled_vars)

resampled_vars
ref
# save the rasters
writeRaster(resampled_vars, "01-Data/covs/dem_derivatives.tif")

# # Load a vector layer (e.g.a soil map) 
# v <- vect("01-Data/AOI.shp")
# plot(v, "shape_Area")
# # Vector rasterization with continuous data
# x <- rasterize(x = v, y = ref, field = "shape_Area", fun = "max")
# plot(x)
# y <- rasterize(x = v, y = ref, field = "CABECERA")
# 
# writeRaster(y, "01-Data/create_covariates/polygon_map.tif")
