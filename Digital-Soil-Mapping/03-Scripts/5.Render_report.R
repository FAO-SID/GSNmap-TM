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
# 0 - Setup and user-defined variables
# 1 -  User-defined variables
# 2 - Render the .Rmd file to generate an automated report as a docx
#_______________________________________________________________________________


# 0 - Initial Setup ============================================================


#install.packages("rmarkdown")
#tinytex::install_tinytex()
# library(sf)
# library(ggplot2)
# library(tidyverse)
# library(terra)
library(knitr)
# library(tidyterra)
# library(patchwork)

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# 1 -  User-defined variables ==================================================

# Specify three-digit ISO code for your country
ISO <- 'AOI'

# Specify the properties you mapped (the code assumes harmonized naming)
# using the output data frame from script 2
dxy <- read.csv("02-Outputs/harmonized_soil_data.csv")

target_properties <- names(dxy)[ !(names(dxy)%in% c("ProfID", "x" ,"y"))]

#target_properties<-c('ph_0_30')

# Map background file
bckg<-vect('01-Data/AOI.shp')

#Adjust figure width and height of the map plot

figw <- 12
figh <-8


# Specify where you want your word document to be saved
output_file = paste0("National Report/Report_GSNmap_",ISO,".docx")

# 2 - Render the .Rmd file to generate an automated report as a docx ===========
path = 'National Report/National GSNmap Report.Rmd'
output_format = 'word_document'
rmarkdown::render(path, output_format, output_file)
