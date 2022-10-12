#
# Digital Soil Mapping
# 
# Install rgee
#
# GSP-Secretariat
# Contact: Isabel.Luotto@fao.org
#          Marcos.Angelini@fao.org
#_______________________________________________________________________________

#Empty environment and cache 
rm(list = ls())
gc()

# GEE account -------------------------------------------------------------

## you need a GEE account
## log in the https://code.earthengine.google.com/ and register for one


# installing conda environment --------------------------------------------------------------------

## the conda environment is where the GEE Python API will be located. The RGEE package uses it.
## first you need to install the Miniconda OUTSIDE of R
## install Miniconda3 at https://docs.conda.io/en/latest/miniconda.html

#Install rgee
remotes::install_github("r-spatial/rgee")
library(rgee)

# Set up rgee environment 
ee_install(py_env = "rgee") # It is just necessary once!1
#ee_reattach() # reattach ee as a reserve word
# Initialize just Earth Engine

#Initialize rgee for the first time
library(rgee)
ee_Initialize() 