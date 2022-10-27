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

# GEE account ------------------------------------------------------------------

## you need a GEE account
## log in the https://code.earthengine.google.com/ and register for one


# installing conda environment -------------------------------------------------
install.packages("geojsonio")
install.packages("cli")
install.packages("openssl")
install.packages("stars")

#Install rgee
remotes::install_github("r-spatial/rgee")
library(rgee)

# Set up rgee environment 
ee_install(py_env = "rgee") 

# Check if you can initialize rgee for the first time
library(rgee)
ee_Initialize() 

# If not, follow next steps: 
# Open Anaconda Prompt and execute the following code, one by one:
# 1. conda create -n rgee_py python=3.9
# 2. activate rgee_py
# 3. pip install google-api-python-client
# 4. pip install earthengine-api
# 5. pip install numpy
# 6. conda env list

# Copy the path of the rgee_py environment and write it correctly 
# (note the backslash \)
# rgee_py            *  C:\Users\angel\MINICO~1\envs\rgee_py <<<
rgee_environment_dir = "C:/Users/angel/miniconda3/envs/rgee_py/"

reticulate::use_python(rgee_environment_dir, required=TRUE)
library(reticulate)
Sys.setenv(RETICULATE_PYTHON=paste0(rgee_environment_dir,"python.exe"))
# Install the environment. Select 'Yes' to restart your R session
rgee::ee_install_set_pyenv(
  py_path = rgee_environment_dir,
  py_env = "rgee_py" 
)
library(reticulate)
library(rgee)
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)

# 1. Run next function and follow the steps in your browser (e.g. Chrome)
# 2. Check the box to give GEE access to your Google Drive folders. 
# 3. Give rgee access to your account
# 3. When it finish, come back to RStudio
ee_Initialize(drive = T)

# If there is no error message, then that is all.
# If you need to remove the credentials to repeat the previous step, use
# ee_clean_credentials()