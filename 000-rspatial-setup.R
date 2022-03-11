#------------------------------------------------------------------------------
# basic rspatial course setup script
# Type: script
# Name: 000-rspatial-setup.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  create/read project folder structure and returns pathes as list
#               load all necessary packages 
#               sources all functions in a defined function folder
# Dependencies: devtools::install_github("envima/envimaR")
# Output: list containing the folder strings as shortcuts
# Copyright: Reudenbach/Nauss 2015-2021, GPL (>= 3)
# Details: Please note that the queries on standard arguments have been set for 
#          pragmatic reasons for a typical course setting. Of course, it is 
#          intended to assign the arguments directly in the createEnvi function
# Date: 2021-12-12
#------------------------------------------------------------------------------
require(envimaR)

# 1 - source files
#-----------------

source(file.path(envimaR::alternativeEnvi(root_folder = "C:/Users/jomue/edu/geoAI",
                                          alt_env_id = "COMPUTERNAME",
                                          alt_env_value = "PCRZP",
                                          alt_env_root_folder = "F:/BEN/edu/geoAI"),
                 "src/geoAI_setup.R"))

# rspatial packages
packagesToLoad = c("rgdal","png","tensorflow","keras","reticulate","osmdata","greenbrown","rsample","tfdatasets","purrr","magick","rprojroot","sen2r","patchwork","ggplot2","rpart","rpart.plot","rasterVis", "forcats","e1071","mapview", "mapedit", "tmap", "raster", "terra", "stars", "gdalcubes", "sf","dplyr","tidyverse","RStoolbox",
                   "randomForest", "ranger", "e1071", "caret", "link2GI","gdalUtils","raster","exactextractr", "doParallel", "CAST","ranger")


# mandantory folder structure
projectDirList   = c("data/",
                     "data/modelling/",
                     "data/modelling/model_training_data/",
                     "data/modelling/model_training_data/dop/",
                     "data/modelling/model_training_data/bui/",
                     "data/modelling/models/",
                     "data/modelling/prediction/",
                     "data/modelling/validation/",
                     "data/modelling/model_testing_data/",
                     "data/modelling/model_testing_data/dop/",
                     "data/modelling/model_testing_data/bui/",
                     "docs/",
                     "run/",
                     "tmp",
                     "src/",
                     "src/functions/"
)

# append additional packages if defined by calling script

if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") 
{
  projectDirList = append(projectDirList,appendProjectDirList)
}

if (exists("appendpackagesToLoad") && appendpackagesToLoad[[1]] != "") 
{
  packagesToLoad = append(packagesToLoad,appendpackagesToLoad)
}

# Now create/read root direcory, folder structure and load packages
# NOTE root_folder MUST be defined in calling script
if (!exists("root_folder")) {
  message("variable root_folder is NOT defined by calling script...\ 'root_folder' is set as rootDIR")
  root_folder=rootDIR
}
if (!exists("alt_env_id")) {
  message("variable alt_env_id is NOT defined by calling script...\n 'COMPUTERNAME' is set as default\n")
  alt_env_id = "COMPUTERNAME"
}
if (!exists("alt_env_value")) {
  message("variable alt_env_value is NOT defined by calling script...\n 'PCRZP' is set as default\n")
  alt_env_value = "PCRZP"
}
if (!exists("alt_env_root_folder")) {
  message("variable alt_env_root_folder is NOT defined by calling script...\n 'F:/BEN/edu' is set as default\n")
  alt_env_root_folder = "F:/BEN/edu"
}

if (!exists("path_prefix")) {
  message("variable  path_prefix is NOT defined by calling script...\n 'path_' is set as default\n")
  path_prefix =  "path_"
}

root_folder = envimaR::alternativeEnvi(root_folder = root_folder,
                                       alt_env_id = alt_env_id,
                                       alt_env_value = alt_env_value,
                                       alt_env_root_folder = alt_env_root_folder)


if (!exists("fcts_folder")) {
  message("variable fcts_folder is NOT defined by calling script...\n 'src/functions/' is set as default\n")
  fcts_folder =  paste0(root_folder,"/src/functions/")
}

# append additional folders if defined by calling script
if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") 
{
  projectDirList = append(projectDirList,appendProjectDirList)
}

# call central function
envrmt = envimaR::createEnvi(root_folder = root_folder,
                             folders = projectDirList,
                             fcts_folder = fcts_folder,
                             path_prefix = path_prefix,
                             libs = packagesToLoad,
                             setup_script = "000-rspatial-setup.R",
                             source_functions = TRUE,
                             alt_env_id = alt_env_id,
                             alt_env_value = alt_env_value,
                             alt_env_root_folder = alt_env_root_folder)

## set temp path to speed up raster package operations

raster::rasterOptions(tmpdir = envrmt$path_tmp)
# suppres gdal warnings
rgdal::set_thin_PROJ6_warnings(TRUE)

# define some color palettes
mvTop = mapview::mapviewPalette("mapviewTopoColors")
mvSpec = mapview::mapviewPalette("mapviewSpectralColors")
mvVec =	 mapview::mapviewPalette("mapviewVectorColors") 
mvRas =	 mapview::mapviewPalette("mapviewRasterColors")
