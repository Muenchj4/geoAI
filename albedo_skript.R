#------------------------------------------------------------------------------
# Type: script
# Name: sentinel_albedo.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  retrieves sentinel data 
#               and exemplary defines AOI and calculates albedo
# Dependencies: geoAI.R  
# Output: original sentinel tile 
#         AOI window of this tile (research_area)
#         top of atmosphere albedo AOI
#         surface albedo AOI
# Copyright: Chris Reudenbach 2021, GPL (>= 3)
# git clone https://github.com/gisma-courses/courses-scripts/get-sentinel.git
#------------------------------------------------------------------------------

# 0 - specific setup
#-----------------------------
library(envimaR)
source(file.path(envimaR::alternativeEnvi(root_folder = "C:/Users/jomue/edu/geoAI",
                                          alt_env_id = "COMPUTERNAME",
                                          alt_env_value = "PCRZP",
                                          alt_env_root_folder = "F:/BEN/edu/geoAI"),
                 "src/000-rspatial-setup.R"))
appendpackagesToLoad = c("rprojroot","sen2r","terra")

# add define project specific subfolders
appendProjectDirList = c("data/sentinel/",
                         "data/vector_data/",
                         "data/sentinel/S2/",
                         "data/sentinel/SAFE/",
                         "data/sentinel/research_area/")


# 2 - define variables
#---------------------
# define area to define which tile(s) of Sentinel are necessary 
# e.g. by existing data
 myextent  = sf::st_read(file.path(envrmt$path_data,"/vector_data/shap.shp")) 



# or from the extent of the rasterstack
envrmt
rasterStack = raster::stack(file.path(envrmt$research_area, "/mof.json"))
myextent = rasterStack


# setup sentinel retrieval object
out_paths_1 <- sen2r::sen2r(
  gui = FALSE,
  step_atmcorr = "l2a",
  online = TRUE,
  extent = myextent,
  extent_name = "MOF",
  timewindow = c(as.Date("2021-06-12"), as.Date("2021-06-14")),
  list_prods = c("BOA","SCL"),
  list_indices = c("NDVI","MSAVI2"),
  list_rgb = c("RGB432B"),
  mask_type = "cloud_and_shadow",
  max_mask = 10,
  path_l2a = envrmt$path_SAFE, # folder to store downloaded SAFE
  server = "scihub",
  preprocess = TRUE,
  parallel = TRUE,
  sen2cor_use_dem = TRUE,
  max_cloud_safe = 10,
  overwrite = TRUE,
  path_out =  envrmt$path_research_area # folder to store downloaded research arec cutoff
)

# subsetting the filename(s) of the interesting file(s)
fn_noext=xfun::sans_ext(basename(list.files(paste0(envrmt$path_research_area,"/BOA/"),pattern = "S2B2A")))
fn = basename(list.files(paste0(envrmt$path_research_area,"/BOA/"),pattern = "S2B2A"))

# creating a raster stack
stack=raster::stack(paste0(envrmt$path_research_area,"/BOA/",fn))

# now  simply calculate the surface albedo following an approch provided
# an adapted regression function of the package 'agriwater'

b2 <- stack[[2]]/10000
b3 <- stack[[3]]/10000
b4 <- stack[[4]]/10000
b8 <- stack[[8]]/10000

alb_top = b2 * 0.32 + b3 * 0.26 + b4 * 0.25 + b8 * 0.17
alb_surface = 0.6054 * alb_top + 0.0797

plot(alb_top)
plot(alb_surface)
