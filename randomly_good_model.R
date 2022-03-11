#randomly_good_model.R
# by Joachim Muench
library(envimaR)
source(file.path(envimaR::alternativeEnvi(root_folder = "C:/Users/jomue/edu/geoAI",
                                          alt_env_id = "COMPUTERNAME",
                                          alt_env_value = "PCRZP",
                                          alt_env_root_folder = "F:/BEN/edu/geoAI"),
                 "src/000-rspatial-setup.R"))


library(osmdata)
# loading OSM data for the Marburg region 
buildings = opq(bbox = "marburg de") %>% 
  add_osm_feature(key = "building") %>% 
  osmdata_sf()
buildings = buildings$osm_polygons

mapview::mapview(buildings)
# 2 - define variables
# creating a raster stack
stack=raster::stack(paste0(envrmt$path_doc,"/marburg_dop.tif"))
# 2 - check CRS and other info
#-----------------------#
raster::crs(stack)
raster::crs(buildings)
raster::compareCRS(stack, buildings)
buildings = sf::st_transform(buildings, crs(stack))
crs(buildings)
# 3 - visualize the data ####
#-----------------------#
# simple RGB plot
raster::plotRGB(stack)

# interactive plot
mapview(stack)
# 4 - calculate RGB indices ####
# We can use raster as simple calculator
# First, we assign the three first layers in the raster image to variables
# called - surprise - red, green and blue (this is to keep it simple and clear)
#-----------------------#
red   <- stack[[1]]
green <- stack[[2]]
blue  <- stack[[3]]
# Then we calculate all of the indices we need or want

## Normalized difference turbidity index (NDTI)
NDTI <- (red - green) / (red + green)
names(NDTI) <- "NDTI"
## Visible Atmospherically Resistant Index (VARI)
VARI <- (green - red) / (green + red - blue)
names(VARI) <- "VARI"
## Triangular greenness index (TGI)
TGI <- -0.5*(190*(red - green)- 120*(red - blue))
names(TGI) <- "TGI"
rgbI <- raster::stack(NDTI, VARI, TGI)
raster::plot(rgbI)
# 5 - stack and save as RDS ####
#-----------------------#
marburg_stack <- stack(stack, rgbI)
saveRDS(marburg_stack, (file.path(envrmt$path_data, "dop_indices.RDS")))
