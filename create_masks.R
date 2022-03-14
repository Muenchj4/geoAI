#ex create masks
require(envimaR)
source(file.path(envimaR::alternativeEnvi(root_folder = "C:/Users/jomue/edu/geoAI",
                                          alt_env_id = "COMPUTERNAME",
                                          alt_env_value = "PCRZP",
                                          alt_env_root_folder = "F:/BEN/edu/geoAI"),
                 "src/000-rspatial-setup.R"))

# read data
ras <- raster::stack(file.path(envrmt$path_data, "/marburg_dop.tif"))

# subset to three channels
ras <- subset(ras, c("red", "green", "blue"))
plot(ras)
mapview(ras)

# download and crop the OSM building data to the extent of the raster data
buildings = opq(bbox = "marburg de") %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

buildings = buildings$osm_polygons

buildings = sf::st_transform(buildings, proj4string(ras))

ras_extent <- c(xmin=480000,xmax=484000,ymin=5626000,ymax=5628000)

mapview(buildings)
buildings <- sf::st_crop(buildings[1], ras_extent)
# rasterize the buildings
rasterized_vector <- rasterize(buildings, ras[[1]])

# reclassify to 0 and 1
rasterized_vector[is.na(rasterized_vector[])] <- 0
rasterized_vector[rasterized_vector > 1] <- 1

#save
raster::writeRaster(rasterized_vector,
                    file.path(envrmt$path_data, "/marburg_mask.tif"),
                    overwrite = T)
mapview(rasterized_vector)
extent(rasterized_vector)<-c(xmin=480000,xmax=484000,ymin=5626000,ymax=5628000)
ext<-extent(rasterized_vector)
e_test <- c(xmin=483000, xmax=484000, ymin=5626000, ymax=5628000)
e_train <- c(xmin=480000, xmax=483000, ymin=5626000, ymax=5628000)

marburg_mask_train <- crop(rasterized_vector,e_train)
marburg_dop_train <- crop(ras, e_train)

marburg_mask_test <-  crop(rasterized_vector, e_test)
marburg_dop_test <- crop(ras, e_test)

writeRaster(
  marburg_mask_test,
  file.path(envrmt$path_model_testing_data, "marburg_mask_test.tif"),
  overwrite = T
)
writeRaster(
  marburg_dop_test,
  file.path(envrmt$path_model_testing_data, "marburg_dop_test.tif"),
  overwrite = T
)
writeRaster(
  marburg_mask_train,
  file.path(envrmt$path_model_training_data, "marburg_mask_train.tif"),
  overwrite = T
)
writeRaster(
  marburg_dop_train,
  file.path(envrmt$path_model_training_data, "marburg_dop_train.tif"),
  overwrite = T
)