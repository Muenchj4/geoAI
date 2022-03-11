# Type: script
# Name: hands-on.R
# Author: Chris Reudenbach, creuden@gmail.com
# Description:  retrieves sentinel data 
#               defines AOI and performs classification tasks
# Dependencies: geoAI.R  
# Output: original sentinel tile 
#         AOI window of this tile (research_area)
#         unsupervised classification with kmeans (via RStoolbox)
#         supervised classification recursive partitioning and regression trees (via rpart)
#         random forest (via caret) 
#         superclust (automated random forest via RStoolbox)# Copyright: Chris Reudenbach 2021, GPL (>= 3)
# git clone https://github.com/gisma-courses/courses-scripts/geoAI.git
#------------------------------------------------------------------------------

# 0 - specific setup
#-----------------------------
library(envimaR)
source(file.path(envimaR::alternativeEnvi(root_folder = "C:/Users/jomue/edu/geoAI",
                                          alt_env_id = "COMPUTERNAME",
                                          alt_env_value = "PCRZP",
                                          alt_env_root_folder = "F:/BEN/edu/geoAI"),
                 "src/000-rspatial-setup.R"))


# add define project specific subfolders
appendProjectDirList = c("data/sentinel/",
                         "data/vector_data/",
                         "data/sentinel/S2/",
                         "data/sentinel/SAFE/",
                         "data/sentinel/research_area/")


       
       
       # 2 - define variables
       #---------------------
       
       # subsetting the filename(s) of the interesting file(s)
       fn_noext=xfun::sans_ext(basename(list.files(paste0(envrmt$path_doc,"/BOA/"),pattern = "S2B2A")))
       fn = basename(list.files(paste0(envrmt$path_doc,"/BOA/"),pattern = "S2B2A"))
       
       # creating a raster stack
       stack=raster::stack(paste0(envrmt$path_doc,"/BOA/",fn))
       # sentinel true and false color composite 
       mapview::viewRGB(stack, r = 4, g = 3, b = 2) + mapview::viewRGB(stack, r = 8, g = 4, b = 3)     

       train_area <- mapview::viewRGB(stack, r = 8, g = 4, b = 3) %>% mapedit::editMap()       
       # add class (text) and id (integer number)
       forest <- train_area$finished$geometry %>% st_sf() %>% mutate(class = "forest", id = 1)
       # fields only
       train_area <- mapview::viewRGB(stack, r = 4, g = 3, b = 2) %>% mapedit::editMap()       
       fields <- train_area$finished$geometry %>% st_sf() %>% mutate(class = "fields", id = 2)     
       # meadows only
       train_area <- mapview::viewRGB(stack, r = 4, g = 3, b = 2) %>% mapedit::editMap()       
       meadows <- train_area$finished$geometry %>% st_sf() %>% mutate(class = "meadows", id = 3)       
       # settlements only
       train_area <- mapview::viewRGB(stack, r = 8, g = 4, b = 3) %>% mapedit::editMap()       
       settlement <- train_area$finished$geometry %>% st_sf() %>% mutate(class = "settlement", id = 4)       
       
       
       # bind it together to one file
       train_areas <- rbind(forest, fields, meadows, settlement)
       # save results
       saveRDS(train_areas, paste0(envrmt$path_doc,"/train_areas.rds"))
       # first we have to project the data into the correct crs
       tp = sf::st_transform(train_areas,crs = sf::st_crs(stack))
       ## next we extract the values from every band of the raster stack 
       # we force the values to be returned as an data frame
       # because extracting the raster way is very slow
       DF <- raster::extract(stack, tp, df=TRUE) 
       # we rename the layers for simplicity
       names(DF) = c("id","S2B2A_20210613_108_MOF_BOA_10.1","S2B2A_20210613_108_MOF_BOA_10.2","S2B2A_20210613_108_MOF_BOA_10.3","S2B2A_20210613_108_MOF_BOA_10.4","S2B2A_20210613_108_MOF_BOA_10.5","S2B2A_20210613_108_MOF_BOA_10.6","S2B2A_20210613_108_MOF_BOA_10.7","S2B2A_20210613_108_MOF_BOA_10.8","S2B2A_20210613_108_MOF_BOA_10.9","S2B2A_20210613_108_MOF_BOA_10.10","S2B2A_20210613_108_MOF_BOA_10.11")
       # now we add the "class" category which we need later on for training
       # it was dropped during extraction
       DF_sf =st_as_sf(inner_join(DF,tp))       
       # finally we produce a simple data frame without geometry
       DF2 = DF_sf       
       st_geometry(DF2)=NULL       
       ## k-means via RStoolbox
       prediction_kmeans = unsuperClass(stack, nSamples = 25000, nClasses = 4, nStarts = 25,
                                        nIter = 250, norm = TRUE, clusterMap = TRUE,
                                        algorithm = "MacQueen")       
       mapview(prediction_kmeans$map, col = c('darkgreen', 'burlywood', 'green', 'orange'))
       # defining the model 
       cart <- rpart(as.factor(DF2$class)~., data=DF2[,2:12], method = 'class')# the tree       
       rpart.plot(cart, box.palette = 0, main = "Classification Tree")       
       prediction_cart <- raster::predict(stack, cart, type='class', progress = 'text')         
       mapview(prediction_cart,col.regions = c('darkgreen', 'burlywood', 'green', 'orange'))       
       ## random forest via caret
       set.seed(123)
       # split data into train and test data and take only a fraction of them
       trainDat =  DF2[createDataPartition(DF2$id,list = FALSE,p = 0.25),]
       # define a training control object for caret with cross-validation, 10 repeats
       ctrlh = trainControl(method = "cv", 
                            number = 10, 
                            savePredictions = TRUE)
       # train random forest via caret model 
       cv_model = train(trainDat[,2:12],
                        trainDat[,13],
                        method = "rf",
                        metric = "Kappa",
                        trControl = ctrlh,
                        importance = TRUE)
       
       
       
       prediction_rf  = predict(stack ,cv_model, progress = "text")
       mapview(prediction_rf,col.regions = c('darkgreen', 'burlywood', 'green', 'orange'))
       # defining the model 
       cart <- rpart(as.factor(DF2$class)~., data=DF2[,2:12], method = 'class',
                     parms = list(prior = c(0.65,0.25,0.05,0.05)))# the tree
       rpart.plot(cart, box.palette = 0, main = "Classification Tree")       
       