##Water quality data interpolation
#
#Interpolation of point data to site area based on site KML file
#Requires WQ data Excel file, KML file, FL_outlines layer, picogrid layer
#
#
##Requires data file in Compiled-data folder, site KML file
#
#Load require packages (install as necessary)  - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               readxl, writexl,
               sf, sp, terra,
               tmap, tmaptools, #Mapping and figures
               mgcv, fpc, fields, interp, #mgcv - interpolation, fpc::bscan - clustering
               RColorBrewer, magicfor, ecorest, #HSV scoring
               marmap, gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
source("Code/autofitVariogram_R.R")
#
Site_code <- c("SL")       #Two letter estuary code
Version <- c("v1")         #Version code for model 
State_Grid <- c("H4")      #Two-letter StateGrid ID
Alt_Grid <- c(NA)          #Two-letter additioanl StateGrid ID, enter NA if no secondary StateGrid needed
Project_code <- c("SLHSM") #Project code given to data, found in file name
Start_year <- c("2020")    #Start year (YYYY) of data, found in file name
End_year <- c("2024")      #End year (YYYY) of data, found in file name
Folder <- c("compiled")    #Data folder: "compiled" or "final"
Data_source <- c("Portal") #Required if Folder = compiled.
#
color_temp <- c("warm")    #"warm" or "cool"
#
####Load data and KML files, plot existing points - will be one function####
#
if(Folder == "compiled"){
  files <- list.files(path = "Data/Compiled-data/", 
                      pattern = paste0(Site_code, "_", Data_source, "_.*_", Project_code, "_", Start_year, "_", End_year,".xlsx"))
  WQ_data <- read_excel(paste0("Data/Compiled-data/", files[1]), na = c("NA", " ", "", "Z")) %>%
    dplyr::rename(Latitude = contains("Latitude"), Longitude = contains("Longitude"), StationID = contains("LocationIdentifier"),
                  Parameter = contains("CharacteristicName"), Value = contains("MeasureValue"))
  } else {paste("Code needs to be updated for 'final' folder location.")}
#
##Site area  
Site_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Site_area[2])
###State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#Load StateGrid(s) of picogrid
PicoGrid <- st_read(paste0("../Reference files/Grids/Florida_PicoGrid_WGS84_",State_Grid,"/Florida_PicoGrid_WGS84_",State_Grid,"_clip.shp"), quiet = TRUE)
if(!is.na(Alt_Grid)){Alt_PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",Alt_Grid,"/Florida_PicoGrid_WGS84_",Alt_Grid,"_clip.shp"), quiet = TRUE)}
#
##Limit to site area
if(!is.na(Alt_Grid)){
  Site_Grid <- rbind(PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,], 
                      Alt_PicoGrid[lengths(st_intersects(Alt_PicoGrid, Site_area))> 0,])
  rm(PicoGrid, Alt_PicoGrid)
} else {
  Site_Grid <- PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,] 
  rm(PicoGrid)
}
#Df of grid data
Site_Grid_df <- Site_Grid %>% st_set_geometry(NULL)
#Map of stations
ggplot()+
  geom_sf(data = Site_area, color = "red")+
  geom_sf(data = Site_Grid, fill = NA)+
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_data, aes(Longitude, Latitude), size = 2.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
#END OF FUNCTION
#
####Summarize data based on parameter of interest - all methods####
#
##Need to summarize data if more than one observation/station and select data to be used for interpolation:
WQ_summ <- WQ_data %>% 
  group_by(StationID, Estuary, Latitude, Longitude, Parameter) %>% 
  summarise(Mean = mean(Value, na.rm = T)) %>% ungroup() %>%
  pivot_wider(names_from = "Parameter", values_from = "Mean") %>% 
  dplyr::select(Longitude, Latitude, Salinity) %>% drop_na() %>% ungroup()
#
#Create grid of area based on station locations - used for all scores
Site_Grid_spdf <- as(Site_Grid, "Spatial")
grid <- spsample(Site_Grid_spdf, type = 'regular', n = 10000)
rast <- rast(Site_Grid, resolution = c(20, 20))
plot(grid) 
#
#Data as spatial df
Site_data_spdf <- SpatialPointsDataFrame(coords = WQ_summ[,1:2], WQ_summ[,3], 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
# Determine which scale to use based on color_temp
if(color_temp == "warm") {
  scale_to_use <- scale_color_viridis_c(option = "rocket", direction = -1)
} else if(color_temp == "cool") {
  scale_to_use <- scale_color_viridis_c(option = "mako", direction = -1)
} else {
  scale_to_use <- scale_color_viridis_c()  # or some other default
}
#
#
#
####Inverse distance weighted####
#
##IDW: model(Parameter), data to use, grid to apply to 
idw_model <- idw(Site_data_spdf$Salinity~1, Site_data_spdf, newdata = grid)
#
#Convert to data frame to rename and add parameters levels as values rounded to 0.1
idw.output <- as.data.frame(idw_model) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Pred_Value = round(Prediction, 2)) 
head(idw.output)
#
#Convert interpolated values to spatial data
idw_spdf <- SpatialPointsDataFrame(coords = idw.output[,1:2],
                                       as.data.frame(idw.output$Pred_Value) %>% rename(Pred_Value = "idw.output$Pred_Value"), 
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of site area
idw_nn <- voronoi(idw_spdf, ext = extent(Site_Grid)) 
qtm(idw_nn, fill = "Pred_Value")
#
#Determine overlay of data on SiteGrid
idw_Site <- intersect(idw_nn, Site_Grid_spdf)
#
###Data frame with interpolated parameter values:
(interp_data <- Site_Grid_df %>% 
  left_join(as.data.frame(idw_Site) %>% dplyr::select(PGID, Pred_Value)) %>% 
  group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1) %>%
  dplyr::rename("Salinity" = Pred_Value))
#Add Salinity data back to Site_grid sf object 
(Site_Grid_idw <- left_join(Site_Grid, interp_data))
#
#
#Plot of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_idw, aes(color = Salinity))+
    scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude), color = "black", size = 2.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle("IDW: Mean salinity 2020 - 2024") +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
##END OF IDW
#
#
####Nearest neighbor####
#
##Voroni
nn_model <- voronoi(x = vect(WQ_summ, geom=c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs"), bnd = Site_area)
plot(nn_model)
points(vect(WQ_summ, geom=c("Longitude", "Latitude")), cex = 0.5)
##Predictions
nn_model <- st_as_sf(nn_model)
#Assign predictions to grid
Site_Grid_nn <- st_intersection(v, st_as_sf(Site_Grid))
Site_Grid_nn <- Site_Grid_nn %>% rename(Salinity_nn = Salinity) %>% dplyr::select(-pred)
#Plot of interpolated values (area)
qtm(Site_Grid_nn, col = "Salinity", fill = "Salinity")
#
#Plot of interpolated values (map):
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_nn, aes(color = Salinity))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude), color = "black", size = 2.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle("NN: Mean salinity 2020 - 2024") +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
##END OF NN
#
####Thin plate spline####
#
#Get extent in meters to create raster:
Site_extent_m <- as.matrix(bb(extent(Site_area), current.projection = 4326, projection = 32617)) #W, S, E, N
#Create base raster using meters:
raster_t_m <- rast(resolution = c(20, 20),
                 xmin = Site_extent_m[1], xmax = Site_extent_m[3], ymin = Site_extent_m[2], ymax = Site_extent_m[4],
                 crs = "+init=EPSG:32617") #expand.grid(long = seq(extent(Site_data)[1], extent(Site_data)[2], length.out = nrow_lon), lat = seq(extent(Site_data)[3], extent(Site_data)[4], length.out = nrow_lat))
#Convert back to dd:
raster_t <- terra::project(raster_t_m, "EPSG:4326")
#Convert WQ points to vector and rasterize over grid:
Param_vec <- vect(Site_data_spdf)
Param_ras <- rasterize(Param_vec, raster_t, field = "Salinity")
#thin plate spline model
tps_model <- interpolate(raster_t, Tps(xyFromCell(Param_ras, 1:ncell(Param_ras)),
                                values(Param_ras)))
plot(tps_model)
#Limit data to area of interest
tps_area <- crop(mask(tps_model, Site_area),Site_area) %>% as.polygons() %>% as("Spatial")
plot(crop(mask(tps_model, Site_area),Site_area))
#Get mean data for each location
TPS_data <- st_intersection(Site_Grid, st_as_sf(tps_area)) %>% rename(Pred_Value = lyr.1) %>% st_set_geometry(NULL) %>%
  dplyr::select(PGID, Pred_Value) %>% group_by(PGID) %>%
  summarize(Pred_Value = mean(Pred_Value, na.rm = T)) 
###Data frame with interpolated parameter values:
(interp_data_TPS <- Site_Grid_df %>% 
    left_join(TPS_data %>% dplyr::select(PGID, Pred_Value)) %>% 
    group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1) %>%
    dplyr::rename("Salinity" = Pred_Value))
#Add Salinity data back to Site_grid sf object 
(Site_Grid_tps <- left_join(Site_Grid, interp_data_TPS))
#
#Plot of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_tps, aes(color = Salinity))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude), color = "black", size = 2.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle("TPS: Mean salinity 2020 - 2024") +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
#
##END OF TPS
#
####Ordinary Kriging####
#
#ok_v <- variogram(Salinity~1, Site_data_spdf)
#plot(ok_v)
ok_vfit <- autofitVariogram(Salinity ~ 1, Site_data_spdf)
#ok_vfit$var_model
ok_model <- gstat(formula = Salinity~1, model = ok_vfit$var_model, data = Site_data_spdf)
ok_pred <- predict(ok_model, grid)
#Convert to data frame to rename and add parameters levels as values rounded to 0.1
ok.output <- as.data.frame(ok_pred) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Pred_Value = round(Prediction, 2)) 
#Convert interpolated values to spatial data
ok_spdf <- SpatialPointsDataFrame(coords = ok.output[,1:2],
                                   as.data.frame(ok.output$Pred_Value) %>% rename(Pred_Value = "ok.output$Pred_Value"), 
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of site area
ok_nn <- voronoi(ok_spdf, ext = extent(Site_Grid)) 
plot(ok_nn, fill = "Pred_Value")
plot(crop(ok_nn, Site_area))
#
#Determine overlay of data on SiteGrid
ok_Site <- intersect(ok_nn, Site_Grid_spdf)
#
###Data frame with interpolated parameter values:
(interp_data_ok <- Site_Grid_df %>% 
    left_join(as.data.frame(ok_Site) %>% dplyr::select(PGID, Pred_Value)) %>% 
    group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1) %>%
    dplyr::rename("Salinity" = Pred_Value))
#Add Salinity data back to Site_grid sf object 
(Site_Grid_ok <- left_join(Site_Grid, interp_data_ok))
#
#
#Plot of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_ok, aes(color = Salinity))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude), color = "black", size = 2.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle("OK: Mean salinity 2020 - 2024") +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
#
##END OF OK
#