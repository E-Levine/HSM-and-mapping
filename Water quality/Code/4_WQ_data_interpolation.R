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
plot(grid) 
#
#Data as spatial df
Site_data_spdf <- SpatialPointsDataFrame(coords = WQ_summ[,1:2], WQ_summ[,3], 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
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
(Site_Grid_updated <- left_join(Site_Grid, interp_data))
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
#Plot of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_updated, aes(color = Salinity))+
    scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude), color = "black", size = 2.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle("Mean salinity 2020 - 2024") +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
##END OF IDW
#
#
####

