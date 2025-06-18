##Functions used for water quality selection
#
#
#
####Set up####
#Combining filtered files
combine_files <- function(Number_files, Estuarycode, DataSource, Start1, End1, Start2, End2, Start3, End3){
if(Filtered_files == 1){
  WQ_data <- as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
} else if(Filtered_files == 2){
  WQ_data <- rbind(as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))),
                   as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year_2,"_", End_year_2,".xlsx"), na = c("NA", " ", "", "Z"))))
} else {
  WQ_data <- rbind(
    rbind(as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))),
          as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year_2,"_", End_year_2,".xlsx"), na = c("NA", " ", "", "Z")))),
    as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year_3,"_", End_year_3,".xlsx"), na = c("NA", " ", "", "Z"))))
}
return(WQ_data)
}
#
#
#
#
#Limiting to specified date range
date_window <- function(DateBegin, DateEnd){
  if(Data_source == "Portal"){
    WQ_selected <- Filtered_data %>% subset(ActivityStartDate >= as.Date(paste0(DateBegin, "-01-01")) & ActivityStartDate <= as.Date(paste0(DateEnd,"-12-31")))
  } else if(Data_source == "WA"){
    WQ_selected <- Filtered_data %>% subset(SampleDate >= as.Date(paste0(DateBegin, "-01-01")) & SampleDate <= as.Date(paste0(DateEnd,"-12-31")))
  } else if(Data_source == "FIM"){
    WQ_selected <- Filtered_data %>% subset(Sampling_Date >= as.Date(paste0(DateBegin, "-01-01")) & Sampling_Date <= as.Date(paste0(DateEnd,"-12-31")))
  } else {paste("Data source not currently supported.")}
  return(WQ_selected)
}
#
#
#
#
#Spatial transformation of data
Spatial_data <- function(DataInput){
  if(Data_source == "Portal"){
    WQ_sp <- spTransform(SpatialPointsDataFrame(coords = DataInput[,c(9,8)], data = DataInput,
                                                proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                         "+proj=longlat +datum=WGS84 +no_defs +type=crs")
    Combined_data_counts <- SpatialPointsDataFrame(coords = (WQ_sp@data %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                   data = WQ_sp@data %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  } else if(Data_source == "WA"){
    WQ_sp <- spTransform(SpatialPointsDataFrame(coords = DataInput[,c(9,8)], data = DataInput,
                                                proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                         "+proj=longlat +datum=WGS84 +no_defs +type=crs")
    Combined_data_counts <- SpatialPointsDataFrame(coords = (WQ_sp@data %>% distinct(StationID, Actual_Latitude, Actual_Longitude, as.Date(SampleDate), KML) %>% group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                   data = WQ_sp@data %>% distinct(StationID, Actual_Latitude, Actual_Longitude, as.Date(SampleDate), KML) %>% group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  } else if (Data_source == "FIM") {
    WQ_sp <- spTransform(SpatialPointsDataFrame(coords = DataInput[,c(5,6)], data = DataInput,
                                                proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                         "+proj=longlat +datum=WGS84 +no_defs +type=crs")
    Combined_data_counts <- SpatialPointsDataFrame(coords = (WQ_sp@data %>% distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>% group_by(Reference, Latitude, Longitude, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                   data = WQ_sp@data %>% distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>% group_by(Reference, Latitude, Longitude, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
    
  }
  return(Combined_data_counts)
}
#
#
#
#
####Station selection - buffers####
#Requires: WQ_selected, Stations_selected, Estuary_area, FL_outline
buffer_selection <- function(FirstBuffer, SecondBuffer, WidgetSave, EstuaryCode, DataSource, DateBegin, DateEnd){
  if(Data_source == "FIM"){
    WQ_data_t <- st_as_sf(WQ_selected, coords = c(5,6), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  } else {WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))}
  Stations_t <- st_as_sf(Stations_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #
  #Loop to select WQ stations located within specified buffers
  WQ_Stations <- data.frame() #create blank data.frame to fill 
  #
  for(i in 1:nrow(Stations_t)){
    ##Create each buffer zone for all stations of interest
    Stations_FD <- st_buffer(Stations_t[i,], FirstBuffer)
    Stations_SD <- st_difference(st_buffer(Stations_t[i,], SecondBuffer), Stations_FD)
    ##Find all points within each buffer
    Points_FD <- st_filter(WQ_data_t, Stations_FD) %>% mutate(Station = i, Buffer = paste0(FirstBuffer, "m"))
    Points_SD <- st_filter(WQ_data_t, Stations_SD) %>% mutate(Station = i, Buffer = paste0(SecondBuffer, "m"))
    ##Join data for each buffer area
    temp <- bind_rows(Points_FD, Points_SD)
    ##Join into final output dataframe of all stations
    WQ_Stations <- rbind(WQ_Stations, temp)
  }
  #
  WQ_Stations <- WQ_Stations %>% mutate(Station = as.factor(Station))
  #
  if(Data_source == "Portal"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Fixed stations") + tm_dots(size = 1, legend.show = TRUE, col = "navyblue")+ #All possible stations
                           tm_shape(WQ_data_t %>% subset(KML == "In"), "Possible stations") + tm_dots(popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(KML == "In"), "Selected stations") + tm_dots(title = "Station within buffer of:", col = "Station", size = 0.25, legend.show = TRUE,
                                                                                   popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Station" = "Station", "Buffer" = "Buffer"))+
                           tm_layout(main.title = paste(EstuaryCode, DataSource, "WQ Stations", DateBegin, "-", DateEnd, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Fixed stations") + tm_dots(size = 1, legend.show = TRUE, col = "navyblue")+ #All possible stations
                           tm_shape(WQ_data_t %>% subset(KML == "In"), "Possible stations") + tm_dots(popup.vars = c("StationID" = "StationID"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(KML == "In"), "Selected stations") + tm_dots(title = "Station within buffer of:", col = "Station", size = 0.25, legend.show = TRUE,
                                                                                   popup.vars = c("StationID" = "StationID", "Station" = "Station", "Buffer" = "Buffer"))+
                           tm_layout(main.title = paste(EstuaryCode, DataSource, "WQ Stations", DateBegin, "-", DateEnd, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "FIM"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Fixed stations") + tm_dots(size = 1, legend.show = TRUE, col = "navyblue")+ #All possible stations
                           tm_shape(WQ_data_t %>% subset(KML == "In"), "Possible stations") + tm_dots(popup.vars = c("StationID" = "Reference"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(KML == "In"), "Selected stations") + tm_dots(title = "Station within buffer of:", col = "Station", size = 0.25, legend.show = TRUE,
                                                                                                        popup.vars = c("StationID" = "Reference", "Station" = "Station", "Buffer" = "Buffer"))+
                           tm_layout(main.title = paste(EstuaryCode, DataSource, "WQ Stations", DateBegin, "-", DateEnd, sep = " ")))) #Selected stations and buffer area
  }
  #
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", EstuaryCode, "_", DataSource,"_WQ_stations_buffer", FirstBuffer, "_buffer", SecondBuffer, "_widget.html"))}
  #
  #
  return(list(Selected_map = map, Stations = WQ_Stations))
  #
}
#
#
#
#
#
#
####Station selection - closest N####
#
Nclosest_selection <- function(NumStations, maxDistance, WidgetSave){
  Stations_t <- st_as_sf(Stations_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #All WQ data possible:
  if(Data_source == "FIM"){
    WQ_data_t <- st_as_sf(WQ_selected, coords = c(5,6), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  } else {WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))}  
  #
  if(Data_source == "Portal"){
    #List of possible stations and their coordinates:
    WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(LatitudeMeasure, LongitudeMeasure) %>% distinct() %>% mutate(Lat_n = LatitudeMeasure, Lon_n = LongitudeMeasure) %>% drop_na(),
                               coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
    #
    #Empty data frame to fill with selected data
    WQ_closest_selected <- data.frame()
    #
    for(i in 1:nrow(Stations_t)){
      #Determine closest N stations for each station
      temp_station <- Stations_t$geometry[i]
      Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = NumStations, maxdist = maxDistance, returnDist = TRUE)
      #Create data frame of information of selected stations
      Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                        Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                        Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                        LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
      #Limit WQ data to selected stations and add information to relate each station to the specified location
      Station_identification <- left_join(Filtered_data %>% filter(LatitudeMeasure %in% Selected_WQstations$Latitude_match & LongitudeMeasure %in% Selected_WQstations$Longitude_match) %>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>% distinct(), Selected_WQstations, by = c("LatitudeMeasure" = "Latitude_match", "LongitudeMeasure" = "Longitude_match"))
      Selected_data <- left_join(WQ_data_t %>% filter(MonitoringLocationIdentifier %in% Station_identification$MonitoringLocationIdentifier), (Station_identification %>% dplyr::select(-LatitudeMeasure, -LongitudeMeasure)))
      #Combine filtered data to final output
      WQ_closest_selected <- rbind(WQ_closest_selected, Selected_data)
      WQ_closest_selected <- WQ_closest_selected %>% mutate(Distance = round(Distance, 1))
      #Map of all possible stations and stations selected for each location
      All_map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
        tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
        tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 2, col = "gray") + #Stations relation to estuary area
        tm_shape(WQ_closest_selected, "Selected stations") + #Stations selected per location
        tm_dots(size = 1.25, col = "LocationID", palette = "viridis", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    }
  } else if (Data_source == "WA"){
    #List of possible stations and their coordinates:
    WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(Latitude, Longitude) %>% distinct() %>% mutate(Lat_n = Latitude, Lon_n = Longitude),
                               coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
    #
    #Empty data frame to fill with selected data
    WQ_closest_selected <- data.frame()
    #
    for(i in 1:nrow(Stations_t)){
      #Determine closest N stations for each station
      temp_station <- Stations_t$geometry[i]
      Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = NumStations, maxdist = maxDistance, returnDist = TRUE)
      #Create data frame of information of selected stations
      Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                        Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                        Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                        LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
      #Limit WQ data to selected stations and add information to relate each station to the specified location
      Station_identification <- left_join(Filtered_data %>% filter(Latitude %in% Selected_WQstations$Latitude_match & Longitude %in% Selected_WQstations$Longitude_match) %>% dplyr::select(StationID, Latitude, Longitude) %>% distinct(), Selected_WQstations, by = c("Latitude" = "Latitude_match", "Longitude" = "Longitude_match"))
      Selected_data <- left_join(WQ_data_t %>% filter(StationID %in% Station_identification$StationID), (Station_identification %>% dplyr::select(-Latitude, -Longitude)))
      #Combine filtered data to final output
      WQ_closest_selected <- rbind(WQ_closest_selected, Selected_data)
      #Map of all possible stations and stations selected for each location
      All_map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
        tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
        tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 2, col = "gray") + #Stations relation to estuary area
        tm_shape(WQ_closest_selected, "Selected stations") + #Stations selected per location
        tm_dots(size = 1.25, col = "LocationID", palette = "viridis", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    }
  } else if (Data_source == "FIM"){
    #List of possible stations and their coordinates:
    WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(Latitude, Longitude) %>% distinct() %>% mutate(Lat_n = Latitude, Lon_n = Longitude),
                               coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
    #
    #Empty data frame to fill with selected data
    WQ_closest_selected <- data.frame()
    #
    for(i in 1:nrow(Stations_t)){
      #Determine closest N stations for each station
      temp_station <- Stations_t$geometry[i]
      Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = NumStations, maxdist = maxDistance, returnDist = TRUE)
      #Create data frame of information of selected stations
      Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                        Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                        Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                        LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
      #Limit WQ data to selected stations and add information to relate each station to the specified location
      Station_identification <- left_join(Filtered_data %>% filter(Latitude %in% Selected_WQstations$Latitude_match & Longitude %in% Selected_WQstations$Longitude_match) %>% dplyr::select(Reference, Latitude, Longitude) %>% distinct(), Selected_WQstations, by = c("Latitude" = "Latitude_match", "Longitude" = "Longitude_match"))
      Selected_data <- left_join(WQ_data_t %>% filter(Reference %in% Station_identification$Reference), (Station_identification %>% dplyr::select(-Latitude, -Longitude)))
      #Combine filtered data to final output
      WQ_closest_selected <- rbind(WQ_closest_selected, Selected_data)
      #Map of all possible stations and stations selected for each location
      All_map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
        tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
        tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 2, col = "gray") + #Stations relation to estuary area
        tm_shape(WQ_closest_selected, "Selected stations") + #Stations selected per location
        tm_dots(size = 1.25, col = "LocationID", palette = "viridis", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    }
  } else {
    print(paste0("Code not yet written for data source ", Data_source))
  }
  #
  #Widget output to identify stations.
  if(Data_source == "Portal"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(MonitoringLocationIdentifier, LocationID, Distance) %>% unique() %>% group_by(MonitoringLocationIdentifier) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Reference stations") + tm_dots(size = 0.5, col = "navyblue")+
                           tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 0.5, col = "gray", legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier", "LocationID" = "LocationID", "Distance" = "Distance"))+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N,  "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(StationID, LocationID, Distance) %>% unique() %>% group_by(StationID) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Reference stations") + tm_dots(size = 0.5, col = "navyblue")+
                           tm_shape(WQ_locations_t,  "Possible stations") + tm_dots(size = 0.5, col = "gray", legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "StationID", "LocationID" = "LocationID", "Distance" = "Distance"), popup.format = list())+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "FIM"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(Reference, LocationID, Distance) %>% unique() %>% group_by(Reference) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Reference stations") + tm_dots(size = 0.5, col = "navyblue")+
                           tm_shape(WQ_locations_t,  "Possible stations") + tm_dots(size = 0.5, col = "gray", legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "Reference", "LocationID" = "LocationID", "Distance" = "Distance"), popup.format = list())+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else {
    print(paste0("Code not yet written for data source ", Data_source))
  }
  #
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_closest_stations_", Begin_data, "_", End_data, "_widget.html"))}
  #
  return(list(StationsMap = All_map, SelectionMap = map, WQclosest = WQ_closest_selected))
}
#
#
#
#
#
#####Station additions/removals####
#
##Station edits, final output file
Selected_data <- function(BufferOrN, Adding, Removing, ProjectCode){
  if(Data_source == "FIM"){
    WQ_data_t <- st_as_sf(WQ_selected, coords = c(5,6), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  } else {WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))}
  #Both NA
  if(length(Adding) == 1 & length(Removing) == 1){
    WQ_stations_final <- WQ_Stations
    #Adding NA, Removing not NA
  } else if(length(Adding) == 1 & length(Removing) > 1) {
    if(Data_source == "Portal"){
      WQ_stations_final <- WQ_Stations %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
    } else if(Data_source == "FIM"){
      WQ_stations_final <- WQ_data_t %>% subset(!Reference %in% Removing$StationID)
    } else {paste0("Data source not yet supported.")}
    #Adding not NA, Removing NA
  } else if(length(Adding) > 1 & length(Removing) == 1){
    if(Data_source == "Portal"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% 
                                   if(BufferOrN == "Buffer"){mutate(Buffer = "Extra")} else {mutate(Distance = "NA")} %>% 
                                   left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  
    } else if(Data_source == "FIM"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(Reference %in% Adding$StationID) %>% 
                                   if(BufferOrN == "Buffer"){mutate(Buffer = "Extra")} else {mutate(Distance = "NA")} %>% 
                                   left_join(Adding, by = c("Reference" = "StationID"))) 
    } else {paste0("Data source not yet supported.")}
    #Neither NA
  } else {
    if(Data_source == "Portal"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% 
                                   if(BufferOrN == "Buffer"){mutate(Buffer = "Extra")} else {mutate(Distance = "NA")} %>% 
                                   left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
        #Stations to exclude
        subset(!MonitoringLocationIdentifier %in% Removing$StationID)
    } else if(Data_source == "FIM"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(Reference %in% Adding$StationID) %>% 
                                   if(BufferOrN == "Buffer"){mutate(Buffer = "Extra")} else {mutate(Distance = "NA")} %>% 
                                   left_join(Adding, by = c("Reference" = "StationID")))  %>%
        #Stations to exclude
        subset(!Reference %in% Removing$StationID)
    } else {paste0("Data source not yet supported.")}
  }
  
  #
  ##Get coordinates into columns
  WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
  #
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #
  ##Export cleaned final data
  if(BufferOrN == "Buffer") {
    write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_selected_buffer_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
  } else {
    write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_closest_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
    }
  #
  return(print(head(WQ_stations_final %>% as.data.frame())))
}
#
#
#
#
####Station selection - location/boundary####
#
#
location_boundary <- function(SelectionType, SelectedStations, BoundingBox, ProjectCode, WidgetSave){
  Type <- SelectionType
  Station_names <- SelectedStations
  bbox <- BoundingBox
  Project_code <- ProjectCode
  #
  #
  if(Type == "Station_name"){
    #Selection by station name
    if(Data_source == "Portal"){
      WQ_name_selected <- Filtered_data %>% subset(grepl(paste(Station_names, collapse = "|"), MonitoringLocationIdentifier))
      WQ_stations_final <- WQ_name_selected
      return(WQ_stations_final)
      #
    } else if(Data_source == "FIM"){
      WQ_name_selected <- Filtered_data %>% subset(grepl(paste(Station_names, collapse = "|"), Reference))
      WQ_stations_final <- WQ_name_selected
      return(WQ_stations_final)
      #
    } else {paste("Code needs to be updated for selection of stations by name for ", Data_source, " data.", sep = "")}
    #
    } else if(Type == "Bounding_box"){
      #Selection within specified bounding box
      if(Data_source == "Portal"){
        WQ_bb_selected <- Filtered_data %>% filter(LatitudeMeasure < bbox[4] & LatitudeMeasure > bbox[2] & LongitudeMeasure > bbox[1] & LongitudeMeasure < bbox[3])
        WQ_stations_final <- WQ_bb_selected
        #Widget output to identify stations.
        Possible_stations <- st_as_sf(Filtered_data, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
        WQ_locations_t <- st_as_sf(WQ_bb_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
        (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                               tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                               tm_shape(Possible_stations,  "Possible stations") + tm_dots(col = "black", size = 0.3, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
                               tm_shape(WQ_locations_t,  "Selected stations") + tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
                               tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
        if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_bounding_box_", Begin_data, "_", End_data, "_widget.html"))}
        return(list(BoundedStations = WQ_stations_final, BoundedMap = map))
        #
        } else if(Data_source == "WA"){
          print(paste0("Code needs to be updated for Water Atlas data."))
          #Possible_stations <- st_as_sf(Filtered_data, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
          #WQ_locations_t <- st_as_sf(WQ_bb_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
          #(map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
          #                       tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
          #                       tm_shape(Possible_stations,  "Possible stations") + tm_dots(col = "black", size = 0.5, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
          #                       tm_shape(WQ_locations_t,  "Selected stations") + tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "StationID"), popup.format = list())+
          #                       tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
        } else if(Data_source == "FIM"){
          WQ_bb_selected <- Filtered_data %>% filter(Latitude < bbox[4] & Latitude > bbox[2] & Longitude > bbox[1] & Longitude < bbox[3])
          WQ_stations_final <- WQ_bb_selected
          #Widget output to identify stations.
          Possible_stations <- st_as_sf(Filtered_data, coords = c(5,6), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
          WQ_locations_t <- st_as_sf(WQ_bb_selected, coords = c(5,6), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
          (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                                 tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                                 tm_shape(Possible_stations,  "Possible stations") + tm_dots(col = "black", size = 0.3, legend.show = TRUE, popup.vars = c("StationID" = "Reference"))+
                                 tm_shape(WQ_locations_t,  "Selected stations") + tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "Reference"))+
                                 tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
          if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_bounding_box_", Begin_data, "_", End_data, "_widget.html"))}
          return(list(BoundedStations = WQ_stations_final, BoundedMap = map))
          #
        } else {print(paste0("Code not yet written for selection of stations within a bounding box for ", Data_source, " data."))}
      #
      #
    } else {
      #Other selection types
      paste("Code needs to be updated.")}
  }
#
#
#
##Station additions or removals, final output file
Modified_data <- function(Selection_Method, Adding, Removing, ProjectCode){
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #Both NA
  if(length(Adding) == 1 & length(Removing) == 1){
    if(Selection_Method == "Station_name"){
      #Selection by station name - no changes
      WQ_stations_final <- WQ_stations_selected
      write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      #
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - no changes
        WQ_stations_final <- WQ_stations_selected$BoundedStations
        write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else {WQ_stations_final <- (paste0("Code not yet written for using ", Selection_Method, "."))}
  } else if(length(Adding) > 1 | length(Removing) > 1){
    if(Selection_Method == "Station_name"){
      #Selection by station - with changes
      if(Data_source == "Portal"){
        if(length(Adding) == 1 & length(Removing) > 1){WQ_stations_final <- WQ_stations_selected %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
        } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
        write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
        #Selection by station with other data sources:
      } else if(Data_source == "FIM"){
        if(length(Adding) == 1 & length(Removing) > 1){WQ_stations_final <- WQ_stations_selected %>% subset(!Reference %in% Removing$StationID)
        } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))}
        write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
        #Selection by station with other data sources:
      } else {WQ_stations_final <- (paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - with changes
        WQ_stations_selected_bb <- WQ_stations_selected$BoundedStations
        if(Data_source == "Portal"){
          if(length(Adding) == 1 & length(Removing) > 1){WQ_stations_final <- WQ_stations_selected_bb %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
          } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
          write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by bounding box with other data sources
        } else if(Data_source == "FIM"){
          if(length(Adding) == 1 & length(Removing) > 1){WQ_stations_final <- WQ_stations_selected_bb %>% subset(!Reference %in% Removing$StationID)
          } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))}
          write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by bounding box with other data sources
        } else {WQ_stations_final <- (paste0("Code not yet written for using bounding box to include/exclude ", Data_source, " stations."))}
        }
    } else if(length(Adding) > 1 & length(Removing) > 1){
      if(Selection_Method == "Station_name"){
        if(Data_source == "Portal"){
          WQ_stations_final <- rbind(WQ_stations_selected, 
                                     WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
          write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by station with other data sources:
        } else if(Data_source == "FIM"){
          WQ_stations_final <- rbind(WQ_stations_selected, 
                                     WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))  %>% subset(!Reference %in% Removing$StationID)
          write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by station with other data sources:
        } else {WQ_stations_final <- (paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
        } else if(Selection_Method == "Bounding_box"){
          if(Data_source == "Portal"){
            WQ_stations_final <- rbind(WQ_stations_selected, 
                                       WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          } else if(Data_source == "FIM"){
            WQ_stations_final <- rbind(WQ_stations_selected, 
                                       WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))  %>% subset(!Reference %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          } else {WQ_stations_final <- (paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
        }
      }
  return(print(head(WQ_stations_final)))
  }
#
#
#
####Station selection - Output all####
#
output_all <- function(WQ_selected, WidgetSave){
  if(Data_source == "FIM"){
    WQ_data_t <- st_as_sf(WQ_selected, coords = c(5,6), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  } else {WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))}  ##Convert FL_outline to same CRS
  FL_outline2 <- FL_outline %>% st_transform(crs = st_crs(3086))
  ##Determine stations that overlap with FL shape (land) and don't overlap (water)
  Stations_land <- WQ_data_t[lengths(st_intersects(WQ_data_t, FL_outline2))>0,] %>% mutate(Location = "Land")
  Stations_water <- WQ_data_t[!lengths(st_intersects(WQ_data_t, Stations_land)),] %>% mutate(Location = "Water")
  ##Join into final output dataframe of all stations
  WQ_Stations <- rbind(Stations_water, Stations_land) %>% mutate(Location = as.factor(Location))
  #
  if(Data_source == "Portal"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                           popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Location" = "Location"))+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "StationID"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                           popup.vars = c("StationID" = "StationID", "Location" = "Location"))+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "FIM"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "Reference"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                           popup.vars = c("StationID" = "Reference", "Location" = "Location"))+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  }
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_WQ_stations_", Begin_data, "_", End_data, "_widget.html"))}
  return(list(AllStations = map, WQStations = WQ_Stations))
}
#
##Station edits, final output file
finalize_data <- function(Adding, Removing, ProjectCode){
  if(length(Adding) == 1 & length(Removing) == 1){
    WQ_stations_final <- All_Stations$WQStations
  } else if(length(Adding) > 1 & length(Removing) == 1){
    if(Data_source == "Portal"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID"))) 
    } else if(Data_source == "FIM"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))  
    } else {WQ_stations_final <- paste0("Adding or removing stations by name for ", Data_source, "is not yet supported.")}
  } else if(length(Adding) == 1 & length(Removing) > 1){
    if(Data_source == "Portal"){
      WQ_stations_final <- WQ_Stations %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
    } else if(Data_source == "FIM"){
      WQ_stations_final <- WQ_Stations %>% subset(!Reference %in% Removing$StationID)
    } else {WQ_stations_final <- paste0("Adding or removing stations by name for ", Data_source, "is not yet supported.")}
  } else {
    if(Data_source == "Portal"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% mutate(Buffer = "Extra") %>% 
                                   left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
        #Stations to exclude
        subset(!MonitoringLocationIdentifier %in% Removing$StationID)
    } else if(Data_source == "FIM"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(Reference %in% Adding$StationID) %>% mutate(Buffer = "Extra") %>% 
                                   left_join(Adding, by = c("Reference" = "StationID")))  %>%
        #Stations to exclude
        subset(!Reference %in% Removing$StationID)
    } else {WQ_stations_final <- paste0("Adding or removing stations by name for ", Data_source, "is not yet supported.")}
  }
  #
  ##Get coordinates into columns
  WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
  #
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #
  ##Export cleaned final data
  write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
  #
  return(print(head(WQ_stations_final)))
}
#
#
#
#
#
#
#
####Data combination####
#
###Load all files needed
load_all_files <- function(){
  #Initialize list to store data frames and summary dataframe
  file_list <- list()
  file_summary <- data.frame('Data source' = character(), Code = character(), 'Selection method' = character())
  file_names <- list()
  #
  # Loop through each combination of Data_selection_method, Project_codes, and Data_sources
  for (selection in Data_selection_method) {
    for (project in Project_codes) {
      for (source in Data_sources) {
        # Construct the file name based on the current combination
        if(is.na(selection)){
          file_name <- paste0(Estuary_code,"_", source, "_", project, "_", Start_year, "_", End_year, ".xlsx")
        } else {
          file_name <- paste0(Estuary_code,"_", source, "_", selection, "_", project, "_", Start_year, "_", End_year, ".xlsx")
        }
        
        # Specify the path to the directory where the files are located
        # Replace 'your_directory_path' with the actual path
        file_path <- file.path("Data/Compiled_data/", file_name)
        
        # Check if the file exists before trying to read it
        if (file.exists(file_path)) {
          # Read the Excel file
          data <- read_excel(file_path)
          
          # Store the data frame in the list with a unique name
          file_list[[paste(source, project, selection, sep = "_")]] <- data
          file_summary <- rbind(file_summary, data.frame('Data source' = source, Code = project, 'Selection method' = selection))
          file_names[[paste(source, project, selection, sep = "_")]] <- file_name
        } else {
          message(paste("Combination does not exist:", source, "-", project, "-", selection))
        }
      }
    }
  }
  return(list(list_of_files = file_list, summary = file_summary, filenames = file_names))
}
#
#
combine_data_sources <- function(){
  #Empty data to fill
  combined_data <- data.frame()
  #
  for(listdata in seq_along(Data_gather$list_of_files)){
    listname <- names(Data_gather$list_of_files[listdata])
    #Check if FIM data
    if(grepl("FIM", substr(listname, 1, regexpr("_", listname) -1))){
      #extract data to work with
      data <- Data_gather$list_of_files[[listdata]]
      #list of desired columns for merging
      list_cols <- c("Estuary", "Sampling_Date", "Reference", "Longitude", "Latitude", "KML", "Characteristic", "Measurement", "Result_Unit", "Station", 'Buffer')
      #Select columns that exist and create/fill NA as needed
      data <- data %>% dplyr::select(any_of(list_cols))
      for (col in setdiff(list_cols, names(data))) {
        data[[col]] <- NA
        }
      All_FIM <- rbind(combined_data, data)
      #Check if Portal data
      } else if(grepl("Portal", substr(listname, 1, regexpr("_", listname) -1))){
        #extract data to work with
        data <- Data_gather$list_of_files[[listdata]]
        #list of desired columns for merging
        list_cols <- c("Estuary", "ActivityStartDate", "MonitoringLocationIdentifier", "Longitude", "Latitude", "KML", "CharacteristicName", "ResultMeasureValue", "Result_Unit", "Station", 'Buffer')
        #Select columns that exist and create/fill NA as needed
        data <- data %>% dplyr::select(any_of(list_cols))
        for (col in setdiff(list_cols, names(data))) {
          data[[col]] <- NA
          }
        All_Portal <- rbind(combined_data, data)
      } else {print(paste("Code currently doesn't support compiling for", listname))}
    }
  #Rename columns and combine data
  All_data <- rbind(
    #FIM data
    All_FIM %>% 
      rename(Date = "Sampling_Date", WQ_ID = "Reference", Parameter = "Characteristic") %>% 
      mutate(Source = "FIM"),
    #Portal data
    All_Portal %>% 
      rename(Date = "ActivityStartDate", WQ_ID = "MonitoringLocationIdentifier",  Parameter = "CharacteristicName", Measurement = "ResultMeasureValue") %>% 
      mutate(Source = "Portal"))
  #
  #MAKE SUMMARY INFO
  SummaryInfo <- cbind(suppressWarnings(Data_gather$summary %>% dplyr::select(Data.source, Selection.method)),
                       cbind(data.frame(Years = paste(Start_year, End_year, sep = " - ")),
                             data.frame(File.Names = unlist(Data_gather$filenames, use.names = FALSE), stringsAsFactors = FALSE))) %>% 
    rbind(data.frame(Data.source = "Notes:", Selection.method = Data_note, Years = NA, File.Names = NA))
  SummaryInfo <- SummaryInfo %>% mutate(Selection.method = replace_na(Selection.method, "All data"))
  ##Create list with data output and summary output 
  sheets <- list(WQData = All_data, Summary = SummaryInfo)
  ##Export cleaned final data
  write_xlsx(sheets,  path = paste0("Data/Final_data/", Estuary_code, "_", Final_code, "_", Start_year, "_", End_year,".xlsx"), format_headers = TRUE)
  #
  return(All_data)
  message(paste0("Combined data saved as: ", Estuary_code, "_", Final_code, "_", Start_year, "_", End_year,".xlsx"))
  }
#
#
#