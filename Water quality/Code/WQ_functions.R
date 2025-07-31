##Functions used for water quality selection
#
#
#
####Set up####
#Combining filtered files
combine_files <- function(Number_files, Estuarycode, DataSource, Start1, End1, Start2, End2, Start3, End3){
if(Filtered_files == 1){
  WQ_data <- as.data.frame(read_excel(paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
} else if(Filtered_files == 2){
  WQ_data <- rbind(as.data.frame(read_excel(paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))),
                   as.data.frame(read_excel(paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_",Start_year_2,"_", End_year_2,".xlsx"), na = c("NA", " ", "", "Z"))))
} else {
  WQ_data <- rbind(
    rbind(as.data.frame(read_excel(paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))),
          as.data.frame(read_excel(paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_",Start_year_2,"_", End_year_2,".xlsx"), na = c("NA", " ", "", "Z")))),
    as.data.frame(read_excel(paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_",Start_year_3,"_", End_year_3,".xlsx"), na = c("NA", " ", "", "Z"))))
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
                           tm_layout(main.title = paste(Site_code, Data_source, "Closest", Stations_N,  "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(StationID, LocationID, Distance) %>% unique() %>% group_by(StationID) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Reference stations") + tm_dots(size = 0.5, col = "navyblue")+
                           tm_shape(WQ_locations_t,  "Possible stations") + tm_dots(size = 0.5, col = "gray", legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "StationID", "LocationID" = "LocationID", "Distance" = "Distance"), popup.format = list())+
                           tm_layout(main.title = paste(Site_code, Data_source, "Closest", Stations_N, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "FIM"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(Reference, LocationID, Distance) %>% unique() %>% group_by(Reference) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Reference stations") + tm_dots(size = 0.5, col = "navyblue")+
                           tm_shape(WQ_locations_t,  "Possible stations") + tm_dots(size = 0.5, col = "gray", legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "Reference", "LocationID" = "LocationID", "Distance" = "Distance"), popup.format = list())+
                           tm_layout(main.title = paste(Site_code, Data_source, "Closest", Stations_N, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else {
    print(paste0("Code not yet written for data source ", Data_source))
  }
  #
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Site_code, "_", Data_source,"_closest_stations_", Begin_data, "_", End_data, "_widget.html"))}
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
    write_xlsx(WQ_stations_final_df, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_selected_buffer_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
  } else {
    write_xlsx(WQ_stations_final_df, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_closest_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
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
                               tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
        if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Site_code, "_", Data_source,"_bounding_box_", Begin_data, "_", End_data, "_widget.html"))}
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
          #                       tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
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
                                 tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
          if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Site_code, "_", Data_source,"_bounding_box_", Begin_data, "_", End_data, "_widget.html"))}
          return(list(BoundedStations = WQ_stations_final, BoundedMap = map))
          #
        } else {print(paste0("Code not yet written for selection of stations within a bounding box for ", Data_source, " data."))}
      #
      #
    } else if(Type == "Estuary"){
      WQ_locations_t <- st_as_sf(Filtered_data, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(Estuary_area))
      Estuary_points <- WQ_locations_t[st_within(WQ_locations_t, Estuary_area, sparse = FALSE), ]
      WQ_stations_final <- Estuary_points
      if(Data_source == "Portal"){
        (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                               tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                               tm_shape(WQ_locations_t,  "Possible stations") + tm_dots(col = "black", size = 0.3, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
                               tm_shape(Estuary_points,  "Selected stations") + tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
                               tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
      }
      return(list(BoundedStations = WQ_stations_final, BoundedMap = map))
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
  if(is.na(Adding) == TRUE && is.na(Removing) == TRUE){
    if(Selection_Method == "Station_name"){
      #Selection by station name - no changes
      WQ_stations_final <- WQ_stations_selected
      write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      #
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - no changes
        WQ_stations_final <- WQ_stations_selected$BoundedStations
        write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else if(Selection_Method == "Estuary"){
        WQ_stations_final <- WQ_stations_selected$BoundedStations
        write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_estuary_area_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else {WQ_stations_final <- (paste0("Code not yet written for using ", Selection_Method, "."))}
    #END BOTH NA
  } else if((length(Adding) > 1 || is.na(Adding) != TRUE)  || (length(Removing) > 1 || is.na(Removing) != TRUE)){
    #If either has station
    if(Selection_Method == "Station_name"){
      #Selection by station - with changes
      if(Data_source == "Portal"){
        if(is.na(Adding) == TRUE && is.na(Removing) != TRUE){WQ_stations_final <- WQ_stations_selected %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
        } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
        write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
        #Selection by station with other data sources:
      } else if(Data_source == "FIM"){
        if(is.na(Adding) == TRUE && is.na(Removing) != TRUE){WQ_stations_final <- WQ_stations_selected %>% subset(!Reference %in% Removing$StationID)
        } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))}
        write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
        #Selection by station with other data sources:
      } else {WQ_stations_final <- (paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - with changes
        WQ_stations_selected_bb <- WQ_stations_selected$BoundedStations
        if(Data_source == "Portal"){
          if(is.na(Adding) == TRUE && is.na(Removing) != TRUE){WQ_stations_final <- WQ_stations_selected_bb %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
          } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
          write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by bounding box with other data sources
        } else if(Data_source == "FIM"){
          if(is.na(Adding) == TRUE && is.na(Removing) != TRUE){WQ_stations_final <- WQ_stations_selected_bb %>% subset(!Reference %in% Removing$StationID)
          } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))}
          write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by bounding box with other data sources
        } else {WQ_stations_final <- (paste0("Code not yet written for using bounding box to include/exclude ", Data_source, " stations."))}
        } else if(Selection_Method == "Estuary"){
          if(Data_source == "Portal"){
            if(is.na(Adding) == TRUE && is.na(Removing) != TRUE){WQ_stations_final <- WQ_stations_selected$BoundedStations %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
            } else {WQ_stations_final <- rbind(WQ_selected %>% filter(MonitoringLocationIdentifier %in% WQ_stations_selected$BoundedStations$MonitoringLocationIdentifier), WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
            write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_estuary_area_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
            #Selection by bounding box with other data sources
          } else if(Data_source == "FIM"){
            if(is.na(Adding) == TRUE && is.na(Removing) != TRUE){WQ_stations_final <- WQ_stations_selected_bb %>% subset(!Reference %in% Removing$StationID)
            } else {WQ_stations_final <- rbind(WQ_selected %>% filter(MonitoringLocationIdentifier %in% WQ_stations_selected$BoundedStations$MonitoringLocationIdentifier), WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))}
            write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_estuary_area_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
            #Selection by bounding box with other data sources
          } else {WQ_stations_final <- (paste0("Code not yet written for using bounding box to include/exclude ", Data_source, " stations."))}
        }
    } else if(is.na(Adding) != TRUE && is.na(Removing) != TRUE){
      if(Selection_Method == "Station_name"){
        if(Data_source == "Portal"){
          WQ_stations_final <- rbind(WQ_stations_selected, 
                                     WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
          write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by station with other data sources:
        } else if(Data_source == "FIM"){
          WQ_stations_final <- rbind(WQ_stations_selected, 
                                     WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))  %>% subset(!Reference %in% Removing$StationID)
          write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by station with other data sources:
        } else {WQ_stations_final <- (paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
        } else if(Selection_Method == "Bounding_box"){
          if(Data_source == "Portal"){
            WQ_stations_final <- rbind(WQ_stations_selected, 
                                       WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          } else if(Data_source == "FIM"){
            WQ_stations_final <- rbind(WQ_stations_selected, 
                                       WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))  %>% subset(!Reference %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          } else {WQ_stations_final <- (paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
        } else if(Selection_Method == "Estuary"){
          if(Data_source == "Portal"){
            WQ_stations_final <- rbind(WQ_selected %>% filter(MonitoringLocationIdentifier %in% WQ_stations_selected$BoundedStations$MonitoringLocationIdentifier), 
                                       WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_estuary_area_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          } else if(Data_source == "FIM"){
            WQ_stations_final <- rbind(WQ_selected %>% filter(MonitoringLocationIdentifier %in% WQ_stations_selected$BoundedStations$MonitoringLocationIdentifier), 
                                       WQ_selected %>% subset(Reference %in% Adding$StationID) %>% left_join(Adding, by = c("Reference" = "StationID")))  %>% subset(!Reference %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_estuary_area_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
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
                           tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "StationID"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                           popup.vars = c("StationID" = "StationID", "Location" = "Location"))+
                           tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "FIM"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "Reference"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                           popup.vars = c("StationID" = "Reference", "Location" = "Location"))+
                           tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  }
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Site_code, "_", Data_source,"_WQ_stations_", Begin_data, "_", End_data, "_widget.html"))}
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
  write_xlsx(WQ_stations_final_df, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
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
          file_name <- paste0(Site_code,"_", source, "_", project, "_", Start_year, "_", End_year, ".xlsx")
        } else {
          file_name <- paste0(Site_code,"_", source, "_", selection, "_", project, "_", Start_year, "_", End_year, ".xlsx")
        }
        
        # Specify the path to the directory where the files are located
        # Replace 'your_directory_path' with the actual path
        file_path <- file.path("Data/Compiled-data/", file_name)
        
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
  write_xlsx(sheets,  path = paste0("Data/Final-data/", Site_code, "_", Final_code, "_", Start_year, "_", End_year,".xlsx"), format_headers = TRUE)
  #
  return(All_data)
  message(paste0("Combined data saved as: ", Site_code, "_", Final_code, "_", Start_year, "_", End_year,".xlsx"))
  }
#
#
#
####Variogram functions####
#
#
###All functions from automap package. See automap package for author/citation information.
#
# This function automatically fits a variogram to input_data
autofitVariogram <- function(formula, input_data, model = c("Sph", "Exp", "Gau", "Ste"), kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), fix.values = c(NA,NA,NA),
                            verbose = FALSE, GLS.model = NA, start_vals = c(NA,NA,NA), miscFitOptions = list(),...) {
  # Check for anisotropy parameters
  if('alpha' %in% names(list(...))) warning('Anisotropic variogram model fitting not supported, see the documentation of autofitVariogram for more details.')
  
  # Take the misc fit options and overwrite the defaults by the user specified ones
  miscFitOptionsDefaults = list(merge.small.bins = TRUE, min.np.bin = 5)
  miscFitOptions = modifyList(miscFitOptionsDefaults, miscFitOptions)
  
  # Create boundaries
  if (is(input_data, "Spatial")) {
    longlat = !is.projected(input_data)
    if(is.na(longlat)) longlat = FALSE
    diagonal = spDists(t(bbox(input_data)), longlat = longlat)[1,2]                # 0.35 times the length of the central axis through the area
  } else {
    longlat = st_is_longlat(input_data)
    if (is.na(longlat)) longlat = FALSE
    bb = st_bbox(input_data)
    diagonal = sqrt(((bb$xmax-bb$xmin)^2)+((bb$ymax-bb$ymin)^2))
  }
  
  
  boundaries = c(2,4,6,9,12,15,25,35,50,65,80,100) * diagonal * 0.35/100         # Boundaries for the bins in km
  
  
  # If you specifiy a variogram model in GLS.model the Generelised least squares sample variogram is constructed
  if(!is(GLS.model, "variogramModel")) {
    experimental_variogram = variogram(formula, input_data,boundaries = boundaries, ...)
  } else {
    if(verbose) cat("Calculating GLS sample variogram\n")
    g = gstat(NULL, "bla", formula, input_data, model = GLS.model, set = list(gls=1))
    experimental_variogram = variogram(g, boundaries = boundaries, ...)
  }
  
  # request by Jon Skoien
  if(miscFitOptions[["merge.small.bins"]]) {
    if(verbose) cat("Checking if any bins have less than 5 points, merging bins when necessary...\n\n")
    while(TRUE) {
      if(length(experimental_variogram$np[experimental_variogram$np < miscFitOptions[["min.np.bin"]]]) == 0 | length(boundaries) == 1) break
      boundaries = boundaries[2:length(boundaries)]			
      if(!is(GLS.model, "variogramModel")) {
        experimental_variogram = variogram(formula, input_data,boundaries = boundaries, ...)
      } else {
        experimental_variogram = variogram(g, boundaries = boundaries, ...)
      }
    }	
  }
  
  # set initial values
  if(is.na(start_vals[1])) {  # Nugget
    initial_nugget = min(experimental_variogram$gamma)
  } else {
    initial_nugget = start_vals[1]
  }
  if(is.na(start_vals[2])) { # Range
    initial_range = 0.1 * diagonal   # 0.10 times the length of the central axis through the area
  } else {
    initial_range = start_vals[2]
  }
  if(is.na(start_vals[3])) { # Sill
    initial_sill = mean(c(max(experimental_variogram$gamma), median(experimental_variogram$gamma)))
  } else {
    initial_sill = start_vals[3]
  }
  
  # Determine what should be automatically fitted and what should be fixed
  # Nugget
  if(!is.na(fix.values[1]))
  {
    fit_nugget = FALSE
    initial_nugget = fix.values[1]
  } else
    fit_nugget = TRUE
  
  # Range
  if(!is.na(fix.values[2]))
  {
    fit_range = FALSE
    initial_range = fix.values[2]
  } else
    fit_range = TRUE
  
  # Partial sill
  if(!is.na(fix.values[3]))
  {
    fit_sill = FALSE
    initial_sill = fix.values[3]
  } else
    fit_sill = TRUE
  
  getModel = function(psill, model, range, kappa, nugget, fit_range, fit_sill, fit_nugget, verbose)
  {
    if(verbose) debug.level = 1 else debug.level = 0
    if(model == "Pow") {
      warning("Using the power model is at your own risk, read the docs of autofitVariogram for more details.")
      if(is.na(start_vals[1])) nugget = 0
      if(is.na(start_vals[2])) range = 1    # If a power mode, range == 1 is a better start value
      if(is.na(start_vals[3])) sill = 1
    }
    obj = try(fit.variogram(experimental_variogram,
                            model = vgm(psill=psill, model=model, range=range,
                                        nugget=nugget,kappa = kappa),
                            fit.ranges = c(fit_range), fit.sills = c(fit_nugget, fit_sill),
                            debug.level = 0), 
              TRUE)
    if("try-error" %in% class(obj)) {
      #print(traceback())
      warning("An error has occured during variogram fitting. Used:\n", 
              "\tnugget:\t", nugget, 
              "\n\tmodel:\t", model, 
              "\n\tpsill:\t", psill,
              "\n\trange:\t", range,
              "\n\tkappa:\t",ifelse(kappa == 0, NA, kappa),
              "\n  as initial guess. This particular variogram fit is not taken into account. \nGstat error:\n", obj)
      return(NULL)
    } else return(obj)
  }
  
  
  # Automatically testing different models, the one with the smallest sums-of-squares is chosen
  test_models = model
  SSerr_list = c()
  vgm_list = list()
  counter = 1
  
  for(m in test_models) {
    if(m != "Mat" && m != "Ste") {        # If not Matern and not Stein
      model_fit = getModel(initial_sill - initial_nugget, m, initial_range, kappa = 0, initial_nugget, fit_range, fit_sill, fit_nugget, verbose = verbose)
      if(!is.null(model_fit)) {	# skip models that failed
        vgm_list[[counter]] = model_fit
        SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))}
      counter = counter + 1
    } else {                 # Else loop also over kappa values
      for(k in kappa) {
        model_fit = getModel(initial_sill - initial_nugget, m, initial_range, k, initial_nugget, fit_range, fit_sill, fit_nugget, verbose = verbose)
        if(!is.null(model_fit)) {
          vgm_list[[counter]] = model_fit
          SSerr_list = c(SSerr_list, attr(model_fit, "SSErr"))}
        counter = counter + 1
      }
    }
  }
  
  # Check for negative values in sill or range coming from fit.variogram
  # and NULL values in vgm_list, and remove those with a warning
  strange_entries = sapply(vgm_list, function(v) any(c(v$psill, v$range) < 0) | is.null(v))
  if(any(strange_entries)) {
    if(verbose) {
      print(vgm_list[strange_entries])
      cat("^^^ ABOVE MODELS WERE REMOVED ^^^\n\n")
    }
    warning("Some models where removed for being either NULL or having a negative sill/range/nugget, \n\tset verbose == TRUE for more information")
    SSerr_list = SSerr_list[!strange_entries]
    vgm_list = vgm_list[!strange_entries]
  }
  
  if(verbose) {
    cat("Selected:\n")
    print(vgm_list[[which.min(SSerr_list)]])
    cat("\nTested models, best first:\n")
    tested = data.frame("Tested models" = sapply(vgm_list, function(x) as.character(x[2,1])), 
                        kappa = sapply(vgm_list, function(x) as.character(x[2,4])), 
                        "SSerror" = SSerr_list)
    tested = tested[order(tested$SSerror),]
    print(tested)
  }
  
  result = list(exp_var = experimental_variogram, var_model = vgm_list[[which.min(SSerr_list)]], sserr = min(SSerr_list))
  class(result) = c("autofitVariogram","list")    
  
  return(result)
}
#
#
plot.autofitVariogram = function(x, plotit = TRUE, ...){ 
  shift = 0.03
  labels = as.character(x$exp_var$np)
  vario = xyplot(gamma ~ dist, data = x$exp_var, panel = autokrige.vgm.panel,
                 labels = labels, shift = shift, model = x$var_model,# subscripts = TRUE,
                 direction = c(x$exp_var$dir.hor[1], x$exp_var$dir.ver[1]),
                 ylim = c(min(0, 1.04 * min(x$exp_var$gamma)), 1.04 * max(x$exp_var$gamma)),
                 xlim = c(0, 1.04 * max(x$exp_var$dist)), xlab = "Distance", ylab = "Semi-variance",
                 main = "Experimental variogram and fitted variogram model", mode = "direct",...)
  if (plotit) print(vario) else vario
}
#
#
# Provides a summary function for the autofitVariogram object
summary.autofitVariogram <- function(object, ...) {
  cat("Experimental variogram:\n")
  print(object$exp_var, ...)
  cat("\nFitted variogram model:\n")
  print(object$var_model, ...)
  cat("Sums of squares betw. var. model and sample var.")
  print(object$sserr)
}
#
#
##END OF VARIOGRAM FUNCTIONS
#
####Data summarization functions####
#
summarize_data <- function(data_frame = WQ_data, Parameter_name = Param_name, Time_period = "Year", Year_range = "NA", Quarter_start = NA, Month_range = NA, Summ_method = "Means") {
  #
  # Initialize Start_year and End_year
  Start_year <- NULL
  End_year <- NULL
  ##Set year range:
  if(grepl("^\\d{4}-\\d{4}$", Year_range)){
    Start_year <- as.numeric(substr(Year_range, 1, 4))
    End_year <- as.numeric(substr(Year_range, 6, 9))
  } else if(nchar(Year_range) == 4){
    Start_year <- as.numeric(Year_range)
    End_year <- as.numeric(Year_range)
  } else if(Year_range == "NA") {
    Year_range <- NA
  } else {
    stop("Year range must be in the format 'YYYY-YYYY' or 'YYYY'.")
  }
  #
  ##Clean and group data
  temp_df <- data_frame %>% 
    #Filter to desired parameter
    dplyr::filter(str_detect(Parameter, Parameter_name)) %>%
    #Add in missing group columns
    mutate(Year = year(as.Date(ActivityStartDate)),
           Month = month(as.Date(ActivityStartDate), label = TRUE)) %>%
    #Assign quarters, starting at month specified or default start of January
    {if(!is.na(Quarter_start)) mutate(., Quarter = set_quarters(as.Date(ActivityStartDate), Quarter_start)) else mutate(., Quarter = quarter(as.Date(ActivityStartDate)))} %>%
    #Filter to specified months if applicable
    {if(length(Month_range) == 2) filter(., between(month(as.Date(ActivityStartDate)), Month_range[1], Month_range[2])) else . } %>%
    #Grouping for evals: station, specified time period
    group_by(Estuary, Latitude, Longitude, Parameter, !!sym(Time_period))
  #
  ##Summarize data using method specified
  if(Summ_method == "Means"){
    summary_data <- station_means(temp_df, Year_range, Start_year, End_year)  
  } else if(Summ_method == "Mins"){
    summary_data <- station_mins(temp_df, Year_range, Start_year, End_year)  
  } else if(Summ_method == "Maxs"){
    summary_data <- station_maxs(temp_df, Year_range, Start_year, End_year)  
  } else if(Summ_method == "Range"){
    summary_data <- station_range(temp_df, values = "No", Year_range, Start_year, End_year)  
  } else if(Summ_method == "Range_values"){
    summary_data <- station_range(temp_df, values = "Yes", Year_range, Start_year, End_year)  
  } else {
    stop("Summarization method supplied is incorrectly speficied or is not currently suppported.")
  }
  #
  output_data <- summary_data %>% ungroup() %>% 
    pivot_longer(cols = intersect(c("Mean", "Minimum", "Maximum", "Range"), names(summary_data)), names_to = "Statistic", values_to = "Value") %>%
    pivot_wider(names_from = "Parameter", values_from = "Value") %>% 
    dplyr::select(Longitude, Latitude, Statistic, all_of(Param_name)) %>% drop_na() %>% ungroup() %>%
    rename(Working_Param = any_of(Param_name))
  #
  return(output_data)
  #
}
####Sub-functions:
#
#Set quarters
set_quarters <- function(date, start_month) {
  month_num <- month(date)
  # Calculate the adjusted month number
  adjusted_month <- (month_num - start_month + 12) %% 12
  # Determine the quarter based on the adjusted month
  return(floor(adjusted_month / 3) + 1)
}
#Means of parameter
station_means <- function(df, Range, StartYr, EndYr){
  temp <- df %>% 
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% group_by(Estuary, Latitude, Longitude, Parameter) %>%
    summarise(Mean = mean(Value, na.rm = TRUE))
  return(temp)
}
#Minimums of parameter
station_mins <- function(df, Range, StartYr, EndYr){
  temp <- df %>%  
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% group_by(Estuary, Latitude, Longitude, Parameter) %>%
    summarise(Minimum = min(Value, na.rm = TRUE))
  return(temp)
}
#Maximums of parameter
station_maxs <- function(df, Range, StartYr, EndYr){
  temp <- df %>%  
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% group_by(Estuary, Latitude, Longitude, Parameter) %>%
    summarise(Maximum = max(Value, na.rm = TRUE))
  return(temp)
}
#Range of parameter: either the range (values = N) or the min and max (values = Y)
station_range <- function(df, values, Range, StartYr, EndYr){
  temp <- df %>%  
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% group_by(Estuary, Latitude, Longitude, Parameter) %>%
    summarise(Maximum = max(Value, na.rm = TRUE),
              Minimum = min(Value, na.rm = TRUE)) %>%
    {if(values == "Yes") . else mutate(., Range = Maximum - Minimum) %>% dplyr::select(., -Maximum, -Minimum) }
  return(temp)
}
####Interpolation####
#
perform_idw_interpolation <- function(Site_data_spdf, grid, Site_Grid, Site_Grid_spdf, Site_Grid_df, Parameter = Param_name) {
  Param_name <- Parameter
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Initiate lists 
  idw.output <- list()
  idw_spdf <- list()
  idw_nn <- list()
  idw_Site <- list()
  # Create a progress bar
  pb <- progress_bar$new(format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
                         total = length(stats) * 4,  
                         complete = "=", incomplete = "-", current = ">",
                         clear = FALSE, width = 100, show_after = 0, force = TRUE)
  #
  tryCatch({
    #Loop over each statistic
    for(i in stats){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == i, ]
      ##IDW: model(Parameter), data to use, grid to apply to 
      idw_model <- suppressMessages(idw(stat_data$Working_Param~1, stat_data, newdata = grid))
      #Convert to data frame to rename and add parameters levels as values rounded to 0.1
      idw.output[[i]] <- as.data.frame(idw_model) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>% 
        mutate(Pred_Value = round(Prediction, 2)) %>% dplyr::select(-var1.var)
      #
      ##PROCESSING:
      pb$tick(tokens = list(step = "Processing"))
      Sys.sleep(1/1000)
      #Convert interpolated values to spatial data
      idw_spdf[[i]] <- SpatialPointsDataFrame(coords = idw.output[[i]][,1:2], data = idw.output[[i]][4], 
                                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
      #
      #Use nearest neighbor to merge values into polygons, limit to bounding box of site area
      idw_nn[[i]] <- dismo::voronoi(idw_spdf[[i]], ext = extent(Site_Grid)) 
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #Determine overlay of data on SiteGrid
      idw_Site[[i]] <- st_as_sf(intersect(idw_nn[[i]], Site_Grid_spdf))
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #Rename column based on model type
      names(idw_Site[[i]])[names(idw_Site[[i]]) == "Pred_Value"] <- "Pred_Value_idw"
      #
      pb$message(paste("Completed:", i, Param_name))
    }
    close(pb)
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  return(idw_Site)
}
#
perform_nn_interpolation <- function(Site_data_spdf, Site_area, Site_Grid, Site_Grid_df, Parameter = Param_name, WQ_summ) {
  Param_name <- Parameter
  WQsumm <- WQ_summ
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Initiate lists 
  nn_model <- list()
  nn_Site <- list()
  # Create a progress bar
  pb <- progress_bar$new(format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
                         total = length(stats) * 3,  
                         complete = "=", incomplete = "-", current = ">",
                         clear = FALSE, width = 100, show_after = 0, force = TRUE)
  #
  tryCatch({
    #Loop over each statistic
    for(i in seq_along(stats)){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == stats[i], ] 
      WQ_data_stat <- WQsumm %>% filter(Statistic == stats[i])
      ##NN: model(Parameter), data to use, grid to apply to 
      nn_model[[i]] <- st_as_sf(voronoi(x = vect(WQ_data_stat, geom=c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs"), bnd = Site_area))
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #Assign predictions to grid
      nn_Site[[i]] <- st_intersection(nn_model[[i]], st_as_sf(Site_Grid %>% dplyr::select(Latitude:MGID)))
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #Rename column based on model type
      names(nn_Site[[i]])[names(nn_Site[[i]]) == "Working_Param"] <- "Pred_Value_nn"
      #
      pb$message(paste("Completed:", i, Param_name))
    }
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  return(nn_Site)
}
#
perform_tps_interpolation <- function(Site_data_spdf, raster_t, Site_area, Site_Grid, Parameter = Param_name) {
  Param_name <- Parameter
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Initiate lists 
  tps_model <- list()
  tps_over <- list()
  tps_Site <- list()
  # Create a progress bar
  pb <- progress_bar$new(format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
                         total = length(stats) * 3,  
                         complete = "=", incomplete = "-", current = ">",
                         clear = FALSE, width = 100, show_after = 0, force = TRUE)
  #
  tryCatch({
    #Loop over each statistic
    for(i in stats){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == i, ]
      #Convert WQ points to vector and rasterize over grid:
      Param_vec <- vect(stat_data)
      crs(Param_vec) <- "EPSG:4326"
      Param_ras <- rasterize(Param_vec, raster_t, field = "Working_Param")
      #thin plate spline model
      tps_model_a <- interpolate(raster_t, Tps(xyFromCell(Param_ras, 1:ncell(Param_ras)),
                                               values(Param_ras)))
      #Limit data to area of interest
      tps_model[[i]] <- crop(mask(tps_model_a, Site_area),Site_area) %>% as.polygons() %>% as("Spatial")
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #Get mean data for each location
      tps_Site[[i]] <- st_intersection(Site_Grid %>% dplyr::select(Latitude:MGID), st_as_sf(tps_model[[i]])) #%>% 
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #Rename column based on model type
      names(tps_Site[[i]])[names(tps_Site[[i]]) == "lyr.1"] <- "Pred_Value_tps"
      #
      pb$message(paste("Completed:", i, Param_name))
    }
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  return(tps_Site)
}
#
perform_ok_interpolation <- function(Site_data_spdf, grid, Site_Grid, Site_Grid_spdf, Parameter = Param_name) {
  Param_name <- Parameter
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Initiate lists 
  ok.output <- list()
  ok_spdf <- list()
  ok_nn <- list()
  ok_Site <- list()
  # Create a progress bar
  pb <- progress_bar$new(format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
                         total = length(stats) * 4, 
                         complete = "=", incomplete = "-", current = ">",
                         clear = FALSE, width = 100, show_after = 0, force = TRUE)
  #
  tryCatch({
    #Loop over each statistic
    for(i in stats){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == i, ]
      ##IOK: model(Parameter), data to use, grid to apply to 
      ok_fit <- autofitVariogram(Working_Param ~ 1, stat_data)
      ok_model <- gstat(formula = Working_Param~1, locations = stat_data, model = ok_fit$var_model, data = st_as_sf(stat_data))
      ok_pred <- predict(ok_model, grid)
      #Convert to data frame to rename and add parameters levels as values rounded to 0.1
      ok.output[[i]] <- as.data.frame(ok_pred) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
        mutate(Pred_Value = round(Prediction, 2)) %>% dplyr::select(-var1.var)
      #
      ##PROCESSING:
      pb$tick(tokens = list(step = "Processing"))
      Sys.sleep(1/1000)
      #Convert interpolated values to spatial data
      ok_spdf[[i]] <- SpatialPointsDataFrame(coords = ok.output[[i]][,1:2], data = ok.output[[i]][4], 
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
      #Use nearest neighbor to merge values into polygons, limit to bounding box of site area
      ok_nn[[i]] <- dismo::voronoi(ok_spdf[[i]], ext = extent(Site_Grid))#
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #Determine overlay of data on SiteGrid
      ok_Site[[i]] <- st_as_sf(intersect(ok_nn[[i]], Site_Grid_spdf))
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #Rename column based on model type
      names(ok_Site[[i]])[names(ok_Site[[i]]) == "Pred_Value"] <- "Pred_Value_ok"
      #
      pb$message(paste("Completed:", i, Param_name))
    }
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  return(ok_Site)
}
#
join_interpolation <- function(Site_Grid_df){
  #Starting data:
  output <- Site_Grid_df
  #List of potential input SF objects to check for:
  sf_objects <- c("idw_data", "nn_data", "tps_data", "ok_data")
  #Determine number of models to combine:
  existing_object <- NULL
  existing_count <- 0
  #Check which spatial objects exist
  for (sf_obj in sf_objects) {
    if (exists(sf_obj, envir = .GlobalEnv)) {
      existing_count <- existing_count + 1
      if (is.null(existing_object)) {
        existing_object <- sf_obj  # Store the first existing object
      }
    }
  }
  #Check the number of existing objects
  if (existing_count == 0) {
    stop("Error: None of the model outputs exist in the global environment.")
  } else if (existing_count == 1) {
    stop(paste("Error: Only one model output exists: ", existing_object," Creation of ensemble model not needed."))
  }
  #Get the list of statistics/list names:
  params <- names(get(existing_object))
  #
  #Iterate over each parameter
  for (i in seq_along(params)){
    temp_results <- list()
    #Iterate through each possible SF object
    for (sf_obj in sf_objects) {
      if (exists(sf_obj, envir = .GlobalEnv)) {
        cat("Joining ", sf_obj, " with existing data...\n")
        temp_data <- get(sf_obj[[i]]) %>% as.data.frame() %>% rename_with(~ sub("^[^.]+\\.", "", .), everything()) %>% 
          dplyr::select(PGID, contains("Pred_Value")) %>% 
          group_by(PGID) %>% arrange(desc(.[,2])) %>% slice(1)
        temp_results[[sf_obj]] <- suppressMessages(output %>% left_join(temp_data))
      } else {
        cat("Warning: ", sf_obj, " does not exist in the global environment.\n")
      }
    }
    #Combine results for the current parameter
    combined_result <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), temp_results)
    combined_result <- combined_result %>% mutate(Statistic = params[i]) 
    #Create a dynamic variable name for the result
    result_name <- paste0("result_", params[i])
    # Assign the combined result to a new variable
    assign(result_name, combined_result, envir = .GlobalEnv)
  } 
}
#
plot_interpolations <- function(results_data, Site_Grid){
  #
  #Add interpolated data back to Site_grid sf object 
  (Site_Grid_interp <- left_join(Site_Grid, results_data))
  #Identify columns
  interp_cols <- grep("Pred_Value_*", names(Site_Grid_interp), value = TRUE)
  #Initiate list to store plots:
  plot_list <- list()
  #Create plots
  for(col in interp_cols){
    #Plot of binned interpolate values for rough comparison
    p <- ggplot()+
      geom_sf(data = Site_Grid_interp, aes(color = !!sym(col)))+
      theme_classic()+
      theme(panel.border = element_rect(color = "black", fill = NA), axis.text =  element_text(size = 16))+
      scale_color_viridis_b(direction = -1)+
      theme(plot.margin = unit(c(0,0,0,0), "cm"), plot.title = element_text(margin = margin(b = 5)))
    plot_list[[col]] <- p
  }
  #
  n <- length(plot_list)
  ncols <- ceiling(sqrt(n))  # Number of columns
  nrows <- ceiling(n / ncols)  # Number of rows
  #Arrange the plots in a grid:
  grid_obj <- grid.arrange(grobs = plot_list, nrow = nrows, ncol = ncols, padding = unit(0, "cm"), 
                           widths = unit(rep(1, ncols), "null"), heights = unit(rep(1, nrows), "null")) #Equal height and widths
  #
  return(list(plots = plot_list, grid = grid_obj))
}
#
final_interpolation <- function(model, selected_models, results_data, weighting, Site_Grid){
  if(model == "ensemble"){
    #Determine column names to match and limit to desired columns:
    pred_cols <- paste0("Pred_Value_", selected_models)
    result_Mean_final <- result_Mean %>% dplyr::select(Latitude:County, Statistic, all_of(pred_cols))
    #
    #Determine model weights:
    model_weighting(result_Mean_final, c(0.75, 0.25))
    ##Create ensemble values
    ens_model <- result_Mean_final %>% dplyr::select(PGID, Statistic, matches("_(idw|nn|tps|ok)$")) %>%
      mutate(Pred_Value_ens = rowSums(across(matches("_(idw|nn|tps|ok)$")) * setNames(as.list(weight_values), sub("weight_", "", names(weight_values))))) %>%
      group_by(PGID) %>% arrange(desc(Pred_Value_ens)) %>% slice(1)
    #Spatial data:
    (Site_Grid_interp <- left_join(Site_Grid, ens_model))
    #plotting
    temp <- plot_interpolations(ens_model, Site_Grid)
    #
    ##Return plots, grid, and shapefile
    return(list(plots = temp$plots, grid = temp$grid, spatialData = Site_Grid_interp))
  } else if(model == "single"){
    #Determine column names to match and limit to desired columns:
    pred_cols <- paste0("Pred_Value_", selected_models)
    result_Mean_final <- result_Mean %>% dplyr::select(Latitude:County, Statistic, all_of(pred_cols))
    #Spatial data:
    (Site_Grid_interp <- left_join(Site_Grid, result_Mean_final))
    #plotting
    temp <- plot_interpolations(result_Mean_final, Site_Grid)
    #
    ##Return plots, grid, and shapefile
    return(list(plots = temp$plots, grid = temp$grid, spatialData = Site_Grid_interp))
    #
  }
}
#
model_weighting <- function(final_data, weighting) {
  #Patterns to search for:
  patterns <- "_(idw|nn|tps|ok)$"
  #Function for equal weighting
  if (length(weighting) == 1 && weighting == "equal") {
    weights_from_columns <- function(dataframe) {
      #Identify columns that match the pattern
      matched_columns <- colnames(dataframe)[grepl(patterns, colnames(dataframe))]
      num_divisions <- length(matched_columns)  # Count the number of matched columns
      #Create weights based on the number of divisions
      if (num_divisions > 0) {
        weights <- rep(1 / num_divisions, num_divisions)
        #Create names for the weights
        names(weights) <- paste0("weight_", sub(".*_", "", matched_columns))  # Extract the pattern part
      } else {
        weights <- numeric(0)  # Return an empty numeric vector if no matches
      }
      return(weights)
    }
    return(weight_values <<- weights_from_columns(final_data))
    print(weight_values)
  } else if(is.numeric(weighting) && length(weighting) > 1) {
    weights_for_columns <- function(dataframe, weighting) {
      #Identify columns that match the pattern
      matched_columns <- colnames(dataframe)[grepl(patterns, colnames(dataframe))]
      num_divisions <- length(matched_columns)  # Count the number of matched columns
      # Check if the number of weights matches the number of matched columns
      if (length(weighting) == num_divisions) {
        # Create names for the weights
        names(weighting) <- paste0("weight_", sub(".*_", "", matched_columns))  # Extract the pattern part
        return(weighting)
      } else {
        stop("The number of weights must match the number of model columns.")
      }
    }
    return(weight_values <<- weights_for_columns(final_data, weighting))
    print(weight_values)
  } else {
    print("Numbers need to be specified.")
  }
}
#
save_model_output <- function(output_data){
  #
  library(openxlsx)
  final_output_data <- output_data
  Stat_type <- unique(final_output_data$spatialData$Statistic)
  #Save plots:
  for (i in seq_along(final_output_data$plots)){
    #Current plot
    p <- final_output_data$plots[[i]]
    p_name <- final_output_data$plots[[i]]$labels$colour
    #Desired file name and specs
    jpg_filename <- paste0("../",Site_code, "_", Version,"/Output/Figure files/", #Save location
                           #File name
                           Param_name,"_",Param_name_2,"_",Stat_type, "_", gsub(".*_","",p_name), "_", Start_year, "_", End_year, 
                           ".jpg")
    width_pixels <- 1000
    aspect_ratio <- 3/4
    height_pixels <- round(width_pixels * aspect_ratio)
    #Save plot
    ggsave(filename = jpg_filename, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
    cat("Interpolation model figure for", gsub(".*_","",p_name), "was saved in 'Output/Figure files'.", "\n")
  }
  ##End figure output
  #
  #Save shapefile:
  shape_file <- final_output_data$spatialData
  shapefile_path <- paste0("../",Site_code, "_", Version,"/Output/Shapefiles/", #Save location
                           #File name
                           Param_name,"_",Param_name_2,"_",Stat_type, "_", Start_year, "_", End_year, 
                           ".shp")
  #Save the sf dataframe as a shapefile
  suppressMessages(st_write(shape_file, shapefile_path, delete_dsn = TRUE, quiet = TRUE))
  #Print a message to confirm saving
  cat("Shapefile saved at:", shapefile_path, "\n",
      "- ", nrow(final_data$spatialData), " features saved with ", ncol(final_data$spatialData)-1, "fields")
  ##End shapefile output
  #
  #Save data to Excel sheet
  model_data <- as.data.frame(shape_file) %>% dplyr::select(-geometry)
  data_path <- paste0("../",Site_code, "_", Version,"/Output/Data files/", #Save location
                      #File name
                      Param_name,"_",Param_name_2,"_",Stat_type, "_", Start_year, "_", End_year, 
                      ".xlsx")
  #Create wb with data:
  new_wb <- createWorkbook()
  addWorksheet(new_wb, "Model_data")  # Add fresh sheet
  writeData(new_wb, sheet = "Model_data", x = model_data) 
  #Save wb
  saveWorkbook(new_wb, data_path, overwrite = TRUE)
  cat("Model data successfully saved to:\n",
      "- Sheet 'Model_data' (", nrow(model_data), " rows)\n",
      "File: ", data_path, "\n")
  ##End data output
  #
  ##Save the summary information:
  sheet_names <- excel_sheets(paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"))
  sheet_name <- paste0(Param_name, "_models")
  summ_info <- data.frame(Parameter = Param_name,
                          Type = Param_name_2,
                          Statistic = Stat_type,
                          Models = paste(final_output_data$spatialData %>% as.data.frame() %>% dplyr::select(matches("_(idw|nn|tps|ok)$")) %>% colnames(), collapse = ", "),
                          Weights = paste(as.vector(weight_values), collapse = ", "),
                          Date_range = paste0(Start_year, "-", End_year))
  #Load the workbook
  wb <- loadWorkbook(paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"))
  # Check if the sheet exists
  if (sheet_name %in% sheet_names) {
    # If it exists, overwrite the existing sheet
    existing_data <- readWorkbook(paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"), sheet = sheet_name)
    new_data <- rbind(existing_data, summ_info)
    writeData(wb, sheet = sheet_name, new_data)
  } else {
    # If it does not exist, create a new sheet
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, summ_info)
  }
  # Save the workbook
  saveWorkbook(wb, paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"), overwrite = TRUE)
  #Print a message to confirm saving
  cat("Summary information was saved as:", sheet_name, "\n")
  ##End summary output
} 
#