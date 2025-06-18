####Water Quality Data Selection###
#
##Selection of WQ stations for data from cleaned files
#If other data sources are needed, submit a request to EWilliams (or via the github repo:https://github.com/E-Levine/Water-Quality-Processing)
##Output of selected data and map
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_unlock()
pacman::p_load(plyr, tidyverse, readxl, writexl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               sf, raster, spData, nngeo,
               tmap, tmaptools, htmltools, htmlwidgets,
               install = TRUE) 
#
source("Code/WQ_functions.R")
#
#
###Setup - specs####
Estuary_code <- c("SL") #Two letter estuary code
Data_source <- c("FIM") #Source of data: "Portal", "WA", "FIM"
#
#Number of files to combine (Enter 1 if only one file. Current max is 3 files):
Filtered_files <- c(1)
#Years of possible data (from file names). Start and end years required for each file. Use 'NA" for any unused files:
Start_year <- c("1997")
End_year <- c("2022")
Start_year_2 <- c(NA)
End_year_2 <- c(NA)
Start_year_3 <- c(NA)
End_year_3 <- c(NA)
#
##Years of desired data:
Begin_data <- c("2000")
End_data <- c("2023")
#
#
####Load Files#####
#
##Read in raw_cleaned data into a combined file
Filtered_data <- combine_files(Filtered_files, Estuary_code, Data_source, Start_year, End_year, Start_year_2, End_year_2, Start_year_3, End_year_2)
head(Filtered_data)
#
#
#Fixed station locations - run if including specified locations in map or selecting WQ stations based on station locations.
#Can skip if not needed.
Station_locations <- as.data.frame(read_excel("Data/Reference_data/Stations_area_selections.xlsx", na = c("NA", " ", "", "Z"))) %>% 
  subset(grepl(Estuary_code, Site))
#
##Estuary area  
Estuary_area <- st_read(paste0("../Reference files/KML/", Estuary_code, ".kml"))
plot(Estuary_area[2])
###State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#
#
#END OF SECTION
#
#
####Limit date ranges####
#
##Limit WQ to years of interest -requires Filtered_data
WQ_selected <- date_window(Begin_data, End_data)
#
#
##Code to limit list of monitoring stations if list contains more stations than needed: update sub-setting code for desired stations
if(exists("Station_locations")){
  Stations_selected <- Station_locations #%>% subset(Station < 6)
} else {
    Stations_selected <- "all"
  } 
#
#
#
#END OF SECTION
#
#
####Station map####
#
Combined_data_counts <- Spatial_data(Filtered_data)
#
#Visualize locations of WQ stations compared to fixed stations (if specified)
if(Data_source == "Portal"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         {if(length(Stations_selected) > 1) tm_shape(SpatialPointsDataFrame(coords = Stations_selected[,c(3,4)], data = Stations_selected, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))) + tm_dots(col = "darkblue", size = 1)}+
                         tm_shape(Combined_data_counts) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Latitude" = "LatitudeMeasure", "Longitude" = "LongitudeMeasure", "Samples" = "N")) +
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", sep = " "))))
} else if(Data_source == "WA"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         {if(length(Stations_selected) > 1) tm_shape(SpatialPointsDataFrame(coords = Stations_selected[,c(3,4)], data = Stations_selected, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))) + tm_dots(col = "darkblue", size = 1)}+
                         tm_shape(Combined_data_counts) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "StationID", "Latitude" = "Actual_Latitude", "Longitude" = "Actual_Longitude", "Samples" = "N")) +
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", sep = " "))))
} else if (Data_source == "FIM") {
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         {if(length(Stations_selected) > 1) tm_shape(SpatialPointsDataFrame(coords = Stations_selected[,c(3,4)], data = Stations_selected, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))) + tm_dots(col = "darkblue", size = 1)}+
                         tm_shape(Combined_data_counts) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "Reference", "Latitude" = "Latitude", "Longitude" = "Longitude", "Samples" = "N")) +
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", sep = " "))))
}
#
#
##END OF SECTION
#
#
####Station selection - specified buffer distance - run only if selecting stations based on proximity to established locations ("Station_locations")####
#
F_Distance <- c(50) #distance in meters
S_Distance <- c(200) #secondary distance for additional search area of potential stations 
#
Selections <- buffer_selection(F_Distance, S_Distance, WidgetSave = "N", Estuary_code, Data_source, Begin_data, End_data)
Selections$Selected_map
WQ_Stations <- Selections$Stations
#
#
# List of any stations to include or exclude from selection: need station ID and station of reference (both within "")- keep matched on one line/column
#If no stations need to be included or excluded, replace with NA
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"),
                         Station = c("5", "1", "1"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0146", "21FLCOSP_WQX-45-03", "21FLCOSP_WQX-CENTRAL CANAL"),
                         Station = c("4", "4", "4"))
#
##Run to include/exclude stations as specified above and save output of final data:
#BufferOrN - method of selection either "Buffer" or "N"
#ProjectCode = short letter code to specify project data gathered for: CAGE, TBWQ (general)
Selected_data(BufferOrN = "Buffer", To_include, To_exclude, ProjectCode = "SLWQ")
#
#
#           
#END OF SECTION
#
####Station selection - closest N stations####
#
##Selection continues from data narrowed by desired date range. If spatial selection is not needed, skip section.
##Desired number of WQ stations per location
Stations_N <- 5
maxDist_m <- Inf #Maximum distance (in  meters) allowed in selection. If no max, set as Inf
#
Selections_N <- Nclosest_selection(Stations_N, maxDist_m, WidgetSave = "N")
Selections_N$SelectionMap #StationsMap will return all possible stations
WQ_Stations <- Selections_N$WQclosest

# List of any stations to include or exclude from selection: need station ID and station of reference (both within "")- keep matched on one line/column
#If no stations need to be included or excluded, replace with NA
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"),
                         LocationID = c("LXN-1", "LXN-1", "LXN-1"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0146", "21FLCOSP_WQX-45-03", "21FLCOSP_WQX-CENTRAL CANAL"),
                         LocationID = c("LXN-4", "LXN-4", "LXN-4"))
#
##Run to include/exclude stations as specified above and save output of final data:
#BufferOrN - method of selection either "Buffer" or "N"
#ProjectCode = 3-4 letter code to specify project data gathered for: CAGE, TBWQ (general)
Selected_data(BufferOrN = "N", To_include, To_exclude, ProjectCode = "SLTE")
#
#
#
#END OF SECTION
#
####Station selection - by station name or designated boundary####
#
#Method of selection: Bounding_box, Station_name
Selection_Method <- c("Bounding_box")
#
##For any unused items in next few lines, enter NA
#Station_name:List of any stations to include based on station names: need station ID within "", can be partial of station name if unique to station
Selected_WQ_stations <- c("SEHAB0115", "WQX-18444", "28010458", "28010365", "BMD-2.5", "02277100")
Selected_WQ_stations <- NA
#Bounding box - enter bounding coordinates in order: West, South, East, North
bbox <- c(-80.26084, 27.19723, -80.20343, 27.22263)
bbox <- NA
#
#
#
#ProjectCode = short code to specify project data gathered for: CAGE, TBWQ (general)
WQ_stations_selected <- location_boundary(Selection_Method, Selected_WQ_stations, bbox, ProjectCode = "SLCTE", WidgetSave = "N")
WQ_stations_selected$BoundedMap #Confirm stations, can chose to include or exclude stations
#
#
#
# List of any stations to include or exclude from selection by name of station
#If no stations need to be included or excluded, replace with NA
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0146", "21FLCOSP_WQX-45-03", "21FLCOSP_WQX-CENTRAL CANAL"))
#
##Run to include/exclude stations as specified above and save output of final data:
#Use same project code from above.
Modified_data(Selection_Method, To_include, To_exclude, ProjectCode = "TEST")
#
#
#
#
##END OF SECTION
#
#
####Output all stations possible####
#
All_Stations <- output_all(WQ_selected, WidgetSave = "N")
All_Stations$AllStations
All_Stations$WQStations
#
# List of any stations to include or exclude from selection: need station ID within ""
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0169", "21FLPDEM_WQX-24-07", "21FLTPA_WQX-G5SW0187", "21FLPDEM_WQX-24-01", "21FLTPA_WQX-G5SW0152", "21FLPDEM_WQX-19-02"))
#
##Run to include and/or exclude based on lines above
#ProjectCode = 3-4 letter code to specify project data gathered for: CAGE, TBWQ (general)
finalize_data(To_include, To_exclude, ProjectCode = "SLAllWQ")
#
# 
#