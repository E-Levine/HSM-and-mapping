####Water Quality Data Compilation and Cleaning###
#
##Compile WQ data - Select parameters, combine station and WQ data
##If other data sources are needed. Submit an issue/request to repo admin
##Output of cleaned data
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) install.packages("pacman")
pacman::p_unlock()
pacman::p_load(plyr, tidyverse, readxl, writexl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               sf, raster, spData, 
               tmap, tmaptools, htmltools, htmlwidgets,
               install = TRUE) 
#
####Compilation setup####
#
#Set parameters - run for each data source type
Site_code <- c("SS")       #Two letter estuary code
Version <- c("v1")         #Version ID for the model data will be used in
Data_source <- c("Portal") #Source of data: "Portal", "WA" , or "FIM"
#
#Years of data (used in file names):
Start_year <- c("2020")
End_year <- c("2024")
#
#If working with FIM data: Skip to "Estuary area", then to Mapping of stations 
#
####Load files####
#
##Read in Excel site file
Location_data <- if(Data_source == "Portal"){as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Site data_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
  } else if(Data_source == "WA"){as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Site data_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z"),
                                          col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "text", "date", "numeric", "text",
                                                        "text", "text", "text", "numeric", "text", "text", "text", "numeric", "text")))
  } else if(Data_source == "FIM"){as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z", "NULL")))
    } else {paste0("Code not yet updated for ", Data_source," data.")}
#
#Skip to "Estuary area" if using WA or FIM data.
#Read in Excel results file (for 1 file) - skip to next section if only 1 results file
Results_data <- as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Results_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#Read in Excel results file (for 2 files)
Results1 <- as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Results_", Start_year, "_", "2007",".xlsx"), na = c("NA", " ", "", "Z")))
Results2 <- as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Results_", "2008", "_", "2014",".xlsx"), na = c("NA", " ", "", "Z")))
Results3 <- as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Results_", "2015", "_", "2019",".xlsx"), na = c("NA", " ", "", "Z")))
Results4 <- as.data.frame(read_excel(paste0("Data/Raw-data/", Site_code, "_", Data_source,"_Results_", "2020", "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
Results_data <- rbind(Results1, Results2, Results3, Results4)
#If more files are needed, copy and edit the proper number of Results# lines of code and make sure to add all versions to the Results_data <- rbind() line.
#
##Estuary area  
Estuary_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Estuary_area[2])
#
##State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#END OF SECTION
#
####Select data columns####
#
##Select location data
#List of columns to keep from original file for locating/mapping stations - (minimum columns that must be included to run code)
if(Data_source == "Portal"){
keep_site <- c("MonitoringLocationIdentifier", "OrganizationIdentifier", "OrganizationFormalName", "MonitoringLocationName", 
               "MonitoringLocationTypeName", "MonitoringLocationDescriptionText", "LatitudeMeasure", "LongitudeMeasure", 
               "HorizontalCoordinateReferenceSystemDatumName", "StateCode", "CountyCode", "ProviderName")
} else if(Data_source == "WA"){
  keep_site <- c("WBodyID", "WaterBodyName", "DataSource", "StationID", "StationName", "Actual_StationID", "Actual_Latitude", "Actual_Longitude", 
                 "SampleDate", "Parameter", "Characteristic", "Result_Value", "Result_Unit")
} else if(Data_source == "FIM"){
  keep_site <- colnames(Location_data)
}
#Subset columns and add estuary ID to column
if(Data_source == "Portal"){
  Location_data <- Location_data[keep_site] %>% add_column(Estuary = Site_code, .before = "MonitoringLocationIdentifier")
} else if(Data_source == "WA"){
  Location_data <- Location_data[keep_site] %>% add_column(Estuary = Site_code, .before = "WBodyID")
} else if(Data_source == "FIM"){
  Location_data <- Location_data[keep_site] %>% add_column(Estuary = Site_code, .before = "TripID")
}
#Check columns
head(Location_data, 4)
#
#
#
##Select Results data - Portal data - Skip if using WA or FIM data
#Subset df by columns to keep - change list in include more columns as needed 
if(Data_source == "Portal"){
  keep_results_portal <- c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate", "ActivityStartTime/Time", 
                           "ActivityStartTime/TimeZoneCode", "CharacteristicName", "ResultMeasureValue", "ResultMeasure/MeasureUnitCode")
  Results <- Results_data[keep_results_portal]
} else if(Data_source == "FIM"){
  paste0("Results file not used for FIM data.")
} else {
    keep_results_atlas <- "TEMP"
    Results <- Results_data[keep_results_atlas]}
#
#
#Confirm desired columns and naming
head(Results, 4)
#
#END OF SECTION
#
####Combine data by station - limit to desired parameters####
#
if(Data_source == "Portal"){
  Combined <- merge(Location_data, Results, by = "MonitoringLocationIdentifier")
} else if(Data_source == "WA"){
  Combined <- Location_data
} else if(Data_source == "FIM"){
  Combined <- Location_data %>% 
    gather("Characteristic", "Measurement", -Estuary, -TripID, -Reference, -Sampling_Date, -Longitude, -Latitude, -Zone, -StartTime) %>% 
    mutate(Result_Unit = case_when(Characteristic == "Depth" ~ "m", Characteristic == "Temperature" ~ "degC", Characteristic == "DissolvedO2" ~ "mg/L", TRUE ~ NA))
}
#
###Filter combined file to only include specified characteristics 
#List of possible characteristics to select from
unique(Combined$CharacteristicName)
unique(Combined$Characteristic)
#
#Assemble list of characteristics to KEEP - Portal data
Characters <- c("Salinity", "Temperature, water", "Depth, bottom", "Depth, Secchi disk depth", "Temperature, air, deg C", "Turbidity", 
                "Conductivity", "Specific conductance", "pH", "Dissolved oxygen (DO)", "Dissolved oxygen saturation", 
                "Chlorophyll a, corrected for pheophytin", "Chlorophyll a", "Total dissolved solids", "Total suspended solids", "Zooplankton", 
                "Diatoms", "Stream flow, instantaneous", "Flow, severity (choice list)", "Stream stage", "Flow", "Stream flow, mean. Daily")
#
#Filter to only the desired characteristics and check remaining list
if(Data_source == "Portal"){
  Combined_filtered <- Combined %>% filter(CharacteristicName %in% Characters) %>% rename(Result_Unit = `ResultMeasure/MeasureUnitCode`)
} else if (Data_source == "WA" | Data_source == "FIM"){
  Combined_filtered <- Combined 
}
#
##Correct basic typos in units/provide clarification - mg/L, mg/m3 = ug/L
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "mg/l", "mg/L")
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "mg/m3", "ug/L")
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "ug/l", "ug/L")
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "ft3/sec", "cfs")
Combined_filtered$ResultMeasureValue <- str_replace(Combined_filtered$ResultMeasureValue, "\\*Non-detect", "NA")
#
#Confirm list of characters selected. - skip for WA or FIM data
unique(Combined_filtered$CharacteristicName)
#
#
#
#
####Map of stations and limitation of stations to estuary area####
#
#Transform CRS and data of WQ data to spatial data
if(Data_source == "Portal"){
  WQ_sp <- spTransform(SpatialPointsDataFrame(coords = Combined_filtered[,c(9,8)], data = Combined_filtered,
                                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                       "+proj=longlat +datum=WGS84 +no_defs +type=crs")
} else if(Data_source == "WA"){
  WQ_sp <- spTransform(SpatialPointsDataFrame(coords = Combined_filtered[,c(9,8)], data = Combined_filtered,
                                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                       "+proj=longlat +datum=WGS84 +no_defs +type=crs")
  paste0("Data may need to be checked. Code not finished for WQ data.")
} else if(Data_source == "FIM"){
  WQ_sp <- spTransform(SpatialPointsDataFrame(coords = Combined_filtered[,c(5,6)], data = Combined_filtered,
                                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                       "+proj=longlat +datum=WGS84 +no_defs +type=crs")
}
#
#Check CRS s
crs(Estuary_area)
crs(WQ_sp)
WQ_sp2 <- st_transform(st_as_sf(WQ_sp), crs = st_crs(Estuary_area)) 
#
##Crop to estuary area
Estuary_data <- WQ_sp[as.vector(st_intersects(Estuary_area, st_as_sf(WQ_sp), sparse = FALSE)), ]
##Run following lines only if issue with Loop above. If there is an issue with "Loop", polygons are overlapping somewhere. 
#sf_use_s2(FALSE)
#Estuary_data <- WQ_sp[lengths(st_intersects(Estuary_area, st_as_sf(WQ_sp)))>0,]
#sf_use_s2(TRUE)
#
Estuary_data@data <- Estuary_data@data %>% mutate(KML = "In") #Add KML classification for Inside KML area ##WC::Estuary_data <- as(Estuary_data %>% drop_na(MonitoringLocationIdentifier) %>% mutate(KML = "In"), "Spatial") 
Outside_data <- WQ_sp[as.vector(st_disjoint(Estuary_area, st_as_sf(WQ_sp), sparse = FALSE)), ] #SS: Outside_data <- WQ_sp2[as.vector(st_disjoint(Estuary_area, st_as_sf(WQ_sp2), sparse = FALSE)), ] 
Outside_data@data <- Outside_data@data %>% mutate(KML = "Out") #SS: Outside_data <- as((Outside_data %>% drop_na(MonitoringLocationIdentifier) %>% mutate(KML = "Out")), "Spatial")
Combined_data <- union(Estuary_data, Outside_data)
#
if(Data_source == "Portal"){
  Combined_data_counts <- SpatialPointsDataFrame(coords = (Combined_data@data %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                 data = Combined_data@data %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
} else if(Data_source == "WA"){
  Combined_data_counts <- SpatialPointsDataFrame(coords = (Combined_data@data %>% distinct(StationID, Actual_Latitude, Actual_Longitude, as.Date(SampleDate), KML) %>% group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                 data = Combined_data@data %>% distinct(StationID, Actual_Latitude, Actual_Longitude, as.Date(SampleDate), KML) %>% group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
} else if (Data_source == "FIM") {
  Combined_data_counts <- SpatialPointsDataFrame(coords = (Combined_data@data %>% distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>% group_by(Reference, Latitude, Longitude, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                 data = Combined_data@data %>% distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>% group_by(Reference, Latitude, Longitude, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  
}
#
#Visualize data locations
if(Data_source == "Portal"){
  (static <- ggplot()+
    geom_sf(data = Estuary_area, fill = "gray")+
    geom_sf(data = FL_outline) + 
    geom_point(data = Combined %>% distinct(LongitudeMeasure, LatitudeMeasure), aes(LongitudeMeasure, LatitudeMeasure), size = 2.5, color = "red")+
    coord_sf(xlim = c(st_bbox(Estuary_area)["xmin"], st_bbox(Estuary_area)["xmax"]),
             ylim = c(st_bbox(Estuary_area)["ymin"], st_bbox(Estuary_area)["ymax"])))
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         tm_shape(Combined_data_counts) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Latitude" = "LatitudeMeasure", "Longitude" = "LongitudeMeasure", "Samples" = "N")) +
                         tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", sep = " "))))
} else if(Data_source == "WA"){
  (static <- ggplot()+
     geom_sf(data = Estuary_area, fill = "gray")+
     geom_sf(data = FL_outline) + 
     geom_point(data = Combined %>% distinct(Actual_Longitude, Actual_Latitude), aes(Actual_Longitude, Actual_Latitude), size = 2.5, color = "red")+
     coord_sf(xlim = c(st_bbox(Estuary_area)["xmin"], st_bbox(Estuary_area)["xmax"]),
              ylim = c(st_bbox(Estuary_area)["ymin"], st_bbox(Estuary_area)["ymax"])))
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         tm_shape(Combined_data_counts) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "StationID", "Latitude" = "Actual_Latitude", "Longitude" = "Actual_Longitude", "Samples" = "N")) +
                         tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", sep = " "))))
} else if (Data_source == "FIM") {
  (static <- ggplot()+
     geom_sf(data = Estuary_area, fill = "gray")+
     geom_sf(data = FL_outline) + 
     geom_point(data = Combined %>% distinct(Longitude, Latitude), aes(Longitude, Latitude), size = 2.5, color = "red")+
     coord_sf(xlim = c(st_bbox(Estuary_area)["xmin"], st_bbox(Estuary_area)["xmax"]),
              ylim = c(st_bbox(Estuary_area)["ymin"], st_bbox(Estuary_area)["ymax"])))
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         tm_shape(Combined_data_counts) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "Reference", "Latitude" = "Latitude", "Longitude" = "Longitude", "Samples" = "N")) +
                         tm_layout(main.title = paste(Site_code, Data_source, "WQ Stations", sep = " "))))
}
#Save widget map:
saveWidget(map, paste0("Maps/", Site_code, "_", Data_source,"_WQ_stations_", Start_year, "_", End_year, "_widget.html"))
#
#
####Clean parameter data####
#
Combined_filteredk <- Combined_data@data 
#
if(Data_source == "Portal"){
Combined_filteredk <- Combined_filteredk %>% 
  mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
  mutate(ResultMeasureValue = case_when(CharacteristicName == "Specific conductance" & Result_Unit == "mS/cm" ~  ResultMeasureValue*1000, #Convert Spec Cond mS to uS
                                        CharacteristicName == "Stream flow, instantaneous" & Result_Unit == "ft3/s" ~ ResultMeasureValue*0.0283168, #Convert ft3 to m3
                                        TRUE ~ ResultMeasureValue),
         Result_Unit = case_when(CharacteristicName == "Salinity" ~ "ppt",  #Change all Salinity values to 'ppt' units
                                 CharacteristicName == "Conductivity" ~ "uS/cm", #Correct all Conductivity results
                                 CharacteristicName == "Specific conductance" ~ "uS/cm",#Correct Specific conductance units
                                 CharacteristicName == "pH" ~ NA,  #Correct pH units
                                 CharacteristicName == "Stream flow, instantaneous" ~ "m3/s", #Correct Stream flow units
                                 TRUE ~ Result_Unit)) %>% 
  dplyr::relocate(KML, .after = last_col())
} else if(Data_source == "WA"){
  Combined_filteredk <- Combined_filteredk %>% 
    mutate(Result_Unit = case_when(Characteristic == "Salinity" ~ "ppt",
                                   Characteristic == "pH" ~ NA, 
                                   Characteristic == 'Dissolved oxygen saturation' ~ "%", 
                                   Characteristic == 'Secchi disc depth' ~ "m", 
                                   TRUE ~ Result_Unit))
} else if(Data_source == "FIM"){
  Combined_filteredk <- Combined_filteredk
}
#
head(Combined_filteredk)
#
#Use next 2 lines if data needs to be divided due to size. Pick date and update for use.
Comb_fil_1 <- Combined_filteredk %>% filter(ActivityStartDate < "2012-01-01")
Comb_fil_2 <- Combined_filteredk %>% filter(ActivityStartDate >= "2012-01-01")
#
#
####Save filtered data####
#
#
write_xlsx(Combined_filteredk, paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_", Start_year, "_", End_year,".xlsx"), format_headers = TRUE)
#
##Dividing files - use below - modify as needed
write_xlsx(Comb_fil_2, paste0("Data/Raw-cleaned/", Site_code, "_", Data_source, "_combined_filtered_2012_", End_year,".xlsx"), format_headers = TRUE)
#
