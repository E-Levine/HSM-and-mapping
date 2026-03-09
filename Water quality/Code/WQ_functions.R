##Functions used for water quality selection
#
#
#
####Set up####
#
#Loading raw files
#library(readxl)
read_raw_data_files <- function(SiteCode, DataSource, year_ranges) {
  # year_ranges: a list of vectors, each with two elements: start_year and end_year
  # Example: list(c(2000, 2007), c(2008, 2014), c(2015, 2019), c(2020, 2023))
  #
  # Determine overall start and end year for site file
  overall_start_year <- as.numeric(Start_year)
  overall_end_year <- as.numeric(End_year)
  #
  ##Read in Excel site file
  Location_data <- NULL
  if(DataSource == "Portal"){
    site_file_path <- paste0("Data/Raw-data/", Site_code, "_", DataSource,"_Site data_", overall_start_year, "_", overall_end_year,".xlsx")
    Location_data <- as.data.frame(read_excel(site_file_path, na = c("NA", " ", "", "Z")))
  } else if(DataSource == "WA"){
    site_file_path <- paste0("Data/Raw-data/", Site_code, "_", DataSource,"_Site data_", overall_start_year, "_", overall_end_year,".xlsx")
    Location_data <- as.data.frame(read_excel(site_file_path, na = c("NA", " ", "", "Z"),
                                              col_types = c("text", "text", "text", "text", "text", "text", "numeric", "numeric", "text", "date", "numeric", "text",
                                                            "text", "text", "text", "numeric", "text", "text", "text", "numeric", "text")))
  } else if(DataSource == "FIM"){
    site_file_path <- paste0("Data/Raw-data/", Site_code, "_", DataSource,"_", overall_start_year, "_", overall_end_year,".xlsx")
    Location_data <- as.data.frame(read_excel(site_file_path, na = c("NA", " ", "", "Z", "NULL")))
  } else {paste0("Code not yet updated for ", DataSource," data.")}
  #
  #
  ##Get Excel Restuls file(s)
  Results_data <- NULL
  #
  if(!(DataSource %in% c("WA", "FIM"))){
    Results_list <- list()
    for (i in seq_along(year_ranges)) {
      start_year <- year_ranges[[i]][1]
      end_year <- year_ranges[[i]][2]
      #
      file_path <- paste0("Data/Raw-data/", Site_code, "_", DataSource, "_Results_", start_year, "_", end_year, ".xlsx")
      #Read the Excel file, handle NA values as specified
      Results_list[[i]] <- as.data.frame(read_excel(file_path, na = c("NA", " ", "", "Z")))
    }
    #Combine all data frames into one
    Results_data <- do.call(rbind, Results_list)
    #Save output
    assign("Results_data", Results_data, envir = .GlobalEnv)
  } else {
    message("Skipping Results_data reading for Data_source: ", DataSource)
  }
  
  #
  ##Save outputs:
  assign("Location_data", Location_data, envir = .GlobalEnv)
  message(paste0("Raw data loaded for ", SiteCode))
}
#
#Subset columns of data
#library(dplyr)
process_location_data <- function(DataSource = Data_source, LocationData = Location_data, SiteCode = Site_code) {
  # Define columns to keep based on Data_source
  if (DataSource == "Portal") {
    keep_site <- c("MonitoringLocationIdentifier", "OrganizationIdentifier", "OrganizationFormalName", "MonitoringLocationName", 
                   "MonitoringLocationTypeName", "MonitoringLocationDescriptionText", "LatitudeMeasure", "LongitudeMeasure", 
                   "HorizontalCoordinateReferenceSystemDatumName", "StateCode", "CountyCode", "ProviderName")
  } else if (DataSource == "WA") {
    keep_site <- c("WBodyID", "WaterBodyName", "DataSource", "StationID", "StationName", "Actual_StationID", "Actual_Latitude", "Actual_Longitude", 
                   "SampleDate", "Parameter", "Characteristic", "Result_Value", "Result_Unit")
  } else if (DataSource == "FIM") {
    keep_site <- colnames(LocationData)
  } else {
    stop(paste0("Data_source '", DataSource, "' not recognized."))
  }
  # Subset columns
  Location_data_sub <- LocationData[keep_site]
  # Add Estuary column before specific column depending on Data_source
  if (DataSource == "Portal") {
    Location_data_sub <- Location_data_sub %>% add_column(Estuary = SiteCode, .before = "MonitoringLocationIdentifier")
  } else if (DataSource == "WA") {
    Location_data_sub <- Location_data_sub %>% add_column(Estuary = SiteCode, .before = "WBodyID")
  } else if (DataSource == "FIM") {
    Location_data_sub <- Location_data_sub %>% add_column(Estuary = SiteCode, .before = "TripID")
  }
  glimpse(Location_data_sub)
  return(Location_data_sub)
}
#
#Subset results data
process_results_data <- function(DataSource, ResultsData){
  # Check inputs
  if (missing(DataSource) || missing(ResultsData)) {
    stop("Please provide both DataSource and ResultsData arguments.")
  }
  ##Select Results data - Portal data - not currently setup for using WA or FIM data
  #Subset df by columns to keep - change list in include more columns as needed 
  Results <- NULL
  if(!(DataSource %in% c("WA", "FIM"))){
    if(DataSource == "Portal"){
      keep_results_portal <- c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate", "ActivityStartTime/Time", 
                               "ActivityStartTime/TimeZoneCode", "CharacteristicName", "ResultMeasureValue", "ResultMeasure/MeasureUnitCode")
      cols_to_keep <- intersect(keep_results_portal, colnames(ResultsData))
      Results <- ResultsData[cols_to_keep]
    } else {
      message("Skipping Results_data subsetting for Data_source: ", DataSource)
    } 
  }
  #
  if(!is.null(Results)) {
    dplyr::glimpse(Results)
  }
  return(Results)
}
#
#Create combine location and results object:
create_combined <- function(DataSource, LocationData, ResultsData){
  if(DataSource == "Portal"){
    Combined_data <- merge(LocationData, ResultsData, by = "MonitoringLocationIdentifier")
  } else if(DataSource == "WA"){
    Combined_data <- LocationData
  } else if(DataSource == "FIM"){
    Combined_data <- LocationData %>% 
      gather("Characteristic", "Measurement", -Estuary, -TripID, -Reference, -Sampling_Date, -Longitude, -Latitude, -Zone, -StartTime) %>% 
      mutate(Result_Unit = case_when(Characteristic == "Depth" ~ "m", Characteristic == "Temperature" ~ "degC", Characteristic == "DissolvedO2" ~ "mg/L", TRUE ~ NA))
  }
  return(Combined_data)
}
#
#Station mapping
library(sf)      # for st_as_sf, st_transform
transform_crop_wq <- function(DataSource, FilteredData, EstuaryArea) {
  #Validate inputs
  if (missing(DataSource) || missing(FilteredData) || missing(EstuaryArea)) {
    stop("Please provide DataSource, FilteredData, and EstuaryArea.")
  }
  #Define source CRS string
  source_crs_str <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs"
  target_crs_str <- "+proj=longlat +datum=WGS84 +no_defs +type=crs"
  #Select coordinate columns based on DataSource
  if (DataSource == "Portal" || DataSource == "WA") {
    coords <- FilteredData[, c(9, 8)]  # longitude, latitude columns
  } else if (DataSource == "FIM") {
    coords <- FilteredData[, c(5, 6)]
  } else {
    stop(paste0("DataSource '", DataSource, "' not recognized."))
  }
  #Create sf object with source CRS 
  WQ_sf <- st_as_sf(FilteredData, coords = colnames(coords), crs = source_crs_str, remove = FALSE)  #spdf <- SpatialPointsDataFrame(coords = coords, data = FilteredData, proj4string = CRS(source_crs_str))
  #Transform CRS to WGS84
  WQ_sf <- st_transform(WQ_sf, crs = target_crs_str)#spTransform(spdf, CRS(target_crs_str))
  #Message for WA data (optional)
  if (DataSource == "WA") {
    message("Data may need to be checked. Code not finished for WQ data.")
  }
  #Check CRS of EstuaryArea and WQ_sp 
  estuary_crs <- st_crs(EstuaryArea)
  wq_sf_crs <- st_crs(st_as_sf(WQ_sf))
  print(paste("Estuary CRS:", estuary_crs))
  print(paste("Data CRS:", wq_sf_crs))
  #Convert to sf and transform to EstuaryArea CRS if needed
  if (!(estuary_crs == wq_sf_crs)) {
    WQ_sf <- st_transform(WQ_sf, crs = estuary_crs)
  } 
  #Add KML classification for points inside EstuaryArea
  inside_logical <- st_intersects(WQ_sf, EstuaryArea, sparse = FALSE)[,1]
  Inside_data <- WQ_sf[inside_logical, ] %>% mutate(KML = "In")
  #Add KML classification for points outside EstuaryArea
  outside_logical <- st_disjoint(WQ_sf, EstuaryArea, sparse = FALSE)[,1]
  Outside_data <- WQ_sf[outside_logical, ] %>% mutate(KML = "Out")
  #Combine inside and outside data
  EstuaryData <- bind_rows(Inside_data, Outside_data) %>% mutate(KML = as.factor(KML))
  return(EstuaryData)
}
#
#Color and map stations/estuary
library(sf)
library(dplyr)
library(ggplot2)
library(tmap)

mapping_wq_data_sf <- function(DataSource, WQ_sf_object, EstuaryArea, FL_outline, SiteCode) {
  #Validate inputs
  if (missing(DataSource) || missing(WQ_sf_object) || missing(EstuaryArea) || missing(FL_outline) || missing(SiteCode)) {
    stop("Please provide all required arguments: DataSource, WQ_sf_object, EstuaryArea, FL_outline, Combined, SiteCode")
  }
  #Ensure all inputs are sf objects
  if (!inherits(WQ_sf_object, "sf")) stop("WQ_sf_object must be an sf object")
  if (!inherits(EstuaryArea, "sf")) stop("EstuaryArea must be an sf object")
  if (!inherits(FL_outline, "sf")) stop("FL_outline must be an sf object")
  #
  #Summarize counts by unique station/location/date/KML based on DataSource
  if (DataSource == "Portal") {
    distinct_df <- WQ_sf_object %>% 
      distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>%
      group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>%
      summarise(N = n(), .groups = "drop") %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = st_crs(WQ_sf_object), remove = FALSE)
  } else if (DataSource == "WA") {
    distinct_df <- WQ_sf_object %>% 
      distinct(StationID, Actual_Latitude, Actual_Longitude, SampleDate, KML) %>%
      group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>%
      summarise(N = n(), .groups = "drop") %>%
      st_as_sf(coords = c("Actual_Longitude", "Actual_Latitude"), crs = st_crs(WQ_sf_object), remove = FALSE)
  } else if (DataSource == "FIM") {
    distinct_df <- WQ_sf_object %>% 
      distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>%
      group_by(Reference, Latitude, Longitude, KML) %>%
      summarise(N = n(), .groups = "drop") %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(WQ_sf_object), remove = FALSE)
  } else {
    stop("DataSource not recognized. Must be one of 'Portal', 'WA', or 'FIM'.")
  }
  #Prepare ggplot visualization
  if (DataSource == "Portal") {
    static <- ggplot() +
      geom_sf(data = EstuaryArea, fill = "gray") +
      geom_sf(data = FL_outline) +
      geom_point(data = distinct_df, aes(x = LongitudeMeasure, y = LatitudeMeasure, color = KML),
                 size = 2.5,) +
      scale_color_manual(values = c(In = "red", Out = "black")) +
      coord_sf(xlim = c(st_bbox(EstuaryArea)["xmin"], st_bbox(EstuaryArea)["xmax"]),
               ylim = c(st_bbox(EstuaryArea)["ymin"], st_bbox(EstuaryArea)["ymax"]))
    
    map <- tmap_leaflet(
      tm_shape(EstuaryArea) + tm_polygons() +
        tm_shape(FL_outline) + tm_borders() +
        tm_shape(distinct_df) +
        tm_dots(col = "KML", palette = c("red", "black"), size = 0.8, legend.show = TRUE,
                popup.vars = c("StationID" = "MonitoringLocationIdentifier",
                               "Latitude" = "LatitudeMeasure",
                               "Longitude" = "LongitudeMeasure",
                               "Samples" = "N")) +
        tm_layout(main.title = paste(SiteCode, DataSource, "WQ Stations"))
    )
  } else if (DataSource == "WA") {
    static <- ggplot() +
      geom_sf(data = EstuaryArea, fill = "gray") +
      geom_sf(data = FL_outline) +
      geom_point(data = distinct_df, aes(x = Actual_Longitude, y = Actual_Latitude, color = KML),
                 size = 2.5) +
      scale_color_manual(values = c("In" = "red", "Out" = "black")) +
      coord_sf(xlim = c(st_bbox(EstuaryArea)["xmin"], st_bbox(EstuaryArea)["xmax"]),
               ylim = c(st_bbox(EstuaryArea)["ymin"], st_bbox(EstuaryArea)["ymax"]))
    
    map <- tmap_leaflet(
      tm_shape(EstuaryArea) + tm_polygons() +
        tm_shape(FL_outline) + tm_borders() +
        tm_shape(distinct_df) +
        tm_dots("Stations", col = "KML", palette = c(In = "red", Out = "black"), size = 0.8, legend.show = TRUE,
                popup.vars = c("StationID" = "StationID",
                               "Latitude" = "Actual_Latitude",
                               "Longitude" = "Actual_Longitude",
                               "Samples" = "N")) +
        tm_layout(main.title = paste(SiteCode, DataSource, "WQ Stations"))
    )
  } else if (DataSource == "FIM") {
    static <- ggplot() +
      geom_sf(data = EstuaryArea, fill = "gray") +
      geom_sf(data = FL_outline) +
      geom_point(data = distinct_df, aes(x = Longitude, y = Latitude, color = KML),
                 size = 2.5) +
      scale_color_manual(values = c("In" = "red", "Out" = "black")) +
      coord_sf(xlim = c(st_bbox(EstuaryArea)["xmin"], st_bbox(EstuaryArea)["xmax"]),
               ylim = c(st_bbox(EstuaryArea)["ymin"], st_bbox(EstuaryArea)["ymax"]))
    
    map <- tmap_leaflet(
      tm_shape(EstuaryArea) + tm_polygons() +
        tm_shape(FL_outline) + tm_borders() +
        tm_shape(distinct_df) +
        tm_dots("Stations", col = "KML", palette = c(In = "red", Out = "black"), size = 0.8, legend.show = TRUE,
                popup.vars = c("StationID" = "Reference",
                               "Latitude" = "Latitude",
                               "Longitude" = "Longitude",
                               "Samples" = "N")) +
        tm_layout(main.title = paste(SiteCode, DataSource, "WQ Stations"))
    )
  }
  # Return a list with combined data and plots
  return(list(
    Stations_list = distinct_df,
    Static_plot = static,
    Interactive_map = map
  ))
}
#
#Save data:
library(dplyr)
library(writexl)
library(fs) # for file_info()

process_and_save_data <- function(CombinedData, DataSource, SiteCode, StartYr, EndYr) {
  #
    if (DataSource == "Portal") {
    Combined_filteredk <- CombinedData %>% 
      mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) %>%
      mutate(ResultMeasureValue = case_when(CharacteristicName == "Specific conductance" & Result_Unit == "mS/cm" ~ ResultMeasureValue * 1000, # mS/cm to uS/cm
                                            CharacteristicName == "Stream flow, instantaneous" & Result_Unit == "ft3/s" ~ ResultMeasureValue * 0.0283168, # ft3/s to m3/s
                                            TRUE ~ ResultMeasureValue),
             Result_Unit = case_when(CharacteristicName == "Salinity" ~ "ppt",
                                     CharacteristicName == "Conductivity" ~ "uS/cm",
                                     CharacteristicName == "Specific conductance" ~ "uS/cm",
                                     CharacteristicName == "pH" ~ NA_character_,
                                     CharacteristicName == "Stream flow, instantaneous" ~ "m3/s",
                                     TRUE ~ Result_Unit)) %>%
      relocate(KML, .after = last_col())
    } else if (DataSource == "WA") {
      Combined_filteredk <- CombinedData %>% 
        mutate(Result_Unit = case_when(Characteristic == "Salinity" ~ "ppt",
                                       Characteristic == "pH" ~ NA_character_,
                                       Characteristic == 'Dissolved oxygen saturation' ~ "%",
                                       Characteristic == 'Secchi disc depth' ~ "m",
                                       TRUE ~ Result_Unit))
      } else if (DataSource == "FIM") {
        # No changes needed
        Combined_filteredk <- CombinedData
      } 
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = "\nCan cleaned water quality data be saved to local folders?")
    if(result == "No"){
      message("Data will not be saved.")
    } else {
      #Save path base
      base_path <- paste0("Data/Raw-cleaned/", SiteCode, "_", DataSource, "_combined_filtered_")
      Combined_filteredk_export <- Combined_filteredk %>% mutate(geometry = sf::st_as_text(geometry))
      #Save combined filtered data temporarily to check file size
      temp_file <- tempfile(fileext = ".xlsx")
      write_xlsx(Combined_filteredk_export, temp_file, format_headers = TRUE)
      file_size_mb <- file_info(temp_file)$size / (1000^2) # size in MB
      file.remove(temp_file)
      
      if (file_size_mb <= 1000^2) {
        # Save as one file
        final_file <- paste0(base_path, StartYr, "_", EndYr, ".xlsx")
        write_xlsx(Combined_filteredk_export, final_file, format_headers = TRUE)
        message("Saved combined filtered data as one file: ", final_file)
        } else {
          #Estimate rows per chunk to keep file size under max_file_size_mb
          total_rows <- nrow(Combined_filteredk_export)
          est_rows_per_file <- floor(total_rows * 1000^2 / file_size_mb)
          est_rows_per_file <- max(est_rows_per_file, 1) # at least 1 row
          #Split data into chunks
          chunks <- split(Combined_filteredk_export, (seq_len(total_rows) - 1) %/% est_rows_per_file)
          #Save each chunk sequentially
          for (i in seq_along(chunks)) {
            chunk_file <- paste0(base_path, StartYr, "_", EndYr, "_part", i, ".xlsx")
            write_xlsx(chunks[[i]], chunk_file, format_headers = TRUE)
            message("Saved chunk ", i, " to file: ", chunk_file)
            message("File too large (", round(file_size_mb, 2), " MB). Data split and saved as ",max(chunks), " files:")
          }
        }
    }
  }
}
  #
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
  #Define target
  target_crs <- 4326
  #
  if(Data_source == "Portal"){
    # If geometry column exists as WKT text, convert it
    if ("geometry" %in% colnames(DataInput)) {
      WQ_sp <- st_as_sf(DataInput, wkt = "geometry", crs = 4326) %>% 
                          st_transform(crs = target_crs)
    } else {
      Combined_data_counts <- WQ_sf %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% 
      group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% 
        summarise(N = n(), .groups = "drop") %>% 
        st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = target_crs)
    }
  } else if(Data_source == "WA"){
    if ("geometry" %in% colnames(DataInput)) {
      WQ_sf <- st_as_sf(DataInput, wkt = "geometry", crs = 4326) %>% 
        st_transform(crs = target_crs)
    } else {
      WQ_sf <- st_as_sf(DataInput, coords = c(9,8), crs = 4326) %>% 
        st_transform(crs = target_crs)
    }
    
    Combined_data_counts <- WQ_sf %>%
      distinct(StationID, Actual_Latitude, Actual_Longitude, SampleDate, KML) %>%
      group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>%
      summarise(N = n(), .groups = "drop") %>%
      st_as_sf(coords = c("Actual_Longitude", "Actual_Latitude"), crs = target_crs)
    
  } else if (Data_source == "FIM") {
    if ("geometry" %in% colnames(DataInput)) {
      WQ_sf <- st_as_sf(DataInput, wkt = "geometry", crs = 4326) %>% 
        st_transform(crs = target_crs)
    } else {
      WQ_sf <- st_as_sf(DataInput, coords = c(5,6), crs = 4326) %>% 
        st_transform(crs = target_crs)
    }
    
    Combined_data_counts <- WQ_sf %>%
      distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>%
      group_by(Reference, Latitude, Longitude, KML) %>%
      summarise(N = n(), .groups = "drop") %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = target_crs)
  } else {
    stop("Curretnly unsupported Data_source")
  }
  
  return(Combined_data_counts)
}
#
#
#
#Mapping of stations
library(tmap)
library(tmaptools)
library(sf)

create_station_map <- function(DataSource, EstuaryArea, StateOutline, SelectedStations = NULL, Stations_sf, SiteCode) {
  #
  #Base map layers: estuary polygons and shoreline outline
  base_map <- tm_shape(EstuaryArea) + tm_polygons() + 
    tm_shape(StateOutline) + tm_borders()
  #
  #Add selected stations if more than one
  if (!is.null(SelectedStations) && nrow(SelectedStations) > 1) {
    #Convert Stations_selected to sf points (assuming columns 3=Longitude, 4=Latitude)
    stations_sf <- st_as_sf(SelectedStations, coords = c(3,4), crs = 4326)
    base_map <- base_map + tm_shape(stations_sf) + tm_dots(col = "darkblue", size = 1)
  }
  #
  #Define popup variables and station ID column based on DataSource
  if (DataSource == "Portal") {
    popup_vars <- c("StationID" = "MonitoringLocationIdentifier", 
                    "Latitude" = "LatitudeMeasure", 
                    "Longitude" = "LongitudeMeasure", 
                    "Samples" = "N")
  } else if (DataSource == "WA") {
    popup_vars <- c("StationID" = "StationID", 
                    "Latitude" = "Actual_Latitude", 
                    "Longitude" = "Actual_Longitude", 
                    "Samples" = "N")
  } else if (DataSource == "FIM") {
    popup_vars <- c("StationID" = "Reference", 
                    "Latitude" = "Latitude", 
                    "Longitude" = "Longitude", 
                    "Samples" = "N")
  } else {
    stop("Currently unsupported DataSource")
  }
  #
  #Add Stations_sf points with colored dots by KML
  map <- base_map + 
    tm_shape(Stations_sf) + 
    tm_dots(col = "KML", palette = c(In = "red", Out = "black"), size = 0.5, legend.show = TRUE,
            popup.vars = popup_vars) +
    tm_layout(main.title = paste(SiteCode, DataSource, "WQ Stations", sep = " "))
  #
  #Convert to leaflet and print
  map_leaflet <- tmap_leaflet(map)
  print(map_leaflet)
  return(map_leaflet)
}
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
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = "\nCan the selected data be saved locally to the 'Compiled-data' folder?")
    if(result == "No"){
      message("The selected data will not be saved.")
    } else {
      if(BufferOrN == "Buffer") {
        write_xlsx(WQ_stations_final_df, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_selected_buffer_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else {
        write_xlsx(WQ_stations_final_df, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_closest_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      }
    }
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
Modified_data <- function(Selection_Method, Adding = NULL, Removing = NULL, ProjectCode){
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  # Helper function to check if Adding/Removing is valid (non-null, non-NA, non-empty)
  is_valid_df <- function(df) {
    if (missing(df)) return(FALSE)
    if (is.null(df)) return(FALSE)
    if (is.na(df)) return(FALSE)
    if (is.data.frame(df) && nrow(df) == 0) return(FALSE)
    TRUE
  }
  
  hasAdding <- is_valid_df(Adding)
  hasRemoving <- is_valid_df(Removing)
  #
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = "\nCan the modified data be saved locally to the 'Compiled-data' folder?")
    if(result == "No"){
      message("The selected data will not be saved.")
      return(invisible(NULL))
    }
  }
  # Initialize WQ_stations_final
  WQ_stations_final <- NULL
  #Both Adding and Removing missing or invalid:
  if(!hasAdding && !hasRemoving){
    if(Selection_Method == "Station_name"){
      #Selection by station name - no changes
      WQ_stations_final <- WQ_stations_selected
      #write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      #
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - no changes
        WQ_stations_final <- WQ_stations_selected$BoundedStations
        #write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else if(Selection_Method == "Estuary"){
        WQ_stations_final <- WQ_stations_selected$BoundedStations
        #write_xlsx(WQ_stations_final, paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_estuary_area_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else {
        stop(paste0("Code not yet written for using ", Selection_Method, "."))
      }
    #END BOTH NA
  } else {
    #At least one of Adding or Removing is valid
    #
    #Helper: get base stations depending on Selection_Method
    get_base_stations <- function() {
      if (Selection_Method == "Station_name") {
        return(WQ_stations_selected)
      } else if (Selection_Method %in% c("Bounding_box", "Estuary")) {
        return(WQ_stations_selected$BoundedStations)
      } else {
        stop(paste0("Code not yet written for using ", Selection_Method, "."))
      }
    }
    base_stations <- get_base_stations()
    #
    #Helper: subset and join for Adding
    add_stations <- function(base, adding_df) {
      if (!hasAdding) return(base)
      if (Data_source == "Portal") {
        added <- WQ_selected %>% 
          filter(MonitoringLocationIdentifier %in% adding_df$StationID) %>% 
          left_join(adding_df, by = c("MonitoringLocationIdentifier" = "StationID"))
      } else if (Data_source == "FIM") {
        added <- WQ_selected %>% 
          filter(Reference %in% adding_df$StationID) %>% 
          left_join(adding_df, by = c("Reference" = "StationID"))
      } else {
        stop(paste0("Adding stations by name for ", Data_source, " is not yet supported."))
      }
      return(bind_rows(base, added))
    }
    #Helper: remove stations
    remove_stations <- function(df, removing_df) {
      if (!hasRemoving) return(df)
      if (Data_source == "Portal") {
        return(df %>% filter(!MonitoringLocationIdentifier %in% removing_df$StationID))
      } else if (Data_source == "FIM") {
        return(df %>% filter(!Reference %in% removing_df$StationID))
      } else {
        stop(paste0("Removing stations by name for ", Data_source, " is not yet supported."))
      }
    }
    #
    # Compose final stations
    WQ_stations_final <- add_stations(base_stations, Adding)
    WQ_stations_final <- remove_stations(WQ_stations_final, Removing)
    #
  }
      #Write output file if saving is enabled and WQ_stations_final is valid
      if (exists("result") && result == "Yes" && !is.null(WQ_stations_final)) {
        # Compose filename suffix based on Selection_Method
        suffix <- switch(Selection_Method,
                         Station_name = "name_selected",
                         Bounding_box = "bounding_box",
                         Estuary = "estuary_area",
                         "unknown_method")
        WQ_stations_final_export <- WQ_stations_final %>% mutate(geometry = sf::st_as_text(geometry))
        filename <- paste0("Data/Compiled-data/", Site_code, "_", Data_source, "_", suffix, "_", Project_code, "_", Begin_data, "_", End_data, ".xlsx")
        #
        write_xlsx(WQ_stations_final_export, filename, format_headers = TRUE)  
      } 
      
      print(head(WQ_stations_final))
      return(invisible(WQ_stations_final))
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