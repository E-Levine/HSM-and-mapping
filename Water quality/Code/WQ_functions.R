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
#Load WQ data file
load_WQ_data <- function(){
  if(Folder == "compiled" && interactive()){
    result <- select.list(c("Yes", "No"), title = "\nCan the data be saved locally to the project version folder?")
    files <- list.files(path = "Data/Compiled-data/", 
                        pattern = paste0(Site_code, "_", Data_source, "_.*_", Project_code, "_", Start_year, "_", End_year,".xlsx"))
    WQ_data <- read_excel(paste0("Data/Compiled-data/", files[1]), na = c("NA", " ", "", "Z")) %>%
      dplyr::rename(Latitude = contains("Latitude"), Longitude = contains("Longitude"), StationID = contains("LocationIdentifier"),
                    Parameter = contains("CharacteristicName"), Value = contains("MeasureValue"))
    
    #Check if Latitude and Longitude columns exist and are not all NA
    lat_exists <- "Latitude" %in% colnames(WQ_data) && any(!is.na(WQ_data$Latitude))
    long_exists <- "Longitude" %in% colnames(WQ_data) && any(!is.na(WQ_data$Longitude))
    
    if(!(lat_exists && long_exists)){
      #Try to find a geometry column (common names: geometry, geom, Shape, WKT, etc.)
      geom_col <- grep("geometry|geom|shape|wkt", tolower(colnames(WQ_data)), value = TRUE)
      if(length(geom_col) > 0){
        #Use first geometry column found
        geom_col <- geom_col[1]
        #Convert to sf object assuming WKT format
        sf_points <- tryCatch({
          st_as_sfc(WQ_data[[geom_col]], crs = 4326)
        }, error = function(e) NULL)
        if(!is.null(sf_points)){
          coords <- st_coordinates(sf_points)
          WQ_data$Longitude <- coords[,1]
          WQ_data$Latitude <- coords[,2]
        } else {
          message("Geometry column found but could not parse coordinates.")
        }
      } else {
        message("No geometry column found and Latitude/Longitude missing.")
      }
    }
    
    WQ_data <<- WQ_data
    #
    if(result == "Yes"){
      write_xlsx(WQ_data, paste0("../",Site_code, "_", Version,"/Data/", Site_code, "_cleaned_WQ_data.xlsx"), format_headers = TRUE)
      } else {
        message("A copy of the data will not be saved to the project folder.")
        } 
    } else {
      paste("Code needs to be updated for 'final' folder location.")
    }
}

#
#
#Load and clip state grids to estuary area
#library(sf)
#library(dplyr)
load_site_grid <- function(StateGrid, SiteArea, Alt_Grid = NA) {
  #Load primary PicoGrid
  PicoGrid <- st_read(
    paste0("../Reference files/Grids/Florida_PicoGrid_WGS84_", StateGrid, "/Florida_PicoGrid_WGS84_", StateGrid, "_clip.shp"),
    quiet = TRUE
  )
  #Load alternative PicoGrid if provided and not NA
  if (!is.na(Alt_Grid)) {
    Alt_PicoGrid <- st_read(
      paste0("../Reference files/Grids/Florida_PicoGrid_WGS84_", Alt_Grid,"/Florida_PicoGrid_WGS84_", Alt_Grid, "_clip.shp"),
      quiet = TRUE
    )
  }
  #Filter grids by intersection with Site_area
  PicoGrid_clipped <- PicoGrid[lengths(st_intersects(PicoGrid, SiteArea)) > 0, ]
  
  if (!is.na(Alt_Grid)) {
    Alt_PicoGrid_clipped <- Alt_PicoGrid[lengths(st_intersects(Alt_PicoGrid, SiteArea)) > 0, ]
    #Combine clipped grids
    Site_Grid <- bind_rows(PicoGrid_clipped, Alt_PicoGrid_clipped) %>%
      rename(Longitude = Long_DD_X, Latitude = Lat_DD_Y)
    #Clean up
    rm(PicoGrid, Alt_PicoGrid, PicoGrid_clipped, Alt_PicoGrid_clipped)
  } else {
    Site_Grid <- PicoGrid_clipped %>%
      rename(Longitude = Long_DD_X, Latitude = Lat_DD_Y)
    rm(PicoGrid, PicoGrid_clipped)
  }
  return(Site_Grid)
}
#
summarize_data <- function(data_frame = WQ_data, Parameter_name = Param_name, Time_period = c("Year", "Month", "Quarter"), Year_range = "NA", Quarter_start = NA, Month_range = NA, Summ_method = c("Means", "Mins", "Maxs", "Range", "Range_values", "Threshold"), Threshold_parameters = c(NA, "above", "below")) {
  #
  Time_period <- match.arg(Time_period)
  Summ_method <- match.arg(Summ_method)
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
  ##Set threshold variables:
  if(Summ_method == "Threshold" && !(Threshold_parameters[1] %in% c("above", "below"))){
    stop("Threshold_parameters must be one of: above of below \n
    Threshold_parameters must also contain a numeric value.")
  }
  #
  if(Summ_method == "Threshold" && Threshold_parameters[1] %in% c("above", "below")){
    threshold_value <<- as.numeric(Threshold_parameters[2])
  }
  #
  #Function to filter and prepare temp_df based on Parameter_name
  get_temp_df <- function(param_name) {
    data_frame %>% 
      #Filter to desired parameter
      dplyr::filter(str_detect(Parameter, param_name)) %>%
      #Add in missing group columns
      mutate(Year = year(as.Date(ActivityStartDate)),
             Month = month(as.Date(ActivityStartDate), label = TRUE)) %>%
      #Assign quarters, starting at month specified or default start of January
      {if(!is.na(Quarter_start)) mutate(., Quarter = set_quarters(as.Date(ActivityStartDate), Quarter_start)) else mutate(., Quarter = quarter(as.Date(ActivityStartDate)))} %>%
      #Filter to specified months if applicable
      {if(length(Month_range) == 2) filter(., between(month(as.Date(ActivityStartDate)), Month_range[1], Month_range[2])) else . }
  }
  #
  #Initial filtering
  temp_df <- get_temp_df(Parameter_name)
  #List unique Parameter values in temp_df
  unique_params <- unique(data_frame$Parameter)
  cat("Current paramter value:\n")
  print(Parameter_name)
  cat("\n Unique Parameter values found:\n")
  print(unique_params)
  #
  #Prompt to continue or update Parameter_name:
  if(interactive()){
    repeat {
      user_input <- readline(prompt = "Type 'c' to continue with current Parameter_name, or enter a new Parameter_name to update: \n(Note: Changing the parameter value here will change the 'Param_name' object. \n Enter a new name without quotation marks.)")
      user_input <- trimws(user_input)
      if(tolower(user_input) == "c") {
        # Continue with current Parameter_name
        cat(paste("Continuing with current parameter:", Parameter_name))
        break
      } else if(nchar(user_input) > 0) {
        # Update Parameter_name and re-filter
        Parameter_name <- user_input
        Param_name <<- user_input
        temp_df <- get_temp_df(Parameter_name)
        cat("Updated to parameter value:", Parameter_name, "\n")
        break
      } else {
        cat("Invalid input. Please type 'c' or enter a new Parameter_name.\n")
      }
    }
  } else {
    cat("Non-interactive session detected; continuing with current Parameter_name.\n")
  }
  #Continue with grouping and summarizing
  temp_df <- temp_df %>%
    #Grouping for evals: station, specified time period
    group_by(Estuary, Latitude, Longitude, Parameter, !!sym(Time_period))
  #
  ##Summarize data using method specified
  if(Summ_method == "Means"){
    summary_data <- station_means(temp_df, Year_range, Start_year, End_year, Time_period)  
  } else if(Summ_method == "Mins"){
    summary_data <- station_mins(temp_df, Year_range, Start_year, End_year, Time_period)  
  } else if(Summ_method == "Maxs"){
    summary_data <- station_maxs(temp_df, Year_range, Start_year, End_year, Time_period)  
  } else if(Summ_method == "Range"){
    summary_data <- station_range(temp_df, values = "No", Year_range, Start_year, End_year, Time_period)  
  } else if(Summ_method == "Range_values"){
    summary_data <- station_range(temp_df, values = "Yes", Year_range, Start_year, End_year, Time_period)  
  } else if(Summ_method == "Threshold"){
    summary_data <- station_threshold(temp_df, Year_range, Start_year, End_year, Time_period, Threshold_parameters, threshold_value)  
  } else {
    stop("Summarization method supplied is incorrectly specified or is not currently suppported.")
  }
  #
  output_data <- summary_data %>% ungroup() %>% 
    pivot_longer(cols = intersect(c("Mean", "Minimum", "Maximum", "Range", "Threshold"), names(summary_data)), names_to = "Statistic", values_to = "Value") %>%
    pivot_wider(names_from = "Parameter", values_from = "Value") %>% 
    dplyr::select(any_of(c("Year", "Month", "Quarter")), Longitude, Latitude, Statistic, all_of(Param_name)) %>% drop_na() %>% ungroup() %>%
    rename(Working_Param = any_of(Param_name))
  #
  message(Param_name," summarized by ", Summ_method)
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
station_means <- function(df, Range, StartYr, EndYr, Time_period){
  temp <- df %>% 
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% 
    {if (Time_period == "Year") group_by(., Year, Estuary, Latitude, Longitude, Parameter) 
      else if (Time_period == "Month") group_by(., Month, Estuary, Latitude, Longitude, Parameter)
      else if (Time_period == "Quarter") group_by(., Quarter, Estuary, Latitude, Longitude, Parameter)
      else (.)} %>%
    summarise(Mean = mean(Value, na.rm = TRUE))
  return(temp)
}
#Minimums of parameter
station_mins <- function(df, Range, StartYr, EndYr, Time_period){
  temp <- df %>%  
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% 
    {if (Time_period == "Year") group_by(., Year, Estuary, Latitude, Longitude, Parameter) 
      else if (Time_period == "Month") group_by(., Month, Estuary, Latitude, Longitude, Parameter)
      else if (Time_period == "Quarter") group_by(., Quarter, Estuary, Latitude, Longitude, Parameter)
      else (.)} %>%
    summarise(Minimum = min(Value, na.rm = TRUE))
  return(temp)
}
#Maximums of parameter
station_maxs <- function(df, Range, StartYr, EndYr, Time_period){
  temp <- df %>%  
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% 
    {if (Time_period == "Year") group_by(., Year, Estuary, Latitude, Longitude, Parameter) 
      else if (Time_period == "Month") group_by(., Month, Estuary, Latitude, Longitude, Parameter)
      else if (Time_period == "Quarter") group_by(., Quarter, Estuary, Latitude, Longitude, Parameter)
      else (.)} %>%
    summarise(Maximum = max(Value, na.rm = TRUE))
  return(temp)
}
#Range of parameter: either the range (values = N) or the min and max (values = Y)
station_range <- function(df, values, Range, StartYr, EndYr, Time_period){
  temp <- df %>%  
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% 
    {if (Time_period == "Year") group_by(., Year, Estuary, Latitude, Longitude, Parameter) 
      else if (Time_period == "Month") group_by(., Month, Estuary, Latitude, Longitude, Parameter)
      else if (Time_period == "Quarter") group_by(., Quarter, Estuary, Latitude, Longitude, Parameter)
      else (.)} %>%
    summarise(Maximum = max(Value, na.rm = TRUE),
              Minimum = min(Value, na.rm = TRUE)) %>%
    {if(values == "Yes") . else mutate(., Range = Maximum - Minimum) %>% dplyr::select(., -Maximum, -Minimum) }
  return(temp)
}
#Threshold of parameter: above or below a value
station_threshold <- function(df, Range, StartYr, EndYr, Time_period, Threshold_parameters, threshold_value){
  #Filtering and grouping
  temp_raw <- df %>% 
    {if(!is.na(Range)) filter(., Year >= StartYr & Year <= EndYr) else . } %>%
    ungroup() %>% 
    {if (Time_period == "Year") group_by(., Year, Estuary, Latitude, Longitude, Parameter) 
      else if (Time_period == "Month") group_by(., Month, Estuary, Latitude, Longitude, Parameter)
      else if (Time_period == "Quarter") group_by(., Quarter, Estuary, Latitude, Longitude, Parameter)
      else (.)} 
  #Calculate number above/below threshold and total number observations
  temp <- left_join(temp_raw %>% {if (length(Threshold_parameters) == 2 && Threshold_parameters[1] %in% c("above", "below") && is.numeric(threshold_value)) {
    if (Threshold_parameters[1] == "above") {
      filter(., Value > threshold_value)
    } else if (Threshold_parameters[1] == "below") {
      filter(., Value < threshold_value)
    }
  } else {
    stop("Invalid Threshold_parameters format. Must specify either 'above' or 'below' and specify the numeric value for the threshold.")
  }
  } %>%
    summarise(Count = n()),
  temp_raw %>% summarise(Total = n())) %>%
    #Proportion of samples related to threshold
    mutate(Threshold = Count/Total)
  #
  temp <- rbind(temp, temp_raw %>% anti_join(temp) %>% summarise(Total = n()) %>% mutate(Count = 0, Threshold = 0/Total))
    return(temp)
}
#
#
####Interpolation####
#
perform_idw_interpolation <- function(Site_data_spdf, grid, Site_Grid_spdf, Parameter) {
  Param_name <- Parameter
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Progress bar setup
  pb <- progress_bar$new(
    format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
    total = (length(stats) * 4) + 2,
    complete = "=", incomplete = "-", current = ">",
    clear = FALSE, width = 100, show_after = 0, force = TRUE)
  pb_active <- TRUE
  #
  cat("Starting time:", format(Sys.time()), "\n")
  #
  tryCatch({
    pb$tick(tokens = list(step = "Set up"))
    Sys.sleep(1/1000)
    #Notify if Threshold statistic is present
    if(any(stats == "Threshold")){
      cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.\n")
      }
    #Initialize lists to store results
    idw_nn <- list()
    idw_Site <- list()
    #
    #Convert Site_Grid_spdf polygons to sf and get centroids
    site_sf <- st_as_sf(Site_Grid_spdf)
    centroids_sf <- st_centroid(site_sf)
    #
    for(i in seq_along(stats)) {
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      #Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == stats[i], ]
      #
      #IDW interpolation (power=2 by default)
      idw_model <- suppressMessages(idw(formula = Working_Param ~ 1, locations = stat_data, newdata = grid, idp = 2))
      #
      #Convert to data.frame and rename columns
      idw_df <- as.data.frame(idw_model) %>% rename(Longitude = x1, Latitude = x2, Prediction = var1.pred) %>%
        mutate(Pred_Value = round(Prediction, 2), Statistic = stats[i]) %>% dplyr::select(-var1.var)
      #
      ##PROCESSING:
      pb$tick(tokens = list(step = "Processing"))
      Sys.sleep(1/1000)
      #
      #Convert to SpatialPointsDataFrame
      coordinates(idw_df) <- ~Longitude + Latitude
      proj4string(idw_df) <- proj4string(idw_model)
      #
      #Create Voronoi polygons clipped to grid extent
      idw_nn[[i]] <- dismo::voronoi(idw_df, ext = raster::extent(grid))
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #
      # Convert voronoi polygons to sf
      voronoi_sf <- st_as_sf(idw_nn[[i]])
      #
      #Spatial join: assign Voronoi polygon values to centroids, join centroids with voronoi polygons by spatial intersection
      centroids_joined <- st_join(centroids_sf, voronoi_sf[, c("Pred_Value")], left = TRUE)
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #
      #Rename prediction column
      centroids_joined <- centroids_joined %>%
        rename(Pred_Value_idw = Pred_Value)
      #
      #Join centroid predictions back to Site_Grid polygons by row order (assuming same order)
      site_sf_temp <- site_sf %>% left_join(st_drop_geometry(centroids_joined)[, c("PGID", "Pred_Value_idw")], by = "PGID")
      idw_Site[[i]] <- site_sf_temp
      #
    }
    })
  #
  pb$tick(tokens = list(step = "Completed processing"))
  Sys.sleep(1/1000)
  cat("Ending time:", format(Sys.time()), "\n")
  #
  pb$terminate()
  pb_active <- FALSE
  return(idw_Site)
}
#
perform_nn_interpolation <- function(Site_data_spdf, Site_area, Site_Grid, Site_Grid_df, Parameter = Param_name, WQ_summ) {
  Param_name <- Parameter
  WQsumm <- WQ_summ
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  # Create a progress bar
  pb <- progress_bar$new(format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
                         total = (length(stats) * 3)+2,  
                         complete = "=", incomplete = "-", current = ">",
                         clear = FALSE, width = 100, show_after = 0, force = TRUE)
  pb_active <- TRUE
  #
  cat("Starting time:", format(Sys.time()), "\n")
  #Initiate lists 
  nn_model <- list()
  nn_Site <- list()
  #
  #
  tryCatch({
    pb$tick(tokens = list(step = "Set up"))
    Sys.sleep(1/1000)
    #Notify if Threshold statistic is present
    if(any(stats == "Threshold")){
      cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.\n")
    }
    #
    #Convert Site_Grid_spdf polygons to sf
    site_sf <- st_as_sf(Site_Grid_spdf)
    centroids_sf <- st_centroid(site_sf)
    # 
    #Loop over each statistic
    for(i in seq_along(stats)){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      WQ_data_stat <- WQsumm %>% filter(Statistic == stats[i])
      ##NN: Convert to terra vect and create Voronoi polygons clipped to Site_area
      vect_data <- vect(WQ_data_stat, geom = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")
      nn_model[[i]] <- st_as_sf(voronoi(x = vect_data, bnd = Site_area))
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #Assign predictions to grid
      nn_joined <- st_join(centroids_sf, nn_model[[i]][, c("Working_Param")], left = TRUE)
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      nn_joined <- nn_joined %>% dplyr::rename(Pred_Value_nn = Working_Param)
      #
      #Join centroid predictions back to Site_Grid polygons
      site_sf_temp <- site_sf %>% left_join(st_drop_geometry(nn_joined)[, c("PGID", "Pred_Value_nn")], by = "PGID")
      nn_Site[[i]] <- site_sf_temp
      }
  })
  #
  pb$tick(tokens = list(step = "Completed processing"))
  Sys.sleep(1/1000)
  cat("Ending time:", format(Sys.time()), "\n")
  #
  pb$terminate()
  pb_active <- FALSE
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
    for(i in seq_along(stats)){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == stats[i], ]
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
      tps_Site[[i]] <- st_intersection(Site_Grid %>% dplyr::select(Latitude:MGID), st_as_sf(tps_model[[i]])) %>% 
        mutate(Statistic = stats[i], .before = Latitude)
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #Rename column based on model type
      names(tps_Site[[i]])[names(tps_Site[[i]]) == "lyr.1"] <- "Pred_Value_tps"
      #
      pb$message(paste("Completed:", stats[i], Param_name))
    }
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  #Print note if threshold is being used:
  if(any(stats == "Threshold")){
    cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.")
  }
  return(tps_Site)
}
#
perform_ok_interpolation <- function(Site_data_spdf, grid, Site_Grid, Site_Grid_spdf, Parameter = Param_name) {
  Param_name <- Parameter
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Initiate lists 
  ok_output <- list()
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
    for(i in seq_along(stats)){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == stats[i], ]
      stat_temp <- as.data.frame(stat_data) %>% group_by(Longitude, Latitude, Statistic) %>%
        summarize(Working_Param = mean(Working_Param, na.rm = TRUE))
      coordinates(stat_temp) <- ~Longitude + Latitude  # Replace with your actual coordinate columns
      proj4string(stat_temp) <- CRS(proj4string(stat_data))  # Keep the same projection as the original
      stat_data <- stat_temp
      ##IOK: model(Parameter), data to use, grid to apply to 
      ok_fit <- autofitVariogram(Working_Param ~ 1, stat_data, miscFitOptions = list(merge.small.bins = FALSE))
      ok_model <- gstat(formula = Working_Param~1, locations = stat_data, model = ok_fit$var_model, data = st_as_sf(stat_data))
      ok_pred <- predict(ok_model, grid)
      #Convert to data frame to rename and add parameters levels as values rounded to 0.1
      ok_output[[i]] <- as.data.frame(ok_pred) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
        mutate(Pred_Value = round(Prediction, 2), Statistic = stats[i]) %>% dplyr::select(-var1.var)
      #
      ##PROCESSING:
      pb$tick(tokens = list(step = "Processing"))
      Sys.sleep(1/1000)
      #Convert interpolated values to spatial data
      ok_spdf[[i]] <- SpatialPointsDataFrame(coords = ok_output[[i]][,c("Longitude","Latitude")], data = ok_output[[i]][c("Statistic", "Pred_Value")], 
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
      pb$message(paste("Completed:", stats[i], Param_name))
      
    }
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  #Print note if threshold is being used:
  if(any(stats == "Threshold")){
    cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.")
  }
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
      existing_object <- sf_obj  # Store the last existing object
    }
  }
  #Check the number of existing objects
  if (existing_count == 0) {
    stop("Error: None of the model outputs exist in the global environment.")
  } else if (existing_count == 1) {
    stop(paste("Error: Only one model output exists: ", existing_object," Creation of ensemble model not applicable"))
  }
  #Get the list of statistics/list names:
  params <- unique(unlist(lapply(get(existing_object), function(df) unique(df$Statistic))))
  #
  #Iterate over each parameter
  for (i in seq_along(params)){
    temp_results <- list()
    #Iterate through each possible SF object
    for (sf_obj in sf_objects) {
      if (exists(sf_obj, envir = .GlobalEnv)) {
        cat("Joining ", sf_obj, " with existing data...\n")
        temp_data <- get(sf_obj)[[i]] %>% as.data.frame() %>% rename_with(~ sub("^[^.]+\\.", "", .), everything()) %>% 
          dplyr::select(PGID, Statistic, contains("Pred_Value")) %>% 
          group_by(PGID) %>% arrange(desc(across(contains("Pred_Value")))) %>% slice(1)
        temp_results[[sf_obj]] <- suppressMessages(output %>% left_join(temp_data))
      } else {
        cat("Warning: ", sf_obj, " does not exist in the global environment.\n")
      }
    }
    #Combine results for the current parameter
    combined_result <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), temp_results)
    #Create a dynamic variable name for the result
    result_name <- paste0("result_", params[i])
    # Assign the combined result to a new variable
    assign(result_name, combined_result, envir = .GlobalEnv)
  }
  #Print note if threshold is being used:
  if(any(params == "Threshold")){
    cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.")
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
      {if(any(Site_Grid_interp$Statistic == "Threshold")) labs(caption = "Values = Threshold sample proportions")}+
      theme(plot.margin = unit(c(0,0,0,0), "cm"), plot.title = element_text(margin = margin(b = 5)), plot.caption = element_text(face = "italic", size = 9))
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
final_interpolation <- function(model = c("ensemble", "single"), selected_models = c("idw", "nn", "tps", "ok"), results_data, weighting, Site_Grid){
  model <- match.arg(model)
  matched_models <- selected_models[selected_models %in% c("idw", "nn", "tps", "ok")]
  if (length(matched_models) > 0) {
    cat("Matched models:", matched_models, "\n")
  } else {
    cat("No matched models found.\n")
  }
  if(model == "ensemble"){
    #Determine column names to match and limit to desired columns:
    pred_cols <- paste0("Pred_Value_", selected_models)
    result_data_final <- results_data %>% dplyr::select(Latitude:County, Statistic, all_of(pred_cols))
    #
    #Determine model weights:
    model_weighting(result_data_final, c(0.75, 0.25))
    ##Create ensemble values
    ens_model <- result_data_final %>% dplyr::select(PGID, Statistic, matches("_(idw|nn|tps|ok)$")) %>%
      mutate(Pred_Value_ens = rowSums(across(matches("_(idw|nn|tps|ok)$")) * setNames(as.list(weight_values), sub("weight_", "", names(weight_values))))) %>%
      group_by(PGID) %>% arrange(desc(Pred_Value_ens)) %>% slice(1)
    #Spatial data:
    (Site_Grid_interp <- left_join(Site_Grid, ens_model))
    #plotting
    temp <- plot_interpolations(ens_model, Site_Grid)
    #
    if(any(Site_Grid_interp$Statistic == "Threshold")){
      cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.")
    }
    ##Return plots, grid, and shapefile
    return(list(plots = temp$plots, grid = temp$grid, spatialData = Site_Grid_interp))
  } else if(model == "single"){
    #Determine column names to match and limit to desired columns:
    pred_cols <- paste0("Pred_Value_", selected_models)
    result_data_final <- results_data %>% dplyr::select(Latitude:County, Statistic, all_of(pred_cols))
    #Spatial data:
    (Site_Grid_interp <- left_join(Site_Grid, result_data_final))
    #plotting
    temp <- plot_interpolations(result_data_final, Site_Grid)
    #
    if(any(Site_Grid_interp$Statistic == "Threshold")){
      cat("Threshold evaluation is being used. Values are the proportion of all samples above or below the set threshold value.")
    }
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
save_model_output <- function(output_data, Month_range = NA, threshold_val = threshold_value){
  #
  if(all(is.na(Month_range)) && interactive()){
    result <- select.list(c("Yes", "No"), title = "\nNo month range has been specified for the data. Is this correct?")
    if(result == "Yes"){cat("Continuing... \n")} else {stop("Please specify Month_range.")}
  } else if(any(is.na(Month_range))){
    stop("Saving stopped: Only one month has been specified properly in Month_range.")
  } else {
    cat("Months have been specified for the data. Continuing ... \n")
  }
  library(openxlsx)
  if(all(!is.na(Month_range))){
    Start_month <- month.abb[Month_range[1]]
    End_month <- month.abb[Month_range[2]]
  }
  threshold_val <- threshold_val
  final_output_data <- output_data
  Stat_type <- unique(final_output_data$spatialData$Statistic)

  #Save plots:
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = paste0("\nShould the plots the chosen interpolation models be saved locally to the '", Site_code, "_", Version,"' project folder?"))
    if(result == "No"){
      message("Interpolation plots will not be saved.")
    } else {
      for (i in seq_along(final_output_data$plots)){
        #Current plot
        p <- final_output_data$plots[[i]]
        p_name <- final_output_data$plots[[i]]$labels$colour
        #Desired file name and specs
        jpg_filename <- paste0("../",Site_code, "_", Version,"/Output/Figure files/", #Save location
                               #File name
                               if(all(!is.na(Month_range))){
                                 paste0(Param_name,"_",Param_name_2,"_",Stat_type, "_", gsub(".*_","",p_name), "_", Start_year, "_", End_year, "_", Start_month, "_", End_month)
                               } else {
                                 paste0(Param_name,"_",Param_name_2,"_",Stat_type, "_", gsub(".*_","",p_name), "_", Start_year, "_", End_year)
                               }, 
                               ".jpg")
        if(is.numeric(threshold_val)) {jpg_filename <- sub("\\.jpg$", paste0("_", threshold_val, ".jpg"), jpg_filename)} else {jpg_filename <- jpg_filename}
        width_pixels <- 1000
        aspect_ratio <- 3/4
        height_pixels <- round(width_pixels * aspect_ratio)
        #Save plot
        ggsave(filename = jpg_filename, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
        cat("Interpolation model figure for", gsub(".*_","",p_name), "model was saved in 'Output/Figure files'.", "\n")
      }
    }
  }
  ##End figure output
  #
  #
  #Save shapefile:
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = paste0("\nShould the shapefile of chosen interpolation values be saved locally to the '", Site_code, "_", Version,"' project folder?"))
    if(result == "No"){
      message("Shapefile will not be saved.")
    } else {
      shape_file <- final_output_data$spatialData
      shapefile_path <- paste0("../",Site_code, "_", Version,"/Output/Shapefiles/", #Save location
                               #File name
                               if(all(!is.na(Month_range))){
                                 paste0(Param_name,"_",Param_name_2,"_",Stat_type, "_", Start_year, "_", End_year, "_", Start_month, "_", End_month)
                               } else {
                                 paste0(Param_name,"_",Param_name_2,"_",Stat_type, "_", Start_year, "_", End_year)
                               }, 
                               ".shp")
      if(is.numeric(threshold_val)) {shapefile_path <- sub("\\.shp$", paste0("_", threshold_val, ".shp"), shapefile_path)} else {shapefile_path <- shapefile_path}
      #Save the sf dataframe as a shapefile
      suppressMessages(st_write(shape_file, shapefile_path, delete_dsn = TRUE, quiet = TRUE))
      #Print a message to confirm saving
      cat("Shapefile saved at:", shapefile_path, "\n",
          "- ", nrow(final_data$spatialData), " features saved with ", ncol(final_data$spatialData)-1, "fields")
    }
  }
  ##End shapefile output
  #
  #
  #Save data to Excel sheet
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = paste0("\nShould an Excel file with chosen interpolation data be saved locally to the '", Site_code, "_", Version,"' project folder?"))
    if(result == "No"){
      message("Interpolation data will not be saved in an Excel file.")
    } else {
      model_data <- as.data.frame(shape_file) %>% dplyr::select(-geometry)
      data_path <- paste0("../",Site_code, "_", Version,"/Output/Data files/", #Save location
                          #File name
                          if(all(!is.na(Month_range))){
                            paste0(Param_name,"_",Param_name_2,"_",Stat_type, "_", Start_year, "_", End_year, "_", Start_month, "_", End_month)
                          } else {
                            paste0(Param_name,"_",Param_name_2,"_",Stat_type, "_", Start_year, "_", End_year)
                          }, 
                          ".xlsx")
      if(is.numeric(threshold_val)) {data_path <- sub("\\.xlsx$", paste0("_", threshold_val, ".xlsx"), data_path)} else {data_path <- data_path}
      #Create wb with data:
      new_wb <- createWorkbook()
      addWorksheet(new_wb, "Model_data")  # Add fresh sheet
      writeData(new_wb, sheet = "Model_data", x = model_data) 
      #Save wb
      saveWorkbook(new_wb, data_path, overwrite = TRUE)
      cat("Model data successfully saved to:\n",
          "- Sheet 'Model_data' (", nrow(model_data), " rows)\n",
          "File: ", data_path, "\n")
    }
  }
  ##End data output
  #
  #
  ##Save the summary information:
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = paste0("\nShould summary data for the chosen interpolation models and output be saved locally to the '", Site_code, "_", Version,"' project folder?"))
    if(result == "No"){
      message("Summary data will not be saved.")
    } else {
      sheet_names <- excel_sheets(paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"))
      sheet_name <- "Interpolation_Summary"
      summ_info <- data.frame(Parameter = Param_name,
                              Type = Param_name_2,
                              Statistic = Stat_type,
                              Models = paste(final_output_data$spatialData %>% as.data.frame() %>% dplyr::select(matches("_(idw|nn|tps|ok)$")) %>% colnames(), collapse = ", "),
                              Weights = paste(as.vector(weight_values), collapse = ", "),
                              Date_range = paste0(Start_year, "-", End_year),
                              Months = if(all(!is.na(Month_range))){paste0(Start_month, "-", End_month)} else {paste("All")},
                              Threshold_value = threshold_val,
                              Date_update = Sys.Date())
      #Load the workbook
      wb <- loadWorkbook(paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"))
      #Check if the sheet exists
      if (sheet_name %in% sheet_names) {
        #If it exists, append data to the existing sheet
        existing_data <- readWorkbook(paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"), sheet = sheet_name)
        new_data <- rbind(existing_data, summ_info)
        writeData(wb, sheet = sheet_name, new_data)
      } else {
        #If it does not exist, create a new sheet
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, summ_info)
      }
      #Save the workbook
      saveWorkbook(wb, paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx"), overwrite = TRUE)
      #Print a message to confirm saving
      cat("Summary information was saved within:", sheet_name, "\n")
    }
  }
  ##End summary output
} 
#
chunked_intersection <- function(polygons, site, chunk_size = 1000) {
# Split polygons into chunks
n <- nrow(polygons)
chunks <- split(polygons, (seq_len(n) - 1) %/% chunk_size)

# Process each chunk
result <- map_dfr(chunks, function(chunk) {
  st_intersection(site, chunk) %>%
    st_make_valid() %>%  # Ensure valid geometries
    suppressWarnings()   # Quiet common minor warnings
})
return(result)
}
