##Flow and salinity data 
#
#Code to relate flow (cfs) to logger salinity data
#
#Files should be named at minimum: SiteCode_logger_[salinity|flow].
#
## Packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, data.table,#Df manipulation, basic summary
               readxl, openxlsx, progress, writexl,
               slider, minpack.lm, #rolling mean
               sf, sp, terra, furrr, future,
               mgcv, fpc, fields, interp, #interpolation, fpc::bscan - clustering
               geospehere, igraph, leaflet, #Cluster points
               RColorBrewer, magicfor, ecorest, #HSV scoring
               gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
#
Site_code <- c("SS")       #Two letter estuary code
Version <- c("v1")         #For saving plots
Start_year <- c("2020")
End_year <- c("2024")
#
### USGS data collection ####
#
library(dataRetrieval)
#
#Parameter code: 00060 = mean daily discharge, 00061 = instantaneous discharge, 00480 = Salinity
#Statistic_id: 00003 = Mean
(temp_data <- read_waterdata_daily(monitoring_location_id = c("USGS-02323592", "USGS-02313700", "USGS-02324170", "USGS-02313272"), 
                                  parameter_code = c("00060", "00061"),
                                  properties = c("value", "statistic_id", "monitoring_location_id", "parameter_code", "time", "unit_of_measure"),
                                  skipGeometry = TRUE))
#
clean_save_usgs_data <- function(rawDF, startDate, endDate, dataType){
  # Date range (use lubridate for robustness)
  start <- lubridate::ymd(startDate)
  end <- lubridate::ymd(endDate)
  Type <- dataType
  if (is.na(start) | is.na(end)) stop("Invalid date format; use YYYY-MM-DD")
  #
  # Data filtering
  data <- rawDF %>% 
    rename(TIMESTAMP = time, VALUE = value, STATION = monitoring_location_id) %>%
    dplyr::filter(TIMESTAMP >= start & TIMESTAMP <= end) %>%
    mutate(PARAMETER = case_when(
      parameter_code == "00060" ~ "FLOW_d", 
      parameter_code == "00061" ~ "FLOW_i", 
      parameter_code == "00480" ~ "Salinity",
      TRUE ~ NA_character_))
  #
  # Check for data
  if (nrow(data) == 0) stop("No data found in the specified date range")
  #
  start_ym <- format(start, "%Y%m")  # e.g., "202301"
  end_ym <- format(end, "%Y%m")      # e.g., "202312"
  data_path <- paste0("Data/Raw-data/", Site_code, "_logger_", Type, "_", start_ym, "_", end_ym,".xlsx")
  #Create wb with data:
  sheetName = paste0("logger_", Type)
  new_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(new_wb, sheetName)  # Add fresh sheet
  openxlsx::writeData(new_wb, sheet = sheetName, x = data) 
  #Save wb
  openxlsx::saveWorkbook(new_wb, data_path, overwrite = TRUE)
  cat("Logger data successfully saved to:\n",
      "- Sheet '",sheetName,"' (", nrow(data), " rows)\n",
      "File: ", data_path, "\n")
  #
}
#
clean_save_usgs_flow(temp_data, "2020-01-01", "2024-12-31", "flow")
#
#
#
### Data from cleaned Storet files ####
# Name of file to use

clean_save_existing_data <- function(fileName, dataType){
  # Data type/parameter
  Type <- dataType
  #
  filePath <- paste0("Data/Raw-cleaned/", fileName, ".xlsx")
  loaded <- read.xlsx(filePath, sheet = "Sheet1")
  #
  # Data filtering
  data <- loaded %>% 
    filter(CharacteristicName == "Salinity") %>% 
    dplyr::select("STATION" = MonitoringLocationName, 
                  "Latitude" = LatitudeMeasure, 
                  "Longitude" = LongitudeMeasure, 
                  "TIMESTAMP" = ActivityStartDate, 
                  "PARAMETER" = CharacteristicName, 
                  "VALUE" = ResultMeasureValue)
  #
  # Check for data
  if (nrow(data) == 0) stop("No data found in the specified date range")
  #
  start_ym <- format(as.Date(min(data$TIMESTAMP)), "%Y%m")  # e.g., "202301"
  end_ym <- format(as.Date(max(data$TIMESTAMP)), "%Y%m")      # e.g., "202312"
  data_path <- paste0("Data/Raw-data/", Site_code, "_logger_", Type, "_", start_ym, "_", end_ym,".xlsx")
  #Create wb with data:
  sheetName = paste0("logger_", Type)
  new_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(new_wb, sheetName)  # Add fresh sheet
  openxlsx::writeData(new_wb, sheet = sheetName, x = data) 
  #Save wb
  openxlsx::saveWorkbook(new_wb, data_path, overwrite = TRUE)
  cat("Logger data successfully saved to:\n",
      "- Sheet '",sheetName,"' (", nrow(data), " rows)\n",
      "File: ", data_path, "\n")
  #
}
#
clean_save_existing_data("SS_Portal_combined_filtered_2020_2024", "Salinity")
#
#
#
#
### Data gather and cleaning####
## Load data (logger_flow and logger_salinity files) requires xlsx files
#Make sure only desired logger data files are in the main folder
load_WQ_data <- function(){
  Stations <- openxlsx::read.xlsx(file.path("Data/Raw-data/Flow_logger_locations.xlsx"), na.strings = c("NA", " ", "", "Z"), detectDates = TRUE) %>%
    dplyr::filter(Site == Site_code)
    flow_file <- list.files(path = "Data/Raw-data/", 
                            pattern = paste0(Site_code, "_logger_flow_.*.xlsx"))
    if(length(flow_file) == 0) stop("No flow file found for Site_code: ", Site_code)
    flow_raw <- openxlsx::read.xlsx(file.path("Data/Raw-data/", flow_file[1]), na.strings = c("NA", " ", "", "Z"), detectDates = TRUE)
    #
    salinity_file <- list.files(path = "Data/Raw-data/", 
                                 pattern = paste0(Site_code, "_logger_[Ss]alinity_.*.xlsx"))
    if(length(salinity_file) == 0) stop("No salinity file found for Site_code: ", Site_code)
    #
    salinity_raw <- openxlsx::read.xlsx(file.path("Data/Raw-data/", salinity_file[1]), na.strings = c("NA", " ", "", "Z"), detectDates = TRUE)
    # Return both items to work with:
    assign("Loggers", Stations, envir = .GlobalEnv)
    assign("flow_raw", flow_raw, envir = .GlobalEnv)
    assign("salinity_raw", salinity_raw, envir = .GlobalEnv)
}
#
load_WQ_data()
#
## Clean data
#
# If many points, can simplify into groups with averaged values
# library(geosphere, igraph, dplyr, leaflet)
# df should have columns: ID, Latitude, Longitude, Value
# distance_threshold in meters (e.g., 2000)
cluster_points <- function(df, distance_threshold, Site = Site_code) {
  SiteCode <- Site
  # Extract coordinates
  coords <- df[, c("Longitude", "Latitude")]
  
  # Compute pairwise distances using Haversine formula (in meters)
  dist_mat <- geosphere::distm(coords, fun = geosphere::distHaversine)
  
  # Ability to check number of groups based on distance and change distance if desired:
  repeat{
    # Create adjacency matrix: TRUE if distance < threshold
    adj_mat <- dist_mat < distance_threshold
    diag(adj_mat) <- FALSE  # No self-connections
    
    # Build undirected graph
    g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected")
    
    # Find connected components (groups)
    comp <- components(g)
    
    # Inform user about number of groups
    num_groups <- comp$no
    cat("Number of groups created with distance threshold", distance_threshold, "meters:", num_groups, "\n")
    
    # Ask user to continue or change threshold
    response <- readline(prompt = "Do you want to continue with this grouping? (y/n): ")
    if (tolower(response) == "y") {
      break  # Exit the loop and proceed
    } else {
      new_threshold <- as.numeric(readline(prompt = "Enter new distance threshold in meters: "))
      distance_threshold <- as.numeric(new_threshold)
      # Loop again with new threshold
    }
  }
  
  # Assign group IDs to original dataframe
  df$groupID <- comp$membership
  
  # Compute group summaries: centroid (mean lat/lon) and mean value
  group_summary <- df %>%
    mutate(groupID = paste0(SiteCode, "Sal", groupID),
           PARAMETER = "Salinity") %>%
    group_by(groupID, TIMESTAMP, PARAMETER) %>%
    summarise(
      grpLat = mean(Latitude, na.rm = T),
      grpLong = mean(Longitude, na.rm = T),
      VALUE = mean(VALUE, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::rename("STATION" = groupID)
  
  # Lat Long to add to Loggers
  group_locations <- group_summary %>%
    group_by(STATION) %>%
    summarise(
      Latitude = mean(grpLat, na.rm = T),
      Longitude = mean(grpLong, na.rm = T),
      .groups = "drop"
    ) %>%
    mutate(Site = SiteCode,
           DataType = "Salinity") %>%
    dplyr::rename("StationID" = STATION)
  
  
  # Create interactive map
  # Color palette for group IDs
  pal <- colorFactor(palette = "Set1", domain = df$groupID)
  
  m <- leaflet() %>%
    addTiles() %>%
    # Original points, colored by groupID
    addCircleMarkers(
      data = df,
      lng = ~Longitude,
      lat = ~Latitude,
      color = ~pal(groupID),
      popup = ~paste("ID:", STATION, "<br>Group:", groupID, "<br>Value:", VALUE)
    ) %>%
    # Group centroids
    addMarkers(
      data = group_locations,
      lng = ~Longitude,
      lat = ~Latitude,
      popup = ~paste("Group:", StationID, "<br>Latitude:", Latitude, "<br>Longitude:", Longitude)
    ) %>%
    addLegend("bottomright", pal = pal, values = df$groupID, title = "Group ID")
  
  print(m)
  # Return a list containing the modified dataframe, group summaries, and map
  return(list(data = group_summary, locations = group_locations, map = m))
}
# Example usage:
sali_grps <- cluster_points(salinity_raw, 7500)
# View the map: sali_grps$map
# Access modified data: sali_grps$data
# Access group summaries: sali_grps$groups#
#
# Add group station locations to Loggers data frame in R and Excel data file
update_logger_locations <- function(new_locations){
  # Columns and file path
  key_columns = c("Site", "StationID", "Latitude", "Longitude", "DataType")
  file_path <- "Data/Raw-data/Flow_logger_locations.xlsx"
  #
  
  All_loggers <- read.xlsx(file_path, na.strings = c("NA", " ", "", "Z"), detectDates = TRUE)
  # Check for duplicates: Identify new rows that don't already exist in Loggers
  existing_keys <- All_loggers %>% dplyr::select(all_of(key_columns)) %>% mutate(across(c(Latitude, Longitude), ~round(.x, 5)))
  new_keys <- new_locations %>% dplyr::select(all_of(key_columns)) %>% mutate(across(c(Latitude, Longitude), ~round(.x, 5)))
  new_unique <- anti_join(new_keys, existing_keys, by = key_columns)
  
  #
  if (nrow(new_unique) == 0) {
    cat("No new locations to add (all are duplicates).\n")
  } else {
    cat("Adding", nrow(new_unique), "new locations.\n")
  }
  #
  # Combine existing Loggers with new unique locations
  updated_All_loggers <- rbind(All_loggers, new_unique)
  updated_Loggers <- rbind(Loggers, new_unique)
  #
  # Save the updated dataframe back to the Excel file (overwrites the original)
  write.xlsx(updated_All_loggers, file_path, overwrite = TRUE)
  cat("Updated file saved to:", file_path, "\n")
  #
  # Return the updated dataframe for further use
  return(updated_Loggers)
  #
}
#
updated_Loggers <- update_logger_locations(sali_grps$locations)
Loggers <- updated_Loggers
#
#
# Total daily flow for each logger
flow_ave <- flow_raw %>% 
  rename_with(~str_to_title(.x)) %>%
  rename("Date" = Timestamp) %>%
  mutate(Site = Site_code, Date = as.Date(Date)) %>% 
  # If using all stations as 1 run next line, if stations should be separate, remove line
  #mutate(Station = "ALL") %>%
  group_by(Site, Date, Station, Parameter) %>% 
  summarise(Flow = mean(Value, na.rm = T)) %>% 
  ungroup()
# Mean daily salinity for each logger: either salinity_raw if no grouping, sali_grps$data if grouped
salinity_ave <- sali_grps$data %>% #salinity_raw %>% 
  rename_with(~str_to_title(.x)) %>%
  rename("Date" = Timestamp) %>%
  mutate(Site = Site_code, Date = as.Date(Date)) %>% 
  group_by(Site, Date, Station, Parameter) %>% 
  summarise(Salinity = mean(Value, na.rm = T)) %>% 
  ungroup()
#
#
#
## Get monthly means
# Function to calculate mean monthly salinity library(dplyr, lubridate)
# Input: df (data frame with 'Date' as Date class and 'Salinity' as numeric)
# Output: A summary data frame with Year, Month, and Mean_Salinity
# Calculates pre salinity logger or total all flow
calculate_monthly_value <- function(df, value_col = "Salinity") {
  # Check for required columns and formats
  if (!"Date" %in% colnames(df)) {
    stop("Error: The data frame must contain a 'Date' column.")
  }
  if (!value_col %in% colnames(df)) {
    stop("Error: The data frame must contain a '",value_col,"' column.")
  }
  if (!inherits(df$Date, "Date")) {
    stop("Error: The 'Date' column must be of class 'Date'. Convert it using as.Date() if necessary.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Error: The '",value_col,"' column must be numeric.")
  }
  #
  # Ensure data is sorted by Date
  df <- df %>% arrange(Date)
  # Dynamic naming
  output_col <- paste0("Mean_", value_col)
  #
  # Calculate mean monthly salinity
  monthly_salinity <- df %>%
    mutate(Year = year(Date), Month = month(Date), Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    group_by(Date, Year, Month, Station) %>%
    summarise(!!output_col := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  #
  return(monthly_salinity)
}
#
(sal_monthly <- calculate_monthly_value(salinity_ave, "Salinity"))
(flow_monthly <- calculate_monthly_value(flow_ave, "Flow"))
#
#
#
### Model fit and plot ####
## Combined data frame - not currently helpful
#monthly_data <- left_join(sal_monthly, flow_monthly)
#head(monthly_data)
#
## Fit curve
#library(stringr, minpack.lm, dplyr)
fit_salinity_flow_models <- function(flow_data, salinity_data, flow_col = "Mean_Flow", salinity_col = "Mean_Salinity"){
  # Assumptions:
  # - flow_data: data frame with columns for Date, Station, and Mean_Flow
  # - salinity_data: data frame with columns for Date, Station, and Mean_Salinity
  # - The function pairs each salinity station with each flow station by matching on Date
  # - It performs an inner join on Date, so only matching time points are used
  # - Requires at least 4 data points for fitting (to avoid underdetermined models)
  station_col <- "Station"
  time_col <- "Date"
  # Get unique station IDs
  salinity_stations <- unique(salinity_data[[station_col]])
  flow_stations <- unique(flow_data[[station_col]])
  # Initialize a list to store results
  results <- list()
  results_data <- list()
  
  # Loop over each combination
  for (sal_station in salinity_stations) {
    for (flow_station in flow_stations) {
      # Subset salinity data for the current station
      sal_sub <- salinity_data %>%
        dplyr::filter(.data[[station_col]] == sal_station) %>%
        dplyr::select(.data[[time_col]], !!salinity_col := .data[[salinity_col]])
      
      # Subset flow data for the current station
      flow_sub <- flow_data %>%
        dplyr::filter(.data[[station_col]] == flow_station) %>%
        dplyr::select(.data[[time_col]], !!flow_col := .data[[flow_col]])
      
      # Merge on time (inner join to get matching time points)
      combined <- inner_join(sal_sub, flow_sub, by = time_col) %>% drop_na()
      
      # Check if there are enough data points (at least 4 for NLS)
      if (nrow(combined) >= 4) {
        # Build the formula dynamically
        formula_str <- paste0(salinity_col, " ~ y0 + (a * b) / (b + ", flow_col, ")")
        
        # Attempt to fit the model
        fit <- try(
          nlsLM(
            as.formula(formula_str),
            data = combined,
            start = list(
              y0 = min(combined[[salinity_col]], na.rm = TRUE),
              a  = max(combined[[salinity_col]], na.rm = TRUE) - min(combined[[salinity_col]], na.rm = TRUE),
              b  = median(combined[[flow_col]], na.rm = TRUE)
            )
          ),
          silent = TRUE
        )
        results_data[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- combined
        # Store the summary if fit succeeded, otherwise store an error message
        if (!inherits(fit, "try-error")) {
          results[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- summary(fit)
        } else {
          results[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- paste("Fit failed for salinity station", sal_station, "and flow station", flow_station, ":", attr(fit, "condition")$message)
        }
      } else {
        results[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- paste("Insufficient data for salinity station", sal_station, "and flow station", flow_station, "(only", nrow(combined), "matching time points)")
      }
    }
  }
  # Print the list of all model names (combinations) that were attempted
  print("Models attempted (salinityStation_flowStation):")
  print(names(results))
  assign("Model_data", results_data, envir = .GlobalEnv)
  return(results)
}
#
models <- fit_salinity_flow_models(flow_monthly, sal_monthly)
#
remove_models <- function(results, models_to_remove) {
  # Filter the results list to remove specified models
  filtered_results <- results[!names(results) %in% models_to_remove]
  
  # If Model_data exists in the global environment, filter it as well
  if (exists("Model_data", envir = .GlobalEnv)) {
    Model_data <- get("Model_data", envir = .GlobalEnv)
    Model_data <- Model_data[!names(Model_data) %in% models_to_remove]
    assign("Model_data", Model_data, envir = .GlobalEnv)
  }
  
  return(filtered_results)
}
#
models <- remove_models(models, c("SSSal1_USGS-02313700", "SSSal3_USGS-02313700", "SSSal4_USGS-02313700", "SSSal5_USGS-02313700"))
#
# Calculate flow at specified salinity (from HSM curves)
flow_at_salinity_hyp2 <- function(results, target_sal) {
  # results: output from fit_salinity_flow_models (list of summaries or error messages)
  # target_sal: target salinity value
  
  # Initialize a data frame to store results
  flow_results <- data.frame(
    salinity_station = character(),
    flow_station = character(),
    flow_at_target = numeric(),
    status = character(),
    stringsAsFactors = FALSE
  )
  # Loop through each result
  for (model_name in names(results)) {
    result <- results[[model_name]]
    
    # Split the model name to get salinity and flow stations (assuming format: sal_station_flow_station)
    parts <- str_split(model_name, "_", n = 2)[[1]]
    sal_station_clean <- parts[1]
    flow_station_clean <- parts[2]
    if (inherits(result, "summary.nls")) {
      # Successful fit: extract coefficients and compute flow
      p <- coef(result)
      y0 <- p[1]
      a  <- p[2]
      b  <- p[3]
      
      if (target_sal <= y0) {
        flow_val <- NA
        status <- "Target salinity <= y0; flow cannot be solved"
      } else {
        flow_val <- (a * b) / (target_sal - y0) - b
        status <- "Success"
      }
    } else {
      # Failed fit or insufficient data
      flow_val <- NA
      status <- result  # Use the error message as status
    }
    # Append to results data frame
    flow_results <- rbind(flow_results, data.frame(
      salinity_station = sal_station_clean,
      flow_station = flow_station_clean,
      flow_at_target = flow_val,
      status = status,
      stringsAsFactors = FALSE
    ))
  }
  return(flow_results)
}
#
adult <- rbind(
  flow_at_salinity_hyp2(models, 11.98) %>% mutate(Sal = "min"), 
  flow_at_salinity_hyp2(models, 35.98) %>% mutate(Sal = "max")) %>% mutate(Type = "Adult")
larvae <- rbind(
  flow_at_salinity_hyp2(models, 10.01) %>% mutate(Sal = "min"), 
  flow_at_salinity_hyp2(models, 31.49) %>% mutate(Sal = "max")) %>% mutate(Type = "Larave")
#
# Plot fit - option to add green fill over optimal salinity range and/or flow range
ggplot_hyperbolic_fit <- function(resultsdf, results, model_name, flow_col = "Flow", value_col = "Salinity", 
                                  Salinity_min = NULL, Salinity_max = NULL, Flow_min = NULL, Flow_max = NULL) {
  df <- resultsdf[[model_name]]
  # Check if the model exists and is successful
  if (!(model_name %in% names(resultsdf))) {
    stop("Model name not found in results.")
  }
  fit <- results[[model_name]]
  # Input validation
  if (!all(c(flow_col, value_col) %in% names(df))) {
    stop("Data frame must contain the specified flow and value columns.")
  }
  coefs <- coef(fit)
  if (!is.numeric(df[[flow_col]]) || !is.numeric(df[[value_col]])) {
    stop("Specified columns must be numeric.")
  }
  # Extract parameters
  p <- coef(fit)
  y0 <- p[1]
  a  <- p[2]
  b  <- p[3]
  
  # Build prediction grid
  xseq <- seq(min(df[[flow_col]]), max(df[[flow_col]]), length.out = 300)
  pred <- y0 + (a * b) / (b + xseq)
  
  pred_df <- data.frame(
    flow = xseq,
    fitted = pred
  ) %>% rename(!!flow_col := flow)
  
  ggplot(df, aes(x = .data[[flow_col]], y = .data[[value_col]])) +
    {if(!is.null(Salinity_min) && !is.null(Salinity_max)) annotate("rect", xmin=-Inf, xmax=Inf, ymin=Salinity_min, ymax=Salinity_max, alpha=0.6, fill="#B8FFB8")}+
    {if(!is.null(Flow_min) && !is.null(Flow_max)) annotate("rect", xmin=Flow_min, xmax=Flow_max, ymin=-Inf, ymax=Inf, alpha=0.6, fill="#97FFFF")}+
    geom_point(color = "gray30", size = 2) +
    geom_line(data = pred_df,
              aes(x = .data[[flow_col]], y = fitted),
              color = "blue", linewidth = 1.2) +
    scale_y_continuous(expand = c(0.005,0.1))+ scale_x_continuous(expand = c(0.005,0.1))+
    labs(
      x = flow_col,
      y = value_col,
      title = paste(model_name),
      subtitle = paste0("Hyperbolic Fit: y =",round(y0,2), " + (", round(a,2), "*", round(b,2), ")/(",round(b,2)," + x)", collapse = "")
    ) +
    theme_classic()
}
#
names(models)
ggplot_hyperbolic_fit(Model_data, models, "SSSal1_USGS-02323592", "Mean_Flow", 
                      "Mean_Salinity", Salinity_min = 11.98, Salinity_max = 38.95)
#ggsave(path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
#       filename = paste("Flow_salinity_curve_", "SS5_SS",".tiff", sep = ""), dpi = 1000)
#
#ggplot_hyperbolic_fit(monthly_data, fit_sp, "Mean_Flow", "Mean_Salinity",
 #                     Salinity_min = 11.98, Salinity_max = 35.98,
  #                    Flow_min = 0, Flow_max = 907.26)
#
#
#
#
### Parameter values and saving ####
#
## Determine mean number of days in year within range 
#min and max Dates to include
optimal_flow_days <- function(df, Station_name, minDate, maxDate, minFlow, maxFlow){
  df %>% 
    # Filter data to date range
    filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    # Count if within ideal range
    mutate(Year = year(Date), 
           Station = Station_name, 
           Conditions = case_when(Flow < maxFlow & Flow > minFlow ~ 1,  
                                  TRUE ~ 0)) %>%
    # Group by year and count number of good days
    group_by(Year, Station) %>%
    summarise(Days = sum(Conditions)) %>% 
    ungroup() %>% group_by(Station) %>%
    # Mean number of annual days within ideal flow at logger point
    summarise(meanDays = mean(Days)) 
}
#
#Calculates the mean optimal days for each station 
automate_optimal_df <- function(df, flow_ave_df, start_date, end_date, dfType){
  # Get unique combinations of salinity_station and flow_station
  unique_combos <- df %>%
    dplyr::select(salinity_station, flow_station) %>%
    dplyr::distinct()
  
  # Initialize a list to store results for each combination
  results <- list()
  
  # Loop over each unique combination
  for (i in 1:nrow(unique_combos)) {
    salStation <- unique_combos$salinity_station[i]
    flowStation <- unique_combos$flow_station[i]
    
    # Filter the adult dataframe for the current combination
    combo_data <- df %>% 
      dplyr::filter(salinity_station == salStation & flow_station == flowStation)
    
    # Compute min and max Sal for the combination (assuming 'Sal' is the salinity column)
    min_flow <- as.numeric(ifelse(is.na((combo_data %>% filter(Sal == "min"))$flow_at_target), 
                                 -900,  
                                 (combo_data %>% filter(Sal == "min"))$flow_at_target))
    max_flow <- as.numeric(ifelse(is.na((combo_data %>% filter(Sal == "max"))$flow_at_target), 
                                 -900,  
                                 (combo_data %>% filter(Sal == "max"))$flow_at_target))
    
    # Call optimal_flow_days with the flow_station and computed min/max Sal as minFlow/maxFlow
    opt_flow <- optimal_flow_days(flow_ave_df, salStation, start_date, end_date, min_flow, max_flow)
    
    # Store the result with a key like "salinity_station_flow_station"
    key <- paste(salStation, flowStation, sep = "_")
    results[[key]] <- opt_flow
  }
  
  # Combine all results into a single dataframe
  if (length(results) == 0) {
    stop("No valid combinations found.")
  }
  combined <- do.call(rbind, results)
  
  # Add the Type column
  combined <- combined %>% 
    dplyr::mutate(Type = dfType) %>%
    group_by(Station, Type) %>%
    summarise(meanDays = mean(meanDays),
              .groups = "drop")
    
  
  return(combined)
}
#
Adult_optimal <- automate_optimal_df(adult, flow_ave, "2020-01-01", "2024-12-31", "Adult")
Larvae_optimal <- automate_optimal_df(larvae, flow_ave, "2020-01-01", "2024-12-31", "Larvae")
#
#(Adult_optimal <- rbind(
#  #HR1
#  optimal_flow_days(flow_ave, "SSSal1","2020-01-01", "2024-12-31", -105.71566, -98.60245),
#  #STLRIVER 
#  optimal_flow_days(flow_ave, "STLRIVER","2020-01-01", "2024-12-31", -294, 784.0671),
#  #STLSTPT
#  optimal_flow_days(flow_ave, "STLSTPT","2020-01-01", "2024-12-31", -415, 8103.8245)
#  ) %>% mutate(Type = "Adult"))
#(Larvae_optimal <- rbind(
#  #HR1
#  optimal_flow_days(flow_ave, "HR1","2020-01-01", "2024-12-31", -202, 770.6214),
#  #STLRIVER 
#  optimal_flow_days(flow_ave, "STLRIVER","2020-01-01", "2024-12-31", -210, 1064.4825),
#  #STLSTPT
#  optimal_flow_days(flow_ave, "STLSTPT","2020-01-01", "2024-12-31", -79, 14472.53761)
#) %>% mutate(Type = "Larvae"))
#
#
# Count number of days in month more than 1.5 SD from monthly mean
count_outlier_flow_days <- function(df, minDate, maxDate, flow_col = "Flow") {
  #
  df %>%
    # Filter data to date range
    filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    #Get Year and Month
    mutate(Year  = year(Date),
           Month = month(Date)) %>%
    group_by(Year, Month) %>%
    # Determine mean monthly flow, 1.5 SD, and identify outliers
    mutate(
      mean_flow = mean(.data[[flow_col]], na.rm = TRUE), #mean monthly flow
      sd_flow   = sd(.data[[flow_col]], na.rm = TRUE), #SD monthly flow
      outlier   = abs(.data[[flow_col]] - mean_flow) > 1.5 * sd_flow,
      within_1.5sd = !outlier
    ) %>%
    # Count number of outliers per MonYr, and mean flow
    summarise(
      days_outlier_flow = sum(outlier, na.rm = TRUE),
      mean_flow = (first(mean_flow)),
      .groups = "drop"
    ) %>%
    # Calculate mean number of days per month and mean flow
    summarise(mean_outlier_days = mean(days_outlier_flow),
              mean_flow = mean(mean_flow))
    }
#
outlier_flow <- count_outlier_flow_days(flow_ave, "2020-01-01", "2024-12-31", "Flow") %>% mutate(Type = "All")
#
#
#
# Save data and/or figure created
save_flow_output <- function(adultFlow, larvaeFlow, adultOptimal, larvaeOptimal, outlierFlow){
  #
  Logger_stations <- "Flow_stations"
  FlowSalinity <- "Flow_at_salinity"
  Optimal <- "Flow_optimal_days"
  Outlier <- "Flow_outlier_days"
  #
  if(interactive()){
    result<- select.list(c("Yes", "No"), title = paste0("\nCan a summary of the flow curve results be saved locally to the version tracking file?"))
    if(result == "No"){
      message("Flow curve output will NOT be saved to the model version tracking file.")
    } else {
      # Define workbook path
      wb_path <- paste0("../",Site_code, "_", Version, "/Data/", Site_code, "_", Version, "_model_setup.xlsx")
      
      # Load the workbook (or create if it doesn't exist)
      if (file.exists(wb_path)) {
        wb <- loadWorkbook(wb_path)
      } else {
        stop("Setup file does not exist or cannot be found.")
      }
      
      # Get existing sheet names
      existing_sheets <- sheets(wb)

      # Save logger location output
      temp_logger <- get("Loggers", envir = .GlobalEnv)
      if (Logger_stations %in% existing_sheets) {
        writeData(wb, sheet = Logger_stations, temp_logger)
      } else {
        addWorksheet(wb, Logger_stations)
        writeData(wb, sheet = Logger_stations, temp_logger)
      }
      # Save flow at salinity output
      temp_data_flow <- rbind(adultFlow, larvaeFlow)
      if (FlowSalinity %in% existing_sheets) {
        writeData(wb, sheet = FlowSalinity, temp_data_flow)
      } else {
        addWorksheet(wb, FlowSalinity)
        writeData(wb, sheet = FlowSalinity, temp_data_flow)
      }
      
      # Save optimal flow output
      temp_data_optimal <- rbind(adultOptimal, larvaeOptimal)
      if (Optimal %in% existing_sheets) {
        writeData(wb, sheet = Optimal, temp_data_optimal)
      } else {
        addWorksheet(wb, Optimal)
        writeData(wb, sheet = Optimal, temp_data_optimal)
      }
      
      # Save outlier flow output
      temp_data_outlier <- outlierFlow
      if (Outlier %in% existing_sheets) {
        writeData(wb, sheet = Outlier, temp_data_outlier)
      } else {
        addWorksheet(wb, Outlier)
        writeData(wb, sheet = Outlier, temp_data_outlier)
      }
      
      # Save the workbook
      saveWorkbook(wb, wb_path, overwrite = TRUE)
      message("Flow curve outputs saved successfully to the model version tracking file.")
    }
  }
}
#
save_flow_output(adult, larvae, Adult_optimal, Larvae_optimal, outlier_flow)
#
##
### Interpolation ####
#
## Get values into dataframe relating values to coordinates: 
#Adult meanOptimal, Larvae meanOptimal, meanOutlier
(A_optimal <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  Adult_optimal) %>%
  # add flow logger data
  mutate(meanDays = case_when(is.na(meanDays) ~ 0, TRUE ~ meanDays)))
#
(L_optimal <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  Larvae_optimal) %>%
  # add flow logger data
  mutate(meanDays = case_when(is.na(meanDays) ~ 0, TRUE ~ meanDays)))
#
(Outliers <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  data.frame(Station = c("S49", "S80", "S97"), 
             outlier_flow %>% rename(meanOutlier = mean_outlier_days) %>% dplyr::select(meanOutlier))) %>%
  # add flow logger data
  mutate(meanOutlier = case_when(is.na(meanOutlier) ~ 0, TRUE ~ meanOutlier)))
#
#
source("Code/WQ_functions.R")
Site_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Site_area[2])
###State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
##Get Site area
State_Grid <- c("E2") #E2, H4
Alt_Grid <- c("F2")
Site_Grid <- load_site_grid(State_Grid, Site_area)
Site_grid_sf <- st_as_sf(Site_Grid)
#
#Map of stations
ggplot()+
  geom_sf(data = Site_area, fill = "#99CCFF")+
  #geom_sf(data = Site_Grid, fill = NA)+
  geom_sf(data = FL_outline)+
  geom_point(data = Loggers, aes(Longitude, Latitude,  color = DataType), size = 3.5)+
  theme_classic()+
  scale_color_manual(values = c("#009E73", "#D55E00"))+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.05, st_bbox(Site_area)["xmax"]+0.05),
           ylim = c(st_bbox(Site_area)["ymin"]-0.05, st_bbox(Site_area)["ymax"]+0.05))
#
Site_Grid_spdf <- as(Site_Grid %>% dplyr::select(Latitude, Longitude, PGID), "Spatial")
#
## Get logger as spatial:
Logger_coords <- Loggers %>% dplyr::select(Longitude, Latitude)
coordinates(Logger_coords) <- ~Longitude + Latitude  # Longitude as x, Latitude as y
proj4string(Logger_coords) <- CRS("+proj=longlat +datum=WGS84")
# Get extents of both spatial objects
ext1 <- extent(Site_Grid_spdf)
ext2 <- extent(Logger_coords)
# Create combined extent (bounding box covering both)
combined_ext <- extent(
  min(ext1@xmin, ext2@xmin), 
  max(ext1@xmax, ext2@xmax), 
  min(ext1@ymin, ext2@ymin), 
  max(ext1@ymax, ext2@ymax)
)
# Convert the combined extent to a SpatialPolygons object
# (This assumes the CRS is the same for both; if not, set it explicitly)
combined_poly <- as(combined_ext, "SpatialPolygons")
proj4string(combined_poly) <- proj4string(Site_Grid_spdf)  # Inherit CRS from Site_Grid_spdf
#
# Sample a regular grid of 10000 points from the combined polygon
grid <- spsample(combined_poly, type = 'regular', n = 10000)
plot(grid) 
#
##Inverse distance weighted:
library(sf)       # For sf operations
library(gstat)    # For idw()
library(dismo)    # For voronoi()
library(raster)   # For extent()
library(sp)       # For SpatialPointsDataFrame
library(dplyr)    # For data manipulation
library(lubridate) # For parse_date_time() and time calculations
flow_idw_interpolation <- function(Site_data_spdf, grid, Site_Grid_spdf, colName) {
  #
  StartTime <- parse_date_time(format(Sys.time()), orders = "%Y-%m-%d %H:%M:%S")
  cat("Starting time:", format(Sys.time()), "\n")
  #
  tryCatch({
   #Convert Site_Grid_spdf polygons to sf and get centroids
    site_sf <- st_as_sf(Site_Grid_spdf)
    centroids_sf <- st_centroid(site_sf)
    #
    # Create formula dynamically from colName string
    idw_formula <- reformulate("1", response = colName)
    # IDW interpolation (power=2 by default)
    idw_model <- suppressMessages(idw(formula = idw_formula, locations = Site_data_spdf, newdata = grid, idp = 2))
    #
    
    # Convert to data.frame and rename columns
    idw_df <- as.data.frame(idw_model) %>% 
      rename(Longitude = x1, Latitude = x2, Prediction = var1.pred) %>%
      mutate(Pred_Value = round(Prediction, 2)) %>% #, Statistic = stats[i]) %>% 
      dplyr::select(-var1.var)
      #
      ##PROCESSING:
      #Convert to SpatialPointsDataFrame
      coordinates(idw_df) <- ~Longitude + Latitude
      proj4string(idw_df) <- proj4string(idw_model)
      #
      #Create Voronoi polygons clipped to grid extent
      voroni_poly <- dismo::voronoi(idw_df, ext = raster::extent(grid))
      #
      ##GRID App:
      # Convert voronoi polygons to sf
      voronoi_sf <- st_as_sf(voroni_poly)
      #
      #Spatial join: assign Voronoi polygon values to centroids, join centroids with voronoi polygons by spatial intersection
      centroids_joined <- st_join(centroids_sf, voronoi_sf[, c("Pred_Value")], left = TRUE)
      #
      ##WRAP UP:
      centroids_joined <- centroids_joined %>%
        rename(!!colName := Pred_Value)
      #
      # Join back to original site polygons (assuming PGID matches)
      final_sf <- site_sf %>% 
        left_join(st_drop_geometry(centroids_joined %>% dplyr::select(-Latitude, -Longitude)), by = "PGID")
  })
  EndTime <- parse_date_time(format(Sys.time()), orders = "%Y-%m-%d %H:%M:%S")
  cat("Ending time:", format(Sys.time()), "\n")
  print(EndTime - StartTime)
  #
  return(final_sf)
}
#
#
## Repeat for each data frame:
data_cols <- if(ncol(A_optimal) >= 3) {
  A_optimal[, !names(A_optimal) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = A_optimal[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
AOP_idw_data <- flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanDays")
#plot_interpolations(AOP_idw_data, Site_Grid, Threshold = "N", simplify_tolerance = 0)
#
#
#
data_cols <- if(ncol(L_optimal) >= 3) {
  L_optimal[, !names(L_optimal) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = L_optimal[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
LOP_idw_data <- flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanDays")
#
#
#
data_cols <- if(ncol(Outliers) >= 3) {
  Outliers[, !names(Outliers) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = Outliers[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
Outlier_idw_data <- flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanOutlier")#
#
#
#
#
plot_flow_interp <- function(results_data, Site_Grid, colName, simplify_tolerance = 0){
  special_cols <- colnames(results_data) %in% c("Latitude", "Longitude", "geometry")
  
  if (simplify_tolerance > 0) {
    df_filtered <- st_simplify(results_data, dTolerance = simplify_tolerance, preserveTopology = TRUE)
  } else {
    df_filtered <- results_data
  }
  # Define base theme for reuse (avoids redundancy)
  base_theme <- theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA), 
          axis.text = element_text(size = 16),
          plot.margin = unit(c(0,0,0,0), "cm"), 
          plot.title = element_text(margin = margin(b = 5)), 
          plot.caption = element_text(face = "italic", size = 9))
  #
  #Create plots
  ggplot() +
    geom_sf(data = df_filtered, aes(color = !!sym(colName))) +
    base_theme +
    scale_color_viridis_b(direction = -1)   # Use shared limits
}
#
plot_flow_interp(Outlier_idw_data, Site_Grid, "meanOutlier")
#
ggsave(path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
     filename = paste("Flow_salinity_curve_", "Adult_optimal_days",".tiff", sep = ""), dpi = 1000)
#
#
#
## Save output
#fileName: SiteCode_fileName
save_flow_output <- function(output_data, fileName){
  final_output_data <- output_data
  #Save shapefile:
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = paste0("\nShould the shapefile and a summary of the chosen interpolation values be saved locally to the '", Site_code, "_", Version,"' project folder?"))
    if(result == "No"){
      message("Shapefile and summary will not be saved.")
    } else {
      #### ---------------------------------------------------------
      #### Save shape file 
      #### --------------------------------------------------------
      #Shape file
      shape_file <- final_output_data
      shapefile_path <- paste0("../",Site_code, "_", Version,"/Output/Shapefiles/", #Save location
                               #File name
                               paste0(Site_code, "_", paste(fileName)), 
                               ".shp")
      #Save the sf dataframe as a shapefile
      suppressMessages(st_write(shape_file, shapefile_path, delete_dsn = TRUE, quiet = TRUE))
      #Print a message to confirm saving
      cat("Shapefile saved at:", shapefile_path, "\n",
          "- ", nrow(final_output_data), " features saved with ", ncol(final_output_data)-1, "fields")
      #
      #
      #### ---------------------------------------------------------
      #### Save model data 
      #### --------------------------------------------------------
      # Excel data
      model_data <- as.data.frame(shape_file) %>% dplyr::select(-geometry)
      data_path <- paste0("../",Site_code, "_", Version,"/Output/Data files/", #Save location
                          #File name
                          paste0(Site_code, "_", paste(fileName)), 
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
      #
      #
      #### ---------------------------------------------------------
      #### Add Interpolation Summary info 
      #### --------------------------------------------------------
      model_setup_path <- paste0("../",Site_code, "_", Version,"/Data/",Site_code, "_", Version,"_model_setup.xlsx")
      # Summary info
      sheet_names <- excel_sheets(model_setup_path)
      sheet_name <- "Interpolation_Summary"
      summ_info <- data.frame(Parameter = fileName,
                              Type = "Flow",
                              Statistic = "Mean",
                              Models = if(grepl("optimal", fileName, ignore.case = TRUE)) {paste("Mean optimal days per year")} else if(grepl("outlier", fileName, ignore.case = TRUE)){paste("Mean outlier days per month")} else {paste("")},
                              Weights = NA,
                              Date_range = paste0(Start_year, "-", End_year),
                              Months = "All",
                              Threshold_value = NA,
                              Date_updated = Sys.Date())
      #Load the workbook
      wb <- loadWorkbook(model_setup_path)
      #Check if the sheet exists
      if (sheet_name %in% sheet_names) {
        #If it exists, append data to the existing sheet
        existing_data <- readWorkbook(model_setup_path, sheet = sheet_name, detectDates = TRUE)
        new_data <- rbind(existing_data, summ_info)
        writeData(wb, sheet = sheet_name, new_data)
      } else {
        #If it does not exist, create a new sheet
        addWorksheet(wb, sheet_name)
        writeData(wb, sheet = sheet_name, summ_info)
      }
      #Save the workbook
      saveWorkbook(wb, model_setup_path, overwrite = TRUE)
      #Print a message to confirm saving
      cat("Summary information was saved within:", sheet_name, "\n")
      #
      #
      #### -----------------------------------------------------------
      #### Add Flow_stations sheet 
      #### -----------------------------------------------------------
      flow_sheet <- "Flow_stations"
      
      # Load again (safe; but avoids overwriting earlier)
      wb <- loadWorkbook(model_setup_path)
      sheet_names <- sheets(wb)
      
      # Replace Logger_data with whatever your logger dataframe is
      if (!exists("Loggers")) {
        warning("'Loggers' object not found. Flow_stations sheet was not added.")
      } else {
        if (flow_sheet %in% sheet_names) {
          # Clear + rewrite
          removeWorksheet(wb, flow_sheet)
          addWorksheet(wb, flow_sheet)
        } else {
          addWorksheet(wb, flow_sheet)
        }
        
        writeData(wb, sheet = flow_sheet, Loggers)
        saveWorkbook(wb, model_setup_path, overwrite = TRUE)
        
        cat("Logger data successfully added to sheet 'Flow_stations' in model_setup.xlsx\n")
      }
    }
  }
}
#
save_flow_output(AOP_idw_data, "flow_optimal_adult")
save_flow_output(LOP_idw_data, "flow_optimal_larvae")
save_flow_output(Outlier_idw_data, "flow_outlier")
#
### Other possible data ####
# 
## Metrics
# Function to calculate 30-day back averaged salinity (rolling 30-day mean) library(dplyr, slider)
# Input: df (data frame with 'Date' as Date class and 'Salinity' as numeric)
#        complete (logical, default TRUE): If TRUE, only calculate when full 30-day window is available; if FALSE, use partial windows
# Output: The input data frame with an added column 'Rolling_30d_Salinity'
calculate_rolling_30d_value <- function(df, value_col = "Salinity", complete = TRUE) {
  # Check for required columns and formats
  if (!"Date" %in% colnames(df)) {
    stop("Error: The data frame must contain a 'Date' column.")
  }
  if (!value_col %in% colnames(df)) {
    stop("Error: The data frame must contain a '",value_col,"' column.")
  }
  if (!inherits(df$Date, "Date")) {
    stop("Error: The 'Date' column must be of class 'Date'. Convert it using as.Date() if necessary.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Error: The '",value_col,"' column must be numeric.")
  }
  #
  # Ensure data is sorted by Date
  df <- df %>% arrange(Date)
  # Dynamic naming
  output_col <- paste0("Roll3d_", value_col)
  #
  # Calculate rolling 30-day mean
  df[[output_col]] <- slide_period_dbl(
    .x = df[[value_col]],
    .i = df$Date,
    .period = "day",
    .f = ~mean(.x, na.rm = TRUE),
    .before = 29,
    .complete = complete
  )
  return(df)
}
#
(sal_30d <- calculate_rolling_30d_value(salinity_ave, "Salinity"))
#
#
#
## Curve fits
#
## Currently working with previously used formula in CERP reports
#non linear, exponential decay to scatter plot of flow vs sal library(minpack.lm) #nlsLM() is more stable/robust than base nls().
fit_month <- nlsLM(
  Mean_Salinity ~ a * exp(-b * Mean_Flow) + c,
  data = monthly_data,
  start = list(a = max(monthly_data$Mean_Salinity, na.rm = T), b = 0.01, c = min(monthly_data$Mean_Salinity, na.rm = T))
)
summary(fit_month)
#
flow_at_salinity <- function(fit, target_sal) {
  # Extract parameters
  p <- coef(fit)
  a <- p["a"]
  b <- p["b"]
  c <- p["c"]
  
  # Check valid range
  if ((target_sal - c) / a <= 0) {
    stop("Target salinity is outside the valid range of the exponential model.")
  }
  
  # Compute flow
  flow <- -(1 / b) * log((target_sal - c) / a)
  return(flow)
}

flow_at_salinity(fit_month, 10)
flow_at_salinity(fit_month, 30)
plot_flow_target <- function(df, fit, target_sal, flow_value, value_col = "Mean_Salinity") {
  # Create sequence for smooth fitted curve
  xseq <- seq(min(df[[flow_value]], na.rm = T), max(df[[flow_value]], na.rm = T), length.out = 300)
  
  # Predict salinity across flow range
  pred <- predict(fit, newdata = data.frame(!!flow_value := xseq))
  
  # Plot
  plot(df[[flow_value]], df[[value_col]],
       pch = 19, col = "gray40",
       xlab = "Flow",
       ylab = "Salinity",
       main = paste("Exponential Decay Fit with Target =", target_sal))
  
  lines(xseq, pred, col = "blue", lwd = 2)
  
  # Add point for flow at salinity target
  points(flow_value, target_sal, pch = 19, col = "red", cex = 1.4)
  abline(h = target_sal, col = "red", lty = 2)
  abline(v = flow_value, col = "red", lty = 2)
  
  text(flow_value, target_sal,
       labels = sprintf("Flow = %.2f", flow_value),
       pos = 4, col = "red")
}
plot_flow_target(monthly_data, fit_month, 20, Mean_Flow, value_col = "Mean_Salinity")
#
#
#
#
#non liner hyperbolic decay 
fit_hyp <- nlsLM(
  Mean_Salinity ~ c + a / (1 + b * Mean_Flow),
  data = monthly_data,
  start = list(
    a = max(monthly_data$Mean_Salinity, na.rm = T) - min(monthly_data$Mean_Salinity, na.rm = T),   # amplitude guess
    b = 0.01,                                  # rate constant guess
    c = min(monthly_data$Mean_Salinity, na.rm = T)                       # lower asymptote guess
  )
)
summary(fit_hyp)
#
flow_at_salinity_hyperbolic <- function(fit, target_sal) {
  # Extract parameters
  p <- coef(fit)
  a <- p["a"]
  b <- p["b"]
  c <- p["c"]
  
  # Ensure target salinity is valid
  if (target_sal <= c) {
    stop("Target salinity is <= the asymptote c. Flow cannot be solved.")
  }
  
  # Calculate flow
  flow <- (a / (target_sal - c) - 1) / b
  return(flow)
}
plot_flow_target_hyperbolic <- function(df, fit, target_sal, flow_value,
                                        value_col = "Salinity") {
  # Generate smooth curve
  xseq <- seq(min(df$flow), max(df$flow), length.out = 300)
  
  pred <- predict(fit, newdata = data.frame(flow = xseq))
  
  plot(df$flow, df[[value_col]],
       pch = 19, col = "gray40",
       xlab = "Flow", ylab = "Salinity",
       main = paste("Hyperbolic Decay Fit with Target =", target_sal))
  
  lines(xseq, pred, col = "blue", lwd = 2)
  
  # Highlight the target point
  points(flow_value, target_sal, pch = 19, col = "red", cex = 1.4)
  abline(h = target_sal, col = "red", lty = 2)
  abline(v = flow_value, col = "red", lty = 2)
  
  text(flow_value, target_sal,
       sprintf("Flow = %.2f", flow_value),
       pos = 4, col = "red")
}

