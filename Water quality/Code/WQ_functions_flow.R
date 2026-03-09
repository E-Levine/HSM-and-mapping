# Functions for working with flow data
#
# Working with USGS data:
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
      parameter_code == "00095" ~ "Conductance",
      TRUE ~ NA_character_))
  #
  # Check for data
  if (nrow(data) == 0) stop("No data found in the specified date range")
  #
  start_ym <- format(start, "%Y%m")  # e.g., "202301"
  end_ym <- format(end, "%Y%m")      # e.g., "202312"
  #
  # Set up for flow file:
  data_path <- paste0("Data/Raw-data/", Site_code, "_logger_", Type, "_", start_ym, "_", end_ym,".xlsx")
  # Create wb with data:
  sheetName = paste0("logger_", Type)
  new_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(new_wb, sheetName)  # Add fresh sheet
  openxlsx::writeData(new_wb, sheet = sheetName, x = data) 
  # Save wb
  openxlsx::saveWorkbook(new_wb, data_path, overwrite = TRUE)
  cat("Logger data successfully saved to:\n",
      "- Sheet '",sheetName,"' (", nrow(data), " rows)\n",
      "File: ", data_path, "\n")
  #
}
#
# Clean and save existing data:
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
# Load Excel data files:
load_WQ_data <- function() {
  
  Stations <- openxlsx::read.xlsx(
    file.path("Data/Raw-data/Flow_logger_locations.xlsx"),
    na.strings = c("NA", " ", "", "Z"),
    detectDates = TRUE
  ) %>%
    dplyr::filter(Site == Site_code)
  
  flow_file <- list.files(
    path = "Data/Raw-data/",
    pattern = paste0(Site_code, "_logger_flow_.*\\.xlsx$")
  )
  
  if (length(flow_file) == 0) {
    stop("No flow file found for Site_code: ", Site_code)
  }
  
  flow_raw <- openxlsx::read.xlsx(
    file.path("Data/Raw-data/", flow_file[1]),
    na.strings = c("NA", " ", "", "Z"),
    detectDates = TRUE
  )
  
  salinity_file <- list.files(
    path = "Data/Raw-data/",
    pattern = paste0(Site_code, "_logger_[Ss]alinity_.*\\.xlsx$")
  )
  
  if (length(salinity_file) == 0) {
    stop("No salinity file found for Site_code: ", Site_code)
  }
  
  salinity_raw <- openxlsx::read.xlsx(
    file.path("Data/Raw-data/", salinity_file[1]),
    na.strings = c("NA", " ", "", "Z"),
    detectDates = TRUE
  )
  
  # Assign outputs to global environment
  assign("Loggers", Stations, envir = .GlobalEnv)
  assign("flow_raw", flow_raw, envir = .GlobalEnv)
  assign("salinity_raw", salinity_raw, envir = .GlobalEnv)
  
  invisible(NULL)
}
#
#
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
# sali_grps <- cluster_points(salinity_raw, 7500)
# View the map: sali_grps$map
# Access modified data: sali_grps$data
# Access group summaries: sali_grps$groups
#
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
#
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
    mutate(
      Year = year(Date), 
      Month = month(Date), 
      Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    group_by(Date, Year, Month, Station) %>%
    summarise(!!output_col := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  #
  return(monthly_salinity)
}
#
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
  data_lookup <- list()
  
  # Loop over each combination
  for (sal_station in salinity_stations) {
    for (flow_station in flow_stations) {
      # Subset salinity data for the current station
      sal_sub <- salinity_data %>%
        dplyr::filter(.data[[station_col]] == sal_station) %>%
        dplyr::select(all_of(time_col), !!salinity_col := all_of(salinity_col))
      
      # Subset flow data for the current station
      flow_sub <- flow_data %>%
        dplyr::filter(.data[[station_col]] == flow_station) %>%
        dplyr::select(all_of(time_col), !!flow_col := all_of(flow_col))
      
      # Merge on time (inner join to get matching time points)
      combined <- inner_join(sal_sub, flow_sub, by = time_col) %>% tidyr::drop_na()
      
      # Get model name
      model_name <- paste(
        stringr::str_replace_all(sal_station, "_", ""),
        stringr::str_replace_all(flow_station, "_", ""),
        sep = "_"
      )
      
      # Store data_lookup table
      if (nrow(combined) > 0) {
        data_lookup[[model_name]] <- combined
      }
      
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
        results[[model_name]] <- combined
        # Store the summary if fit succeeded, otherwise store an error message
        if (!inherits(fit, "try-error")) {
          results[[model_name]] <- summary(fit)
        } else {
          results[[model_name]] <- paste("Fit failed for salinity station", sal_station, "and flow station", flow_station, ":", attr(fit, "condition")$message)
        }
      } else {
        results[[model_name]] <- paste("Insufficient data for salinity station", sal_station, "and flow station", flow_station, "(only", nrow(combined), "matching time points)")
      }
    }
  }
  # Print the list of all model names (combinations) that were attempted
  print("Models attempted (salinityStation_flowStation):")
  print(names(results))
  #assign("Model_data", results_data, envir = .GlobalEnv)
  return(list(models = results,
              data_lookup = data_lookup))
}
#
# Update models:
filter_models <- function(results, models, mode = c("keep", "remove")) {
  #
  stopifnot(
    is.list(results),
    all(c("models", "data_lookup") %in% names(results))
  )
  #
  mode <- match.arg(mode)
  # Get model names
  model_names <- names(results$models)
  # Keep or remove models 
  if (mode == "keep") {
    valid_models <- intersect(models, model_names)
    
    if (length(valid_models) == 0) {
      stop("None of the specified models were found in results$models.")
    }
    
    if (length(valid_models) < length(models)) {
      warning("Some requested models were not found and were ignored.")
    }
    
    keep_names <- valid_models
    
  } else {  # mode == "remove"
    
    invalid_models <- setdiff(models, model_names)
    if (length(invalid_models) > 0) {
      warning("Some models to remove were not found and were ignored.")
    }
    
    keep_names <- setdiff(model_names, models)
  }
  # Output filtered data
  list(
    models      = results$models[keep_names],
    data_lookup = results$data_lookup[names(results$data_lookup) %in% keep_names]
  )
}
#
#
# Calculate flow at specified salinity (from HSM curves)
flow_at_salinity_hyp2 <- function(results, target_sal, data_lookup) {
  # results: output from fit_salinity_flow_models (list of summaries or error messages)
  # target_sal: target salinity value
  # data_lookup: named list of data frames used in each model
  #              must include columns: salinity, flow
  
  # Initialize a data frame to store results
  flow_results <- data.frame(
    salinity_station = character(),
    flow_station = character(),
    flow_min         = numeric(),
    flow_max         = numeric(),
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
    
    flow_min <- NA
    flow_max <- NA
    flow_val <- NA
    status   <- "Failed fit"
    #
    # Successful fit ----
    if (inherits(result, "summary.nls")) {
      # Successful fit: extract coefficients and compute flow
      p <- coef(result)
      y0 <- p[1]
      a  <- p[2]
      b  <- p[3]
      
      if (target_sal <= y0) {
        status <- "Target salinity <= y0"
      } else {
        flow_val <- (a * b) / (target_sal - y0) - b
        status <- "Success"
      }
    }
    #
    # All flow in ideal salinity range ----
    if (is.na(flow_val) && model_name %in% names(data_lookup)) {
      dat <- data_lookup[[model_name]]
      
      if (target_sal <= min(dat$Mean_Salinity, na.rm = TRUE) ||
          target_sal >= max(dat$Mean_Salinity, na.rm = TRUE)) {
        
        flow_min <- min(dat$Mean_Flow, na.rm = TRUE)
        flow_max <- max(dat$Mean_Flow, na.rm = TRUE)
        status   <- "All flows within target range; returning flow bounds"
        
        if(target_sal <= min(dat$Mean_Salinity, na.rm = TRUE)){
          flow_val <- flow_max
        } else if(target_sal >= max(dat$Mean_Salinity, na.rm = TRUE)){
          flow_val <- flow_min
        }
      }
    }
    # Append to results data frame ----
    flow_results <- rbind(
      flow_results, 
      data.frame(
        salinity_station = sal_station_clean,
        flow_station = flow_station_clean,
        flow_min = flow_min,
        flow_max = flow_max,
        flow_at_target = flow_val,
        status = status,
        stringsAsFactors = FALSE
    ))
  }
  return(flow_results)
}
#
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
#
## Determine mean number of days in year within range 
#min and max Dates to include
optimal_flow_days <- function(df, Station_name, minDate, maxDate, minFlow, maxFlow){
  #
  # Setup data
  data_setup <- df %>% 
    # Filter data to date range
    dplyr::filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    # Add Years and Stations
    dplyr::mutate(
      Year = lubridate::year(Date))
  #
  # Get total possible days in date range
  total_days <- data_setup %>%
    # Group by Year
    dplyr::group_by(Year) %>%
    # Get total number
    dplyr::summarise(
      TotalDays = dplyr::n_distinct(Date)
    )
  
  # Get optimal days
  opt_days <- data_setup %>% 
    # Identify optimal days
    dplyr::mutate(
      is_optimal = Flow > minFlow & Flow < maxFlow
    ) %>%
    #
    # Count optimal days per year
    dplyr::group_by(Station, Year) %>%
    dplyr::summarise(
      Days = sum(is_optimal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    #
    dplyr::left_join(total_days, by = "Year") %>%
    # Calculate days/year
    dplyr::mutate(OptimalDays = Days/TotalDays) %>%
    # Get mean optimal days with station ID
    dplyr::group_by(Station) %>% rename(FlowStation = Station) %>%
    dplyr::summarise(
      meanOptimal = mean(OptimalDays, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Add salinity station
    dplyr::mutate(SalStation = Station_name)
  
  return(opt_days)
}
#
#
#Calculates the mean optimal days for each station 
#returndf = individual, combined, both
automate_optimal_df <- function(df, flow_ave_df, start_date, end_date, dfType, returndf = c("individual", "combined", "both")){
  
  returndf <- match.arg(returndf,
                        choices = c("individual", "combined", "both"))
  
  flow_ave_df <- flow_ave_df %>%
    dplyr::mutate(Station_clean = gsub("_", "", Station))
  
  # Get unique combinations of salinity_station and flow_station
  unique_combos <- df %>%
    dplyr::distinct(salinity_station, flow_station)
  
  # Initialize a list to store results for each combination
  results <- vector("list", nrow(unique_combos))
  
  # Loop over each unique combination
  for (i in seq_len(nrow(unique_combos))) {
    salStation <- unique_combos$salinity_station[i]
    flowStation <- unique_combos$flow_station[i]
    
    # Filter the adult dataframe for the current combination
    combo_data <- df %>% 
      dplyr::filter(
        salinity_station == salStation,
        flow_station == flowStation)
    
    # Filter flow to current flow station
    single_flow <- flow_ave_df %>% 
      dplyr::filter(Station_clean == flowStation)
    
    # Extract min and max flow for the combination
    min_flow <- combo_data %>%
      dplyr::filter(Flow == "min") %>%
      dplyr::pull(flow_at_target) %>%
      dplyr::first()
    
    max_flow <- combo_data %>%
      dplyr::filter(Flow == "max") %>%
      dplyr::pull(flow_at_target) %>%
      dplyr::first()
    
    min_flow <- ifelse(is.na(min_flow), -900, min_flow)
    max_flow <- ifelse(is.na(max_flow), -900, max_flow)
    
    if (nrow(single_flow) == 0) next
    
    # Call optimal_flow_days with the flow_station and computed min/max Sal as minFlow/maxFlow
    opt_flow <- optimal_flow_days(
      single_flow, 
      salStation, 
      start_date, 
      end_date, 
      min_flow, 
      max_flow)
    
    # Store the result with a key like "salinity_station_flow_station"
    results[[i]] <- opt_flow
  }
  
  # Combine individual results
  individual <- dplyr::bind_rows(results)
  
  # Combine all results into a single dataframe
  if (nrow(individual) == 0) {
    stop("No valid station combinations produced results.")
  }
  
  # Combined (mean) summary 
  combined <- individual %>% 
    dplyr::mutate(Type = dfType) %>%
    dplyr::group_by(SalStation, Type) %>%
    dplyr::summarise(meanOptimal = mean(meanOptimal, na.rm = T),
                     .groups = "drop")
  
  # return based on function input
  if (returndf == "individual") {
    return(individual)
    
  } else if (returndf == "combined") {
    return(combined)
    
  } else if (returndf == "both") {
    return(list(Individual = individual, Mean = combined))
  }
}
#
#
## Number above or below optimal
nonoptimal_flow_days <- function(df, Station_name, minDate, maxDate, flowThreshold, type = c("sub", "super")) {
  
  type <- match.arg(type)
  
  # ---- Setup data ----
  data_setup <- df %>% 
    dplyr::filter(
      Date >= as.Date(minDate),
      Date <= as.Date(maxDate)
    ) %>%
    dplyr::mutate(
      Year = lubridate::year(Date)
    )
  
  # ---- Total possible days per year ----
  total_days <- data_setup %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      TotalDays = dplyr::n_distinct(Date),
      .groups = "drop"
    )
  
  # ---- Identify non-optimal days ----
  result <- data_setup %>%
    dplyr::mutate(
      Conditions = dplyr::case_when(
        type == "sub"   & Flow < flowThreshold ~ 1,
        type == "super" & Flow > flowThreshold ~ 1,
        TRUE                                   ~ 0
      )
    ) %>%
    # Count non-optimal days per year
    dplyr::group_by(Station, Year) %>%
    dplyr::summarise(
      Days = sum(Conditions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Join total days
    dplyr::left_join(total_days, by = "Year") %>%
    # Proportion of days per year
    dplyr::mutate(PropDays = Days/TotalDays) %>%
    # Mean across years
    dplyr::group_by(Station) %>%
    dplyr::rename(FlowStation = Station) %>%
    dplyr::summarise(
      meanDays = mean(PropDays, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Add salinity station + type label
    dplyr::mutate(
      SalStation = Station_name,
      FlowType   = type
    )
  
  return(result)
}
#
automate_nonoptimal_df <- function(df, flow_ave_df, start_date, end_date, dfType, returndf = c("individual", "combined", "both")){
  
  returndf <- match.arg(returndf,
                        choices = c("individual", "combined", "both"))
  
  flow_ave_df <- flow_ave_df %>%
    dplyr::mutate(Station_clean = gsub("_", "", Station))
  
  # Get unique combinations of salinity_station and flow_station
  unique_combos <- df %>%
    dplyr::distinct(salinity_station, flow_station)
  
  # Initialize a list to store results for each combination
  results <- vector("list", nrow(unique_combos))
  
  # Loop over each unique combination
  for (i in seq_len(nrow(unique_combos))) {
    salStation <- unique_combos$salinity_station[i]
    flowStation <- unique_combos$flow_station[i]
    
    # Filter the adult dataframe for the current combination
    combo_data <- df %>% 
      dplyr::filter(
        salinity_station == salStation,
        flow_station == flowStation)
    
    # Filter flow to current flow station
    single_flow <- flow_ave_df %>% 
      dplyr::filter(Station_clean == flowStation)
    
    # Extract min and max flow for the combination
    min_flow <- combo_data %>%
      dplyr::filter(Flow == "min") %>%
      dplyr::pull(flow_at_target) %>%
      dplyr::first()
    
    max_flow <- combo_data %>%
      dplyr::filter(Flow == "max") %>%
      dplyr::pull(flow_at_target) %>%
      dplyr::first()
    
    min_flow <- ifelse(is.na(min_flow), -900, min_flow)
    max_flow <- ifelse(is.na(max_flow), -900, max_flow)
    
    if (nrow(single_flow) == 0) next
    
    # Determine non-optimal days
    sub_flow <- nonoptimal_flow_days(
      df = single_flow,
      Station_name = salStation,
      minDate = start_date,
      maxDate = end_date,
      flowThreshold = min_flow,
      type = "sub")
    super_flow <- nonoptimal_flow_days(
      df = single_flow,
      Station_name = salStation,
      minDate = start_date,
      maxDate = end_date,
      flowThreshold = max_flow,
      type = "super")
    
    # Store the result with a key like "salinity_station_flow_station"
    results[[i]] <- dplyr::bind_rows(sub_flow, super_flow)
  }
  
  # Combine individual results
  individual <- dplyr::bind_rows(results)
  
  # Combine all results into a single dataframe
  if (nrow(individual) == 0) {
    stop("No valid combinations produced results.")
  }
  
  # Combined (mean) summary 
  combined <- individual %>% 
    dplyr::mutate(Type = dfType) %>%
    dplyr::group_by(SalStation, Type, FlowType) %>%
    dplyr::summarise(meanDays = mean(meanDays, na.rm = T),
                     .groups = "drop")
  
  # return based on function input
  if (returndf == "individual") {
    return(individual)
    
  } else if (returndf == "combined") {
    return(combined)
    
  } else if (returndf == "both") {
    return(list(Individual = individual, Mean = combined))
  }
  
}
#
#
# Count number of days in month more than 1.5 SD from monthly mean
count_outlier_flow_days <- function(df, minDate, maxDate, flow_col = "Flow") {
  #
  # Set up ----
  working_df <- df %>% 
    dplyr::mutate(
      Year  = lubridate::year(Date),
      Month = factor(
        format(Date, "%b"),
        levels = month.abb,
        ordered = TRUE))
  #
  # Get annual ranges per logger ----
  monthly_range <- working_df %>% 
    dplyr::group_by(Year, Month, Station) %>% 
    summarise(
      MonthMin = min(Flow, na.rm = T), 
      MonthMax = max(Flow, na.rm = T), 
      .groups = "drop") %>%
    # Get monthly range
    dplyr::mutate(MonthRange = MonthMax - MonthMin) %>%
    # Get average monthly range
    dplyr::group_by(Month, Station) %>% 
    summarise(
      meanRange = mean(MonthRange, na.rm = T), 
      sdRange = sd(MonthRange, na.rm = T), 
      .groups = "drop")
  #
  # Get total possible months in date range ----
  total_months <- working_df %>%
    filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    # Group by Year
    dplyr::group_by(Year) %>%
    # Get total number
    dplyr::summarise(
      TotalMonths = dplyr::n_distinct(Month)
    )
  # Get monthly values ----
  monthly_summary <- working_df %>% 
    # Filter data to date range
    filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    group_by(Year, Month, Station) %>%
    # Determine daily range
    summarise(
      MinFlow = min(Flow, na.rm = T), 
      MaxFlow = max(Flow, na.rm = T), 
      .groups = "drop") %>%
    # Get daily range
    dplyr::mutate(Range = MaxFlow - MinFlow) %>%
    # Add in monthly mean range
    dplyr::left_join(monthly_range, by = c("Month", "Station")) %>%
    # Determine outliers
    dplyr::mutate(outlier_1sd = abs(Range - meanRange) > 1 * sdRange &
                    abs(Range - meanRange) <= 2 * sdRange,
                  outlier_2sd = abs(Range - meanRange) > 2 * sdRange) %>%
    dplyr::group_by(Year, Station) %>%
    # Count number of outliers per Year, and mean flow
    summarise(
      month_outlier1sd = sum(outlier_1sd, na.rm = TRUE),
      month_outlier2sd = sum(outlier_2sd, na.rm = TRUE),
      meanRange = mean(meanRange, na.rm = T),
      sdRange = mean(sdRange, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::left_join(total_months, by = "Year") %>%
    # Calculate months/year
    dplyr::mutate(Out_1_Months = month_outlier1sd/TotalMonths,
                  Out_2_Months = month_outlier2sd/TotalMonths)
  #
  # Get average per logger ----
  logger_rows <- monthly_summary %>%
    dplyr::group_by(Station) %>%
    dplyr::summarise(
      Out_1_Months = mean(Out_1_Months, na.rm = TRUE),
      Out_2_Months = mean(Out_2_Months, na.rm = TRUE),
      meanRange   = mean(meanRange, na.rm = TRUE),
      meanSDRange = mean(sdRange, na.rm = TRUE))
  # Get overall average ----
  overall_row <- monthly_summary %>%
    dplyr::summarise(
      Station = "Overall",
      Out_1_Months = mean(Out_1_Months, na.rm = TRUE),
      Out_2_Months = mean(Out_2_Months, na.rm = TRUE),
      meanRange   = mean(meanRange, na.rm = TRUE),
      meanSDRange = mean(sdRange, na.rm = TRUE))
  #
  # Join together
  final_summary <- dplyr::bind_rows(logger_rows, overall_row)
  return(final_summary)
  #
}
#
#
# Save data and/or figure created
#make sure to specify list item if list objetc used
save_flow_output <- function(adultFlow, 
                             larvaeFlow, 
                             adultOptimal,
                             adultNonOptimal,
                             larvaeOptimal, 
                             larvaeNonOptimal,
                             outlierFlow){
  #
  Logger_stations <- "Flow_stations"
  FlowSalinity <- "Flow_at_salinity"
  Optimal <- "Flow_optimal_days"
  NonOptimal <- "Flow_days_above_below"
  Outlier <- "Flow_outlier_months"
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
      temp_data_optimal <- dplyr::bind_rows(adultOptimal, larvaeOptimal)
      if (Optimal %in% existing_sheets) {
        writeData(wb, sheet = Optimal, temp_data_optimal)
      } else {
        addWorksheet(wb, Optimal)
        writeData(wb, sheet = Optimal, temp_data_optimal)
      }
      
      # Save nonoptimal flow output
      temp_data_nonoptimal <- dplyr::bind_rows(adultNonOptimal, larvaeNonOptimal)
      if (NonOptimal %in% existing_sheets) {
        writeData(wb, sheet = NonOptimal, temp_data_nonoptimal)
      } else {
        addWorksheet(wb, NonOptimal)
        writeData(wb, sheet = NonOptimal, temp_data_nonoptimal)
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
#
##Inverse distance weighted interpolation:
library(sf)       # For sf operations
library(gstat)    # For idw()
library(dismo)    # For voronoi()
library(raster)   # For extent()
library(sp)       # For SpatialPointsDataFrame
library(dplyr)    # For data manipulation
library(lubridate) # For parse_date_time() and time calculations
flow_idw_interpolation <- function(Site_data_spdf, grid, Site_Grid_spdf, colName) {
  #
  StartTime <- Sys.time()
  cat("Starting time:", format(StartTime), "\n")
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
      dplyr::rename(Longitude = x1, Latitude = x2) %>%
      dplyr::rename(Prediction = var1.pred) %>%
      dplyr::mutate(Pred_Value = round(Prediction, 2)) %>% #, Statistic = stats[i]) %>% 
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
      dplyr::rename(!!colName := Pred_Value)
    #
    # Join back to original site polygons (assuming PGID matches)
    final_sf <- site_sf %>% 
      dplyr::left_join(st_drop_geometry(centroids_joined %>% 
                                          dplyr::select(-Latitude, -Longitude)), 
                       by = "PGID")
  })
  EndTime <- Sys.time()
  cat("Ending time:", format(EndTime), "\n")
  print(EndTime - StartTime)
  #
  return(final_sf)
}
#
#
# Plot interp flow output
plot_flow_interp <- function(results_data, colName, simplify_tolerance = 0){
  # Subset 
  special_cols <- c("Latitude", "Longitude", "geometry")
  plot_cols <- c("geometry", colName)
  df_filtered <- results_data[, plot_cols, drop = FALSE]
  
  
  if (simplify_tolerance > 0) {
    df_filtered <- st_simplify(df_filtered, dTolerance = simplify_tolerance, preserveTopology = FLASE)
  } else {
    df_filtered <- df_filtered
  }
  # Define base theme for reuse (avoids redundancy)
  base_theme <- theme_classic() +
    theme(panel.border = element_rect(color = "black", fill = NA), 
          axis.text = element_text(size = 12),
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
#
## Save interp output
#fileName: SiteCode_fileName
save_flow_interp_output <- function(output_data, fileName){
  final_output_data <- output_data
  #Save shapefile:
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = paste0("\nShould the shapefile and a summary of the chosen interpolation values be saved locally to the '", Site_code, "_", Version,"' project folder?"))
    if(result == "No"){
      message("Shapefile and summary will not be saved.")
    } else {
      #### Save shape file ----
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
      #### Save model data ----
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
      #### Add Interpolation Summary info ----
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
      #### Add Flow_stations sheet ----
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
#