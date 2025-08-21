###Code for HSM scoring and output
##Requires shapefile of area with data layers applied
##Currently working with files output from Arc
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load required packages (should install missing packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, #Df manipulation, basic summary
               sf, raster, terra,
               leaflet, tmap, openxlsx, writexl,
               install = TRUE) #Mapping and figures
#
#
#source("HSM code/Functions/HSM_Creation_Functions.R")
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("UN") #two-letter site code
Version <- c("v1") #Model version
#
#
###Load shape file with data:
#
#Load data from all matching folders: SiteCode_Version_data
load_model_files <- function(SiteCode = Site_Code, VersionNumber = Version, shp_filename = "_HSM_datalayer"){
  data_dir <- paste0(SiteCode, "_", VersionNumber, "/Output/Shapefiles/")
  file_name <- shp_filename
  output_name <- paste0(SiteCode, "_", VersionNumber, "_data")
  #Load shape file, assign name
  shape_obj <- st_read(paste0(data_dir, SiteCode, file_name, ".shp"))
  assign(output_name, shape_obj, envir = .GlobalEnv)
  #Load Parameter summary file
  Param_summ <<- read_excel(paste0(SiteCode, "_", VersionNumber, "/Data/", SiteCode, "_", VersionNumber, "_model_setup.xlsx"), sheet = "Parameter_Summary")
  #Load curve summary file
  Curve_summ <<- read_excel(paste0(SiteCode, "_", VersionNumber, "/Data/", SiteCode, "_", VersionNumber, "_model_setup.xlsx"), sheet = "Curve_Summary")
  #Get file names and load curve score data
  curve_names <- unique(Curve_summ$Curve)
  curve_dir <- paste0(SiteCode, "_", VersionNumber, "/Data/HSI curves/")
  for (file_name in curve_names) {
    #Construct the file path (assuming files are in the current working directory)
    file_path <- paste0(curve_dir, file_name, ".xlsx")
    #Check if the file exists
    if (file.exists(file_path)) {
      #Read the Excel file
      data <- read_excel(file_path)
      #Assign the data to a variable in the global environment
      assign(file_name, data, envir = .GlobalEnv)
    } else {
      warning(paste("File does not exist:", file_path))
    }
  }
  
}
#
load_model_files()
#
#
####Assign scores
#
##Oysters
temp <- get(paste0(Site_Code, "_", Version, "_data"))
#Function to assign values based on the reference table
assign_oyster_values <- function(shapefile_data) {
  #
  #Identify columns that contain "Oyster" in their names. Limit data for processing:
  oyster_columns <- grep("Oyster", names(shapefile_data), value = TRUE)
  data <- shapefile_data %>% dplyr::select(PGID, all_of(oyster_columns)) %>% st_drop_geometry()
  # Iterate over each identified column
  for (col in oyster_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- sapply(data[[col]], function(x) {
      match_value <- Oysters$Value[match(x, Oysters$Param)]
      # Return the matched value or 0 if not found
      ifelse(is.na(match_value), 0, match_value)
    })
  }
  #Rename columns by note "_score"
  new_names <- ifelse(grepl("Oyster", names(data)), 
                      paste0(names(data), "_score"),
                      names(data))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
Oyster_scores <- assign_oyster_values(temp)
#
#
#
##Seagrass
assign_seagrass_values <- function(shapefile_data) {
  #
  variable_name <- "Seagrass"
  Score_tab <- get(variable_name)
  #Identify columns that contain variable_name in their names. Limit data for processing:
  data_columns <- grep(variable_name, names(shapefile_data), value = TRUE)
  data <- shapefile_data %>% dplyr::select(PGID, all_of(data_columns)) %>% st_drop_geometry()
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- sapply(data[[col]], function(x) {
      match_value <- Score_tab$Value[match(x, Score_tab$Param)]
      # Return the matched value or 0 if not found
      ifelse(is.na(match_value), 1, match_value)
    })
  }
  #Rename columns by note "_score"
  new_names <- ifelse(grepl(variable_name, names(data)), 
                      paste0(names(data), "_score"),
                      names(data))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
Seagrass_scores <- assign_seagrass_values(temp)
#
#
#
##Channels
#If not NA, then score = 0 (present in buffer zone). If NA then score = 1
assign_buffer_values <- function(shapefile_data) {
  #
  variable_name <- "Channel"
  #Identify columns that contain variable_name in their names. Limit data for processing:
  data_columns <- grep(variable_name, names(shapefile_data), value = TRUE)
  data <- shapefile_data %>% dplyr::select(PGID, all_of(data_columns)) %>% st_drop_geometry()
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- sapply(data[[col]], function(x) {
      #Return 1 if NA or 0 if !na
      ifelse(is.na(x), 1, 0)
    })
  }
  #Rename columns by note "_score"
  new_names <- ifelse(grepl(variable_name, names(data)), 
                      paste0(names(data), "_score"),
                      names(data))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
Channel_scores <- assign_buffer_values(temp)
#
#
##Function to process range value columns: average of values
process_ranges <- function(df){
  #Identify range columns:
  matching_columns <- grep("R[EAI].", names(df), value = TRUE)
  
  if(length(matching_columns) == 0){
    message("No range columns identified.")
    return(df)
  }
  
  #Extract pattern codes after R
  codes <- gsub(".*R([A-Z]{2}).*", "\\1", matching_columns)
  
  # Create grouping based on second letter after R
  groups <- substr(codes, 2, 2)
  unique_groups <- unique(groups)
  message("Found range patterns: ", paste(unique_groups, collapse=", "))
  
  for (group in unique_groups){
    group_cols <- matching_columns[groups == group]
    #Create new column name
    new_col <- paste0(gsub("R[A-Z].*", "R", group_cols[1]), "v", group)
    message("Processing group ", group, " (", length(group_cols), " columns)")
    message("- Columns: ", paste(group_cols, collapse=", "))  
    
    #Calculate row averages for the matching columns
    df[[new_col]] <- rowMeans(st_drop_geometry(df)[, group_cols, drop = FALSE], na.rm = TRUE)
  }
  
  #Remove the original Rang columns
  df <- df[, !names(df) %in% matching_columns]
  return(df)
}
#process_ranges(temp)
#
#
##Salinity - all year
assign_salinity_values <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "_L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("S")) %>% dplyr::select(PGID, matches(".*(O|I)$")) %>% dplyr::select(-contains("spwn")) %>%
    mutate_if(is.numeric, round, digits = 2) 
  data_columns <- setdiff(names(data), c("PGID", "geometry"))
  #Named vector for faster look up
  score_lookup <- setNames(curve_table$Value, curve_table$Param)
  #
  #If ensemble, get average interpolated value
  if(type == "ensemble"){
    #Create a new data frame for averaged columns
    averaged_data <- data.frame(PGID = data$PGID)
    #Group columns by their names excluding the last character
    column_groups <- split(data_columns, substr(data_columns, 1, nchar(data_columns) - 1))
    for (group in column_groups) {
      if (length(group) > 1) {
        #Base name 
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        # Average the columns in the group
        averaged_data[[new_name]] <- round(rowMeans(st_drop_geometry(data)[group], na.rm = TRUE),2)
      } else {
        # If only one column, just copy it
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        averaged_data[[new_name]] <- data[[group[1]]]
      }
    }
    data <- averaged_data
    data_columns <- names(data)[-1]  # Update data_columns to reflect the new averaged columns
  }
  #
  ##Scoring of values:
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  #
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
  #
  ##Output 
  print(head(data))
  return(data)
}
#
Salinity_scores <- assign_salinity_values(temp, Salinity_adult, type = "ensemble")
#
#Spawning period
assign_sal_spawn_values <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "_L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("S")) %>% dplyr::select(PGID, matches(".*(O|I)$")) %>% dplyr::select(PGID, contains("spwn")) %>%
    mutate_if(is.numeric, round, digits = 2)
  data_columns <- setdiff(names(data), c("PGID", "geometry"))
  #Named vector for faster lookup
  score_lookup <- setNames(curve_table$Value, curve_table$Param)
  #
  #If ensemble, get average interpolated value
  if(type == "ensemble"){
    #Create a new data frame for averaged columns
    averaged_data <- data.frame(PGID = data$PGID)
    #Group columns by their names excluding the last character
    column_groups <- split(data_columns, substr(data_columns, 1, nchar(data_columns) - 1))
    for (group in column_groups) {
      if (length(group) > 1) {
        #Base name 
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        # Average the columns in the group
        averaged_data[[new_name]] <- round(rowMeans(st_drop_geometry(data)[group], na.rm = TRUE),2)
      } else {
        # If only one column, just copy it
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        averaged_data[[new_name]] <- data[[group[1]]]
      }
    }
    data <- averaged_data
    data_columns <- names(data)[-1]  # Update data_columns to reflect the new averaged columns
  }
  #
  ##Scoring of values:
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  ##Check for ranges and process:
  data <- process_ranges(data)
  #
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
  #
  ##Output
  print(head(data))
  return(data)
}
#
Salinity_spawn_scores_t <- assign_sal_spawn_values(temp, Salinity_adult, type = "ensemble")
Salinity_spawn_scores <- left_join(Salinity_spawn_scores_t, assign_sal_spawn_values(temp, Salinity_larvae, type = "ensemble"))
#
#
#
##Temperature - all year
assign_temperature_values <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "_L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("T")) %>% dplyr::select(PGID, matches(".*(O|I)$")) %>% dplyr::select(-contains("spwn")) %>%
    mutate_if(is.numeric, round, digits = 2)
  data_columns <- setdiff(names(data), c("PGID", "geometry"))
  #Named vector for faster lookup
  score_lookup <- setNames(curve_table$Value, curve_table$Param)
  #
  #If ensemble, get average interpolated value
  if(type == "ensemble"){
    #Create a new data frame for averaged columns
    averaged_data <- data.frame(PGID = data$PGID)
    #Group columns by their names excluding the last character
    column_groups <- split(data_columns, substr(data_columns, 1, nchar(data_columns) - 1))
    for (group in column_groups) {
      if (length(group) > 1) {
        #Base name 
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        # Average the columns in the group
        averaged_data[[new_name]] <- round(rowMeans(st_drop_geometry(data)[group], na.rm = TRUE),2)
      } else {
        # If only one column, just copy it
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        averaged_data[[new_name]] <- data[[group[1]]]
      }
    }
    data <- averaged_data
    data_columns <- names(data)[-1]  # Update data_columns to reflect the new averaged columns
  }
  #
  #
  ##Scoring of values:
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  #
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
  ##Output
  print(head(data))
  return(data)
}
#
Temperature_scores <- assign_temperature_values(temp, Temperature_adult, type = "ensemble")
#
#Spawning period
assign_temperature_spawn_values <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "_L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("T")) %>% dplyr::select(PGID, matches(".*(O|I)$")) %>% 
    dplyr::select(PGID, contains("spwn")) %>% dplyr::select(-contains("spwnT")) %>%
    mutate_if(is.numeric, round, digits = 2)
  data_columns <- setdiff(names(data), c("PGID", "geometry"))
  #Named vector for faster lookup
  score_lookup <- setNames(curve_table$Value, curve_table$Param)
  #
  #If ensemble, get average interpolated value
  if(type == "ensemble"){
    #Create a new data frame for averaged columns
    averaged_data <- data.frame(PGID = data$PGID)
    #Group columns by their names excluding the last character
    column_groups <- split(data_columns, substr(data_columns, 1, nchar(data_columns) - 1))
    for (group in column_groups) {
      if (length(group) > 1) {
        #Base name 
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        # Average the columns in the group
        averaged_data[[new_name]] <- round(rowMeans(st_drop_geometry(data)[group], na.rm = TRUE),2)
      } else {
        # If only one column, just copy it
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        averaged_data[[new_name]] <- data[[group[1]]]
      }
    }
    data <- averaged_data
    data_columns <- names(data)[-1]  # Update data_columns to reflect the new averaged columns
  }
  #
  #
  ##Scoring of values:
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  #
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
Temperature_spawn_scores_t <- assign_temperature_spawn_values(temp, Temperature_adult, type = "ensemble")
Temperature_spawn_scores <- left_join(Temperature_spawn_scores_t, assign_temperature_spawn_values(temp, Temperature_larvae, type = "ensemble"))
#
#Threshold period - number = proportion above.below the threshold - score is inverse of values
assign_threshold_values <- function(shapefile_data, type = "separate") {
  #
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("T")) %>% dplyr::select(PGID, matches(".*(O|I)$")) %>% dplyr::select(PGID, contains("spwnT"))
  data_columns <- setdiff(names(data), c("PGID", "geometry"))
  #
  #
  #If ensemble, get average interpolated value
  if(type == "ensemble"){
    #Create a new data frame for averaged columns
    averaged_data <- data.frame(PGID = data$PGID)
    #Group columns by their names excluding the last character
    column_groups <- split(data_columns, substr(data_columns, 1, nchar(data_columns) - 1))
    for (group in column_groups) {
      if (length(group) > 1) {
        #Base name 
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        # Average the columns in the group
        averaged_data[[new_name]] <- round(rowMeans(st_drop_geometry(data)[group], na.rm = TRUE),2)
      } else {
        # If only one column, just copy it
        base_name <- substr(group[1], 1, nchar(group[1])-1)
        new_name <- paste0(base_name, "E")
        averaged_data[[new_name]] <- data[[group[1]]]
      }
    }
    data <- averaged_data
    data_columns <- names(data)[-1]  # Update data_columns to reflect the new averaged columns
  }
  #Re-scale values
  data <- data %>% mutate(across(-PGID, ~1-.x))
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score"))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
Temperture_thres_scores <- assign_threshold_values(temp, type = "ensemble")
#
#
#
#
###Add scores back to data
join_score_dataframes <- function(shp_df, join_by = "PGID", env = .GlobalEnv, verbose = TRUE) {
  # Input validation
  if (!inherits(shp_df, "sf")) {
    stop("Primary input shp_df must be a sf object")
  }
  
  if (!join_by %in% names(shp_df)) {
    stop(paste("Join column", join_by, "not found in shp_df"))
  }
  
  # Get all objects in the specified environment
  all_objects <- ls(envir = env)
  
  # Filter for data frames that match the pattern ".*_scores"
  score_df <- all_objects[sapply(all_objects, function(x) {
    obj <- get(x, envir = env)
    is.data.frame(obj) && grepl(".*_scores$", x)
  })]
  
  if (length(score_df) == 0) {
    if (verbose) message("No dataframes matching '.*_scores' pattern found")
    return(shp_df)
  }
  
  if (verbose) {
    message("Found ", length(score_df), " spatial score dataframes to join:")
    message(paste(score_df, collapse = ", "))
  }
  
  # Create a copy of original geometry for restoration
  original_geom <- st_geometry(shp_df)
  
  # Perform left joins with shapefile dataframe
  for (df_name in score_df) {
    if (verbose) message("Joining ", df_name, "...")
    
    score_df <- get(df_name, envir = env)
    
    # Verify join column exists in score dataframe
    if (!join_by %in% names(score_df)) {
      warning(paste("Join column", join_by, "not found in", df_name, "- skipping"))
      next
    }
    #Perform join 
    shp_df <- shp_df %>% left_join(score_df, by = join_by, suffix = c("", paste0(".", df_name)))
    
  }

  if (verbose) message("All score dataframes joined successfully")
  
  print(head(shp_df))
  return(shp_df)
}
#
assign(paste0(Site_Code, "_", Version, "_scores_data"), join_score_dataframes(temp))
#
#
#
###Calculate total HSM score
calculate_totals <- function(data_scores){
  #Oyster score
  oyster_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Oyster") & ends_with("score")) %>%
    mutate(across(contains("2024"), ~ .*1),
           across(contains("2023"), ~ .*0.8),
           across(contains("2022"), ~ .*0.6),
           across(contains("2021"), ~ .*0.4),
           across(contains("2020"), ~ .*0.2)) %>%
    mutate(Oyster_total = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>% 
    mutate(Oyster_count = ncol(dplyr::select(., -c(PGID, Oyster_total)))) %>% 
    dplyr::select(PGID, Oyster_total, Oyster_count) %>%
    mutate(Oyster_ave = Oyster_total/Oyster_count) %>%
    mutate(row_id = row_number())
  #
  #Seagrass score
  seagrass_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Seagrass") & ends_with("score")) %>%
    mutate(Seagrass_total = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(Seagrass_count = ncol(dplyr::select(., -c(PGID, Seagrass_total)))) %>% 
    dplyr::select(PGID, Seagrass_total, Seagrass_count) %>%
    mutate(Seagrass_ave = Seagrass_total/Seagrass_count) %>%
    mutate(row_id = row_number())
  #
  #Channel score
  channel_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Channel") & ends_with("score")) %>%
    #If type noted to any of the channel columns (meaning score was 0), score = 0
    mutate(Channel_total = ifelse(if_any(everything(dplyr::select(., -PGID)), ~. == 0), 0, 1)) %>%
    #mutate(Channel_count = ncol(dplyr::select(., -c(PGID, Channel_total)))) %>% 
    dplyr::select(PGID, Channel_total) %>% #, Channel_count) %>%
    mutate(row_id = row_number())
  #
  #Salinity score
  salinity_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, starts_with("S") & ends_with("E_score")) %>%
    mutate(Salinity_total = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(Salinity_count = ncol(dplyr::select(., -c(PGID, Salinity_total)))) %>% 
    dplyr::select(PGID, Salinity_total, Salinity_count) %>%
    mutate(Salinity_ave = Salinity_total/Salinity_count) %>%
    mutate(row_id = row_number())
  #
  #Temperature score
  temperature_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, starts_with("T") & ends_with("E_score")) %>%
    mutate(Temperature_total = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(Temperature_count = ncol(dplyr::select(., -c(PGID, Temperature_total)))) %>% 
    dplyr::select(PGID, Temperature_total, Temperature_count) %>%
    mutate(Temperature_ave = Temperature_total/Temperature_count) %>%
    mutate(row_id = row_number())
  #
  #Combine all totals to data frame
  all_totals <- (data_scores %>% mutate(row_id = row_number())) %>%
    left_join(oyster_total, by = c("row_id", "PGID")) %>%
    left_join(seagrass_total, by = c("row_id", "PGID")) %>%
    left_join(channel_total, by = c("row_id", "PGID")) %>%
    left_join(salinity_total, by = c("row_id", "PGID")) %>%
    left_join(temperature_total, by = c("row_id", "PGID")) %>%
    dplyr::select(-row_id)
  #
  print(head(all_totals))
  return(all_totals)
  #
}
#
assign(paste0(Site_Code, "_", Version, "_data_totals"), calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_data"))))
#
clean_model_data <- function(data){
  data_clean <- data %>% dplyr::select(-dplyr::ends_with("_count"), -dplyr::ends_with("_total"), dplyr::any_of("Channel_total"))
  #
  print(head(data_clean))
  return(data_clean)
}
#
assign(paste0(Site_Code, "_", Version, "_data_clean"), clean_model_data(get(paste0(Site_Code, "_", Version, "_data_totals"))))
#
#
#
#
#
UN_HSM_data <- UN_v1_data_clean %>% st_drop_geometry() %>% 
  mutate(Curve_count = sum(grepl("_ave$", names(st_drop_geometry(UN_v1_data_clean))))) %>% #as.numeric(rowSums(dplyr::select(., ends_with("_count")), na.rm = TRUE))) %>%
  mutate(HSM_all = case_when(Channel_total == 1 ~ (Oyster_ave + Seagrass_ave + Salinity_ave + Temperature_ave)/Curve_count,
                             Channel_total == 0 ~ 0, 
                             TRUE ~ NA_real_)) %>%
  mutate(HSM_allR = round(HSM_all, 2))
#Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#Assign groups using cut()
UN_HSM_data_grps <- UN_HSM_data %>%
  mutate(HSM_grp = as.factor(cut(HSM_allR, breaks = breaks, include.lowest = TRUE, right = FALSE))) %>%
  mutate(HSM_grp = case_when(HSM_allR == 0 ~ "0", 
                             HSM_grp == '[0,0.1)' ~ '(0,0.1)',
                             TRUE ~ as.character(HSM_grp))) %>%
  mutate(HSM_grp = factor(HSM_grp, levels = c("0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")))
summary(UN_HSM_data_grps$HSM_grp)
#
legend_holder <- data.frame(PGID = c("T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10"),
                            HSM_grp = factor(c("0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]"), 
                                             levels = c("0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")),
                            Lat_DD_Y = rep(12.0423067, 11),
                            Long_DD_X = rep(-61.7468122, 11))
#
#US_HSM_data_grps <- bind_rows(US_HSM_data_grps, legend_holder)
#
UN_HSM_spdf <- left_join(UN_v1_data, UN_HSM_data_grps)
#
#Check data
library(viridis)
tm_shape(US_HSM_spdf)+
  tm_polygons("HSM_grp")
#
#
##Save shape file output:
#st_write(US_HSM_spdf, paste0(Site_Code, "_", Version, "/Output/Shapefiles/", Site_Code, "_", Version, "_HSM_model.shp"), delete_dsn = TRUE)
#
# Function to save shapefile and split if necessary
save_shapefile <- function(data, SiteCode = Site_Code, VerNum = Version) {
  #
  #Temporary file path for the shapefile
  temp_file_path <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/temp.shp")
  temp_dbf <- sub("\\.shp$", ".dbf", temp_file_path)
  file_path <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/", SiteCode, "_", VerNum, "_HSM_model.shp")
  #Write the shape file to a temporary location
  st_write(data, temp_file_path, delete_dsn = TRUE)
  
  # Check the file size
  file_size <- file.info(temp_dbf)$size
  # If the file size is greater than 2 GB (2 * 1024^3 bytes)
  if (file_size > (2 * 1024^3)) {
    # Split the data into chunks
    chunk_size <- 2 * 1024^3  # 2 GB
    num_chunks <- ceiling(file_size / chunk_size)
    
    # Calculate the number of rows per chunk
    rows_per_chunk <- ceiling(nrow(data) / num_chunks)
    
    #New file path
    new_path <- sub("\\.shp", "", file_path)
    for (i in seq_len(num_chunks)) {
      # Determine the row indices for the current chunk
      start_row <- (i - 1) * rows_per_chunk + 1
      end_row <- min(i * rows_per_chunk, nrow(data))
      
      # Create a subset of the data for the current chunk
      chunk_data <- data[start_row:end_row, ]
      
      # Create a new file path for the chunk
      chunk_file_path <- paste0(new_path, "_section", i, ".shp")
      
      # Write the chunk to a new shapefile
      st_write(chunk_data, chunk_file_path, delete_dsn = TRUE)
    }
    
    message("Data split into ", num_chunks, " files.")
  } else {
    # If the file size is under 2 GB, save normally
    st_write(data, file_path, delete_dsn = TRUE)
    message("Shapefile saved successfully.")
  }
}
# Example usage: Assuming 'my_sf_data' is your sf object
# save_shapefile(my_sf_data)        
save_shapefile(UN_HSM_spdf)
