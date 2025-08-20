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
#Load data from all matching folders
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
temp <- UN_v1_data
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
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
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
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
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
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
  #Rename columns by note "_score"
  new_names <- ifelse(names(data) == "PGID", "PGID", 
                      paste0(names(data), "_score", helper))
  names(data) <- new_names
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
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(data[[col]] %in% names(score_lookup),
                          score_lookup[as.character(data[[col]])],
                          0)
  }
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
US_v1_data_scores <- join_score_dataframes(temp)
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
    mutate(row_id = row_number())
  #
  #Seagrass score
  seagrass_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Seagrass") & ends_with("score")) %>%
    mutate(Seagrass_total = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(Seagrass_count = ncol(dplyr::select(., -c(PGID, Seagrass_total)))) %>% 
    dplyr::select(PGID, Seagrass_total, Seagrass_count) %>%
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
    mutate(row_id = row_number())
  #
  #Temperature score
  temperature_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, starts_with("T") & ends_with("E_score")) %>%
    mutate(Temperature_total = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(Temperature_count = ncol(dplyr::select(., -c(PGID, Temperature_total)))) %>% 
    dplyr::select(PGID, Temperature_total, Temperature_count) %>%
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
US_v1_data_totals <- calculate_totals(US_v1_data_scores)
#
#
US_HSM_data <- US_v1_data_totals %>% st_drop_geometry() %>% 
  mutate(Curve_count = as.numeric(rowSums(dplyr::select(., ends_with("_count")), na.rm = TRUE))) %>%
  mutate(HSM_all = case_when(Channel_total == 1 ~ (Oyster_total + Seagrass_total + Salinity_total + Temperature_total)/Curve_count,
                             Channel_total == 0 ~ 0, 
                             TRUE ~ NA_real_)) %>%
  mutate(HSM_all = round(HSM_all, 2),
         HSM_allR = round(HSM_all, 1))
#Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)
#Assign groups using cut()
US_HSM_data <- US_HSM_data %>%
  mutate(HSM_grp = as.factor(cut(HSM_allR, breaks = breaks, include.lowest = TRUE, right = FALSE)))
#
US_HSM_spdf <- left_join(US_v1_data, US_HSM_data)
#
#
US_HSM_spdf_working <- US_HSM_spdf %>% group_by(HSM_all) %>%
  summarise(geometry = st_union(geometry))
#Check data
library(viridis)
tm_shape(US_HSM_spdf)+
  tm_polygons("HSM_grp")
#
tmap_leaflet(leaflet_map)
#
##Save shape file output:
st_write(US_HSM_spdf, paste0(Site_Code, "_", Version, "/Output/Shapefiles/", Site_Code, "_", Version, "_HSM_model.shp"), delete_dsn = TRUE)
         