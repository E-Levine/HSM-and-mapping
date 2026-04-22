## Functions for ground truthing and model comparisons
#
#
## Load validation data from matching shape files in Output/Shapefiles folder: SiteCode_Version_validation_data
#Currently loads as sfc for potential mapping, can change to df if not mapping later
load_survey_shpfiles <- function(SiteCode = Site_Code, VersionNumber = Version, shp_filename = "model_srvys"){
  data_dir <- paste0(SiteCode, "_", VersionNumber, "/Output/Shapefiles/")
  output_name <- paste0(SiteCode, "_", VersionNumber, "_validation_data")  
  #
  # Build match pattern:
  pattern <- paste0("^", SiteCode, ".*", shp_filename, ".*", SurveyYYMM,"\\.shp$")
  # List all matching shape files
  shp_files <- list.files(path = data_dir, pattern = pattern, full.names = TRUE)
  if (length(shp_files) == 0) {
    stop("No shapefiles found matching the pattern.")
  } else {
    #Print list of files loaded:
    message("Files loaded:\n", paste(shp_files, collapse = "\n"))
    #
    
    if (length(shp_files) == 1) {
      shape_obj <- st_read(shp_files[1])
    } else {
      # Read and combine all shape files
      shape_list <- lapply(shp_files, function(file) {
        sf_obj <- st_read(file)
        
        # Extract Section from file name
        base_name <- basename(file)  # e.g., "ABC123_datalayer.shp"
        section <- str_extract(base_name,"(.*)(?=_model_srvys)")  # Remove _datalayer suffix, e.g., "ABC123"
        sf_obj$Section <- section  # Add Section column
        return(sf_obj)
      })
      shape_obj <- do.call(rbind, shape_list)  
    }
  }
  #Check for and remove duplicate rows (created during split)
  # Install and load necessary packages
  if (!require(data.table)) install.packages("data.table")
  library(data.table)
  # Convert to data.table (handles both sf and data.frame)
  if (inherits(shape_obj, "sf")) {
    object_type <- "sf"
    geometry_col <- attr(shape_obj, "sf_column")
    setDT(shape_obj)  # Converts sf to data.table in place
  } else if (!is.data.table(shape_obj)) {
    object_type <- "not_sf"
    setDT(shape_obj)  # Convert standard data.frame to data.table
    geometry_col <- NULL
  }
  #Get column names
  group_by_cols <- setdiff(names(shape_obj), c("Section", geometry_col))
  # Perform grouping by "PGID" and remove duplicates
  result <- shape_obj[, .SD[1], by = group_by_cols]
  # If it was an sf object, convert back to sf to preserve geometry
  if (object_type == "sf") {
    setDF(result)  # Convert back to data.frame (if needed)
    result_sf <- st_as_sf(result)  # Re-attach sf class
    result <- result_sf  # Update result to the sf object
  }
  #Clean up memory
  rm(shape_obj)  # Remove original object
  gc()    # Run garbage collection, especially for large data
  #assign shp to object
  assign(output_name, result, envir = .GlobalEnv)
}
#
#
## Load survey/ground truthing data 
load_survey_data <- function(Site_Code, Version, SurveyYYMM, FileType = "data") {
  
  library(readxl)
  # Create file path:
  file_path <- file.path(
    paste0(Site_Code, "_", Version),
    "Data",
    paste0(Site_Code, "_groundtruthing_", SurveyYYMM, ".xlsx")
  )
  
  Srvy_quad <- NULL
  Srvy_LL <- NULL
  # Check for file:
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(list(Srvy_quad = NULL, Srvy_LL = NULL))
  }
  
  sheets <- excel_sheets(file_path)
  
  # Load Quadrat sheet:
  if ("Quadrat" %in% sheets) {
    Srvy_quad <- read_excel(file_path, sheet = "Quadrat", .name_repair = "universal")
  } else {
    warning("'Quadrat' sheet not found in file.")
  }
  
  # Load SampleEvent sheet:
  if ("SampleEvent" %in% sheets) {
    Srvy_LL <- read_excel(file_path, sheet = "SampleEvent", .name_repair = "universal")
  } else {
    warning("'SampleEvent' sheet not found in file.")
  }
  
  return(list(
    Srvy_quad = Srvy_quad,
    Srvy_LL = Srvy_LL
  ))
}
#
#
## Clean database file
# Combines LL with count data, calculates DeadRatio, summarizes by station
# Requires a Date column in either the quad or LL df.
clean_database_file <- function(Srvy_quad, Srvy_LL) {
  
  library(dplyr)
  
  # Add Latitude / Longitude to quadrat data
  Srvy_quad2 <- Srvy_quad %>%
    left_join(
      Srvy_LL %>%
        dplyr::select(
          SampleEventID,
          Latitude = LatitudeDec,
          Longitude = LongitudeDec
        ),
      by = "SampleEventID"
    ) %>%
    mutate(across(c(Latitude, Longitude, TotalVolume, TotalWeight, NumDrills, NumLive, NumDead), as.numeric)) %>%
    mutate(Station = substr(SampleEventID, 19, 22),
           DeadRatio = as.numeric(NumDead/(NumLive+NumDead)))
  
  # Make sure data is valid and Longitudes are correct:
  Srvy_quad3 <- Srvy_quad2 %>%
    mutate(NumLive = case_when(QuadratNumber == 0 ~ 0, TRUE ~ NumLive),
           NumDead = case_when(QuadratNumber == 0 ~ 0, TRUE ~ NumDead)) %>%
    mutate(Longitude = as.numeric(ifelse(Longitude > 0, Longitude*-1, Longitude)))
  
  # Columns that must exist
  req_cols <- c("Spat", "Adult", "Legal", "SpatAdult")
  
  # Create missing columns
  for (col in req_cols) {
    if (!col %in% names(Srvy_quad3)) {
      if (col == "SpatAdult" && "NumLive" %in% names(Srvy_quad3)) {
        Srvy_quad3[[col]] <- Srvy_quad3$NumLive
      } else {
        Srvy_quad3[[col]] <- NA
      }
    }
  }
  
  # Summarize by station and clean to desired columns 
  Srvy_quad4 <- Srvy_quad3 %>%
    mutate(across(any_of(c("Spat", "Adult", "Legal")), as.numeric)) %>%
    group_by(Date, SampleEventID, Latitude, Longitude) %>%
    summarise(
      n_quadrats = n_distinct(QuadratID),
      across(
        any_of(c("NumLive", "NumDead", "TotalVolume", "TotalWeight", "NumLegal",
                 "DeadRatio", "Spat", "Adult", "Legal", "SpatAdult")),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) 
  
  Srvy_quad5 <- Srvy_quad4 %>%
    mutate(SampleEvent = substr(SampleEventID, 1, 17)) %>%
    distinct(across(-SampleEventID), .keep_all = TRUE)
  
  return(Srvy_quad5)
}
#
#
## Summarize data
# Summarize NumLive, DeadRatio, SpatAdult, Presence by HSMgrp score
clean_survey_data <- function(surveyData){
  # checks
  if (!is.data.frame(surveyData) && !is_tibble(surveyData)) {
    stop("Input 'surveyData' must be a data frame or tibble.")
  }
  #
  if (!any(grepl("NumLive", names(surveyData)))) {
    stop("No column containing 'NumLive' found in surveyData.")
  }
  if (!any(grepl("DeadRatio", names(surveyData)))) {
    stop("No column containing 'DeadRatio' found in surveyData.")
  }
  required_cols <- c("Spat", "Adult", "Legal", "SpatAdult")
  missing_cols <- setdiff(required_cols, names(surveyData))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  #
  # Define HSM grps
  expected_levels <- c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")
  
  # Define na replacement case:
  na_condition <- (surveyData$Spat %in% c(0, NA) & surveyData$Adult %in% c(0, NA) & surveyData$Legal %in% c(0, NA))
  #
  cleaned_data <- surveyData %>% 
    as.data.frame() %>%
    # Rename columns for consistency 
    rename_with(~ sub(".*NumLive.*", "NumLive", .x), matches("NumLive")) %>%
    rename_with(~ sub(".*DeadRatio.*", "DeadRatio", .x), matches("DeadRatio")) %>%
    mutate(across(starts_with("HSMgrp"), ~ factor(.x, levels = expected_levels, ordered = TRUE))) %>%
    # Replace 0 with NA when proper
    mutate(DeadRatio = as.numeric(DeadRatio),
           SpatAdult = as.numeric(SpatAdult),
           DeadRatio = if_else(na_condition, NA_real_, DeadRatio),
           SpatAdult = if_else(na_condition, NA_real_, SpatAdult))
  #
  # Create Presence column if missing
  if (!"Presence" %in% names(cleaned_data)) {
    if (!"NumLive" %in% names(cleaned_data)) {
      stop("Column 'NumLive' is required to create Presence.")
    }
    
    cleaned_data <- cleaned_data %>%
      mutate(
        PresenceL = case_when(
          is.na(NumLive) ~ NA_real_,
          NumLive > 0 ~ 1,
          TRUE ~ 0
        ),
        PresenceD = case_when(
          is.na(NumDead) ~ NA_real_,
          NumDead > 0 ~ 1,
          TRUE ~ 0
        ),
        Presence = case_when(
          is.na(NumLive) & is.na(NumDead) ~ NA_real_,
          NumLive > 0 | NumDead > 0 ~ 1,
          TRUE ~ 0
        )
      )
  }
  
  return(cleaned_data)
  #
}
#
#
#
# Function to summarize (mean/sd, min, max) GT data by raw HSM values. Updated to calculate for each HSM* column:
summarize_data <- function(cleanedData){
  
  # checks
  if (!is.data.frame(cleanedData) && !tibble::is_tibble(cleanedData)) {
    stop("Input 'cleanedData' must be a data frame or tibble.")
  }
  
  # find ALL HSM grp columns
  hsm_cols <- grep("^HSMgrp", names(cleanedData), value = TRUE)
  
  if (length(hsm_cols) == 0) {
    stop("No grouped 'HSM' columns found in cleanedData")
  }
  
  required_cols <- c("NumLive", "DeadRatio", "SpatAdult", grep("Presence", names(cleanedData), value = TRUE))
  
  missing_cols <- setdiff(required_cols, names(cleanedData))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  for (col in required_cols) {
    if (!is.numeric(cleanedData[[col]])) {
      stop(paste("Column", col, "must be numeric for summarization."))
    }
  }
  
  # Define HSM grps
  expected_levels <- c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")
  
  # Check if all expected levels are present
  #actual_levels <- unique(na.omit(cleanedData$HSMgrp))
  #missing_levels <- setdiff(expected_levels, actual_levels)
  
  #if (length(missing_levels) > 0) {
  #  warning(paste("Expected HSMgrp levels missing:", paste(missing_levels, collapse = ", "), ". Proceeding with available levels."))
  #}
  
  # reshape to long format across HSMgrp variants
  summarized_data <- suppressWarnings(
    cleanedData %>%
      tidyr::pivot_longer(
        cols = all_of(hsm_cols),
        names_to = "HSM_type",
        values_to = "HSMgrp"
      ) %>%
      mutate(
        HSMgrp = factor(HSMgrp, levels = expected_levels, ordered = TRUE)
      ) %>%
      group_by(HSM_type, HSMgrp) %>%
      summarise(
        across(
          all_of(required_cols),
          list(
            n = ~ sum(!is.na(.x)),
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            min = ~ min(.x, na.rm = TRUE),
            max = ~ max(.x, na.rm = TRUE)
          ),
          .names = "{.col}_{fn}"
        ),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(
        cols = -c(HSM_type, HSMgrp),
        names_to = c("variable", "stat"),
        names_sep = "_",
        values_to = "value"
      ) %>%
      tidyr::pivot_wider(names_from = stat, values_from = value) %>%
      mutate(
        min = ifelse(is.infinite(min), NA_real_, min),
        max = ifelse(is.infinite(max), NA_real_, max)
      )
  )
  
  return(summarized_data)
}
#
#
#
# Function to save shapefile and split if necessary
save_final_model_output <- function(data = NULL, #good for single output, but not needed
                                    SiteCode = Site_Code, 
                                    VerNum = Version,
                                    output_type = c("all", "data", "scores", "model")) {
  #
  output_type <- match.arg(output_type)
  #
  # Break data by type:
  scores <- data %>% dplyr::select(PGID, Lat_DD_Y, Long_DD_X, contains("SC"), ChnlTO, Curve_val, contains("AV"))
  model <- data %>% dplyr::select(PGID, Lat_DD_Y, Long_DD_X, contains("AV"), ChnlTO, Curve_val, contains("HSM"))
  # Prepare objects ----
  HSM_shp_output <- model %>% dplyr::select(-ends_with("SC"), -ends_with("SCL"),-contains("Shape"), -ends_with("CO"), -ends_with("TO"))
  HSM_model_csv <- HSM_shp_output %>% st_drop_geometry()
  Model_scores_csv <- scores %>% dplyr::select(-contains("HSM")) %>% st_drop_geometry()
  Model_data_csv <- data %>% st_drop_geometry()
  #
  
  # Paths ----
  CSV_data_path <- paste0(SiteCode, "_", VerNum, "/Output/Data files/", 
                          SiteCode, "_", VerNum, "_final_model_data_",Sys.Date(),".csv")
  CSV_scores_path <- paste0(SiteCode, "_", VerNum, "/Output/Data files/", 
                            SiteCode, "_", VerNum, "_final_model_scores_",Sys.Date(),".csv")
  shapefile_temp <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/temp.shp")
  shapefile_path <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/", 
                           SiteCode, "_", VerNum, "_HSM_final_model_",Sys.Date(),".shp")
  #
  # ---- Save CSV for "data" ----
  if(output_type %in% c("data", "all")) {
    data.table::fwrite(Model_data_csv, CSV_data_path)
    message(paste0("CSV data file saved to: ", CSV_data_path))
  }
  
  # ---- Save CSV for "scores" ----
  if(output_type %in% c("scores", "all")) {
    data.table::fwrite(Model_scores_csv, CSV_scores_path)
    message(paste0("CSV of scores saved to: ", CSV_scores_path))
  }
  
  # ---- Save shapefile and model CSV ----
  if(output_type %in% c("model", "all")) {
    
    st_write(HSM_shp_output, shapefile_temp, delete_dsn = TRUE)
    
    dbf_temp <- sub("\\.shp$", ".dbf", shapefile_temp)
    file_size <- file.info(dbf_temp)$size
    
    if(file_size > (2 * 900^3)) {
      chunk_size <- 2 * 900^3
      num_chunks <- ceiling(file_size / chunk_size)
      rows_per_chunk <- ceiling(nrow(HSM_shp_output) / num_chunks)
      new_path <- sub("\\.shp$", "", shapefile_path)
      
      for(i in seq_len(num_chunks)) {
        start_row <- (i - 1) * rows_per_chunk + 1
        end_row <- min(i * rows_per_chunk, nrow(HSM_shp_output))
        chunk_data <- HSM_shp_output[start_row:end_row, ]
        chunk_file <- paste0(new_path, "_section", i, ".shp")
        st_write(chunk_data, chunk_file, delete_dsn = TRUE)
      }
      message("Data split into ", num_chunks, " shapefiles.")
      
    } else {
      st_write(HSM_shp_output, shapefile_path, delete_dsn = TRUE)
      message("Shapefile saved successfully.")
    }
    
    # CSV of shapefile/model
    model_csv_path <- paste0(SiteCode, "_", VerNum, "/Output/Data files/", 
                             SiteCode, "_", VerNum, "_final_model_shp.csv")
    data.table::fwrite(HSM_model_csv, model_csv_path)
    message(paste0("CSV of model outcome saved to: ", model_csv_path))
  }
}