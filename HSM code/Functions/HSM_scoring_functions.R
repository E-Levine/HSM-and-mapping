#### Scoring HSM functions
#
###Load shape file with data:
#
#Load data from all matching folders: SiteCode_Version_data
load_model_files <- function(SiteCode = Site_Code, VersionNumber = Version, shp_filename = "_datalayer"){
  data_dir <- paste0(SiteCode, "_", VersionNumber, "/Output/Shapefiles/")
  output_name <- paste0(SiteCode, "_", VersionNumber, "_data")  
  #
  # Build match pattern:
  pattern <- paste0("^", SiteCode, ".*", shp_filename, "\\.shp$")
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
        #without_ext <- sub("\\.shp$", "", base_name)  # Remove .shp extension, e.g., "ABC123_datalayer"
        section <- str_extract(base_name,"(?<=Model_)(.*)(?=_datalayer)")  # Remove _datalayer suffix, e.g., "ABC123"
        sf_obj$Section <- section  # Add Section column
        return(sf_obj)
      })
      shape_obj <- do.call(rbind, shape_list)  
      #shape_list <- lapply(shp_files, st_read)
      #shape_obj <- do.call(rbind, shape_list)
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
#
#Function to list all files in specfied directory of specified file type. Used for loading interp data:
list_files <- function(directory,
                       pattern = NULL,
                       recursive = FALSE) {
  
  if (!dir.exists(directory)) {
    stop("Directory does not exist: ", directory)
  }
  
  files <- list.files(
    path = directory,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )
  
  if (length(files) == 0) {
    return(tibble::tibble(file = character(), column = character()))
  }
  
  get_cols <- function(file) {
    ext <- tolower(tools::file_ext(file))
    
    cols <- try({
      switch(
        ext,
        csv  = names(readr::read_csv(file, show_col_types = FALSE, n_max = 0)),
        xlsx = names(readxl::read_excel(file, n_max = 0)),
        xls  = names(readxl::read_excel(file, n_max = 0)),
        fst  = names(fst::read_fst(file, from = 1, to = 1)),
        rds  = {
          obj <- readRDS(file)
          if (is.data.frame(obj)) names(obj) else character(0)
        },
        character(0)
      )
    }, silent = TRUE)
    
    if (inherits(cols, "try-error")) cols <- character(0)
    
    tibble::tibble(
      file   = basename(file),
      column = cols
    )
  }
  
  purrr::map(files, get_cols) |>
    dplyr::bind_rows()
}
#
#
#Function to load specified Excel file, join column to existing object under specified column name,
add_excel_columns_sf <- function(existing_sf,
                                 excel_path,
                                 join_by,
                                 excel_join_by = join_by,
                                 excel_columns,
                                 new_column_names = NULL,
                                 sheet = 1,
                                 join_type = c("left", "inner", "right", "full"),
                                 drop_geometry = FALSE,
                                 cache = FALSE) {
  
  join_type <- match.arg(join_type)
  
  # Checks ----
  if (!inherits(existing_sf, "sf")) {
    stop("existing_sf must be an sf object")
  }
  
  if (!file.exists(excel_path)) {
    stop("Excel file does not exist: ", excel_path)
  }
  
  if (!join_by %in% names(existing_sf)) {
    stop("Join column not found in existing_sf: ", join_by)
  }
  
  ext <- tools::file_ext(excel_path)  
  
  # Column names without loading Excel ----
  excel_colnames <- switch(
    ext,
    xlsx = names(readxl::read_excel(excel_path, sheet = sheet, n_max = 0)),
    xls  = names(readxl::read_excel(excel_path, sheet = sheet, n_max = 0)),
    csv  = names(readr::read_csv(excel_path, n_max = 0, show_col_types = FALSE)),
    fst  = fst::fst_metadata(excel_path)$columnNames,
    stop("Unsupported file type: ", ext)
  )
  
  if (!excel_join_by %in% excel_colnames) {
    stop("Join column not found in Excel data: ", excel_join_by)
  }

  # Resolve tidyselect / character input ----
  selected_cols <- tidyselect::eval_select(
    rlang::enquo(excel_columns),
    setNames(seq_along(excel_colnames), excel_colnames)
  )
  
  selected_names <- names(selected_cols)
  numeric_cols <- selected_names
  
  if (length(selected_names) == 0) {
    stop("No columns matched `excel_columns`")
  }
  
  cols_to_keep <- unique(c(excel_join_by, selected_names))
  
  # Optional renaming ----
  if (!is.null(new_column_names)) {
    
    if (is.character(new_column_names)) {
      
      if (is.null(names(new_column_names))) {
        if (length(new_column_names) != length(selected_names)) {
          stop("new_column_names must match number of selected columns")
        }
        names(new_column_names) <- selected_names
      }
      
      if (!all(names(new_column_names) %in% selected_names)) {
        stop("Names of new_column_names must match selected columns")
      }
      
    } else {
      stop("new_column_names must be a character vector or NULL")
    }
  }
  
  # ---- Reader with caching ----
  reader <- function() {
    switch(
      ext,
      xlsx = readxl::read_excel(excel_path, sheet = sheet),
      xls  = readxl::read_excel(excel_path, sheet = sheet),
      csv  = readr::read_csv(excel_path,
                             show_col_types = FALSE),
      fst  = fst::read_fst(excel_path, columns = cols_to_keep)
    )
  }
  
  if (cache) {
    reader <- memoise::memoise(reader)
  }
  
  excel_data <- reader()
  
  excel_subset <- excel_data %>%
    dplyr::select(dplyr::all_of(cols_to_keep)) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(numeric_cols),
      ~ suppressWarnings(as.numeric(.x))
    ))
  
  
  # Apply renaming only if requested ----
  if (!is.null(new_column_names)) {
    excel_subset <- excel_subset %>%
      dplyr::rename(!!!new_column_names)
    
    numeric_cols <- unname(new_column_names[numeric_cols])
  }
  
  
  # Join (sf-safe) ----
  geom <- sf::st_geometry(existing_sf)
  sf::st_geometry(existing_sf) <- NULL
  
  joined_sf <- switch(
    join_type,
    left  = dplyr::left_join(existing_sf, excel_subset,
                             by = setNames(excel_join_by, join_by)),
    inner = dplyr::inner_join(existing_sf, excel_subset,
                              by = setNames(excel_join_by, join_by)),
    right = dplyr::right_join(existing_sf, excel_subset,
                              by = setNames(excel_join_by, join_by)),
    full  = dplyr::full_join(existing_sf, excel_subset,
                             by = setNames(excel_join_by, join_by))
  )
  
  sf::st_geometry(joined_sf) <- geom
  
  if (drop_geometry) {
    joined_sf <- sf::st_drop_geometry(joined_sf)
  }
  
  return(joined_sf)
}
#
#Function to load multiple CSV files and join into one
read_data_files_csv <- function(Site_Code,
                                Version,
                                data_subdir,  
                                pattern = "\\.csv$",
                                ...) {
  # Set base path
  base_path <- file.path(
    paste0(Site_Code, "_", Version),
    "Output",
    "Data files",
    data_subdir
  )
  # Get list of files to load
  files <- list.files(
    path = base_path,
    pattern = pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No files found in: ", base_path)
  }
  # Read and join files
  data.table::rbindlist(
    lapply(files, data.table::fread, ...),
    use.names = TRUE,
    fill = TRUE
  )
}
#
#Function to average across specified columns (cols) and return only specified columms (keep_columns) with new aver co;l:
row_average <- function(data,
                           cols,
                           new_column_name = "row_mean",
                           keep_columns = NULL,
                           na.rm = TRUE,
                           warn_non_numeric = TRUE) {
  
  # Drop geometry if sf
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }
  
  # Resolve tidyselect / character input
  selected_cols <- tidyselect::eval_select(
    rlang::enquo(cols),
    data
  )
  
  col_names <- names(selected_cols)
  
  if (length(col_names) == 0) {
    stop("No columns matched `cols`")
  }
  
  # Keep only numeric columns
  is_num <- vapply(data[, col_names, drop = FALSE], is.numeric, logical(1))
  numeric_cols <- col_names[is_num]
  dropped_cols <- col_names[!is_num]
  
  if (length(numeric_cols) == 0) {
    stop("None of the selected columns are numeric")
  }
  
  if (warn_non_numeric && length(dropped_cols) > 0) {
    warning("Dropped non-numeric columns: ", paste(dropped_cols, collapse = ", "))
  }
  
  # Compute row-wise mean
  data[[new_column_name]] <- rowMeans(
    data[, numeric_cols, drop = FALSE],
    na.rm = na.rm
  )
  
  # Subset columns if requested
  if (!is.null(keep_columns)) {
    if (!all(keep_columns %in% names(data))) {
      stop("Some keep_columns do not exist in data")
    }
    final_cols <- unique(c(keep_columns, new_column_name))
    data <- data[, final_cols, drop = FALSE]
  }
  
  # Ensure plain data frame
  data <- as.data.frame(data)
  
  return(data)
}
#
#
#Function to assign values based on the reference table
assign_oyster_scores <- function(shapefile_data) {
  #
  # Check if "Oysters" exists
  if (!exists("Oysters", envir = .GlobalEnv)) {
    stop("Error: 'Oysters' not found. Please checked loaded Parameter curve information.")
  }
  #Identify columns that contain "Oyst" in their names. Limit data for processing:
  oyster_columns <- grep("Oyst", names(shapefile_data), value = TRUE)
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
  #Rename columns by note "SC"
  new_names <- ifelse(grepl("Oyst", names(data)), 
                      paste0(names(data), "SC"),
                      names(data))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
#
##Function to assign reef buffer values based on the reference table
assign_oybuffer_scores <- function(shapefile_data) {
  #
  # Check if curve info exists in the global environment
  if (!exists("Oyster reef buffer", envir = .GlobalEnv)) {
    stop("Error: 'Oyster reef buffer' not found in the global environment. Please ensure it is loaded and available.")
  }
  #Identify columns that contain "Buff" in their names. Limit data for processing:
  buffer_columns <- grep("Buff", names(shapefile_data), value = TRUE)
  data <- shapefile_data %>% dplyr::select(PGID, all_of(buffer_columns)) %>% st_drop_geometry()
  # Iterate over each identified column
  for (col in buffer_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- sapply(data[[col]], function(x) {
      match_value <- `Oyster reef buffer`$Value[match(x, `Oyster reef buffer`$Param)]
      # Return the matched value or 0 if not found
      ifelse(is.na(match_value), 0, match_value)
    })
  }
  #Rename columns by note "SC"
  new_names <- ifelse(grepl("Buff", names(data)), 
                      paste0(names(data), "SC"),
                      names(data))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
#
##Seagrass scoring
assign_seagrass_scores <- function(shapefile_data) {
  #
  # Check if curve info exists in the global environment
  if (!exists("Seagrass", envir = .GlobalEnv)) {
    stop("Error: 'Seagrass' not found in the global environment. Please ensure it is loaded and available.")
  }
  #
  variable_name <- "Seagrass"
  column_name <- "Sgrs"
  Score_tab <- get(variable_name)
  #Identify columns that contain column_name in their names. Limit data for processing:
  data_columns <- grep(column_name, names(shapefile_data), value = TRUE)
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
  #Rename columns by note "SC"
  new_names <- ifelse(grepl(column_name, names(data)), 
                      paste0(names(data), "SC"),
                      names(data))
  #Make sure score is numeric column
  names(data) <- new_names
  data <- data %>%
    mutate(across(-PGID, ~ suppressWarnings(as.numeric(as.character(.)))))
  print(head(data))
  return(data)
}
#
#
##Channels
#If not NA, then score = 0 (present in buffer zone). If NA then score = 1
assign_buffer_scores <- function(shapefile_data) {
  #
  # Check if curve info exists in the global environment
  if (!exists("Channel buffer", envir = .GlobalEnv)) {
    stop("Error: 'Channel' not found in the global environment. Please ensure it is loaded and available.")
  }
  #
  variable_name <- "Channel"
  column_name <- "Chnl"
  #Identify columns that contain column_name in their names. Limit data for processing:
  data_columns <- grep(column_name, names(shapefile_data), value = TRUE)
  data <- shapefile_data %>% dplyr::select(PGID, all_of(data_columns)) %>% st_drop_geometry()
  # Iterate over each identified column
  for (col in data_columns) {
    #Find corresponding values from the reference table
    data[[col]] <- sapply(data[[col]], function(x) {
      #Return 1 if NA or 0 if !na
      ifelse(is.na(x), 1, 0)
    })
  }
  #Rename columns by note "SC"
  new_names <- ifelse(grepl(column_name, names(data)), 
                      paste0(names(data), "SC"),
                      names(data))
  names(data) <- new_names
  print(head(data))
  return(data)
}
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
##Salinity scores
assign_salinity_scores <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("S")) %>% dplyr::select(PGID, matches(".*(o|i|e)$")) %>% dplyr::select(-contains("spwn")) %>%
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
    #Round data values
    rounded_col <- round(data[[col]], 2)
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(rounded_col %in% names(score_lookup),
                          score_lookup[as.character(rounded_col)],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  
  # Make sure numeric
  data <- data %>%
    mutate(across(-c(PGID, geometry), ~ suppressWarnings(as.numeric(as.character(.)))))
  #
  #Rename columns by note "_score"
  new_names <- names(data)  # start with current names
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    if (!(col %in% c("PGID", "geometry"))) {
      new_names[i] <- paste0(col, "SC", helper)
    }
  }
  
  names(data) <- new_names
  #
  ##Output 
  print(head(data))
  return(data)
}
#
##Spawning period
assign_sal_spawn_scores <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("S")) %>% dplyr::select(PGID, matches(".*(o|i|e)$")) %>% dplyr::select(PGID, contains("Spwn")) %>%
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
    #Round data values
    rounded_col <- round(data[[col]], 2)
    
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(rounded_col %in% names(score_lookup),
                          score_lookup[as.character(rounded_col)],
                          0)
  }
  ##Check for ranges and process:
  data <- process_ranges(data)
  # Make sure numeric
  data <- data %>%
    mutate(across(-c(PGID, geometry), ~ suppressWarnings(as.numeric(as.character(.)))))
  #
  #Rename columns by note "_score"
  new_names <- names(data)  # start with current names
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    if (!(col %in% c("PGID", "geometry"))) {
      new_names[i] <- paste0(col, "SC", helper)
    }
  }
  
  names(data) <- new_names
 #
  ##Output
  print(head(data))
  return(data)
}
#
#
##Temperature scores
assign_temperature_scores <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("T")) %>% dplyr::select(PGID, matches(".*(o|i|e)$")) %>% dplyr::select(-matches(".*Spwn.*")) %>% dplyr::select(-matches("T[AB]\\d{1,3}")) %>%
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
    #Round data values
    rounded_col <- round(data[[col]], 2)
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(rounded_col %in% names(score_lookup),
                          score_lookup[as.character(rounded_col)],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  # Make sure numeric
  data <- data %>%
    mutate(across(-c(PGID, geometry), ~ suppressWarnings(as.numeric(as.character(.)))))
  #
  #
  #Rename columns by note "_score"
  new_names <- names(data)  # start with current names
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    if (!(col %in% c("PGID", "geometry"))) {
      new_names[i] <- paste0(col, "SC", helper)
    }
  }
  
  names(data) <- new_names

  ##Output
  print(head(data))
  return(data)
}
#
#Spawning period
assign_temperature_spawn_scores <- function(shapefile_data, curve_table, type = "separate") {
  #
  table_name <- deparse(substitute(curve_table))
  helper <- ifelse(grepl("larvae", table_name, ignore.case = TRUE), "L", "")
  Score_tab <- curve_table
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("T")) %>% dplyr::select(PGID, matches(".*(o|i|e)$")) %>% 
    dplyr::select(PGID, contains("Spwn")) %>% dplyr::select(-contains("SpwnT")) %>% #Keep spwn but not spwn threshold
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
    #Round data values
    rounded_col <- round(data[[col]], 2)
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(rounded_col %in% names(score_lookup),
                          score_lookup[as.character(rounded_col)],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  # Make sure numeric
  data <- data %>%
    mutate(across(-c(PGID, geometry), ~ suppressWarnings(as.numeric(as.character(.)))))
  #
  #
  #Rename columns by note "_score"
  new_names <- names(data)  # start with current names
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    if (!(col %in% c("PGID", "geometry"))) {
      new_names[i] <- paste0(col, "SC", helper)
    }
  }
  
  names(data) <- new_names
  #
  print(head(data))
  return(data)
}
#
#
#Threshold period - number = proportion above.below the threshold - score is inverse of values
assign_threshold_scores <- function(shapefile_data, type = "separate") {
  #  
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, matches(".*(o|i|e)T.\\d{1,3}$"))#dplyr::select(PGID, starts_with("T")) %>% dplyr::select(PGID, matches(".*(o|i|e)$")) %>% dplyr::select(PGID, matches(".*SpwnT.*|T[AB]\\d{1,3}"))
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
  data <- data %>% mutate(across(-c(PGID, geometry), ~1-.x))
  #Rename columns by note "_score"
  new_names <- names(data)  # start with current names
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    if (!(col %in% c("PGID", "geometry"))) {
      new_names[i] <- paste0(col, "SC")
    }
  }
  
  names(data) <- new_names
  #
  print(head(data))
  return(data)
}
#
#
##Flow optimal scores
assign_flow_scores <- function(shapefile_data, curve_table, col_pattern, type = c("separate", "ensemble")) {
  #
  type <- match.arg(type)
  #
  # Named vector for faster lookup
  score_lookup <- setNames(curve_table$Value, curve_table$Param)
  #Identify columns of salinity, not spawning:
  data <- shapefile_data %>% dplyr::select(PGID, starts_with("F")) %>% dplyr::select(PGID, matches(col_pattern)) %>%
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
      #Base name 
      base_name <- substr(group[1], 1, nchar(group[1])-1)
      new_name <- paste0(base_name, "E")     
      if (length(group) > 1) {
        # Average the columns in the group
        averaged_data[[new_name]] <- round(rowMeans(st_drop_geometry(data)[group], na.rm = TRUE),2)
      } else {
        # If only one column, just copy it
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
    #Round data values
    rounded_col <- round(data[[col]], 2)
    #Find corresponding values from the reference table
    data[[col]] <- ifelse(rounded_col %in% names(score_lookup),
                          score_lookup[as.character(rounded_col)],
                          0)
  }
  #
  ##Check for ranges and process:
  data <- process_ranges(data)
  #
  #Rename columns by note "_score"
  new_names <- names(data)  # start with current names
  
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    if (!(col %in% c("PGID", "geometry"))) {
      new_names[i] <- paste0(col, "SC")
    }
  }
  
  names(data) <- new_names
  #
  ##Output 
  print(head(data))
  return(data)
}
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
    shp_df <- shp_df %>% left_join(st_drop_geometry(score_df), by = join_by, suffix = c("", paste0(".", df_name)))
    
  }
  
  if (verbose) message("All score dataframes joined successfully")
  
  print(head(shp_df))
  return(shp_df)
}
#
#
###Calculate total HSM score
calculate_totals <- function(data_scores){
  #Oyster score
  oyster_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Oyst") & ends_with("SC")) %>%
    mutate(across(contains("2024"), ~ .*1),
           across(contains("2023"), ~ .*0.8),
           across(contains("2022"), ~ .*0.6),
           across(contains("2021"), ~ .*0.4),
           across(contains("2020"), ~ .*0.2)) %>%
    mutate(OystTO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>% 
    mutate(OystCO = ncol(dplyr::select(., -c(PGID, OystTO)))) %>% 
    dplyr::select(PGID, OystTO, OystCO) %>%
    mutate(OystAV = OystTO/OystCO) %>%
    mutate(row_id = row_number())
  #
  #Buffer score
  buffer_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Buff") & ends_with("SC")) %>%
    mutate(BuffTO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>% 
    mutate(BuffCO = ncol(dplyr::select(., -c(PGID, BuffTO)))) %>% 
    dplyr::select(PGID, BuffTO, BuffCO) %>%
    mutate(BuffAV = BuffTO/BuffCO) %>%
    mutate(row_id = row_number())
  #
  #Seagrass score
  seagrass_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Sgrs") & ends_with("SC")) %>%
    mutate(SgrsTO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(SgrsCO = ncol(dplyr::select(., -c(PGID, SgrsTO)))) %>% 
    dplyr::select(PGID, SgrsTO, SgrsCO) %>%
    mutate(SgrsAV = SgrsTO/SgrsCO) %>%
    mutate(row_id = row_number())
  #
  #Channel score
  channel_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, contains("Chnl") & ends_with("SC")) %>%
    #If type noted to any of the channel columns (meaning score was 0), score = 0
    mutate(ChnlTO = ifelse(if_any(everything(dplyr::select(., -PGID)), ~. == 0), 0, 1)) %>%
    #mutate(ChannelCO = ncol(dplyr::select(., -c(PGID, ChnlTO)))) %>% 
    dplyr::select(PGID, ChnlTO) %>% #, ChnlCO) %>%
    mutate(row_id = row_number())
  #
  #Salinity score
  salinity_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, matches("S.*e.\\S(C|CL)$")) %>%
    mutate(STO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(SCO = ncol(dplyr::select(., -c(PGID, STO)))) %>% 
    dplyr::select(PGID, STO, SCO) %>%
    mutate(SAV = STO/SCO) %>%
    mutate(row_id = row_number())
  #
  #Temperature score
  temperature_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, matches("T.*e.\\S(C|CL)$")) %>%
    mutate(TTO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(TCO = ncol(dplyr::select(., -c(PGID, TTO)))) %>% 
    dplyr::select(PGID, TTO, TCO) %>%
    mutate(TAV = TTO/TCO) %>%
    mutate(row_id = row_number())
  #
  #Flow score
  flow_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, matches("^F.*S(C|CL)$")) %>%
    mutate(FTO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(FCO = ncol(dplyr::select(., -c(PGID, FTO)))) %>% 
    dplyr::select(PGID, FTO, FCO) %>%
    mutate(FAV = FTO/FCO) %>%
    mutate(row_id = row_number())
  #
  #Combine all totals to data frame
  all_totals <- (data_scores %>% mutate(row_id = row_number())) %>%
    left_join(oyster_total, by = c("row_id", "PGID")) %>%
    left_join(buffer_total, by = c("row_id", "PGID")) %>%
    left_join(seagrass_total, by = c("row_id", "PGID")) %>%
    left_join(channel_total, by = c("row_id", "PGID")) %>%
    left_join(salinity_total, by = c("row_id", "PGID")) %>%
    left_join(temperature_total, by = c("row_id", "PGID")) %>%
    left_join(flow_total, by = c("row_id", "PGID")) %>%
    dplyr::select(-row_id)
  #
  print(head(all_totals))
  return(all_totals)
  #
}
#
#
##Clean model data frame
clean_model_data <- function(data){
  data_clean <- data %>% dplyr::select(-dplyr::ends_with("CO"), -dplyr::ends_with("TO"), dplyr::any_of("ChnlTO"))
  #
  print(head(data_clean))
  return(data_clean)
}
#
#

# Function to save shapefile and split if necessary
save_model_output <- function(data = NULL, #good for single output, but not needed
                              SiteCode = Site_Code, 
                              VerNum = Version,
                              output_type = c("all", "data", "scores", "model")) {
  #
  output_type <- match.arg(output_type)
  #
  # Retrieve objects from global environment if data is missing ----
  if(is.null(data)) {
    if(exists("HSM_spdf", envir = .GlobalEnv)) {
      data <- get("temp", envir = .GlobalEnv)
      scores <- get("HSM_data_grps", envir = .GlobalEnv)
      model <- get("HSM_spdf", envir = .GlobalEnv)
      message("Using 'temp', 'HSM_data_grps', and 'HSM_spdf' from global environment.")
    } else {
      stop("No 'data' provided and 'temp' and 'HSM_spdf' not found in global environment.")
    }
  }
  # Prepare objects ----
  HSM_shp_output <- model %>% dplyr::select(-ends_with("SC"), -ends_with("SCL"),-contains("Shape"), -ends_with("CO"), -ends_with("TO"))
  HSM_model_csv <- HSM_shp_output %>% st_drop_geometry()
  Model_scores_csv <- scores %>% dplyr::select(-contains("HSM"))
  Model_data_csv <- data %>% st_drop_geometry()
  #
  # Paths ----
  CSV_data_path <- paste0(SiteCode, "_", VerNum, "/Output/Data files/", 
                          SiteCode, "_", VerNum, "_model_data.csv")
  CSV_scores_path <- paste0(SiteCode, "_", VerNum, "/Output/Data files/", 
                            SiteCode, "_", VerNum, "_model_scores.csv")
  shapefile_temp <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/temp.shp")
  shapefile_path <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/", 
                           SiteCode, "_", VerNum, "_HSM_model.shp")
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
                             SiteCode, "_", VerNum, "_model_shp.csv")
    data.table::fwrite(HSM_model_csv, model_csv_path)
    message(paste0("CSV of model outcome saved to: ", model_csv_path))
  }
}
# Example usage: Assuming 'my_sf_data' is your sf object
# save_model_output(my_sf_data) 
#
#
# Model/layer plotting:
plot_model_map <- function(sf_data,
                           column,
                           palette = "viridis",
                           na_color = "grey80",
                           title = NULL,
                           force_scale = c("auto", "continuous", "discrete")) {
  #
  stopifnot(inherits(sf_data, "sf"))
  #
  force_scale <- match.arg(force_scale)
  #
  col_sym <- rlang::ensym(column)
  col_name <- rlang::as_string(col_sym)
  #
  if (is.null(title)) {
    title <- col_name
  }
  ## ---- base theme ----
  base_theme <- ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      axis.text = ggplot2::element_text(size = 16),
      plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
      plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5)),
      plot.caption = ggplot2::element_text(face = "italic", size = 9)
    )
  ## ---- plot ----
  base_plot <- ggplot2::ggplot() + base_theme
  p <- base_plot +
    geom_sf(data = sf_data, aes(fill = !!col_sym), color = NA) +
    #theme_minimal() +
    labs(fill = title, title = title)
  #
  ## ---- Scaling ----
  if (force_scale == "continuous" ||
      (force_scale == "auto" && is.numeric(values))) {
    
    p <- p + scale_fill_viridis_c(
      na.value = na_color,
      option = palette
    )
    
  } else {
    
    p <- p + scale_fill_viridis_d(
      na.value = na_color,
      option = palette
    )
  }
  
  return(p)
  #
}