##Functions used in habitat suitability mapping project 2_HSM_creation code file
##
#
####Grid loading and cropping####
#
##load setup information
load_working_info <- function(SiteCode, VersionID){
  #Combine Site and version information
  Site_version <- paste0(SiteCode, "_", VersionID)
  #Load set up information
  filename <- paste0(Site_version, "/Data/", Site_version, "_model_setup.xlsx") 
  sheets <- excel_sheets(filename)
  df_list <<- lapply(sheets, function(sheet){
    read_excel(filename, sheet = sheet)
  })
  names(df_list) <- sheets
  return(df_list)
}
#
#
#### Function to load grid(s) and site area, clips to overlap, saves final grid, and keeps final grid for use
get_base_grid <- function(SiteCode, VersionID, SectionsDesignated, Save_data, Save_figure){
  #
  StateGrid <- State_Grid
  AltGrid <- Alt_Grid
  #Load StateGrid(s) of picogrid
  PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",StateGrid,"/Florida_PicoGrid_WGS84_",StateGrid,"_clip.shp"), quiet = TRUE)
  if(!is.na(AltGrid)){Alt_PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",AltGrid,"/Florida_PicoGrid_WGS84_",AltGrid,"_clip.shp"), quiet = TRUE)}
  #
  #Check grid(s), to confirm area
  head(PicoGrid)
  if(!is.na(AltGrid)){head(AltGrid)} else {print("No additional grid is being used.")}
  #
  ##Load site KML and section KMLs as needed
  OrderSections <- df_list[[2]]
  Site_area <- st_read(paste0(SiteCode, "_", VersionID,"/Data/Layers/KML/", SiteCode, ".kml"), quiet = TRUE)
  #plot(Site_area[1]) #Output site area plot
  #
  SectionList <- unlist((OrderSections %>% arrange(Order))[,"KML_Name"]) #Output list of names
  for (i in seq_along(unique(SectionList))) {
    temp <- st_read(paste0(SiteCode, "_", VersionID, "/Data/Layers/KML/", unique(SectionList)[i], ".kml"), quiet = TRUE)
    assign(paste0("Section",i), temp)
  }
  #
  ##Limit to site area
  if(!is.na(AltGrid)){
    Site_Grid <<- rbind(PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,], 
                        Alt_PicoGrid[lengths(st_intersects(Alt_PicoGrid, Site_area))> 0,])
    rm(PicoGrid, Alt_PicoGrid)
  } else {
    Site_Grid <<- PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,] 
    rm(PicoGrid)
  }
  #
  ##Assign Site and Section information if designated
  if(SectionsDesignated == "N"){
    Section_grid <- suppressMessages(Site_Grid %>% 
                                       #Add Site code and Sections as NA
                                       mutate(Site = SiteCode, Section = NA) %>% 
                                       #Add site long name
                                       left_join(df_list[[1]] %>% filter(Type == "Site") %>% dplyr::select(Designation, LongName) %>% rename(Site = Designation))) 
    ##Output head of updated data frame and map of sections
    head(Section_grid)
    Section_plot <<- tm_shape(Section_grid) + tm_fill(col = "Section")
    #
  } else if(SectionsDesignated == "Y"){
    #Create empty list
    temp_sections <- list()
    for (i in mget(ls(pattern = "Section[0-9]"))) {
      #Assign grid cells to section based on overlap with section layer
      temp <- suppressMessages(Site_Grid[lengths(st_intersects(Site_Grid, i)) > 0,] %>% #Limit to section area
                                 mutate(Site = SiteCode, #Add Site code, section code
                                        Section = OrderSections$Section[OrderSections$KML_Name == i$Name]) %>%
                                 #Add site long name
                                 left_join(df_list[[1]] %>% filter(Type == "Site") %>% mutate(Site = Designation) %>% dplyr::select(Site, LongName)) %>%
                                 #Add section long names from KML file names
                                 left_join(df_list[[2]] %>% mutate(Section_Name = str_extract(KML_Name, "(?<=-).*")) %>% dplyr::select(Section, Section_Name)))
      #
      temp_sections[[length(temp_sections) + 1]] <- temp
      #Add all data together into one output
      Section_grid <- do.call(rbind, temp_sections) %>% 
        #Re-level Section based on specified order
        mutate(Section = factor(Section, levels = unique(df_list[[2]]$Section[order(df_list[[2]]$Order)]), ordered = TRUE)) %>% 
        #Keep only one Section-assignment per grid cell
        arrange(Section) %>% group_by(PGID) %>% slice(1) 
    }
    rm(temp, i, temp_sections)
    gc()  
    #
    ##Output head of updated data frame and map of sections
    head(Section_grid)
    Section_plot <<- tm_shape(Section_grid) + tm_fill(col = "Section")
  } else {paste("Incorrect specification of if sections should be designated in data.")}
  #
  Section_grid <<- Section_grid
  #
  ##Save data file and shape file if requested:
  if(Save_data == "Y"){
    Datafile_name <- paste0(SiteCode, "_", VersionID, "/Output/Data files/", SiteCode, "_", VersionID, "_GridData_Sections.xlsx")
    Shapefile_name <- paste0(SiteCode, "_", VersionID, "/Output/Shapefiles/", SiteCode, "_", VersionID,"_Sections.shp")
    ##Save data frame as Excel file - need to drop geometry 
    Section_output_data <- as.data.frame(Section_grid) %>% dplyr::select(-geometry)
    if(file.exists(Datafile_name)){
      new_Datafilename <- sub("\\.xlsx$", paste0("_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"), Datafile_name)
    } else{
      new_Datafilename <- Datafile_name
    }
    suppressMessages(write_xlsx(Section_output_data, new_Datafilename, format_headers = TRUE))
    #Save shapefile of designations
    if(file.exists(Shapefile_name)){
      new_Shapefilename <- sub("\\.shp$", paste0("_", format(Sys.Date(), "%Y-%m-%d"), ".shp"), Shapefile_name)
    } else {
      new_Shapefilename <- Shapefile_name
    }
    suppressMessages(st_write(Section_grid, new_Shapefilename, overwrite = TRUE, quiet = TRUE))
    #
    print("An Excel data file was saved to [Output/Data files].")
    print("A shapefile was saved to [Output/Shapefiles].")
  }
  if(Save_figure == "Y"){
    jpg_filename <- paste0(SiteCode, "_", VersionID, "/Output/Figure files/",SiteCode, "_", VersionID, "_Sections.jpg")
    width_pixels <- 1000
    aspect_ratio <- 3/4
    height_pixels <- round(width_pixels * aspect_ratio)
    if (file.exists(jpg_filename)) {
      new_filename <- sub("\\.jpg$", paste0("_", format(Sys.Date(), "%Y-%m-%d"), ".jpg"), jpg_filename)
    } else {
      new_filename <- jpg_filename
    }
    #
    p <-suppressMessages(ggplot(Section_grid) + geom_sf(aes(color = Section_Name, fill = Section_Name)) + 
                           theme_minimal() + theme(axis.text = element_text(size = 14, color = "black")) +  
                           scale_fill_viridis_c() + scale_fill_viridis_d() + scale_color_viridis_c() + scale_color_viridis_d())
    #Save predictions to Excel with the sheet named "Salinity_adults"
    ggsave(filename = jpg_filename, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
    print("A figure colored by sections was saved to [Output/Figure files].")
  }
  #
  return(list(paste0("Sections included in site:"), SectionList, Section_plot))
}
#
#
#
#### Assigning polygon data #####
#
#find Layer_name using Parameter
find_folder_names <- function(Parameter_name){
  #Directory
  data_dir <- "Data layers/"
  #Summary information
  Ref_info <- data.frame(df_list[3])
  #ID parameter in summary table
  if(Parameter_name == "Oysters"){Param_find <- "Oysters"}
  #
  #Check if Param to find is included in summary
  if (!Param_find %in% unique(Ref_info$Parameter)) {
    stop("The 'Parameter_Order' dataframe does not contain an associated 'Layer_name'")
  }
  #
  match_pattern <- (Ref_info%>% filter(Parameter == Param_find))$Layer_name
  #Get list of folder names
  all_folders <- list.dirs(path = data_dir, full.names = FALSE, recursive = FALSE)
  #Narrow to list containing Layer name
  matched_folders <<- all_folders[grepl(match_pattern, x = all_folders, ignore.case = TRUE)]
  #Check for matches
  if(length(matched_folders) > 0){
    message(paste0("Matching folders found for ", Parameter_name, " as '", Param_find, "': \n", 
                   paste(unlist(matched_folders), collapse = "\n")))
  } else {
    warning(paste0("No matching folders were found for ", Parameter_name, " as '", Param_find, "'"))
  }
  #
}
#
#Load data from all matching folders
load_matching_shp <- function(Parameter_name, StartDate = Start_date, EndDate = End_date){
  data_dir <- "Data layers/"
  #ID parameter in summary table
  if(Parameter_name == "Oysters"){Param_file <- "/Oyster_Beds_in_Florida.shp"}
  #
  StartDate <- as.Date(Start_date)
  EndDate <- as.Date(End_date)
  loaded_files <- list()
  #
  ##For each matching folder:
  for(folder in matched_folders){
    #Get date of folder, skip if outside range:
    folder_date <- as.Date(paste0(substr(folder, nchar(folder) - 5, nchar(folder)), "01"), format = "%Y%m%d")
    if(folder_date > Start_date & folder_date < End_date){
      print(paste0("Loading: ", folder))
      #Load shapefile, assign name
      shape_obj <- crop(as(st_read(paste0(data_dir, folder, Param_file)), "Spatial"),
                        extent(Site_Grid))
      obj_name <- paste0("Oyster_", substr(folder, nchar(folder) - 5, nchar(folder)))
      assign(obj_name, shape_obj, envir = .GlobalEnv)
      #Add to loaded list
      loaded_files[[folder]] <- paste0(str_extract(folder, "[^_]+"), "_", str_extract(folder, "[^_]+$"))
    } else {
      print(paste0("Skipping: ", folder))
    }
  }
  files_loaded <<- loaded_files
}

#### Scoring ####
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
  # List all matching shapefiles
  shp_files <- list.files(path = data_dir, pattern = pattern, full.names = TRUE)
  if (length(shp_files) == 0) {
    stop("No shapefiles found matching the pattern.")
  } else {
    #Print list of files loaded:
    message("Files loaded:\n", paste(shp_files, collapse = "\n"))

    if (length(shp_files) == 1) {
      shape_obj <- st_read(shp_files[1])
      } else {
        # Read and combine all shapefiles
        shape_list <- lapply(shp_files, st_read)
        shape_obj <- do.call(rbind, shape_list)
      }
  }
  #assign shp to object
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
  names(data) <- new_names
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
                      paste0(names(data), "SC", helper))
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
                      paste0(names(data), "SC", helper))
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
                      paste0(names(data), "SC", helper))
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
                      paste0(names(data), "SC", helper))
  names(data) <- new_names
  print(head(data))
  return(data)
}
#
#
#Threshold period - number = proportion above.below the threshold - score is inverse of values
assign_threshold_scores <- function(shapefile_data, type = "separate") {
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
                      paste0(names(data), "SC"))
  names(data) <- new_names
  print(head(data))
  return(data)
}
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
    dplyr::select(PGID, starts_with("S") & ends_with("ESC")) %>%
    mutate(STO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(SCO = ncol(dplyr::select(., -c(PGID, STO)))) %>% 
    dplyr::select(PGID, STO, SCO) %>%
    mutate(SAV = STO/SCO) %>%
    mutate(row_id = row_number())
  #
  #Temperature score
  temperature_total <- data_scores %>% st_drop_geometry() %>%
    dplyr::select(PGID, starts_with("T") & ends_with("ESC")) %>%
    mutate(TTO = as.numeric(rowSums(dplyr::select(., -PGID), na.rm = TRUE))) %>%
    mutate(TCO = ncol(dplyr::select(., -c(PGID, TTO)))) %>% 
    dplyr::select(PGID, TTO, TCO) %>%
    mutate(TAV = TTO/TCO) %>%
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
save_model_output <- function(data, SiteCode = Site_Code, VerNum = Version) {
  #
  #Cleaned shp and CSV data
  HSM_shp_output <- data %>% dplyr::select(-ends_with("SC"), -ends_with("SCL"),-contains("Shape"), -ends_with("CO"), -ends_with("TO"))
  HSM_csv <- data %>% st_set_geometry(NULL) %>% as.data.frame()
  #
  ##Output CSV data file
  CSV_path <- paste0(SiteCode, "_", VerNum, "/Output/Data files/", SiteCode, "_", VerNum, "_model_data.csv")
  data.table::fwrite(HSM_csv, CSV_path)
  message(paste0("CSV data file saved to ", CSV_path))
  #
  #Temporary file path for the shapefile
  temp_file_path <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/temp.shp")
  temp_dbf <- sub("\\.shp$", ".dbf", temp_file_path)
  file_path <- paste0(SiteCode, "_", VerNum, "/Output/Shapefiles/", SiteCode, "_", VerNum, "_HSM_model.shp")
  #Write the shape file to a temporary location
  st_write(HSM_shp_output, temp_file_path, delete_dsn = TRUE)
  
  # Check the file size
  file_size <- file.info(temp_dbf)$size
  # If the file size is greater than 2 GB (2 * 1024^3 bytes) - modified to be conservative
  if (file_size > (2 * 1000^3)) {
    # Split the data into chunks
    chunk_size <- 2 * 1000^3  # 2 GB
    num_chunks <- ceiling(file_size / chunk_size)
    
    # Calculate the number of rows per chunk
    rows_per_chunk <- ceiling(nrow(HSM_shp_output) / num_chunks)
    
    #New file path
    new_path <- sub("\\.shp", "", file_path)
    for (i in seq_len(num_chunks)) {
      # Determine the row indices for the current chunk
      start_row <- (i - 1) * rows_per_chunk + 1
      end_row <- min(i * rows_per_chunk, nrow(HSM_shp_output))
      
      # Create a subset of the data for the current chunk
      chunk_data <- HSM_shp_output[start_row:end_row, ]
      
      # Create a new file path for the chunk
      chunk_file_path <- paste0(new_path, "_section", i, ".shp")
      
      # Write the chunk to a new shapefile
      st_write(chunk_data, chunk_file_path, delete_dsn = TRUE)
    }
    
    message("Data split into ", num_chunks, " files.")
  } else {
    # If the file size is under 2 GB, save normally
    st_write(HSM_shp_output, file_path, delete_dsn = TRUE)
    message("Shapefile saved successfully.")
  }
}
# Example usage: Assuming 'my_sf_data' is your sf object
# save_model_output(my_sf_data)  