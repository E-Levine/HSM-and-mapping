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
# Updated 7/1/26 to allow for more AltGrids and better loading/error messaging
get_base_grid <- function(SiteCode, VersionID, SectionsDesignated, Save_data, Save_figure){
  #
  StateGrid <- State_Grid
  AltGrid <- Alt_Grid
  # Combine and identify all StateGrids needed
  grid_names <- c(StateGrid, AltGrid)
  grid_names <- unique(grid_names[!is.na(grid_names)])
  #
  if (length(grid_names) > 1) {#print("Primary grid:") print(grid_names[1])
    print("Additional grids:")
    print(grid_names[-1])
  } else {
    print("No additional grids are being used.")
  }
  # Get full list of StateGrids
  grid_list <- vector("list", length(grid_names))
  # Load StateGrid(s)
  for (i in seq_along(grid_names)) {
    #
    g <- grid_names[i]
    #
    shp <- paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",g,
                  "/Florida_PicoGrid_WGS84_",g,
                  "_clip.shp")
    #
    message(sprintf("Loading grid %d of %d: %s", i, length(grid_names), g))
    #
    grid_list[[i]] <- tryCatch(
      st_read(shp, quiet = TRUE),
      error = function(e) {
        stop(sprintf(
          "Failed to load grid %d (%s)\nPath: %s\n\n%s", i, g, shp, e$message),
          call. = FALSE)}
    )}
  #
  PicoGrid <- do.call(rbind, grid_list)
  #
  ## Load site KML and section KMLs as needed
  OrderSections <- df_list[[2]]
  Site_area <- st_read(paste0(SiteCode, "_", VersionID,"/Data/Layers/KML/", SiteCode, ".kml"), quiet = TRUE)
  #plot(Site_area[1]) #Output site area plot
  #
  SectionList <- unlist((OrderSections %>% arrange(Order))[,"KML_Name"]) #Output list of names
  #for (i in seq_along(unique(SectionList))) {
  #  temp <- st_read(paste0(SiteCode, "_", VersionID, "/Data/Layers/KML/", unique(SectionList)[i], ".kml"), quiet = TRUE)
  #  assign(paste0("Section",i), temp)
  #}
  unique_sections <- unique(SectionList)
  section_list <- lapply(unique_sections, function(x) {
    s <- st_read(
      paste0(SiteCode, "_", VersionID,"/Data/Layers/KML/",x,".kml"),
      quiet = TRUE)
    s$KML_Name <- x
    s
  })
  sections_sf <- bind_rows(section_list)
  #
  #
  ## Limit to site area & add Site column
  Site_Grid <- st_filter(PicoGrid, Site_area) %>% #Site_Grid <- PicoGrid[lengths(st_intersects(PicoGrid, Site_area)) > 0, ]
    mutate(Site = SiteCode) #Add Site code, section code
  #
  #
  ## Assign Site and Section information if designated
  site_lookup <- df_list[[1]] %>%
    filter(Type == "Site") %>%
    transmute(Site = Designation, LongName)
  #
  section_lookup <- df_list[[2]] %>%
    mutate(Section_Name = str_extract(KML_Name, "(?<=-).*")) %>%
    dplyr::select(Section, KML_Name, Section_Name, Order)
  #
  
  if(SectionsDesignated == "N"){
    Section_grid <- suppressMessages(Site_Grid %>% 
                                       #Add Site code and Sections as NA
                                       mutate(Site = SiteCode, Section = NA) %>% 
                                       #Add site long name
                                       left_join(df_list[[1]] %>% 
                                                   filter(Type == "Site") %>% 
                                                   dplyr::select(Designation, LongName) %>% 
                                                   rename(Site = Designation))) 
    ##Output head of updated data frame and map of sections
    head(Section_grid)
    Section_plot <<- tm_shape(Section_grid) + tm_fill(col = "Section")
    #
  } else if(SectionsDesignated == "Y"){
    ## Spatial join (one operation instead of looping over sections)
    Section_grid <- st_join(
      Site_Grid,
      sections_sf %>% dplyr::select(KML_Name),
      join = st_intersects, left = FALSE) %>%
      left_join(OrderSections %>% dplyr::select(KML_Name, Section), by = "KML_Name") %>%
      left_join(site_lookup, by = "Site") %>%
      left_join(section_lookup %>% dplyr::select(Section, Section_Name, Order), by = "Section") %>%
      mutate(Site = SiteCode,
             Section = factor(Section, levels = OrderSections %>% arrange(Order) %>% pull(Section), ordered = TRUE)
             ) %>%
      arrange(Section) %>%
      group_by(PGID) %>%
      slice(1) %>%
      ungroup()
    
    if (nrow(Section_grid) == 0) {
      message("No overlapping sections were found.")
      Section_grid <- NULL
    }
    #
    ##Output head of updated data frame and map of sections
    head(Section_grid)
    Section_plot <<- tm_shape(Section_grid) + tm_fill(col = "Section")
  } else {
    paste("Incorrect specification of if sections should be designated in data.")
    }
  #
  Section_grid <<- Section_grid
  #
  #
  ## Save data file and shape file if requested:
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
  if(Parameter_name == "Seagrass"){Param_find <- "Seagrass"}
  if(Parameter_name == "Channels"){Param_find <- "Channels"}
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
# Site_Grid: gridded object to apply data to
# Parameter_name: name of parameter being added (found in Parameter column of the Parameter Order set up sheet)
load_matching_shp <- function(Site_Grid, Parameter_name, StartDate, EndDate){
  data_dir <- "Data layers/"
  #ID parameter in summary table
  if(Parameter_name == "Oysters"){Param_file <- "/Oyster_Beds_in_Florida.shp"}
  if(Parameter_name == "Seagrass"){Param_file <- "/Seagrass_Habitat_in_Florida.shp"}
  if(Parameter_name == "Channels"){Param_file <- "/Waterways_Florida.shp"}
  #
  loaded_files <- list()
  #
  ##For each matching folder:
  for(folder in matched_folders){
    #Get date of folder, skip if outside range:
    folder_date <- as.Date(paste0(substr(folder, nchar(folder) - 5, nchar(folder)), "01"), format = "%Y%m%d")
    if(is.na(folder_date)){
      print("No date associated with folder.")
      #Load shapefile, assign name
      shp_t <- st_read(paste0(data_dir, folder, Param_file))
      shp_t <- st_transform(shp_t, st_crs(Site_Grid))
      #shp_t <- sf::st_make_valid(shp_t)
      shape_obj <- crop(as(shp_t, "Spatial"), extent(Site_Grid))
      obj_name <- paste0(sub("_.*", "", folder))
      assign(obj_name, shape_obj, envir = .GlobalEnv)
      #Add to loaded list
      loaded_files[[folder]] <- paste0(str_extract(folder, "[^_]+"), "_", str_extract(folder, "[^_]+$"))
      #
      #
    } else if(folder_date > StartDate & folder_date < EndDate){
      print(paste0("Loading: ", folder))
      #Load shapefile, assign name
      shp_t <- st_read(paste0(data_dir, folder, Param_file))
      shp_t <- st_transform(shp_t, st_crs(Site_Grid))
      #shp_t <- sf::st_make_valid(shp_t)
      shape_obj <- crop(as(shp_t, "Spatial"), extent(Site_Grid))
      obj_name <- paste0(sub("_.*", "", folder), "_", substr(folder, nchar(folder) - 5, nchar(folder)))
      assign(obj_name, shape_obj, envir = .GlobalEnv)
      #Add to loaded list
      loaded_files[[folder]] <- paste0(str_extract(folder, "[^_]+"), "_", str_extract(folder, "[^_]+$"))
      #
      #
    } else {
      print(paste0("Skipping: ", folder))
    }
  }
  files_loaded <<- loaded_files
}
#
#
# Function to apply data to grid cells based on overlapping polygons with most overlap. Assigns only one data value per cell based on largest overlap.
#modelGrid = grid object to apply data to
#polygonData = polygon data object with data to be applied
#dataColumn = name of data column to be added
#fillValue = value to fill if polygons have data of NA (i.e., OYSTERS = NA, fill with "Live")
#df_list = df_list returned from load_working_info function
#poly_name
#
# Function to process all polygon data of specified type:
apply_polygon_overlap <- function(modelGrid,
                                  files_loaded,
                                  dataColumn,
                                  Parameter_name,
                                  fillValue,
                                  df_list) {
  
  library(progress)
  suppressWarnings(sf_use_s2(FALSE))
  #
  # Progress bar setup
  total_steps <- length(files_loaded)*4 + 2
  #
  pb <- progress_bar$new(
    format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
    total = total_steps,
    complete = "=", incomplete = "-", current = ">",
    clear = FALSE, width = 100, show_after = 0, force = TRUE)
  pb_active <- TRUE
  #
  # ---- model grid built once ----
  modelGrid_sf <- st_as_sf(modelGrid)
  modelGrid_sf <- st_make_valid(modelGrid_sf)
  modelGrid_sf$grid_id <- seq_len(nrow(modelGrid_sf))
  grid_crs <- st_crs(modelGrid_sf)
  pb$tick(tokens = list(step = "Model grid build"))
  Sys.sleep(1/1000)
  # ---- column naming helper ----
  modelColName <- (df_list[[3]] %>%
                     dplyr::filter(Parameter == Parameter_name & Column_name == dataColumn))$Model_column_name

  # LOOP PER FILE → EACH FILE CREATES ONE OUTPUT COLUMN
  for (f in files_loaded) {
    #
    pb$tick(tokens = list(
    step = paste0("Processing ", f)
    ))
    Sys.sleep(1/1000)
    # Build polygons
    polygon_sf <- st_as_sf(get(f))
    polygon_sf <- st_make_valid(polygon_sf)
    polygon_sf <- st_transform(polygon_sf, st_crs(modelGrid_sf))
    #
    pb$tick(tokens = list(
      step = paste0("Processing: ", f, " : Overlap indexing")
    ))
    Sys.sleep(1/1000)
    # Spatial index for overlap
    idx <- suppressMessages(
      suppressWarnings(
        st_intersects(modelGrid_sf, polygon_sf, sparse = TRUE)
      ))
    nonempty <- which(lengths(idx) > 0)
    #
    pb$tick(tokens = list(
      step = paste0("Processing: ", f, " : Overlap intersecting")
    ))
    Sys.sleep(1/1000)
    # Intersection loop
    results <- vector("list", length(nonempty))
    
    ## Chunking method
    # Size of each chunk
    chunk_size <- 1000
    # Split the nonempty grid IDs into chunks
    chunks <- split(
      nonempty,
      ceiling(seq_along(nonempty) / chunk_size)
    )
    for (j in seq_along(chunks)) {
      
      grid_ids <- chunks[[j]]
      
      # Candidate polygons for every grid cell in this chunk
      poly_ids <- unique(unlist(idx[grid_ids]))
      
      inter <- suppressMessages(
        suppressWarnings(
          st_intersection(
            modelGrid_sf[grid_ids, , drop = FALSE],
            polygon_sf[poly_ids, , drop = FALSE]
          )
        )
      )
      
      results[[j]] <- inter
    }
    
    # Combine all intersections and compute areas
    inters <- dplyr::bind_rows(results)
    if (nrow(inters) == 0) {
      next
    }
    inters$overlap_area <- st_area(inters)
    #
    pb$tick(tokens = list(
      step = paste0("Processing: ", f, " : Overlap selection & cleaning")
    ))
    Sys.sleep(1/1000)
    # Best overlap
    best <- inters %>%
      dplyr::group_by(grid_id) %>%
      dplyr::slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    #
    # Replace NA
    best[[dataColumn]][is.na(best[[dataColumn]])] <- fillValue
    #
    # Keep only needed columns & rename data column
    best_drop <- sf::st_drop_geometry(best)
    suffix <- stringr::str_sub(stringr::str_extract(f, "(?<=_).*"), 3, 4)
    newColName <- paste0(modelColName, suffix)
    # 
    modelGrid_sf <- modelGrid_sf %>%
      dplyr::left_join(
        best_drop %>%
          dplyr::select(PGID, Lat_DD_Y, Long_DD_X, all_of(dataColumn)),
        by = c("PGID", "Long_DD_X", "Lat_DD_Y")
      )
    
    if (is.na(st_crs(modelGrid_sf))) {
      st_crs(modelGrid_sf) <- grid_crs
    }
    
    modelGrid_sf <- dplyr::rename(modelGrid_sf, !!newColName := all_of(dataColumn))
    
    message(
      sprintf(
        "Added '%s' from polygon layer '%s' as model grid column '%s'.",
        dataColumn,
        f,
        newColName
      )
    )
  }
  #
  suppressWarnings(sf_use_s2(TRUE))
  pb$tick(tokens = list(step = "Completed processing"))
  Sys.sleep(1/1000)
  if (!pb$finished) {
    pb$tick(0, tokens = list(step = "Completed processing"))
  }
  pb$terminate()
  pb_active <- FALSE
  as(modelGrid_sf, "Spatial")
}
#
#
#
apply_distance_buffers_ori <- function(modelGrid,
                                  files_loaded,
                                  LayerName, #Name of data layer output/type
                                  dataColumn, #Name of column of data to use
                                  buffer_breaks = c(200, 400),
                                  df_list = NULL) {
  
  library(sf)
  library(dplyr)
  library(progress)
  
  suppressWarnings(sf_use_s2(FALSE))
  
  # Progress bar setup ----
  total_steps <- length(files_loaded) * (2 + length(buffer_breaks) - 1) + 2
  #
  pb <- progress_bar$new(
    format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
    total = total_steps,
    complete = "=", incomplete = "-", current = ">",
    clear = FALSE, width = 100, show_after = 0, force = TRUE)
  pb_active <- TRUE
  #
  # Model grid ----
  pb$tick(tokens = list(step = "Building model grid"))
  Sys.sleep(1/1000)
  modelGrid_sf <- st_as_sf(modelGrid)
  original_crs <- st_crs(modelGrid_sf)
  # Convert for buffer distances
  if (st_is_longlat(modelGrid_sf)) {
    # use a projected CRS for buffering/distances
    work_crs <- 3857
    modelGrid_sf <- st_transform(modelGrid_sf, work_crs)
  }
  modelGrid_sf <- st_make_valid(modelGrid_sf)
  
  # ensure projected CRS in meters
  if (st_is_longlat(modelGrid_sf)) {
    stop("modelGrid must be in a projected CRS with meter units.")
  }
  
  # Column naming helper ----
  modelColName <- (df_list[[3]] %>%
                     dplyr::filter(Column_name == dataColumn & Parameter == LayerName))$Model_column_name
  
  # Buffer processing ----
  for (f in files_loaded) {
    #
    pb$tick(tokens = list(step = paste("Processing", f)))
    Sys.sleep(1/1000)
    # Build polygons
    feature_sf <- st_as_sf(get(f))
    feature_sf <- st_make_valid(feature_sf)
    feature_sf <- st_transform(feature_sf, st_crs(modelGrid_sf))
    #
    pb$tick(tokens = list(
      step = paste0("Processing: ", f, " : Buffering")
    ))
    Sys.sleep(1/1000)
    nearest_geom <- st_nearest_feature(modelGrid_sf, feature_sf)
    
    nearest_dist <- as.numeric(
      st_distance(
        modelGrid_sf,
        feature_sf[nearest_geom, ],
        by_element = TRUE
      )
    )
    
    if (is.null(buffer_breaks)) {
      buffer_breaks <- c(200, 400)
    }
    buffer_breaks <- as.numeric(buffer_breaks)
    if (any(is.na(buffer_breaks))) {
      stop("buffer_breaks must be numeric")
    }
    buffer_breaks <- sort(unique(buffer_breaks)) 
    buffer_value <- rep(NA_real_, length(nearest_dist))
    buffer_value[nearest_dist <= buffer_breaks[1]] <- buffer_breaks[1] 
    
    for (i in seq_along(buffer_breaks)[-1]) { 
      pb$tick(tokens = list(
        step = paste0("Buffering: ", f, " // " , buffer_breaks[i])
      ))
      Sys.sleep(1/1000)
      
      buffer_value[
        nearest_dist > buffer_breaks[i - 1] &
          nearest_dist <= buffer_breaks[i]
      ] <- buffer_breaks[i]
    }
    
    suffix <- stringr::str_sub(stringr::str_extract(f, "(?<=_).*"), 3, 4)
    
    newColName <- paste0(modelColName, suffix)
    
    modelGrid_sf[[newColName]] <- buffer_value
  }
  
  suppressWarnings(sf_use_s2(TRUE))
  
  # Convert output back to original CRS
  if (st_crs(modelGrid_sf) != original_crs) {
    modelGrid_sf <- st_transform(modelGrid_sf, original_crs)
  }
  
  message(
    sprintf(
      "Added '%s' from feature layer '%s' as model grid column '%s'.",
      dataColumn,
      f,
      newColName
    )
  )
  
  suppressWarnings(sf_use_s2(TRUE))
  pb$tick(tokens = list(step = "Completed processing"))
  Sys.sleep(1/1000)
  if (!pb$finished) {
    pb$tick(0, tokens = list(step = "Completed processing"))
  }
  pb$terminate()
  pb_active <- FALSE
  
  as(modelGrid_sf, "Spatial")
}
#
#
#
#
apply_distance_buffers <- function(
    modelGrid, #grid to apply top
    files_loaded, #files to use
    LayerName, #Name of data layer output/type
    dataColumn, #Name of column of data to use
    df_list = NULL,
    buffer_method = c("fixed","lookup"), #Fixed = specified numbers, lookup = use of reference table
    buffer_breaks = NULL, #Option to specify fixed breaks (distance in meters)
    Ref_table = NULL, #Option to supply reference table for lookup 
    buffer_multiplier = 100, #Multiplier for scoring to value conversion 
    buffer_units = c("ft","m","km","mi","keep") #Assumes the reference is in meters so buffer_units = "m" is 1:1
){
  # Packages
  library(sf)
  library(dplyr)
  library(progress)
  library(stringr)
  #
  suppressWarnings(sf_use_s2(FALSE))
  #
  # Progress bar setup ----
  total_steps <- length(files_loaded) * 2 + 3
  #
  pb <- progress_bar$new(
    format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
    total = total_steps,
    complete = "=", incomplete = "-", current = ">",
    clear = FALSE, width = 100, show_after = 0, force = TRUE)
  pb_active <- TRUE
  #
  # Checks ----
  pb$tick(tokens = list(step = "Function checks"))
  Sys.sleep(1/1000)
  #
  buffer_method <- match.arg(buffer_method)
  buffer_units  <- match.arg(buffer_units)
  #
  if(buffer_method=="fixed"){
    if(is.null(buffer_breaks))
      stop("'buffer_breaks' must be supplied.")
    buffer_breaks <- sort(unique(as.numeric(buffer_breaks)))
  }
  #
  if(buffer_method=="lookup"){
    if(is.null(Ref_table))
      stop("'Ref_table' must be supplied.")
    if(!all(c("Param","Value") %in% names(Ref_table)))
      stop("Ref_table must contain Param and Value columns.")
  }
  #
  if(buffer_units == "keep"){
    conv <- 1
  } else {
    conv <- switch(buffer_units,
                   m=1,
                   ft=0.3048,
                   km=1000,
                   mi=1609.344)
  }
  #
  # Model grid ----
  pb$tick(tokens = list(step = "Building model grid"))
  Sys.sleep(1/1000)
  #
  modelGrid_sf <- st_as_sf(modelGrid)
  original_crs <- st_crs(modelGrid_sf)
  # Convert for buffer distances
  if(st_is_longlat(modelGrid_sf)){
    # use a projected CRS for buffering/distances
    modelGrid_sf <- st_transform(modelGrid_sf,3857)
  }
  modelGrid_sf <- st_make_valid(modelGrid_sf)
  #
  modelColName <- df_list[[3]] %>%
    dplyr::filter(Column_name==dataColumn,
                  Parameter==LayerName) %>%
    dplyr::pull(Model_column_name)
  #
  #
  # Buffer processing ----
  for(f in files_loaded){
    #
    pb$tick(tokens = list(step = paste("Processing", f)))
    Sys.sleep(1/1000)
    # Build features
    feature_sf <- st_as_sf(get(f))
    feature_sf <- st_make_valid(feature_sf)
    feature_sf <- st_transform(feature_sf, st_crs(modelGrid_sf))
    #
    #
    pb$tick(tokens = list(
      step = paste0("Processing: ", f, " : Buffering")
    ))
    Sys.sleep(1/1000)
    # Column naming -Check if 'f' ends in a number
    if (stringr::str_detect(f, "[0-9]$")) {
      # Extract numbers if it ends in numbers
      suffix <- stringr::str_sub(stringr::str_extract(f, "(?<=_).*"), 3, 4)
    } else {
      # Leave suffix as blank if it ends in letters (or anything else)
      suffix <- ""
    }
    #
    newColName <- paste0(modelColName,suffix)
    #
    buffer_value <- rep(NA_real_, nrow(modelGrid_sf))
    #
    if(buffer_method == "fixed"){
      #
      assigned <- rep(FALSE,nrow(modelGrid_sf))
      #
      for(b in buffer_breaks){
        buf <- st_buffer(feature_sf,b)
        inside <- lengths(
          st_intersects(modelGrid_sf,buf)
        )>0
        idx <- inside & !assigned
        buffer_value[idx] <- b
        assigned[idx] <- TRUE
      }
    } else {
      if(!(dataColumn %in% names(feature_sf)))
        stop(sprintf("%s not found.",dataColumn))
      
      ## Initialize Value column
      feature_sf$Value <- rep(0, nrow(feature_sf))
      
      for (i in seq_len(nrow(Ref_table))) {
        
        ## Split comma-separated search terms
        terms <- trimws(
          strsplit(Ref_table$Param[i], ",")[[1]]
        )
        
        ## Match any whole word (case-insensitive)
        pattern <- paste0(
          "\\b(",
          paste(terms, collapse = "|"),
          ")\\b"
        )
        
        matches <- grepl(
          pattern,
          feature_sf[[dataColumn]],
          ignore.case = TRUE,
          perl = TRUE
        )
        
        ## Maximum Value wins
        feature_sf$Value[matches] <-
          pmax(
            feature_sf$Value[matches],
            Ref_table$Value[i]
          )
      }
      
      ## Replace Inf values created by pmax(NA, ..., na.rm=TRUE)
      feature_sf$Value[feature_sf$Value == 0] <- NA
      #feature_sf <- feature_sf %>%
      #  left_join(Ref_table,
      #            by=setNames("Param",dataColumn))
      feature_sf$buffer_m <- feature_sf$Value * buffer_multiplier * conv
      buf <- st_buffer(feature_sf,
                       dist=feature_sf$buffer_m)
      hits <- st_intersects(modelGrid_sf,buf)
      for(i in seq_along(hits)){
        if(length(hits[[i]])>0){
          buffer_value[i] <- max(buf$buffer_m[hits[[i]]], na.rm=TRUE)
        }
      }
    }
    #
    modelGrid_sf[[newColName]] <- buffer_value
    #
    message(
      sprintf(
        "Added '%s' from feature layer '%s' as model grid column '%s'.",
        dataColumn,
        f,
        newColName
      )
    )
    #
  }
  # Convert output back to original CRS
  if(st_crs(modelGrid_sf)!=original_crs)
    modelGrid_sf <- st_transform(modelGrid_sf,
                                 original_crs)
  
  suppressWarnings(sf_use_s2(TRUE))
  #
  pb$tick(tokens = list(step = "Completed processing"))
  Sys.sleep(1/1000)
  if (!pb$finished) {
    pb$tick(0, tokens = list(step = "Completed processing"))
  }
  pb$terminate()
  pb_active <- FALSE
  #
  as(modelGrid_sf,"Spatial")
  #
}

# Example fixed:
# apply_distance_buffers(
#   modelGrid, files_loaded,
#   LayerName="Wetlands",
#   dataColumn="Distance",
#   df_list=df_list,
#   buffer_method="fixed",
#   buffer_breaks=c(200,400)
# )

# Example lookup:
# Ref_table <- data.frame(
#   Param=c("Offshore","Primary","Large Vessel"),
#   Value=c(1,2,5)
# )
#
# apply_distance_buffers(
#   modelGrid, files_loaded,
#   LayerName="Shipping",
#   dataColumn="Distance",
#   df_list=df_list,
#   buffer_method="lookup",
#   Ref_table=Ref_table,
#   type_column="TYPE",
#   buffer_multiplier=100,
#   buffer_units="ft"
# )
#
#
#
# Function to split specified column into new columns based on original column values. Names new columns using value split.
split_column_by_value <- function(x, column, remove_original = FALSE) {
  
  # Get attribute table
  dat <- if (inherits(x, "Spatial")) {
    x@data
  } else if (inherits(x, "sf")) {
    sf::st_drop_geometry(x)
  } else if (is.data.frame(x)) {
    x
  } else {
    stop("x must be a data.frame, Spatial* object, or sf object.")
  }
  
  # Check column exists
  if (!column %in% names(dat)) {
    stop(sprintf("Column '%s' not found.", column))
  }
  
  # Unique non-NA values
  vals <- unique(dat[[column]])
  vals <- vals[!is.na(vals)]
  
  # Create new columns
  for (v in vals) {
    
    new_name <- paste0(column, str_replace(make.names(v), "^X(?=\\d)", ""))
    
    dat[[new_name]] <- ifelse(
      dat[[column]] == v,
      dat[[column]],
      NA
    )
  }
  
  # Optionally remove original column
  if (remove_original) {
    dat[[column]] <- NULL
  }
  
  # Put data back into original object
  if (inherits(x, "Spatial")) {
    x@data <- dat
  } else if (inherits(x, "sf")) {
    geom <- sf::st_geometry(x)
    x <- sf::st_as_sf(dat)
    sf::st_geometry(x) <- geom
  } else {
    x <- dat
  }
  
  return(x)
}