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
get_base_grid <- function(StateGrid, AltGrid, SiteCode, VersionID, SectionsDesignated, Save_data, Save_figure){
  #
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
      new_filename <- sub("\\.jpg$", paste0("_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"), jpg_filename)
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