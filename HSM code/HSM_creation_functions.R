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
#
#### Function to load grid(s) and site area, clips to overlap, saves final grid, and keeps final grid for use
get_base_grid <- function(StateGrid, AltGrid, SiteCode, VersionID, SectionsDesignated){
  #
  #Load StateGrid(s) of picogrid
  PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",StateGrid,"/Florida_PicoGrid_WGS84_",StateGrid,"_clip.shp"))
  if(!is.na(AltGrid)){Alt_PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",AltGrid,"/Florida_PicoGrid_WGS84_",AltGrid,"_clip.shp"))}
  #
  #Check grid(s), to confirm area
  head(PicoGrid)
  if(!is.na(AltGrid)){head(AltGrid)} else {print("No additional grid is being used.")}
  #
  ##Load site KML and section KMLs as needed
  OrderSections <- df_list[[2]]
  Site_area <- st_read(paste0("Reference files/KML/KML_",SiteCode, "_", VersionID,"/", SiteCode, ".kml"))
  #plot(Site_area[1]) #Output site area plot
  #
  SectionList <- unlist((OrderSections %>% arrange(Order))[,"KML_Name"]) #Output list of names
  for (i in seq_along(unique(SectionList))) {
    temp <- st_read(paste0("Reference files/KML/KML_",SiteCode, "_", VersionID,"/", unique(SectionList)[i], ".kml"))
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
    Section_grid <- Site_Grid %>% 
      #Add Site code and Sections as NA
      mutate(Site = SiteCode, Section = NA) %>% 
      #Add site long name
      left_join(df_list[[1]] %>% filter(Type == "Site") %>% dplyr::select(Designation, LongName) %>% rename(Site = Designation)) 
    ##Output head of updated data frame and map of sections
    head(Section_grid)
    Section_plot <<- tm_shape(Section_grid) + tm_fill(col = "Section")
    #
  } else if(SectionsDesignated == "Y"){
    #Create empty list
    temp_sections <- list()
    for (i in mget(ls(pattern = "Section[0-9]"))) {
      #Assign grid cells to section based on overlap with section layer
      temp <- Site_Grid[lengths(st_intersects(Site_Grid, i)) > 0,] %>% #Limit to section area
        mutate(Site = SiteCode, #Add Site code, section code
               Section = OrderSections$Section[OrderSections$KML_Name == i$Name]) %>%
        #Add site long name
        left_join(df_list[[1]] %>% filter(Type == "Site") %>% dplyr::select(Designation, LongName) %>% rename(Site = Designation))
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
  return(list(paste0("Sections included in site:"), SectionList, Section_plot))
}
#