###Code for HSI creation
##Requires StateGrid picogrid layer, site area layer (plus any section area layers) before use
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load required packages (should install missing packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, #Df manipulation, basic summary
               sf, raster, terra,
               leaflet, tmap, openxlsx,
               install = TRUE) #Mapping and figures
#
#
#Working parameters - to be set each time a new site or version is being created. Make sure to use same code and v from setup file.
Site_Code <- c("SL") #two-letter site code
Version <- c("v1") #Model version
State_Grid <- c("H4")
Alt_Grid <- c(NA) #Two-letter StateGrid ID, enter NA if no secondary StateGrid needed
#
##Parameters
Sections_designatied <- c("Y") #Y/N are section designations used
#
#
#
####Load files####
#
##Base grid files
PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",State_Grid,"/Florida_PicoGrid_WGS84_",State_Grid,"_clip.shp"))
if(!is.na(Alt_Grid)){Alt_PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",Alt_Grid,"/Florida_PicoGrid_WGS84_",Alt_Grid,"_clip.shp"))}
#
#Check grid(s), view map(s)  to confirm area
head(PicoGrid)
if(!is.na(Alt_Grid)){head(Alt_Grid)} else {print("No additional grid is being used.")}
#
#
##Excel setup information
load_working_info <- function(Site_version){
  filename <- paste0(Site_version, "/Data/", Site_version, "_model_setup.xlsx") 
  sheets <- excel_sheets(filename)
  df_list <<- lapply(sheets, function(sheet){
    read_excel(filename, sheet = sheet)
  })
  names(df_list) <- sheets
  return(df_list)
}
load_working_info(paste0(Site_Code, "_", Version))
#
#
##Load site KML and section KMLs as needed
OrderSections <- df_list[[2]]
Site_area <- st_read(paste0("Reference files/KML/",Site_Code, "_", Version,"/", Site_Code, ".kml"))
plot(Site_area[1]) #Output site area plot

SectionList <- unlist((OrderSections %>% arrange(Order))[,"KML_Name"]) #Output list of names
for (i in seq_along(unique(SectionList))) {
  temp <- st_read(paste0("Reference files/KML/",Site_Code, "_", Version,"/", unique(SectionList)[i], ".kml"))
  assign(paste0("Section",i), temp)
}
#
#END OF FILE LOADING
#
#
####Limit to desired area, assign sections####
#
####ADD CODE TO RUN IF Sections_designated == Y or skip if == N
##Limit to site area
if(!is.na(Alt_Grid)){
  Site_Grid <- rbind(PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,], 
                     Alt_PicoGrid[lengths(st_intersects(Alt_PicoGrid, Estuary_area))> 0,])
  rm(PicoGrid, Alt_PicoGrid)
} else {
  Site_Grid <- PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,] 
  rm(PicoGrid)
}
#
##Assign sections 
#Create empty list
temp_sections <- list()
 for (i in mget(ls(pattern = "Section[0-9]"))) {
   #Assign grid cells to section based on overlap with section layer
  temp <- Site_Grid[lengths(st_intersects(Site_Grid, i)) > 0,] %>% #Limit to section area
    mutate(Site = Site_Code, #Add Site code, section code
           Section = OrderSections$Section[OrderSections$KML_Name == i$Name]) %>%
    #Add site long name
    left_join(df_list[[1]] %>% filter(Type == "Site") %>% dplyr::select(Designation, LongName) %>% rename(Site = Designation))
  temp_sections[[length(temp_sections) + 1]] <- temp
  #Add all data together into one output
  Section_grid <- do.call(rbind, temp_sections) %>% 
    mutate(Section = factor(Section, levels = unique(df_list[[2]]$Section[order(df_list[[2]]$Order)]), ordered = TRUE)) %>% #Relevel Section based on specified order
    arrange(Section) %>% group_by(PGID) %>% slice(1) #Keep only one Section-assignment per grid cell
 }
rm(temp, i, temp_sections)
gc()
#
##Output head of updated data frame and map of sections
head(Section_grid)
tm_shape(Section_grid) + tm_fill(col = "Section")
#
#
###END OF SECTION ASSIGNMENT
#
#