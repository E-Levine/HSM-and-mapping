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
               leaflet, tmap, open
               install = TRUE) #Mapping and figures
#
#
#Working parameters - to be set each time a new site or version is being created. Make sure to use same code and v from setup file.
Site_Code <- c("SL") #two-letter site code
Version <- c("v1") #Model version
State_Grid <- c("H4")
Alt_Grid <- c(NA) #Two-letter StateGrid ID, enter NA if no secondary StateGrid needed
#
#
#
####Load files####
#
##Base grid files
Grid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",State_Grid,"/Florida_PicoGrid_WGS84_",State_Grid,"_clip.shp"))
if(!is.na(Alt_Grid)){Alt_MicroGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",Alt_State_Grid,"/Florida_PicoGrid_WGS84_",Alt_State_Grid,"_clip.shp"))}
#
#Check grid(s), view map(s)  to confirm area
head(Grid)
if(!is.na(Alt_Grid)){head(Alt_Grid)} else {print("No additional grid is being used.")}
#
##Load site KML and section KMLs as needed
OrderSections <- read_excel(paste0(Site_Code, "_", Version, "/Data/", Site_Code, "_", Version, "_model_setup.xlsx"), sheet = "Section_Order")
Site_area <- st_read(paste0("Reference files/KML/",Site_Code, "_", Version,"/", Site_Code, ".kml"))
plot(Site_area[1]) #Output site area plot

SectionList <- unlist((OrderSections %>% arrange(Order))[,"KML_Name"]) #Output list of names
for (i in seq_along(unique(SectionList))) {
  temp <- st_read(paste0("Reference files/KML/",Site_Code, "_", Version,"/", unique(SectionList)[i], ".kml"))
  assign(paste0("Section",i), temp)
  
}
#
#Sections have been loaded into objects for us. Next steps to limit grid cells and assign Section. Then move into function.