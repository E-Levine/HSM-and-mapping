###Code for HSI creation
##Requires StateGrid picogrid layer, site area layer (plus any section area layers) before use
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load require packages (should install missing packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, terra,
               leaflet, tmap, 
               install = TRUE) #Mapping and figures
#
#
#Working parameters - to be set each time a new site or version is being created. Make sure to use same code and v from setup file.
Site_Code <- c("SL") #two-letter site code
State_Grid <- c("H4")
Alt_State_Grid <- c(NA) #Two-letter StateGrid ID, enter NA if no secondary StateGrid needed
#
#
#
####Load files####
#
##Base grid files
Grid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",State_Grid,"/Florida_PicoGrid_WGS84_",State_Grid,"_clip.shp"))
if(!is.na(Alt_Grid)){Alt_MicroGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",Alt_State_Grid,"/Florida_PicoGrid_WGS84_",Alt_State_Grid,"_clip.shp"))}
#
#Check data, view map  to confirm area
plot(Grid$geometry); head(Grid)
#Check alternate
if(!is.na(Alt_Grid)){plot(Alt_Grid$geometry); head(Alt_Grid)} else {print("No additional grid is being used.")}
#