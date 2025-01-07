###Code for HSI creation
##Requires StateGrid picogrid layer, estuary area layer (plus any section area layers) before use
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