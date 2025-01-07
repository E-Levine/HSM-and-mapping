###Working code for HSI creation.
##Requires StateGrid picogrid layer, estuary area layer (+ any section area layers)
#
##General outline:
#Load microgrid
#Overlay various data types
#Set scoring - make editable
#Calculate HSI score
#Output data, maps, etc.
#
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
#Working parameters - to be set each time - need to be able to track data files used (i.e., WQ, Oyster layers, etc.)
Site_Code <- c("SL") #two-letter site code
State_Grid <- c("H4")
Alt_State_Grid <- c(NA) #Two-letter StateGrid ID, enter NA if no secondary StateGrid needed
#
#
####Load files####
#
PicoGrid 
####Create function file that loads grid and estuary area, clips to overlap, saves final grid, and keeps final grid for use