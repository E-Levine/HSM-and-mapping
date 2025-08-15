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
               leaflet, tmap, openxlsx, writexl,
               foreach, doParallel,
               install = TRUE) #Mapping and figures
#
#
source("HSM code/Functions/HSM_Creation_Functions.R")
#
#Working parameters - to be set each time a new site or version is being created. Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SL") #two-letter site code
Version <- c("v1") #Model version
State_Grid <- c("H4")
Alt_Grid <- c(NA) #Two-letter StateGrid ID, enter NA if no secondary StateGrid needed
#
##Parameters
Sections_designated <- c("Y") #Y/N are section designations used
##Polygon data:
FL_Oysters <- c("Y") #Oyster beds in Florida: Include Oyster layer data ("Data"), include layer data and scoring ("Score"), or don't include data or scoring ("None")
#
#
#
####Load base information and assign Site/Sections####
#
#Loads Excel file data information as designated during version set up:
load_working_info(Site_Code, Version)
#
#Assign site and section designations to grid cells.
#Data and shapefile can be saved using Save_data = "Y" and save a figure using Save_figure = "Y". Default is "N"
get_base_grid(State_Grid, Alt_Grid, Site_Code, Version, Sections_designated, Save_data = "Y", Save_figure = "Y")
#
#
###END OF SECTION
#
####Loading data layers####
#
##Identify folders and files based on data type
find_folder_names("Oysters")
#
##Load data for specified folders
Start_date <- "2020-01-01"
End_date <- "2024-12-31"
load_matching_shp("Oysters")
#