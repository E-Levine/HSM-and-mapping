##Code for HSM mapping
##Requires shapefile of area with data layers/scores applied
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load required packages (should install missing packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, #Df manipulation, basic summary
               sf, raster, terra, fst,
               leaflet, tmap, openxlsx, writexl,
               ggrepel, extrafont,
               classInt, BAMMtools, #Jenks
               install = TRUE) #Mapping and figures
#Run once to get Arial:
#font_import(prompt = FALSE)
loadfonts(device = "win")
#
HSMfunc <- new.env()
source("HSM code/Functions/HSM_scoring_functions.R", local = HSMfunc)
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SL") #two-letter site code
Version <- c("v1") #Model version
#
#
# Data setup ----
#
###Load shape file with model data: default shp_filename = "_datalayer"
# Also loads files for scoring
HSMfunc$load_model_files(shp_filename = "HSM_model")
#
# Model area
Site_area <- st_read(paste0(Site_Code,"_", Version, "/Data/Layers/KML/", Site_Code, ".kml"))
plot(Site_area[2])
#
###State Outline
FL_outline <- st_read("Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#
#
# Area map ----
#
ggplot()+
  geom_sf(data = Site_area, fill = "#99CCFF")+
  geom_sf(data = FL_outline)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 12, color = "black"), 
        axis.text =  element_text(size = 10, color = "black"))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.05, st_bbox(Site_area)["xmax"]+0.15),
           ylim = c(st_bbox(Site_area)["ymin"]-0.05, st_bbox(Site_area)["ymax"]+0.05))
