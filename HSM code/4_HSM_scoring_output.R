###Code for HSM scoring and output
##Requires shapefile of area with data layers applied
##Currently working with files output from Arc
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
               install = TRUE) #Mapping and figures
#
#
source("HSM code/Functions/HSM_Creation_Functions.R")
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("US") #two-letter site code
Version <- c("v1") #Model version
#
#
###Load shape file with data: default shp_filename = "_datalayer"
load_model_files()
#
#
####Assign scores
#
##Oysters
temp <- get(paste0(Site_Code, "_", Version, "_data"))
Oyster_scores <- assign_oyster_scores(temp)
#
#
#Oyster reef buffer scores
Oybuffer_scores <- assign_oybuffer_scores(temp)
#
#
#
#
##Seagrass scores
Seagrass_scores <- assign_seagrass_scores(temp)
#
#
#
##Channels
#If not NA, then score = 0 (present in buffer zone). If NA then score = 1
Channel_scores <- assign_buffer_scores(temp)
#
#
#
##Salinity - all year
Salinity_scores <- assign_salinity_scores(temp, Salinity_adult, type = "ensemble")
#
#Spawning period
Salinity_spawn_scores_t <- assign_sal_spawn_scores(temp, Salinity_adult, type = "ensemble")
Salinity_spawn_scores <- left_join(Salinity_spawn_scores_t, assign_sal_spawn_scores(temp, Salinity_larvae, type = "ensemble"))
#
#
#
##Temperature - all year
Temperature_scores <- assign_temperature_scores(temp, Temperature_adult, type = "ensemble")
#
#Spawning period
Temperature_spawn_scores_t <- assign_temperature_spawn_scores(temp, Temperature_adult, type = "ensemble")
Temperature_spawn_scores <- left_join(Temperature_spawn_scores_t, assign_temperature_spawn_scores(temp, Temperature_larvae, type = "ensemble"))
#
#Threshold period - number = proportion above.below the threshold - score is inverse of values
Temperture_thres_scores <- assign_threshold_scores(temp, type = "ensemble")
#
#
#
#
#
###Add scores back to data
assign(paste0(Site_Code, "_", Version, "_scores_data"), join_score_dataframes(temp))
#
#
#
###Calculate total HSM score
assign(paste0(Site_Code, "_", Version, "_data_totals"), calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_data"))))
#
#Clean model data frame
assign(paste0(Site_Code, "_", Version, "_data_clean"), clean_model_data(get(paste0(Site_Code, "_", Version, "_data_totals"))))
#
#
#
#SCORING
#
HSM_data <- get(paste0(Site_Code, "_", Version, "_data_clean")) %>% st_drop_geometry() %>% 
  mutate(CurveCO = sum(grepl("AV$", names(st_drop_geometry(get(paste0(Site_Code, "_", Version, "_data_clean"))))))) %>% 
  mutate(HSM = case_when(ChnlTO == 1 ~ (OystAV + BuffAV + SgrsAV + SAV + TAV)/CurveCO,
                             ChnlTO == 0 ~ 0, 
                             TRUE ~ NA_real_)) %>%
  mutate(HSMround = round(HSM, 2))
#Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#Assign groups using cut()
HSM_data_grps <- HSM_data %>%
  mutate(HSMgrp = as.factor(cut(HSMround, breaks = breaks, include.lowest = TRUE, right = FALSE))) %>%
  mutate(HSMgrp = case_when(HSMround == 0 ~ "0", 
                             HSMgrp == '[0,0.1)' ~ '(0,0.1)',
                             TRUE ~ as.character(HSMgrp))) %>%
  mutate(HSMgrp = factor(HSMgrp, levels = c("0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]"))) %>%
  mutate(HSMgyr = factor(case_when(HSMgrp %in% c("(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)") ~ "Low",
                             HSMgrp %in% c("[0.4,0.5)", "[0.5,0.6)") ~ "Moderate",
                             HSMgrp %in% c("[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]") ~ "High",
                           TRUE ~ HSMgrp), levels = c("0", "Low", "Moderate", "High")))
summary(HSM_data_grps$HSMgrp)
summary(HSM_data_grps$HSMgyr)
#
#
#
HSM_spdf <- left_join(get(paste0(Site_Code,"_", Version, "_data")), HSM_data_grps)
#
#Check data
#library(viridis)
#tm_shape(US_HSM_spdf)+
#  tm_polygons(fill = "HSM_grp", col = NA)
#
#
##Output data file and shape file:
save_model_output(HSM_spdf)
#