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
               sf, raster, terra, fst,
               leaflet, tmap, openxlsx, writexl,
               classInt, BAMMtools, #Jenks
               install = TRUE) #Mapping and figures
#
#
#source("HSM code/Functions/HSM_Creation_Functions.R")
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
###Load shape file with data from Arc: default shp_filename = "_datalayer"
# Also loads files for scoring
HSMfunc$load_model_files(shp_filename = "datalayers_260106")
#
# Check potential file names:
(datafiles <- HSMfunc$list_files(paste0(Site_Code,"_",Version,"/Output/Data files"),
                   pattern = "\\.xlsx$"))
#
# Add and clean interp data ----
#
# Add interp data: one call per data column/type
#
# Annual mean salinity
SL_v1_salMonMean <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Mean_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)
#
SL_v1_data <- left_join(SL_v1_data, 
          HSMfunc$row_average(
            data = SL_v1_salMonMean,
            cols = contains("ens"),
            new_column_name = "SAnnueE",
            keep_columns = c("PGID")
            ),
        by = "PGID")
#
# May-Oct mean salinity
(SL_v1_data <- left_join(SL_v1_data,
                        HSMfunc$row_average(
                          data = SL_v1_salMonMean,
                          cols = c("ens_May_Mean", "ens_Jun_Mean", "ens_Jul_Mean", "ens_Aug_Mean", "ens_Sep_Mean", "ens_Oct_Mean"),
                          new_column_name = "SSpwneE",
                          keep_columns = c("PGID")
                          ),
                        by = "PGID"))
#
# Annual minimum salinity
SL_v1_salMonMin <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Minimum_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)
#
(SL_v1_data <- left_join(SL_v1_data,
                         HSMfunc$row_average(
                           data = SL_v1_salMonMin,
                           cols = contains("ens"),
                           new_column_name = "SAnnueI",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
# May-Oct range salinity
SL_v1_salMonRange <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Range_2020_2024_May_Oct.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)
#
# Average min max score within each month:
SL_v1_salMonRange <- SL_v1_salMonRange %>%
  mutate(
    ens_May_Range = rowMeans(cbind(ens_May_Maximum, ens_May_Minimum), na.rm = TRUE),
    ens_Jun_Range = rowMeans(cbind(ens_Jun_Maximum, ens_Jun_Minimum), na.rm = TRUE),
    ens_Jul_Range = rowMeans(cbind(ens_Jul_Maximum, ens_Jul_Minimum), na.rm = TRUE),
    ens_Aug_Range = rowMeans(cbind(ens_Aug_Maximum, ens_Aug_Minimum), na.rm = TRUE),
    ens_Sep_Range = rowMeans(cbind(ens_Sep_Maximum, ens_Sep_Minimum), na.rm = TRUE),
    ens_Oct_Range = rowMeans(cbind(ens_Oct_Maximum, ens_Oct_Minimum), na.rm = TRUE)
  )
#
(SL_v1_data <- left_join(SL_v1_data,
                         HSMfunc$row_average(
                           data = SL_v1_salMonRange,
                           cols = contains("Range"),
                           new_column_name = "SSpwneR",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
# Annual mean temperature
SL_v1_temMonMean <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Temperature, water_Monthly_Mean_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)
#
SL_v1_data <- left_join(SL_v1_data, 
                        HSMfunc$row_average(
                          data = SL_v1_temMonMean,
                          cols = contains("ens"),
                          new_column_name = "TAnnueE",
                          keep_columns = c("PGID")
                        ),
                        by = "PGID")
#
# May-Oct mean temperature
(SL_v1_data <- left_join(SL_v1_data,
                         HSMfunc$row_average(
                           data = SL_v1_temMonMean,
                           cols = c("ens_May_Mean", "ens_Jun_Mean", "ens_Jul_Mean", "ens_Aug_Mean", "ens_Sep_Mean", "ens_Oct_Mean"),
                           new_column_name = "TSpwneE",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
# Annual T > 35 temperature
SL_v1_temMonT35 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Temperature, water_Monthly_Threshold_2020_2024_30.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)
#
SL_v1_data <- left_join(SL_v1_data, 
                        HSMfunc$row_average(
                          data = SL_v1_temMonT35,
                          cols = contains("Threshold"),
                          new_column_name = "TAnnueT30",
                          keep_columns = c("PGID")
                        ),
                        by = "PGID")
#
# May-Oct T < 20 temperature
SL_v1_temMonB20 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Temperature, water_Monthly_Threshold_2020_2024_May_Oct_20.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)
SL_v1_temMonB20$ens_Jun_Threshold <- as.numeric(SL_v1_temMonB20$ens_Jun_Threshold)
SL_v1_temMonB20$ens_Sep_Threshold <- as.numeric(SL_v1_temMonB20$ens_Sep_Threshold)
SL_v1_temMonB20$ens_Oct_Threshold <- as.numeric(SL_v1_temMonB20$ens_Oct_Threshold)
#
SL_v1_data <- left_join(SL_v1_data, 
                        HSMfunc$row_average(
                          data = SL_v1_temMonB20,
                          cols = contains("Threshold"),
                          new_column_name = "TSpwneT20",
                          keep_columns = c("PGID")
                        ),
                        by = "PGID")
#
# OUtlier1 flow
(SL_v1_outlier1 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_outlier1.xlsx"),
  join_by = "PGID",
  excel_columns = "meanOut1",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                        HSMfunc$row_average(
                          data = SL_v1_outlier1,
                          cols = contains("Out"),
                          new_column_name = "FAnnui1",
                          keep_columns = c("PGID")
                        ),
                        by = "PGID"))
#
# OUtlier2 flow
(SL_v1_outlier2 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_outlier2.xlsx"),
  join_by = "PGID",
  excel_columns = "meanOut2",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_outlier2,
                           cols = contains("Out"),
                           new_column_name = "FAnnui2",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
# Adult optimal flow
(SL_v1_adop <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_optimal_adult.xlsx"),
  join_by = "PGID",
  excel_columns = "meanOptimal",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_adop,
                           cols = contains("Optimal"),
                           new_column_name = "FAnnuiAO",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae optimal flow
(SL_v1_laop <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_optimal_larvae.xlsx"),
  join_by = "PGID",
  excel_columns = "meanOptimal",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_laop,
                           cols = contains("Optimal"),
                           new_column_name = "FAnnuiLO",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Adult super flow
(SL_v1_adsup <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_super_adult.xlsx"),
  join_by = "PGID",
  excel_columns = "meanDays",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_adsup,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiAP",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Adult sub flow
(SL_v1_adsub <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_sub_adult.xlsx"),
  join_by = "PGID",
  excel_columns = "meanDays",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_adsub,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiAB",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae super flow
(SL_v1_lasup <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_super_larvae.xlsx"),
  join_by = "PGID",
  excel_columns = "meanDays",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_lasup,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiLP",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae sub flow
(SL_v1_lasub <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_sub_larvae.xlsx"),
  join_by = "PGID",
  excel_columns = "meanDays",
  sheet = 1,
  join_type = "left"
))
#
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_lasub,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiLB",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
#
rm(datafiles, SL_v1_salMonMean, SL_v1_salMonMin, SL_v1_salMonRange, 
   SL_v1_temMonMean, SL_v1_temMonT35, SL_v1_temMonB20,
   SL_v1_outlier1, SL_v1_outlier2, SL_v1_adop, SL_v1_laop, SL_v1_adsup, SL_v1_adsub, SL_v1_lasup, SL_v1_lasub)
#
#
# Assign scores ----
#
##Oysters
temp <- get(paste0(Site_Code, "_", Version, "_data"))
Oyster_scores <- HSMfunc$assign_oyster_scores(temp)
#
#
#Oyster reef buffer scores
Oybuffer_scores <- HSMfunc$assign_oybuffer_scores(temp)
#
#
#
#
##Seagrass scores
Seagrass_scores <- HSMfunc$assign_seagrass_scores(temp)
#
#
#
##Channels
#If not NA, then score = 0 (present in buffer zone). If NA then score = 1
Channel_scores <- HSMfunc$assign_buffer_scores(temp)
#
#
#
##Salinity - all year
Salinity_scores <- HSMfunc$assign_salinity_scores(temp, Salinity_adult, type = "separate")
#
#Spawning period
Salinity_spawn_scores_t <- HSMfunc$assign_sal_spawn_scores(temp, Salinity_adult, type = "separate")
Salinity_spawn_scores <- left_join(Salinity_spawn_scores_t, 
                                   HSMfunc$assign_sal_spawn_scores(temp, Salinity_larvae, type = "separate") %>% st_drop_geometry()) 
#
#
#
##Temperature - all year
Temperature_scores <- HSMfunc$assign_temperature_scores(temp, Temperature_adult, type = "separate")
#
#Spawning period
Temperature_spawn_scores_t <- HSMfunc$assign_temperature_spawn_scores(temp, Temperature_adult, type = "separate")
Temperature_spawn_scores <- left_join(Temperature_spawn_scores_t, 
                                      HSMfunc$assign_temperature_spawn_scores(temp, Temperature_larvae, type = "separate") %>% st_drop_geometry()) 
#
#Threshold period - number = proportion above.below the threshold - score is inverse of values
Temperture_thres_scores <- HSMfunc$assign_threshold_scores(temp, type = "separate")
#
#
#
##Flow data
Optimal_flow_t <- HSMfunc$assign_flow_scores(temp, `Optimal flow`, col_pattern = ".*O$",type = "separate")
Above_flow_t <- HSMfunc$assign_flow_scores(temp, `Non-optimal flow`, col_pattern = ".*P$",type = "separate")
Sub_flow_t <- HSMfunc$assign_flow_scores(temp, `Non-optimal flow`, col_pattern = ".*B$",type = "separate")
Out1_flow_t <- HSMfunc$assign_flow_scores(temp, `Outlier1 flow`, col_pattern = ".*1$",type = "separate")#
Out2_flow_t <- HSMfunc$assign_flow_scores(temp, `Outlier2 flow`, col_pattern = ".*2$",type = "separate")#
#
Flow_scores <- left_join(Optimal_flow_t, st_drop_geometry(Above_flow_t)) %>% 
  left_join(st_drop_geometry(Sub_flow_t)) %>%
  left_join(st_drop_geometry(Out1_flow_t)) %>%
  left_join(st_drop_geometry(Out2_flow_t))
#
#
#
#
# Model wrap-up ----
###Add scores back to data
assign(paste0(Site_Code, "_", Version, "_scores_data"), HSMfunc$join_score_dataframes(temp))
##Work with just scores
assign(paste0(Site_Code, "_", Version, "_scores_only"),  get(paste0(Site_Code, "_", Version, "_scores_data")) %>% 
         dplyr::select(PGID, Lat_DD_Y, Long_DD_X, ends_with("SC"), ends_with("SCL")))
#
##Set working with "all" data or with just "scores" data:
model_data <- c("scores")
#
###Calculate total HSM score
if(model_data == "all"){
  assign(paste0(Site_Code, "_", Version, "_data_totals"), HSMfunc$calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_data"))))
} else {
  assign(paste0(Site_Code, "_", Version, "_data_totals"), HSMfunc$calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_only"))))
}
#
#Clean model data frame
assign(paste0(Site_Code, "_", Version, "_data_clean"), HSMfunc$clean_model_data(get(paste0(Site_Code, "_", Version, "_data_totals"))))
#
#
#
## HSM values ----
#
HSM_data <- get(paste0(Site_Code, "_", Version, "_data_clean")) %>% st_drop_geometry() %>% 
  mutate(CurveCO = sum(grepl("AV$", names(st_drop_geometry(get(paste0(Site_Code, "_", Version, "_data_clean"))))))) %>% 
  mutate(HSM = case_when(ChnlTO == 1 ~ (OystAV + BuffAV + SgrsAV + SAV + TAV + FAV)/CurveCO,
                             ChnlTO == 0 ~ 0, 
                             TRUE ~ NA_real_)) %>%
  mutate(HSMround = round(HSM, 2))
#Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#Determine natural Jenks breaks (thirds)
jenks_breaks <- getJenksBreaks(var = HSM_data$HSM, k = 4)
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
                           TRUE ~ HSMgrp), levels = c("0", "Low", "Moderate", "High"))) %>%
  ungroup() %>%
  mutate(HSMjb = factor(if_else(is.na(HSM), NA_character_, 
                                as.character(cut(HSM,
                                                 breaks = jenks_breaks,
                                                 include.lowest = TRUE,
                                                 labels = c("Low", "Medium", "High")))), levels = c("Low", "Medium", "High")))
summary(HSM_data_grps$HSMgrp)
summary(HSM_data_grps$HSMgyr)
summary(HSM_data_grps$HSMjb)
levels(cut(HSM_data$HSM, breaks = jenks_breaks, include.lowest = TRUE))
jenks.tests(classIntervals(HSM_data$HSM, n = 3, style = "fixed", fixedBreaks = jenks_breaks))
#
hist(HSM_data$HSM, col = "gray90", main = "Jenks Breakpoints Overlay", xlab = "HSM score")
abline(v = jenks_breaks, col = "red", lwd = 2, lty = 2)
text(x = jenks_breaks, y = 5, labels = round(jenks_breaks, 2), pos = 4, col = "red")
### SAVE PLOT: SiteCode_version_HSMjb_hist - ~850 * auto
#
#
HSM_spdf <- left_join(get(paste0(Site_Code,"_", Version, "_data")), HSM_data_grps) %>% st_zm() %>% 
  dplyr::select(any_of(c("PGID", "Lat_DD_Y", "Long_DD_X", "State_Ref", "Ref_Region", "County", "Section")), contains("HSM"))
#
#Check data
#library(viridis)
#tm_shape(US_HSM_spdf)+
#  tm_polygons(fill = "HSM_grp", col = NA)
#
#
##Output data file and shape file: currently required temp and HSM_data_grps
HSMfunc$save_model_output(output_type = "all")
#
HSMfunc$plot_model_map(HSM_spdf, "HSMgrp") #SiteCode_Version_HSM_scores
HSMfunc$plot_model_map(HSM_spdf, "HSMjb") #SiteCode_Version_HSM_jb
