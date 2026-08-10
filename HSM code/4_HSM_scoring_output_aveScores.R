###Code for HSM scoring and output - scoring then average of scores
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
               ggrepel, extrafont,
               classInt, BAMMtools, #Jenks
               install = TRUE) #Mapping and figures
#Run once to get Arial:
#font_import(prompt = FALSE)
loadfonts(device = "win")
#
#source("HSM code/Functions/HSM_Creation_Functions.R")
HSMfunc <- new.env()
source("HSM code/Functions/HSM_scoring_functions.R", local = HSMfunc)
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("WI") #two-letter site code
Version <- c("v1") #Model version
#
#
# Data setup ----
#
###Load shape file with data from Arc: default shp_filename = "_datalayer"
# Also loads files for scoring
HSMfunc$load_model_files(shp_filename = "datalayers_20260724")
#
# Check potential file names:
#(datafiles <- HSMfunc$list_files(paste0(Site_Code,"_",Version,"/Output/Data files"),
#                                 pattern = "\\.(xlsx|csv)$"))
#
#
# Data setup, updates ----
#
# Load model files with updated data:
HSMfunc$load_model_files(shp_filename = "datalayers_260217")
# Limit to PGID and data being updated:
glimpse(WI_v1_data)
#Combine original data with new data, then skip to scoring
#t <- st_join(SS_v0_data %>% dplyr::select(PGID:Long_DD_X_, Oyst26, Buff26), 
#             SS_vori_data%>% dplyr::select(-c(Oyst20, Buff23))) 
#SS_v0_data <- t
(WC_v1_2data <- WC_v1_data %>% dplyr::select(PGID, Oyst26, Buff26))
# Load previous data scores to update. Scores to include interpolation data:
model_data <- HSMfunc$load_model_data(Site_Code, Version)
# Remove scores needing updates, final scoring columns:
(model_data_2 <- model_data %>% 
  dplyr::select(-c(Buff23SC, Oyst20SC, BuffAV, OystAV)))
# Re-score data as needed -- skip to "Assign scores" and run required scoring code
# Combine original scores and updated scores, recalculate model composite score:
model_scores <- model_data_2 %>% dplyr::select(-c(contains("AV"), contains("HSM")))
# -- skip to "Model scoring". Run 941-1039 at least, then 1220
#
# Add and clean interp data: Excel cols----
#
currentsf <- WI_v1_data
#
#Annual mean salinity
(WI_v1_salMonMean <- HSMfunc$add_excel_columns_sf(
  existing_sf = currentsf,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Mean_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
))
#
#
#
# Annual minimum salinity
(SL_v1_salMonMin <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Minimum_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
)) 
#
#
#
# May-Oct range salinity
(SL_v1_salMonRange <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Range_2020_2024_May_Oct.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
))
#
#
#
# Annual mean temperature
(SL_v1_temMonMean <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Temperature, water_Monthly_Mean_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
))
#
#
#
#
#
#
#
#
# Annual T > 35 temperature
(SL_v1_temMonT35 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Temperature, water_Monthly_Threshold_2020_2024_30.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
))
#
#
#
#
#
# May-Oct T < 20 temperature
(SL_v1_temMonB20 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Temperature, water_Monthly_Threshold_2020_2024_May_Oct_20.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
))
#
#SS_v1_temMonB20$ens_Jun_Threshold <- as.numeric(SS_v1_temMonB20$ens_Jun_Threshold)
#SS_v1_temMonB20$ens_Sep_Threshold <- as.numeric(SS_v1_temMonB20$ens_Sep_Threshold)
#SS_v1_temMonB20$ens_Oct_Threshold <- as.numeric(SS_v1_temMonB20$ens_Oct_Threshold)
#
#
#
# Outlier1 flow
(SL_v1_outlier1 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data %>% dplyr::select(PGID:Long_DD_X),
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_outlier1.xlsx"),
  join_by = "PGID",
  excel_columns = "meanOut1",
  sheet = 1,
  join_type = "left"
))
#
#
#
#
#
# Outlier2 flow
(SL_v1_outlier2 <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/SL_flow_outlier2.xlsx"),
  join_by = "PGID",
  excel_columns = "meanOut2",
  sheet = 1,
  join_type = "left"
))
#
#
#
#
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
#
#
#
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
#
#
#
#
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
#
#
#
#
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
#
#
#
#
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
#
#
#
#
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
#
rm(datafiles)
#
#
#
#
#
# Add and clean interp data: CSV files----
#
currentsf <- WI_v1_data
#
#Annual mean salinity
(WI_v1_salMonMean <- HSMfunc$read_data_files_csv(Site_Code, 
                                                Version, 
                                                data_subdir = "Salinity_Monthly_Mean_2020_2024") %>%
  as.data.frame())
#
#
#
#
# Annual minimum salinity
(SS_v1_salMonMin <- HSMfunc$read_data_files_csv(Site_Code, 
                                                Version, 
                                                data_subdir = "Salinity_Monthly_Mins_2020_2024") %>%
    as.data.frame())
#
#
#
#
# May-Oct range salinity
(SS_v1_salMonRange <- HSMfunc$read_data_files_csv(Site_Code, 
                                                  Version, 
                                                  data_subdir = "Salinity_Monthly_Range_2020_2024") %>%
    as.data.frame())
#
#
#
#
# Annual mean temperature
(SS_v1_temMonMean <- HSMfunc$read_data_files_csv(Site_Code, 
                                                 Version, 
                                                 data_subdir = "Temperature, water_Monthly_Means_2020_2024") %>%
    as.data.frame())
#
#
#
#
# Annual T > 35 temperature
(SS_v1_temMonT35 <- HSMfunc$read_data_files_csv(Site_Code, 
                                                Version, 
                                                data_subdir = "Temperature, water_Monthly_ThresholdA35_2020_2024") %>%
    as.data.frame())
#
#
#
#
#
# May-Oct T < 20 temperature
(SS_v1_temMonB20 <- HSMfunc$read_data_files_csv(Site_Code, 
                                                Version, 
                                                data_subdir = "Temperature, water_Monthly_ThresholdB20_2020_2024") %>%
    as.data.frame())
#
#SS_v1_temMonB20$ens_Jun_Threshold <- as.numeric(SS_v1_temMonB20$ens_Jun_Threshold)
#SS_v1_temMonB20$ens_Sep_Threshold <- as.numeric(SS_v1_temMonB20$ens_Sep_Threshold)
#SS_v1_temMonB20$ens_Oct_Threshold <- as.numeric(SS_v1_temMonB20$ens_Oct_Threshold)
#
#
#
#
#
#
# Outlier1 flow
(SS_v1_outlier1 <- HSMfunc$read_data_files_csv(Site_Code, 
                                               Version, 
                                               data_subdir = "SS_flow_outlier1") %>%
    as.data.frame())
#
#
#
#
# Outlier2 flow
(SS_v1_outlier2 <- HSMfunc$read_data_files_csv(Site_Code, 
                                               Version, 
                                               data_subdir = "SS_flow_outlier2") %>%
    as.data.frame())
#
#
#
#
# Adult optimal flow
(SS_v1_adop <- HSMfunc$read_data_files_csv(Site_Code, 
                                           Version, 
                                           data_subdir = "SS_flow_optimal_adult") %>%
    as.data.frame())
#
#
#
#
# Larvae optimal flow
(SS_v1_laop <- HSMfunc$read_data_files_csv(Site_Code, 
                                           Version, 
                                           data_subdir = "SS_flow_optimal_larvae") %>%
    as.data.frame())
#
#
#
#
# Adult super flow
(SS_v1_adsup <- HSMfunc$read_data_files_csv(Site_Code, 
                                            Version, 
                                            data_subdir = "SS_flow_super_adult") %>%
    as.data.frame())#
#
#
#
#
# Adult sub flow
(SS_v1_adsub <- HSMfunc$read_data_files_csv(Site_Code, 
                                            Version, 
                                            data_subdir = "SS_flow_sub_adult") %>%
    as.data.frame())
#
#
#
#
# Larvae super flow
(SS_v1_lasup <- HSMfunc$read_data_files_csv(Site_Code, 
                                            Version, 
                                            data_subdir = "SS_flow_super_larvae") %>%
    as.data.frame())
#
#
#
#
# Larvae sub flow
(SS_v1_lasub <- HSMfunc$read_data_files_csv(Site_Code, 
                                            Version, 
                                            data_subdir = "SS_flow_sub_larvae") %>%
    as.data.frame())
#
rm(datafiles)
#
#
#
#
# Add and clean interp data: Shapefiles----
#
currentsf <- WI_v1_data
#
#Annual mean salinity
WI_v1_salMonMean <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                   sf_object = currentsf,
                                                   shapefile_name = "Salinity_Monthly_Mean_2020_2024",
                                                   columns = starts_with("e") & ends_with("e"))
head(WI_v1_salMonMean)
#
#
#
#
# Annual minimum salinity
WI_v1_salMonMin <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                   sf_object = currentsf,
                                                   shapefile_name = "Salinity_Monthly_Minimum_2020_2024",
                                                   columns = starts_with("e") & ends_with("i"))
#
#
#
#
# May-Oct range salinity
WI_v1_salMonRange <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                    sf_object = currentsf,
                                                    shapefile_name = "Salinity_Monthly_Range_2020_2024_May_Oct",
                                                    columns = starts_with("e"))
#
#
#
#
# Annual mean temperature
WI_v1_temMonMean <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                   sf_object = currentsf,
                                                   shapefile_name = "Temperature, water_Monthly_Mean_2020_2024",
                                                   columns = starts_with("e") & ends_with("e"))
#
#
#
#
#
#
#
# Annual T > 35 temperature
WI_v1_temMonT35 <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                  sf_object = currentsf,
                                                  shapefile_name = "Temperature, water_Monthly_Threshold_2020_2024_35",
                                                  columns = starts_with("e") & ends_with("T"))
#
#
#
#
#
# May-Oct T < 20 temperature
WI_v1_temMonB20 <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                  sf_object = currentsf,
                                                  shapefile_name = "Temperature, water_Monthly_Threshold_2020_2024_May_Oct_20",
                                                  columns = starts_with("e") & ends_with("T"))
#
#SS_v1_temMonB20$ens_Jun_Threshold <- as.numeric(SS_v1_temMonB20$ens_Jun_Threshold)
#SS_v1_temMonB20$ens_Sep_Threshold <- as.numeric(SS_v1_temMonB20$ens_Sep_Threshold)
#SS_v1_temMonB20$ens_Oct_Threshold <- as.numeric(SS_v1_temMonB20$ens_Oct_Threshold)
#
#
#
# Outlier1 flow
WI_v1_outlier1 <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                 sf_object = currentsf,
                                                 shapefile_name = "WI_flow_outlier1",
                                                 columns = "meanOut1")
#
#
#
#
#
#
# Outlier2 flow
WI_v1_outlier2 <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                                 sf_object = currentsf,
                                                 shapefile_name = "WI_flow_outlier2",
                                                 columns = "meanOut2")
#
#
#
#
#
# Adult optimal flow
WI_v1_adop <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                             sf_object = currentsf,
                                             shapefile_name = "WI_flow_optimal_adult",
                                             columns = "mnOptml")
#
#
#
#
# Larvae optimal flow
WI_v1_laop <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                             sf_object = currentsf,
                                             shapefile_name = "WI_flow_optimal_larvae",
                                             columns = "mnOptml")
#
#
#
#
#
# Adult super flow
WI_v1_adsup <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                              sf_object = currentsf,
                                              shapefile_name = "WI_flow_super_adult",
                                              columns = "meanDays")
#
#
#
#
#
# Adult sub flow
WI_v1_adsub <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                              sf_object = currentsf,
                                              shapefile_name = "WI_flow_sub_adult",
                                              columns = "meanDays")
#
#
#
#
#
# Larvae super flow
WI_v1_lasup <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                              sf_object = currentsf,
                                              shapefile_name = "WI_flow_super_larvae",
                                              columns = "meanDays")
#
#
#
#
#
# Larvae sub flow
WI_v1_lasub <- HSMfunc$load_shapefile_columns(Site_Code, Version,
                                              sf_object = currentsf,
                                              shapefile_name = "WI_flow_sub_larvae",
                                              columns = "meanDays")
#
#
#
#
#
#
#
# Base data----
#
ASalE <- WI_v1_salMonMean %>% 
       st_drop_geometry() %>%
       dplyr::select(PGID, contains("ens"), starts_with("e")) %>%
       dplyr::rename_with(
         ~ sub("^[^_]+_([^_]+)_.*$", "\\1", .x),
         -PGID
       ) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA)))
#
# Annual minimum salinity
ASalI <- WI_v1_salMonMin %>% 
  st_drop_geometry() %>%
  dplyr::select(PGID, contains("ens"), starts_with("e")) %>%
  dplyr::rename_with(
    ~ sub("^[^_]+_([^_]+)_.*$", "\\1", .x),
    -PGID
  ) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA)))
#
#
#
# May-Oct range salinity
ASalR <- WI_v1_salMonRange %>% 
  st_drop_geometry() %>%
  dplyr::select(PGID, contains("ens"), starts_with("e")) %>%
  dplyr::rename_with(
    ~ ifelse(
      grepl("^[^_]+_[^_]+_[^_]+_.*$", .x),
      sub("^[^_]+_([^_]+_[^_]+)_.*$", "\\1", .x),
      sub("^[^_]+_", "", .x)
    ),
    -PGID
  ) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA)))
#
#
#
# Annual mean temperature
ATemE <- WI_v1_temMonMean %>% 
  st_drop_geometry() %>%
  dplyr::select(PGID, contains("ens"), starts_with("e")) %>%
  dplyr::rename_with(
    ~ sub("^[^_]+_([^_]+)_.*$", "\\1", .x),
    -PGID
  ) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA)))
#
#
#
#
# Annual T > 35 temperature
ATemA35 <- WI_v1_temMonT35 %>% 
  st_drop_geometry() %>%
  dplyr::select(PGID, contains("ens"), starts_with("e")) %>%
  dplyr::rename_with(
    ~ sub("^[^_]+_([^_]+)_.*$", "\\1", .x),
    -PGID
  ) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA)))
#
#
#
#
# May-Oct T < 20 temperature
STemB20 <- WI_v1_temMonB20 %>% 
  st_drop_geometry() %>%
  dplyr::select(PGID, contains("ens"), starts_with("e")) %>%
  dplyr::rename_with(
    ~ sub("^[^_]+_([^_]+)_.*$", "\\1", .x),
    -PGID
  ) %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA)))
#
#
#
#
# Summary of data----
#
#Annual mean salinity
bind_rows(
  # By month:
  ASalE %>% 
    # Summarize
    summarise(across(where(is.numeric), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = T),
      min = \(x) min(x, na.rm = T),
      max = \(x) max(x, na.rm = T)))) %>%
    # Reformat summary data
    pivot_longer(cols = everything(),
                 names_to = "Month", 
                 values_to = "Score") %>%
    tidyr::separate(
      Month,
      into = c("AnnualESal", "SummaryStat"),
      sep = "_"
      ) %>%
    pivot_wider(names_from = "SummaryStat", 
                values_from = "Score"),
  # Overall 
  ASalE %>%
    dplyr::select(-PGID) %>%
    pivot_longer(
      cols = everything(),
      values_to = "Value"
      ) %>%
    summarise(
      AnnualESal = "Overall",
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      min  = min(Value, na.rm = TRUE),
      max  = max(Value, na.rm = TRUE)))
#
#
#
# Annual minimum salinity
bind_rows(
  # By month:
  ASalI %>% 
    # Summarize
    summarise(across(where(is.numeric), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = T),
      min = \(x) min(x, na.rm = T),
      max = \(x) max(x, na.rm = T)))) %>%
    # Reformat summary data
    pivot_longer(cols = everything(),
                 names_to = "Month", 
                 values_to = "Score") %>%
    tidyr::separate(
      Month,
      into = c("AnnualISal", "SummaryStat"),
      sep = "_"
    ) %>%
    pivot_wider(names_from = "SummaryStat", 
                values_from = "Score"),
  # Overall 
  ASalI %>%
    dplyr::select(-PGID) %>%
    pivot_longer(
      cols = everything(),
      values_to = "Value"
    ) %>%
    summarise(
      AnnualISal = "Overall",
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      min  = min(Value, na.rm = TRUE),
      max  = max(Value, na.rm = TRUE)))
#
#
#
# May-Oct range salinity
bind_rows(
  # By month:
  ASalR %>% 
    # Summarize
    summarise(across(where(is.numeric), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = T),
      min = \(x) min(x, na.rm = T),
      max = \(x) max(x, na.rm = T)))) %>%
    # Reformat summary data
    pivot_longer(cols = everything(),
                 names_to = "Month", 
                 values_to = "Score") %>%
    tidyr::separate(
      Month,
      into = c("SpwnRSal", "SummaryStat"),
      sep = "_(?=[^_]+$)"
    ) %>%
    pivot_wider(names_from = "SummaryStat", 
                values_from = "Score"),
  # Overall 
  ASalR %>%
    dplyr::select(-PGID) %>%
    pivot_longer(
      cols = everything(),
      values_to = "Value"
    ) %>%
    summarise(
      SpwnRSal = "Overall",
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      min  = min(Value, na.rm = TRUE),
      max  = max(Value, na.rm = TRUE)))
#
#
#
#
# Annual mean temperature
bind_rows(
  # By month:
  ATemE %>% 
    # Summarize
    summarise(across(where(is.numeric), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = T),
      min = \(x) min(x, na.rm = T),
      max = \(x) max(x, na.rm = T)))) %>%
    # Reformat summary data
    pivot_longer(cols = everything(),
                 names_to = "Month", 
                 values_to = "Score") %>%
    tidyr::separate(
      Month,
      into = c("AnnualETem", "SummaryStat"),
      sep = "_"
    ) %>%
    pivot_wider(names_from = "SummaryStat", 
                values_from = "Score"),
  # Overall 
  ATemE %>%
    dplyr::select(-PGID) %>%
    pivot_longer(
      cols = everything(),
      values_to = "Value"
    ) %>%
    summarise(
      AnnualETem = "Overall",
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      min  = min(Value, na.rm = TRUE),
      max  = max(Value, na.rm = TRUE)))
#
#
#
#
# Annual T > 35 temperature
bind_rows(
  # By month:
  ATemA35 %>% 
    # Summarize
    summarise(across(where(is.numeric), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = T),
      min = \(x) min(x, na.rm = T),
      max = \(x) max(x, na.rm = T)))) %>%
    # Reformat summary data
    pivot_longer(cols = everything(),
                 names_to = "Month", 
                 values_to = "Score") %>%
    tidyr::separate(
      Month,
      into = c("AnnualA35Tem", "SummaryStat"),
      sep = "_"
    ) %>%
    pivot_wider(names_from = "SummaryStat", 
                values_from = "Score"),
  # Overall 
  ATemA35 %>%
    dplyr::select(-PGID) %>%
    pivot_longer(
      cols = everything(),
      values_to = "Value"
    ) %>%
    summarise(
      AnnualA35Tem = "Overall",
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      min  = min(Value, na.rm = TRUE),
      max  = max(Value, na.rm = TRUE)))
#
#
#
# May-Oct T < 20 temperature
bind_rows(
  # By month:
  STemB20 %>% 
    # Summarize
    summarise(across(where(is.numeric), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = T),
      min = \(x) min(x, na.rm = T),
      max = \(x) max(x, na.rm = T)))) %>%
    # Reformat summary data
    pivot_longer(cols = everything(),
                 names_to = "Month", 
                 values_to = "Score") %>%
    tidyr::separate(
      Month,
      into = c("SpwnB20Tem", "SummaryStat"),
      sep = "_"
    ) %>%
    pivot_wider(names_from = "SummaryStat", 
                values_from = "Score"),
  # Overall 
  STemB20 %>%
    dplyr::select(-PGID) %>%
    pivot_longer(
      cols = everything(),
      values_to = "Value"
    ) %>%
    summarise(
      SpwnB20Tem = "Overall",
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      min  = min(Value, na.rm = TRUE),
      max  = max(Value, na.rm = TRUE)))
#
#
#
# Assign scores ----
#
### Polygon data 
#
##Oysters
temp <- get(paste0(Site_Code, "_", Version, "_data"))
Oyster_scores <- HSMfunc$assign_oyster_scores(temp)
#
#Oyster reef buffer scores
Oybuffer_scores <- HSMfunc$assign_oybuffer_scores(temp)
#
#Seagrass scores
# Add in Discontinous option
Seagrass <- rbind(Seagrass,
                  c(0.1, "Discontinuous"))
Seagrass_scores <- HSMfunc$assign_seagrass_scores(temp)
#
#
##Channels
#If not NA, then score = 0 (present in buffer zone). If NA then score = 1
Channel_scores <- HSMfunc$assign_buffer_scores(temp)
#
#
#
#
### Interpolations from Arc:
# Annual means
Salinity_scores <- HSMfunc$assign_salinity_scores(temp, Salinity_adult, 
                                                  column_type = "averaged", type = "emsemble")
# Spawning means
Salinity_spawn_scores_t <- HSMfunc$assign_sal_spawn_scores(temp %>% dplyr::select(-c(SspwnRII, SspwnRIO, SspwnRAI, SspwnRAO)), Salinity_adult, 
                                                           column_type = "averaged", type = "emsemble")
Salinity_spawn_scores <- left_join(Salinity_spawn_scores_t, 
                                   HSMfunc$assign_sal_spawn_scores(temp %>% dplyr::select(-c(SspwnRII, SspwnRIO, SspwnRAI, SspwnRAO)), Salinity_larvae, 
                                                                   column_type = "averaged", type = "emsemble") %>% 
                                     st_drop_geometry()) 
#
# Annual means
Temperature_scores <- HSMfunc$assign_temperature_scores(temp, Temperature_adult, 
                                                        column_type = "averaged", type = "emsemble")
#Spawning period
Temperature_spawn_scores_t <- HSMfunc$assign_temperature_spawn_scores(temp, Temperature_adult, 
                                                                      column_type = "averaged", type = "emsemble")
Temperature_spawn_scores <- left_join(Temperature_spawn_scores_t, 
                                      HSMfunc$assign_temperature_spawn_scores(temp, Temperature_larvae, 
                                                                              column_type = "averaged", type = "emsemble") %>% 
                                        st_drop_geometry()) 
# Threshold period - number = proportion above.below the threshold - score is inverse of values
Temperture_thres_scores <- HSMfunc$assign_threshold_scores(temp, column_type = "averaged", type = "emsemble")
#
#
#
#
#
### Interpolations from R, multiple columns needing averaging:
#
# Salinity - all year Mean
Salinity_scores_mean <- HSMfunc$assign_salinity_scores(WI_v1_salMonMean, Salinity_adult, 
                                                  column_type = "individual", 
                                                  individual_key = "e",
                                                  type = "separate")
#
(Salinity_mean_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Salinity_scores_mean,
                                             cols = starts_with("e"),#contains("ens"),
                                             new_column_name = "SAnnueESC",
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Salinity - all year Min
Salinity_scores_min <- HSMfunc$assign_salinity_scores(WI_v1_salMonMin, Salinity_adult, 
                                                  column_type = "individual", 
                                                  individual_key = "e",
                                                  type = "separate")
#
(Salinity_min_scores <- left_join(WI_v1_data %>% dplyr::select(PGID),
                         HSMfunc$row_average(data = Salinity_scores_min,
                                             cols = starts_with("e"),#contains("ens"),
                                             new_column_name = "SAnnueISC",
                                             keep_columns = c("PGID")),
                         by = "PGID"))
#
#
#
# Salinity - spawning period Mean
Salinity_spawn_scores_mean_t <- HSMfunc$assign_sal_spawn_scores(
  WI_v1_salMonMean %>% dplyr::select(PGID, matches("May|Jun|Jul|Aug|Sep|Oct")), 
  Salinity_adult, 
  column_type = "individual",
  individual_key = "e",
  type = "separate")
#
Salinity_spawn_scores_mean <- left_join(Salinity_spawn_scores_mean_t, 
                                        HSMfunc$assign_sal_spawn_scores(WI_v1_salMonMean %>% dplyr::select(PGID, matches("May|Jun|Jul|Aug|Sep|Oct")), 
                                                                        Salinity_larvae, 
                                                                        column_type = "individual",
                                                                        individual_key = "e",
                                                                        type = "separate") %>% 
                                          st_drop_geometry()) 
#
(Salinity_spawn_mean_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Salinity_spawn_scores_mean,
                                             cols = contains("e"),
                                             new_column_name = "SSpwneESC",
                                             keep_columns = c("PGID")), 
                         by = "PGID"))

#
#
#
# Salinity - spawning period Ranges
#Score at Maximum and score at Minimum, Range_score = mean(Max_score, Min_score)
#Adult
Salinity_spawn_scores_range_t <- HSMfunc$assign_sal_spawn_scores(
  WI_v1_salMonRange, 
  Salinity_adult, 
  column_type = "individual",
  individual_key = "e",
  type = "separate")
#
(Salinity_spawn_scores_range_t2 <- Salinity_spawn_scores_range_t %>% 
  # Pivot Maximum & Minimum columns to long format
  pivot_longer(
    cols = matches("^(ens_[A-Za-z]+_(MaximumSC|MinimumSC)|e[A-Za-z]+(ASC|ISC))$"),
    names_to = c("prefix", "month", "type"),
    names_pattern = "^(ens|e)_?([A-Za-z]+)_?(MaximumSC|MinimumSC|ASC|ISC)$",
    values_to = "value"
  ) %>%
    # Rename values if needed
    dplyr::mutate(type = dplyr::case_when(type == "ASC" ~ "MaximumSC",
                                          type == "ISC" ~ "MinimumSC",
                                          TRUE ~ type)) %>%
  # Spread Max/Min into separate columns
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  # Compute difference (absolute) and subtract from 1 for inverse
  mutate(
    avgSC = (MaximumSC + MinimumSC)/2
  ) %>%
  # Pivot back to 1 month per column
  dplyr::select(PGID, prefix, month, avgSC) %>%
  pivot_wider(
    names_from = c(prefix, month),
    values_from = avgSC,
    names_glue = "{prefix}_{month}_avgSC"
  ))
#
#Larvae
Salinity_spawn_scores_range_Lt <- HSMfunc$assign_sal_spawn_scores(
  WI_v1_salMonRange, 
  Salinity_larvae, 
  column_type = "individual",
  individual_key = "e",
  type = "separate")
#
(Salinity_spawn_scores_range_Lt2 <- Salinity_spawn_scores_range_Lt %>% 
    # Pivot Maximum & Minimum columns to long format
    pivot_longer(
      cols = matches("^(ens_[A-Za-z]+_(MaximumSCL|MinimumSCL)|e[A-Za-z]+(ASCL|ISCL))$"),
      names_to = c("prefix", "month", "type"),
      names_pattern = "^(ens|e)_?([A-Za-z]+)_?(MaximumSCL|MinimumSCL|ASCL|ISCL)$",
      values_to = "value"
    ) %>%
    # Rename values if needed
    dplyr::mutate(type = dplyr::case_when(type == "ASCL" ~ "MaximumSCL",
                                          type == "ISCL" ~ "MinimumSCL",
                                          TRUE ~ type)) %>%
    # Spread Max/Min into separate columns
    pivot_wider(
      names_from = type,
      values_from = value
    ) %>%
    # Compute difference (absolute) and subtract from 1 for inverse
    mutate(
      avgSC = (MaximumSCL + MinimumSCL)/2
    ) %>%
    # Pivot back to 1 month per column
    dplyr::select(PGID, prefix, month, avgSC) %>%
    pivot_wider(
      names_from = c(prefix, month),
      values_from = avgSC,
      names_glue = "{prefix}_{month}_avgSCL"
    ))

#
(Salinity_spawn_scores_range <- left_join(Salinity_spawn_scores_range_t2, 
                                         Salinity_spawn_scores_range_Lt2%>% 
                                          st_drop_geometry())) 
#
(Salinity_spawn_range_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Salinity_spawn_scores_range,
                                             cols = contains("e"),
                                             new_column_name = "SSpwneRSC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Temperature - all year Mean
Temperature_scores_t <- HSMfunc$assign_temperature_scores(WI_v1_temMonMean, Temperature_adult, 
                                                         column_type = "individual",
                                                         individual_key = "e",
                                                         type = "separate")
#
(Temperature_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperature_scores_t,
                                             cols = contains("e"),
                                             new_column_name = "TAnnueESC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Temperature - Spawning period Mean
Temperature_spawn_scores_t <- HSMfunc$assign_temperature_spawn_scores(
  WI_v1_temMonMean %>% dplyr::select(PGID, matches("May|Jun|Jul|Aug|Sep|Oct")), 
  Temperature_adult,
  column_type = "individual",
  individual_key = "e",
  type = "separate")
(Temperature_spawn_scores_t2 <- left_join(Temperature_spawn_scores_t, 
                                      HSMfunc$assign_temperature_spawn_scores(
                                        WI_v1_temMonMean %>% dplyr::select(PGID, matches("May|Jun|Jul|Aug|Sep|Oct")),
                                        Temperature_larvae,
                                        column_type = "individual",
                                        individual_key = "e", 
                                        type = "separate") %>% 
                                        st_drop_geometry()))
#
(Temperature_spawn_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperature_spawn_scores_t2,
                                             cols = contains("e"),
                                             new_column_name = "TSpwneESC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Temperature - Threshold period - number = proportion above.below the threshold - score is inverse of values
Temperture_thres_scoresA <- HSMfunc$assign_threshold_scores(WI_v1_temMonT35,
                                                            column_type = "individual",
                                                            individual_key = "e",
                                                            type = "separate")
#
(Temperature_thresA_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperture_thres_scoresA,
                                             cols = contains("e"),
                                             new_column_name = "TAnnueT35SC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))

#
#
Temperture_thres_scoresB <- HSMfunc$assign_threshold_scores(WI_v1_temMonB20,
                                                            column_type = "individual",
                                                            individual_key = "e",
                                                            type = "separate")
#
(Temperature_thresB_scores <- left_join(WI_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperture_thres_scoresB,
                                             cols = contains("e"),
                                             new_column_name = "TSpwneT20SC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
#
### Interpolations single columns, no individual scores before averaging:
#
# Flow data
# Adult optimal
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_adop,
                           cols = contains("Opt"),
                           new_column_name = "FAnnuiAO",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#Adult super/sub
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_adsup,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiAP",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_adsub,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiAB",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae optimal
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_laop,
                           cols = contains("Opt"),
                           new_column_name = "FAnnuiLO",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae super/sub
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_lasup,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiLP",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_lasub,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiLB",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Outlier 1
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_outlier1,
                           cols = contains("Out"),
                           new_column_name = "FAnnui1",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Outlier 2
(WI_v1_data <- left_join(WI_v1_data, 
                         HSMfunc$row_average(
                           data = WI_v1_outlier2,
                           cols = contains("Out"),
                           new_column_name = "FAnnui2",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
Optimal_flow_t <- HSMfunc$assign_flow_scores(WI_v1_data, `Optimal flow`, col_pattern = ".*O$",type = "separate")
Above_flow_t <- HSMfunc$assign_flow_scores(WI_v1_data, `Non-optimal flow`, col_pattern = ".*P$",type = "separate")
Sub_flow_t <- HSMfunc$assign_flow_scores(WI_v1_data, `Non-optimal flow`, col_pattern = ".*B$",type = "separate")
Out1_flow_t <- HSMfunc$assign_flow_scores(WI_v1_data, `Outlier1 flow`, col_pattern = ".*1$",type = "separate")#
Out2_flow_t <- HSMfunc$assign_flow_scores(WI_v1_data, `Outlier2 flow`, col_pattern = ".*2$",type = "separate")#
#
(Flow_scores <- left_join(Optimal_flow_t, st_drop_geometry(Above_flow_t)) %>% 
  left_join(st_drop_geometry(Sub_flow_t)) %>%
  left_join(st_drop_geometry(Out1_flow_t)) %>%
  left_join(st_drop_geometry(Out2_flow_t)))
#
#
#
#
#
#### Figure formatting ####
#
basetheme <- theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold", color = "black", family = "Arial"), 
        axis.text = element_text(size = 11, family = "Arial", color = "black"), 
        axis.text.x = element_text(margin = margin(t=0.25, r=0.5, b=0, l=0.5, unit = "cm")),#unit(c(0.25, 0.5, 0, 0.5), "cm")), 
        axis.text.y = element_text(margin = margin(t=0, r=0.25, b=0, l=0, unit = "cm")), #unit(c(0, 0.25, 0, 0), "cm")),
        axis.ticks = element_line(color = "black", linewidth = 0.1),
        axis.ticks.length = unit(-0.15, "cm"),
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(color = "black", linewidth = 0.1))

papertheme <- theme(
  axis.text = element_text(size = 13),
  axis.title = element_text(size = 14),
  plot.title =  element_blank(), plot.margin = margin(t = 10, r=10)
)
#
base_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title = element_text(size = 17, face = "bold", color = "black", family = "Arial"),
    axis.text = ggplot2::element_text(size = 15, family = "Arial", color = "black"),
    axis.text.x = element_text(margin = margin(t=0.25, r=0.5, b=0, l=0.5, unit = "cm")), #unit(c(0.25, 0.5, 0, 0.5), "cm")), 
    axis.text.y = element_text(margin = margin(t=0, r=0.35, b=0, l=0, unit = "cm")), #unit(c(0, 0.25, 0, 0), "cm")),
    axis.ticks = element_line(color = "black", linewidth = 0.1),
    axis.ticks.length = unit(-0.15, "cm"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.1),
    plot.margin = grid::unit(c(0.05, 0, 0, 0), "cm"),
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5), family = "Arial"),
    plot.caption = ggplot2::element_text(face = "italic", size = 9),
    legend.title = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 10, family = "Arial"))
#
# presentation formatting
maptheme <- theme_classic()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.title = element_blank(),#element_text(size = 14, color = "black"), 
    axis.text =  element_text(size = 15, color = "black", family = "Arial"),
    axis.text.x = element_text(angle = 30, vjust = 0.5)
  )
#
# for plots
plot_theme <- theme(plot.margin = unit(c(0.25, 0.45, 0.1, 0.1), "cm"),
                    panel.border = element_rect(color = NA))
#
legendtheme <- theme(
  legend.title = element_text(size = 14, color = "black", family = "Arial"),
  legend.text = element_text(size = 13, color = "black", family = "Arial"),
  legend.background = element_blank(),
  legend.key = element_blank()
)
#
FacetTheme <- theme(strip.text.y = element_text(face = "bold", size = 12),
                    strip.background = element_rect(fill = "#CCCCCC"),
                    panel.spacing = unit(0.75, "lines"),
                    strip.text.x = element_text(face = "bold", size = 12))

#Manuscript format
manu_theme <- theme_bw()+
  theme(axis.title = element_text(size = 12, face = "bold", color = "black", family = "Arial"), 
        axis.text = element_text(size = 11, family = "Arial"), 
        axis.text.x = element_text(margin = unit(c(0.25, 0.5, 0, 0.5), "cm")), 
        axis.text.y = element_text(margin = unit(c(0, 0.25, 0, 0), "cm")),
        axis.ticks = element_line(color = "black", linewidth = 0.1),
        axis.ticks.length = unit(-0.15, "cm"),
        panel.grid = element_blank(),
        panel.border = element_blank(), 
        axis.line = element_line(color = "black", linewidth = 0.1))
#
#
#
##### Model scoring ####
#
### Add scores to data
assign(paste0(Site_Code, "_", Version, "_scores_data"), HSMfunc$join_score_dataframes(temp))
assign(
  paste0(Site_Code, "_", Version, "_scores_data"),
  get(paste0(Site_Code, "_", Version, "_scores_data")) %>%
    rename(Long_DD_X = any_of("Ln_DD_X"),
           Lat_DD_Y = any_of("Lt_DD_Y"),
           State_Ref = any_of("Stat_Rf"),
           Ref_Region = any_of("Ref_Rgn"),
           FWC_Region = any_of("FWC_Rgn"), 
           StatePlane = any_of("StatPln"),
           UTM_Zone = any_of("UTM_Zon"),
           KML_Name = any_of("KML_Nam"),
           LongName = any_of("LongNam"),
           Section_Name = any_of("Sctn_Nm"))
)
#
## df of just scores (no raw data values)
assign(paste0(Site_Code, "_", Version, "_scores_only"),  get(paste0(Site_Code, "_", Version, "_scores_data")) %>% 
         dplyr::select(PGID, Lat_DD_Y, Long_DD_X, ends_with("SC"), ends_with("SCL")))
#
## Set working with "all" data or with just "scores" data:
model_data <- c("scores")
#
### Calculate total HSM score
if(model_data == "all"){
  assign(paste0(Site_Code, "_", Version, "_data_totals"), HSMfunc$calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_data")), nyears = 5))
} else {
  assign(paste0(Site_Code, "_", Version, "_data_totals"), HSMfunc$calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_only")), nyears = 5))
}
#
# Clean model data frame
(assign(paste0(Site_Code, "_", Version, "_data_clean"), HSMfunc$clean_model_data(get(paste0(Site_Code, "_", Version, "_data_totals")))))
#
#
# Additive model:
## Flow as exclusionary when 0:
HSM_data <- get(paste0(Site_Code, "_", Version, "_data_clean")) %>% 
  st_drop_geometry() %>% 
  {
    av_data <- dplyr::select(., ends_with("AV") & !matches("^FAV$"))
    
    # Keep only AV columns with at least one real (non-NA/NaN) value
    valid_av <- av_data[, colSums(!is.na(av_data)) > 0, drop = FALSE]
    
    CurveCO_val <- ncol(valid_av)
    
    mutate(.,
           HSM_f = case_when(
             ChnlTO == 1 & FAV == 0 ~ 0,  # HSM = 0 when Flow is 0
             ChnlTO == 1 ~ (rowSums(valid_av, na.rm = TRUE) / CurveCO_val) * FAV,
             ChnlTO == 0 ~ 0, 
             TRUE ~ NA_real_
           )) %>%
      mutate(HSMround_f = round(HSM_f, 2),
             Curve_val = ncol(valid_av))
  }
#
# Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#
# Determine natural Jenks breaks (thirds)
set.seed(54321)
vals <- sample(HSM_data$HSM_f, min(20000, length(HSM_data$HSM_f))) #Sample then calculate breaks
jenks_breaks <- classInt::classIntervals(vals, n = 4, style = "jenks")$brks#getJenksBreaks(var = HSM_data$HSM, k = 4)
#
# Clean breaks then make sure they cover full data range:
jenks_breaks <- sort(unique(
  signif(jenks_breaks, 6)
))
jenks_breaks[c(1, length(jenks_breaks))] <-
  range(HSM_data$HSM_f, na.rm = TRUE)
jenks_breaks
#
# Assign groups using cut()
HSM_data_grps <- HSM_data %>%
  mutate(
    # HSM 0.1 groups
    HSMgrp = case_when(
      HSMround_f < 0.1 & HSMround_f >= 0 ~ "[0,0.1)",
      HSMround_f < 0.2 & HSMround_f >= 0.1 ~ "[0.1,0.2)",
      HSMround_f < 0.3 & HSMround_f >= 0.2 ~ "[0.2,0.3)",
      HSMround_f < 0.4 & HSMround_f >= 0.3 ~ "[0.3,0.4)",
      HSMround_f < 0.5 & HSMround_f >= 0.4 ~ "[0.4,0.5)",
      HSMround_f < 0.6 & HSMround_f >= 0.5 ~ "[0.5,0.6)",
      HSMround_f < 0.7 & HSMround_f >= 0.6 ~ "[0.6,0.7)",
      HSMround_f < 0.8 & HSMround_f >= 0.7 ~ "[0.7,0.8)",
      HSMround_f < 0.9 & HSMround_f >= 0.8 ~ "[0.8,0.9)",
      TRUE           ~ "[0.9,1]"
    ),
    # Aggregated bins
    HSMgyr = case_when(
      HSMgrp == "0" ~ "0",
      HSMround_f < 0.4 ~ "Low",
      HSMround_f < 0.6 ~ "Moderate",
      TRUE           ~ "High"
    ),
    # Jenks breaks
    HSMjb = cut(HSM_f,
                breaks = jenks_breaks,
                include.lowest = TRUE,
                labels = c("Low", "Medium", "High")),
    # Quantiles
    HSM_q4 = factor(
      ntile(HSM_f, 4),
      levels = 1:4,
      labels = c("Least", "Low", "Moderate", "Most"))
  ) %>%
  #Make sure grp is factors
  mutate(
    HSMgrp = factor(
      HSMgrp,
      levels = c(
        "[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
        "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
        "[0.8,0.9)", "[0.9,1]"
      )
    ),
    HSMgyr = factor(
      HSMgyr,
      levels = c("0", "Low", "Moderate", "High")
    ),
    HSMjb = factor(
      HSMjb,
      levels = c("Low", "Medium", "High")
    )
  )
#
head(HSM_data_grps)
#
#
#
#### Summary and mapping ####
#
## Parameter summary:
(Scoring_summ <- HSM_data %>% 
  # Select columns
  dplyr::select(PGID, contains("SC")) %>%
  # Get summary info
  summarise(across(where(is.numeric), list(
    mean = \(x) mean(x, na.rm = TRUE),
    sd = \(x) sd(x, na.rm = T),
    min = \(x) min(x, na.rm = T),
    max = \(x) max(x, na.rm = T)))) %>%
  # Reformat summary data
  pivot_longer(cols = everything(),
               names_to = "Column", 
               values_to = "Score") %>%
  tidyr::separate(
    Column,
    into = c("Parameter", "SummaryStat"),
    sep = "_"
  ) %>%
  pivot_wider(names_from = "SummaryStat", 
              values_from = "Score"))
#
write_xlsx(Scoring_summ, 
           paste0(Site_Code, "_", Version,"/Output/Scoring_summary_",Sys.Date(),".xlsx"), 
           format_headers = TRUE)
# 
(Suit_summ <- HSM_data %>% 
  dplyr::select(PGID, contains("SC")) %>%
  # Reorganize data to add Suitability Group info:
  pivot_longer(
    cols = -PGID,
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    Group = case_when(
      Value >= 0.6 & Value <= 1   ~ "High",
      Value >= 0.4 & Value < 0.6  ~ "Moderate",
      Value >= 0   & Value < 0.4  ~ "Low",
      TRUE                        ~ NA_character_
    )
  ) %>%
  # Get counts and percentages per group*column
  count(Parameter, Group) %>%
  group_by(Parameter) %>%
  mutate(
    Percent = 100 * n / sum(n)
  ) %>%
  ungroup() %>%
  # Nicer output format
  pivot_wider(
    names_from = Group,
    values_from = c(n, Percent),
    names_glue = "{Group}_{.value}",
    values_fill = list(n = 0, Percent = 0)
  ) %>%
  dplyr::select(
    Parameter,
    High_n, High_Percent,
    Moderate_n, Moderate_Percent,
    Low_n, Low_Percent
  ))
#
write_xlsx(Suit_summ, 
           paste0(Site_Code, "_", Version,"/Output/Suitability_summary_",Sys.Date(),".xlsx"), 
           format_headers = TRUE)
#
## Model summary:
#
HSM_data_grps %>% 
  #mutate(HSM_r = round(HSMround, 1)) %>%
  group_by(HSMgrp) %>%
  summarise(n())

 ggplot(HSM_data_grps, aes(x = HSMgrp)) +
  geom_histogram(stat = "count", fill = "gray50", color = "black") +
  labs(
    title = "HSM scores",
    x = "Suitability score",
    y = "Count"
  ) +
  basetheme + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 1000000), breaks = seq(0, 1500000, 250000))+ #120000, 3000000
  scale_x_discrete(expand = c(0.005,0))+
  theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
  papertheme + theme(axis.text.x = element_text(size = 11, angle = 20))
### SAVE PLOT: SiteCode_version_HSMscores_hist - ~850 * auto 600 paper
#
summary(HSM_data_grps$HSMgrp)
summary(HSM_data_grps$HSMgyr)
summary(HSM_data_grps$HSMjb)
#Jenks breaks summary:
table(
  cut(HSM_data$HSM_f, breaks = jenks_breaks, include.lowest = TRUE),
  useNA = "ifany"
)
jenks.tests(classIntervals(HSM_data$HSM_f, style = "fixed", fixedBreaks = jenks_breaks))
#
#hist(HSM_data$HSM, col = "gray90", main = "Jenks Breakpoints Overlay", xlab = "HSM score")
#abline(v = jenks_breaks, col = "red", lwd = 2, lty = 2)
#text(x = jenks_breaks, y = 59500, labels = round(jenks_breaks, 2), pos = 4, col = "red", cex = 1.15)
#SL: -15000 - repel; ylim - 60000
#SS: -250000 - repel; ylim - 2500000
ggplot(HSM_data, aes(x = HSM_f)) +
  geom_histogram(fill = "gray50", color = "black", bins = 30, boundary = 0) +
  geom_vline(xintercept = jenks_breaks, linetype = "dashed", linewidth = 1, color = "red") +
  ggrepel::geom_text_repel(data = data.frame(x = jenks_breaks, y = max(hist(HSM_data$HSM_f, plot = FALSE)$counts-250000)), #250000
                           aes(x = x, y = y, label = round(x, 2)), color = "red", angle = 0, direction = "y", 
                           nudge_y = max(hist(HSM_data$HSM_f, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                           segment.color = NA)+
  #annotate("text", x = jenks_breaks, y = 0, label = round(jenks_breaks, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
  labs(
    title = "Jenks Breakpoints Overlay",
    x = "HSM score",
    y = "Count"
  ) +
  basetheme + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 750000), breaks = seq(0, 750000, 250000)) + #60000
  scale_x_continuous(expand = c(0.005,0), breaks = seq(0, 1, by = 0.1), limits = c(0, 1))+
  theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
  papertheme
### SAVE PLOT: SiteCode_version_HSMjb_hist - ~850 * auto 500 paper
#
summary(HSM_data_grps$HSM_q4)
#
(temp_cuts <- HSM_data_grps %>%
    group_by(HSM_q4) %>%
    summarise(
      n = n(),
      min = min(HSM_f, na.rm = TRUE),
      max = max(HSM_f, na.rm = TRUE),
      mean = mean(HSM_f, na.rm = TRUE),
      .groups = "drop"
    ))
#
#SL: -15000 - repel; ylim - 60000
#SS: -350000 - repel; ylim - 2500000
ggplot(HSM_data, aes(HSM_f)) +
  geom_histogram(bins = 30, fill = "grey50", color = "black", boundary = 0) +
  geom_vline(data = temp_cuts, aes(xintercept = min), linetype = "dashed", linewidth = 1, color = "red") +
  ggrepel::geom_text_repel(data = data.frame(x = temp_cuts$min, y = max(hist(HSM_data$HSM_f, plot = FALSE)$counts-250000)), 
                           aes(x = x, y = y, label = round(x, 3)), color = "red", angle = 0, direction = "y", 
                           nudge_y = max(hist(HSM_data$HSM_f, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.35,
                           segment.color = NA)+
  #annotate("text", x = temp_cuts$min, y = 0, label = round(temp_cuts$min, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
  labs(
    title = "Quartile Bins Overlay",
    x = "HSM score",
    y = "Count"
  ) +
  basetheme + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 750000), breaks = seq(0, 750000, 250000)) +
  scale_x_continuous(expand = c(0.005,0), breaks = seq(0, 1, by = 0.1), limits = c(0,1))+
  theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"))+
  papertheme
#
### SAVE PLOT: SiteCode_version_HSMq4_hist - ~850 * auto 500 paper
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
HSMfunc$plot_model_map(HSM_spdf, "HSMgrp") #SiteCode_Version_HSM_scores Output/Map files - 950
HSMfunc$plot_model_map(HSM_spdf, "HSMjb") #SiteCode_Version_HSM_jb Output/Map files
HSMfunc$plot_model_map(HSM_spdf, "HSM_q4") #SiteCode_Version_HSM_q4 Output/Map files
#
#
# Parameter groups summary:
Scoring_summ %>%
  group_by(Param = substr(Parameter, 1, 4)) %>%
  summarise(meanVal = mean(mean, na.rm = T),
            sdVal = sd(mean, na.rm = T))
# Flow
Scoring_summ %>%
  filter(substr(Parameter,1, 4) == "FAnn") %>%
  group_by(Param = substr(Parameter, 1, 7)) %>%
  summarise(meanVal = mean(mean, na.rm = T),
            sdVal = sd(mean, na.rm = T))
#
#
#
#

#### Final model output ----
#
# Limit to HSM, HSMround, and HSMgrp of best model
# Make sure HSMgrp, HSMgyr, HSMjb, and HSM_q4 exists
(final_data_raw <- HSM_data_grps %>% dplyr::select(PGID, Lat_DD_Y, Long_DD_X, 
                                              contains("SC"), contains("AV"), ChnlTO, Curve_val,
                                              HSM_f, HSMround_f, HSMgrp, HSMgyr, HSMjb, HSM_q4))
#
# Make sure object is sfc
final_data_raw <- left_join(HSM_spdf %>% dplyr::select(PGID, Lat_DD_Y, Long_DD_X), 
                            final_data_raw)
#
# Limit model cells to aquatic area (remove cells completely covered by land)
# Make sure same CRS
# Load land:
FL_outline <- st_read("Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
FL_outline <- st_transform(FL_outline, st_crs(final_data_raw))
#
# Determine HSM polygons covered by land polygons
covered_mat <- st_covered_by(final_data_raw, FL_outline, sparse = FALSE)
#
# Identify rows where polygon is covered by ANY land polygon
covered_any <- apply(covered_mat, 1, any)
#
# Keep only those NOT fully covered
final_data <- final_data_raw[!covered_any, ]
#
# Check final area coverage
ggplot()+
  geom_sf(data = final_data, aes(color = HSM_f))+
  scale_color_viridis_c(limits = c(0,1))
 #
#
#
summary(final_data$HSMgrp) %>%
  as.data.frame() %>%
  mutate(Pct = round((./nrow(final_data))*100,2))
#
final_data %>% st_drop_geometry() %>%
  group_by(HSMgyr) %>%
  summarise(
    n = n(),
    min = min(HSM_f, na.rm = TRUE),
    max = max(HSM_f, na.rm = TRUE),
    mean = mean(HSM_f, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Pct = round((n/nrow(final_data))*100,2))
#
#
#
#
#Jenks breaks summary:
table(
  cut(final_data$HSM_f, breaks = jenks_breaks, include.lowest = TRUE),
  useNA = "ifany"
)
jenks.tests(classIntervals(final_data$HSM_f, style = "fixed", fixedBreaks = jenks_breaks))
#
(jb_plot <- ggplot(final_data, aes(x = HSM_f)) +
    geom_histogram(fill = "gray50", color = "black", bins = 30,  center = 0.05) +
    geom_vline(xintercept = jenks_breaks, linetype = "dashed", linewidth = 1, color = "red") +
    ggrepel::geom_text_repel(data = data.frame(x = jenks_breaks, y = max(hist(final_data$HSM_f, plot = FALSE)$counts-100000)), #-300000, 1000
                             aes(x = x, y = y, label = round(x, 2)), color = "red", angle = 0, direction = "y", 
                             nudge_y = max(hist(final_data$HSM_f, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                             segment.color = NA)+
    #annotate("text", x = jenks_breaks, y = 0, label = round(jenks_breaks, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
    labs(
      title = "Jenks Breakpoints Overlay",
      x = "HSM score",
      y = "Count"
    ) +
    base_theme + plot_theme +
   scale_y_continuous(expand = c(0,0), limits = c(0, 750000), breaks = seq(0, 750000, 250000))+ #1250000, 20000 
    scale_x_continuous(limits = c(0,1), expand = c(0,0.0025)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_final_jb_hist.png"),
  plot = jb_plot,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
summary(final_data$HSM_q4)
(temp_cuts <- final_data %>%
    st_drop_geometry() %>%
    group_by(HSM_q4) %>%
    summarise(
      n = n(),
      min = min(HSM_f, na.rm = TRUE),
      max = max(HSM_f, na.rm = TRUE),
      mean = mean(HSM_f, na.rm = TRUE),
      .groups = "drop"
    ))
#
(q4_plot <- ggplot(final_data, aes(HSM_f)) +
    geom_histogram(bins = 40, fill = "grey50", color = "black") +
    geom_vline(data = temp_cuts, aes(xintercept = min), linetype = "dashed", linewidth = 1, color = "red") +
    ggrepel::geom_text_repel(data = data.frame(x = temp_cuts$min, y = max(hist(final_data$HSM_f, plot = FALSE)$counts)-100000), #300000, 1000
                             aes(x = x, y = y, label = round(x, 3)), color = "red", angle = 0, direction = "y", 
                             nudge_y = max(hist(final_data$HSM_f, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                             segment.color = NA)+
    #annotate("text", x = temp_cuts$min, y = 0, label = round(temp_cuts$min, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
    labs(
      title = "Quartile Bins Overlay",
      x = "HSM score",
      y = "Count"
    ) +
    base_theme + plot_theme +
    scale_y_continuous(expand = c(0,0), limits = c(0, 750000), breaks = seq(0, 750000, 250000))+ #1250000, 20000
    scale_x_continuous(limits = c(0, 1.0), expand = c(0,0.0015)))
#
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_final_q4hist.png"),
  plot = q4_plot,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
#
HSMfuncGT <- new.env()
source("HSM code/Functions/HSM_gt_model_functions.R", local = HSMfuncGT)
#
HSMfuncGT$save_final_model_output(data = final_data, output_type = "all")
#
#Output of just model scores
HSMfuncGT$save_model_scores(data = final_data)
#
## Once saved, code #5 for maps of data and model output.
#
#
#