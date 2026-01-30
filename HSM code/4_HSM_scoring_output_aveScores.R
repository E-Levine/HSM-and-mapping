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
#
#
#
#
# Add and clean interp data ----
#
# Add interp data: one call per data column/type
#
#Annual mean salinity
(SL_v1_salMonMean <- HSMfunc$add_excel_columns_sf(
  existing_sf = SL_v1_data,
  excel_path = paste0(Site_Code,"_",Version,"/Output/Data files/Salinity_Monthly_Mean_2020_2024.xlsx"),
  join_by = "PGID",
  excel_columns = contains("ens"),
  sheet = 1,
  join_type = "left"
))
#
#
(SS_v1_salMonMean <- HSMfunc$read_data_files_csv(Site_Code, 
                                                Version, 
                                                data_subdir = "Salinity_Monthly_Means_2020_2024") %>%
  as.data.frame())
#
#
#
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
(SS_v1_salMonMin <- HSMfunc$read_data_files_csv(Site_Code, 
                                               Version, 
                                               data_subdir = "Salinity_Monthly_Mins_2020_2024") %>%
  as.data.frame())
#
#
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
(SS_v1_salMonRange <- HSMfunc$read_data_files_csv(Site_Code, 
                                                 Version, 
                                                 data_subdir = "Salinity_Monthly_Range_2020_2024") %>%
  as.data.frame())
#
#
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
(SS_v1_temMonMean <- HSMfunc$read_data_files_csv(Site_Code, 
                                                Version, 
                                                data_subdir = "Temperature, water_Monthly_Means_2020_2024") %>%
  as.data.frame())
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
(SS_v1_temMonT35 <- HSMfunc$read_data_files_csv(Site_Code, 
                                               Version, 
                                               data_subdir = "Temperature, water_Monthly_ThresholdA35_2020_2024") %>%
  as.data.frame())
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
(SS_v1_outlier1 <- HSMfunc$read_data_files_csv(Site_Code, 
                                              Version, 
                                              data_subdir = "SS_flow_outlier1") %>%
  as.data.frame())
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
(SS_v1_outlier2 <- HSMfunc$read_data_files_csv(Site_Code, 
                                              Version, 
                                              data_subdir = "SS_flow_outlier2") %>%
  as.data.frame())
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
(SS_v1_adop <- HSMfunc$read_data_files_csv(Site_Code, 
                                          Version, 
                                          data_subdir = "SS_flow_optimal_adult") %>%
  as.data.frame())
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
(SS_v1_laop <- HSMfunc$read_data_files_csv(Site_Code, 
                                          Version, 
                                          data_subdir = "SS_flow_optimal_larvae") %>%
  as.data.frame())
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
(SS_v1_adsup <- HSMfunc$read_data_files_csv(Site_Code, 
                                           Version, 
                                           data_subdir = "SS_flow_super_adult") %>%
  as.data.frame())
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
(SS_v1_adsub <- HSMfunc$read_data_files_csv(Site_Code, 
                                           Version, 
                                           data_subdir = "SS_flow_sub_adult") %>%
  as.data.frame())
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
(SS_v1_lasup <- HSMfunc$read_data_files_csv(Site_Code, 
                                           Version, 
                                           data_subdir = "SS_flow_super_larvae") %>%
  as.data.frame())
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
(SS_v1_lasub <- HSMfunc$read_data_files_csv(Site_Code, 
                                           Version, 
                                           data_subdir = "SS_flow_sub_larvae") %>%
  as.data.frame())
#
#
#
rm(datafiles)
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
### Interpolations multiple columns needing averaging:
#
# Salinity - all year Mean
Salinity_scores_mean <- HSMfunc$assign_salinity_scores(SL_v1_salMonMean, Salinity_adult, 
                                                  column_type = "individual", 
                                                  individual_key = "ens",
                                                  type = "separate")
#
(Salinity_mean_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Salinity_scores_mean,
                                             cols = contains("ens"),
                                             new_column_name = "SAnnueESC",
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Salinity - all year Min
Salinity_scores_min <- HSMfunc$assign_salinity_scores(SL_v1_salMonMin, Salinity_adult, 
                                                  column_type = "individual", 
                                                  individual_key = "ens",
                                                  type = "separate")
#
(Salinity_min_scores <- left_join(SL_v1_data %>% dplyr::select(PGID),
                         HSMfunc$row_average(data = Salinity_scores_min,
                                             cols = contains("ens"),
                                             new_column_name = "SAnnueISC",
                                             keep_columns = c("PGID")),
                         by = "PGID"))
#
#
#
# Salinity - spawning period Mean
Salinity_spawn_scores_mean_t <- HSMfunc$assign_sal_spawn_scores(
  SL_v1_salMonMean %>% dplyr::select(PGID:Long_DD_X, matches("May|Jun|Jul|Aug|Sep|Oct")), 
  Salinity_adult, 
  column_type = "individual",
  individual_key = "ens",
  type = "separate")
#
Salinity_spawn_scores_mean <- left_join(Salinity_spawn_scores_mean_t, 
                                        HSMfunc$assign_sal_spawn_scores(SL_v1_salMonMean %>% dplyr::select(PGID:Long_DD_X, matches("May|Jun|Jul|Aug|Sep|Oct")), 
                                                                        Salinity_larvae, 
                                                                        column_type = "individual",
                                                                        individual_key = "ens",
                                                                        type = "separate") %>% 
                                          st_drop_geometry()) 
#
(Salinity_spawn_mean_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Salinity_spawn_scores_mean,
                                             cols = contains("ens"),
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
  SL_v1_salMonRange, 
  Salinity_adult, 
  column_type = "individual",
  individual_key = "ens",
  type = "separate")
#
(Salinity_spawn_scores_range_t2 <- Salinity_spawn_scores_range_t %>% 
  # Pivot Maximum & Minimum columns to long format
  pivot_longer(
    cols = matches("^ens_[A-Za-z]+_(MaximumSC|MinimumSC)$"),
    names_to = c("prefix", "month", "type"),
    names_pattern = "(ens)_(.*)_(MaximumSC|MinimumSC)",
    values_to = "value"
  ) %>%
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
  SL_v1_salMonRange, 
  Salinity_larvae, 
  column_type = "individual",
  individual_key = "ens",
  type = "separate")
#
(Salinity_spawn_scores_range_Lt2 <- Salinity_spawn_scores_range_Lt %>% 
    # Pivot Maximum & Minimum columns to long format
    pivot_longer(
      cols = matches("^ens_[A-Za-z]+_(MaximumSCL|MinimumSCL)$"),
      names_to = c("prefix", "month", "type"),
      names_pattern = "(ens)_(.*)_(MaximumSCL|MinimumSCL)",
      values_to = "value"
    ) %>%
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
(Salinity_spawn_range_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Salinity_spawn_scores_range,
                                             cols = contains("ens"),
                                             new_column_name = "SSpwneRSC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Temperature - all year Mean
Temperature_scores_t <- HSMfunc$assign_temperature_scores(SL_v1_temMonMean, Temperature_adult, 
                                                         column_type = "individual",
                                                         individual_key = "ens",
                                                         type = "separate")
#
(Temperature_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperature_scores_t,
                                             cols = contains("ens"),
                                             new_column_name = "TAnnueESC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Temperature - Spawning period Mean
Temperature_spawn_scores_t <- HSMfunc$assign_temperature_spawn_scores(
  SL_v1_temMonMean %>% dplyr::select(PGID:Long_DD_X, matches("May|Jun|Jul|Aug|Sep|Oct")), 
  Temperature_adult,
  column_type = "individual",
  individual_key = "ens",
  type = "separate")
(Temperature_spawn_scores_t2 <- left_join(Temperature_spawn_scores_t, 
                                      HSMfunc$assign_temperature_spawn_scores(
                                        SL_v1_temMonMean %>% dplyr::select(PGID:Long_DD_X, matches("May|Jun|Jul|Aug|Sep|Oct")),
                                        Temperature_larvae,
                                        column_type = "individual",
                                        individual_key = "ens", 
                                        type = "separate") %>% 
                                        st_drop_geometry()))
#
(Temperature_spawn_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperature_spawn_scores_t2,
                                             cols = contains("ens"),
                                             new_column_name = "TSpwneESC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))
#
#
#
# Temperature - Threshold period - number = proportion above.below the threshold - score is inverse of values
Temperture_thres_scoresA <- HSMfunc$assign_threshold_scores(SL_v1_temMonT35,
                                                            column_type = "individual",
                                                            individual_key = "ens",
                                                            type = "separate")
#
(Temperature_thresA_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperture_thres_scoresA,
                                             cols = contains("ens"),
                                             new_column_name = "TAnnueT35SC", 
                                             keep_columns = c("PGID")), 
                         by = "PGID"))

#
#
Temperture_thres_scoresB <- HSMfunc$assign_threshold_scores(SL_v1_temMonB20,
                                                            column_type = "individual",
                                                            individual_key = "ens",
                                                            type = "separate")
#
(Temperature_thresB_scores <- left_join(SL_v1_data %>% dplyr::select(PGID), 
                         HSMfunc$row_average(data = Temperture_thres_scoresB,
                                             cols = contains("ens"),
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
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_adop,
                           cols = contains("Optimal"),
                           new_column_name = "FAnnuiAO",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#Adult super/sub
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_adsup,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiAP",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_adsub,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiAB",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae optimal
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_laop,
                           cols = contains("Optimal"),
                           new_column_name = "FAnnuiLO",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Larvae super/sub
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_lasup,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiLP",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_lasub,
                           cols = contains("Days"),
                           new_column_name = "FAnnuiLB",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Outlier 1
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_outlier1,
                           cols = contains("Out"),
                           new_column_name = "FAnnui1",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
# Outlier 2
(SL_v1_data <- left_join(SL_v1_data, 
                         HSMfunc$row_average(
                           data = SL_v1_outlier2,
                           cols = contains("Out"),
                           new_column_name = "FAnnui2",
                           keep_columns = c("PGID")
                         ),
                         by = "PGID"))
#
Optimal_flow_t <- HSMfunc$assign_flow_scores(SL_v1_data, `Optimal flow`, col_pattern = ".*O$",type = "separate")
Above_flow_t <- HSMfunc$assign_flow_scores(SL_v1_data, `Non-optimal flow`, col_pattern = ".*P$",type = "separate")
Sub_flow_t <- HSMfunc$assign_flow_scores(SL_v1_data, `Non-optimal flow`, col_pattern = ".*B$",type = "separate")
Out1_flow_t <- HSMfunc$assign_flow_scores(SL_v1_data, `Outlier1 flow`, col_pattern = ".*1$",type = "separate")#
Out2_flow_t <- HSMfunc$assign_flow_scores(SL_v1_data, `Outlier2 flow`, col_pattern = ".*2$",type = "separate")#
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
#
## df of jusct scores (no raw data values)
assign(paste0(Site_Code, "_", Version, "_scores_only"),  get(paste0(Site_Code, "_", Version, "_scores_data")) %>% 
         dplyr::select(PGID, Lat_DD_Y, Long_DD_X, ends_with("SC"), ends_with("SCL")))
#
## Set working with "all" data or with just "scores" data:
model_data <- c("scores")
#
### Calculate total HSM score
if(model_data == "all"){
  assign(paste0(Site_Code, "_", Version, "_data_totals"), HSMfunc$calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_data"))))
} else {
  assign(paste0(Site_Code, "_", Version, "_data_totals"), HSMfunc$calculate_totals(get(paste0(Site_Code, "_", Version, "_scores_only"))))
}
#
# Clean model data frame
(assign(paste0(Site_Code, "_", Version, "_data_clean"), HSMfunc$clean_model_data(get(paste0(Site_Code, "_", Version, "_data_totals")))))
#
#
#
HSM_data <- get(paste0(Site_Code, "_", Version, "_data_clean")) %>% 
  st_drop_geometry() %>% 
  mutate(CurveCO = sum(grepl("AV$", names(st_drop_geometry(get(paste0(Site_Code, "_", Version, "_data_clean"))))))) %>% 
  mutate(HSM = case_when(ChnlTO == 1 ~ (OystAV + BuffAV + SgrsAV + SAV + TAV + FAV)/CurveCO,
                         ChnlTO == 0 ~ 0, 
                         TRUE ~ NA_real_)) %>%
  mutate(HSMround = round(HSM, 2))
#
# Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#
# Determine natural Jenks breaks (thirds)
set.seed(54321)
vals <- sample(HSM_data$HSM, min(20000, length(HSM_data$HSM))) #Sample then calculate breaks
jenks_breaks <- classInt::classIntervals(vals, n = 3, style = "jenks")$brks#getJenksBreaks(var = HSM_data$HSM, k = 4)
#
# Clean breaks then make sure they cover full data range:
jenks_breaks <- sort(unique(
  signif(jenks_breaks, 6)
))
jenks_breaks[c(1, length(jenks_breaks))] <-
  range(HSM_data$HSM, na.rm = TRUE)
#
# Assign groups using cut()
HSM_data_grps <- HSM_data %>%
  mutate(
    # HSM 0.1 groups
    HSMgrp = case_when(
      HSMround == 0 ~ "0",
      HSMround < 0.1 ~ "(0,0.1)",
      HSMround < 0.2 ~ "[0.1,0.2)",
      HSMround < 0.3 ~ "[0.2,0.3)",
      HSMround < 0.4 ~ "[0.3,0.4)",
      HSMround < 0.5 ~ "[0.4,0.5)",
      HSMround < 0.6 ~ "[0.5,0.6)",
      HSMround < 0.7 ~ "[0.6,0.7)",
      HSMround < 0.8 ~ "[0.7,0.8)",
      HSMround < 0.9 ~ "[0.8,0.9)",
      TRUE           ~ "[0.9,1]"
    ),
    # Aggregated bins
    HSMgyr = case_when(
      HSMgrp == "0" ~ "0",
      HSMround < 0.4 ~ "Low",
      HSMround < 0.6 ~ "Moderate",
      TRUE           ~ "High"
    ),
    # Jenks breaks
    HSMjb = cut(HSM,
                breaks = jenks_breaks,
                include.lowest = TRUE,
                labels = c("Low", "Medium", "High")),
    # Quantiles
    HSM_q4 = factor(
      ntile(HSM, 4),
      levels = 1:4,
      labels = c("Least", "Low", "Moderate", "Most"))
  ) %>%
  #Make sure grp is factors
  mutate(
    HSMgrp = factor(
      HSMgrp,
      levels = c(
        "0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
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
summary(HSM_data_grps$HSMgrp)
summary(HSM_data_grps$HSMgyr)
summary(HSM_data_grps$HSMjb)
#Jenks breaks summary:
table(
  cut(HSM_data$HSM, breaks = jenks_breaks, include.lowest = TRUE),
  useNA = "ifany"
)
jenks.tests(classIntervals(HSM_data$HSM, style = "fixed", fixedBreaks = jenks_breaks))
#
#hist(HSM_data$HSM, col = "gray90", main = "Jenks Breakpoints Overlay", xlab = "HSM score")
#abline(v = jenks_breaks, col = "red", lwd = 2, lty = 2)
#text(x = jenks_breaks, y = 59500, labels = round(jenks_breaks, 2), pos = 4, col = "red", cex = 1.15)
ggplot(HSM_data, aes(x = HSM)) +
  geom_histogram(fill = "gray90", color = "black", bins = 30) +
  geom_vline(xintercept = jenks_breaks, linetype = "dashed", linewidth = 1, color = "red") +
  ggrepel::geom_text_repel(data = data.frame(x = jenks_breaks, y = max(hist(HSM_data$HSM, plot = FALSE)$counts)), 
                           aes(x = x, y = y, label = round(x, 2)), color = "red", angle = 0, direction = "y", 
                           nudge_y = max(hist(HSM_data$HSM, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                           segment.color = NA)+
  #annotate("text", x = jenks_breaks, y = 0, label = round(jenks_breaks, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
  labs(
    title = "Jenks Breakpoints Overlay",
    x = "HSM score",
    y = "Count"
  ) +
  basetheme + scale_y_continuous(expand = c(0,0))
### SAVE PLOT: SiteCode_version_HSMjb_hist - ~850 * auto
#
summary(HSM_data_grps$HSM_q4)
#
(temp_cuts <- HSM_data_grps %>%
    group_by(HSM_q4) %>%
    summarise(
      n = n(),
      min = min(HSM, na.rm = TRUE),
      max = max(HSM, na.rm = TRUE),
      mean = mean(HSM, na.rm = TRUE),
      .groups = "drop"
    ))
#
ggplot(HSM_data, aes(HSM)) +
  geom_histogram(bins = 40, fill = "grey80", color = "grey40") +
  geom_vline(data = temp_cuts, aes(xintercept = min), linetype = "dashed", linewidth = 1, color = "red") +
  ggrepel::geom_text_repel(data = data.frame(x = temp_cuts$min, y = max(hist(HSM_data$HSM, plot = FALSE)$counts)), 
                           aes(x = x, y = y, label = round(x, 3)), color = "red", angle = 0, direction = "y", 
                           nudge_y = max(hist(HSM_data$HSM, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                           segment.color = NA)+
  #annotate("text", x = temp_cuts$min, y = 0, label = round(temp_cuts$min, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
  labs(
    title = "Quartile Bins Overlay",
    x = "HSM score",
    y = "Count"
  ) +
  basetheme + scale_y_continuous(expand = c(0,0))
#
### SAVE PLOT: SiteCode_version_HSMq4_hist - ~850 * auto
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