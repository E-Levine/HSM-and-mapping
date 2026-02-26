##Flow and salinity data 
#
#Code to relate flow (cfs) to logger salinity data
#
#Files should be named at minimum: SiteCode_logger_[salinity|flow].
#
## Packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, data.table,#Df manipulation, basic summary
               readxl, openxlsx, progress, writexl,
               slider, minpack.lm, #rolling mean
               sf, sp, terra, furrr, future,
               mgcv, fpc, fields, interp, #interpolation, fpc::bscan - clustering
               geospehere, igraph, leaflet, #Cluster points
               RColorBrewer, magicfor, ecorest, #HSV scoring
               gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
#
Site_code <- c("WI")       #Two letter estuary code
Version <- c("v1")         #For saving plots
Start_year <- c("2020")
End_year <- c("2024")
#
WQ <- new.env()
source("Code/WQ_functions_flow.R", local = WQ)
#
### USGS data collection ####
#
library(dataRetrieval)
#
# USGS package function - use to load flow data and salinity data separately
#Parameter code: 00060 = mean daily discharge, 00061 = instantaneous discharge
# 00480 = Salinity, 00095 = Specific conductance
#Statistic_id: 00003 = Mean
(temp_data <- read_waterdata_daily(monitoring_location_id = c("USGS-02310700", "USGS-02313700", "USGS-02313230", "USGS-02313250", "USGS-02310750",
                                                              "USGS-285447082445100", "USGS-285531082412600", "USGS-02310752", "USGS-02313272", "USGS-02310712", "USGS-284506082435801"), 
                                  parameter_code = c("00060", "00061"),
                                  properties = c("value", "statistic_id", "monitoring_location_id", "parameter_code", "time", "unit_of_measure"),
                                  skipGeometry = TRUE))
#
unique(temp_data$parameter_code)
#
WQ$clean_save_usgs_data(temp_data, "2020-01-01", "2024-12-31", "flow")
#
#
#
### Data from cleaned Storet files ####
# Name of file to use
#
WQ$clean_save_existing_data("SS_Portal_combined_filtered_2020_2024", "Salinity")
#
#
#
#
### Data gather and cleaning####
## Load data (logger_flow and logger_salinity files) requires xlsx files
# Make sure only desired logger data files are in the main folder
WQ$load_WQ_data()
#
## Clean data
#
# If many points, can simplify into groups with averaged values
# library(geosphere, igraph, dplyr, leaflet)
# df should have columns: ID, Latitude, Longitude, Value
# distance_threshold in meters (e.g., 2000)
sali_grps <- WQ$cluster_points(salinity_raw, 7500)
# View the map: sali_grps$map
# Access modified data: sali_grps$data
# Access group summaries: sali_grps$groups
#
# Add group station locations to Loggers data frame in R and Excel data file
updated_Loggers <- WQ$update_logger_locations(sali_grps$locations)
Loggers <- updated_Loggers
#
#
# If working with conductance data from USGS loggers, "load"USGS-R/CSI" package is used:
devtools::install_github("USGS-R/CSI")
library("CSI")
#
#
# Total daily flow for each logger
flow_ave <- flow_raw %>% 
  rename_with(~str_to_title(.x)) %>%
  rename("Date" = Timestamp) %>%
  mutate(Site = Site_code, Date = as.Date(Date)) %>% 
  # If using all stations as 1 run next line, if stations should be separate, remove line
  #mutate(Station = "ALL") %>%
  group_by(Site, Date, Station, Parameter) %>% 
  summarise(Flow = sum(Value, na.rm = T)) %>% 
  ungroup()
#
# Clean conductance data if needed:
sal_raw_temp <- salinity_raw %>% 
  # Limit to conductance data
  filter(parameter_code == "00095") %>%
  # Add Year-Month column
  mutate(Year = format(TIMESTAMP, "%Y"),
         Month = format(TIMESTAMP, "%m")) %>%
  dplyr::select(Year, Month, STATION, VALUE)
(salinity_raw <- sal_raw_temp %>%
  dplyr::group_by(STATION) %>%
  dplyr::group_modify(~ CSIspec_con(.x)) %>%
  dplyr::ungroup() %>%
  mutate(PARAMETER = "Salinity"))
#
# Mean daily salinity for each logger: either salinity_raw if no grouping, sali_grps$data if grouped
salinity_ave <- salinity_raw %>% #sali_grps$data %>%  
  rename_with(~str_to_title(.x)) %>%
  {# Rename Timestamp to Date or create Date from Year and Month
    if ("Timestamp" %in% names(.)) {
      dplyr::rename(., Date = Timestamp)
    } else if (all(c("Year", "Month") %in% names(.))) {
      dplyr::mutate(
        .,
        Date = as.Date(paste(Year, Month, "01", sep = "-"))
      )
    } else {
      .
    }
  } %>%
  mutate(Site = Site_code, Date = as.Date(Date)) %>% 
  group_by(Site, Date, Station, Parameter) %>% 
  summarise(Salinity = mean(Value, na.rm = T)) %>% 
  ungroup()
#
#
#
## Get monthly means
(sal_monthly <- WQ$calculate_monthly_value(salinity_ave, "Salinity"))
(flow_monthly <- WQ$calculate_monthly_value(flow_ave, "Flow"))
#
#
#
### Logger station mapping ###
#
# Map stations to confirm if all flow*salinity loggers should be related.
# Identify any relationships not needed.
WQ2 <- new.env()
source("Code/WQ_functions_interpolation.R", local = WQ2)
#WQ$load_site_grid()

Site_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Site_area[2])
###State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
##Get Site area
State_Grid <- c("E2") #E2, H4
Alt_Grid <- c("F2")
Site_Grid <- WQ2$load_site_grid(State_Grid, Site_area, Alt_Grid = Alt_Grid)
Site_grid_sf <- st_as_sf(Site_Grid)
#
#Map of stations
ggplot()+
  geom_sf(data = Site_area, fill = "#99CCFF")+
  #geom_sf(data = Site_Grid, fill = NA)+
  geom_sf(data = FL_outline)+
  #Individual station points if grouping:
  #geom_point(data = salinity_raw, aes(Longitude, Latitude),  color = "#666666", shape = 8, size = 4)+
  geom_point(data = Loggers, aes(Longitude, Latitude,  color = DataType, shape = DataType), alpha = 0.8, size = 4)+
  theme_classic()+
  scale_color_manual(values = c("#333333", "#D55E00"))+
  scale_shape_manual(values = c(16, 15))+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 12, color = "black"), 
        axis.text =  element_text(size = 10, color = "black"))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.05, st_bbox(Site_area)["xmax"]+0.15),
           ylim = c(st_bbox(Site_area)["ymin"]-0.05, st_bbox(Site_area)["ymax"]+0.05))
#Save 800*auto, Output/Map files/Flow_sal_loggers
#
#
#
### Model fit and plot ####
## Combined data frame - not currently helpful
monthly_data <- left_join(sal_monthly, flow_monthly)
#head(monthly_data)
#
## Fit curve
#library(stringr, minpack.lm, dplyr)
models <- WQ$fit_salinity_flow_models(flow_monthly, sal_monthly)
#Fit fails unless means used: models <- fit_salinity_flow_models(flow_ave, sal_monthly, flow_col = "Flow")
#
#
models <- WQ$filter_models(models, c("USGS-02313700-USGS-02313700", 
                                     "USGS-02313272_USGS-02313700", 
                                     "USGS-285447082445100_USGS-02313700", 
                                     "USGS-02313272_USGS-02313250",
                                     "USGS-02313272_USGS-02313230",
                                     "USGS-285447082445100_USGS-02313250",
                                     "USGS-285447082445100_USGS-02313230",
                                     "USGS-02310750_USGS-02310750",
                                     "USGS-02310752_USGS-02310750",
                                     "USGS-285531082412600_USGS-02310750",
                                     "USGS-285447082445100_USGS-02310750",
                                     "USGS-284506082435801_USGS-02310750"))
#
# Calculate flow at specified salinity (from HSM curves)
adult <- rbind(
  WQ$flow_at_salinity_hyp2(models$models, 11.98, models$data_lookup) %>% mutate(Sal = "min", Flow = "max"), 
  WQ$flow_at_salinity_hyp2(models$models, 35.98, models$data_lookup) %>% mutate(Sal = "max", Flow = "min")) %>% 
  mutate(Type = "Adult")
larvae <- rbind(
  WQ$flow_at_salinity_hyp2(models$models, 10.01, models$data_lookup) %>% mutate(Sal = "min", Flow = "max"), 
  WQ$flow_at_salinity_hyp2(models$models, 31.49, models$data_lookup) %>% mutate(Sal = "max", Flow = "min")) %>% mutate(Type = "Larave")
#
# Plot fit - option to add green fill over optimal salinity range and/or flow range
names(models$models)
WQ$ggplot_hyperbolic_fit(models$data_lookup, models$models, 
                         "USGS-02313272_USGS-02313700", 
                         "Mean_Flow", "Mean_Salinity", 
                         Salinity_min = 11.98, Salinity_max = 38.95)
#
#ggsave(path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
#       filename = paste("Flow_salinity_curve_", "SS2_WC",".tiff", sep = ""), dpi = 1000)
#
#ggplot_hyperbolic_fit(monthly_data, fit_sp, "Mean_Flow", "Mean_Salinity",
 #                     Salinity_min = 11.98, Salinity_max = 35.98,
  #                    Flow_min = 0, Flow_max = 907.26)
#
#
#
#
### Parameter values and saving ####
#
#Number of days of data per year
flow_ave %>% 
  # Filter data to date range
  dplyr::filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31")) %>%
  # Add Years and Stations
  dplyr::mutate(
    Year = lubridate::year(Date)) %>%
  # Group by Year
  dplyr::group_by(Year) %>%
  # Get total number
  dplyr::summarise(
    TotalDays = dplyr::n_distinct(Date)
    ) %>%
  pivot_wider(names_from = Year, values_from = TotalDays)
## Determine mean number of days in year within range 
#min and max Dates to include
#
#Calculate the mean optimal days for each station 
#returndf = individual, combined, both
Adult_optimal <- WQ$automate_optimal_df(adult, flow_ave, "2020-01-01", "2024-12-31", "Adult", "both")
Larvae_optimal <- WQ$automate_optimal_df(larvae, flow_ave, "2020-01-01", "2024-12-31", "Larvae", "both")
#
## Number above or below optimal
Adult_nonoptimal <- WQ$automate_nonoptimal_df(adult, flow_ave, "2020-01-01", "2024-12-31", "Adult", "both")
Larvae_nonoptimal <- WQ$automate_nonoptimal_df(larvae, flow_ave, "2020-01-01", "2024-12-31", "Larvae", "both")
#
#
#
# Count number of days in month more than 1.5 SD from monthly mean
outlier_flow <- WQ$count_outlier_flow_days(flow_ave, "2020-01-01", "2024-12-31", "Flow") 
#
#
#
# Save data and/or figure created
#make sure to specify list item if list object used
WQ$save_flow_output(adult, 
                 larvae, 
                 Adult_optimal$Mean, 
                 Adult_nonoptimal$Mean, 
                 Larvae_optimal$Mean, 
                 Larvae_nonoptimal$Mean,
                 outlier_flow)
#
##
### Interpolation ####
#
## Get values into dataframe relating values to coordinates: 
#Adult meanOptimal, Larvae meanOptimal, meanOutlier
(A_optimal <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  Adult_optimal$Mean %>% rename(Station = SalStation)) %>%
  # add flow logger data
  mutate(meanOptimal = case_when(is.na(meanOptimal) ~ 0, TRUE ~ meanOptimal)))
#
(A_nonoptimal <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  Adult_nonoptimal$Mean %>% rename(Station = SalStation)) %>%
    # add flow logger data
    mutate(meanDays = case_when(is.na(meanDays) ~ 0, TRUE ~ meanDays)))
#
(L_optimal <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  Larvae_optimal$Mean %>% rename(Station = SalStation)) %>%
    # add flow logger data
    mutate(meanOptimal = case_when(is.na(meanOptimal) ~ 0, TRUE ~ meanOptimal)))
#
(L_nonoptimal <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  Larvae_nonoptimal$Mean %>% rename(Station = SalStation)) %>%
  # add flow logger data
  mutate(meanDays = case_when(is.na(meanDays) ~ 0, TRUE ~ meanDays)))
#
(Outliers <- left_join(
  # Get into same format
  Loggers %>% rename(Station = StationID) %>% mutate(Station = str_replace(Station, "_", "")),  
  # add values
  outlier_flow %>%
    rename(meanOut1 = Out_1_Months,
           meanOut2 = Out_2_Months) %>% 
    mutate(Station = gsub("_", "", Station)) %>%
    dplyr::filter(Station != "Overall") %>%
    dplyr::select(-contains("Range"))) %>%
  # add flow logger data
  mutate(meanOut1 = case_when(is.na(meanOut1) ~ 0, TRUE ~ meanOut1),
         meanOut2 = case_when(is.na(meanOut2) ~ 0, TRUE ~ meanOut2)))
#
#
# Mapping section above is required before running following code:
Site_Grid_spdf <- as(Site_Grid %>% dplyr::select(Latitude, Longitude, PGID), "Spatial")
#
## Get logger as spatial:
Logger_coords <- Loggers %>% dplyr::select(Longitude, Latitude)
coordinates(Logger_coords) <- ~Longitude + Latitude  # Longitude as x, Latitude as y
proj4string(Logger_coords) <- CRS("+proj=longlat +datum=WGS84")
# Get extents of both spatial objects
ext1 <- extent(Site_Grid_spdf)
ext2 <- extent(Logger_coords)
# Create combined extent (bounding box covering both)
combined_ext <- extent(
  min(ext1@xmin, ext2@xmin), 
  max(ext1@xmax, ext2@xmax), 
  min(ext1@ymin, ext2@ymin), 
  max(ext1@ymax, ext2@ymax)
)
# Convert the combined extent to a SpatialPolygons object
# (This assumes the CRS is the same for both; if not, set it explicitly)
combined_poly <- as(combined_ext, "SpatialPolygons")
proj4string(combined_poly) <- proj4string(Site_Grid_spdf)  # Inherit CRS from Site_Grid_spdf
#
# Sample a regular grid of 10000 points from the combined polygon
grid <- spsample(combined_poly, type = 'regular', n = 10000)
plot(grid) 
#
##Inverse distance weighted:
library(sf)       # For sf operations
library(gstat)    # For idw()
library(dismo)    # For voronoi()
library(raster)   # For extent()
library(sp)       # For SpatialPointsDataFrame
library(dplyr)    # For data manipulation
library(lubridate) # For parse_date_time() and time calculations
library(ggrastr)
#
## Repeat for each data frame:
#
# Adult optimal
data_cols <- if(ncol(A_optimal) >= 3) {
  A_optimal[, !names(A_optimal) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = A_optimal[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
AOP_idw_data <- WQ$flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanOptimal")
#
WQ$plot_flow_interp(AOP_idw_data, "meanOptimal")
#
ggsave(path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "adult_meanOptimal",".tiff", sep = ""), 
       dpi = 400)
#
#
#
# Larvae optimal
data_cols <- if(ncol(L_optimal) >= 3) {
  L_optimal[, !names(L_optimal) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = L_optimal[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
LOP_idw_data <- WQ$flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanOptimal")
#
WQ$plot_flow_interp(LOP_idw_data, "meanOptimal")
#
ggsave(path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "larval_meanOptimal",".tiff", sep = ""), 
       dpi = 400,
       device = ragg::agg_tiff,
       width = 8,
       height = 7,
       units = "in",
       compression = "lzw")
#
#
#
# Sub and super
data_cols <- if(ncol(A_nonoptimal) >= 3) {
  A_nonoptimal[, !names(A_nonoptimal) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = A_nonoptimal[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
AnonSub_idw_data <- WQ$flow_idw_interpolation(
  Site_data_spdf[is.na(Site_data_spdf$FlowType) | Site_data_spdf$FlowType != "super", ], 
  grid, Site_Grid_spdf, "meanDays")
#
AnonSuper_idw_data <- WQ$flow_idw_interpolation(
  Site_data_spdf[is.na(Site_data_spdf$FlowType) | Site_data_spdf$FlowType != "sub", ], 
  grid, Site_Grid_spdf, "meanDays")
#
p <- WQ$plot_flow_interp(AnonSub_idw_data, "meanDays")
#
ggsave(plot = p,
       path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "adult_sub_meanDays",".tiff", sep = ""), 
       dpi = 450,
       device = ragg::agg_tiff,
       width = 8,
       height = 8,
       units = "in",
       compression = "lzw")
#
p <- WQ$plot_flow_interp(AnonSuper_idw_data, "meanDays")
#
ggsave(plot = p,
       path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "adult_super_meanDays",".tiff", sep = ""), 
       dpi = 450,
       device = ragg::agg_tiff,
       width = 8,
       height = 8,
       units = "in",
       compression = "lzw")
#
#
#
#
# Larval sub and super days
data_cols <- if(ncol(L_nonoptimal) >= 3) {
  L_nonoptimal[, !names(L_nonoptimal) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = L_nonoptimal[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
LnonSub_idw_data <- WQ$flow_idw_interpolation(
  Site_data_spdf[is.na(Site_data_spdf$FlowType) | Site_data_spdf$FlowType != "super", ], 
  grid, Site_Grid_spdf, "meanDays")
#
LnonSuper_idw_data <- WQ$flow_idw_interpolation(
  Site_data_spdf[is.na(Site_data_spdf$FlowType) | Site_data_spdf$FlowType != "sub", ], 
  grid, Site_Grid_spdf, "meanDays")
#
#
p <- WQ$plot_flow_interp(LnonSub_idw_data, "meanDays")
#
ggsave(plot = p,
       path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "larval_sub_meanDays",".tiff", sep = ""), 
       dpi = 450,
       device = ragg::agg_tiff,
       width = 8,
       height = 8,
       units = "in",
       compression = "lzw")
#
p <- WQ$plot_flow_interp(LnonSuper_idw_data, "meanDays")
p_fast <- p +
  ggrastr::rasterise(geom_sf(), dpi = 450)
#
ggsave(plot = p_fast,
       path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "larval_super_meanDays",".tiff", sep = ""), 
       dpi = 450,
       device = ragg::agg_tiff,
       width = 8,
       height = 8,
       units = "in",
       compression = "lzw")
#
#
##
#
#
data_cols <- if(ncol(Outliers) >= 3) {
  Outliers[, !names(Outliers) %in% c("Latitude", "Longitude"), drop = FALSE]#c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
} else {
  stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
}
Site_data_spdf <- SpatialPointsDataFrame(coords = Outliers[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
Outlier_idw_data <- WQ$flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanOut1")#
#
Outlier2_idw_data <- WQ$flow_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, "meanOut2")#
#
p <- WQ$plot_flow_interp(Outlier_idw_data, "meanOut1")
p_fast <- p +
  ggrastr::rasterise(geom_sf(), dpi = 450)
#
ggsave(plot = p_fast,
       path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "Outlier1",".tiff", sep = ""), 
       dpi = 450,
       device = ragg::agg_tiff,
       width = 8,
       height = 8,
       units = "in",
       compression = "lzw")
#
p <- WQ$plot_flow_interp(Outlier2_idw_data, "meanOut2")
p_fast <- p +
  ggrastr::rasterise(geom_sf(), dpi = 450)
#
ggsave(plot = p_fast, 
       path = paste0("../", Site_code, "_", Version, "/Data/HSI curves/"), 
       filename = paste("Flow_salinity_curve_", "Outlier2",".tiff", sep = ""), 
       dpi = 450,
       device = ragg::agg_tiff,
       width = 8,
       height = 8,
       units = "in",
       compression = "lzw")

#
#
#
#
#
## Save output
#fileName: SiteCode_fileName
WQ$save_flow_interp_output(AOP_idw_data, "flow_optimal_adult") #meanOptimal
WQ$save_flow_interp_output(AnonSub_idw_data, "flow_sub_adult") #meanDays
WQ$save_flow_interp_output(AnonSuper_idw_data, "flow_super_adult")
WQ$save_flow_interp_output(LOP_idw_data, "flow_optimal_larvae") #meanOptimal
WQ$save_flow_interp_output(LnonSub_idw_data, "flow_sub_larvae") #meanDays
WQ$save_flow_interp_output(LnonSuper_idw_data, "flow_super_larvae")
WQ$save_flow_interp_output(Outlier_idw_data, "flow_outlier1") #meanOut1
WQ$save_flow_interp_output(Outlier2_idw_data, "flow_outlier2") #meanOut2
#
#
## Saving Excel data files: had issue with remove geometry and file size
write_csv_chunks <- function(
    df,
    out_dir,
    prefix = "data",
    chunk_size = 1e6
) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  n <- nrow(df)
  idx <- ceiling(seq_len(n) / chunk_size)
  
  split_df <- split(df, idx)
  
  purrr::iwalk(split_df, function(x, i) {
    readr::write_csv(
      x,
      file.path(out_dir, sprintf("%s_part_%s.csv", prefix, i))
    )
  })
  
  invisible(length(split_df))
}
#
(temp_data <- st_drop_geometry(Outlier2_idw_data) %>% 
  as.data.frame() %>%
  dplyr::rename("Long_DD_X" = Longitude, "Lat_DD_Y" = Latitude) %>%
  mutate(Long_DD_X = as.numeric(Long_DD_X),
         Lat_DD_Y = as.numeric(Lat_DD_Y),
         meanOut2 = as.numeric(meanOut2),
         dplyr::across(
           where(is.character),
           ~ na_if(trimws(.x), "")
         )))
write_csv_chunks(
  df = temp_data,
  out_dir = paste0("../",Site_code, "_", Version,"/Output/Data files/", #Save location
                   #File name
                   paste0(Site_code, "_", paste("flow_outlier2"))),
  prefix = "flow_outlier2"
)
#
#
### Other possible data ####
# 
## Metrics
# Function to calculate 30-day back averaged salinity (rolling 30-day mean) library(dplyr, slider)
# Input: df (data frame with 'Date' as Date class and 'Salinity' as numeric)
#        complete (logical, default TRUE): If TRUE, only calculate when full 30-day window is available; if FALSE, use partial windows
# Output: The input data frame with an added column 'Rolling_30d_Salinity'
calculate_rolling_30d_value <- function(df, value_col = "Salinity", complete = TRUE) {
  # Check for required columns and formats
  if (!"Date" %in% colnames(df)) {
    stop("Error: The data frame must contain a 'Date' column.")
  }
  if (!value_col %in% colnames(df)) {
    stop("Error: The data frame must contain a '",value_col,"' column.")
  }
  if (!inherits(df$Date, "Date")) {
    stop("Error: The 'Date' column must be of class 'Date'. Convert it using as.Date() if necessary.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Error: The '",value_col,"' column must be numeric.")
  }
  #
  # Ensure data is sorted by Date
  df <- df %>% arrange(Date)
  # Dynamic naming
  output_col <- paste0("Roll3d_", value_col)
  #
  # Calculate rolling 30-day mean
  df[[output_col]] <- slide_period_dbl(
    .x = df[[value_col]],
    .i = df$Date,
    .period = "day",
    .f = ~mean(.x, na.rm = TRUE),
    .before = 29,
    .complete = complete
  )
  return(df)
}
#
(sal_30d <- calculate_rolling_30d_value(salinity_ave, "Salinity"))
#
#
#
## Curve fits
#
## Currently working with previously used formula in CERP reports
#non linear, exponential decay to scatter plot of flow vs sal library(minpack.lm) #nlsLM() is more stable/robust than base nls().
fit_month <- nlsLM(
  Mean_Salinity ~ a * exp(-b * Mean_Flow) + c,
  data = monthly_data,
  start = list(a = max(monthly_data$Mean_Salinity, na.rm = T), b = 0.01, c = min(monthly_data$Mean_Salinity, na.rm = T))
)
summary(fit_month)
#
flow_at_salinity <- function(fit, target_sal) {
  # Extract parameters
  p <- coef(fit)
  a <- p["a"]
  b <- p["b"]
  c <- p["c"]
  
  # Check valid range
  if ((target_sal - c) / a <= 0) {
    stop("Target salinity is outside the valid range of the exponential model.")
  }
  
  # Compute flow
  flow <- -(1 / b) * log((target_sal - c) / a)
  return(flow)
}

flow_at_salinity(fit_month, 10)
flow_at_salinity(fit_month, 30)
plot_flow_target <- function(df, fit, target_sal, flow_value, value_col = "Mean_Salinity") {
  # Create sequence for smooth fitted curve
  xseq <- seq(min(df[[flow_value]], na.rm = T), max(df[[flow_value]], na.rm = T), length.out = 300)
  
  # Predict salinity across flow range
  pred <- predict(fit, newdata = data.frame(!!flow_value := xseq))
  
  # Plot
  plot(df[[flow_value]], df[[value_col]],
       pch = 19, col = "gray40",
       xlab = "Flow",
       ylab = "Salinity",
       main = paste("Exponential Decay Fit with Target =", target_sal))
  
  lines(xseq, pred, col = "blue", lwd = 2)
  
  # Add point for flow at salinity target
  points(flow_value, target_sal, pch = 19, col = "red", cex = 1.4)
  abline(h = target_sal, col = "red", lty = 2)
  abline(v = flow_value, col = "red", lty = 2)
  
  text(flow_value, target_sal,
       labels = sprintf("Flow = %.2f", flow_value),
       pos = 4, col = "red")
}
plot_flow_target(monthly_data, fit_month, 20, Mean_Flow, value_col = "Mean_Salinity")
#
#
#
#
#non liner hyperbolic decay 
fit_hyp <- nlsLM(
  Mean_Salinity ~ c + a / (1 + b * Mean_Flow),
  data = monthly_data,
  start = list(
    a = max(monthly_data$Mean_Salinity, na.rm = T) - min(monthly_data$Mean_Salinity, na.rm = T),   # amplitude guess
    b = 0.01,                                  # rate constant guess
    c = min(monthly_data$Mean_Salinity, na.rm = T)                       # lower asymptote guess
  )
)
summary(fit_hyp)
#
flow_at_salinity_hyperbolic <- function(fit, target_sal) {
  # Extract parameters
  p <- coef(fit)
  a <- p["a"]
  b <- p["b"]
  c <- p["c"]
  
  # Ensure target salinity is valid
  if (target_sal <= c) {
    stop("Target salinity is <= the asymptote c. Flow cannot be solved.")
  }
  
  # Calculate flow
  flow <- (a / (target_sal - c) - 1) / b
  return(flow)
}
plot_flow_target_hyperbolic <- function(df, fit, target_sal, flow_value,
                                        value_col = "Salinity") {
  # Generate smooth curve
  xseq <- seq(min(df$flow), max(df$flow), length.out = 300)
  
  pred <- predict(fit, newdata = data.frame(flow = xseq))
  
  plot(df$flow, df[[value_col]],
       pch = 19, col = "gray40",
       xlab = "Flow", ylab = "Salinity",
       main = paste("Hyperbolic Decay Fit with Target =", target_sal))
  
  lines(xseq, pred, col = "blue", lwd = 2)
  
  # Highlight the target point
  points(flow_value, target_sal, pch = 19, col = "red", cex = 1.4)
  abline(h = target_sal, col = "red", lty = 2)
  abline(v = flow_value, col = "red", lty = 2)
  
  text(flow_value, target_sal,
       sprintf("Flow = %.2f", flow_value),
       pos = 4, col = "red")
}

