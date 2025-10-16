##Water quality data interpolation - **in progress - edits to be made**
#
#Interpolation of point data to site area based on site KML file
#Requires WQ data Excel file, KML file, FL_outlines layer, picogrid layer
#
#
##Requires data file in Compiled-data folder, site KML file
#
#Load require packages (install as necessary)  - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               readxl, openxlsx, progress, writexl,
               sf, sp, terra, furrr, future,
               tmap, tmaptools, gridExtra, #Mapping and figures
               mgcv, fpc, fields, interp, #mgcv - interpolation, fpc::bscan - clustering
               RColorBrewer, magicfor, ecorest, #HSV scoring
               marmap, gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
source("Code/WQ_functions.R")
#
Site_code <- c("WC")       #Two letter estuary code
Version <- c("v1")         #Version code for model 
State_Grid <- c("F2")      #Two-letter StateGrid ID
Alt_Grid <- c("F3")        #Two-letter additional StateGrid ID, enter NA if no secondary StateGrid needed
Project_code <- c("WCHSM") #Project code given to data, found in file name
Start_year <- c("2020")    #Start year (YYYY) of data, found in file name
End_year <- c("2024")      #End year (YYYY) of data, found in file name
Folder <- c("compiled")    #Data folder: "compiled" or "final"
Data_source <- c("Portal") #Required if Folder = compiled.
Param_name <- c("Salinity")#Column/parameter name of interest - from WQ data file.
Param_name_2 <- c("Annual")#Additional identify for parameter: i.e. Annual, Quarterly, etc.
#
color_temp <- c("cool")    #"warm" or "cool"
#
####Load data and KML files, plot existing points - will be one function####
#
load_WQ_data()
#
Site_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Site_area[2])
###State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
##Get Site area  
Site_Grid <- load_site_grid(State_Grid, Site_area, Alt_Grid)
Site_grid_sf <- st_as_sf(Site_Grid)
#
#Df of grid data
Site_Grid_df <- Site_Grid %>% st_set_geometry(NULL)
#Map of stations
ggplot()+
  geom_sf(data = Site_area, fill = "#6699CC")+
  #geom_sf(data = Site_Grid, fill = NA)+
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_data, aes(Longitude, Latitude), size = 3.5)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
#END OF SECTION
#
####Grid/raster set up - run once per session####
#
Site_Grid_spdf <- as(Site_Grid %>% dplyr::select(Latitude, Longitude, PGID, MGID), "Spatial")
grid <- spsample(Site_Grid_spdf, type = 'regular', n = 10000) 
plot(grid) 
#Get extent in meters to create raster:
Site_extent_m <- as.matrix(bb(extent(Site_area), current.projection = 4326, projection = 32617)) #W, S, E, N
#Create base raster using meters:
raster_t_m <- rast(resolution = c(20, 20),
                   xmin = Site_extent_m[1], xmax = Site_extent_m[3], ymin = Site_extent_m[2], ymax = Site_extent_m[4],
                   crs = "+init=EPSG:32617")
#Convert back to dd:
raster_t <- terra::project(raster_t_m, "EPSG:4326")
#
# Determine which scale to use based on color_temp
if(color_temp == "warm") {
  scale_to_use <- scale_color_viridis_c(option = "rocket", direction = -1)
} else if(color_temp == "cool") {
  scale_to_use <- scale_color_viridis_c(option = "mako", direction = -1)
} else {
  scale_to_use <- scale_color_viridis_c()  # or some other default
}
#
#
#
####Summarize data based on parameter of interest - all methods####
#
##Summarize data based on method specified:
#Time_period - Period of time to group by: Year, Month, Quarter
#Year_range - Range of years of data to include. Blank/enter "NA" for all years, 4-digit year for one year, or enter a character string of 4-digit start year followed by 4-digit end year, separated by a dash "-"
#Quarter_start - Starting month of quarter 1, entered as an integer corresponding to month. NA if January (1) start. Not needed if not wokring with quarters.
#Month_range - Start and end month to include in final data, specified by month's integer value c(#, #)
#Summ_method - Summarization method: Means, Mins, Maxs, Range, Range_values, Threshold
#Threshold_parameters - Required if Summ_method = Threshold: two parameters to enter: [1] above or below, [2] value to reference entered as numeric
#
WQ_summ <- summarize_data(WQ_data, Time_period = "Year", Summ_method = "Means")
head(WQ_summ)
#write_xlsx(WQ_summ, paste0("../", Site_code, "_", Version, "/Data/", Site_code, "_WQ_", Param_name, "_", Param_name_2,".xlsx"), format_headers = TRUE)
#
#
#Data as spatial df:
data_cols <- if(ncol(WQ_summ) >= 3) {
  WQ_summ[, c(which(names(WQ_summ) == "Statistic"):ncol(WQ_summ)), drop = FALSE]
  } else {
    stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
    }
Site_data_spdf <- SpatialPointsDataFrame(coords = WQ_summ[,c("Longitude","Latitude")], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#
#
#
####Interpolation models####
#
#
##Inverse distance weighted
idw_data <- perform_idw_interpolation(Site_data_spdf, grid, Site_Grid_spdf, Param_name)
#
##Nearest neighbor
nn_data <- perform_nn_interpolation(Site_data_spdf, Site_area, Site_Grid, Site_Grid_spdf, Param_name, WQ_summ)
#
##Thin plate spline
tps_data <- perform_tps_interpolation(Site_data_spdf, raster_t, Site_area, Site_Grid, Param_name)
#
####Ordinary Kriging
ok_data <- perform_ok_interpolation(Site_data_spdf, grid, Site_Grid_spdf, Param_name)
#
#
#
####Joining and comparing####
#
#Outputs df of 'results_[Param]'
join_interpolation(Site_Grid_df)
#
#Generates plots for each model and output of all models together - run for each parameter
plotting <- plot_interpolations(result_Mean, Site_Grid)
#
#
#
####Ensemble or model selection####
#
#weighting <- c("equal") #Specify "equal" for equal weighting, or values between 0 and 1 for specific weights.
#Specific weights should be listed in order based on models select idw > nn > tps > ok. Only put values for models selected.
final_data <- final_interpolation("ensemble", c("idw", "ok"), result_Threshold, c(0.75, 0.25), Site_Grid)
#
#
####Save model####
#
save_model_output(final_data)
#
#
#If continuing to work, good practice to remove objects to make sure correct data is used:
rm(final_data, plotting, ok_data, tps_data, nn_data, idw_data, Site_data_spdf, WQ_summ, list = ls(pattern = "result_"))
