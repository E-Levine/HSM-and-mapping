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
               readxl, writexl, progress,
               sf, sp, terra,
               tmap, tmaptools, #Mapping and figures
               mgcv, fpc, fields, interp, #mgcv - interpolation, fpc::bscan - clustering
               RColorBrewer, magicfor, ecorest, #HSV scoring
               marmap, gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
source("Code/WQ_functions.R")
#
Site_code <- c("SL")       #Two letter estuary code
Version <- c("v1")         #Version code for model 
State_Grid <- c("H4")      #Two-letter StateGrid ID
Alt_Grid <- c(NA)          #Two-letter additional StateGrid ID, enter NA if no secondary StateGrid needed
Project_code <- c("SLHSM") #Project code given to data, found in file name
Start_year <- c("2020")    #Start year (YYYY) of data, found in file name
End_year <- c("2024")      #End year (YYYY) of data, found in file name
Folder <- c("compiled")    #Data folder: "compiled" or "final"
Data_source <- c("Portal") #Required if Folder = compiled.
Param_name <- c("Salinity")#Column/parameter name of interest - from WQ data file.
#
color_temp <- c("cool")    #"warm" or "cool"
#
####Load data and KML files, plot existing points - will be one function####
#
if(Folder == "compiled"){
  files <- list.files(path = "Data/Compiled-data/", 
                      pattern = paste0(Site_code, "_", Data_source, "_.*_", Project_code, "_", Start_year, "_", End_year,".xlsx"))
  WQ_data <- read_excel(paste0("Data/Compiled-data/", files[1]), na = c("NA", " ", "", "Z")) %>%
    dplyr::rename(Latitude = contains("Latitude"), Longitude = contains("Longitude"), StationID = contains("LocationIdentifier"),
                  Parameter = contains("CharacteristicName"), Value = contains("MeasureValue"))
  } else {paste("Code needs to be updated for 'final' folder location.")}
#
##Site area  
Site_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Site_area[2])
###State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#Load StateGrid(s) of picogrid
PicoGrid <- st_read(paste0("../Reference files/Grids/Florida_PicoGrid_WGS84_",State_Grid,"/Florida_PicoGrid_WGS84_",State_Grid,"_clip.shp"), quiet = TRUE)
if(!is.na(Alt_Grid)){Alt_PicoGrid <- st_read(paste0("Reference files/Grids/Florida_PicoGrid_WGS84_",Alt_Grid,"/Florida_PicoGrid_WGS84_",Alt_Grid,"_clip.shp"), quiet = TRUE)}
#
##Limit to site area
if(!is.na(Alt_Grid)){
  Site_Grid <- rbind(PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,], 
                      Alt_PicoGrid[lengths(st_intersects(Alt_PicoGrid, Site_area))> 0,]) %>%
    rename(Longitude = Long_DD_X, Latitude = Lat_DD_Y)
  rm(PicoGrid, Alt_PicoGrid)
} else {
  Site_Grid <- PicoGrid[lengths(st_intersects(PicoGrid, Site_area))> 0,] %>%
    rename(Longitude = Long_DD_X, Latitude = Lat_DD_Y)
  rm(PicoGrid)
}
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
####Summarize data based on parameter of interest - all methods####
#
##Summarize data based on method specified:
#Time_period - Period of time to group by: Year, Month, Quarter
#Year_range - Range of years of data to include. Blank/enter "NA" for all years, 4-digit year for one year, or enter a character string of 4-digit start year followed by 4-digit end year, separated by a dash "-"
#Quarter_start - Starting month of quarter 1, entered as an integer corresponding to month. NA if January (1) start. Not needed if not wokring with quarters.
#Month_range - Start and end month to include in final data, specified by month's integer value c(#, #)
#Summ_method - Summarization method: Means, Mins, Maxs, Range, Range_values

WQ_summ <- summarize_data(WQ_data, Summ_method = "Means")
head(WQ_summ)
#
#
#Create grid of area based on station locations - used for all scores - only need location information 
Site_Grid_spdf <- as(Site_Grid %>% dplyr::select(Latitude:MGID), "Spatial")
grid <- spsample(Site_Grid_spdf, type = 'regular', n = 10000) #st_as_sf(Site_Grid_spdf@data %>% dplyr::select(Longitude, Latitude, PGID), coords = c("Longitude", "Latitude"), crs = 4326)
#rast <- rast(Site_Grid, resolution = c(20, 20))
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
#Data as spatial df:
data_cols <- if(ncol(WQ_summ) >= 3) {
  WQ_summ[, 3:ncol(WQ_summ), drop = FALSE]
  } else {
    stop("WQ_summ must have at least 3 columns (2 for coordinates + 1 for data)")
    }
Site_data_spdf <- SpatialPointsDataFrame(coords = WQ_summ[,1:2], data_cols, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
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
####Inverse distance weighted####
#
perform_idw_interpolation <- function(Site_data_spdf, grid, Site_Grid, Site_Grid_spdf, Site_Grid_df, Parameter = Param_name) {
  Param_name <- Parameter
  #Determine number of statistics to loop over
  stats <- unique(Site_data_spdf@data$Statistic)
  #Initiate lists 
  idw.output <- list()
  idw_spdf <- list()
  idw_nn <- list()
  idw_Site <- list()
  # Create a progress bar
  pb <- progress_bar$new(format = "[:bar] :percent | Step: :step | [Elapsed time: :elapsedfull]",
                         total = length(stats) * 4,  
                         complete = "=", incomplete = "-", current = ">",
                         clear = FALSE, width = 100, show_after = 0, force = TRUE)
  #
  tryCatch({
    #Loop over each statistic
    for(i in stats){
      ##MODELLING:
      pb$tick(tokens = list(step = "Modeling"))
      Sys.sleep(1/1000)
      # Filter data for current statistic
      stat_data <- Site_data_spdf[Site_data_spdf@data$Statistic == i, ]
      ##IDW: model(Parameter), data to use, grid to apply to 
      idw_model <- suppressMessages(idw(stat_data$Working_Param~1, stat_data, newdata = grid))
      #Convert to data frame to rename and add parameters levels as values rounded to 0.1
      idw.output[[i]] <- as.data.frame(idw_model) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>% 
        mutate(Pred_Value = round(Prediction, 2)) %>% dplyr::select(-var1.var)
      #
      ##PROCESSING:
      pb$tick(tokens = list(step = "Processing"))
      Sys.sleep(1/1000)
      #Convert interpolated values to spatial data
      idw_spdf[[i]] <- SpatialPointsDataFrame(coords = idw.output[[i]][,1:2], data = idw.output[[i]][4], 
                                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
      #
      #Use nearest neighbor to merge values into polygons, limit to bounding box of site area
      idw_nn[[i]] <- dismo::voronoi(idw_spdf[[i]], ext = extent(Site_Grid)) 
      #
      ##GRID App:
      pb$tick(tokens = list(step = "Grid Application"))
      Sys.sleep(1/1000)
      #Determine overlay of data on SiteGrid
      idw_Site[[i]] <- intersect(idw_nn[[i]], Site_Grid_spdf)
      #
      ##WRAP UP:
      pb$tick(tokens = list(step = "Finishing up"))
      Sys.sleep(1/1000)
      #
      pb$message(paste("Completed:", i, Param_name))
    }
    close(pb)
  }, error = function(e){ 
    message("The progress bar has ended")
    pb$terminate()
  }, finally = {
    pb$terminate() 
  })
  #
  return(idw_Site)
}
#
idw_data <- perform_idw_interpolation(Site_data_spdf, grid, Site_Grid, Site_Grid_spdf, Param_name)
#
#
#
###Data frame with interpolated parameter values:
(interp_data <- Site_Grid_df %>% 
  left_join(as.data.frame(idw_Site) %>% dplyr::select(PGID, Pred_Value)) %>% 
  group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1) %>%
  dplyr::rename(!!paste0(Param_name,"_idw") := Pred_Value))
#Add interpolated data back to Site_grid sf object 
(Site_Grid_interp <- left_join(Site_Grid, interp_data))
#
#Plot of binned interpolate values for rough comparison
ggplot()+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_idw"))))+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), axis.text =  element_text(size = 16))+
  scale_color_viridis_b(direction = -1)
#Map of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_idw"))))+
    scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude, fill = Working_Param), color = "white", size = 4.5, shape = 21)+#, color = "black", size = 3)+
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle(paste0("IDW: Mean ",Param_name)) +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
##END OF IDW
#
#
####Nearest neighbor####
#
##Voroni
nn_model <- voronoi(x = vect(WQ_summ, geom=c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs"), bnd = Site_area)
#
##Predictions
nn_model <- st_as_sf(nn_model)
#Assign predictions to grid
nn_Site <- st_intersection(nn_model, st_as_sf(Site_Grid %>% dplyr::select(Latitude:MGID)))
#Plot of interpolated values (area)
qtm(nn_Site, col = "Salinity_nn", fill = "Salinity_nn")
#
###Data frame with interpolated parameter values: - add to existing data (other model) or start new
(interp_data <- interp_data %>% #Site_Grid_df %>% 
    left_join(as.data.frame(nn_Site) %>% dplyr::select(PGID, Working_Param) %>% 
    group_by(PGID) %>% arrange(desc(Working_Param)) %>% slice(1)) %>%
    dplyr::rename(!!paste0(Param_name,"_nn") := Working_Param))
#Add interpolated data back to Site_grid sf object 
(Site_Grid_interp <- left_join(Site_Grid, interp_data))
#
#Plot of binned interpolate values for rough comparison
ggplot()+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_nn"))))+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), axis.text =  element_text(size = 16))+
  scale_color_viridis_b(direction = -1)
#Map of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_nn"))))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude, fill = Working_Param), shape = 21, size = 4.5, color = "white")+#, color = "black", size = 3.5)+
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle(paste0("NN: Mean ", Param_name)) +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
##END OF NN
#
####Thin plate spline####
#
#Convert WQ points to vector and rasterize over grid:
Param_vec <- vect(Site_data_spdf)
Param_ras <- rasterize(Param_vec, raster_t, field = "Working_Param")
#thin plate spline model
tps_model <- interpolate(raster_t, Tps(xyFromCell(Param_ras, 1:ncell(Param_ras)),
                                values(Param_ras)))
#
#Limit data to area of interest
tps_model <- crop(mask(tps_model, Site_area),Site_area) %>% as.polygons() %>% as("Spatial")
#
#Get mean data for each location
tps_Site <- st_intersection(Site_Grid %>% dplyr::select(Latitude:MGID), st_as_sf(tps_model)) %>% 
  rename(Pred_Value = lyr.1) %>% st_set_geometry(NULL) %>%
  dplyr::select(PGID, Pred_Value) %>% group_by(PGID) %>%
  summarize(Pred_Value = mean(Pred_Value, na.rm = T)) 
###Data frame with interpolated parameter values: - add to existing data (other model) or start new
(interp_data <- interp_data %>% # Site_Grid_df %>% 
    left_join(tps_Site %>% dplyr::select(PGID, Pred_Value)) %>% 
    group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1) %>%
    dplyr::rename(!!paste0(Param_name,"_tps") := Pred_Value))
#Add interpolated data back to Site_grid sf object 
(Site_Grid_interp <- left_join(Site_Grid, interp_data))
#
#Plot of binned interpolate values for rough comparison
ggplot()+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_tps"))))+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), axis.text =  element_text(size = 16))+
  scale_color_viridis_b(direction = -1)
#Map of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_tps"))))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude, fill = Working_Param), shape = 21, size = 4.5, color = "white")+#color = "black", size = 3.5)+
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle(paste0("TPS: Mean ",Param_name)) +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
#
##END OF TPS
#
####Ordinary Kriging####
#
#ok_v <- variogram(Salinity~1, Site_data_spdf)
#plot(ok_v)
ok_fit <- autofitVariogram(Working_Param ~ 1, Site_data_spdf)
#ok_vfit$var_model
ok_model <- gstat(formula = Working_Param~1, model = ok_fit$var_model, data = Site_data_spdf)
ok_pred <- predict(ok_model, grid)
#Convert to data frame to rename and add parameters levels as values rounded to 0.1
ok.output <- as.data.frame(ok_pred) %>% rename("Longitude" = x1, "Latitude" = x2, "Prediction" = var1.pred) %>%
  mutate(Pred_Value = round(Prediction, 2)) %>% dplyr::select(-var1.var)
#Convert interpolated values to spatial data
ok_spdf <- SpatialPointsDataFrame(coords = ok.output[,1:2], data = ok.output[4], 
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Use nearest neighbor to merge values into polygons, limit to bounding box of site area
ok_nn <- dismo::voronoi(ok_spdf, ext = extent(Site_Grid))
#
#Determine overlay of data on SiteGrid
ok_Site <- intersect(ok_nn, Site_Grid_spdf)
#
###Data frame with interpolated parameter values:
(interp_data <- interp_data %>% #Site_Grid_df %>% 
    left_join(as.data.frame(ok_Site) %>% dplyr::select(PGID, Pred_Value) %>% 
    group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1)) %>%
    dplyr::rename(!!paste0(Param_name,"_ok") := Pred_Value))
#Add interpolated data back to Site_grid sf object 
(Site_Grid_interp <- left_join(Site_Grid, interp_data))
#
#Plot of binned interpolate values for rough comparison
ggplot()+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_ok"))))+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), axis.text =  element_text(size = 16))+
  scale_color_viridis_b(direction = -1)
#Map of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_ok"))))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude, fill = Working_Param), shape = 21, size = 4.5, color = "white")+#color = "black", size = 3.5)+
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle(paste0("OK: Mean ", Param_name)) +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
#
##END OF OK
#
####Ensemble####
#
weighting <- c("equal") #Specify "equal" for equal weighting, or values between 0 and 1 for specific weights
model_weighting <- function(weighting) {
  #Patterns to search for:
  patterns <- "_(idw|nn|tps|ok)$"
  
  #Function for equal weighting
  if (weighting == "equal") {
    weights_from_columns <- function(dataframe) {
      #Identify columns that match the pattern
      matched_columns <- colnames(dataframe)[grepl(patterns, colnames(dataframe))]
      num_divisions <- length(matched_columns)  # Count the number of matched columns
      #Create weights based on the number of divisions
      if (num_divisions > 0) {
        weights <- rep(1 / num_divisions, num_divisions)
        #Create names for the weights
        names(weights) <- paste0("weight_", sub(".*_", "", matched_columns))  # Extract the pattern part
      } else {
        weights <- numeric(0)  # Return an empty numeric vector if no matches
      }
      return(weights)
    }
    return(weight_values <<- weights_from_columns(Site_Grid_interp))
    print(weight_values)
  } else {
    print("Numbers need to be specified.")
  }
}
#
model_weighting(weighting)
#
#
#Select columns of interpolated data 
ens_Site <- Site_Grid_interp %>% dplyr::select(PGID, matches("_(idw|nn|tps|ok)$")) %>%
  mutate(Pred_Value = rowSums(across(matches("_(idw|nn|tps|ok)$")) * setNames(as.list(weight_values), sub("weight_", "", names(weight_values)))))
#
###Data frame with interpolated parameter values:
(interp_data <- interp_data %>% #Site_Grid_df %>% 
    left_join(as.data.frame(ens_Site) %>% dplyr::select(PGID, Pred_Value) %>% 
                group_by(PGID) %>% arrange(desc(Pred_Value)) %>% slice(1)) %>%
    dplyr::rename(!!paste0(Param_name,"_ens") := Pred_Value))
#Add interpolated data back to Site_grid sf object 
(Site_Grid_interp <- left_join(Site_Grid, interp_data))
#
#Plot of binned interpolate values for rough comparison
ggplot()+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_ens"))))+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), axis.text =  element_text(size = 16))+
  scale_color_viridis_b(direction = -1)
#Map of interpolated values:
ggplot()+
  geom_sf(data = Site_area, fill = "white")+
  geom_sf(data = Site_Grid_interp, aes(color = !!sym(paste0(Param_name,"_ens"))))+
  scale_to_use +
  geom_sf(data = FL_outline)+
  geom_point(data = WQ_summ, aes(Longitude, Latitude, fill = Working_Param), shape = 21, size = 4.5, color = "white")+#color = "black", size = 3.5)+
  scale_fill_viridis_c(option = "magma", direction = -1)+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA), 
        axis.title = element_text(size = 18), axis.text =  element_text(size = 16))+
  ggtitle(paste0("Ensemble: Mean ", Param_name)) +
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"], st_bbox(Site_area)["xmax"]),
           ylim = c(st_bbox(Site_area)["ymin"], st_bbox(Site_area)["ymax"]))
#
##END OF ENSEMBLE
#