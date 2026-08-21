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
               install = TRUE) #Mapping and figures
#
#
source("HSM code/Functions/HSM_Creation_Functions.R")
#
#Working parameters - to be set each time a new site or version is being created. Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SA") #two-letter site code
Version <- c("v1") #Model version
State_Grid <- c("C1")
Alt_Grid <- c("C2") #Two-letter StateGrid ID, enter NA if no secondary StateGrid needed
#
##Parameters
Sections_designated <- c("Y") #Y/N are section designations used
#
#
#
####Load base information and assign Site/Sections####
#
#Loads Excel file data information as designated during version set up:
load_working_info(Site_Code, Version)
#
#Assign site and section designations to grid cells. Data and shapefile can be saved using Save_data = "Y" and save a figure using Save_figure = "Y".
get_base_grid(Site_Code, Version, Sections_designated, Save_data = "N", Save_figure = "N")
#
#
###END OF SECTION
#
#
####Load and apply polygon layers####
#
#Refer to Parameter_Order Excel sheet, Parameter column for names to reference data:
df_list[3]
#Use Parameter name and date range identify possible data and to gather data needed
find_folder_names("Oysters")
#
#If folders match, load shape files:
Start_date <- "2020-01-01"
End_date <- "2024-12-31"
#
# Apply data to grid cells:
# Oysters
load_matching_shp(Section_grid, "Oysters", StartDate = Start_date, EndDate = End_date)
modelGrid_sp <- apply_polygon_overlap(modelGrid = Section_grid, 
                                      files_loaded = files_loaded, 
                                      dataColumn =  "OYSTER",
                                      Parameter_name = "Oysters",
                                      fillValue = "Live", 
                                      df_list = df_list)
#
rm(list = ls(pattern = "^Oyster_"))
#
#
#
# Seagrass
#
find_folder_names("Seagrass")
Start_date <- "2024-01-01"
load_matching_shp(Section_grid, "Seagrass", StartDate = Start_date, EndDate = End_date)
# Repair shapefile as needed...
check_geometry(Seagrass_202412)
repair <- repair_geometry(Seagrass_202412,
                          action = "repair")
Seagrass_202412 <- repair
#
modelGrid_sp <- apply_polygon_overlap(modelGrid = modelGrid_sp, 
                                      files_loaded = files_loaded, 
                                      dataColumn =  "SEAGRASS", 
                                      Parameter_name = "Seagrass",
                                      fillValue = "Present", 
                                      df_list = df_list)
#
rm(list = ls(pattern = "^Seagrass_"))
#
#
#
#
####Load and apply buffer layers####
#
# Oyster buffers
find_folder_names("Oysters")
load_matching_shp(Section_grid, "Oysters", StartDate = "2023-01-01", EndDate = "2024-12-31")
modelGrid_sp2 <- apply_distance_buffers(modelGrid = modelGrid_sp, 
                                        files_loaded = files_loaded, 
                                        LayerName = "Buffers",
                                        dataColumn =  "OYSTER", 
                                        buffer_method="fixed",
                                        buffer_breaks = c(200, 400, 1000), 
                                        df_list = df_list)
#
# Plot to check data application 
# Overall view
ggplot(st_as_sf(modelGrid_sp2))+
  geom_sf(aes(fill = Buff24))+
  geom_sf(aes(color = Oyst24), fill = NA)
# Zoom
ggplot(st_as_sf(modelGrid_sp2))+
  geom_sf(aes(fill = Buff24))+
  geom_sf(aes(color = Oyst24), fill = NA)+
  #scale_color_discrete()+
  coord_sf(xlim = c(-85.8, -85.7),
           ylim = c(30.22, 30.27))
#
rm(list = ls(pattern = "^Oyster_"))
#
#
# Navigational channel buffers
find_folder_names("Channels")
load_matching_shp(Section_grid, "Channels", StartDate = "2023-12-01", EndDate = "2024-12-31")
#
files_loaded[1] <- "Waterways"
names(files_loaded) <- "Waterways"
# Reference table:
(Reference_t <- df_list[[12]] %>%
    filter(Curve == "Channel buffer") %>%
    mutate(Param = str_replace_all(Param, "[\r\n]+", "")) %>%
    dplyr::select(-Date_Updated))
#
modelGrid_sp3 <- apply_distance_buffers(modelGrid = modelGrid_sp2, 
                                        files_loaded = files_loaded, 
                                        LayerName = "Channels",
                                        dataColumn =  "TYPE", 
                                        buffer_method ="lookup",
                                        Ref_table = Reference_t,
                                        buffer_multiplier = 100,
                                        buffer_units = "keep",
                                        df_list = df_list)
#
# Plot to check data application 
ggplot(st_as_sf(modelGrid_sp3))+
  geom_sf(aes(fill = Chnl24))+
  geom_sf(data = st_as_sf(Waterways_202401), aes(color = TYPE), linewidth = 1.5)+
  scale_color_discrete()+
  coord_sf(xlim = c(-82.75, -82.738),
           ylim = c(29.005, 29.011))

#
#
# Divide Chnl column by designation:
modelGrid_sp4 <- split_column_by_value(modelGrid_sp3, "Chnl24", remove_original = TRUE)
head(modelGrid_sp4@data)
#
#
#
#

####Output layer with data applied ####
#
# Save model datalayers 
save_model_data(Site_Code, Version, modelGrid_sp4)
#
#
#
#