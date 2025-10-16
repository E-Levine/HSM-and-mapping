####Water Quality Data Compilation and Cleaning###
#
##Compile WQ data - Select parameters, combine station and WQ data
##If other data sources are needed. Submit an issue/request to repo admin
##Output of cleaned data
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, tidyverse, readxl, writexl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               sf, raster, spData, 
               tmap, tmaptools, htmltools, htmlwidgets,
               install = TRUE) 
#
source("Code/WQ_functions.R")
#
####Compilation setup####
#
#Set parameters - run for each data source type
Site_code <- c("WC")       #Two letter estuary code
Version <- c("v1")         #Version ID for the model data will be used in
Data_source <- c("Portal") #Source of data: "Portal", "WA" , or "FIM"
#
#Years of data (used in file names):
Start_year <- c("2020")
End_year <- c("2024")
#
#If working with FIM data: Skip to "Estuary area", then to Mapping of stations 
#
####Load files####
#
##Read in raw data files
read_raw_data_files(Site_code, Data_source, list(c(2020, 2024)))
#
#
##Estuary area  
Estuary_area <- st_read(paste0("../",Site_code,"_", Version, "/Data/Layers/KML/", Site_code, ".kml"))
plot(Estuary_area[2])
#
##State Outline
FL_outline <- st_read("../Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#END OF SECTION
#
####Select data columns####
#
##Select location data
Location_data_clean <- process_location_data()
#
#
##Select results data
Results_data_clean <- process_results_data(Data_source, Results_data)
#
#
#
#END OF SECTION
#
####Combine data by station - limit to desired parameters####
#
#combine data
Combined <- create_combined(Data_source, Location_data_clean, Results_data_clean)
#
###Filter combined file to only include specified characteristics 
#List of possible characteristics to select from
unique(Combined$CharacteristicName)
unique(Combined$Characteristic)
#
#Assemble list of characteristics to KEEP - Portal data
Characters <- c("Salinity", "Temperature, water", "Depth, bottom", "Depth, Secchi disk depth", "Temperature, air, deg C", "Turbidity", 
                "Conductivity", "Specific conductance", "pH", "Dissolved oxygen (DO)", "Dissolved oxygen saturation", 
                "Chlorophyll a, corrected for pheophytin", "Chlorophyll a", "Total dissolved solids", "Total suspended solids", "Zooplankton", 
                "Diatoms", "Stream flow, instantaneous", "Flow, severity (choice list)", "Stream stage", "Flow", "Stream flow, mean. Daily")
#
#Filter to only the desired characteristics and check remaining list
if(Data_source == "Portal"){
  Combined_filtered <- Combined %>% filter(CharacteristicName %in% Characters) %>% rename(Result_Unit = `ResultMeasure/MeasureUnitCode`)
} else if (Data_source == "WA" | Data_source == "FIM"){
  Combined_filtered <- Combined 
}
#
##Correct basic typos in units/provide clarification - mg/L, mg/m3 = ug/L
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "mg/l", "mg/L")
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "mg/m3", "ug/L")
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "ug/l", "ug/L")
Combined_filtered$Result_Unit <- str_replace(Combined_filtered$Result_Unit, "ft3/sec", "cfs")
Combined_filtered$ResultMeasureValue <- str_replace(Combined_filtered$ResultMeasureValue, "\\*Non-detect", "NA")
#
#Confirm list of characters selected. - skip for WA or FIM data
unique(Combined_filtered$CharacteristicName)
#
#
#
#
####Map of stations and limitation of stations to estuary area####
#
Estuary_data <- transform_crop_wq(Data_source, Combined_filtered, Estuary_area)
Stations_map <- mapping_wq_data_sf(Data_source, Estuary_data, Estuary_area, FL_outline, Site_code)
Stations_map$Static_plot #Static map
Stations_map$Interactive_map #Interactive map
#
##Run following lines only if issue with Loop above. If there is an issue with "Loop", polygons are overlapping somewhere. 
#sf_use_s2(FALSE)
#Estuary_data <- WQ_sp[lengths(st_intersects(Estuary_area, st_as_sf(WQ_sp)))>0,]
#sf_use_s2(TRUE)
#
#Save widget map:
saveWidget(Stations_map$Interactive_map, paste0("Maps/", Site_code, "_", Data_source,"_WQ_stations_", Start_year, "_", End_year, "_widget.html"))
#
#
####Clean and save parameter data####
#
process_and_save_data(Estuary_data, Data_source, Site_code, Start_year, End_year)
#
#
#