####Water Quality Data Combination###
#
##Combination of WQ data into single file
##Output of selected data and map
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_unlock()
pacman::p_load(plyr, tidyverse, readxl, writexl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               sf, raster, spData, nngeo,
               tmap, tmaptools, htmltools, htmlwidgets,
               install = TRUE) 
#
source("Code/WQ_functions.R")
#
#
####Setup - specs####
Estuary_code <- c("SL") #Two letter estuary code
Data_sources <- c("Portal", "FIM") #List of all data sources to combine: "Portal", "FIM"
Project_codes <- c("SLAllWQ", "TEST") #List of any project code IDs used (file name portion before years)
Data_selection_method <- c("bounding_box", NA) #List of data selection methods used: NA, "selected_buffer", "closest_selected", "bounding_box", "name_selected"
Final_code <- c("SLHSI") #Short code to distinguish what data is compiled to be used for 
#Years of data selected (same in all file names - request update if different years needed)
Start_year <- c("2000")
End_year <- c("2023")
#
##Any notes to be add to metadata of output file. All should be entered as string of text
Data_note <- c("Data collected for testing HSM modeling.")
#
#
#END OF SET UP
#
#
####Load files
Data_gather <- load_all_files()
Data_gather$summary
#
#
#
####Combine and output data file
Final_data <- combine_data_sources()
Final_data
#
#
#
#