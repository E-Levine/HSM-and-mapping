##Code to set up folders for HSM and separate KML files as needed
#
##This code should be run before any other files or setup is completed.
##Determine if the site already exists locally. If so, make sure to use the same code and identify which version is being created. 
#
##This code does NOT need to be run if updating an existing site/version. 
#
##Establish the site code and version for this project. (Required for both code chunks.)
Site_Code <- c("SS") #two-letter site code used throughout for identifying files
Version <- c("v1") #current version number of the model for the specified site
source("HSM code/Functions/HSM_Functions.R")
#
#Proceed to folder setup, file separation, and parameter assignment as needed
#
####Folder set up####
##Function checks if folders exist before creating folders for organizing by site and version. 
create_folders(Site_Code, Version)
#
#
#
#
#
#
####File separation####
#
##Ensure the [KML]_all file has been copied to Reference Files/KML/PreProcessing before proceeding and named using the Site Code and version ID + "_all"
##Run the following code if site and section polygons need to be separated (i.e. One file from Google Earth)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(sf, tidyverse, stringr, install = TRUE) #Mapping and figures
#
#Does file need to be separated or existing files gathered? 
#If requiring separation of one file into one per section, status = "separated"; if existing, status = list of names of existing files from Reference files/KML.
#If only using one file and separation is not needed, enter 'one_file = "Y"' in KML_separation function
KML_status <- c("SS", "SS-Central", "SS-North", "SS-South")
#
KML_separation(KML_status, one_file = "Y")
##If file was separated, confirm all polygons needed are listed in the output. If any are missing, they are missing from the "all" file. 
#Confirm names are accurate, updated as needed within the local directory or return to the KML file creation step.
#
#
####Parameter assignment####
#
##Creation of reference file for completing the model
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(readxl, tidyverse, openxlsx, install = TRUE)
#
##List of information required (Y/N) 
#Set if the information is needed for the current iteration of Site*Version. Information needs to already exist and be defined within the base Setup_data file.
Long_Names <- c("Y")
Order_of_Sections <- c("Y")
Order_of_Parameters <- c("Y")
FL_Oysters <- c("Y")
Seagrass <- c("Y")
Navigation_channels <- c("Y")
Shellfish_Harvest_Area_Designations <- c("N")
Aquaculture_lease <- c("N")
#
##Gather required data.##Code does not need to be changed. 
Gather_setup_data(Long_Names, Order_of_Sections, Order_of_Parameters, FL_Oysters, Shellfish_Harvest_Area_Designations, Navigation_channels, Aquaculture_lease)
#Check output list of all possible data frames. Confirm data needed is listed. If changes are needed, update Y/N status above, then re-run Gather_setup_data()
#
##Return list of data included and excluded from model.
Identify_dataframes(df_list)
#
#