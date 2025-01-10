##Code to set up folders for HSM and separate KML files as needed
#
##This code should be run before any other files or setup is completed.
##Determine if the site already exists locally. If so, make sure to use the same code and identify which version is being created. 
#
##This code does NOT need to be run if updating an existing site/version. 
#
##Establish the site code and version for this project. (Required for both code chunks.)
Site_Code <- c("SL") #two-letter site code used throughout for identifying files
Version <- c("v1") #current version number of the model for the specified site
source("HSM code/HSM_Functions.R")
#
#Proceed to folder setup, file separation, and parameter assignment as needed
#
####Folder set up####
##Code checks if folders exist before creating folders for organizing by site and version. 
if(!dir.exists(paste0(Site_code, "_", Version))){dir.create(paste0(Site_code, "_", Version)) 
  print(paste(Site_code, " version ", Version, " folder created."))} else {print(paste(Site_code, " version ", Version, " folder already exists."))}
if(!dir.exists(paste0(Site_code, "_", Version,"/Data"))){dir.create(paste0(Site_code, "_", Version, "/Data"))
  print(paste(Site_code, " version ", Version, " Data folder created."))} else {print(paste(Site_code, " version ", Version, " Data folder already exists."))}
if(!dir.exists(paste0(Site_code, "_", Version, "/Output"))){dir.create(paste0(Site_code, "_", Version, "/Output"))
  print(paste(Site_code, " version ", Version, " Output folder created."))} else {print(paste(Site_code, " version ", Version, " Output folder already exists."))}
if(!dir.exists(paste0("Reference files/KML/",Site_code, "_", Version))){dir.create(paste0("Reference files/KML/",Site_code, "_", Version))
  print(paste(Site_code, " version ", Version, " KML folder created."))}  else {print(paste(Site_code, " version ", Version, " KML folder already exists."))}
#
#
#
#
#
#
####File separation####
#
##Ensure the KML_all file has been copied to the appropriate folder created in the step above (Site and version) before proceeding.
##Run the following code if site and section polygons need to be separated (i.e. One file from Google Earth)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(sf, tidyverse, install = TRUE) #Mapping and figures
#
kml_file <- st_read(paste0("Reference files/KML/", Site_Code, "_", Version, "/", Site_Code,"_all.kml")) #Load file
#
KML_separation(kml_file)
##Confirm all polygons needed are listed in the output. If any are missing, they are missing from the "all" file. 
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
Shellfish_Harvest_Area_Designations <- c("N")
#
##Gather required data.
Gather_setup_data(Long_Names, Order_of_Sections, Order_of_Parameters, Shellfish_Harvest_Area_Designations)
#
#List of all possible data frames. Secondary check of possible data-confirm Y is enetered above for anyting needed. 
df_list <- list("Long_Names" = Names, 
                "Section_Order" = Sections, 
                "Parameter_Order" = Parameters, 
                "SHA" = SHAreas)
#
##Return list of data included and excluded from model.
Identify_dataframes(df_list)
#
##Write data used to the version tracking files (Data/Site_Code_Version).
write.xlsx(selected_data, file = paste0(Site_Code, "_", Version, "/Data/", Site_Code, "_", Version, "_model_setup.xlsx"), sheetName = names(selected_data))
#