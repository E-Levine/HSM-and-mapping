##Code to set up folders for HSM
#
##This code should be run before any other files or setup is completed.
##Determine if the site already exists locally. If so, make sure to use the same code and identify which version is being created. 
#
##This code does NOT need to be run if updating an existing site/version. 
#
##Establish the site code and version for this project.
#
Site_code <- c("SL") #two-letter site code used throughout for identifying files
Version <- c("v1") #current version number of the model for the specified site
#
#
##Code checks if folders exist before creating folders for organizing by site and version. 
if(!dir.exists(paste0("Data/",Site_code, "_", Version))){dir.create(paste0("Data/",Site_code, "_", Version))}
if(!dir.exists(paste0("Output/",Site_code, "_", Version))){dir.create(paste0("Output/",Site_code, "_", Version))}
