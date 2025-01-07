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
#
#Proceed to folder setup, file separation, and parameter assignment as needed
#
####Folder set up####
##Code checks if folders exist before creating folders for organizing by site and version. 
if(!dir.exists(paste0("Data/",Site_Code, "_", Version))){dir.create(paste0("Data/",Site_Code, "_", Version))}
if(!dir.exists(paste0("Output/",Site_Code, "_", Version))){dir.create(paste0("Output/",Site_Code, "_", Version))}
if(!dir.exists(paste0("Reference files/KML/",Site_Code, "_", Version))){dir.create(paste0("Output/",Site_Code, "_", Version))}
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
Polygons <- kml_file %>% group_by(Name) %>% summarise(count = n()) #Identify all polygons
for (i in 1:nrow(Polygons)) {
  polygon <- kml_file %>% filter(Name == Polygons$Name[i]) #Get the current unique polygon
  filename <- paste0("Reference files/KML/", Site_Code, "_", Version, "/",Polygons$Name[i], ".kml") # Write to separate KML file
  st_write(polygon, filename, driver = "kml")
  print(paste("Polygon", i, "to", filename))
}
##Confirm all polygons needed are listed in the output. If any are missing, they are missing from the "all" file. 
#Confirm names are accurate, updated as needed within the local directory or return to the KML file creation step.
#
#
####Parameter assignment####
#
##Creation of reference file for completing the model
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(readxl, tidyverse, install = TRUE)
#
##List of information required (Y/N) 
#Set if the information is needed for the current iteration of Site*Version
Long_Names <- c("Y")
Order_of_Sections <- c("Y")
Shellfish_Harvest_Area_Designations <- c("N")
#
if(file.exists("Reference files/Setup_data.xlsx")){
  #Load sheets if file exists and if data is required. If data isn't required, state as such.
  if(Long_Names == "Y"){Names <- read_excel("Reference files/Setup_data.xlsx", sheet = "Long_Names")} else {Names <- "Not needed"}
  if(Order_of_Sections == "Y"){Section_Order <- read_excel("Reference files/Setup_data.xlsx", sheet = "Section_Order")} else {Section_Order <- "Not needed"}
  if(Shellfish_Harvest_Area_Designations == "Y"){SHA  <- read_excel("Reference files/Setup_data.xlsx", sheet = "Testing")} else {SHA <- "Not needed"}
  print("Excel file found. Required sheets loaded into data frames")
} else {
  if(Long_Names == "Y"){Names <- data.frame(Type = character(), Designation = character(), LongName = character(), stringsAsFactors = FALSE)} else {Names <- "Not needed."}
  if(Order_of_Sections == "Y"){Section_Orders <- data.frame(Site = character(), Section = character(), Order = integer(), stringsAsFactors = FALSE)} else {Section_Orders <- "Not needed."}
  if(Shellfish_Harvest_Area_Designations == "Y"){SHA <- data.frame()} else {SHA <- "Not needed."}
  print("Excel file not found. Required dataframe created.")
}
#
##Check data frames for required data.
Names
Section_Order
SHA
#
##Add required data if missing. 
#
##Write data used to the version tracking files (Data/Site_Code_Version).
#
##Add any missing data back to the primary SetUp file for consistency. 
