##Code to set up folders for HSM and separate KML files as needed
#
##This code should be run before any other files or setup is completed.
##Determine if the site already exists locally. If so, make sure to use the same code and identify which version is being created. 
#
##This code does NOT need to be run if updating an existing site/version. 
#
##Establish the site code and version for this project. (Required for both code chunks.)
#
Site_code <- c("SL") #two-letter site code used throughout for identifying files
Version <- c("v1") #current version number of the model for the specified site
#
#
####Folder set up####
##Code checks if folders exist before creating folders for organizing by site and version. 
if(!dir.exists(paste0(Site_code, "_", Version))){dir.create(paste0(Site_code, "_", Version)) 
  print(paste(Site_code, " version ", Version, " folder created."))} else {print(paste(Site_code, " version ", Version, " folder altready exists."))}
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
kml_file <- st_read(paste0("Reference files/KML/", Site_code, "_", Version, "/", Site_code,"_all.kml")) #Load file
Polygons <- kml_file %>% group_by(Name) %>% summarise(count = n()) #Identify all polygons
for (i in 1:nrow(Polygons)) {
  polygon <- kml_file %>% filter(Name == Polygons$Name[i]) #Get the current unique polygon
  filename <- paste0("Reference files/KML/", Site_code, "_", Version, "/",Polygons$Name[i], ".kml") # Write to separate KML file
  st_write(polygon, filename, driver = "kml")
  print(paste("Polygon", i, "to", filename))
}
##Confirm all polygons needed are listed in the output. If any are missing, they are missing from the "all" file. 
#Confirm names are accurate, updated as needed within the local directory or return to the KML file creation step.
#
#