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
pacman::p_load(readxl, tidyverse, openxlsx, install = TRUE)
#
##List of information required (Y/N) 
#Set if the information is needed for the current iteration of Site*Version. Information needs to already exist and be defined within the base Setup_data file.
Long_Names <- c("Y")
Order_of_Sections <- c("Y")
Order_of_Parameters <- c("Y")
Shellfish_Harvest_Area_Designations <- c("N")
#
if(file.exists("Reference files/Setup_data.xlsx")){
  #Load sheets if file exists and if data is required. If data isn't required, state as such.
  if(Long_Names == "Y"){Names <- read_excel("Reference files/Setup_data.xlsx", sheet = "Long_Names") %>% as.data.frame()} else {Names <- "Long names not needed"}
  if(Order_of_Sections == "Y"){Sections <- read_excel("Reference files/Setup_data.xlsx", sheet = "Section_Order") %>% as.data.frame() %>% mutate(Order = as.integer(Order))} else {Sections <- "Section order not needed"}
  if(Order_of_Parameters == "Y"){Parameters <- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Order") %>% as.data.frame() %>% mutate(Priority = as.integer(Priority))} else {Parameters <- "Parameter order not needed"}
  if(Shellfish_Harvest_Area_Designations == "Y"){SHAreas  <- read_excel("Reference files/Setup_data.xlsx", sheet = "Testing")} else {SHAreas <- "Test not needed"}
  print("Excel file found. Required sheets loaded into data frames.")
} else {
  print("Excel file not found. Please find the data file and check for required data.")
} 
#
#
##Limit data to site data and designated data
if(Long_Names == "Y"){Names <- Names %>% subset(Designation == Site_Code | Needed == "Y")} else {Names <- Names}
if(Order_of_Sections == "Y"){Sections%>% subset(Site == Site_Code & !is.na(Order))} else {Sections <- Sections}
if(Order_of_Parameters == "Y"){Parameters %>% subset(!is.na(Priority))} else {Parameters <- Parameters}
if(Shellfish_Harvest_Area_Designations == "Y"){SHAreas <- "In progress. Code needs to be updated."} else {SHAreas <- SHAreas}
#
#
###Function to check list of objects, identify which are data frames or text.
#List of all possible data frames
df_list <- list("Long_Names" = Names, 
                "Section_Order" = Sections, 
                "Parameter_Order" = Parameters, 
                "SHA" = SHAreas)
#
Identify_dataframes <- function(object_list){
  #Empty object to fill
  dataframes <- ls()
  notdataframes <- ls()
  #Identify objects that are data frames
  df_types <- sapply(object_list, is.data.frame)
  #Return names of objects that are data frames 
  dataframes <<- names(df_types[df_types == TRUE])
  notdataframes <<- names(df_types[df_types == FALSE])
}
#
##Return list of data included and excluded from model.
Identify_dataframes(df_list)
paste("These data are to be included in the model: ", paste(unlist(dataframes), collapse = ", "))
paste("These data are NOT included in the model: ", paste(unlist(notdataframes), collapse = ", "))
#
##Write data used to the version tracking files (Data/Site_Code_Version).
selected_data <- df_list[(names(df_list) %in% dataframes == TRUE)]
write.xlsx(selected_data, file = paste0(Site_Code, "_", Version, "/Data/", Site_Code, "_", Version, "_model_setup.xlsx"), sheetName = names(selected_data))
#