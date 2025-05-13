##Functions used for habitat suitability mapping project
##
####SetUp_Folders####
create_folders <- function(Site_Code, Version) {
  # Create main folder
  main_folder <- paste0(Site_Code, "_", Version)
  if (!dir.exists(main_folder)) {
    dir.create(main_folder) 
    print(paste(Site_Code, " version ", Version, " folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " folder already exists."))
    }
  # Create Data folder
  data_folder <- paste0(main_folder, "/Data")
  if (!dir.exists(data_folder)) {
    dir.create(data_folder) 
    print(paste(Site_Code, " version ", Version, " Data folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Data folder already exists."))
    }
  # Create Output folder
  output_folder <- paste0(main_folder, "/Output")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder) 
    print(paste(Site_Code, " version ", Version, " Output folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Output folder already exists."))
    }
  # Create KML folder
  kml_folder <- paste0("Reference files/KML/KML_", main_folder)
  if (!dir.exists(kml_folder)) {
    dir.create(kml_folder)  
    print(paste(Site_Code, " version ", Version, " KML folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " KML folder already exists."))
    }
}
#
####File separation####
#
KML_separation <- function(Status_of_KML){
  if(length(Status_of_KML) == 1){
    kml_file <- st_read(paste0("Reference files/KML/PreProcessing/", Site_Code, "_", Version, "/", Site_Code,"_all.kml")) #Load file
    Polygons <- kml_file %>% group_by(Name) %>% summarise(count = n()) #Identify all polygons
    filelist <- list()
    for (i in 1:nrow(Polygons)) {
      name <- Polygons$Name[1]
      polygon <- kml_file %>% filter(Name == Polygons$Name[i]) #Get the current unique polygon
      filename <- paste0("Reference files/KML/", Site_Code, "_", Version, "/",Polygons$Name[i], ".kml") # Write to separate KML file
      st_write(polygon, filename, driver = "kml", append = FALSE)
      filelist[i] <- paste("Polygon", name, "to", filename)
      }
    return(filelist)
  } else {
    search_strings  <- Status_of_KML
    # List of available files 
    files <- list.files("Reference files/KML/", full.names = TRUE)
    #
    for (file in files) {
      # Get the filename without the path
      filename <- basename(file)
      
      # Check if any of the search strings are in the filename
      if (any(str_detect(filename, search_strings))) {
        # Copy the file to locationB
        file.copy(file, file.path(paste0("Reference files/KML/", Site_Code, "_", Version, "/"), filename))
        }
    }
    # Print a message indicating completion
    message(paste0("Files copied from [Reference files/KML/] to [Reference files/KML/", Site_Code, "_", Version, "/]."))
  }
}
#
#
#
####Parameter Assignment####
##Function to read excel files and limit to required setup data
Gather_setup_data <- function(Long_Names, Order_of_Sections, Order_of_Parameters, FL_Oysters, Shellfish_Harvest_Area_Designations){
if(file.exists("Reference files/Setup_data.xlsx")){
  #Load sheets if file exists and if data is required. If data isn't required, state as such. Limit to data required for model.
  if(Long_Names == "Y"){Names <<- suppressWarnings(read_excel("Reference files/Setup_data.xlsx", sheet = "Long_Names") %>% as.data.frame() %>% subset(Designation == Site_Code | Is_Used == "Y"))} else {Names <<- "Long names are not needed."}
  if(Order_of_Sections == "Y"){suppressWarnings(Sections <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Section_Order") %>% as.data.frame() %>% mutate(Order = as.integer(Order)) %>% subset(Site == Site_Code & !is.na(Order)))} else {Sections <<- "Section order is not needed."}
  if(Order_of_Parameters == "Y"){suppressWarnings(Parameters <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Order") %>% as.data.frame() %>% mutate(Priority = as.integer(Priority)) %>% subset(!is.na(Priority)))} else {Parameters <<- "Parameter order is not needed."}
  if(FL_Oysters == "Y"){suppressWarnings(Oysters <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Scoring") %>% as.data.frame() %>% mutate(Score = as.numeric(Score)) %>% subset(Parameter == "Oysters" & !is.na(Score)))} else {Parameters <<- "'Oyster Beds in Florida' layer is not needed."}
  if(Shellfish_Harvest_Area_Designations == "Y"){suppressWarnings(SHAreas  <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Scoring") %>% as.data.frame() %>% mutate(Score = as.numeric(Score)) %>% subset(Parameter == "SHA_Class" & !is.na(Score)))} else {SHAreas <<- "Shellfish Harvest Areas is not needed."}
  df_list <<- list("Long_Names" = Names, 
                   "Section_Order" = Sections, 
                   "Parameter_Order" = Parameters, 
                   "FLOysters" = Oysters,
                   "SHAs" = SHAreas)
  return(list("Excel file found. Required sheets loaded into data frames.", df_list))
} else {
  return(print("Excel file not found. Please find the data file and check for required data."))
} 
  }
#
##Function to check list of objects, identify which are data frames or text.
Identify_dataframes <- function(object_list){
  #Empty object to fill
  dataframes <- list()
  notdataframes <- list()
  #Identify objects that are data frames
  df_types <- sapply(object_list, is.data.frame)
  #Return names of objects that are data frames 
  dataframes <- names(df_types[df_types == TRUE])
  notdataframes <- names(df_types[df_types == FALSE])
  selected_data <<- object_list[(names(object_list) %in% dataframes == TRUE)]
  t_line1 <- paste("These data are to be included in the model: ", paste(unlist(dataframes), collapse = ", "))
  t_line2 <- paste("These data are NOT included in the model: ", paste(unlist(notdataframes), collapse = ", "))
  dfoutput <<- sprintf("%s\n%s", t_line1, t_line2)
  cat(dfoutput)
  selected_data[["Data not included"]] <<- notdataframes
}
