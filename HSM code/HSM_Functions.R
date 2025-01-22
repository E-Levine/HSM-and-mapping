##Functions used for habitat suitability mapping project
##
####SetUp_Folders####
#
###  File separation
#
KML_separation <- function(kml_file){
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
}
#
#
#
###  Parameter Assignment
##Function to read excel files and limit to required setup data
Gather_setup_data <- function(Long_Names, Order_of_Sections, Order_of_Parameters, Shellfish_Harvest_Area_Designations){
if(file.exists("Reference files/Setup_data.xlsx")){
  #Load sheets if file exists and if data is required. If data isn't required, state as such. Limit to data required for model.
  if(Long_Names == "Y"){Names <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Long_Names") %>% as.data.frame() %>% subset(Designation == Site_Code | Needed == "Y")} else {Names <<- "Long names not needed"}
  if(Order_of_Sections == "Y"){Sections <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Section_Order") %>% as.data.frame() %>% mutate(Order = as.integer(Order)) %>% subset(Site == Site_Code & !is.na(Order))} else {Sections <<- "Section order not needed"}
  if(Order_of_Parameters == "Y"){Parameters <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Order") %>% as.data.frame() %>% mutate(Priority = as.integer(Priority)) %>% subset(!is.na(Priority))} else {Parameters <<- "Parameter order not needed"}
  if(Shellfish_Harvest_Area_Designations == "Y"){SHAreas  <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Testing")} else {SHAreas <<- "Test not needed"}
  return(print("Excel file found. Required sheets loaded into data frames."))
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
}
