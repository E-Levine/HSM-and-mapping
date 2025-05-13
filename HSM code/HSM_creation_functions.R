##Functions used in habitat suitability mapping project 2_HSM_creation code file
##
#
####Grid loading and cropping####
#
##load setup information
load_working_info <- function(Site_Code, Version){
  #Combine Site and version information
  Site_version <- paste0(Site_Code, "_", Version)
  #Load set up information
  filename <- paste0(Site_version, "/Data/", Site_version, "_model_setup.xlsx") 
  sheets <- excel_sheets(filename)
  df_list <<- lapply(sheets, function(sheet){
    read_excel(filename, sheet = sheet)
  })
  names(df_list) <- sheets
  return(df_list)
}