##Database data formattting for HSM ground truthing
#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl)
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SS") #two-letter site code
Version <- c("v1") #Model version
#
#
##Load database data
read_database_data <- function(Site, VersionNumber, dataTypes){
  #
  # Specify file path
  file_path <- paste0(Site, "_", VersionNumber,"/Data/",Site,"_database_data.xlsx")
  #
  # Get all sheet names
  all_sheets <- excel_sheets(file_path)
  #
  # Filter sheets that contain any of the specified dataTypes
  matching_sheets <- all_sheets[sapply(all_sheets, function(sheet) {
    any(sapply(dataTypes, function(pattern) grepl(pattern, sheet)))
  })]
  #
  # Always include "SampleEvent" if it exists and isn't already in the list
  if ("SampleEvent" %in% all_sheets && !("SampleEvent" %in% matching_sheets)) {
    matching_sheets <- c(matching_sheets, "SampleEvent")
  }
  #
  # Read each matching sheet and assign to an object named after the sheet
  for (sheet in matching_sheets) {
    assign(sheet, read_excel(file_path, sheet = sheet), envir = .GlobalEnv)
  }
  #
  # Return the list of loaded sheet names for confirmation
  return(matching_sheets)
  #
}

read_database_data(Site_Code, Version, dataTypes = c("SRVY", "SHBG"))
#
##Clean data
clean_survey_data <- function(){
  
}
#Get SRVY lat and long with counts
srvy_ll <- full_join(SRVY, 
                     SampleEvent %>% filter(grepl("SRVY", SampleEventID)),
                     by = "SampleEventID")
srvy_summ <- srvy_ll %>%
  mutate(across(c(NumLive, NumDead, NumLegal), as.numeric)) %>%
  group_by(SampleEventID) %>% 
  rstatix::get_summary_stats(NumLive, NumDead, NumLegal, type = "mean_sd") %>%
  pivot_wider(names_from = variable, values_from = c(mean, sd), names_glue = "{variable}_{.value}")

srvy_sh_summ <- SRVYSH %>% 
  left_join(SRVY %>% dplyr::select(SampleEventID, QuadratID)) %>%
  group_by(SampleEventID) %>% 
  rstatix::get_summary_stats(type = "mean_sd")

#
##Save cleaned data 