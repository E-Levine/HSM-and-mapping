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
  return(cat("Data objects created:\n", matching_sheets, "\n"))
  #
}

read_database_data(Site_Code, Version, dataTypes = c("SRVY", "SHBG"))
#
##Clean data
summarize_survey_data <- function(){
  #
  # Check for SRVY data
  objects_to_check <- c("SRVY", "SRVYSH")
  existing_objects <- objects_to_check[sapply(objects_to_check, exists, envir = .GlobalEnv)]
  if(length(existing_objects) == 0){
    stop("No survey data found.")
  } else {
    cat("Existing objects to be summarized:", paste(existing_objects, collapse = ", "), "\n")
    #
    if ("SRVY" %in% existing_objects) {
      # Get SRVY lat and long with counts
      srvy_ll <- full_join(SRVY, 
                           SampleEvent %>% filter(grepl("SRVY", SampleEventID)),
                           by = "SampleEventID")
      #
      # Summarize live, dead, legals by SampleEventID
      suppressWarnings(srvy_summ <- srvy_ll %>%
        mutate(across(c(NumLive, NumDead, NumLegal), as.numeric)) %>%
        group_by(SampleEventID) %>% 
        rstatix::get_summary_stats(NumLive, NumDead, NumLegal, type = "mean_sd") %>%
        pivot_wider(names_from = variable, values_from = c(mean, sd), names_glue = "{variable}_{.value}"))
      #
      assign("SRVY_summary", srvy_summ, envir = globalenv())
      cat("'SRVY_summary' created\n")
    } else {
      cat("SRVY not found; skipping SRVY summary.\n")
    }
    
    #
    if ("SRVYSH" %in% existing_objects) {
      # Get SH summary by SampleEventID
      srvy_sh_summ <- SRVYSH %>% 
        left_join(SRVY %>% dplyr::select(SampleEventID, QuadratID), by = "QuadratID") %>%
        group_by(SampleEventID) %>% 
        rstatix::get_summary_stats(type = "mean_sd")
      #
      assign("SRVYSH_summary", srvy_sh_summ, envir = globalenv())
      cat("'SRVYSH_summary' created\n")
    } else {
      cat("SRVYSH not found; skipping SRVYSH summary.\n")
    }
      #
  }
  #
}
#
summarize_survey_data()
#
#
summarize_shellBudget_data <- function(){
  #
  # Check for SRVY data
  objects_to_check <- c("SHBG", "SHBGSH")
  existing_objects <- objects_to_check[sapply(objects_to_check, exists, envir = .GlobalEnv)]
  if(length(existing_objects) == 0){
    stop("No survey data found.")
  } else {
    cat("Existing objects to be summarized:", paste(existing_objects, collapse = ", "), "\n")
    #
    if ("SHBG" %in% existing_objects) {
      # Get SRVY lat and long with counts
      shbg_ll <- full_join(SHBG, 
                           SampleEvent %>% filter(grepl("SHBG", SampleEventID)),
                           by = "SampleEventID")
      #
      # Summarize live, dead, legals by SampleEventID
      suppressWarnings(shbg_summ <- shbg_ll %>%
        dplyr::rename_with(~ str_remove(.x, "Oyster"), everything()) %>%
        mutate(across(c(NumLives, NumDeads), as.numeric)) %>%
        group_by(SampleEventID) %>% 
        rstatix::get_summary_stats(NumLives, NumDeads, type = "mean_sd") %>%
        pivot_wider(names_from = variable, values_from = c(mean, sd), names_glue = "{variable}_{.value}"))
      #
      assign("SHBG_summary", shbg_summ, envir = globalenv())
      cat("'SHBG_summary created\n")
    } else {
      cat("SHBG not found; skipping SHBG summary.\n")
    }
    browser()
    
    #
    if ("SHBGSH" %in% existing_objects) {
      # Get SH summary by SampleEventID
      shbg_sh_summ <- SHBGSH %>% 
        left_join(SHBG %>% dplyr::select(SampleEventID, QuadratID), by = "QuadratID") %>%
        mutate(across(c(ShellHeight), as.numeric)) %>%
        group_by(SampleEventID) %>% 
        rstatix::get_summary_stats(ShellHeight, type = "mean_sd")
      #
      assign("SHBGSH_summary", shbg_sh_summ, envir = globalenv())
      cat("'SHBGSH_summary' created\n")
    } else {
      cat("SHBGSH not found; skipping SHBGSH summary.\n")
    }
    #
  }
  #
}
#
summarize_shellBudget_data()
#
##Add counts <25, 25-75, >75 to both SH tables
#
##Save cleaned data 