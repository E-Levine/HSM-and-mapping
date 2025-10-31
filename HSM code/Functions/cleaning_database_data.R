##Database data formattting for HSM ground truthing
#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, openxlsx)
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
      srvy_df <- SRVYSH %>% 
        left_join(SRVY %>% dplyr::select(SampleEventID, QuadratID), by = "QuadratID") 
      # SHs
      srvy_sh_summ <- srvy_df %>%
        group_by(SampleEventID) %>% 
        rstatix::get_summary_stats(type = "mean_sd")
      # Counts
      Sizes <- srvy_df %>% 
        dplyr::mutate(Size = as.factor(case_when(ShellHeight < 25 ~ "Spat", 
                                                 ShellHeight >= 75 ~ "Legal", 
                                                 ShellHeight >= 25 & ShellHeight < 75 ~ "Adult" , 
                                                 TRUE ~ "Z")))
      # Combine totals and size counts
      srvy_counts <- left_join(Sizes %>% 
                                 group_by(SampleEventID, Size) %>%
                                 summarise(Count = n()) %>%
                                 pivot_wider(names_from = Size, values_from = Count),
                               Sizes %>% 
                                 group_by(SampleEventID) %>%
                                 summarise(Total = n()),
                               by = "SampleEventID")
      #
      assign("SRVYSH_summary", srvy_sh_summ, envir = globalenv())
      assign("SRVYCounts_summary", srvy_counts, envir = globalenv())
      cat("'SRVYSH_summary' created\n'SRVYCounts_summary' created\n")
    } else {
      cat("SRVYSH not found; skipping SRVYSH summary and counts.\n")
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
    #
    if ("SHBGSH" %in% existing_objects) {
      # Get SH summary by SampleEventID
      suppressWarnings(shbg_Sizes <- SHBGSH %>% 
                         left_join(SHBG %>% dplyr::select(SampleEventID, QuadratID), by = "QuadratID") %>%
                         mutate(across(c(ShellHeight), as.numeric)) %>%
                         dplyr::mutate(Size = as.factor(case_when(ShellHeight < 25 ~ "Spat", 
                                                                  ShellHeight >= 75 ~ "Legal", 
                                                                  ShellHeight >= 25 & ShellHeight < 75 ~ "Adult" , 
                                                                  TRUE ~ "Z"))))
      # SH means
      shbg_sh_summ <- shbg_Sizes %>%
        group_by(SampleEventID) %>% 
        rstatix::get_summary_stats(ShellHeight, type = "mean_sd")
      # SH counts
      shbg_counts <- left_join(shbg_Sizes %>% 
                                 group_by(SampleEventID, Size) %>%
                                 summarise(Count = n()) %>%
                                 pivot_wider(names_from = Size, values_from = Count),
                               shbg_Sizes %>% 
                                 group_by(SampleEventID) %>%
                                 summarise(Total = n()),
                               by = "SampleEventID")
      #
      assign("SHBGSH_summary", shbg_sh_summ, envir = globalenv())
      assign("SHBGSHCounts_summary", shbg_counts, envir = globalenv())
      
      cat("'SHBGSH_summary' created\n'SHBGSHCounts_summary' created")
    } else {
      cat("SHBGSH not found; skipping SHBGSH summary and counts\n")
    }
    #
  }
  #
}
#
summarize_shellBudget_data()
#
#
##Save cleaned data 
output_summary_tables <- function(Site, VersionNumber){
  #
  # Get all objects in the global environment that end with "_summary"
  summary_objects <- ls(pattern = "_summary$", envir = .GlobalEnv)
  #
  # Create a new workbook
  wb <- createWorkbook()
  #
  # Add each summary object as a sheet
  for (obj_name in summary_objects) {
    obj <- get(obj_name, envir = .GlobalEnv)
    addWorksheet(wb, obj_name)
    writeData(wb, obj_name, obj)
  }
  #
  # Base filename
  file_path <- paste0(Site, "_", VersionNumber,"/Output/Data files/")
  base_filename <- paste0(Site,"_", VersionNumber,"_database_summary.xlsx")
  #
  # Check if file exists and append date if it does
  if (file.exists(paste0(file_path,base_filename))) {
    date_suffix <- format(Sys.Date(), "%Y-%m-%d")
    filename <- paste0(Site,"_", VersionNumber,"database_summary_", date_suffix, ".xlsx")
  } else {
    filename <- base_filename
  }
  #
  # Save the workbook
  saveWorkbook(wb, paste0(file_path,filename), overwrite = TRUE)
  
  # Optional: Print confirmation
  cat("Workbook saved as'", filename, "' with sheets:\n", paste(summary_objects, collapse = "\n "), "\n")
  #
}
#
output_summary_tables(Site_Code, Version)



