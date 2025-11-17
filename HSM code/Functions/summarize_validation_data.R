##HSM ground truthing data summary
#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, openxlsx,
               sf, install = TRUE)
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SS") #two-letter site code
Version <- c("v1") #Model version
SurveyYYMM <- c("2312")
#
#
#Load validation data from matching shape files in Output/Shapefiles folder: SiteCode_Version_validation_data
#Currently loads as sfc for potential mapping, can change to df if not mapping later
load_survey_files <- function(SiteCode = Site_Code, VersionNumber = Version, shp_filename = "model_srvys"){
  data_dir <- paste0(SiteCode, "_", VersionNumber, "/Output/Shapefiles/")
  output_name <- paste0(SiteCode, "_", VersionNumber, "_validation_data")  
  #
  # Build match pattern:
  pattern <- paste0("^", SiteCode, ".*", shp_filename, ".*", SurveyYYMM,"\\.shp$")
  # List all matching shape files
  shp_files <- list.files(path = data_dir, pattern = pattern, full.names = TRUE)
  if (length(shp_files) == 0) {
    stop("No shapefiles found matching the pattern.")
  } else {
    #Print list of files loaded:
    message("Files loaded:\n", paste(shp_files, collapse = "\n"))
    #
  
    if (length(shp_files) == 1) {
      shape_obj <- st_read(shp_files[1])
    } else {
      # Read and combine all shape files
      shape_list <- lapply(shp_files, function(file) {
        sf_obj <- st_read(file)

        # Extract Section from file name
        base_name <- basename(file)  # e.g., "ABC123_datalayer.shp"
        section <- str_extract(base_name,"(.*)(?=_model_srvys)")  # Remove _datalayer suffix, e.g., "ABC123"
        sf_obj$Section <- section  # Add Section column
        return(sf_obj)
      })
      shape_obj <- do.call(rbind, shape_list)  
    }
  }
  #Check for and remove duplicate rows (created during split)
  # Install and load necessary packages
  if (!require(data.table)) install.packages("data.table")
  library(data.table)
  # Convert to data.table (handles both sf and data.frame)
  if (inherits(shape_obj, "sf")) {
    object_type <- "sf"
    geometry_col <- attr(shape_obj, "sf_column")
    setDT(shape_obj)  # Converts sf to data.table in place
  } else if (!is.data.table(shape_obj)) {
    object_type <- "not_sf"
    setDT(shape_obj)  # Convert standard data.frame to data.table
    geometry_col <- NULL
  }
  #Get column names
  group_by_cols <- setdiff(names(shape_obj), c("Section", geometry_col))
  # Perform grouping by "PGID" and remove duplicates
  result <- shape_obj[, .SD[1], by = group_by_cols]
  # If it was an sf object, convert back to sf to preserve geometry
  if (object_type == "sf") {
    setDF(result)  # Convert back to data.frame (if needed)
    result_sf <- st_as_sf(result)  # Re-attach sf class
    result <- result_sf  # Update result to the sf object
  }
  #Clean up memory
  rm(shape_obj)  # Remove original object
  gc()    # Run garbage collection, especially for large data
  #assign shp to object
  assign(output_name, result, envir = .GlobalEnv)
}
#
load_survey_files()
str(SS_v1_validation_data)
#
#
### Summarize NumLive, DeadRatio, SpatAdult, Presence by HSMgrp score
clean_survey_data <- function(surveyData){
  # checks
  if (!is.data.frame(surveyData) && !is_tibble(surveyData)) {
    stop("Input 'surveyData' must be a data frame or tibble.")
  }
  #
  if (!any(grepl("NumLive", names(surveyData)))) {
    stop("No column containing 'NumLive' found in surveyData.")
  }
  if (!any(grepl("DeadRatio", names(surveyData)))) {
    stop("No column containing 'DeadRatio' found in surveyData.")
  }
  required_cols <- c("Spat", "Adult", "Legal", "SpatAdult")
  missing_cols <- setdiff(required_cols, names(surveyData))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  #
  # Define na replacement case:
  na_condition <- (surveyData$Spat %in% c(0, NA) & surveyData$Adult %in% c(0, NA) & surveyData$Legal %in% c(0, NA))
  #
  cleaned_data <- surveyData %>% 
    as.data.frame() %>%
    # Rename columns for consistency 
    rename_with(~ sub(".*NumLive.*", "NumLive", .x), matches("NumLive")) %>%
    rename_with(~ sub(".*DeadRatio.*", "DeadRatio", .x), matches("DeadRatio")) %>%
    # Replace 0 with NA when proper
    mutate(DeadRatio = as.numeric(DeadRatio),
           SpatAdult = as.numeric(SpatAdult),
           DeadRatio = if_else(na_condition, NA_real_, DeadRatio),
           SpatAdult = if_else(na_condition, NA_real_, SpatAdult))
  #
  return(cleaned_data)
  #
}
#
validation_data <- clean_survey_data(SS_v1_validation_data)
head(validation_data)
val_df <- validation_data %>%
  group_by(HSMgrp) %>%
  mutate(Live_scale = scale(sqrt(NumLive+0.5))[,1]) %>%
  ungroup()
#
summarize_data <- function(cleanedData){
  # 
  # checks
  if (!is.data.frame(cleanedData) && !is_tibble(cleanedData)) {
    stop("Input 'cleanedData' must be a data frame or tibble.")
  }
  if (!any(grepl("HSMgrp", names(cleanedData)))) {
    stop("No column containing 'HSMgrp' found in cleanedData")
  }
  required_cols <- c("NumLive", "DeadRatio", "SpatAdult", "Presence")
  missing_cols <- setdiff(required_cols, names(cleanedData))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  for (col in required_cols) {
    if (!is.numeric(cleanedData[[col]])) {
      stop(paste("Column", col, "must be numeric for summarization."))
    }
  }
  # Define HSM grps, warn unrepresented grps:
  expected_levels <- c("0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")
  # Check if all expected levels are present (warn if not)
  actual_levels <- unique(na.omit(cleanedData$HSMgrp))
  missing_levels <- setdiff(expected_levels, actual_levels)
  if (length(missing_levels) > 0) {
    warning(paste("Expected HSMgrp levels missing:", paste(missing_levels, collapse = ", "), ". Proceeding with available levels."))
  }
  #
  summarized_data <- suppressWarnings(cleanedData %>%
    mutate(HSMgrp = factor(HSMgrp, levels = expected_levels, ordered = TRUE)) %>%
    group_by(HSMgrp) %>%
    summarise(across(
      all_of(required_cols),
      list(
        n = ~ sum(!is.na(.x)),  # Count non-NA values
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{fn}"
      ),
      .groups = "drop") %>%
    # Reorganize output
    pivot_longer(cols = -HSMgrp, names_to = c("variable", "stat"), names_sep = "_", values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    mutate(min = ifelse(is.infinite(min), NA_real_, min),
           max = ifelse(is.infinite(max), NA_real_, max))) 
  #
  return(summarized_data)
  #
}
#
validation_summary <- summarize_data(validation_data)
#
#
## Plot summaries x = score, y = mean values
validation_summary %>% 
  ggplot(aes(HSMgrp, mean))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(variable~., scales = "free_y")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() + 
  basetheme + FacetTheme
#
validation_summary %>% filter(HSMgrp != "[0.9,1]") %>%
  ggplot(aes(HSMgrp, mean))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(variable~., scales = "free_y")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+ 
  basetheme + FacetTheme
#
validation_summary %>% mutate(range = max-min) %>%
  ggplot(aes(HSMgrp, range))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(variable~., scales = "free_y")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()+ 
  basetheme + FacetTheme
#
basetheme <- theme_bw()+
  theme(axis.title.x = element_text(size = 12, face = "bold", color = "black"), axis.text.x = element_text(size = 11, margin = unit(c(0.5, 0.5, 0, 0.5), "cm")),
        axis.title.y = element_text(size = 12, face = "bold", color = "black"), axis.text.y = element_text(size = 11, margin = unit(c(0, 0.5, 0, 0), "cm")),
        panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line(color = "black"),
        axis.ticks.length = unit(-0.15, "cm"))
FacetTheme <- theme(strip.text.y = element_text(face = "bold", size = 12),
                    strip.background = element_rect(fill = "#CCCCCC"),
                    panel.spacing = unit(0.75, "lines"),
                    strip.text.x = element_text(face = "bold", size = 12))
#
val_df %>% group_by(HSMgrp) %>%
  rstatix::get_summary_stats(Live_scale, show = c("mean", "min", "max")) %>% 
  mutate(range = max-min)  %>%
  ggplot(aes(HSMgrp, range))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(variable~., scales = "free_y")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() + 
  basetheme + FacetTheme
