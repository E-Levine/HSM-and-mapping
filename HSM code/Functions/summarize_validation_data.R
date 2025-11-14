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
#Load validation data from matching sahpefiles in Output/Shapefiles folder: SiteCode_Version_validation_data
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
validation_data <- SS_v1_validation_data %>% as.data.frame() %>%
  rename("NumLive" = contains("NumLive"),
         "DeadRatio" = contains("DeadRatio")) %>%
  mutate(DeadRatio = case_when(Spat == 0 & Adult == 0 & Legal == 0 ~ NA, TRUE ~ DeadRatio),
         SpatAdult = case_when(Spat == 0 & Adult == 0 & Legal == 0 ~ NA, TRUE ~ SpatAdult))
validation_summary <- validation_data %>%
  group_by(HSMgrp) %>%
  rstatix::get_summary_stats(values = c(NumLive, DeadRatio, SpatAdult, Presence), show = c("mean", "sd", "min", "max")) %>%
  mutate(HSMgrp = factor(HSMgrp, levels = c("0", "(0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")))
  #pivot_wider(names_from = variable, values_from = c(mean, sd, min, max), names_glue = "{variable}_{.value}"))
head(validation_data)
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
