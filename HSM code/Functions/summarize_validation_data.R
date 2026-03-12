##HSM ground truthing data summary
#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, openxlsx,
               pROC, extrafont,
               sf, install = TRUE)
#
# Setup ----
# Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SS") #two-letter site code
Version <- c("v1") #Model version
SurveyYYMM <- c("2401")
FileType <- c("data") #data or shapefile
#
#
# 
# Formatting ----
#
#Run once to get Arial:
#font_import(prompt = FALSE)
loadfonts(device = "win")
#
# manuscript formatting
base_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title = element_text(size = 12, face = "bold", color = "black", family = "Arial"),
    axis.text = ggplot2::element_text(size = 12, family = "Arial", color = "black"),
    axis.text.x = element_text(margin = margin(t=0.25, r=0.5, b=0, l=0.5, unit = "cm")), #unit(c(0.25, 0.5, 0, 0.5), "cm")), 
    axis.text.y = element_text(margin = margin(t=0, r=0.35, b=0, l=0, unit = "cm")), #unit(c(0, 0.25, 0, 0), "cm")),
    axis.ticks = element_line(color = "black", linewidth = 0.1),
    axis.ticks.length = unit(-0.15, "cm"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.1),
    plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5), family = "Arial"),
    plot.caption = ggplot2::element_text(face = "italic", size = 9),
    legend.title = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 10, family = "Arial"))
#
# presentation formatting
maptheme <- theme_classic()+
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.title = element_blank(),#element_text(size = 14, color = "black"), 
    axis.text =  element_text(size = 15, color = "black", family = "Arial"),
    axis.text.x = element_text(angle = 30, vjust = 0.5)
  )
#
legendtheme <- theme(
  legend.title = element_text(size = 14, color = "black", family = "Arial"),
  legend.text = element_text(size = 13, color = "black", family = "Arial"),
  legend.background = element_blank(),
  legend.key = element_blank()
)
#
FacetTheme <- theme(strip.text.y = element_text(face = "bold", size = 12),
                    strip.background = element_rect(fill = "#CCCCCC"),
                    panel.spacing = unit(0.75, "lines"),
                    strip.text.x = element_text(face = "bold", size = 12))

#
#
#
#
# Load shapefile data ----
#Load validation data from matching shape files in Output/Shapefiles folder: SiteCode_Version_validation_data
#Currently loads as sfc for potential mapping, can change to df if not mapping later
load_survey_shpfiles <- function(SiteCode = Site_Code, VersionNumber = Version, shp_filename = "model_srvys"){
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
load_survey_shpfiles()
str(SS_v1_validation_data)
#
#
# Load data file and combine with model ----
#
load_survey_data <- function(Site_Code, Version, SurveyYYMM, FileType = "data") {
  
  library(readxl)
  # Create file path:
  file_path <- file.path(
    paste0(Site_Code, "_", Version),
    "Data",
    paste0(Site_Code, "_groundtruthing_", SurveyYYMM, ".xlsx")
  )
  
  Srvy_quad <- NULL
  Srvy_LL <- NULL
  # Check for file:
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(list(Srvy_quad = NULL, Srvy_LL = NULL))
  }
  
  sheets <- excel_sheets(file_path)
  
  # Load Quadrat sheet:
  if ("Quadrat" %in% sheets) {
    Srvy_quad <- read_excel(file_path, sheet = "Quadrat", .name_repair = "universal")
  } else {
    warning("'Quadrat' sheet not found in file.")
  }
  
  # Load SampleEvent sheet:
  if ("SampleEvent" %in% sheets) {
    Srvy_LL <- read_excel(file_path, sheet = "SampleEvent", .name_repair = "universal")
  } else {
    warning("'SampleEvent' sheet not found in file.")
  }
  
  return(list(
    Srvy_quad = Srvy_quad,
    Srvy_LL = Srvy_LL
  ))
}
#
survey_data <- load_survey_data(Site_Code, Version, SurveyYYMM, FileType)
Srvy_LL <- survey_data$Srvy_LL %>%
  # Filter to only most recent survey if repeated surveys
  group_by(FixedLocationID) %>% arrange(desc(TripID)) %>% slice(1)
Srvy_quad <- survey_data$Srvy_quad %>%
  filter(SampleEventID %in% Srvy_LL$SampleEventID)
#
# Combines LL with count data, calculates DeadRatio, summarizes by station
clean_database_file <- function(Srvy_quad, Srvy_LL) {
  
  library(dplyr)
  
  # Add Latitude / Longitude to quadrat data
  Srvy_quad <- Srvy_quad %>%
    left_join(
      Srvy_LL %>%
        dplyr::select(
          SampleEventID,
          Latitude = LatitudeDec,
          Longitude = LongitudeDec
        ),
      by = "SampleEventID"
    ) %>%
    mutate(across(c(Latitude, Longitude, TotalVolume, TotalWeight, NumDrills, NumLive, NumDead), as.numeric)) %>%
    mutate(Station = substr(SampleEventID, 19, 22),
           DeadRatio = as.numeric(NumDead/(NumLive+NumDead)))
  
  # Columns that must exist
  req_cols <- c("Spat", "Adult", "Legal", "SpatAdult")
  
  # Create missing columns
  for (col in req_cols) {
    if (!col %in% names(Srvy_quad)) {
      if (col == "SpatAdult" && "NumLive" %in% names(Srvy_quad)) {
        Srvy_quad[[col]] <- Srvy_quad$NumLive
      } else {
        Srvy_quad[[col]] <- NA
      }
    }
  }
  
  # Summarize by station and clean to desired columns 
  Srvy_quad <- Srvy_quad %>%
    mutate(across(any_of(c("Spat", "Adult", "Legal")), as.numeric)) %>%
    group_by(SampleEventID, Latitude, Longitude) %>%
    summarise(
      n_quadrats = n_distinct(QuadratID),
      across(
        any_of(c("NumLive", "NumDead", "TotalVolume", "TotalWeight", "NumLegal",
          "DeadRatio", "Spat", "Adult", "Legal", "SpatAdult")),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) 
    
  return(Srvy_quad)
}
#
Srvy_data <- clean_database_file(Srvy_quad, Srvy_LL)
head(Srvy_data)
points_sf <- st_as_sf(Srvy_data, coords = c("Longitude", "Latitude"), crs = 4326)
#
###Load shape file with model data: 
model_file_name <- "HSM_model"
model_scores_date <- c("2026-02-05")#c("2026-03-04") #
# Also loads files for scoring
shp_pattern <- paste0("^", Site_Code, "_", Version, "_", model_file_name, "_", model_scores_date, ".*\\.shp$")
shp_files <- list.files(path = file.path(paste0(Site_Code, "_", Version), "Output", "Shapefiles"),
                        pattern = shp_pattern,
                        full.names = TRUE
)
HSMmodel <- shp_files %>%
  map(st_read, quiet = TRUE) %>%
  bind_rows()
#
points_sf <- st_transform(points_sf, st_crs(HSMmodel))
HSM_ground <- st_join(points_sf, HSMmodel)
#
#
#
# Summarize ----
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
  # Define HSM grps
  expected_levels <- c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")
  
  # Define na replacement case:
  na_condition <- (surveyData$Spat %in% c(0, NA) & surveyData$Adult %in% c(0, NA) & surveyData$Legal %in% c(0, NA))
  #
  cleaned_data <- surveyData %>% 
    as.data.frame() %>%
    # Rename columns for consistency 
    rename_with(~ sub(".*NumLive.*", "NumLive", .x), matches("NumLive")) %>%
    rename_with(~ sub(".*DeadRatio.*", "DeadRatio", .x), matches("DeadRatio")) %>%
    mutate(HSMgrp = factor(HSMgrp, levels = expected_levels, ordered = TRUE)) %>%
    # Replace 0 with NA when proper
    mutate(DeadRatio = as.numeric(DeadRatio),
           SpatAdult = as.numeric(SpatAdult),
           DeadRatio = if_else(na_condition, NA_real_, DeadRatio),
           SpatAdult = if_else(na_condition, NA_real_, SpatAdult))
  #
  # Create Presence column if missing
  if (!"Presence" %in% names(cleaned_data)) {
    if (!"NumLive" %in% names(cleaned_data)) {
      stop("Column 'NumLive' is required to create Presence.")
    }
    
    cleaned_data <- cleaned_data %>%
      mutate(
        PresenceL = case_when(
          is.na(NumLive) ~ NA_real_,
          NumLive > 0 ~ 1,
          TRUE ~ 0
        ),
        PresenceD = case_when(
          is.na(NumDead) ~ NA_real_,
          NumDead > 0 ~ 1,
          TRUE ~ 0
        ),
        Presence = case_when(
          is.na(NumLive) & is.na(NumDead) ~ NA_real_,
          NumLive > 0 | NumDead > 0 ~ 1,
          TRUE ~ 0
        )
      )
  }
  
  return(cleaned_data)
  #
}
#
validation_data <- clean_survey_data(HSM_ground)
head(validation_data)
val_df <- validation_data %>%
  group_by(HSMgrp) %>%
  mutate(Live_scale = scale(sqrt(NumLive+0.5))[,1]) %>%
  ungroup()
#
summarize_data <- function(cleanedData){
  
  # checks
  if (!is.data.frame(cleanedData) && !tibble::is_tibble(cleanedData)) {
    stop("Input 'cleanedData' must be a data frame or tibble.")
  }
  
  if (!any(grepl("HSMgrp", names(cleanedData)))) {
    stop("No column containing 'HSMgrp' found in cleanedData")
  }
  
  required_cols <- c("NumLive", "DeadRatio", "SpatAdult", grep("Presence", names(cleanedData), value = TRUE))
  
  missing_cols <- setdiff(required_cols, names(cleanedData))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  for (col in required_cols) {
    if (!is.numeric(cleanedData[[col]])) {
      stop(paste("Column", col, "must be numeric for summarization."))
    }
  }
  
  # Define HSM grps
  expected_levels <- c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")
  
  # Check if all expected levels are present
  actual_levels <- unique(na.omit(cleanedData$HSMgrp))
  missing_levels <- setdiff(expected_levels, actual_levels)
  
  if (length(missing_levels) > 0) {
    warning(paste("Expected HSMgrp levels missing:", paste(missing_levels, collapse = ", "), ". Proceeding with available levels."))
  }
  
  summarized_data <- suppressWarnings(
    cleanedData %>%
      mutate(HSMgrp = factor(HSMgrp, levels = expected_levels, ordered = TRUE)) %>%
      group_by(HSMgrp) %>%
      summarise(
        across(
          all_of(required_cols),
          list(
            n = ~ sum(!is.na(.x)),
            mean = ~ mean(.x, na.rm = TRUE),
            sd = ~ sd(.x, na.rm = TRUE),
            min = ~ min(.x, na.rm = TRUE),
            max = ~ max(.x, na.rm = TRUE)
          ),
          .names = "{.col}_{fn}"
        ),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = -HSMgrp,
        names_to = c("variable", "stat"),
        names_sep = "_",
        values_to = "value"
      ) %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(
        min = ifelse(is.infinite(min), NA_real_, min),
        max = ifelse(is.infinite(max), NA_real_, max)
      )
  )
  
  return(summarized_data)
}
#
validation_summary <- summarize_data(validation_data)
#
nrow(HSM_ground %>% drop_na(HSM))
nrow(HSMmodel %>% drop_na(HSM))
(nrow(HSM_ground %>% drop_na(HSM))/nrow(HSMmodel %>% drop_na(HSM)))*100
#
HSM_ground <- HSM_ground %>%
  mutate(HSMgrp = factor(HSMgrp,
                         levels = c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)", "[0.4,0.5)", 
                                    "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)", "[0.8,0.9)", "[0.9,1]")
                         ))
#
(p0 <- ggplot() +
    geom_sf(data = HSMmodel, color = "#CCCCCC")+
    geom_sf(data = left_join(HSM_ground,
                             validation_data %>% dplyr::select(PGID, Presence)), 
            aes(color = HSMgrp, shape = as.factor(Presence)), size = 6, alpha = 0.8)+ 
    scale_color_viridis_d()+
    scale_shape_manual(values = c(8, 19))+
    labs(color = "Suitability score", shape = "Oyster Presence")+
    legendtheme)
#
p0 + base_theme #manuscript
p0 + maptheme #presentation 
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_GTLocations_p.png"),
  plot = p0 + maptheme,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_GTLocations.png"),
  plot = p0,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
left_join(HSM_ground,
          validation_data %>% dplyr::select(PGID, Presence))
#
# Analysis ----
#
set.seed(5432)
model <- glm(Presence ~ round(HSMround,1), data = validation_data, family = binomial)
summary(model)
anova(model, test = "Chisq")
#Likely due to small sample size, HSM range too narrow for true validation
# ROC (Receiver Operating Characteristic) curve shows the trade off between true positive rate and false postie rate
roc_obj <- roc(validation_data$Presence, fitted(model))
(auc_val <- auc(roc_obj))
plot(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities
)
roc_df$FPR <- 1 - roc_df$specificity
(p1 <- ggplot(roc_df, aes(x = FPR, y = sensitivity)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Specificity",#"False Positive Rate",
    y = "Sensitivity",#"True Positive Rate",
  ) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  basetheme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_ROC.png"),
  plot = p1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#AUC = probability that a randomly chosen presence site has a higher predicted suitability than a randomly chosen absence site.
#0..5 = random, 0.6-0.7 = poor, 0.7-0.8 acceptable, 0.8-0.9 good, 0.9-1 excellent
#EXAMPLE: Presence probability increased significantly with HSM suitability class (logistic regression, p < 0.01). Model discrimination was acceptable (AUC = 0.76), indicating that sites with higher HSM scores were more likely to contain oysters.
(presence_summary <- validation_data %>%
  group_by(HSMgrp) %>%
  summarize(
    n = n(),
    pres = sum(Presence),
    presence_rate = mean(Presence)
  ) %>% 
    mutate(
      se = sqrt((presence_rate * (1 - presence_rate)) / n),
      lower = presence_rate - 1.96 * se,
      upper = presence_rate + 1.96 * se
    ))
#
(p2 <- ggplot(presence_summary, aes(x = HSMgrp, y = presence_rate)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.7) +
  labs(
    x = "Habitat suitability class",
    y = "Observed presence probability"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.1)) +
  basetheme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_HSM_presence.png"),
  plot = p2,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
# Plotting ----
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
val_df %>% group_by(HSMgrp) %>%
  rstatix::get_summary_stats(Live_scale, show = c("mean", "min", "max")) %>% 
  mutate(range = max-min)  %>%
  ggplot(aes(HSMgrp, range))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(variable~., scales = "free_y")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() + 
  basetheme + FacetTheme
