##HSM ground truthing data summary and model comparison
#
##Requires shapefile of area with data layers/scores applied
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load required packages (should install missing packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, openxlsx, writexl,
               pROC, extrafont, classInt, BAMMtools, #Jenks
               rms, ecospat, ggeffects, #ROC, Boyce, response curve
               sf, install = TRUE)
#
HSMfunc <- new.env()
source("HSM code/Functions/HSM_gt_model_functions.R", local = HSMfunc)
#
# Setup ----
# Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SS") #two-letter site code
Version <- c("v1") #Model version
SurveyYYMM <- c("2401") #SS-2401, SL-2305
FileType <- c("shapefile") #data or shapefile
#
HSMfunc <- new.env()
source("HSM code/Functions/HSM_scoring_functions.R", local = HSMfunc)
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
    axis.title = element_text(size = 17, face = "bold", color = "black", family = "Arial"),
    axis.text = ggplot2::element_text(size = 15, family = "Arial", color = "black"),
    axis.text.x = element_text(margin = margin(t=0.25, r=0.5, b=0, l=0.5, unit = "cm")), #unit(c(0.25, 0.5, 0, 0.5), "cm")), 
    axis.text.y = element_text(margin = margin(t=0, r=0.35, b=0, l=0, unit = "cm")), #unit(c(0, 0.25, 0, 0), "cm")),
    axis.ticks = element_line(color = "black", linewidth = 0.1),
    axis.ticks.length = unit(-0.15, "cm"),
    panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.1),
    plot.margin = grid::unit(c(0.05, 0, 0, 0), "cm"),
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
# for plots
plot_theme <- theme(plot.margin = unit(c(0.25, 0.45, 0.1, 0.1), "cm"),
                    panel.border = element_rect(color = NA))
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
# Load original model for comparisons ----
#
###Load shape file with model data: 
model_file_name <- "HSM_model"
model_scores_date <- c("2026-02-05") #SS c("2026-02-05")##SL "2026-03-04"
# Also loads files for scoring
shp_pattern <- paste0("^", Site_Code, "_", Version, "_", model_file_name, "_", model_scores_date, ".*\\.shp$")
shp_files <- list.files(path = file.path(paste0(Site_Code, "_", Version), "Output", "Shapefiles"),
                        pattern = shp_pattern,
                        full.names = TRUE
)
HSMmodel <- shp_files %>%
  map(st_read, quiet = TRUE) %>%
  bind_rows()

HSM_scores_t <- read_csv(paste0(Site_Code,"_", Version, "/Output/Data files/", Site_Code,"_", Version, "_model_scores_", model_scores_date, ".csv"))
HSM_scores <- left_join(HSMmodel, 
                        HSM_scores_t) %>%
  mutate(
    HSMgrp = factor(
      HSMgrp,
      levels = c(
        "[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
        "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
        "[0.8,0.9)", "[0.9,1]"
      )
    ))
#
head(HSM_scores)
#
#
#
#
#
# Comparison to other models: model 2 ----
#
## Flow as exclusionary when 0:
HSM_data_flow <- HSM_scores %>% 
  st_drop_geometry() %>% 
  {
    av_data <- dplyr::select(., ends_with("AV") & !matches("^FAV$"))
    
    # Keep only AV columns with at least one real (non-NA/NaN) value
    valid_av <- av_data[, colSums(!is.na(av_data)) > 0, drop = FALSE]
    
    CurveCO_val <- ncol(valid_av)
    
    mutate(.,
           HSM_f = case_when(
             ChnlTO == 1 & FAV == 0 ~ 0,  # HSM = 0 when Flow is 0
             ChnlTO == 1 ~ (rowSums(valid_av, na.rm = TRUE) / CurveCO_val) * FAV,
             ChnlTO == 0 ~ 0, 
             TRUE ~ NA_real_
           )) %>%
      mutate(HSMround_f = round(HSM_f, 2),
             Curve_val = ncol(valid_av))
  }
#
# Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#
# Determine natural Jenks breaks (thirds)
set.seed(54321)
vals_f <- sample(HSM_data_flow$HSM_f, min(20000, length(HSM_data_flow$HSM_f))) #Sample then calculate breaks
jenks_breaks_f <- classInt::classIntervals(vals_f, n = 4, style = "jenks")$brks#getJenksBreaks(var = HSM_data$HSM, k = 4)
#
# Clean breaks then make sure they cover full data range:
jenks_breaks_f <- sort(unique(
  signif(jenks_breaks_f, 6)
))
jenks_breaks_f[c(1, length(jenks_breaks_f))] <-
  range(HSM_data_flow$HSM_f, na.rm = TRUE)
#
# Assign groups using cut()
HSM_data_grps_f <- HSM_data_flow %>%
  mutate(
    # HSM 0.1 groups
    HSMgrp_f = case_when(
      HSMround_f < 0.1 & HSMround_f >= 0 ~ "[0,0.1)",
      HSMround_f < 0.2 & HSMround_f >= 0.1 ~ "[0.1,0.2)",
      HSMround_f < 0.3 & HSMround_f >= 0.2 ~ "[0.2,0.3)",
      HSMround_f < 0.4 & HSMround_f >= 0.3 ~ "[0.3,0.4)",
      HSMround_f < 0.5 & HSMround_f >= 0.4 ~ "[0.4,0.5)",
      HSMround_f < 0.6 & HSMround_f >= 0.5 ~ "[0.5,0.6)",
      HSMround_f < 0.7 & HSMround_f >= 0.6 ~ "[0.6,0.7)",
      HSMround_f < 0.8 & HSMround_f >= 0.7 ~ "[0.7,0.8)",
      HSMround_f < 0.9 & HSMround_f >= 0.8 ~ "[0.8,0.9)",
      TRUE           ~ "[0.9,1]"
    ),
    # Aggregated bins
    HSMgyr_f = case_when(
      HSMgrp_f == "0" ~ "0",
      HSMround_f < 0.4 ~ "Low",
      HSMround_f < 0.6 ~ "Moderate",
      TRUE           ~ "High"
    ),
    # Jenks breaks
    HSMjb_f = cut(HSM_f,
                  breaks = jenks_breaks_f,
                  include.lowest = TRUE,
                  labels = c("Low", "Medium", "High")),
    # Quantiles
    HSM_q4_f = factor(
      ntile(HSM_f, 4),
      levels = 1:4,
      labels = c("Least", "Low", "Moderate", "Most"))
  ) %>%
  #Make sure grp is factors
  mutate(
    HSMgrp_f = factor(
      HSMgrp_f,
      levels = c(
        "[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
        "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
        "[0.8,0.9)", "[0.9,1]"
      )
    ),
    HSMgyr_f = factor(
      HSMgyr_f,
      levels = c("0", "Low", "Moderate", "High")
    ),
    HSMjb_f = factor(
      HSMjb_f,
      levels = c("Low", "Medium", "High")
    )
  )
#
head(HSM_data_grps_f)
#
## Model summary:
#
HSM_data_grps_f %>% 
  #mutate(HSM_r = round(HSMround, 1)) %>%
  group_by(HSMgrp_f) %>%
  summarise(n())

(p1 <- ggplot(HSM_data_grps_f, aes(x = HSMgrp_f)) +
    geom_histogram(stat = "count", fill = "gray50", color = "black") +
    labs(
      title = "HSM scores",
      x = "Suitability score",
      y = "Count"
    ) +
    base_theme + 
    scale_y_continuous(expand = c(0,0))+#, limits = c(0, 120000))+
    scale_x_discrete(expand = c(0.005,0))+
    theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    plot_theme + theme(axis.text.x = element_text(size = 11, angle = 20)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_JenksBreaks_flow2.png"),
  plot = p1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
## Compare model distributions
#
models_df_f <- data.frame(
  value = c(HSM_data_grps_f$HSM, HSM_data_grps_f$HSM_f),
  Model = rep(c("Flow+","Flow*"),
              each = nrow(HSM_data_grps_f))
)
# 
(p2 <- ggplot(models_df_f, aes(value, linetype = Model)) +
    geom_density(linewidth = 1) +
    scale_linetype_manual(values = c("dashed", "solid"))+
    scale_x_continuous("HSM value", limits = c(0,1), expand = c(0,0)) + 
    scale_y_continuous("Density", limits = c(0, 80), expand = c(0,0))+ #Modify as needed: SL 0-10, SS 0-80
    base_theme + plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_flow_comparison.png"),
  plot = p2,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
## Compare ranking agreement
cor(HSM_data_grps_f$HSM,
    HSM_data_grps_f$HSM_f,
    method = "spearman")
#0.8887132 - raw SL; 0.9710259 SS
(p3 <- ggplot(HSM_data_grps_f,
              aes(HSM, HSM_f)) +
    geom_point() +
    geom_abline(linetype = "dashed") +
    scale_x_continuous("Flow+", limits = c(0,1), expand = c(0,0))+
    scale_y_continuous("Flow*", limits = c(0,1), expand = c(0,0))+
    base_theme + theme(plot.margin = unit(c(0.25, 0.2, 0.1, 0.1), "cm"), 
                       panel.border = element_rect(color = NA)))
# Additive scores higher than flow*
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_flow_agreement.png"),
  plot = p3,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
HSM_data_grps_f$diff <- HSM_data_grps_f$HSM_f - HSM_data_grps_f$HSM
mean(HSM_data_grps_f$diff)
HSM_scores2 <- left_join(HSM_scores, HSM_data_grps_f)
(p4 <- ggplot()+
    geom_sf(data = HSM_scores2, aes(color = diff)) +
    base_theme+
    scale_color_viridis_c(limits = c(0,min(HSM_scores2$diff)))+
    labs(color = "Model difference"))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_flow_difference.png"),
  plot = p4,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#ggplot(HSM_data_grps_f, aes(Long_DD_X, Lat_DD_Y, color = diff)) + geom_point()+ coord_fixed()
#
#
#
#
#
# Comparison to other models: model 3 ----
#
#
## Salinity as exclusionary when 0:
HSM_data_sal <- HSM_scores %>% 
  st_drop_geometry() %>% 
  {
    av_data <- dplyr::select(., ends_with("AV") & !matches("^SAV$"))
    
    # Keep only AV columns with at least one real (non-NA/NaN) value
    valid_av <- av_data[, colSums(!is.na(av_data)) > 0, drop = FALSE]
    
    CurveCO_val <- ncol(valid_av)
    
    mutate(.,
           HSM_s = case_when(
             ChnlTO == 1 & SAV == 0 ~ 0,  # HSM = 0 when Flow is 0
             ChnlTO == 1 ~ (rowSums(valid_av, na.rm = TRUE) / CurveCO_val) * SAV,
             ChnlTO == 0 ~ 0, 
             TRUE ~ NA_real_
           )) %>%
      mutate(HSMround_s = round(HSM_s, 2),
             Curve_val = ncol(valid_av))
  }
#
# Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#
# Determine natural Jenks breaks (thirds)
set.seed(54321)
vals_s <- sample(HSM_data_sal$HSM_s, min(20000, length(HSM_data_sal$HSM_s))) #Sample then calculate breaks
jenks_breaks_s <- classInt::classIntervals(vals_s, n = 3, style = "jenks")$brks#getJenksBreaks(var = HSM_data$HSM, k = 4)
#
# Clean breaks then make sure they cover full data range:
jenks_breaks_s <- sort(unique(
  signif(jenks_breaks_s, 6)
))
jenks_breaks_s[c(1, length(jenks_breaks_s))] <-
  range(HSM_data_sal$HSM_s, na.rm = TRUE)
#
# Assign groups using cut()
HSM_data_grps_s <- HSM_data_sal %>%
  mutate(
    # HSM 0.1 groups
    HSMgrp_s = case_when(
      HSMround_s < 0.1 & HSMround_s >= 0 ~ "[0,0.1)",
      HSMround_s < 0.2 & HSMround_s >= 0.1 ~ "[0.1,0.2)",
      HSMround_s < 0.3 & HSMround_s >= 0.2 ~ "[0.2,0.3)",
      HSMround_s < 0.4 & HSMround_s >= 0.3 ~ "[0.3,0.4)",
      HSMround_s < 0.5 & HSMround_s >= 0.4 ~ "[0.4,0.5)",
      HSMround_s < 0.6 & HSMround_s >= 0.5 ~ "[0.5,0.6)",
      HSMround_s < 0.7 & HSMround_s >= 0.6 ~ "[0.6,0.7)",
      HSMround_s < 0.8 & HSMround_s >= 0.7 ~ "[0.7,0.8)",
      HSMround_s < 0.9 & HSMround_s >= 0.8 ~ "[0.8,0.9)",
      TRUE           ~ "[0.9,1]"
    ),
    # Aggregated bins
    HSMgyr_s = case_when(
      HSMgrp_s == "0" ~ "0",
      HSMround_s < 0.4 ~ "Low",
      HSMround_s < 0.6 ~ "Moderate",
      TRUE           ~ "High"
    ),
    # Jenks breaks
    HSMjb_s = cut(HSM_s,
                  breaks = jenks_breaks_s,
                  include.lowest = TRUE,
                  labels = c("Low", "Medium", "High")),
    # Quantiles
    HSM_q4_s = factor(
      ntile(HSM_s, 4),
      levels = 1:4,
      labels = c("Least", "Low", "Moderate", "Most"))
  ) %>%
  #Make sure grp is factors
  mutate(
    HSMgrp_s = factor(
      HSMgrp_s,
      levels = c(
        "[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
        "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
        "[0.8,0.9)", "[0.9,1]"
      )
    ),
    HSMgyr_s = factor(
      HSMgyr_s,
      levels = c("0", "Low", "Moderate", "High")
    ),
    HSMjb_s = factor(
      HSMjb_s,
      levels = c("Low", "Medium", "High")
    )
  )
#
head(HSM_data_grps_s)
#
## Model summary:
#
HSM_data_grps_s %>% 
  #mutate(HSM_r = round(HSMround, 1)) %>%
  group_by(HSMgrp_s) %>%
  summarise(n())

(p5 <- ggplot(HSM_data_grps_s, aes(x = HSMgrp_s)) +
    geom_histogram(stat = "count", fill = "gray50", color = "black") +
    labs(
      title = "HSM scores",
      x = "Suitability score",
      y = "Count"
    ) +
    base_theme + 
    scale_y_continuous(expand = c(0,0))+#, limits = c(0, 120000))+
    scale_x_discrete(expand = c(0.005,0))+
    theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    plot_theme + theme(axis.text.x = element_text(size = 11, angle = 20)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_JenksBreaks_sal3.png"),
  plot = p5,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
## Compare model distributions
#
models_df_s <- data.frame(
  value = c(HSM_data_grps_s$HSM, HSM_data_grps_s$HSM_s),
  Model = rep(c("Salinity+","Salinity*"),
              each = nrow(HSM_data_grps_s))
)
# 
(p6 <- ggplot(models_df_s, aes(value, linetype = Model)) +
    geom_density(linewidth = 1) +
    scale_linetype_manual(values = c("dashed", "solid"))+
    scale_x_continuous("HSM value", limits = c(0,1), expand = c(0,0)) + 
    scale_y_continuous("Density", limits = c(0, 80), expand = c(0,0))+
    base_theme + plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_sal3_comparison.png"),
  plot = p6,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
## Compare ranking agreement
cor(HSM_data_grps_s$HSM,
    HSM_data_grps_s$HSM_s,
    method = "spearman")
#0.9068286 - raw SL; SS 0.966154
(p7 <- ggplot(HSM_data_grps_s,
              aes(HSM, HSM_s)) +
    geom_point() +
    geom_abline(linetype = "dashed") +
    scale_x_continuous("Salinity+", limits = c(0,1), expand = c(0,0))+
    scale_y_continuous("Salinity*", limits = c(0,1), expand = c(0,0))+
    base_theme + theme(plot.margin = unit(c(0.25, 0.2, 0.1, 0.1), "cm"), 
                       panel.border = element_rect(color = NA)))
# Additive scores higher than salinity*
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_sal3_agreement.png"),
  plot = p7,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
HSM_data_grps_s$diff <- HSM_data_grps_s$HSM_s - HSM_data_grps_s$HSM
mean(HSM_data_grps_s$diff)
HSM_scores3 <- left_join(HSM_scores, HSM_data_grps_s)
(p8 <- ggplot()+
    geom_sf(data = HSM_scores3, aes(color = diff)) +
    base_theme+
    scale_color_viridis_c(limits = c(0,min(HSM_scores3$diff)))+
    labs(color = "Model difference"))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_sal3_difference.png"),
  plot = p8,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
#
#
#
# Comparison to other models: model 4 ----
#
#
## Flow & salinity as exclusionary when 0:
HSM_data_fs <- HSM_scores %>% 
  st_drop_geometry() %>% 
  {
    av_data <- dplyr::select(., ends_with("AV") & !matches("^SAV$") & !matches("^FAV$"))
    
    # Keep only AV columns with at least one real (non-NA/NaN) value
    valid_av <- av_data[, colSums(!is.na(av_data)) > 0, drop = FALSE]
    
    CurveCO_val <- ncol(valid_av)
    
    mutate(.,
           HSM_fs = case_when(
             ChnlTO == 1 & FAV == 0 ~ 0,  # HSM = 0 when Flow is 0
             ChnlTO == 1 & SAV == 0 ~ 0,  # HSM = 0 when Salinity is 0
             ChnlTO == 1 ~ (rowSums(valid_av, na.rm = TRUE) / CurveCO_val) * SAV * FAV,
             ChnlTO == 0 ~ 0, 
             TRUE ~ NA_real_
           )) %>%
      mutate(HSMround_fs = round(HSM_fs, 2),
             Curve_val = ncol(valid_av))
  }
#
# Define the breaks for grouping (0 to 1 by 0.1)
breaks <- seq(0, 1, by = 0.1)#seq(0, 1, by = 0.1)
#
# Determine natural Jenks breaks (thirds)
set.seed(54321)
vals_fs <- sample(HSM_data_fs$HSM_fs, min(20000, length(HSM_data_fs$HSM_fs))) #Sample then calculate breaks
jenks_breaks_fs <- classInt::classIntervals(vals_fs, n = 4, style = "jenks")$brks#getJenksBreaks(var = HSM_data$HSM, k = 4)
#
# Clean breaks then make sure they cover full data range:
jenks_breaks_fs <- sort(unique(
  signif(jenks_breaks_fs, 6)
))
jenks_breaks_fs[c(1, length(jenks_breaks_fs))] <-
  range(HSM_data_fs$HSM_fs, na.rm = TRUE)
#
# Assign groups using cut()
HSM_data_grps_fs <- HSM_data_fs %>%
  mutate(
    # HSM 0.1 groups
    HSMgrp_fs = case_when(
      HSMround_fs < 0.1 & HSMround_fs >= 0 ~ "[0,0.1)",
      HSMround_fs < 0.2 & HSMround_fs >= 0.1 ~ "[0.1,0.2)",
      HSMround_fs < 0.3 & HSMround_fs >= 0.2 ~ "[0.2,0.3)",
      HSMround_fs < 0.4 & HSMround_fs >= 0.3 ~ "[0.3,0.4)",
      HSMround_fs < 0.5 & HSMround_fs >= 0.4 ~ "[0.4,0.5)",
      HSMround_fs < 0.6 & HSMround_fs >= 0.5 ~ "[0.5,0.6)",
      HSMround_fs < 0.7 & HSMround_fs >= 0.6 ~ "[0.6,0.7)",
      HSMround_fs < 0.8 & HSMround_fs >= 0.7 ~ "[0.7,0.8)",
      HSMround_fs < 0.9 & HSMround_fs >= 0.8 ~ "[0.8,0.9)",
      TRUE           ~ "[0.9,1]"
    ),
    # Aggregated bins
    HSMgyr_fs = case_when(
      HSMgrp_fs == "0" ~ "0",
      HSMround_fs < 0.4 ~ "Low",
      HSMround_fs < 0.6 ~ "Moderate",
      TRUE           ~ "High"
    ),
    # Jenks breaks
    HSMjb_fs = cut(HSM_fs,
                   breaks = jenks_breaks_fs,
                   include.lowest = TRUE,
                   labels = c("Low", "Medium", "High")),
    # Quantiles
    HSM_q4_fs = factor(
      ntile(HSM_fs, 4),
      levels = 1:4,
      labels = c("Least", "Low", "Moderate", "Most"))
  ) %>%
  #Make sure grp is factors
  mutate(
    HSMgrp_fs = factor(
      HSMgrp_fs,
      levels = c(
        "[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
        "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
        "[0.8,0.9)", "[0.9,1]"
      )
    ),
    HSMgyr_fs = factor(
      HSMgyr_fs,
      levels = c("0", "Low", "Moderate", "High")
    ),
    HSMjb_fs = factor(
      HSMjb_fs,
      levels = c("Low", "Medium", "High")
    )
  )
#
head(HSM_data_grps_fs)
#
## Model summary:
#
HSM_data_grps_fs %>% 
  #mutate(HSM_r = round(HSMround, 1)) %>%
  group_by(HSMgrp_fs) %>%
  summarise(n())

(p9 <- ggplot(HSM_data_grps_fs, aes(x = HSMgrp_fs)) +
    geom_histogram(stat = "count", fill = "gray50", color = "black") +
    labs(
      title = "HSM scores",
      x = "Suitability score",
      y = "Count"
    ) +
    base_theme + 
    scale_y_continuous(expand = c(0,0))+#, limits = c(0, 120000))+
    scale_x_discrete(expand = c(0.005,0))+
    theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    plot_theme + theme(axis.text.x = element_text(size = 11, angle = 20)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_JenksBreaks_fs4.png"),
  plot = p9,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
## Compare model distributions
#
models_df_fs <- data.frame(
  value = c(HSM_data_grps_fs$HSM, HSM_data_grps_fs$HSM_fs),
  Model = rep(c("Flow+ Salinity+","Flow* Salinity*"),
              each = nrow(HSM_data_grps_fs))
)
# 
(p10 <- ggplot(models_df_fs, aes(value, linetype = Model)) +
    geom_density(linewidth = 1) +
    scale_linetype_manual(values = c("dashed", "solid"))+
    scale_x_continuous("HSM value", limits = c(0,1), expand = c(0,0)) + 
    scale_y_continuous("Density", limits = c(0, 10), expand = c(0,0))+
    base_theme + plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_fs4_comparison.png"),
  plot = p10,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
## Compare ranking agreement
cor(HSM_data_grps_fs$HSM,
    HSM_data_grps_fs$HSM_fs,
    method = "spearman")
#0.9759851 - raw SL; SS 0.9912836
(p11 <- ggplot(HSM_data_grps_fs,
               aes(HSM, HSM_fs)) +
    geom_point() +
    geom_abline(linetype = "dashed") +
    scale_x_continuous("Flow+ Salinity+", limits = c(0,1), expand = c(0,0))+
    scale_y_continuous("Flow* Salinity*", limits = c(0,1), expand = c(0,0))+
    base_theme + theme(plot.margin = unit(c(0.25, 0.2, 0.1, 0.1), "cm"), 
                       panel.border = element_rect(color = NA)))
# Additive scores higher than salinity*
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_fs4_agreement.png"),
  plot = p11,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
HSM_data_grps_fs$diff <- HSM_data_grps_fs$HSM_fs - HSM_data_grps_fs$HSM
mean(HSM_data_grps_fs$diff)
HSM_scores4 <- left_join(HSM_scores, HSM_data_grps_fs)
(p12 <- ggplot()+
    geom_sf(data = HSM_scores4, aes(color = diff)) +
    base_theme+
    scale_color_viridis_c(limits = c(0,min(HSM_scores4$diff)))+
    labs(color = "Model difference"))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_fs4_difference.png"),
  plot = p12,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
#
#
#
# Load and clean data ----
#
#Load validation data
load_survey_shpfiles()
str(SS_v1_validation_data)
#
#
# Load data file and combine with model
survey_data <- load_survey_data(Site_Code, Version, SurveyYYMM, FileType)
Srvy_LL <- survey_data$Srvy_LL %>%
  # Filter to only most recent survey if repeated surveys (can only use LL since CR lab has different IDs for same location)
  group_by(LatitudeDec, LongitudeDec) %>% 
  arrange(desc(TripID)) %>% slice(1)
# Filter and add date column
Srvy_quad <- survey_data$Srvy_quad %>%
  filter(SampleEventID %in% Srvy_LL$SampleEventID) %>%
  mutate(Date = as.Date(substr(SampleEventID, 8, 15), format = "%Y%m%d"))
#
# Clean data: Combines LL with count data, calculates DeadRatio, summarizes by station
# Requires a Date column in either the quad or LL df.
Srvy_data <- clean_database_file(Srvy_quad, Srvy_LL)
head(Srvy_data)
points_sf <- st_as_sf(Srvy_data, coords = c("Longitude", "Latitude"), crs = 4326)
#
###Load shape file with model data: 
model_file_name <- "HSM_model"
#model_scores_date <- c("2026-03-04") #c("2026-02-05")#
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
HSM_ground <- st_join(points_sf, HSMmodel) %>%
  mutate(
    HSMgrp = factor(
      HSMgrp,
      levels = c(
        "[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
        "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
        "[0.8,0.9)", "[0.9,1]"
      )
    ))
#
ggplot()+
  geom_sf(data = points_sf, aes(color = NumLive))
#
#
#
#
# Summarize ----
#
(HSM_ground <- left_join(HSM_ground,
                         HSM_data_grps_f %>% dplyr::select(PGID:Long_DD_X, HSM_f, HSMround_f, HSMgrp_f)) %>%
    left_join(HSM_data_grps_s %>% dplyr::select(PGID:Long_DD_X, HSM_s, HSMround_s, HSMgrp_s)) %>%
    left_join(HSM_data_grps_fs %>% dplyr::select(PGID:Long_DD_X, HSM_fs, HSMround_fs, HSMgrp_fs)))

#
# Summarize NumLive, DeadRatio, SpatAdult, Presence by HSMgrp score
validation_data <- clean_survey_data(HSM_ground) %>%
  drop_na(HSMgrp, Presence)
head(validation_data)
(val_df1 <- validation_data %>%
    group_by(HSMgrp) %>%
    mutate(Live_scale = scale(sqrt(NumLive+0.5))[,1]) %>%
    ungroup())
#
validation_summary <- summarize_data(validation_data)
#
# Number of grid cells surveyed:
nrow(HSM_ground %>% drop_na(HSM))
nrow(HSMmodel %>% drop_na(HSM))
(nrow(HSM_ground %>% drop_na(HSM))/nrow(HSMmodel %>% drop_na(HSM)))*100
#
HSM_ground2 <- HSM_ground %>%
  drop_na(HSMgrp)
#
(p0 <- ggplot() +
    geom_sf(data = HSMmodel, color = "#CCCCCC")+
    geom_sf(data = left_join(HSM_ground2,
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
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_GTLocations_m.png"),
  plot = p0 + base_theme,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
left_join(HSM_ground2,
          validation_data %>% dplyr::select(PGID, Presence))
#
#
#
#
#
# Analysis ----
#
# Original model:
set.seed(5432)
model1 <- glm(Presence ~ HSMround, data = validation_data, family = binomial)
summary(model1)
anova(model1, test = "Chisq")
#Likely due to small sample size, HSM range too narrow for true validation
# ROC (Receiver Operating Characteristic) curve shows the trade off between true positive rate and false postie rate
roc_obj1 <- pROC::roc(validation_data$Presence, fitted(model1)) #model$y
(auc_val1 <- auc(roc_obj1))
plot(roc_obj1)
roc_df1 <- data.frame(
  specificity = roc_obj1$specificities,
  sensitivity = roc_obj1$sensitivities
)
roc_df1$FPR <- 1 - roc_df1$specificity
#Threshold sensitivity:
coords(roc_obj1, "best", ret = c("threshold","sensitivity","specificity"))
cal <- calibrate(lrm(Presence ~ HSMround, data = validation_data, x= TRUE, y = TRUE),
                 method = "boot", B = 500)
plot(cal)
#
(p1_1 <- ggplot(roc_df1, aes(x = FPR, y = sensitivity)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Specificity",#"False Positive Rate",
      y = "Sensitivity",#"True Positive Rate",
    ) +
    scale_x_continuous(expand = c(0,0.025))+
    scale_y_continuous(expand = c(0,0.005))+
    base_theme +
    plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_ROC.png"),
  plot = p1_1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#AUC = probability that a randomly chosen presence site has a higher predicted suitability than a randomly chosen absence site.
#0..5 = random, 0.6-0.7 = poor, 0.7-0.8 acceptable, 0.8-0.9 good, 0.9-1 excellent
#EXAMPLE: Presence probability increased significantly with HSM suitability class (logistic regression, p < 0.01). Model discrimination was acceptable (AUC = 0.76), indicating that sites with higher HSM scores were more likely to contain oysters.
#
#
#
# Flow* model:
set.seed(5432)
model2 <- glm(Presence ~ HSMround_f, data = validation_data, family = binomial)
summary(model2)
anova(model2, test = "Chisq")
#Likely due to small sample size, HSM range too narrow for true validation
# ROC (Receiver Operating Characteristic) curve shows the trade off between true positive rate and false postie rate
roc_obj2 <- pROC::roc(validation_data$Presence, fitted(model2)) #model$y
(auc_val2 <- auc(roc_obj2))
plot(roc_obj2)
roc_df2 <- data.frame(
  specificity = roc_obj2$specificities,
  sensitivity = roc_obj2$sensitivities
)
roc_df2$FPR <- 1 - roc_df2$specificity
#Threshold sensitivity:
coords(roc_obj2, "best", ret = c("threshold","sensitivity","specificity"))
cal2 <- calibrate(lrm(Presence ~ round(HSMround_f,1), data = validation_data, x= TRUE, y = TRUE),
                  method = "boot", B = 500)
plot(cal2)
#
(p1_2 <- ggplot(roc_df2, aes(x = FPR, y = sensitivity)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Specificity",#"False Positive Rate",
      y = "Sensitivity",#"True Positive Rate",
    ) +
    scale_x_continuous(expand = c(0,0.025))+
    scale_y_continuous(expand = c(0,0.005))+
    base_theme +
    plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_ROC_flow2.png"),
  plot = p1_2,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#AUC = probability that a randomly chosen presence site has a higher predicted suitability than a randomly chosen absence site.
#0..5 = random, 0.6-0.7 = poor, 0.7-0.8 acceptable, 0.8-0.9 good, 0.9-1 excellent
#
#
#
# Salinity* model:
set.seed(5432)
model3 <- glm(Presence ~ HSMround_s, data = validation_data, family = binomial)
summary(model3)
anova(model3, test = "Chisq")
#Likely due to small sample size, HSM range too narrow for true validation
# ROC (Receiver Operating Characteristic) curve shows the trade off between true positive rate and false postie rate
roc_obj3 <- pROC::roc(validation_data$Presence, fitted(model3)) #model$y
(auc_val3 <- auc(roc_obj3))
plot(roc_obj3)
roc_df3 <- data.frame(
  specificity = roc_obj3$specificities,
  sensitivity = roc_obj3$sensitivities
)
roc_df3$FPR <- 1 - roc_df3$specificity
#Threshold sensitivity:
coords(roc_obj3, "best", ret = c("threshold","sensitivity","specificity"))
cal3 <- calibrate(lrm(Presence ~ round(HSMround_s,1), data = validation_data, x= TRUE, y = TRUE),
                  method = "boot", B = 500)
plot(cal3)
#
(p1_3 <- ggplot(roc_df3, aes(x = FPR, y = sensitivity)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Specificity",#"False Positive Rate",
      y = "Sensitivity",#"True Positive Rate",
    ) +
    scale_x_continuous(expand = c(0,0.025))+
    scale_y_continuous(expand = c(0,0.005))+
    base_theme +
    plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_ROC_sal3.png"),
  plot = p1_3,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#AUC = probability that a randomly chosen presence site has a higher predicted suitability than a randomly chosen absence site.
#0..5 = random, 0.6-0.7 = poor, 0.7-0.8 acceptable, 0.8-0.9 good, 0.9-1 excellent
#
#
#
# Flow&Salinity* model:
set.seed(5432)
model4 <- glm(Presence ~ HSMround_fs, data = validation_data, family = binomial)
summary(model4)
anova(model4, test = "Chisq")
#Likely due to small sample size, HSM range too narrow for true validation
# ROC (Receiver Operating Characteristic) curve shows the trade off between true positive rate and false postie rate
roc_obj4 <- pROC::roc(validation_data$Presence, fitted(model4)) #model$y
(auc_val4 <- auc(roc_obj4))
plot(roc_obj4)
roc_df4 <- data.frame(
  specificity = roc_obj4$specificities,
  sensitivity = roc_obj4$sensitivities
)
roc_df4$FPR <- 1 - roc_df4$specificity
#Threshold sensitivity:
coords(roc_obj4, "best", ret = c("threshold","sensitivity","specificity"))
cal4 <- calibrate(lrm(Presence ~ round(HSMround_fs,1), data = validation_data, x= TRUE, y = TRUE),
                  method = "boot", B = 500)
plot(cal4)
#
(p1_4 <- ggplot(roc_df4, aes(x = FPR, y = sensitivity)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      x = "Specificity",#"False Positive Rate",
      y = "Sensitivity",#"True Positive Rate",
    ) +
    scale_x_continuous(expand = c(0,0.025))+
    scale_y_continuous(expand = c(0,0.005))+
    base_theme +
    plot_theme)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_ROC_fs4.png"),
  plot = p1_4,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#AUC = probability that a randomly chosen presence site has a higher predicted suitability than a randomly chosen absence site.
#0..5 = random, 0.6-0.7 = poor, 0.7-0.8 acceptable, 0.8-0.9 good, 0.9-1 excellent
#
#
#
# Model Comparison ----
#
# Distributions:
## Compare model distributions
models_dists <- data.frame(
  value = c(HSM_data_grps_f$HSM, HSM_data_grps_f$HSM_f, HSM_data_grps_s$HSM_s, HSM_data_grps_fs$HSM_fs),
  Model = rep(c("Original", "Flow*", "Salinity*","Flow* Salinity*"),
              each = nrow(HSM_data_grps_f))) %>%
  mutate(Model = factor(Model, levels = c("Original", "Flow*", "Salinity*","Flow* Salinity*")))
# 
(p1_d <- ggplot(models_dists, aes(value)) +
    geom_density(linewidth = 1) +
    #scale_linetype_manual(values = c("solid", "longdash", "dotted", "dotdash"))+
    facet_wrap(.~Model, nrow = 2, ncol = 2)+
    scale_x_continuous("Habitat suitability score", limits = c(0,1), expand = c(0,0)) + 
    scale_y_continuous("Count", limits = c(0, 80), expand = c(0,0))+
    base_theme + FacetTheme +
    theme(legend.position = "none",
          panel.border = element_rect(color = "black"),
          panel.spacing.x = unit(2.25, "lines"),
          plot.margin = unit(c(0.2, 0.5, 0.1, 0.1), "cm")))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_distributions.png"),
  plot = p1_d,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
# By presence/HSMgrp:
(presence_summary <- validation_data %>%
    drop_na(HSMgrp) %>%
    tidyr::pivot_longer(
      cols = all_of(grep("^HSMgrp", names(validation_data), value = TRUE)),
      names_to = "HSM_type",
      values_to = "HSMgrp"
    ) %>%
    group_by(HSM_type, HSMgrp) %>%
    dplyr::summarize(
      n = n(),
      pres = sum(Presence),
      presence_rate = mean(Presence)
    ) %>% 
    mutate(
      se = sqrt((presence_rate * (1 - presence_rate)) / n),
      lower = presence_rate - 1.96 * se,
      upper = presence_rate + 1.96 * se
    ))
presence_summary$HSM_type <- factor(
  presence_summary$HSM_type,
  levels = c("HSMgrp", "HSMgrp_f", "HSMgrp_s", "HSMgrp_fs")  # <-- desired order
)
# Compile and use once
(p2 <- ggplot(presence_summary, 
              aes(x = HSMgrp, y = presence_rate)) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.7) +
    facet_wrap(.~HSM_type, nrow = 2, ncol = 2,
               labeller = labeller(HSM_type = c("HSMgrp" = "Original",
                                                "HSMgrp_f" = "Flow*",
                                                "HSMgrp_s" = "Salinity*",
                                                "HSMgrp_fs" = "Flow* Salinity*")))+
    labs(
      x = "Habitat suitability class",
      y = "Observed presence probability"
    ) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.3)) +
    base_theme +
    plot_theme + FacetTheme+
    theme(panel.border = element_rect(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 0.8),
          panel.spacing.x = unit(1.5, "lines")))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_HSM_presence_by_model.png"),
  plot = p2,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Combined ROC plot
all_roc <- rbind(
  roc_df1 %>% mutate(Model = "Original"),
  roc_df2 %>% mutate(Model = "Flow*")) %>%
  rbind(roc_df3 %>% mutate(Model = "Salinity*")) %>%
  rbind(roc_df4 %>% mutate(Model = "Flow* Salinity*")) %>%
  mutate(Model = factor(Model, levels = c("Original", "Flow*", "Salinity*", "Flow* Salinity*")))
#
(p3r <- ggplot(all_roc, aes(x = FPR, y = sensitivity)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(.~Model, nrow = 2, ncol = 2)+
    labs(
      x = "Specificity",#"False Positive Rate",
      y = "Sensitivity",#"True Positive Rate",
    ) +
    scale_x_continuous(expand = c(0,0.025))+
    scale_y_continuous(expand = c(0,0.005))+
    base_theme +
    plot_theme + FacetTheme +
    theme(panel.border = element_rect(color = "black"),
          plot.margin = unit(c(0.2, 0.25, 0.1, 0.1), "cm"),
          panel.spacing.x = unit(1.5, "lines")))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_ROC_all.png"),
  plot = p3r,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
# Boyce Index - on raw data
m1_Boyce <- ecospat.boyce(
  fit = HSM_scores$HSM, #validation_data$HSM,
  obs = validation_data$HSM[validation_data$Presence == 1],
  nclass = 0,                   
  window.w = "default",         
  res = 100,                    
  PEplot = TRUE
) 
m1_Boyce$cor
m2_Boyce <- ecospat.boyce(
  fit = HSM_data_flow$HSM_f, #validation_data$HSM_f,
  obs = validation_data$HSM_f[validation_data$Presence == 1],
  nclass = 0,                   
  window.w = "default",         
  res = 100,                    
  PEplot = TRUE
)
m2_Boyce$cor
m3_Boyce <- ecospat.boyce(
  fit = HSM_data_sal$HSM_s, #validation_data$HSM_s,
  obs = validation_data$HSM_s[validation_data$Presence == 1],
  nclass = 0,                   
  window.w = "default",         
  res = 100,                    
  PEplot = TRUE
)
m3_Boyce$cor
m4_Boyce <- ecospat.boyce(
  fit = HSM_data_fs$HSM_fs, #validation_data$HSM_fs,
  obs = validation_data$HSM_fs[validation_data$Presence == 1],
  nclass = 0,                   
  window.w = "default",         
  res = 100,                    
  PEplot = TRUE
)
m4_Boyce$cor
#
boyce_df <- data.frame(
  Model = c("Original", "Flow*", "Salinity*", "Flow* Salinity*"),
  Boyce = c(m1_Boyce$cor, m2_Boyce$cor, m3_Boyce$cor, m4_Boyce$cor)
) %>%
  mutate(Model = factor(Model, levels = c("Original", "Flow*", "Salinity*", "Flow* Salinity*")))
#
(p4b <- ggplot(boyce_df, aes(x = Model, y = Boyce)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Model",
      y = "Boyce Index",
    ) +
    scale_x_discrete(expand = c(0,0.025))+
    scale_y_continuous(expand = c(0,0), limits = c(0, 1), breaks = seq(0, 1, 0.25))+    #SL 0, 0.75    
    base_theme + 
    theme(plot.margin = unit(c(0.2, 0.25, 0.1, 0.1), "cm")))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_Boyce_cont.png"),
  plot = p4b,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Response curve
(pred_all <- bind_rows(
  ggpredict(model1, terms = "HSMround") %>% mutate(Model = "Original"),
  ggpredict(model2, terms = "HSMround_f") %>% mutate(Model = "Flow*"),
  ggpredict(model3, terms = "HSMround_s") %>% mutate(Model = "Salinity*"),
  ggpredict(model4, terms = "HSMround_fs") %>% mutate(Model = "Flow* Salinity*")
) %>% as.data.frame() %>% drop_na() %>%
    mutate(Model = factor(Model, levels = c("Original", "Flow*", "Salinity*", "Flow* Salinity*"))))
#
(p5r <- ggplot(pred_all, aes(x = x, y = predicted, linetype = Model)) +
    geom_line(linewidth = 1.5) +
    geom_ribbon(data = pred_all, aes(x, ymin = conf.low, ymax = conf.high), fill = "#666666", alpha = 0.3)+
    #geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Habitat suitabilty score",
      y = "Predicted probabillity",
    ) +
    facet_wrap(.~Model)+
    scale_linetype_manual(values = c("solid", "longdash", "dotted", "dotdash"))+
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0, 1.00), breaks = seq(0, 1.00, 0.25))+    
    base_theme + FacetTheme +
    theme(legend.position =  "none", 
          panel.border = element_rect(color = "black"),
          plot.margin = unit(c(0.2, 0.5, 0.1, 0.1), "cm"),
          panel.spacing.x = unit(2.25, "lines")))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/Comps/",Site_Code,"_", Version,"_response.png"),
  plot = p5r,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
AIC(model1, model2, model3, model4)
#
#
#
# Best model output ----
#
# Limit to HSM, HSMround, and HSMgrp of best model
# Make sure HSMgrp, HSMgyr, HSMjb, and HSM_q4 exists
(final_data_raw <- HSM_data_grps_f %>% dplyr::select(PGID, Lat_DD_Y, Long_DD_X, 
                                                contains("SC"), contains("AV"), ChnlTO, Curve_val,
                                                HSM_f, HSMround_f, HSMgrp_f, HSMgyr_f, HSMjb_f, HSM_q4_f))
#
# Make sure object is sfc
final_data_raw <- left_join(HSMmodel %>% dplyr::select(PGID, Lat_DD_Y, Long_DD_X), 
                            final_data_raw)
#
# Limit model cells to aquatic area (remove cells completely covered by land)
# Load land:
FL_outline <- st_read("Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
# Make sure same CRS
FL_outline <- st_transform(FL_outline, st_crs(final_data_raw))
#
# Determine HSM polygons covered by land polygons
covered_mat <- st_covered_by(final_data_raw, FL_outline, sparse = FALSE)
#
# Identify rows where polygon is covered by ANY land polygon
covered_any <- apply(covered_mat, 1, any)
#
# Keep only those NOT fully covered
final_data <- final_data_raw[!covered_any, ]
#
# Check final are coverage
ggplot()+
  geom_sf(data = final_data, aes(color = HSM_f))+
  scale_color_viridis_c(limits = c(0,1))
#
#

#
summary(final_data$HSMgrp_f) %>%
  as.data.frame() %>%
  mutate(Pct = round((./nrow(final_data))*100,2))
#
final_data %>% st_drop_geometry() %>%
  group_by(HSMgyr_f) %>%
  summarise(
    n = n(),
    min = min(HSM_f, na.rm = TRUE),
    max = max(HSM_f, na.rm = TRUE),
    mean = mean(HSM_f, na.rm = TRUE),
    .groups = "drop"
 ) %>%
  mutate(Pct = round((n/nrow(final_data))*100,2))
#Jenks breaks summary:
table(
  cut(final_data$HSM_f, breaks = jenks_breaks_f, include.lowest = TRUE),
  useNA = "ifany"
)
jenks.tests(classIntervals(final_data$HSM_f, style = "fixed", fixedBreaks = jenks_breaks_f))
#
(jb_plot <- ggplot(final_data, aes(x = HSM_f)) +
  geom_histogram(fill = "gray50", color = "black", bins = 30) +
  geom_vline(xintercept = jenks_breaks_f, linetype = "dashed", linewidth = 1, color = "red") +
  ggrepel::geom_text_repel(data = data.frame(x = jenks_breaks_f, y = max(hist(final_data$HSM_f, plot = FALSE)$counts)-1000), #-300000, 1000
                           aes(x = x, y = y, label = round(x, 2)), color = "red", angle = 0, direction = "y", 
                           nudge_y = max(hist(final_data$HSM_f, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                           segment.color = NA)+
  #annotate("text", x = jenks_breaks, y = 0, label = round(jenks_breaks, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
  labs(
    title = "Jenks Breakpoints Overlay",
    x = "HSM score",
    y = "Count"
  ) +
  base_theme + plot_theme +
  scale_y_continuous(expand = c(0,0), limits = c(0, 20000))+ #1250000, 20000
  scale_x_continuous(limits = c(0,1), expand = c(0,0.0025)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_final_jb_hist.png"),
  plot = jb_plot,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
summary(final_data$HSM_q4_f)
(temp_cuts <- final_data %>%
    st_drop_geometry() %>%
    group_by(HSM_q4_f) %>%
    summarise(
      n = n(),
      min = min(HSM_f, na.rm = TRUE),
      max = max(HSM_f, na.rm = TRUE),
      mean = mean(HSM_f, na.rm = TRUE),
      .groups = "drop"
    ))
#
(q4_plot <- ggplot(final_data, aes(HSM_f)) +
  geom_histogram(bins = 40, fill = "grey50", color = "black") +
  geom_vline(data = temp_cuts, aes(xintercept = min), linetype = "dashed", linewidth = 1, color = "red") +
  ggrepel::geom_text_repel(data = data.frame(x = temp_cuts$min, y = max(hist(final_data$HSM_f, plot = FALSE)$counts)-1000), #300000, 1000
                           aes(x = x, y = y, label = round(x, 3)), color = "red", angle = 0, direction = "y", 
                           nudge_y = max(hist(final_data$HSM_f, plot = FALSE)$counts) * 0.05, hjust = -0.25, vjust = 0.5,
                           segment.color = NA)+
  #annotate("text", x = temp_cuts$min, y = 0, label = round(temp_cuts$min, 2), hjust = -0.15, vjust = -0.25, color = "red", size = 5) +
  labs(
    title = "Quartile Bins Overlay",
    x = "HSM score",
    y = "Count"
  ) +
  base_theme + plot_theme +
    scale_y_continuous(expand = c(0,0), limits = c(0, 20000))+ #1200000, 20000
    scale_x_continuous(limits = c(0, 1.0), expand = c(0,0.0015)))
#
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_final_q4hist.png"),
  plot = q4_plot,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
#
HSMfunc$save_final_model_output(data = HSM_final, output_type = "all")
## Once saved, revisit code #5 for updated maps of data and model output.
#
#
#
# GT data Plotting ----
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
val_df1 %>% group_by(HSMgrp) %>%
  rstatix::get_summary_stats(Live_scale, show = c("mean", "min", "max")) %>% 
  mutate(range = max-min)  %>%
  ggplot(aes(HSMgrp, range))+
  geom_bar(stat = "identity")+
  lemon::facet_rep_grid(variable~., scales = "free_y")+
  scale_y_continuous(expand = c(0,0))+
  theme_classic() + 
  base_theme + FacetTheme
