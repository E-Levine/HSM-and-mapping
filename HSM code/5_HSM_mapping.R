##Code for HSM mapping
##Requires shapefile of area with data layers/scores applied
#
#.rs.restartR() #Restarts session (good if rerunning after working with other files)
#graphics.off()  # turns off any plots from previous work session
#rm(list=ls(all=TRUE)) # clears out environment 
#
#Load required packages (should install missing packages as necessary) - MAKE SURE PACMAN IS INSTALLED AND RUNNING!
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, readxl, #Df manipulation, basic summary
               sf, raster, terra, fst,
               leaflet, tmap, openxlsx, writexl,
               ggrepel, extrafont,
               shadowtext,
               install = TRUE) #Mapping and figures
#Run once to get Arial:
#font_import(prompt = FALSE)
loadfonts(device = "win")
#
HSMfunc <- new.env()
source("HSM code/Functions/HSM_scoring_functions.R", local = HSMfunc)
#
#Working parameters - to be set each time a new site or version is being used Make sure to use same Site_code and Version number from setup file.
Site_Code <- c("SL") #two-letter site code
Version <- c("v1") #Model version
Final_version <- c("Y") #Final model output? Y/N
#
#
# Data setup ----
#
###Load shape file with model data: 
model_file_name <- "HSM_final_model"
model_scores_date <- c("2026-04-23")#c("2026-03-04") #
# Also loads files for scoring
shp_pattern <- paste0("^", Site_Code, "_", Version, "_", model_file_name, "_", model_scores_date, ".*\\.shp$")
shp_files <- list.files(path = file.path(paste0(Site_Code, "_", Version), "Output", "Shapefiles"),
                        pattern = shp_pattern,
                        full.names = TRUE
)
HSMmodel <- shp_files %>%
  map(st_read, quiet = TRUE) %>%
  bind_rows()

HSM_scores_t <- read_csv(paste0(Site_Code,"_", Version, "/Output/Data files/", Site_Code,"_", Version, "_final_model_scores_", model_scores_date, ".csv"))
HSM_scores <- left_join(HSMmodel, 
                        HSM_scores_t)
#
# Model area
Site_area <- st_read(paste0(Site_Code,"_", Version, "/Data/Layers/KML/", Site_Code, ".kml"))
plot(Site_area[2])
#
###State Outline
FL_outline <- st_read("Data layers/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
# Reference locations
Ref_locs <- read_xlsx("Data layers/Important_locations.xlsx", sheet = 1, .name_repair = "universal")
Cities <- Ref_locs %>% filter(Type == "City")
Loggers <- Ref_locs %>% filter(str_detect(Type, "logger"))
#
#
# Formatting ----
#
#
basetheme <- theme_classic()+
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
papertheme <- theme(
  axis.text = element_text(size = 13),
  axis.title = element_text(size = 14),
  plot.title =  element_blank(), plot.margin = margin(t = 10, r=10)
)
#
SL_overview_zoom <- coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.055, st_bbox(Site_area)["xmax"]+0.095),
                             ylim = c(st_bbox(Site_area)["ymin"]-0.045, st_bbox(Site_area)["ymax"]+0.045))
SL_zoom <- coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.015, st_bbox(Site_area)["xmax"]+0.015),
                    ylim = c(st_bbox(Site_area)["ymin"]-0.005, st_bbox(Site_area)["ymax"]+0.010))
SS_overview_zoom <- coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.03, st_bbox(Site_area)["xmax"]+0.03),
                             ylim = c(st_bbox(Site_area)["ymin"]-0.02, st_bbox(Site_area)["ymax"]+0.02))
SS_zoom <- coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.015, st_bbox(Site_area)["xmax"]+0.015),
                    ylim = c(st_bbox(Site_area)["ymin"]-0.005, st_bbox(Site_area)["ymax"]+0.010))
SS_logger_zoom <- coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.03, st_bbox(Site_area)["xmax"]+0.17),
                             ylim = c(st_bbox(Site_area)["ymin"]-0.02, st_bbox(Site_area)["ymax"]+0.02))
#
# Model summary ----
#
(Metric_summ <- HSM_scores %>% 
   st_drop_geometry() %>%
   dplyr::select(PGID, contains("SC"), -contains("Chnl")) %>%
   summarise(across(-PGID, list(
     mean = ~ mean(.x, na.rm = TRUE),
     sd   = ~ sd(.x, na.rm = TRUE),
     min  = ~ min(.x, na.rm = TRUE),
     max  = ~ max(.x, na.rm = TRUE)
     ),
     .names = "{.col}_{.fn}"
     )) %>%
   pivot_longer(
     cols = everything(),
     names_to = c("parameter", ".value"),
     names_sep = "_"))
#
write_xlsx(Metric_summ, 
           paste0(Site_Code, "_", Version,"/Output/Metric_scoring_summary_",Sys.Date(),".xlsx"), 
           format_headers = TRUE)
#
(Param_Summ <- rbind(
  HSM_scores %>% 
    st_drop_geometry() %>%
    dplyr::select(contains("Chnl"), -contains("TO")) %>%
    pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
    summarise(Mean = mean(Score, na.rm = T),
              SD = sd(Score, na.rm = T)) %>%
    mutate(Parameter = "Channel"),
  HSM_scores %>% 
    st_drop_geometry() %>%
    dplyr::select(contains("FAnnuiA")) %>%
    pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
    summarise(Mean = mean(Score, na.rm = T),
              SD = sd(Score, na.rm = T)) %>%
    mutate(Parameter = "FLowAdult")) %>%
  rbind(HSM_scores %>% 
          st_drop_geometry() %>%
          dplyr::select(contains("FAnnuiL")) %>%
          pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
          summarise(Mean = mean(Score, na.rm = T),
                    SD = sd(Score, na.rm = T)) %>%
          mutate(Parameter = "FLowLarvae")) %>%
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("FAnnui1"), contains("FAnnui2")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "FLowOutlier")) %>%
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("Buff")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "Buffer")) %>%
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("Oyst")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "Oyster")) %>%
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("SAnn")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "SalAdult")) %>%
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("SSp")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "SalLarvae")) %>% 
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("TAnn")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "TempAdult")) %>%
  rbind(
    HSM_scores %>% 
      st_drop_geometry() %>%
      dplyr::select(contains("TSp")) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Score") %>%
      summarise(Mean = mean(Score, na.rm = T),
                SD = sd(Score, na.rm = T)) %>%
      mutate(Parameter = "TempLarvae")))
#
write_xlsx(Param_Summ, 
           paste0(Site_Code, "_", Version,"/Output/Parameter_summary_",Sys.Date(),".xlsx"), 
           format_headers = TRUE)
#
#
#
(Suit_summ <- HSM_scores %>% 
    st_drop_geometry() %>%
    dplyr::select(PGID, contains("SC")) %>%
    # Reorganize data to add Suitability Group info:
    pivot_longer(
      cols = -PGID,
      names_to = "Parameter",
      values_to = "Value"
    ) %>%
    mutate(
      Group_set = case_when(
        Value >= 0.6 & Value <= 1   ~ "High_0.6",
        Value >= 0.4 & Value < 0.6  ~ "Moderate_0.4",
        Value >= 0   & Value < 0.4  ~ "Low_e",
        TRUE                        ~ NA_character_
      ),
      Group_site = case_when(
        Value >= 0.595 & Value <= 1   ~ "High_0.595",
        Value >= 0.395 & Value < 0.595  ~ "Moderate_0.395",
        Value >= 0   & Value < 0.395  ~ "Low_i",
        TRUE                        ~ NA_character_
      )
    ) %>%
    # Stack both grouping schemes
    pivot_longer(
      cols = c(Group_set, Group_site),
      names_to = "Group_type",
      values_to = "Group"
    ) %>%
    # Get counts and percentages per group*column
    count(Parameter, Group_type, Group) %>%
    group_by(Parameter, Group_type) %>%
    mutate(
      Percent = 100 * n / sum(n)
    ) %>%
    ungroup() %>%
    # Nicer output format
    pivot_wider(
      names_from = Group,
      values_from = c(n, Percent),
      names_glue = "{Group}_{.value}",
      values_fill = list(n = 0, Percent = 0)
    ) %>%
    arrange(Group_type, Parameter) %>%
    dplyr::select(Parameter, Group_type, contains("0.6"), contains("0.4"), contains("Low_e"),
                  contains("0.595"), contains("0.395"), contains("Low_i")))
#
write_xlsx(Suit_summ, 
           paste0(Site_Code, "_", Version,"/Output/Suitability_summary_",Sys.Date(),".xlsx"), 
           format_headers = TRUE)
#
## Model summary:
#
HSMmodel %>% 
  st_drop_geometry() %>%
  #mutate(HSM_r = round(HSMround, 1)) %>%
  group_by(HSMgrp_f) %>%
  summarise(n())
#
(p0 <- ggplot(HSMmodel, aes(x = HSMgrp_f)) +
  geom_histogram(stat = "count", fill = "gray50", color = "black") +
  labs(
    title = "HSM scores",
    x = "Suitability score",
    y = "Count"
  ) +
  basetheme + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 36000), breaks = seq(0, 36000, 12000))+ #2000000
  scale_x_discrete(expand = c(0.005,0))+
  theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
  papertheme + theme(axis.text.x = element_text(size = 11, angle = 20)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_final_grp_hist.png"),
  plot = p0,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
# Area map ----
#
(p1 <- ggplot()+
  geom_sf(data = Site_area, fill = "#006699")+
  geom_sf(data = FL_outline, linewidth = 0.35)+
  geom_point(data = Cities, aes(Longitude, Latitude), alpha = 0.8, size = 4)+
  geom_shadowtext(data = Cities, aes(Longitude, Latitude, label = Location),
            nudge_x = 0.05, nudge_y = -0.025, # ADJUST AS NEEDED
            size = 5, fontface = "bold", family = "Arial", 
            color = "black", bg.color = "white")+
  annotate("text", label = "Atlantic\nOcean", x = -80.12, y = 27.25, # UPDATE AS NEEDED
           fontface = "italic", size = 5, family = "Arial")+
  basetheme +
   theme(panel.background = element_rect(fill = "#CCFFFF"))+
  SS_overview_zoom)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_area_map.png"),
  plot = p1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
# 
#
# Inset/reference map ----
#
# Create bounding box polygon of your site (for clearer highlight if desired)
site_bbox <- st_as_sfc(st_bbox(Site_area))
bbox_buffer <- st_buffer(site_bbox, dist = 0)
(p2 <- ggplot() +
    geom_sf(data = FL_outline, fill = "grey90", color = "black", linewidth = 0.3) +
    geom_sf(data = bbox_buffer, 
            fill = "#CC0033",
            color = "#CC0033",
            linewidth = 1)+
    theme_void()+
    theme(panel.border =  element_rect(fill = "white", color = "black", linewidth = 0.35),
          panel.background = element_rect(fill = "white")))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_location_map.png"),
  plot = p2,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
# Flow stations ----
#
(pflow <- ggplot()+
   geom_sf(data = Site_area, fill = "#99CCFF")+
   #geom_sf(data = Site_Grid, fill = NA)+
   geom_sf(data = FL_outline)+
   # Loggers
   geom_point(data = Loggers, aes(Longitude, Latitude,  color = Type, shape = Type), alpha = 0.9, size = 4)+
   # Cities
   geom_point(data = Cities, aes(Longitude, Latitude), alpha = 0.8, size = 5.5, shape = 17)+
   geom_shadowtext(data = Cities, aes(Longitude, Latitude, label = Location),
                   nudge_x = -0.05, nudge_y = -0.03, # ADJUST AS NEEDED
                   size = 5, fontface = "bold", family = "Arial", 
                   color = "black", bg.color = "white")+
   annotate("text", label = "Atlantic\nOcean", x = -80.12, y = 27.22, # UPDATE AS NEEDED
            fontface = "italic", size = 5, family = "Arial")+
   # Extra notes
   geom_text(aes(-82.87, 29.37, label = "Suwannee\nRiver", fontface = "italic"), color = "black", size = 4.25)+
      geom_segment(aes(x = -83.065, y = 29.36, xend = -82.91, yend = 29.37), linewidth = 1)+
   geom_text(aes(-82.87, 29.13, label = "Waccasassa\nBay", fontface = "italic"), color = "black", size = 3.5)+
   geom_text(aes(-82.87, 29.26, label = "Waccasassa\nRiver", fontface = "italic"), color = "black", size = 4.25)+
      geom_segment(aes(x = -82.80, y = 29.175, xend = -82.83, yend = 29.25), linewidth = 1)+
   # Formatting
   theme_classic()+
   scale_color_manual(values = c("#333333", "#D55E00"))+
   scale_shape_manual(values = c(16, 15))+
   theme(panel.border = element_rect(color = "black", fill = NA), 
         axis.title = element_text(size = 12, color = "black"), 
         axis.text =  element_text(size = 10, color = "black"))+
   SS_logger_zoom
)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_loggers_locations.png"),
  plot = pflow,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
#
# Polygon layer scores ----
#
# Oyster habitat
(p3 <- ggplot()+
   geom_sf(data = HSM_scores, aes(color = OystAV)) +
   geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
   basetheme + legendtheme +
   scale_color_viridis_c(limits = c(0,1))+
   labs(color = "Oyster habitat") + # UPDATE AS NEEDED
   theme(axis.text.x = element_text(angle = 0, vjust = 0))+
   SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Oyster_habitat.png"),
    plot = p3,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Oyster_habitat.png"),
    plot = p3,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#
# Oyster buffers
(p4 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = BuffAV)) +
    geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
    basetheme + legendtheme +
    scale_color_viridis_c(limits = c(0,1))+
    labs(color = "Oyster buffer") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Oyster_buffer.png"),
    plot = p4,
    width = 9,
    height = 5,  
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
  } else {
    ggsave(
      filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Oyster_buffer.png"),
      plot = p4,
      width = 9,
      height = 5,  
      units = "in",
      dpi = 300 # Use 300 dpi for high quality
    )
}
#
#
# Seagrass
(p5 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = SgrsAV)) +
    geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
    basetheme + legendtheme +
    scale_color_viridis_c(limits = c(0,1))+
    labs(color = "Seagrass habitat") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Seagrass.png"),
    plot = p5,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Seagrass.png"),
    plot = p5,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#
# Channels
(p5 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = ChnlTO)) +
    geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
    basetheme + legendtheme +
    scale_color_viridis_c()+
    labs(color = "Channels") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Channels.png"),
    plot = p5,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Channels.png"),
    plot = p5,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#
# Interpolation layer scores ----
#
# Salinity
(p6 <- ggplot()+
   geom_sf(data = HSM_scores, aes(color = SAV)) +
   geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
   basetheme + legendtheme +
   scale_color_viridis_c(limits = c(0,1))+
   labs(color = "Salinity") + # UPDATE AS NEEDED
   theme(axis.text.x = element_text(angle = 0, vjust = 0)) +
   SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Salinity.png"),
    plot = p6,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Salinity.png"),
    plot = p6,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#
# Temperature
(p7 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = TAV)) +
    geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
    basetheme + legendtheme +
    scale_color_viridis_c(limits = c(0,1))+
    labs(color = "Temperature") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Temperature.png"),
    plot = p7,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Temperature.png"),
    plot = p7,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#
#
# Composite model ----
#
## HSM groups
(p8 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = HSMgrp_f, fill = HSMgrp_f), show.legend = TRUE) +
   geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
   basetheme + legendtheme +
    scale_color_viridis_d(limits = c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
                                     "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
                                     "[0.8,0.9)", "[0.9,1]"))+ 
   scale_fill_viridis_d(limits = c("[0,0.1)", "[0.1,0.2)", "[0.2,0.3)", "[0.3,0.4)",
                                   "[0.4,0.5)", "[0.5,0.6)", "[0.6,0.7)", "[0.7,0.8)",
                                   "[0.8,0.9)", "[0.9,1]"))+
    labs(color = "HSM score", fill = "HSM score") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
   guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
   SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_Composite.png"),
    plot = p8,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Composite.png"),
    plot = p8,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#
## Jenks breaks
#Make sure in proper order:
HSM_scores <- HSM_scores %>% mutate(HSMjb_f = factor(HSMjb_f, levels = c("Low", "Medium", "High")))
(p9 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = HSMjb_f, fill = HSMjb_f)) +
    geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
    basetheme + legendtheme +
    scale_color_viridis_d()+ scale_fill_viridis_d()+
    labs(color = "Jenks breaks", fill = "Jenks breaks") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))+
    SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_JenksBreaks.png"),
    plot = p9,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_JenksBreaks.png"),
    plot = p9,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
  }
#
#
## Quartile breaks
(p10 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = HSM_q4_f, fill = HSM_q4_f)) +
    geom_sf(data = Site_area, fill = NA)+ geom_sf(data = FL_outline)+
    basetheme + legendtheme +
    scale_color_viridis_d()+ scale_fill_viridis_d()+
    labs(color = "Quartile breaks", fill = "Quartile breaks") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))+
    SS_zoom)
#
if(Final_version == "Y"){
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_final_QuartileBreaks.png"),
    plot = p10,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
    )
} else {
  ggsave(
    filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_QuartileBreaks.png"),
    plot = p10,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300 # Use 300 dpi for high quality
  )
}
#
#