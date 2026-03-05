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
Site_Code <- c("SS") #two-letter site code
Version <- c("v1") #Model version
#
#
# Data setup ----
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

HSM_scores_t <- read_csv(paste0(Site_Code,"_", Version, "/Output/Data files/", Site_Code,"_", Version, "_model_scores_", model_scores_date, ".csv"))
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
#
# Area map ----
#
(p1 <- ggplot()+
  geom_sf(data = Site_area, fill = "#006699")+
  geom_sf(data = FL_outline, linewidth = 0.35)+
  geom_point(data = Ref_locs, aes(Longitude, Latitude), alpha = 0.8, size = 4)+
  geom_shadowtext(data = Ref_locs, aes(Longitude, Latitude, label = Location),
            nudge_x = 0.05, nudge_y = -0.025, # ADJUST AS NEEDED
            size = 5, fontface = "bold", family = "Arial", 
            color = "black", bg.color = "white")+
  annotate("text", label = "Atlantic\nOcean", x = -80.12, y = 27.25, # UPDATE AS NEEDED
           fontface = "italic", size = 5, family = "Arial")+
  basetheme +
   theme(panel.background = element_rect(fill = "#CCFFFF"))+
  coord_sf(xlim = c(st_bbox(Site_area)["xmin"]-0.05, st_bbox(Site_area)["xmax"]+0.05),
           ylim = c(st_bbox(Site_area)["ymin"]-0.025, st_bbox(Site_area)["ymax"]+0.025)))
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
# Polygon layer scores ----
#
# Oyster habitat
(p3 <- ggplot()+
  geom_sf(data = HSM_scores, aes(color = OystAV)) +
  basetheme + legendtheme +
  scale_color_viridis_c(limits = c(0,1))+
  labs(color = "Oyster habitat") + # UPDATE AS NEEDED
  theme(axis.text.x = element_text(angle = 0, vjust = 0)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Oyster_habitat.png"),
  plot = p3,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Oyster buffers
(p4 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = BuffAV)) +
    basetheme + legendtheme +
    scale_color_viridis_c(limits = c(0,1))+
    labs(color = "Oyster buffer") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Oyster_buffer.png"),
  plot = p4,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Seagrass
(p5 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = SgrsAV)) +
    basetheme + legendtheme +
    scale_color_viridis_c(limits = c(0,1))+
    labs(color = "Seagrass habitat") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Seagrass.png"),
  plot = p5,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Channels
(p5 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = ChnlTO)) +
    basetheme + legendtheme +
    scale_color_viridis_c()+
    labs(color = "Channels") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Channels.png"),
  plot = p5,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Interpolation layer scores ----
#
# Salinity
(p6 <- ggplot()+
   geom_sf(data = HSM_scores, aes(color = SAV)) +
   basetheme + legendtheme +
   scale_color_viridis_c(limits = c(0,1))+
   labs(color = "Salinity") + # UPDATE AS NEEDED
   theme(axis.text.x = element_text(angle = 0, vjust = 0)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Salinity.png"),
  plot = p6,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Temperature
(p7 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = TAV)) +
    basetheme + legendtheme +
    scale_color_viridis_c(limits = c(0,1))+
    labs(color = "Temperature") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Temperature.png"),
  plot = p7,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
#
# Composite model ----
#
## HSM groups
(p8 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = HSMgrp, fill = HSMgrp)) +
    basetheme + legendtheme +
    scale_color_viridis_d()+ scale_fill_viridis_d()+
    labs(color = "HSM score", fill = "HSM score") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
   guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_Composite.png"),
  plot = p8,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
## Jenks breaks
#Make sure in proper order:
HSM_scores <- HSM_scores %>% mutate(HSMjb = factor(HSMjb, levels = c("Low", "Medium", "High")))
(p9 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = HSMjb, fill = HSMjb)) +
    basetheme + legendtheme +
    scale_color_viridis_d()+ scale_fill_viridis_d()+
    labs(color = "Jenks breaks", fill = "Jenks breaks") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_JenksBreaks.png"),
  plot = p9,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
## Quartile breaks
(p10 <- ggplot()+
    geom_sf(data = HSM_scores, aes(color = HSM_q4, fill = HSM_q4)) +
    basetheme + legendtheme +
    scale_color_viridis_d()+ scale_fill_viridis_d()+
    labs(color = "Quartile breaks", fill = "Quartile breaks") + # UPDATE AS NEEDED
    theme(axis.text.x = element_text(angle = 0, vjust = 0))+
    guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)))
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Map files/",Site_Code,"_", Version,"_QuartileBreaks.png"),
  plot = p10,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#