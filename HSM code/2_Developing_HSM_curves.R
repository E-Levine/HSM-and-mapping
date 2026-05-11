###Building parameter scoring curves for Habitat Suitability Models
#
#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               stats, statforbiology, splines, 
               writexl, readxl, openxlsx, 
               install = TRUE) 
#
##Input the site code and version for this project. (Required)
Site_Code <- c("SL") #two-letter site code used throughout for identifying files
Version <- c("RE") #current version number of the model for the specified site
source("HSM code/Functions/HSM_Functions.R")
#
##Naming convention for parameter - change with each new parameter and version of a parameter to score (i.e. if multiple Salinity curves will be used don't jsut use "Salinity".)
#Name will be used in Excel file/sheet names and figure title. If redoing curves, name should be the same as the Excel sheet name.
Parameter_name = c("Salinity") # X-axis
Param_title = c("Salinity")   # Title
#
###Function to create habitat suitability parameter score curves.
##Inputs required: LineType, FitType, Parameter_values, Parameter_limits, Parameter_step, Parameter_title, Title, show_points, save_option 
#Additional required input if FitType = bimodal: bimodal_Yvalues = c(0, 0.5, 0.25, 1, 0))
#
#
##LineType options: straight, power, expoDecay, Gaussian, bimodal, logistic, skewed, step, categorical
##FitType options: "NA" (categorical), "hard" (values provided must be in output, lines directly connect points), "soft" (values provided are estimates, line is best-fit to points, max Y may not be included), "mid" (most values are included, maximum Y is mean value [power, expoDecay, Gaussian, logistic])
#
##Parameter values must be entered according to the type of curve used. Please refer to documentation to ensure the correct number of values are added.
#Curve templates: Replace the 0/0.5/1 values with your values
#Straight line: c(0, 0.25, 0.50, 0.75, 1) - increasing values for positive line, decreasing values for negative line
#Power-like curve: c(0, 0.05, 0.50, 0.95, 1)
#Exponential decay: c(0.9999, 0.95, 0.50, 0.05, 0.00001)
#Gaussian-like curve: c(0, 0.5, 1, 1, 0.5, 0)
#bimodal: requires the extra parameter of bimodal_Yvalues listing the minimum and maximum Y values to define the two peaks in the format: c(0, 1, 0, 1, 0)
#Logistic: c(0, 0, 0.5, 1, 1) - Can enter Param = NA if don't want to specify central y = 0.5 point. (i.e., c(0, 2, NA, 15, 40))
#Skewed: c(0, 1, 0.5. 0.05, 0) - unimodal skewed curve: for right-skewed enter Param normally, for left-skewed enter in reverse order
#Step: End points for all steps should be specified in Parameter_values. Values for each step can be specified using step_values. Enter 1 step_value for each Parameter_value pair. 
#Categorical: Name of each parameter factor level entered in order of parameter values listed in Parameter_limits (i.e., c("High", "Medium", "Low"))
#
##Parameter limits should be the min and max values to be considered (if numerical) or list of Score (Y) values that correspond to Parameter factor levels supplied in Parameter_values
##Parameter step is the increments by which to estimate values: i.e., 0 to 40 by step = 1 (Should be NA for categorical curves)
##Parameter title should be the x-axis title for the plot output
##Show_points: should the user-provided points used to make the curve be shown in the plot "Y" or "N"
#
#
#Continuous data example:
curve_output(LineType = "Gaussian", FitType = "hard", 
             Parameter_values = c(5, 10, 15, 25, 32.5, 40), Parameter_limits = c(0, 45), Parameter_step = 0.01, 
             show_points = "N") #, step_values = c(0, 0.25, 1, 0.75, 0)), bimodal_Yvalues = c(0, 0.5, 0.25, 1, 0))
#Categorical data example:
curve_output(LineType = "categorical", FitType = "NA", 
             Parameter_values = c("Dense Shell", "Scattered Shell"), Parameter_limits = c(1, 0.25), 
             show_points = "N")
#p <- p + labs(subtitle = expression(italic("Buffer distance (ft) = SI Score * 100")))
#
###Function to gather existing curve point data from existing model setup summary data or create new:
#Adds curve point info to Curve_Summary sheet.
curve_point_data()
#
###Function to save the curve values and/or curve figure created
#Save_options: should the figure ("figure"), the score values ("scores"), or "both" be saved to external files? Default:both
#File_Title will be used as the file name. 
save_curve_output()
#
#
###Optional: use curves from other project saved locally:
copy_curve_summary("US", "v1")
copy_curve_files("US", "v1")
#
#
# Presentation formatting ----
#
(p1 <- p + 
   geom_point(data = cp2, aes(Param, Value), color = "blue", size = 6)+
   geom_line(data = pred2, aes(Param, Value), color = "black", linetype = "dashed", linewidth = 2.5)+
  #scale_y_continuous(expand = c(0,0))+
  theme(plot.title = element_blank(),
        plot.margin = margin(0.45, 0.75, 0.2, 0.25, "cm"),
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20)))

p1$layers[[1]]$aes_params$linewidth <- 2.5 
p1$layers[[2]]$aes_params$size <- 6 
(p1)
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Curves_presentation/",Param_title,"s.png"),
  plot = p1,
  width = 9,
  height = 5,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
#
#
# Multiple lines
cp2 <- curve_points
pred2 <- predictions
pred2 <- pred2 %>% filter(Param < 0.751)
#
#
# Load existing curves to edit:
curves <- load_curve_files("SL", "v1", include_regression_curves = FALSE)
#
# Formatting:
extrafont::loadfonts(device = "win")
#
base_theme <- ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title = element_text(size = 20, face = "bold", color = "black", family = "Arial"),
    axis.text = ggplot2::element_text(size = 18, family = "Arial", color = "black"),
    axis.text.x = element_text(margin = margin(t=0.25, r=0.5, b=0, l=0.5, unit = "cm")), #unit(c(0.25, 0.5, 0, 0.5), "cm")), 
    axis.text.y = element_text(margin = margin(t=0, r=0.35, b=0, l=0, unit = "cm")), #unit(c(0, 0.25, 0, 0), "cm")),
    axis.ticks = element_line(color = "black", linewidth = 0.1),
    axis.ticks.length = unit(-0.15, "cm"),
    plot.margin = grid::unit(c(0.25, 0.25, 0, 0), "cm"),
    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5), family = "Arial"),
    plot.caption = ggplot2::element_text(face = "italic", size = 9),
    legend.title = element_text(size = 12, family = "Arial"),
    legend.text = element_text(size = 10, family = "Arial"))
#
(p1 <- p + #curves$Seagrass %>%
    #rbind(data.frame(Param = "> 1000 m", Value = 0)) %>%
    #mutate(Param = case_when(Param == "Absent/None/NA" ~ "Unclassfied/\nNone", Param == "Patchy" ~ "Discontinuous",TRUE ~ Param)) %>%
    #mutate(Param = fct_reorder(Param, Value, .desc = TRUE)) %>%
    #ggplot(aes(Param, Value))+
    #eom_histogram(stat = "identity", fill = "#333333")+
    #geom_point()+
    #xlab("Seagrass")+
    #ylab("SI score")+
    base_theme+ theme(plot.title = element_blank())+
    #scale_x_discrete(expand = c(0,0.5))+
    scale_x_continuous(expand = c(0,0.5))+
    scale_y_continuous(expand = c(0,0)))
#
#
ggsave(
  filename = paste0(Site_Code,"_", Version, "/Output/Figure files/",Site_Code,"_", Version,"_Salinity_pres.png"),
  plot = p1,
  width = 8,
  height = 4,
  units = "in",
  dpi = 300 # Use 300 dpi for high quality
)
