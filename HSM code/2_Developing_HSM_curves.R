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
Version <- c("v1") #current version number of the model for the specified site
source("HSM code/Functions/HSM_Functions.R")
#
##Naming convention for parameter - change with each new parameter and version of a parameter to score (i.e. if multiple Salinity curves will be used don't jsut use "Salinity".)
#Name will be used in Excel file/sheet names and figure title. If redoing curves, name should be the same as the Excel sheet name.
Parameter_name = c("Temperature_spawning")
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
curve_output(LineType = "logistic", FitType = "hard", 
             Parameter_values = c(0, 19, 19.5, 20, 40), Parameter_limits = c(0, 40), Parameter_step = 0.01, 
             Parameter_title = "Temperature", show_points = "Y") #, step_values = c(0, 0.25, 1, 0.75, 0)), bimodal_Yvalues = c(0, 0.5, 0.25, 1, 0))
#Categorical data example:
curve_output(LineType = "categorical", FitType = "NA", 
             Parameter_values = c("Unclassified", "Contains 'prohibited'",	"Contains 'restricted'", "Contains 'approved'"), Parameter_limits = c(1, 0.66, 0.33, 0), 
             Parameter_title = "Designation", show_points = "N")
#
#
###Function to gather existing curve point data from existing model setup summary data or create new:
curve_point_data()
#
###Function to save the curve values and/or curve figure created
#Save_options: should the figure ("figure"), the score values ("scores"), or "both" be saved to external files? Default:both
#File_Title will be used as the file name. 
save_curve_output()
#
#