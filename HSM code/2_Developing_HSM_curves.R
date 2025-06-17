###Building parameter scoring curves for Habitat Suitability Models
#

#
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, #Df manipulation, basic summary
               sf, raster, sp, terra, statforbiology, splines,
               tmap, tmaptools, #Mapping and figures
               RColorBrewer, magicfor, ecorest, #HSV scoring
               marmap, gstat, xts, dismo, #Depth, interpolation
               writexl,
               install = TRUE) 
#
###Continuous data
Limits <- c(0, 40)
Parameter_name <- c("Testing")
test_line <- data.frame(Value = c(0.00001, 0.05, 0.50, 0.95, 0.99999)) #(will be Y)
test_curve <- test_line %>% mutate(Param = c(0, 0.5, 1, 5, 10)) #(will be X)
#Get values to caluculate for:
seq_values_test <- seq(0, 40, by = 1)
#
fit_line_t <- lm(log(Value) ~ Param, data = test_curve)
##Soft
Spredictions_t <- data.frame(Param = seq_values_test, Value = exp(predict(fit_line_t, newdata = data.frame(Param = seq_values_test))))
##Hard
Hfit_line_t <- loess(Value ~ Param, data = test_curve, span = 0.6)
Hpredictions_t <- data.frame(Param = seq_values_test, 
                              Value = predict(Hfit_line_t, newdata = data.frame(Param = seq_values_test)))
#Hfit_line_pts_t <- approx(test_curve$Param, test_curve$Value, xout = seq_values_test)#lm(Value ~ Param, data = temp_curve)
#Hfit_line_t <- smooth.spline(Hfit_line_pts_t$x, Hfit_line_pts_t$y)
#Predict values:
#Hpredictions_t <- predict(Hfit_line_t, seq_values_test) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
Hpredictions_t <- Hpredictions_t %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))
#
ggplot()+
  geom_line(data = Hpredictions_t, aes(Param, Value), linetype = 1, linewidth = 1.5)+ #Hard
  geom_line(data = Spredictions_t, aes(Param, Value), linetype = 1, linewidth = 1.5, color = "orange")+ #Soft fit
  #geom_line(data = predictions_mid_t, aes(Param, Value), linetype = 2, linewidth = 1.5, color = "blue")+ #Mid fit
  geom_point(data = as.matrix(test_curve), aes(Param, Value), size = 3, color = "red")+
  scale_y_continuous(limits = c(0,1.5), expand = c(0,0))+
  scale_x_continuous(limits = Limits, expand = c(0,0))+
  xlab(Parameter_name) + ylab("SI Score")+
  theme_classic()
#
###Categorical data - no predictions
Limits <- NA
Parameter_name <- c("Testing")
test_line <- data.frame(Value = c(1, 0.75, 0.5, 0.25, 0)) #(will be Y)
test_curve <- test_line %>% mutate(Param = c("Live Oyster", "50/50", "Dead Oyster", "Hard Substrate", "Seagrass")) #(will be X)
test_curve <- test_curve %>% mutate(Param = factor(test_curve$Param, levels = (test_curve %>% arrange(desc("Value")))$Param)) 
ggplot()+
  geom_col(data = test_curve, aes(Param, Value), fill = "#333333") +
  scale_y_continuous(limits = c(0,1.01), expand = c(0,0))+ 
  xlab(Parameter_name) + ylab("SI Score")+
  theme_classic()
#
#
#####Function####
#
curve_output <- function(LineType, FitType, Parameter_values, Parameter_limits, Parameter_step, Parameter_title, Title, show_points, save_option, bimodial_Yvalues){
  # Create the line data frame
  if(LineType == "straight"){base_line <- data.frame(Value = c(0, 0.25, 0.50, 0.75, 1))}
  else if(LineType == "power"){base_line <- data.frame(Value = c(0, 0.05, 0.50, 0.95, 1))}
  else if(LineType == "expoDecay"){base_line <- data.frame(Value = c(0.9999, 0.95, 0.50, 0.05, 0.00001))}
  else if(LineType == "Gaussian"){base_line <- data.frame(Value = c(0, 0.5, 1, 1, 0.5, 0))}
  else if(LineType == "bimodial"){base_line <- data.frame(Value = bimodial_Yvalues)}
  else if(LineType == "logistic"){base_line <- data.frame(Value = c(0, 0, 0.5, 1, 1))}
  else if(LineType == "skewed"){base_line <- data.frame(Value = c(0, 1, 0.5, 0.05, 0))}
  else if(LineType == "categorical"){base_line <- Parameter_limits}
  else {return("LineType must be either 'straight', 'power', 'expoDecay', 'Gaussian', 'bimodial', 'logistic', or 'skewed'.")}
  #
  # Add the curve data 
  if(LineType != "categorical"){
    temp_curve <- base_line %>% mutate(Param = Parameter_values)
  } else {
    temp_curve <- data.frame(Value = as.numeric(base_line), Param = Parameter_values)
  }
  # Get values to calculate for:
  if(length(Parameter_limits) == 2){
    seq_values <- seq(Parameter_limits[1], Parameter_limits[2], by = Parameter_step) 
    } else {seq_values <- NA} 
  #Based on fit type, fit line:
  if(FitType == 'soft'){
    #Predict values:
    if(LineType == "straight"){
      fit_line <- lm(Value ~ Param, data = temp_curve)
      predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values)))
      #End straight
      } else if(LineType == "power"){
        fit_line <- nls(Value ~ a*Param^b, data = temp_curve, start = list(a = 1, b = 1))
        predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values)))
        #End power
        } else if(LineType == "expoDecay"){
          prep_model <- lm(log(Value - min(base_line)*0.5) ~ Param, data = temp_curve)
          start <- list(a = exp(coef(prep_model)[1]), b = coef(prep_model)[2], c = min(base_line)*0.5)
          fit_line <- nls(Value ~ a * exp(b * Param) + c, data = temp_curve, start = start)
          predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values)))
          #End expoDecay
          } else if(LineType == "Gaussian"){
            fit_line <- smooth.spline(temp_curve$Param, temp_curve$Value)
            predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
            #End Gaussian
            } else if(LineType == "bimodial"){
              fit_line_s <- loess(Value ~ Param, data = temp_curve, span = 0.8)
              predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
              #End bimodial
              } else if(LineType == "logistic"){
                fit_line <- glm(Value ~ Param, data = temp_curve, family = binomial(link = "logit"))
                predictions <- data.frame(Param = seq_values, Value = predict(fit_line, data.frame(Param = seq_values), type = "response")) 
                #End logistic
              } else if(LineType == "skewed"){
                fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
                fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
                predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
                #End skewed
              } else {return("LineType must be either 'straight', 'power', 'expoDecay', 'Gaussian', 'bimodial', 'logistic', or 'skewed'..")}
    ##END SOFT
    } else if(FitType == "hard"){
      if(LineType == "straight" | LineType == "power" | LineType == "expoDecay" | LineType == "logistic"){
        fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
        fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
        predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
        #End straight, power, Gaussian, expoDecay, logisitc
        } else if(LineType == "Gaussian"){
          fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) %>% data.frame() %>% mutate(y = ifelse(row_number() < which.min(y == 0), 0, y)) %>% mutate(max_x_zero = max(x[y == 0], na.rm = TRUE)) %>% mutate(y = ifelse(x > max_x_zero, 0, y)) %>% dplyr::select(-max_x_zero) #Replace missing 0 values at extremes
          fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
          predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
          #End Gaussian 
        } else if(LineType == "bimodial"){
          fit_line_pts_h <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
          fit_line_h <- smooth.spline(fit_line_pts_h$x, fit_line_pts_h$y, spar = 0.2)
          predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) #%>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (temp_curve %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
          #End binomial
          } else if(LineType == "skewed"){
            fit_line <- loess(Value ~ Param, data = temp_curve, span = 0.6)
            predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values))) 
            #End skewed
          } else {return("FitType 'hard' not an option for specified line type.")}
      ##END HARD
      } else if(FitType == "mid"){
        if(LineType == "power"){
          fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
          fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
          predictions_temp <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (temp_curve %>% filter(Value == 1))$Param, 1, Value)))
          smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value) #smooth curve
          predictions_mTemp <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) #clean data to work with
          review <- predictions_mTemp %>% filter(Param >= temp_curve$Param[4] & Param <= ((temp_curve$Param[5] - temp_curve$Param[4])/2)) #Values to review for peaks
          peak_Param <- review$Param[which.max(review$Value)] #Identify Param value at peak by identifying location of Param value with peak
          replacements <- seq((predictions_mTemp %>% filter(Param == peak_Param))$Value, (predictions_mTemp %>% filter(Param == max(Param)))$Value, length.out = length(seq(peak_Param, temp_curve$Param[5], by = Parameter_step))) #Create Values to replace with
          predictions_m <- predictions_mTemp %>% mutate(idx = if_else(Param >= peak_Param, row_number() - min(which(Param >= peak_Param)) + 1L, NA_integer_)) %>% #Add number to identify rows
            mutate(Value = if_else(Param >= peak_Param, replacements[idx], Value)) %>%  dplyr::select(-idx) #Replace values in order and remove ID column
          #End power
        } else if(LineType == "expoDecay"){
          fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
          fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.7)
          predictions_m <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (temp_curve %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
          #End expoDecay
        } else if(LineType == "Gaussian"){
          fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
          predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
          mid_data <- predictions_temp %>% mutate(Value = ifelse(Param >= temp_curve$Param[3] & Param <= temp_curve$Param[4], 1, Value))
          smooth_fit <- smooth.spline(mid_data$Param, mid_data$Value)
          predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
          #End Gaussian
          } else if(LineType == "bimodial"){
            fit_line_pts_m <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
            fit_line_m <- smooth.spline(fit_line_pts_m$x, fit_line_pts_m$y, spar = 0.5)
            predictions_m <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
            #End bimodial
          } else if(LineType == "logistic"){
            if(is.na(temp_curve$Param[3]) == FALSE){
              fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
              predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
              smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
              predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
              } else {
                temp_curve <- temp_curve %>% filter(!is.na(Param))
                fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
                predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
                smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
                predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
                #End logistic
              }
            } else if(LineType == "skewed"){
              fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
              fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
              predictions_temp <- predict(fit_line, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (temp_curve %>% filter(Value == 1))$Param, 1, Value)))
              smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
              predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
              ### End skewed
            } 
        ##END MID
        } else if(FitType == "NA"){
              predictions <- temp_curve %>% mutate(Param = factor(temp_curve$Param, levels = (temp_curve %>% arrange(desc(Value)))$Param), Value = as.numeric(Value)) 
              } else {return("FitType mid not an option for 'straight', 'power', 'expoDecay', or 'bimodial'.")}
  #
    predictions <<- predictions %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))
    #Save if specified:
    if(save_option == "scores" | save_option == "both") {
      #Desired names
      base_filename <- paste0("HSI_curves/Scores/",Title,".xlsx")
      #Check if the file already exists
      if (file.exists(base_filename)) {
        #Append current date in YYYY-MM-DD format before the extension
        date_str <- format(Sys.Date(), "%Y-%m-%d")
        #Create new file name with date appended
        new_filename <- sub("\\.xlsx$", paste0("_", date_str, ".xlsx"), base_filename)
      } else {
        new_filename <- base_filename
      }
      #Save predictions to Excel with the sheet named "Salinity_adults"
      write_xlsx(predictions, path = new_filename)
    }
  #
  #Generate the plot
  p <- ggplot() +
    {if(LineType != "categorical") geom_line(data = predictions, aes(x = Param, y = Value), linetype = 1, linewidth = 1.5)} +
    {if(LineType != "categorical" & show_points == "Y") geom_point(data = as.matrix(temp_curve), aes(Param, Value), size = 2.75, color = "red")} +
    {if(LineType == "categorical") geom_col(data = predictions, aes(Param, Value), fill = "#333333")} +
    {if(LineType != "logistic") scale_y_continuous(limits = c(-0.001, 1.01), expand = c(0, 0))} +
    {if(LineType == "logistic") scale_y_continuous(limits = c(-0.01, 1.05), expand = c(0, 0))} +
    {if(LineType != "categorical") scale_x_continuous(limits = Parameter_limits, expand = c(0, 0))} +
    xlab(Parameter_title) +  ylab("SI Score") +
    {if(!is.na(Title)) ggtitle(Title)}+
    theme_classic() +
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16)) + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), plot.title = element_text(size = 20, face = "bold"))
    
  #Print the plot
  print(p)
  
  #Save if specified
  if(save_option == "figure" | save_option == "both") {
    # Desired filename and specs
    jpg_filename <- paste0("HSI_curves/Figures/",Title,".jpg")
    width_pixels <- 1000
    aspect_ratio <- 3/4
    height_pixels <- round(width_pixels * aspect_ratio)
    #Check if the file already exists
    if (file.exists(jpg_filename)) {
      #Append current date in YYYY-MM-DD format before the extension
      date_str <- format(Sys.Date(), "%Y-%m-%d")
      #Create new filename with date appended
      new_filename <- sub("\\.jpg$", paste0("_", date_str, ".xlsx"), jpg_filename)
    } else {
      new_filename <- jpg_filename
    }
    #Save predictions to Excel with the sheet named "Salinity_adults"
    ggsave(filename = jpg_filename, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
    #End figure output
    }
}
#
##LineType options: straight, power, expoDecay, Gaussian, bimodial, logistic, skewed, categorical
##FitType options: "NA" (categorical), "hard" (values provided must be in output, lines directly connect points), "soft" (values provided are estimates, line is best-fit to points, max Y may not be included), "mid" (most values are included, maximum Y is mean value [power, expoDecay, Gaussian, logistic])
#
##Parameter values must be entered according to the type of curve used. Please refer to documentation to ensure the correct number of values are added.
#Curve templates: Replace the 0/0.5/1 values with your values
#Straight line: c(0, 0.25, 0.50, 0.75, 1) - increasing values for positive line, decreasing values for negative line
#Power-like curve: c(0, 0.05, 0.50, 0.95, 1)
#Exponential decay: c(0.9999, 0.95, 0.50, 0.05, 0.00001)
#Gaussian-like curve: c(0, 0.5, 1, 1, 0.5, 0)
#Bimodial: requires the extra parameter of bimodial_Yvalues listing the minimum and maximum Y values to define the two peaks in the format: c(0, 1, 0, 1, 0)
#Logistic: c(0, 0, 0.5, 1, 1) - Can enter Param = NA if don't want to specify central y = 0.5 point. (i.e., c(0, 2, NA, 15, 40))
#Skewed: c(0, 1, 0.5. 0.05, 0) - unimodal skewed curve: for right-skewed enter Param normally, for left-skewed enter in reverse order
#Categorical: Name of each parameter factor level entered in order of parameter values listed in Parameter_limits (i.e., c("High", "Medium", "Low"))
#
##Parameter limits should be the min and max values to be considered (if numerical) or list of Score (Y) values that correspond to Parameter factor levels supplied in Parameter_values
##Parameter step is the increments by which to estimate values: i.e., 0 to 40 by step = 1 (Should be NA for categorical curves)
##Parameter title should be the x-axis title for the plot output
##Title: title of figure produced, appears left-justified above plot; no title = NA, title = c("Some text")
##Show_points: should the user-provided points used to make the curve be shown in the plot "Y" or "N"
##Save_options: should the figure ("figure"), the score values ("scores"), "both", or "neither" be saved to external files? Title will be used as the file name
#
#Continuous data example:
curve_output(LineType = "logistic", FitType = "soft", 
             Parameter_values = c(0, 19, 19.5, 20, 40), Parameter_limits = c(0, 40), Parameter_step = 1, 
             Parameter_title = "Temperature", Title = "Temperature - Spawning", show_points = "Y",
             save_option = "both") #), bimodial_Yvalues = c(0, 0.5, 0.25, 1, 0))
#Categorical data example:
curve_output(LineType = "categorical", FitType = "NA", 
             Parameter_values = c("Offshore, Primary, \n Large Vessel, \n General, Secondary", "Tertiary", "Terminus", "Shallow, Shortcut, \n Not Designated"), Parameter_limits = c(1, 0.5, 0.333, 0.1667), Parameter_step = NA, 
             Parameter_title = "Channel designation", Title = "Buffer distance for navigational channels", show_points = "N",
             save_option = "both")

#
#
#####Curve examples####
#
curve_examples <- function(LineType, Parameter_values, Parameter_limits, Parameter_step, Parameter_title, Title, bimodial_Yvalues){
  # Create the line data frame
  if(LineType == "straight"){base_line <- data.frame(Value = c(0, 0.25, 0.50, 0.75, 1))}
  else if(LineType == "power"){base_line <- data.frame(Value = c(0, 0.05, 0.50, 0.95, 1))}
  else if(LineType == "expoDecay"){base_line <- data.frame(Value = c(0.9999, 0.95, 0.50, 0.05, 0.00001))}
  else if(LineType == "Gaussian"){base_line <- data.frame(Value = c(0, 0.5, 1, 1, 0.5, 0))}
  else if(LineType == "bimodial"){base_line <- data.frame(Value = bimodial_Yvalues)}
  else if(LineType == "logistic"){base_line <- data.frame(Value = c(0, 0, 0.5, 1, 1))}
  else if(LineType == "skewed"){base_line <- data.frame(Value = c(0, 1, 0.5, 0.05, 0))}
  else if(LineType == "categorical"){base_line <- Parameter_limits}
  else {return("LineType must be either 'straight', 'power', 'expoDecay', 'Gaussian', 'bimodial', 'logistic', or 'skewed'.")}
  #
  # Add the curve data 
  if(LineType != "categorical"){
    temp_curve <- base_line %>% mutate(Param = Parameter_values)
  } else {
    temp_curve <- data.frame(Value = as.numeric(base_line), Param = Parameter_values)
  }
  # Get values to calculate for:
  if(length(Parameter_limits) == 2){
    seq_values <- seq(Parameter_limits[1], Parameter_limits[2], by = Parameter_step) 
  } else {seq_values <- NA} 
  ##Based on line type:
  if(LineType == "straight"){
    #Soft
    fit_line_s <- lm(Value ~ Param, data = temp_curve)
    predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
    #Hard
    fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)  
    fit_line_h <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
  } else if(LineType == "power"){
    #Soft
    fit_line_s <- nls(Value ~ a*Param^b, data = temp_curve, start = list(a = 1, b = 1))
    predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
    #Hard
    fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
    fit_line_h <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
    #Mid - using fit_line_pts from Hard fit:
    fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
    predictions_temp <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (temp_curve %>% filter(Value == 1))$Param, 1, Value)))
    smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value) #smooth curve
    predictions_mTemp <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) #clean data to work with
    review <- predictions_mTemp %>% filter(Param >= temp_curve$Param[4] & Param <= ((temp_curve$Param[5] - temp_curve$Param[4])/2)) #Values to review for peaks
    peak_Param <- review$Param[which.max(review$Value)] #Identify Param value at peak by identifying location of Param value with peak
    replacements <- seq((predictions_mTemp %>% filter(Param == peak_Param))$Value, (predictions_mTemp %>% filter(Param == max(Param)))$Value, length.out = length(seq(peak_Param, temp_curve$Param[5], by = Parameter_step))) #Create Values to replace with
    predictions_m <- predictions_mTemp %>% mutate(idx = if_else(Param >= peak_Param, row_number() - min(which(Param >= peak_Param)) + 1L, NA_integer_)) %>% #Add number to identify rows
      mutate(Value = if_else(Param >= peak_Param, replacements[idx], Value)) %>%  dplyr::select(-idx) #Replace values in order and remove ID column
  } else if(LineType == "expoDecay"){
    #Soft
    prep_model <- lm(log(Value - min(base_line)*0.5) ~ Param, data = temp_curve)
    start <- list(a = exp(coef(prep_model)[1]), b = coef(prep_model)[2], c = min(base_line)*0.5)
    fit_line_s <- nls(Value ~ a * exp(b * Param) + c, data = temp_curve, start = start)
    predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
    #Hard
    fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
    fit_line_h <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
    #Mid - using fit_line_pts from Hard fit:
    fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.7)
    predictions_m <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (temp_curve %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
  } else if(LineType == "Gaussian"){
    #Soft
    fit_line_s <- smooth.spline(temp_curve$Param, temp_curve$Value)
    predictions_s <- predict(fit_line_s, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
    #Hard
    fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
    fit_line_h <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
    #Mid
    fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
    predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
    mid_data <- predictions_temp %>% mutate(Value = ifelse(Param >= temp_curve$Param[3] & Param <= temp_curve$Param[4], 1, Value))
    smooth_fit <- smooth.spline(mid_data$Param, mid_data$Value)
    predictions_m <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
  } else if(LineType == "bimodial"){
    #Soft
    fit_line_s <- loess(Value ~ Param, data = temp_curve, span = 0.8)
    predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
    #Hard
    fit_line_pts_h <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
    fit_line_h <- smooth.spline(fit_line_pts_h$x, fit_line_pts_h$y, spar = 0.2)
    predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) #%>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (temp_curve %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
    #Mid 
    fit_line_pts_m <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
    fit_line_m <- smooth.spline(fit_line_pts_m$x, fit_line_pts_m$y, spar = 0.5)
    predictions_m <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
  } else if(LineType == "logistic"){
    #Soft
    fit_line_s <- glm(Value ~ Param, data = temp_curve, family = binomial(link = "logit"))
    predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, data.frame(Param = seq_values), type = "response")) 
    #Hard
    fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
    fit_line_h <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
    #Mid
    if(is.na(temp_curve$Param[3]) == FALSE){
      fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
      predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
      smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
      predictions_m <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
    } else {
      temp_curve <- temp_curve %>% filter(!is.na(Param))
      fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
      predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
      smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
      predictions_m <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
    }
  } else if(LineType == "skewed"){
    #Soft
    fit_line_pts_s <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
    fit_line_s <- smooth.spline(fit_line_pts_s$x, fit_line_pts_s$y, spar = 0.45)
    predictions_s <- predict(fit_line_s, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
    #Hard
    fit_line_h <- loess(Value ~ Param, data = temp_curve, span = 0.6)
    predictions_h <- data.frame(Param = seq_values, Value = predict(fit_line_h, newdata = data.frame(Param = seq_values))) 
    #Mid
    fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
    fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
    predictions_temp <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (temp_curve %>% filter(Value == 1))$Param, 1, Value)))
    smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
    predictions_m <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% rename("Param" = x, "Value" = y) 
  } else if (LineType == "categorical"){
    predictions <- temp_curve %>% mutate(Param = factor(temp_curve$Param, levels = (temp_curve %>% arrange(desc("Value")))$Param), Value = as.numeric(Value)) 
  }
  #
  {if(exists("predictions_s") == TRUE) predictions_s <- predictions_s %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))}
  {if(exists("predictions_h") == TRUE) predictions_h <- predictions_h %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))}
  {if(exists("predictions_m") == TRUE) predictions_m <- predictions_m %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))}
  {if(exists("predictions") == TRUE) predictions <- predictions %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))}
  #
  # Generate the plot
  p <- ggplot() +
    {if(LineType != "categorical" & exists("predictions_s")) geom_line(data = predictions_s, aes(x = Param, y = Value), linetype = 1, linewidth = 1.75)} +
    {if(LineType != "categorical" & exists("predictions_h")) geom_line(data = predictions_h, aes(x = Param, y = Value), linetype = 1, linewidth = 1.75, color = "blue")} +
    {if(LineType != "categorical" & exists("predictions_m")) geom_line(data = predictions_m, aes(x = Param, y = Value), linetype = 2, linewidth = 1.75, color = "orange")} +
    {if(LineType != "categorical") geom_point(data = as.matrix(temp_curve), aes(Param, Value), size = 3.75, color = "red")} +
    {if(LineType == "categorical") geom_col(data = predictions, aes(Param, Value), fill = "#333333")} +
    scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0)) +
    {if(LineType != "categorical") scale_x_continuous(limits = Parameter_limits, expand = c(0.01, 0))} +
    xlab(Parameter_title) +  ylab("SI Score") +
    {if(!is.na(Title)) ggtitle(Title)} +
    theme_classic() + 
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16)) + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), plot.title = element_text(size = 20, face = "bold"))
  
  # Print the plot
  print(p)
}
#
curve_examples(LineType = "categorical", Parameter_values = c("Live", "Shell", "Hash", "Soft"), Parameter_limits = c(1, 0.75, 0.5, 0.25), Parameter_step = NA, Parameter_title = "Parameter value", Title = "Categorical curve")
#
#####Code already included in function####
#
###Fit line
##Exponential decay: -included in function -- https://stats.stackexchange.com/questions/160552/why-is-nls-giving-me-singular-gradient-matrix-at-initial-parameter-estimates
prep_model <- lm(log(Value - min(test_line)*0.5) ~ Param, data = test_curve)
start <- list(a = exp(coef(prep_model)[1]), b = coef(prep_model)[2], c = min(test_line)*0.5)
fit_line_t <- nls(Value ~ a * exp(b * Param) + c, data = test_curve, start = start)
#
##Power - included in function
fit_line_t <- nls(Value ~ a*Param^b, data = test_curve, start = list(a = 1, b = 1)) 
#
##Gaussian - included in function
fit_line_t <- lm(Value ~ Param + I(Param^2), data = test_curve)
#Fit lines: (soft = predictions_s_t, hard = predictions_t, middle = smooth_predict)
#Soft
fit_line_s_t <- smooth.spline(test_curve$Param, test_curve$Value)
predictions_s_t <- predict(fit_line_s_t, seq_values_test) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
#Hard
fit_line_pts <- approx(test_curve$Param, test_curve$Value, xout = seq_values_test) #%>% as.data.frame() %>% rename("Param" = x, "Value" = y)
fit_line_t <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
predictions_t <- predict(fit_line_t, seq_values_test) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
#Mid
#make sure max is included in soft fit:
mid_data_t <- predictions_s_t %>% mutate(Value = ifelse(Param >= test_curve$Param[3] & Param <= test_curve$Param[4], 1, Value))
smooth_fit_t <- smooth.spline(mid_data_t$Param, mid_data_t$Value)
smooth_predict_m <- predict(smooth_fit_t, seq_values_test)%>% as.data.frame() %>% rename("Param" = x, "Value" = y) %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))
#
#polynomial - included in function
#Soft
fit_line_pts <- approx(test_curve$Param, test_curve$Value, xout = seq_values_test) #%>% as.data.frame() %>% rename("Param" = x, "Value" = y)
fit_line_t <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.5)
predictions_t <- predict(fit_line_t, seq_values_test) %>% as.data.frame() %>% rename("Param" = x, "Value" = y)
#
#Hard
fit_line_l_t <- loess(Value ~ Param, data = test_curve, span = 0.8)
predictions_l_t <- data.frame(Param = seq_values_test, 
                              Value = predict(fit_line_l_t, newdata = data.frame(Param = seq_values_test))) 