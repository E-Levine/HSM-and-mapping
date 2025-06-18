##Functions used for habitat suitability mapping project set up
##
#### SetUp_Folders
create_folders <- function(Site_Code, Version) {
  # Create main folder
  main_folder <- paste0(Site_Code, "_", Version)
  if (!dir.exists(main_folder)) {
    dir.create(main_folder) 
    print(paste(Site_Code, " version ", Version, " folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " folder already exists."))
  }
  # Create Data folder
  data_folder <- paste0(main_folder, "/Data")
  if (!dir.exists(data_folder)) {
    dir.create(data_folder) 
    print(paste(Site_Code, " version ", Version, " Data folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Data folder already exists."))
  }
  # Create Data/Layers folder
  layers_folder <- paste0(main_folder, "/Data/Layers")
  if (!dir.exists(layers_folder)) {
    dir.create(layers_folder) 
    print(paste(Site_Code, " version ", Version, " Layers folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Layers folder already exists."))
  }
  # Create KML folder (in Data/Layers)
  kml_folder <- paste0(main_folder, "/Data/Layers/KML")
  if (!dir.exists(kml_folder)) {
    dir.create(kml_folder)  
    print(paste(Site_Code, " version ", Version, " KML folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " KML folder already exists."))
  }
  # Create HSI curves folder (in Data/Layers)
  HSMcurves_folder <- paste0(main_folder, "/Data/Layers/HSI curves")
  if (!dir.exists(HSMcurves_folder)) {
    dir.create(HSMcurves_folder)  
    print(paste(Site_Code, " version ", Version, " HSI curves folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " HSI curves folder already exists."))
  }
  # Create Output folder
  output_folder <- paste0(main_folder, "/Output")
  if (!dir.exists(output_folder)) {
    dir.create(output_folder) 
    print(paste(Site_Code, " version ", Version, " Output folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Output folder already exists."))
  }
  # Create Output/Data files folder
  outputData_folder <- paste0(main_folder, "/Output/Data files")
  if (!dir.exists(outputData_folder)) {
    dir.create(outputData_folder) 
    print(paste(Site_Code, " version ", Version, " Output Data files folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Output Data files folder already exists."))
  }
  # Create Output/Figure files folder
  outputFigure_folder <- paste0(main_folder, "/Output/Figure files")
  if (!dir.exists(outputFigure_folder)) {
    dir.create(outputFigure_folder) 
    print(paste(Site_Code, " version ", Version, " Output Figure files folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Output Figure files folder already exists."))
  }
  # Create Output/Map files folder
  outputMap_folder <- paste0(main_folder, "/Output/Map files")
  if (!dir.exists(outputMap_folder)) {
    dir.create(outputMap_folder) 
    print(paste(Site_Code, " version ", Version, " Output Map files folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Output Map files folder already exists."))
  }
  # Create Output/Shapefiles folder
  outputShape_folder <- paste0(main_folder, "/Output/Shapefiles")
  if (!dir.exists(outputShape_folder)) {
    dir.create(outputShape_folder) 
    print(paste(Site_Code, " version ", Version, " Output Shapefiles folder created."))
  } else {
    print(paste(Site_Code, " version ", Version, " Output Shapefiles folder already exists."))
  }
}
#
#
#
#
#### File separation
#
KML_separation <- function(Status_of_KML){
  if(length(Status_of_KML) == 1){
    #If separation required:
    
    kml_file <- st_read(paste0("Reference files/KML/PreProcessing/", Site_Code, "_", Version, "/", Site_Code,"_all.kml")) #Load file
    Polygons <- kml_file %>% group_by(Name) %>% summarise(count = n()) #Identify all polygons
    filelist <- list()
    for (i in 1:nrow(Polygons)) {
      name <- Polygons$Name[1]
      polygon <- kml_file %>% filter(Name == Polygons$Name[i]) #Get the current unique polygon
      filename <- paste0("Reference files/KML/", Polygons$Name[i], ".kml") #Write to separate KML file within References/KML folder
      st_write(polygon, filename, driver = "kml", append = FALSE)
      st_write(polygon, paste0(Site_Code, "_", Version, "/Data/Layers/KML/", Polygons$Name[i], ".kml"), driver = "kml", append = FALSE) #Write to HSM folder for records.
      filelist[i] <- paste("Polygon", name, "to", filename)
    }
    return(filelist)
  } else {
    search_strings  <- Status_of_KML
    # List of available files 
    files <- list.files("Reference files/KML", full.names = TRUE)
    #
    for (file in files) {
      # Get the filename without the path
      filename <- basename(file)
      
      # Check if any of the search strings are in the filename
      if (any(str_detect(filename, search_strings))) {
        # Copy the file to locationB
        file.copy(file, file.path(paste0(Site_Code, "_", Version, "/Data/Layers/KML/"), filename))
      }
    }
    # Print a message indicating completion
    message(paste0("Files copied from [Reference files/KML/] to [", Site_Code, "_", Version, "/Data/Layers/KML]."))
  }
}
#
#
#
#### Parameter Assignment
#
##Function to read excel files and limit to required setup data
Gather_setup_data <- function(Long_Names, Order_of_Sections, Order_of_Parameters, FL_Oysters, Shellfish_Harvest_Area_Designations){
  if(file.exists("Reference files/Setup_data.xlsx")){
    #Load sheets if file exists and if data is required. If data isn't required, state as such. Limit to data required for model.
    if(Long_Names == "Y"){Names <<- suppressWarnings(read_excel("Reference files/Setup_data.xlsx", sheet = "Long_Names") %>% as.data.frame() %>% subset(Designation == Site_Code | Is_Used == "Y"))} else {Names <<- "Long names are not needed."}
    if(Order_of_Sections == "Y"){suppressWarnings(Sections <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Section_Order") %>% as.data.frame() %>% mutate(Order = as.integer(Order)) %>% subset(Site == Site_Code & !is.na(Order)))} else {Sections <<- "Section order is not needed."}
    if(Order_of_Parameters == "Y"){suppressWarnings(Parameters <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Order") %>% as.data.frame() %>% mutate(Priority = as.integer(Priority)) %>% subset(!is.na(Priority)))} else {Parameters <<- "Parameter order is not needed."}
    if(FL_Oysters == "Y"){suppressWarnings(Oysters <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Scoring") %>% as.data.frame() %>% mutate(Score = as.numeric(Score)) %>% subset(Parameter == "Oysters" & !is.na(Score)))} else {Parameters <<- "'Oyster Beds in Florida' layer is not needed."}
    if(Shellfish_Harvest_Area_Designations == "Y"){suppressWarnings(SHAreas  <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Scoring") %>% as.data.frame() %>% mutate(Score = as.numeric(Score)) %>% subset(Parameter == "SHA_Class" & !is.na(Score)))} else {SHAreas <<- "Shellfish Harvest Areas is not needed."}
    df_list <<- list("Long_Names" = Names, 
                     "Section_Order" = Sections, 
                     "Parameter_Order" = Parameters, 
                     "FLOysters" = Oysters,
                     "SHAs" = SHAreas)
    return(list("Excel file found. Required sheets loaded into data frames.", df_list))
  } else {
    return(print("Excel file not found. Please find the data file and check for required data."))
  } 
}
#
#
##Function to check list of objects, identify which are data frames or text.
Identify_dataframes <- function(object_list){
  #Empty object to fill
  dataframes <- list()
  notdataframes <- list()
  #Identify objects that are data frames
  df_types <- sapply(object_list, is.data.frame)
  #Return names of objects that are data frames 
  dataframes <- names(df_types[df_types == TRUE])
  notdataframes <- names(df_types[df_types == FALSE])
  selected_data <<- object_list[(names(object_list) %in% dataframes == TRUE)]
  t_line1 <- paste("These data are to be included in the model: ", paste(unlist(dataframes), collapse = ", "))
  t_line2 <- paste("These data are NOT included in the model: ", paste(unlist(notdataframes), collapse = ", "))
  dfoutput <<- sprintf("%s\n%s", t_line1, t_line2)
  cat(dfoutput)
  selected_data[["Data not included"]] <<- notdataframes
}
#
#
#
#
####Creation of HSI curves
curve_output <- function(LineType, FitType, Parameter_values, Parameter_limits, Parameter_step, Parameter_title, Title, show_points, save_option, bimodal_Yvalues){
  # Create the line data frame
  if(LineType == "straight"){base_line <- data.frame(Value = c(0, 0.25, 0.50, 0.75, 1))}
  else if(LineType == "power"){base_line <- data.frame(Value = c(0, 0.05, 0.50, 0.95, 1))}
  else if(LineType == "expoDecay"){base_line <- data.frame(Value = c(0.9999, 0.95, 0.50, 0.05, 0.00001))}
  else if(LineType == "Gaussian"){base_line <- data.frame(Value = c(0, 0.5, 1, 1, 0.5, 0))}
  else if(LineType == "bimodal"){base_line <- data.frame(Value = bimodal_Yvalues)}
  else if(LineType == "logistic"){base_line <- data.frame(Value = c(0, 0, 0.5, 1, 1))}
  else if(LineType == "skewed"){base_line <- data.frame(Value = c(0, 1, 0.5, 0.05, 0))}
  else if(LineType == "categorical"){base_line <- Parameter_limits}
  else {return("LineType must be either 'straight', 'power', 'expoDecay', 'Gaussian', 'bimodal', 'logistic', or 'skewed'.")}
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
      predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
      #End Gaussian
    } else if(LineType == "bimodal"){
      fit_line_s <- loess(Value ~ Param, data = temp_curve, span = 0.8)
      predictions_s <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
      #End bimodal
    } else if(LineType == "logistic"){
      fit_line <- glm(Value ~ Param, data = temp_curve, family = binomial(link = "logit"))
      predictions <- data.frame(Param = seq_values, Value = predict(fit_line, data.frame(Param = seq_values), type = "response")) 
      #End logistic
    } else if(LineType == "skewed"){
      fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
      fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
      predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
      #End skewed
    } else {return("LineType must be either 'straight', 'power', 'expoDecay', 'Gaussian', 'bimodal', 'logistic', or 'skewed'..")}
    ##END SOFT
  } else if(FitType == "hard"){
    if(LineType == "straight" | LineType == "power" | LineType == "expoDecay" | LineType == "logistic"){
      fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
      fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
      predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
      #End straight, power, Gaussian, expoDecay, logisitc
    } else if(LineType == "Gaussian"){
      fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) %>% data.frame() %>% mutate(y = ifelse(row_number(x) < which.min(y == 0), 0, y)) %>% mutate(max_x_zero = max(x[y == 0], na.rm = TRUE)) %>% mutate(y = ifelse(x > max_x_zero, 0, y)) %>% dplyr::select(-max_x_zero) #Replace missing 0 values at extremes
      fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
      predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
      #End Gaussian 
    } else if(LineType == "bimodal"){
      fit_line_pts_h <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
      fit_line_h <- smooth.spline(fit_line_pts_h$x, fit_line_pts_h$y, spar = 0.2)
      predictions_h <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) #%>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (temp_curve %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
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
      predictions_temp <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (temp_curve %>% filter(Value == 1))$Param, 1, Value)))
      smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value) #smooth curve
      predictions_mTemp <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) #clean data to work with
      review <- predictions_mTemp %>% filter(Param >= temp_curve$Param[4] & Param <= ((temp_curve$Param[5] - temp_curve$Param[4])/2)) #Values to review for peaks
      peak_Param <- review$Param[which.max(review$Value)] #Identify Param value at peak by identifying location of Param value with peak
      replacements <- seq((predictions_mTemp %>% filter(Param == peak_Param))$Value, (predictions_mTemp %>% filter(Param == max(Param)))$Value, length.out = length(seq(peak_Param, temp_curve$Param[5], by = Parameter_step))) #Create Values to replace with
      predictions_m <- predictions_mTemp %>% mutate(idx = if_else(Param >= peak_Param, row_number() - min(which(Param >= peak_Param)) + 1L, NA_integer_)) %>% #Add number to identify rows
        mutate(Value = if_else(Param >= peak_Param, replacements[idx], Value)) %>%  dplyr::select(-idx) #Replace values in order and remove ID column
      #End power
    } else if(LineType == "expoDecay"){
      fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values)
      fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.7)
      predictions_m <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (temp_curve %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
      #End expoDecay
    } else if(LineType == "Gaussian"){
      fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
      predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
      mid_data <- predictions_temp %>% mutate(Value = ifelse(Param >= temp_curve$Param[3] & Param <= temp_curve$Param[4], 1, Value))
      smooth_fit <- smooth.spline(mid_data$Param, mid_data$Value)
      predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
      #End Gaussian
    } else if(LineType == "bimodal"){
      fit_line_pts_m <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
      fit_line_m <- smooth.spline(fit_line_pts_m$x, fit_line_pts_m$y, spar = 0.5)
      predictions_m <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
      #End bimodal
    } else if(LineType == "logistic"){
      if(is.na(temp_curve$Param[3]) == FALSE){
        fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
        predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
        smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
        predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
      } else {
        temp_curve <- temp_curve %>% filter(!is.na(Param))
        fit_line_temp <- smooth.spline(temp_curve$Param, temp_curve$Value)
        predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
        smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
        predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
        #End logistic
      }
    } else if(LineType == "skewed"){
      fit_line_pts <- approx(temp_curve$Param, temp_curve$Value, xout = seq_values) 
      fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
      predictions_temp <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (temp_curve %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (temp_curve %>% filter(Value == 1))$Param, 1, Value)))
      smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
      predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
      ### End skewed
    } 
    ##END MID
  } else if(FitType == "NA"){
    predictions <- temp_curve %>% mutate(Param = factor(temp_curve$Param, levels = (temp_curve %>% arrange(desc(Value)))$Param), Value = as.numeric(Value)) 
  } else {return("FitType mid not an option for 'straight', 'power', 'expoDecay', or 'bimodal'.")}
  #
  predictions <<- predictions %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))
  #Save if specified:
  if(save_option == "scores" | save_option == "both") {
    #Desired names
    base_filename <- paste0(Site_Code, "_", Version, "/Data/HSI curves/",Title,".xlsx")
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
  p <<- ggplot() +
    {if(LineType != "categorical") geom_line(data = predictions, aes(x = Param, y = Value), linetype = 1, linewidth = 1.5)} +
    {if(LineType != "categorical" & show_points == "Y") geom_point(data = as.matrix(temp_curve), aes(Param, Value), size = 2.75, color = "red")} +
    {if(LineType == "categorical") geom_col(data = predictions, aes(Param, Value), fill = "#333333")} +
    {if(LineType != "logistic") scale_y_continuous(limits = c(-0.001, 1.01), expand = c(0, 0))} +
    {if(LineType == "logistic") scale_y_continuous(limits = c(-0.01, 1.05), expand = c(0, 0))} +
    {if(LineType != "categorical") scale_x_continuous(limits = Parameter_limits, expand = c(0, 0))} +
    xlab(Parameter_title) +  ylab("SI Score") +
    {if(!is.na(Title)) ggtitle(Title)}+
    theme_classic() +
    theme(axis.title = element_text(size = 20, color = "black"), axis.text = element_text(size = 18, color = "black")) + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), plot.title = element_text(size = 20, face = "bold"))
  
  #Print the plot
  print(p)
  
  #Save if specified
  if(save_option == "figure" | save_option == "both") {
    # Desired filename and specs
    jpg_filename <- paste0(Site_Code, "_", Version,"/Data/HSI curves/",Title,".jpg")
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
    ggsave(filename = new_filename, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
    #End figure output
  }
}

