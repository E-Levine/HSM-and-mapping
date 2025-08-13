##Functions used for habitat suitability mapping project set up
##
#### SetUp_Folders
create_folders <- function(Site_Code, Version) {
  if(interactive()){
    result <- select.list(c("Yes", "No"), title = "\nCan local folders be created to organize model files in a Site and Version specific directory?")
    if(result == "No"){
      message(paste0("Local folders will not be created. Folders required for proper code functioning:
              - main directory ",Site_Code, "_", Version," folder
              - ",Site_Code, "_", Version,"/Data
              - ",Site_Code, "_", Version,"/Data/Layers                     
              - ",Site_Code, "_", Version,"/Data/Layers/KML
              - ",Site_Code, "_", Version,"/Data/HSI curves                     
              - ",Site_Code, "_", Version,"/Output
              - ",Site_Code, "_", Version,"/Output/Data files
              - ",Site_Code, "_", Version,"/Output/Figure files
              - ",Site_Code, "_", Version,"/Output/Map files
              - ",Site_Code, "_", Version,"/Output/Shapefiles"))
    } else {
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
  # Create HSI curves folder (in Data)
  HSMcurves_folder <- paste0(main_folder, "/Data/HSI curves")
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
  }
}
#
#
#
#
#### File separation
#
KML_separation <- function(Status_of_KML){
  if(interactive()){
    result<- select.list(c("Yes", "No"), title = paste0("\nCan KML files be saved locally to the '",Site_Code,"_",Version,"' folder?"))
    if(result == "No"){
      message("KML files will not be saved. A local copy of the site boundary KML is required at a minimum. Section-specific KMLs are not required.")
    } else {
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
        #List of available files 
        files <- list.files("Reference files/KML", full.names = TRUE)
        #
        for (file in files) {
          #Get the filename without the path
          filename <- basename(file)
          #Check if any of the search strings are in the filename
          if (any(str_detect(filename, search_strings))) {
            #Copy the file to locationB
            file.copy(file, file.path(paste0(Site_Code, "_", Version, "/Data/Layers/KML/"), filename))
          }
        }
        #Print a message indicating completion
        message(paste0("Files copied from [Reference files/KML/] to [", Site_Code, "_", Version, "/Data/Layers/KML]."))
      }
    }
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
    temp <- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Levels") %>% as.data.frame()
    #Load sheets if file exists and if data is required. If data isn't required, state as such. Limit to data required for model.
    if(Long_Names == "Y"){Names <<- suppressWarnings(read_excel("Reference files/Setup_data.xlsx", sheet = "Long_Names") %>% as.data.frame() %>% subset(Designation == Site_Code | Is_Used == "Y"))} else {Names <<- "Long names are not needed."}
    if(Order_of_Sections == "Y"){suppressWarnings(Sections <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Section_Order") %>% as.data.frame() %>% mutate(Order = as.integer(Order)) %>% subset(Site == Site_Code & !is.na(Order)))} else {Sections <<- "Section order is not needed."}
    if(Order_of_Parameters == "Y"){suppressWarnings(Parameters <<- read_excel("Reference files/Setup_data.xlsx", sheet = "Parameter_Order") %>% as.data.frame() %>% mutate(Priority = as.integer(Priority)) %>% subset(!is.na(Priority)))} else {Parameters <<- "Parameter order is not needed."}
    if(FL_Oysters == "Y"){Oysters <<- temp %>% subset(Parameter == "Oysters")} else {Parameters <<- "'Oyster Beds in Florida' layer is not needed."}
    if(Seagrass == "Y"){Seagrasses <<- temp %>% subset(Parameter == "Seagrass")} else {Parameters <<- "'Seagrass habitat in Florida' layer is not needed."}
    if(Shellfish_Harvest_Area_Designations == "Y"){SHAreas  <<- temp %>% subset(Parameter == "SHA_Class")} else {SHAreas <<- "Shellfish Harvest Areas is not needed."}
    if(Navigation_channels == "Y"){NavCha <<- temp %>% subset(Parameter == "Nav_Channels")} else {Parameters <<- "'Navigational channels' layer is not needed."}
    if(Aquaculture_lease == "Y"){AquaLease <<- temp %>% subset(Parameter == "Aquaculture_Lease")} else {Parameters <<- "'Aquaculture leases' layer is not needed."}
    df_list <<- list("Long_Names" = Names, 
                     "Section_Order" = Sections, 
                     "Parameter_Order" = Parameters, 
                     "FLOysters" = Oysters,
                     "Seagrass" = Seagrasses,
                     "SHAs" = SHAreas,
                     "Nav_Channels" = NavCha,
                     "Aquaculture" = AquaLease,
                     "Continuous" = temp %>% subset(Parameter %in% (Parameters %>% filter(Parameter %in% c("Salinity", "Temperature")) %>% filter(!is.na(Priority)))$Parameter))
    return(list("Excel file found. Required sheets loaded into data frames. Levels of specified parameters listed.", df_list))
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
  named_data <- object_list[(names(object_list) %in% dataframes == TRUE)]
  #Combine data levels info into one output
  selected_data <<- c(named_data[1:3], list(Parameter_Summary = bind_rows(named_data[4:(length(named_data))])))
  t_line1 <- paste("These data are to be included in the model: ", paste(unlist(dataframes), collapse = ", "))
  t_line2 <- paste("These data are NOT included in the model: ", paste(unlist(notdataframes), collapse = ", "))
  dfoutput <<- sprintf("%s\n%s", t_line1, t_line2)
  cat(dfoutput)
  selected_data[["Data not included"]] <<- notdataframes
  if(interactive()){
    result<- select.list(c("Yes", "No"), title = paste0("\nIs the list of included data correct and can the information be saved to the version tracking file?"))
    if(result == "No"){
      message("A list of the included and excluded data will not be saved to the version tracking file.")
    } else {
      write.xlsx(selected_data, file = paste0(Site_Code, "_", Version, "/Data/", Site_Code, "_", Version, "_model_setup.xlsx"), sheetName = names(selected_data))
    }
  }
}
#
#
#
#
####Creation of HSI curves
curve_output <- function(LineType, FitType, Parameter_values, Parameter_limits, Parameter_step = NA, Parameter_title, show_points, bimodal_Yvalues = NA, step_values = NA){
  #Line dataframe
  curve_points <<- line_dataframe(LineType, Parameter_values, step_values, Parameter_limits)
  #
  # Get values to calculate for:
  if(length(Parameter_limits) == 2){
    seq_values <- seq(Parameter_limits[1], Parameter_limits[2], by = Parameter_step) 
  } else {seq_values <- NA} 
  #
  #Based on line type, fit line:
  if(LineType == "straight"){
    predictions <- straight_fit(FitType, curve_points, seq_values)
  } else if(LineType == "power"){
    predictions <- power_fit(FitType, curve_points, seq_values)
  } else if(LineType == "expoDecay"){
    predictions <- expoD_fit(FitType, base_line, curve_points, seq_values)
  } else if(LineType == "Gaussian"){
    predictions <- Gaussian_fit(FitType, curve_points, seq_values)
  } else if(LineType == "bimodal"){
    predictions <- bimodal_fit(FitType, curve_points, seq_values)
  } else if(LineType == "logistic"){
    predictions <- logistic_fit(FitType, curve_points, seq_values)
  } else if(LineType == "skewed"){
    predictions <- skewed_fit(FitType, curve_points, seq_values)
  } else if(LineType == "step"){
    predictions <- step_fit(FitType, curve_points, seq_values, step_values)
  } else if(FitType == "NA"){
  predictions <- curve_points %>% mutate(Param = factor(curve_points$Param, levels = (curve_points %>% arrange(desc(Value)))$Param), Value = as.numeric(Value)) 
}
#
predictions <<- predictions %>% mutate(Value = as.numeric(ifelse(Value > 1, 1, ifelse(Value < 0, 0, Value))))
#
  #Generate the plot
  p <<- ggplot() +
    {if(LineType != "categorical") geom_line(data = predictions, aes(x = Param, y = Value), linetype = 1, linewidth = 1.5)} +
    {if(LineType != "categorical" & show_points == "Y") geom_point(data = as.matrix(curve_points), aes(Param, Value), size = 2.75, color = "red")} +
    {if(LineType == "categorical") geom_col(data = predictions, aes(Param, Value), fill = "#333333")} +
    {if(LineType != "logistic") scale_y_continuous(limits = c(-0.001, 1.01), expand = c(0, 0))} +
    {if(LineType == "logistic") scale_y_continuous(limits = c(-0.01, 1.05), expand = c(0, 0))} +
    {if(LineType != "categorical") scale_x_continuous(limits = Parameter_limits, expand = c(0, 0))} +
    xlab(Parameter_title) +  ylab("SI Score") +
    {if(!is.na(Parameter_name)) ggtitle(Parameter_name)}+
    theme_classic() +
    theme(axis.title = element_text(size = 20, color = "black"), axis.text = element_text(size = 18, color = "black")) + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), plot.title = element_text(size = 20, face = "bold"))
  
  #Print the plot
  print(p)
  message("Objects created in the global environment: 'curve_points', 'p', 'predictions'.")
}
#
###Curve data gather/combination:
curve_point_data <- function(data_table = curve_points){
  Param_summ_name <- "Parameter_summary"
  #Check for model setup Excel file, check if sheet already exists, load if exists:
  if(file.exists(paste0(Site_Code, "_", Version,"/Data/",Site_Code, "_", Version,"_model_setup.xlsx"))){
    print(paste0(Site_Code, " ", Version, " Excel file found."))
    #Get list of sheet names in model setup file. Skip if already present. 
    if(!exists("Model_sheets")){Model_sheets <<- excel_sheets(paste0(Site_Code, "_", Version,"/Data/",Site_Code, "_", Version,"_model_setup.xlsx"))}
    #
    #Load sheet if exists, message if it doesn't 
    if(Param_summ_name %in% Model_sheets) {
      assign(Param_summ_name, suppressWarnings(read_excel(paste0(Site_Code, "_", Version,"/Data/",Site_Code, "_", Version,"_model_setup.xlsx"), sheet = Param_summ_name)))
      print(paste0(Param_summ_name, " Excel sheet loaded succesfully."))
    } else {
      print(paste0(Param_summ_name, " Excel sheet will be created."))
    }
  } else {
    stop("Excel file not found. Please check that the model setup file is located in the Data folder. R code file '1_SetUp_Folders' should have already been run.")
  } 
  #
  curve_points_f <- get("data_table")
  ##Create data table or add to existing data:
  if(exists(Param_summ_name)){
    #Add data to existing data
    curve_points_f <- curve_points_f %>% mutate(Curve = Parameter_name, Date_Updated = Sys.Date()) %>% dplyr::select(Curve, everything())
    combined_data <- rbind(get(Param_summ_name), curve_points_f)
    assign(Param_summ_name, combined_data, envir = globalenv())
  } else {
    #Create data frame to save to Excel 
    combined_data <- curve_points_f %>% mutate(Curve = Parameter_name, Date_Updated = Sys.Date()) %>% dplyr::select(Curve, everything())
    assign(Param_summ_name, combined_data, envir = globalenv())
  }
  message(paste0("Objects created in the global environment: '", Param_summ_name,"', 'Model_sheets'."))
  return(tail(combined_data, 10))
}
#
#
# Save data and/or figure created
save_curve_output <- function(save_option = "both", sheet_names = Model_sheets){
  #
  if(interactive()){
    result<- select.list(c("Yes", "No"), title = paste0("\nCan a summary of the HSI curve be saved locally to the version tracking file?"))
    if(result == "No"){
      message("HSI curve summary will not be saved to the model version tracking file.")
    } else {
      #For all save types, save the curve point information:
      if (exists(Param_summ_name)) {
        temp_data <- get(Param_summ_name)
        sheet_name <- Param_summ_name
        # Load the workbook
        wb <- loadWorkbook(paste0(Site_Code, "_", Version,"/Data/",Site_Code, "_", Version,"_model_setup.xlsx"))
        # Check if the sheet exists
        if (sheet_name %in% sheet_names) {
          # If it exists, overwrite the existing sheet
          writeData(wb, sheet = sheet_name, temp_data)
        } else {
          # If it does not exist, create a new sheet
          addWorksheet(wb, sheet_name)
          writeData(wb, sheet = sheet_name, temp_data)
        }
        # Save the workbook
        saveWorkbook(wb, paste0(Site_Code, "_", Version,"/Data/",Site_Code, "_", Version,"_model_setup.xlsx"), overwrite = TRUE)
      } else {
        warning("The variable does not exist.")
      }
    }
  }
  #
  saving_occured <- FALSE
  #
  if(interactive()){
    result<- select.list(c("Yes", "No"), title = paste0("\nCan curve scoring and/or a curve figure be saved locally to the 'HSI curves' folder?"))
    if(result == "No"){
      message("Curve score and figure will not be saved.")
    } else {
      #Save if scores is specified:
      if(save_option == "scores") {
        #Desired names
        base_filename <- paste0(Site_Code, "_", Version, "/Data/HSI curves/",Parameter_name,".xlsx")
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
        return("Parameter scores were saved in 'Data/HSI curves.")
        saving_occured <- TRUE
      } 
      #Save if figure is specified:
      if(save_option == "figure") {
        # Desired file name and specs
        jpg_filename <- paste0(Site_Code, "_", Version,"/Data/HSI curves/",Parameter_name,".jpg")
        width_pixels <- 1000
        aspect_ratio <- 3/4
        height_pixels <- round(width_pixels * aspect_ratio)
        #Check if the file already exists
        if (file.exists(jpg_filename)) {
          #Append current date in YYYY-MM-DD format before the extension
          date_str <- format(Sys.Date(), "%Y-%m-%d")
          #Create new filename with date appended
          new_filename <- sub("\\.jpg$", paste0("_", date_str, ".jpg"), jpg_filename)
        } else {
          new_filename <- jpg_filename
        } 
        #Save predictions to Excel with the sheet named "Salinity_adults"
        ggsave(filename = new_filename, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
        return("Parameter scoring figure was saved in 'Data/HSI curves.")
        saving_occured <- TRUE
        #End figure output
      } 
      #Save if both is specified:
      if(save_option == "both") {
        #Desired names
        base_filename_x <- paste0(Site_Code, "_", Version, "/Data/HSI curves/",Parameter_name,".xlsx")
        #Check if the file already exists
        if (file.exists(base_filename_x)) {
          #Append current date in YYYY-MM-DD format before the extension
          date_str <- format(Sys.Date(), "%Y-%m-%d")
          #Create new file name with date appended
          new_filename_x <- sub("\\.xlsx$", paste0("_", date_str, ".xlsx"), base_filename_x)
        } else {
          new_filename_x <- base_filename_x
        }
        #Save predictions to Excel with the sheet named "Salinity_adults"
        write_xlsx(predictions, path = new_filename_x)
        #
        #
        # Desired file name and specs
        jpg_filename <- paste0(Site_Code, "_", Version,"/Data/HSI curves/",Parameter_name,".jpg")
        width_pixels <- 1000
        aspect_ratio <- 3/4
        height_pixels <- round(width_pixels * aspect_ratio)
        #Check if the file already exists
        if (file.exists(jpg_filename)) {
          #Append current date in YYYY-MM-DD format before the extension
          date_str <- format(Sys.Date(), "%Y-%m-%d")
          #Create new file name with date appended
          new_filename_p <- sub("\\.jpg$", paste0("_", date_str, ".jpg"), jpg_filename)
        } else {
          new_filename_p <- jpg_filename
        } 
        #Save predictions to Excel with the sheet named "Salinity_adults"
        ggsave(filename = new_filename_p, plot = p, width = width_pixels / 100, height = height_pixels / 100, units = "in", dpi = 300)  
        return("Parameter scores were saved in 'Data/HSI curves' and parameter scoring figure was saved in 'Data/HSI curves'.")
        saving_occured <- TRUE
        #End both output
      } 
      #
      if(!save_option %in% c("scores", "figure", "both")){
        print("Only parameter score point information will be saved.")
      }
    }
  }
}
#
#
###SUB-FUNCTIONS
#
# Create the line data frame
line_dataframe <- function(LineType, Parameter_values, step_values, Parameter_limits){
  if(LineType == "straight"){base_line <- data.frame(Value = c(0, 0.25, 0.50, 0.75, 1))}
  else if(LineType == "power"){base_line <- data.frame(Value = c(0, 0.05, 0.50, 0.95, 1))}
  else if(LineType == "expoDecay"){base_line <- data.frame(Value = c(0.9999, 0.95, 0.50, 0.05, 0.00001))}
  else if(LineType == "Gaussian"){base_line <- data.frame(Value = c(0, 0.5, 1, 1, 0.5, 0))}
  else if(LineType == "bimodal"){base_line <- data.frame(Value = bimodal_Yvalues)}
  else if(LineType == "logistic"){base_line <- data.frame(Value = c(0, 0, 0.5, 1, 1))}
  else if(LineType == "skewed"){base_line <- data.frame(Value = c(0, 1, 0.5, 0.05, 0))}
  else if(LineType == "step"){
    if(length(step_values) == 0){
      stop("Please specify the suitability values for each step range by specifying 'step_values' in the function call.")
      } else {
        #Get number of step_scores needed:
        half_count = length(Parameter_values)/2
        #Set step_scores as the base_line Values or give message about error.
        if(length(step_values) > half_count){message("Too many Parameter_values are provided. There should only be two Parameter_values per one step_score.")}
        if(length(step_values) < half_count){message("Too few Parameter_values are provided. There should be two Parameter_values per one step_score.")}
        if(length(step_values) == half_count){base_line <- data.frame(Value = rep(step_values, each = 2))}
        }
    #
  }
  else if(LineType == "categorical"){base_line <- data.frame(Value = Parameter_limits)}
  else {stop(message("LineType must be either 'straight', 'power', 'expoDecay', 'Gaussian', 'bimodal', 'logistic', 'skewed', or 'step'."))}
  # Add the curve data 
  if(LineType != "categorical"){
    base_line %>% mutate(Param = Parameter_values)
  } else {
    base_line %>% mutate(Param = Parameter_values)
  }
}
# Fits by line type:
straight_fit <- function(FitType, curve_points, seq_values){
  if(FitType == "soft" | FitType == "mid"){
    fit_line <- lm(Value ~ Param, data = curve_points)
    predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values)))
    return(predictions)
    #End soft
  } else if(FitType == "hard"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values)
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  }
}
#
power_fit <- function(FitType, curve_points, seq_values){
  if(FitType == "soft"){
    fit_line <- nls(Value ~ a*Param^b, data = curve_points, start = list(a = 1, b = 1))
    predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values)))
    return(predictions)
  } else if(FitType == "hard"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values)
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  } else if(FitType == "mid"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values)
    fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
    predictions_temp <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (curve_points %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (curve_points %>% filter(Value == 1))$Param, 1, Value)))
    smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value) #smooth curve
    predictions_mTemp <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) #clean data to work with
    review <- predictions_mTemp %>% filter(Param >= curve_points$Param[4] & Param <= ((curve_points$Param[5] - curve_points$Param[4])/2)) #Values to review for peaks
    peak_Param <- review$Param[which.max(review$Value)] #Identify Param value at peak by identifying location of Param value with peak
    replacements <- seq((predictions_mTemp %>% filter(Param == peak_Param))$Value, (predictions_mTemp %>% filter(Param == max(Param)))$Value, length.out = length(seq(peak_Param, curve_points$Param[5], by = Parameter_step))) #Create Values to replace with
    predictions <- predictions_mTemp %>% mutate(idx = if_else(Param >= peak_Param, row_number() - min(which(Param >= peak_Param)) + 1L, NA_integer_)) %>% #Add number to identify rows
      mutate(Value = if_else(Param >= peak_Param, replacements[idx], Value)) %>%  dplyr::select(-idx) #Replace values in order and remove ID column
    return(predictions)
  }
}
#
expoD_fit <- function(FitType, base_line, curve_points, seq_values){
  if(FitType == "soft"){
    prep_model <- lm(log(Value - min(base_line)*0.5) ~ Param, data = curve_points)
    start <- list(a = exp(coef(prep_model)[1]), b = coef(prep_model)[2], c = min(base_line)*0.5)
    fit_line <- nls(Value ~ a * exp(b * Param) + c, data = curve_points, start = start)
    predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values)))
    return(predictions)
  } else if(FitType == "hard"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values)
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  } else if(FitType == "mid"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values)
    fit_line_m <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.7)
    predictions <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (curve_points %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (curve_points %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
    return(predictions)
  }
}
#
Gaussian_fit <- function(FitType, curve_points, seq_values){
  if(FitType == "soft"){
    fit_line <- smooth.spline(curve_points$Param, curve_points$Value)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  } else if(FitType == "hard"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values) %>% data.frame() %>% mutate(y = ifelse(row_number(x) < which.min(y == 0), 0, y)) %>% mutate(max_x_zero = max(x[y == 0], na.rm = TRUE)) %>% mutate(y = ifelse(x > max_x_zero, 0, y)) %>% dplyr::select(-max_x_zero) #Replace missing 0 values at extremes
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  } else if(FitType == "mid"){
    fit_line_temp <- smooth.spline(curve_points$Param, curve_points$Value)
    predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
    mid_data <- predictions_temp %>% mutate(Value = ifelse(Param >= curve_points$Param[3] & Param <= curve_points$Param[4], 1, Value))
    smooth_fit <- smooth.spline(mid_data$Param, mid_data$Value)
    predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
    return(predictions)
  }
}
#
bimodal_fit <- function(FitType, curve_points, seq_values){
  if(FitType == "soft"){
    fit_line_s <- loess(Value ~ Param, data = curve_points, span = 0.8)
    predictions <- data.frame(Param = seq_values, Value = predict(fit_line_s, newdata = data.frame(Param = seq_values)))
    return(predictions)
  } else if(FitType == "hard"){
    fit_line_pts_h <- approx(curve_points$Param, curve_points$Value, xout = seq_values) 
    fit_line_h <- smooth.spline(fit_line_pts_h$x, fit_line_pts_h$y, spar = 0.2)
    predictions <- predict(fit_line_h, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) #%>% mutate(Value = ifelse(Param %in% (curve_points %>% filter(Value == 0.9999))$Param, 0.9999, ifelse(Param %in% (curve_points %>% filter(Value == 0.00001))$Param, 0.00001, Value)))
    return(predictions)
  } else if(FitType == "mid"){
    fit_line_pts_m <- approx(curve_points$Param, curve_points$Value, xout = seq_values) 
    fit_line_m <- smooth.spline(fit_line_pts_m$x, fit_line_pts_m$y, spar = 0.5)
    predictions <- predict(fit_line_m, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
    return(predictions)
  }
}
#
logistic_fit <- function(FitType, curve_points, seq_values){
  if(FitType == "soft"){
    fit_line <- glm(Value ~ Param, data = curve_points, family = binomial(link = "logit"))
    predictions <- data.frame(Param = seq_values, Value = predict(fit_line, data.frame(Param = seq_values), type = "response")) 
    return(predictions)
  } else if(FitType == "hard"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values)
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  } else if(FitType == "mid"){
    if(is.na(curve_points$Param[3]) == FALSE){
      fit_line_temp <- smooth.spline(curve_points$Param, curve_points$Value)
      predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
      smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
      predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    } else {
      curve_points <- curve_points %>% filter(!is.na(Param))
      fit_line_temp <- smooth.spline(curve_points$Param, curve_points$Value)
      predictions_temp <- predict(fit_line_temp, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
      smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
      predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
    }
    return(predictions)
  }
}
#
skewed_fit <- function(FitType, curve_points, seq_values){
  if(FitType == "soft"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values) 
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
    predictions <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y)
    return(predictions)
  } else if(FitType == "hard"){
    fit_line <- loess(Value ~ Param, data = curve_points, span = 0.6)
    predictions <- data.frame(Param = seq_values, Value = predict(fit_line, newdata = data.frame(Param = seq_values))) 
    return(predictions)
  } else if(FitType == "mid"){
    fit_line_pts <- approx(curve_points$Param, curve_points$Value, xout = seq_values) 
    fit_line <- smooth.spline(fit_line_pts$x, fit_line_pts$y, spar = 0.45)
    predictions_temp <- predict(fit_line, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) %>% mutate(Value = ifelse(Param %in% (curve_points %>% filter(Value == 0))$Param, 0, ifelse(Param %in% (curve_points %>% filter(Value == 1))$Param, 1, Value)))
    smooth_fit <- smooth.spline(predictions_temp$Param, predictions_temp$Value)
    predictions <- predict(smooth_fit, seq_values) %>% as.data.frame() %>% dplyr::rename("Param" = x, "Value" = y) 
    return(predictions)
  }
}
#
step_fit <- function(FitType, curve_points, seq_values, step_values){
  if(FitType == "hard"){
    step_scores <- step_values
    #Identify indices where Value changes
    change_indices <- which(diff(curve_points$Value) != 0)
    #Identify indices where Value does NOT change
    nochange_indices <- which(diff(curve_points$Value) == 0)
    
    ##Predicting "between" (segment) values: 
    #Initialize a list to store models
    models <- list()
    preds <- list()
    #Loop through each segment defined by change_indices
    for (i in seq_along(change_indices)) {
      #Define the start and end indices for the segment
      start_index <- change_indices[i]
      end_index <- change_indices[i] + 1
      #Get all values for the current segment
      segment_data <- curve_points[start_index:(end_index), ]
      segment_seq <- seq_values[seq_values >= min(segment_data$Param, na.rm = TRUE) & seq_values <= max(segment_data$Param, na.rm = TRUE)]
      #Fit a linear model to the segment and get predicted values
      model <- lm(Value ~ Param, data = segment_data)
      pred <- data.frame(Param = segment_seq, Value = predict(model, newdata = data.frame(Param = segment_seq)))
      #Store the model and predictions in the lists
      models[[i]] <- model
      preds[[i]] <- pred
    }
    
    ##Add missing values where Value does NOT change:
    #Create a new data frame to fill in missing Param values
    nochange_data <- data.frame(Param = integer(), Value = numeric())
    #Loop through indices to fill in missing values
    for (i in seq_along(nochange_indices)) {
      start_index_n <- nochange_indices[i]
      end_index_n <- nochange_indices[i] + 1
      #Get the current Value
      current_value <- curve_points$Value[start_index_n]
      #Fill in missing Param values
      if (end_index_n <= nrow(curve_points)) {
        for (param in (curve_points$Param[start_index_n] + 1):(curve_points$Param[end_index_n] - 1)) {
          nochange_data <- rbind(nochange_data, data.frame(Param = param, Value = current_value))
        }
      }
      }
    #Combine predicted values into one dataframe:
    predictions <- rbind(rbind(nochange_data, curve_points),
                         do.call(rbind, preds) %>% filter(!Param %in% curve_points$Param))  %>% 
      arrange(Param)
    return(predictions)
  } else stop("Only FitType option for a 'step' curve is 'hard'.")
}