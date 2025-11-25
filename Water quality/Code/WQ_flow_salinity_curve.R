##Flow and salinity data 
#
#Code to relate flow (cfs) to logger salinity data
#
#Files should be named at minimum: SiteCode_logger_[salinity|flow].
#
## Packages
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(plyr, tidyverse, data.table,#Df manipulation, basic summary
               readxl, openxlsx, progress, writexl,
               slider, minpack.lm, #rolling mean
               sf, sp, terra, furrr, future,
               mgcv, fpc, fields, interp, #mgcv - interpolation, fpc::bscan - clustering
               RColorBrewer, magicfor, ecorest, #HSV scoring
               gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
#
Site_code <- c("SL")       #Two letter estuary code
#
## Load data (logger_flow and logger_salinity files) requires xlsx files
#Make sure only desired logger data files are in the main folder
load_WQ_data <- function(){
  Stations <- openxlsx::read.xlsx(file.path("Data/Raw-data/Flow_logger_locations.xlsx"), na.strings = c("NA", " ", "", "Z"), detectDates = TRUE)
    flow_file <- list.files(path = "Data/Raw-data/", 
                            pattern = paste0(Site_code, "_logger_flow_.*.xlsx"))
    if(length(flow_file) == 0) stop("No flow file found for Site_code: ", Site_code)
    flow_raw <- openxlsx::read.xlsx(file.path("Data/Raw-data/", flow_file[1]), na.strings = c("NA", " ", "", "Z"), detectDates = TRUE)
    #
    salinity_file <- list.files(path = "Data/Raw-data/", 
                                 pattern = paste0(Site_code, "_logger_salinity_.*.xlsx"))
    if(length(salinity_file) == 0) stop("No salinity file found for Site_code: ", Site_code)
    #
    salinity_raw <- openxlsx::read.xlsx(file.path("Data/Raw-data/", salinity_file[1]), na.strings = c("NA", " ", "", "Z"), detectDates = TRUE)
    # Return both items to work with:
    assign("Loggers", Stations, envir = .GlobalEnv)
    assign("flow_raw", flow_raw, envir = .GlobalEnv)
    assign("salinity_raw", salinity_raw, envir = .GlobalEnv)
}
#
load_WQ_data()
#
## Clean data
# Total daily flow for each logger
flow_sum <- flow_raw %>% 
  rename_with(~str_to_title(.x)) %>%
  rename("Date" = Timestamp) %>%
  mutate(Site = Site_code) %>% 
  group_by(Site, Date, Station, Parameter) %>% 
  summarise(Flow = sum(Value, na.rm = T)) %>% 
  ungroup()
# Mean daily salinity for each logger
salinity_ave <- salinity_raw %>% 
  rename_with(~str_to_title(.x)) %>%
  rename("Date" = Timestamp) %>%
  mutate(Site = Site_code) %>% 
  group_by(Site, Date, Station, Parameter) %>% 
  summarise(Salinity = mean(Value, na.rm = T)) %>% 
  ungroup()
#
#
#
## Get monthly means
# Function to calculate mean monthly salinity library(dplyr, lubridate)
# Input: df (data frame with 'Date' as Date class and 'Salinity' as numeric)
# Output: A summary data frame with Year, Month, and Mean_Salinity
calculate_monthly_value <- function(df, value_col = "Salinity") {
  # Check for required columns and formats
  if (!"Date" %in% colnames(df)) {
    stop("Error: The data frame must contain a 'Date' column.")
  }
  if (!value_col %in% colnames(df)) {
    stop("Error: The data frame must contain a '",value_col,"' column.")
  }
  if (!inherits(df$Date, "Date")) {
    stop("Error: The 'Date' column must be of class 'Date'. Convert it using as.Date() if necessary.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Error: The '",value_col,"' column must be numeric.")
  }
  #
  # Ensure data is sorted by Date
  df <- df %>% arrange(Date)
  # Dynamic naming
  output_col <- paste0("Mean_", value_col)
  #
  # Calculate mean monthly salinity
  monthly_salinity <- df %>%
    mutate(Year = year(Date), Month = month(Date), Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    group_by(Date, Year, Month, Station) %>%
    summarise(!!output_col := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  #
  return(monthly_salinity)
}
#
(sal_monthly <- calculate_monthly_value(salinity_ave, "Salinity"))
(flow_monthly <- calculate_monthly_value(flow_sum, "Flow"))
#
## Combined data frame - not currently helpful
#monthly_data <- left_join(sal_monthly, flow_monthly)
#head(monthly_data)
#
## Fit curve
#
fit_salinity_flow_models <- function(flow_data, salinity_data, flow_col = "Mean_Flow", salinity_col = "Mean_Salinity"){
  # Assumptions:
  # - flow_data: data frame with columns for Date, Station, and Mean_Flow
  # - salinity_data: data frame with columns for Date, Station, and Mean_Salinity
  # - The function pairs each salinity station with each flow station by matching on Date
  # - It performs an inner join on Date, so only matching time points are used
  # - Requires at least 4 data points for fitting (to avoid underdetermined models)
  station_col <- "Station"
  time_col <- "Date"
  # Get unique station IDs
  salinity_stations <- unique(salinity_data[[station_col]])
  flow_stations <- unique(flow_data[[station_col]])
  # Initialize a list to store results
  results <- list()
  
  # Loop over each combination
  for (sal_station in salinity_stations) {
    for (flow_station in flow_stations) {
      # Subset salinity data for the current station
      sal_sub <- salinity_data %>%
        dplyr::filter(.data[[station_col]] == sal_station) %>%
        dplyr::select(.data[[time_col]], !!salinity_col := .data[[salinity_col]])
      
      # Subset flow data for the current station
      flow_sub <- flow_data %>%
        dplyr::filter(.data[[station_col]] == flow_station) %>%
        dplyr::select(.data[[time_col]], !!flow_col := .data[[flow_col]])
      
      # Merge on time (inner join to get matching time points)
      combined <- inner_join(sal_sub, flow_sub, by = time_col)
      
      # Check if there are enough data points (at least 4 for NLS)
      if (nrow(combined) >= 4) {
        # Build the formula dynamically
        formula_str <- paste0(salinity_col, " ~ y0 + (a * b) / (b + ", flow_col, ")")
        
        # Attempt to fit the model
        fit <- try(
          nlsLM(
            as.formula(formula_str),
            data = combined,
            start = list(
              y0 = min(combined[[salinity_col]], na.rm = TRUE),
              a  = max(combined[[salinity_col]], na.rm = TRUE) - min(combined[[salinity_col]], na.rm = TRUE),
              b  = median(combined[[flow_col]], na.rm = TRUE)
            )
          ),
          silent = TRUE
        )
        
        # Store the summary if fit succeeded, otherwise store an error message
        if (!inherits(fit, "try-error")) {
          results[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- summary(fit)
        } else {
          results[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- paste("Fit failed for salinity station", sal_station, "and flow station", flow_station, ":", attr(fit, "condition")$message)
        }
      } else {
        results[[paste(str_replace_all(sal_station, "_", ""), str_replace_all(flow_station, "_", ""), sep = "_")]] <- paste("Insufficient data for salinity station", sal_station, "and flow station", flow_station, "(only", nrow(combined), "matching time points)")
      }
    }
  }
  # Print the list of all model names (combinations) that were attempted
  print("Models attempted (salinityStation_flowStation):")
  print(names(results))
  return(results)
}
#
# moved to formula ####
# SigmaPlot formula
# Fit line
fit_sp <- nlsLM(
  Mean_Salinity ~ y0 + (a * b) / (b + Mean_Flow),
  data = monthly_data,
  start = list(
    y0 = min(monthly_data$Mean_Salinity, na.rm = T),
    a  = max(monthly_data$Mean_Salinity, na.rm = T) - min(monthly_data$Mean_Salinity, na.rm = T),
    b  = median(monthly_data$Mean_Flow, na.rm = T)  # good stable guess
  )
)
summary(fit_sp)
#####
#
models <- fit_salinity_flow_models(flow_monthly, sal_monthly)
#
# Calculate flow at specified salinity
flow_at_salinity_hyp2 <- function(fit, target_sal) {
  
  p <- coef(fit)
  y0 <- p["y0"]
  a  <- p["a"]
  b  <- p["b"]
  
  if (target_sal <= y0) {
    stop("Target salinity is <= y0. Flow cannot be solved.")
  }
  
  flow <- (a * b) / (target_sal - y0) - b
  return(flow)
}
# Plot fit - option to add green fill over optimal salinity range and/or flow range
ggplot_hyperbolic_fit <- function(df, fit, flow_col = "Flow", value_col = "Salinity", 
                                  Salinity_min = NULL, Salinity_max = NULL, Flow_min = NULL, Flow_max = NULL) {
  # Input validation
  if (!all(c(flow_col, value_col) %in% names(df))) {
    stop("Data frame must contain the specified flow and value columns.")
  }
  coefs <- coef(fit)
  if (!all(c("y0", "a", "b") %in% names(coefs))) {
    stop("Fit must have coefficients named 'y0', 'a', 'b'.")
  }
  if (!is.numeric(df[[flow_col]]) || !is.numeric(df[[value_col]])) {
    stop("Specified columns must be numeric.")
  }
  # Extract parameters
  p <- coef(fit)
  y0 <- p["y0"]
  a  <- p["a"]
  b  <- p["b"]
  
  # Build prediction grid
  xseq <- seq(min(df[[flow_col]]), max(df[[flow_col]]), length.out = 300)
  pred <- y0 + (a * b) / (b + xseq)
  
  pred_df <- data.frame(
    flow = xseq,
    fitted = pred
  ) %>% rename(!!flow_col := flow)

  ggplot(df, aes(x = .data[[flow_col]], y = .data[[value_col]])) +
    {if(!is.null(Salinity_min) && !is.null(Salinity_max)) annotate("rect", xmin=-Inf, xmax=Inf, ymin=Salinity_min, ymax=Salinity_max, alpha=0.6, fill="#B8FFB8")}+
    {if(!is.null(Flow_min) && !is.null(Flow_max)) annotate("rect", xmin=Flow_min, xmax=Flow_max, ymin=-Inf, ymax=Inf, alpha=0.6, fill="#97FFFF")}+
    geom_point(color = "gray30", size = 2) +
    geom_line(data = pred_df,
              aes(x = .data[[flow_col]], y = fitted),
              color = "blue", linewidth = 1.2) +
    scale_y_continuous(expand = c(0.005,0.1))+ scale_x_continuous(expand = c(0.005,0.1))+
    labs(
      x = flow_col,
      y = value_col,
      title = paste0("Hyperbolic Fit: y =",round(y0,2), " + (", round(a,2), "*", round(b,2), ")/(",round(b,2)," + x)", collapse = "")
    ) +
    theme_classic()
}
#
ggplot_hyperbolic_fit(monthly_data, fit_sp, "Mean_Flow", "Mean_Salinity")
flow_at_salinity_hyp2(fit_sp, 11.98)
flow_at_salinity_hyp2(fit_sp, 35.98)
flow_at_salinity_hyp2(fit_sp, 10.01)
flow_at_salinity_hyp2(fit_sp, 31.49)
#
ggplot_hyperbolic_fit(monthly_data, fit_sp, "Mean_Flow", "Mean_Salinity",
                      Salinity_min = 11.98, Salinity_max = 35.98,
                      Flow_min = 0, Flow_max = 907.26)
#
#
#
## Determine mean number of days in year within range 
#min and max Dates to include
optimal_flow_days <- function(df, minDate, maxDate, minFlow, maxFlow){
  df %>% 
    # Filter data to date range
    filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    # Count if within ideal range
    mutate(Year = year(Date), 
           Conditions = case_when(Flow < maxFlow & Flow > minFlow ~ 1,  
                                  TRUE ~ 0)) %>%
    # Group by year and count number of good days
    group_by(Year) %>%
    summarise(Days = sum(Conditions)) %>% 
    ungroup() %>%
    # Mean number of annual days within ideal flow at logger point
    summarise(meanDays = mean(Days)) 
}
#
Adult_optimal <- optimal_flow_days(flow_sum, "2020-01-01", "2024-12-31", -225, 907.257)
Larval_optimal <- optimal_flow_days(flow_sum, "2020-01-01", "2024-12-31", -140, 1216.267)
#
# Count number of days in month more than 1.5 SD from monthly mean
count_outlier_flow_days <- function(df, minDate, maxDate, flow_col = "Flow") {
  #
  df %>%
    # Filter data to date range
    filter(Date >= as.Date(minDate) & Date <= as.Date(maxDate)) %>%
    #Get Year and Month
    mutate(Year  = year(Date),
           Month = month(Date)) %>%
    group_by(Year, Month) %>%
    # Determine mean monthly flow, 1.5 SD, and identify outliers
    mutate(
      mean_flow = mean(.data[[flow_col]], na.rm = TRUE), #mean monthly flow
      sd_flow   = sd(.data[[flow_col]], na.rm = TRUE), #SD monthly flow
      outlier   = abs(.data[[flow_col]] - mean_flow) > 1.5 * sd_flow,
      within_1.5sd = !outlier
    ) %>%
    # Count number of outliers per MonYr, and mean flow
    summarise(
      days_outlier_flow = sum(outlier, na.rm = TRUE),
      mean_flow = (first(mean_flow)),
      .groups = "drop"
    ) %>%
    # Calculate mean number of days per month and mean flow
    summarise(mean_outlier_days = mean(days_outlier_flow),
              mean_flow = mean(mean_flow))
    }
outlier_flow <- count_outlier_flow_days(flow_sum, "2020-01-01", "2024-12-31", "Flow")
#
#
## Get compiled data of logger and river point
# Set central point based on river/flow input:

#
#
#
####Example idea####
interpolate_flow_grid <- function(salinity_grid, flow_value,
                                  mode = c("proportional", "inverse"),
                                  rescale_mean = TRUE) {
  mode <- match.arg(mode)
  
  # Normalize salinity to 0-1
  sal_norm <- (salinity_grid - min(salinity_grid, na.rm = TRUE)) /
    (max(salinity_grid, na.rm = TRUE) - min(salinity_grid, na.rm = TRUE))
  
  # Apply transformation
  flow_grid <- switch(mode,
                      proportional = flow_value * sal_norm,
                      inverse      = flow_value * (1 - sal_norm))
  
  # Optional: rescale so mean matches the measured flow
  if(rescale_mean) {
    flow_grid <- flow_grid / mean(flow_grid, na.rm = TRUE) * flow_value
  }
  
  return(flow_grid)
}

# Simulate 100x100 salinity grid
set.seed(123)
salinity_grid <- matrix(runif(10000, 5, 35), nrow = 100, ncol = 100)

# Single Flow measurement
flow_value <- 50

# Interpolate Flow over grid, proportional to Salinity
flow_grid <- interpolate_flow_grid(salinity_grid, flow_value,
                                   mode = "proportional",
                                   rescale_mean = TRUE)
library(ggplot2)
library(reshape2)

flow_df <- melt(flow_grid)  # convert matrix to long format

ggplot(flow_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Interpolated Flow Grid",
       x = "X", y = "Y", fill = "Flow") +
  theme_minimal()

#
#
### Other possible data ####
# 
## Metrics
# Function to calculate 30-day back averaged salinity (rolling 30-day mean) library(dplyr, slider)
# Input: df (data frame with 'Date' as Date class and 'Salinity' as numeric)
#        complete (logical, default TRUE): If TRUE, only calculate when full 30-day window is available; if FALSE, use partial windows
# Output: The input data frame with an added column 'Rolling_30d_Salinity'
calculate_rolling_30d_value <- function(df, value_col = "Salinity", complete = TRUE) {
  # Check for required columns and formats
  if (!"Date" %in% colnames(df)) {
    stop("Error: The data frame must contain a 'Date' column.")
  }
  if (!value_col %in% colnames(df)) {
    stop("Error: The data frame must contain a '",value_col,"' column.")
  }
  if (!inherits(df$Date, "Date")) {
    stop("Error: The 'Date' column must be of class 'Date'. Convert it using as.Date() if necessary.")
  }
  if (!is.numeric(df[[value_col]])) {
    stop("Error: The '",value_col,"' column must be numeric.")
  }
  #
  # Ensure data is sorted by Date
  df <- df %>% arrange(Date)
  # Dynamic naming
  output_col <- paste0("Roll3d_", value_col)
  #
  # Calculate rolling 30-day mean
  df[[output_col]] <- slide_period_dbl(
    .x = df[[value_col]],
    .i = df$Date,
    .period = "day",
    .f = ~mean(.x, na.rm = TRUE),
    .before = 29,
    .complete = complete
  )
  return(df)
}
#
(sal_30d <- calculate_rolling_30d_value(salinity_ave, "Salinity"))
#
#
#
## Curve fits
#
## Currently working with previously used formula in CERP reports
#non linear, exponential decay to scatter plot of flow vs sal library(minpack.lm) #nlsLM() is more stable/robust than base nls().
fit_month <- nlsLM(
  Mean_Salinity ~ a * exp(-b * Mean_Flow) + c,
  data = monthly_data,
  start = list(a = max(monthly_data$Mean_Salinity, na.rm = T), b = 0.01, c = min(monthly_data$Mean_Salinity, na.rm = T))
)
summary(fit_month)
#
flow_at_salinity <- function(fit, target_sal) {
  # Extract parameters
  p <- coef(fit)
  a <- p["a"]
  b <- p["b"]
  c <- p["c"]
  
  # Check valid range
  if ((target_sal - c) / a <= 0) {
    stop("Target salinity is outside the valid range of the exponential model.")
  }
  
  # Compute flow
  flow <- -(1 / b) * log((target_sal - c) / a)
  return(flow)
}

flow_at_salinity(fit_month, 10)
flow_at_salinity(fit_month, 30)
plot_flow_target <- function(df, fit, target_sal, flow_value, value_col = "Mean_Salinity") {
  # Create sequence for smooth fitted curve
  xseq <- seq(min(df[[flow_value]], na.rm = T), max(df[[flow_value]], na.rm = T), length.out = 300)
  
  # Predict salinity across flow range
  pred <- predict(fit, newdata = data.frame(!!flow_value := xseq))
  
  # Plot
  plot(df[[flow_value]], df[[value_col]],
       pch = 19, col = "gray40",
       xlab = "Flow",
       ylab = "Salinity",
       main = paste("Exponential Decay Fit with Target =", target_sal))
  
  lines(xseq, pred, col = "blue", lwd = 2)
  
  # Add point for flow at salinity target
  points(flow_value, target_sal, pch = 19, col = "red", cex = 1.4)
  abline(h = target_sal, col = "red", lty = 2)
  abline(v = flow_value, col = "red", lty = 2)
  
  text(flow_value, target_sal,
       labels = sprintf("Flow = %.2f", flow_value),
       pos = 4, col = "red")
}
plot_flow_target(monthly_data, fit_month, 20, Mean_Flow, value_col = "Mean_Salinity")
#
#
#
#
#non liner hyperbolic decay 
fit_hyp <- nlsLM(
  Mean_Salinity ~ c + a / (1 + b * Mean_Flow),
  data = monthly_data,
  start = list(
    a = max(monthly_data$Mean_Salinity, na.rm = T) - min(monthly_data$Mean_Salinity, na.rm = T),   # amplitude guess
    b = 0.01,                                  # rate constant guess
    c = min(monthly_data$Mean_Salinity, na.rm = T)                       # lower asymptote guess
  )
)
summary(fit_hyp)
#
flow_at_salinity_hyperbolic <- function(fit, target_sal) {
  # Extract parameters
  p <- coef(fit)
  a <- p["a"]
  b <- p["b"]
  c <- p["c"]
  
  # Ensure target salinity is valid
  if (target_sal <= c) {
    stop("Target salinity is <= the asymptote c. Flow cannot be solved.")
  }
  
  # Calculate flow
  flow <- (a / (target_sal - c) - 1) / b
  return(flow)
}
plot_flow_target_hyperbolic <- function(df, fit, target_sal, flow_value,
                                        value_col = "Salinity") {
  # Generate smooth curve
  xseq <- seq(min(df$flow), max(df$flow), length.out = 300)
  
  pred <- predict(fit, newdata = data.frame(flow = xseq))
  
  plot(df$flow, df[[value_col]],
       pch = 19, col = "gray40",
       xlab = "Flow", ylab = "Salinity",
       main = paste("Hyperbolic Decay Fit with Target =", target_sal))
  
  lines(xseq, pred, col = "blue", lwd = 2)
  
  # Highlight the target point
  points(flow_value, target_sal, pch = 19, col = "red", cex = 1.4)
  abline(h = target_sal, col = "red", lty = 2)
  abline(v = flow_value, col = "red", lty = 2)
  
  text(flow_value, target_sal,
       sprintf("Flow = %.2f", flow_value),
       pos = 4, col = "red")
}

