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
               sf, sp, terra, furrr, future,
               mgcv, fpc, fields, interp, #mgcv - interpolation, fpc::bscan - clustering
               RColorBrewer, magicfor, ecorest, #HSV scoring
               gstat, dismo, #Depth, interpolation
               install = TRUE) 
#
#
Site_code <- c("SL")       #Two letter estuary code
#
## Load data
load_WQ_data <- function(){
    flow_file <- list.files(path = "Data/Raw-data/", 
                            pattern = paste0(Site_code, "_flow_.*.xlsx"))
    if(length(flow_file) == 0) stop("No flow file found for Site_code: ", Site_code)
    flow_raw <- openxlsx::read.xlsx(file.path("Data/Raw-data/", files_flow[1]), na.strings = c("NA", " ", "", "Z"))
    #
    files_salinity <- list.files(path = "Data/Raw-data/", 
                                 pattern = paste0(Site_code, "_salinity_.*.xlsx"))
    if(length(files_salinity) == 0) stop("No salinity file found for Site_code: ", Site_code)
    #
    salinity_raw <- openxlsx::read.xlsx(file.path("Data/Raw-data/", files_salinity[1]), na.strings = c("NA", " ", "", "Z"))
    # Return both items to work with:
    assign("flow_raw", flow_raw, envir = .GlobalEnv)
    assign("salinity_raw", salinity_raw, envir = .GlobalEnv)
 #   
}
flow_raw <- 
salinity_raw

## Clean data

## Get monthly means
# Need: Date, mean monthly salinity, mean monthly flow, 30-dat back averaged flow

#
## Fit curve
#non linear, exponential decay to scatter plot of flow vs sal

#
## Estimate flow at optimal salinity values to determine range of flow

#
## Interpolate flow amounts over grid cells

#
#
## Wet year vs dry year