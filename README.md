# HSM-and-mapping
Project for creating and updating habitat suitability models based on StateGrid pico system. Models are created according to the StateGrid(s) in which the desired area is located and within the predefined area established. Please refer to "Reference files" to determine proper StateGrid and how to set area boundaries. 
<br> *Project is under development.*
<br>
## Project set up
All files other than R code and R project files will be saved locally due to the typically large file size associated with the project. To use the project, create a fork and establish the R Project within the main local directory before running the "1_SetUp_Folders" R code. The area of interest will need to be assigned a two-letter "Site" code unless allready established. The site's two-letter code and model version will need to be determined prior to running the setup code. Running the setup code will create all necessary folders and will provide code for separating KML polygons from one joint file into individual files. After running the setup code, the "HSM_Creation" code can be used to create a new site model or version. <br> <br>
*Set up steps:*
- Create local fork of the repo. (Fork should contain project; code "1"; folders: Data layers, HSM code, Output, Reference files").
- Determine two-letter site code and section names to be used for whole site of intereest and subregions within the area.
- Identify the appropriate StateGrid and add the required pico-grid folder to the "Reference files/Grids" folder.
- Create or locate the necessary KML files (refer to "Site_Section_KML_creation_SOP" found in the Reference documentation for file creation). If the KML files are already separated, coppy the files to the "Reference files/KML" folder. If the KML file needs to be separated, copy the [SiteCode_version]_all file to the "Reference files/KML/PreProcessing" folder.
- Determine data layers to include in model. *[need to update: Gather data as needed and add to the "Data layers" folder.]*
- Update the "Setup_data" file as needed for the model and based on the data being used. (Refer to the *Set up data* section for more guidance.)

### Set up data
A base "Setup_data" file is located within the "Reference files" folder. This file operates as both a control for consistency among models (i.e., naming conventions) and as a tool to track model parameters in individual models. Data should only be ADDED to this file, not removed, to maintain consistency among versions. However, data can be updated to include or exclude data from models. Updated versions of the file will be added to the proper Site code_version folder for documentation to track parameters and scoring used within specific models. Check the base file for data needed, add missing data, and update existing data to include or exclude where appropriate. <br>
*Current data types included/accounted for:*
- Long_Names: Long names for sites, parameters, layer types, and data types. Requires a Yes/No desgnation for inclusion or exclusion in models.
- Section_Order: Prioirty order for sections within sites. Used to assign section designations in the case of overlapping spatial polygons. Data is filtered based on site code.
- Parameter_Order: Prioirty order for data layers used within the model. Used to assign parameter order for model weighting. Parameters to included should be assigned a priority number and data to exclude should be prioritized as "NA".
- Parameter_Scoring: Scoring values for data layers used within the model. Used to assign parameter score based on parameter value. Data layer used should be marked as "Y" under "Is_Used" or "N" if not being used in the model.

### Data layers
Folder for storing all data layers used within the model. Due to file size, data should be stored locally. A text file is included to note data sources. Names or values listed below are currently accounted for in the code and set up file. Anything not listed has not been used in models created by the code owner(s). <br>
*Current data layers*: <br>
- Oysters in Florida (current) - Live, Mostly Live, Restored, 50/50, Mostly Dead, NA

## Model set up
Progress through the numbered code files to set up the file organization and initial parameter information, establish habitat suitability scoring curves, and assemble the model data. Sections of code and their use are outlined below.
### 1_SetUp_Folders
Location: main; Project: HSM_and_mapping
- "Folder set up" creates all required folders.
- "File separation" compiles KML files from shared files located in the "Reference files" folder or creates individual files from a combined file.
- "Parameter assignment" provides a summary of the data to be incorporated into the model and is used in building the model.

### Water quality 
Location: Water quality; Project: Water quality
- Refer to "Water Quality Data SOP" located in the "Reference files" folder for steps on water quality data retrieval, data cleaning, and data combination. Assocaiated code can be found in Water quality/Code.
- Once data points have been gathered and compiled, water quality data can be interpolated to cover the area of interest as determined by the KML file. Interpolation methods include nearest neighbor, thin plate spline, inverse distance weighted, and ordinary kriging. An additional option of an ensemble model using a combination of models is provided.
- Refer to "Interpolation Methods" located in the "Reference files" folder for additioanl information on interpolation methods. Interpolation model output can be used for scoring habitat suitability. 

### 2_Developing_HSM_curves
Location: HSM code; Project: HSM_and_mapping
- Refer to "HSI_curve_descriptions" located in the "Referene files" folder for information on the types of habitat suitabiltiy curves.
- Determine initial breakpoints or category names for all parameters being added to the model to act as a starting point to establish curves.
- Use "2_Devloping_HSM_curves" to establish suitability curves for all parameters to be used in the model.
- Final breakpoints and layer names will be added to the "model_setup" summary file in the Site_version Data folder, and scores for values and figures of scores will be saved in the Data/HSI curves folder for use when building the model. 

### 3_HSM_Creation  
