# HSM-and-mapping
Project for creating and updating habitat suitability models based on StateGrid pico system. *Project is under development.*
Models are created according to the StateGrid(s) in which the desired area is location and within the predefined area established. Please refer to "Reference files" to determine proper StateGrid and how to set area boundaries. 
<br>
## Project set up
All files other than R code and R project files will be saved locally due to the typically large file size associated with the project. To use the project, create a fork and establish the R Project within the main local directory before running the "1_SetUp_Folders" R code. The site's two-letter code and model version will need to be determined prior to running the setup code. Running the setup code will create all necessary folders and provides code for separating KML polygons from one joint file into indiviudal files. After running the setup code, the "HSM_Creation" code can be used to create a new site model or version. <br> <br>
*Set up steps:*
- Create local fork of the repo. (Fork should contain project; code "1"; folders: Data, HSM code, Output, Reference files").
- Determine two-letter site code and section names.
- Identify the approriate StateGrid and add the required pico-grid fodler to the "Reference files/Grids" folder.
- Create or locate the necessary KML files (refer to "Site_Section_KML_creation_SOP" found in the Reference documentation for file creation).
- Run the first chunk from "1_SetUp_Folders": "Folder set up" section.
- Copy the KML files into the appropriate "Reference files/KML/Site code_version" folder.
- Run the second chunk from "1_SetUp_Folder": "File separation" section. Skip this step if files are already separated.
- Determine data layers to include in model. Gather data as needed and add to the approriate "Site code_verion/Data" folder.
- Update the "Setup_data" file as need for the model and based on the data being used. (Refer to the *Set up data* section for more guidance.)
- Run the third chunk from "1_SetUp_Folder": "Parameter assignment" section. Update the code as approraite for the model.
- Proceed to the "HSM_Creation" R code file.  

### Set up data
A base "Setup_data" file is located within the "Reference files" folder. This file operates as both a control for consistency among models (i.e., naming conventions) and as a tool to track model parameters in individual models. Data should only be ADDED to this file, not removed to maintain consistency among versions. However, data can be updated to include or exclude data from models. Updated versions of the file will be added to the proper Site code_version folder for documentation to track parameters and scoring used within specific models. Check the base file for data needed, add missing data, and updated exitsing data to inlclude or exclude where appropriate. <br>
*Current data types included/accounted for:*
- Long_Names: Long names for sites, parameters, layer types, and data types. Requires a Yes/No desgnation for inclusion or exclusion in models.
- Section_Order: Prioirty order for sections within sites. Used to assign section designations in the case of overlapping spatial polygons. Data is filtered based on site code.
- Parameter_Order: Prioirty order for data layers used within the model. Used to assign parameter order for model weighting. Parameters to included should be assigned a priority number and data to exclude should be prioritized as "NA".
- Parameter_Scoring: Scoring values for data layers used within the model. Used to assign parameter score based on parameter value. Data layer used should be marked as "Y" under "Is_Used" or "N" if not being used in the model.

## Data Layers
Folder for storing all data layers used within the model. Due to file size, data should be stored locally. A text file is included to note data sources. Names or values listed below are currently accounted for in the code and set up file. Anything not listed has not been used in models created by the code owner(s). <br>
*Current data layers*: <br>
- Oysters in Florida - Live, Mostly Live, Restored, 50/50, Mostly Dead, NA
