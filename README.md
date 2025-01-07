# HSM-and-mapping
Project for creating and updating habitat suitability models based on StateGrid pico system. *Project is under development.*
Models are created according to the StateGrid(s) in which the desired area is location and within the predefined area established. Please refer to "Reference files" to determine proper StateGrid and how to set area boundaries. 
<br>
## Project set up
All files other than R code and R project files will be saved locally due to the typically large file size associated with the project. To use the project, create a fork and establish the R Project within the main local directory before running the "1_SetUp_Folders" R code. The site's two-letter code and model version will need to be determined prior to running the setup code. Running the setup code will create all necessary folders and provides code for separating KML polygons from one joint file into indiviudal files. After running the setup code, the "HSM_Creation" code can be used to create a new site model or version. <br> <br>
*Set up steps:*
- Create local fork of the repo. (Fork should contain project; code "1"; folders: Data, HSM code, Output, Reference files").
- Determine two-letter site code and section names.
- Create or locate the necessary KML files (refer to "Site_Section_KML_creation_SOP" found in the Reference documentation for file creation).
- Run the first chunk from "1_SetUp_Folders": "Folder set up" section.
- Copy the KML files into the appropriate "Reference files/KML/Site code_version" folder.
- Run the second chunk from "1_SetUp_Folder": "File separation" section. Skip this step if files are already separated.
- Proceed to the "HSM_Creation" R code file.  
