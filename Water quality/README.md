# Water Quality Data Processing
Code for cleaning, compiling, and summarizing water quality data from multiple sources. 
Code for interpolation of data to designated (KML) areas.

## Water qualty data
USEAGE:<br>
Branch repro or set up R Project folder on local instance for Water-Quality-Processing work.<br>
Within the R Project folder, confirm the appropriate folder schema exists with "Code", "Data", and "Maps" folders. 
Within the "Data" folder confirm existence of folders for "Compiled-data", "Raw-cleaned", "Raw-data", and "Reference-data" subfolders. *To limit project size, data files will not be synced.*<br> <br>
SCHEMA: <br>
\local\Water-quality\<br>
<br>
_Nested within the Water quality folder:_ 
*   *\Code :   Location for all code files. Only base files and files designated with "keep" will be shared. All others will be stored locally. (Refer to the SOP for additional information.) <br>
*   *\Data :   Location for all data files with nested folders based on data types <br>
    *   *\Data\Raw-data :   Location for all portal and atlas raw data files <br>
    *   *\Data\Raw-cleaned:   Location for all cleaned data files (proper units, outliers removed, etc.)<br>
    *   *\Data\Compiled-data:   Location for summary data outputs (i.e. daily, monthly, etc.) <br>
    *   *\Data\Reference-data:   Location for data used to select or limit water quality data (i.e. daily, monthly, etc.) <br>
*   *\Maps :   Location for output of map files <br>
    *   *\Maps\Station_selection:   Location for output of WQ station map showing possible stations and selected stations<br>

## Interpolation
REQUIRED ITEMS:<br>
Interpolation methods require:
* an existing KML area file located in the "Site_code+VersionID/Data/Layers/KML" folder
* Presence of FL_outlines layer in the "Data layers" folder
* Water quality data in the Compiled-data folder (*updates to allow for Final-data need to be added*)
