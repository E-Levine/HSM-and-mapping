# Water-Quality-Processing
Code for compiling, cleaning, and summarizing water quality data from multiple sources. 

USEAGE:<br>
Set up R Project folder on local instance for Water-Quality-Processing work.<br>
Within the R Project folder, confirm the appropriate folder schema exists with "Code", "Data", and "Maps" folders. 
Within the "Data" folder confirm existence of folders for "Compiled-data", "Raw-cleaned", "Raw-data", and "Reference-data" subfolders. *To limit project size, data files will not be synced.*<br> <br>
SCHEMA: <br>
\local\Water-quality\<br>
<br>
_Nested within the Water quality folder:_ 
*   *\Code :   Location for all code files. Only base files and files designated with "keep" will be shared. All others will be stored locally. (Refer to the SOP for additional information.) <br>
*   *\Data :   Location for all data files with nested folders based on data types <br>
    *   *\Data\Raw_data :   Location for all portal and atlas raw data files <br>
    *   *\Data\Raw_cleaned:   Location for all cleaned data files (proper units, outliers removed, etc.)<br>
    *   *\Data\Compiled_data:   Location for summary data outputs (i.e. daily, monthly, etc.) <br>
    *   *\Data\Reference_data:   Location for data used to select or limit water quality data (i.e. daily, monthly, etc.) <br>
*   *\Maps :   Location for output of map files <br>
    *   *\Maps\Station_selection:   Location for output of WQ station map showing possible stations and selected stations<br> 
