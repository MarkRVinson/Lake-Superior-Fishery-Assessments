# RVCAT
R project that contains a suite of data and R programs for summarizing and presenting RVCAT data for Lake Superior

## Directories
/Data
RVCAT.csv
Site_Names.xlsx
Species_Taxonomy.xlsx
Table_of_Contents.xlsx
trawl_constants_by_vessel.xlsx
LENGTHS_RVCAT.csv
LS_GLERL_Ice.csv
LS_WTMP_fromTaylor.xlsx
LSBS_BTdata.xlsx
LSBS_Stomach_Contents.xlsx

/Data/shapefiles - Shape files of Lake Superior for constructing maps

/Plots and Tables -- where the created tables and plots are stored
  1. /Ice_Temp
  2. /Lengths
  3. /RVCAT
  4. /Stomachs


Data files
Data input files
RVCAT.xlsx -- Excel file containing all the fish catch and effort data. File is created as spool out of RVCAT system.
LSBS_BTdata.xlsx -- Excel file containing all the Lake Superior Biological Station's electronic water profile data. The modern bathythermograph data. Contains records from 1991-present. File is from an Excel file that is created by cutting and pasting the bathythermograph data from individual BT cast files to the Excel file.
LENGTHS_RVCAT.xlsx -- Excel file containing all fish length data. File is created as spool out of RVCAT system.
Data supporting files
Species_Taxonomy.xlsx -- Excel file containing fish taxonomic information
trawl_constants_by_vessel.xlsx -- the constant data to convert fish catch data, counts or biomass, from bottom trawls into density estimates. Number or weight of fish per hectare. Includes information for all vessels used by the Lake Superior Biological Station over time.
Scripts
RVCAT.R -- A long R script that summarizes, analyzes, and creates graphical products using the RVCAT.xlsx file
BTdata.R -- R script that summarizes the LSBS_BTdata.xlsx file
FishLengths.R -- R script that summarizes the LENGTHS_RVCAT.xlsx file

Table_of_contents. xlsx -- Information on the RVCAT.R code that summarizes, analyzes, and creates graphical products. Likely not up to data, but it's better than nothing.
