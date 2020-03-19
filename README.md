# RVCAT
R project that contains a suite of data and R programs for summarizing and presenting RVCAT data for Lake Superior
<br>
<br>
[<img src="ns_annual_biomass_ciscobloater_lwf.png?raw=true"/>](ns_annual_biomass_ciscobloater_lwf.png) 
<br>
<br>
## Directories
/Data
  1. RVCAT.csv, Contains all the fish catch and effort data. File is created as spool out of RVCAT system.
  2. Site_Names.xlsx, Contains site names, mid-point latitude and longitude, and management unit information
  3. Species_Taxonomy.xlsx, Fish species taxonomy
  4. Table_of_Contents.xlsx, Information on the RVCAT.R code that summarizes, analyzes, and creates graphical products. Likely not up to date, but it's better than nothing.
  5. trawl_constants_by_vessel.xlsx, Contains equations and the constants used to calculate area swept for different vessels
  6. LENGTHS_RVCAT.csv, Fish length data. File is created as spool out of RVCAT system.
  7. LS_GLERL_Ice.csv, Downloaded from https://www.glerl.noaa.gov/data/ice/#historical
  8. LS_GLERL_WTemp, Downloaded from https://coastwatch.glerl.noaa.gov/statistic/statistic.html
  9. LSBS_BTdata.xlsx, All of the Lake Superior Biological Station's electronic water profile data. The modern bathythermograph data. Contains records from 1991-present. File is from an Excel file that is created by cutting and pasting the bathythermograph data from individual BT cast files to the Excel file.
  10. LSBS_Stomach_Contents.xlsx, Lake Superior Biological Station fish stomach contents, 2012-present 

/Data/shapefiles - Shape files of Lake Superior for constructing maps

/Plots and Tables -- separate folders by discipline where the created tables and plots are stored
  1. /Ice_Temp
  2. /Lengths
  3. /RVCAT
  4. /Stomachs

/R Markdown
1. wordstyles.docx
2. RVCAT_report.docx
3. RVCAT_report.Rmd
4. RVCAT_species_summaries.docx
5. RVCAT_species_summaries.Rmd
6. RVCAT_station_descriptions.docx
7. RVCAT_station_descriptions.Rmd

/R Scripts
1. Stomachs.R
2. BTdata.R
3. FishLengths.R
4. Ice_Temps.R
5. RVCAT.R
