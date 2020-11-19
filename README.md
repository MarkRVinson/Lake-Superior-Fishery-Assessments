# RVCAT
R project that contains a suite of data and R programs for summarizing and presenting RVCAT data for Lake Superior
<br>
<br>
[<img src="ns_annual_biomass_ciscobloater_lwf.png?raw=true"/>](ns_annual_biomass_ciscobloater_lwf.png) 
<br>
<br>
[<img src="Animated_ciscoe_pies.gif?raw=true"/>](Animated_ciscoe_pies.gif) 
<br>
<br>
## Directories
/Data
  1. RVCAT.csv, Contains all the fish catch and effort data. File is created as spool out of RVCAT system.
  2. Site_Names.xlsx, Contains site names, mid-point latitude and longitude, and management unit information
  3. Species_Taxonomy.xlsx, Fish species taxonomy
  4. Table_of_Contents.xlsx, Information on the RVCAT.R code that summarizes, analyzes, and creates graphical products. Likely not up to date, but it's better than nothing.
  5. LSBS_Trawl_Information_Vessel_Consants.xlsx, Contains equations and the constants used to calculate area swept for different vessels, as well as trawl and door specifications
  6. LENGTHS_RVCAT.csv, Fish length data. File is created as spool out of RVCAT system.
  7. LS_GLERL_Ice.csv, Downloaded from https://www.glerl.noaa.gov/data/ice/#historical
  8. LS_GLERL_WTemp, Downloaded from https://coastwatch.glerl.noaa.gov/statistic/statistic.html
  9. LSBS_BTdata.xlsx, All of the Lake Superior Biological Station's electronic water profile data. The modern bathythermograph data. Contains records from 1991-present. File is from an Excel file that is created by cutting and pasting the bathythermograph data from individual BT cast files to the Excel file.
  10. LSBS_Stomach_Contents.xlsx, Lake Superior Biological Station fish stomach contents, 2012-present 

/Data/shapefiles - Shape files of Lake Superior for constructing maps

/Plots and Tables -- separate folders by discipline where the created tables and plots are stored
  1. /Ice_Temp
  2. /Length Bin Densities
  3. /Lengths
  4. /RVCAT
  5. /Stomachs

/R Markdown
1. RVCAT_report.Rmd, annual report GLFC meeting
2. RVCAT_species_summaries.Rmd, standard summary of collections by species, one page per species
3. RVCAT_station_descriptions.Rmd, standard summary by survey locations, one page per location

/R Scripts
1. BTdata.R, summarizes water profile data from SeaBird vertical profiler, 1991-present
2. Density_Length_Tables, creates custom tables of densities by length bins
3. FishLengths.R, annual length frequency plots
4. Ice_Temps.R, summarizes NOAA GLERL ice and water temperature data
5. RVCAT.R, summarizes RVCAT data from 1963-present, lots of plots and tables, basis for annual report
6. Stomachs.R, summarizes our station's stomach content data, 2012-present
