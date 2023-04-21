
##library
library(tidyverse)
library(doBy)
library(readxl)
library(ggplot2)
library(plotrix)
library(psych)
library(rlang)
library(purrr)
library(forcats)
library(viridis)
library(reshape)
library(rgdal)
library(xlsx)
library(lubridate)
library(gganimate)
library(magick)
library(grid)
library(ggjoy)
library(ggridges)
library(here)
library(scales)


options(max.print=999999)


##load the raw RVCAT data file
##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

raw.data<-read.csv(here('Data','RVCAT.csv'))

raw.data$SPECIES<-as.factor(raw.data$SPECIES) 

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data <- raw.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Date = dmy(paste(Day, Month, YEAR, sep = "-"))) %>%
  select(-c('OP_DATE')) %>%
  renameCol('Date', 'OP_DATE')



###Calculate mid lat and long for each trawl

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

raw.data$YearClass<-raw.data$YEAR-1

##add country based on states
raw.data <- raw.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'E'  ~ "Canada",
    STATE == 'W'  ~ "Canada"))


##Select records and fields of interest
data1 <- raw.data %>%
  subset(STATE == 'MI' & YEAR == max(YEAR)) %>%
  select(1, 3, 4, 9, 10, 28, 29, 31, 33, 34) 

data.sum.cnt <- data1 %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 25 ~ "Bottom trawl", 
    TR_DESIGN == 28  ~ "Midwater trawl")) %>%
  group_by(SPECIES, TARGET, Gear) %>%
  summarize(n = sum(NUM))

data.sum.cnt <- data1 %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 25 ~ "Bottom trawl", 
    TR_DESIGN == 28  ~ "Midwater trawl")) %>%
  mutate(survey = case_when(
    TARGET == 2 ~ "Nearshore MI waters of Lake Superior", 
    TARGET == 117 ~ "Near and offshore MI waters of Lake Superior", 
    TARGET == 118 ~ "Offshore MI waters of Lake Superior")) %>%
  unite(Cruise, TARGET, Gear, sep = '-', remove = FALSE) %>%
  group_by(Cruise, SPECIES) %>%
  summarize(n = sum(NUM))

###########################
##load Fish Lengths file into R
raw.lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.lengths<-subset(raw.lengths, EXP_N>0)
raw.lengths$SPECIES<-as.factor(raw.lengths$SPECIES) 

##Join SPECIES names & Effort data
data2 <- raw.lengths %>%
  left_join(sci.names) %>%
  right_join(data1) %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 25 ~ "Bottom trawl", 
    TR_DESIGN == 28  ~ "Midwater trawl")) %>%
  mutate(survey = case_when(
    TARGET == 2 ~ "Nearshore MI waters of Lake Superior", 
    TARGET == 117 ~ "Near and offshore MI waters of Lake Superior", 
    TARGET == 118 ~ "Offshore MI waters of Lake Superior")) %>%
  unite(Cruise, TARGET, Gear, sep = '-', remove = FALSE)

##summarize records
surveys <- data2 %>%
  distinct(Cruise, .keep_all = TRUE) %>%
  select(9, 10, 12, 17, 18)

survey.dates <- data2 %>%
  group_by(Cruise) %>% 
  summarize(minDate = min(OP_DATE), 
          maxDate = max(OP_DATE)) %>%
  left_join(surveys)

data.sum <- data2 %>%
  group_by(SPECIES, Cruise) %>%
  summarize(minL = min(LENGTH), 
            maxL = max(LENGTH),
            lengthfish=round(sum(EXP_N))) %>%
  ungroup() %>%
  left_join(sci.names) %>%
  left_join(data.sum.cnt) %>%
  left_join(survey.dates) %>%
  subset(SPECIES!= 0) 

Michigan <- data.sum %>%
  mutate(MinMonth = month(minDate, label = TRUE, abbr = FALSE), 
         MaxMonth = month(maxDate, label = TRUE, abbr = FALSE), 
         MinDay = day(minDate), 
         MaxDay = day(maxDate), 
         Year = year(maxDate)) %>%
  unite(StartDate, MinMonth, MinDay, sep = " ", remove = FALSE) %>%
  unite(EndDate, MaxMonth, MaxDay, sep = " ", remove = FALSE) %>%
  unite(Dates, StartDate, EndDate, sep = "-") %>%
  unite(Dates, Dates, Year, sep = ", ") %>%
  renameCol('COMMON_NAME', 'Common Name') %>%
  renameCol('SCIENTIFIC_NAME', 'Scientific Name') %>%
  renameCol('n', '# caught') %>%
  renameCol('minL', 'min(length) mm') %>%
  renameCol('maxL', 'max(length) mm') %>%
  renameCol('survey', 'Water & Location') %>%
  select(6, 7, 8, 5, 3, 4, 13, 15, 14) 



###########################################################################################################
###############
##Neuston data

##load the raw RVCAT data file
##load the species names file for when needed

codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

neuston.data<-read.csv(here('Data','2014-2022_Neuston_data.csv'))
neuston.data$SPECIES<-as.factor(neuston.data$SPECIES) 

##change date format into usable form
neuston.data$OP_DATE<-as.character(neuston.data$OP_DATE)
neuston.data$OP_DATE<-parse_date(neuston.data$OP_DATE, format='%d-%b-%y')

neuston.data <- neuston.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Year = year(OP_DATE), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  select(-c('OP_DATE')) %>%
  renameCol('Date', 'OP_DATE')

neuston.data[is.na(neuston.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

neuston.data[is.na(neuston.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

neuston.data$Mid.Lat.DD<-(neuston.data$BEG_LATITUDE_DD+neuston.data$END_LATITUDE_DD)/2
neuston.data$Mid.Long.DD<-(neuston.data$BEG_LONGITUDE_DD+neuston.data$END_LONGITUDE_DD)/2

#####################################
neuston.effort <- neuston.data %>%
  select(1, 3, 5, 7, 33, 32, 34, 35) %>%
  mutate(Gear = 'Surface trawl') %>% 
  mutate(survey = case_when(
    TARGET == 2 ~ "Nearshore MI waters of Lake Superior", 
    TARGET == 117 ~ "Near and offshore MI waters of Lake Superior", 
    TARGET == 118 ~ "Offshore MI waters of Lake Superior")) %>%
  unite(Cruise, TARGET, Gear, sep = '-', remove = FALSE) %>%
  distinct(OP_ID, .keep_all = TRUE) 


neuston.sites <- neuston.effort %>%
  distinct(Year, LOCATION, .keep_all = TRUE)


###########################
##load Fish Lengths file into R
neuston.lengths<-read.csv(here('Data','2014-2022_Neuston_lengths.csv'))
neuston.lengths$SPECIES<-as.factor(neuston.lengths$SPECIES) 
neuston.lengths <- neuston.lengths %>%
  left_join(neuston.effort)

##Join SPECIES names & Effort data
neuston.length.sum <- neuston.lengths %>%
  subset(SPECIES==217 |
           SPECIES==202 |
           SPECIES==203 |
           SPECIES==204 |
           SPECIES==206) %>%
  left_join(neuston.effort) %>%
  group_by(Year, Cruise, SPECIES) %>%
  summarize(minL = min(LENGTH), 
            maxL = max(LENGTH)) %>%
  ungroup() 


####################################################################################################################NEARSHORE DATA####
##Composite the two nets into one sample
##Subset data frame to SPECIES = 217, UNIDENTIFIED Coregonus or SPECIES = 0 for no fish tows
neuston.catch <- neuston.data %>% 
  subset(SPECIES==217 |
           SPECIES==202 |
           SPECIES==203 |
           SPECIES==204 |
           SPECIES==206) %>%
  group_by(Year, OP_DATE, LOCATION, SPECIES) %>%
  summarise(fish = sum(N)) %>%
  ungroup() %>%
  left_join(neuston.sites) %>%
  subset(STATE == 'MI' & Year == max(Year)) %>%
  group_by(Year, Cruise, SPECIES) %>%
  summarise(catch = sum(fish), 
            minDate = min(OP_DATE), 
            maxDate = max(OP_DATE)) %>%
  ungroup() %>%
  left_join(neuston.length.sum) %>%
  left_join(neuston.sites) %>%
  distinct(Cruise, .keep_all = TRUE) %>%
  mutate(MinMonth = month(minDate, label = TRUE, abbr = FALSE), 
         MaxMonth = month(maxDate, label = TRUE, abbr = FALSE), 
         MinDay = day(minDate), 
         MaxDay = day(maxDate), 
         Year = year(maxDate)) %>%
  unite(StartDate, MinMonth, MinDay, sep = " ", remove = FALSE) %>%
  unite(EndDate, MaxMonth, MaxDay, sep = " ", remove = FALSE) %>%
  unite(Dates, StartDate, EndDate, sep = "-") %>%
  unite(Dates, Dates, Year, sep = ", ") %>%
  left_join(sci.names) %>%
  select(22, 23, 4, 7, 8, 16, 1, 17) %>%
  renameCol('COMMON_NAME', 'Common Name') %>%
  renameCol('SCIENTIFIC_NAME', 'Scientific Name') %>%
  renameCol('catch', '# caught') %>%
  renameCol('minL', 'min(length) mm') %>%
  renameCol('maxL', 'max(length) mm') %>%
  renameCol('survey', 'Water & Location') 

  
  
Michigan.permit <- Michigan %>%
  select(-lengthfish) %>% 
  rbind(neuston.catch)


################################################################################################################### 
###Wisconsin Permit

##load the raw RVCAT data file
##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

raw.data<-read.csv(here('Data','RVCAT.csv'))

raw.data$SPECIES<-as.factor(raw.data$SPECIES) 

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data <- raw.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Date = dmy(paste(Day, Month, YEAR, sep = "-"))) %>%
  select(-c('OP_DATE')) %>%
  renameCol('Date', 'OP_DATE')



###Calculate mid lat and long for each trawl

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

raw.data$YearClass<-raw.data$YEAR-1

##add country based on states
raw.data <- raw.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'E'  ~ "Canada",
    STATE == 'W'  ~ "Canada"))


##Select records and fields of interest
data1 <- raw.data %>%
  subset(STATE == 'WI' & YEAR == max(YEAR)) %>%
  select(1, 3, 4, 9, 10, 28, 29, 31, 33, 34) 

data.sum.cnt <- data1 %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 25 ~ "Bottom trawl", 
    TR_DESIGN == 28  ~ "Midwater trawl")) %>%
  group_by(SPECIES, TARGET, Gear) %>%
  summarize(n = sum(NUM))

data.sum.cnt <- data1 %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 25 ~ "Bottom trawl", 
    TR_DESIGN == 28  ~ "Midwater trawl")) %>%
  mutate(survey = case_when(
    TARGET == 2 ~ "Nearshore WI waters of Lake Superior", 
    TARGET == 117 ~ "Near and offshore WI waters of Lake Superior", 
    TARGET == 118 ~ "Offshore WI waters of Lake Superior")) %>%
  unite(Cruise, TARGET, Gear, sep = '-', remove = FALSE) %>%
  group_by(Cruise, SPECIES) %>%
  summarize(n = sum(NUM))

###########################
##load Fish Lengths file into R
raw.lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.lengths<-subset(raw.lengths, EXP_N>0)
raw.lengths$SPECIES<-as.factor(raw.lengths$SPECIES) 

##Join SPECIES names & Effort data
data2 <- raw.lengths %>%
  left_join(sci.names) %>%
  right_join(data1) %>%
  mutate(Gear = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 25 ~ "Bottom trawl", 
    TR_DESIGN == 28  ~ "Midwater trawl")) %>%
  mutate(survey = case_when(
    TARGET == 2 ~ "Nearshore WI waters of Lake Superior", 
    TARGET == 117 ~ "Near and offshore WI waters of Lake Superior", 
    TARGET == 118 ~ "Offshore WI waters of Lake Superior")) %>%
  unite(Cruise, TARGET, Gear, sep = '-', remove = FALSE)

##summarize records
surveys <- data2 %>%
  distinct(Cruise, .keep_all = TRUE) %>%
  select(9, 10, 12, 17, 18)

survey.dates <- data2 %>%
  group_by(Cruise) %>% 
  summarize(minDate = min(OP_DATE), 
            maxDate = max(OP_DATE)) %>%
  left_join(surveys)

data.sum <- data2 %>%
  group_by(SPECIES, Cruise) %>%
  summarize(minL = min(LENGTH), 
            maxL = max(LENGTH),
            lengthfish=round(sum(EXP_N))) %>%
  ungroup() %>%
  left_join(sci.names) %>%
  left_join(data.sum.cnt) %>%
  left_join(survey.dates) %>%
  subset(SPECIES!= 0) 

Wisconsin <- data.sum %>%
  mutate(MinMonth = month(minDate, label = TRUE, abbr = FALSE), 
         MaxMonth = month(maxDate, label = TRUE, abbr = FALSE), 
         MinDay = day(minDate), 
         MaxDay = day(maxDate), 
         Year = year(maxDate)) %>%
  unite(StartDate, MinMonth, MinDay, sep = " ", remove = FALSE) %>%
  unite(EndDate, MaxMonth, MaxDay, sep = " ", remove = FALSE) %>%
  unite(Dates, StartDate, EndDate, sep = "-") %>%
  unite(Dates, Dates, Year, sep = ", ") %>%
  renameCol('COMMON_NAME', 'Common Name') %>%
  renameCol('SCIENTIFIC_NAME', 'Scientific Name') %>%
  renameCol('n', '# caught') %>%
  renameCol('minL', 'min(length) mm') %>%
  renameCol('maxL', 'max(length) mm') %>%
  renameCol('survey', 'Water & Location') %>%
  select(6, 7, 8, 5, 3, 4, 13, 15, 14) 



###########################################################################################################
###############
##Neuston data

##load the raw RVCAT data file
##load the species names file for when needed

codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

neuston.data<-read.csv(here('Data','2014-2022_Neuston_data.csv'))
neuston.data$SPECIES<-as.factor(neuston.data$SPECIES) 

##change date format into usable form
neuston.data$OP_DATE<-as.character(neuston.data$OP_DATE)
neuston.data$OP_DATE<-parse_date(neuston.data$OP_DATE, format='%d-%b-%y')

neuston.data <- neuston.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Year = year(OP_DATE), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  select(-c('OP_DATE')) %>%
  renameCol('Date', 'OP_DATE')

neuston.data[is.na(neuston.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

neuston.data[is.na(neuston.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

neuston.data$Mid.Lat.DD<-(neuston.data$BEG_LATITUDE_DD+neuston.data$END_LATITUDE_DD)/2
neuston.data$Mid.Long.DD<-(neuston.data$BEG_LONGITUDE_DD+neuston.data$END_LONGITUDE_DD)/2

#####################################
neuston.effort <- neuston.data %>%
  select(1, 3, 5, 7, 33, 32, 34, 35) %>%
  mutate(Gear = 'Surface trawl') %>% 
  mutate(survey = case_when(
    TARGET == 2 ~ "Nearshore WI waters of Lake Superior", 
    TARGET == 117 ~ "Near and offshore WI waters of Lake Superior", 
    TARGET == 118 ~ "Offshore WI waters of Lake Superior")) %>%
  unite(Cruise, TARGET, Gear, sep = '-', remove = FALSE) %>%
  distinct(OP_ID, .keep_all = TRUE) 


neuston.sites <- neuston.effort %>%
  distinct(Year, LOCATION, .keep_all = TRUE)


###########################
##load Fish Lengths file into R
neuston.lengths<-read.csv(here('Data','2014-2022_Neuston_lengths.csv'))
neuston.lengths$SPECIES<-as.factor(neuston.lengths$SPECIES) 
neuston.lengths <- neuston.lengths %>%
  left_join(neuston.effort)

##Join SPECIES names & Effort data
neuston.length.sum <- neuston.lengths %>%
  subset(SPECIES==217 |
           SPECIES==202 |
           SPECIES==203 |
           SPECIES==204 |
           SPECIES==206) %>%
  left_join(neuston.effort) %>%
  group_by(Year, Cruise, SPECIES) %>%
  summarize(minL = min(LENGTH), 
            maxL = max(LENGTH)) %>%
  ungroup() 


####################################################################################################################NEARSHORE DATA####
##Composite the two nets into one sample
##Subset data frame to SPECIES = 217, UNIDENTIFIED Coregonus or SPECIES = 0 for no fish tows
neuston.catch <- neuston.data %>% 
  subset(SPECIES==217 |
           SPECIES==202 |
           SPECIES==203 |
           SPECIES==204 |
           SPECIES==206) %>%
  group_by(Year, OP_DATE, LOCATION, SPECIES) %>%
  summarise(fish = sum(N)) %>%
  ungroup() %>%
  left_join(neuston.sites) %>%
  subset(STATE == 'WI' & Year == max(Year)) %>%
  group_by(Year, Cruise, SPECIES) %>%
  summarise(catch = sum(fish), 
            minDate = min(OP_DATE), 
            maxDate = max(OP_DATE)) %>%
  ungroup() %>%
  left_join(neuston.length.sum) %>%
  left_join(neuston.sites) %>%
  distinct(Cruise, .keep_all = TRUE) %>%
  mutate(MinMonth = month(minDate, label = TRUE, abbr = FALSE), 
         MaxMonth = month(maxDate, label = TRUE, abbr = FALSE), 
         MinDay = day(minDate), 
         MaxDay = day(maxDate), 
         Year = year(maxDate)) %>%
  unite(StartDate, MinMonth, MinDay, sep = " ", remove = FALSE) %>%
  unite(EndDate, MaxMonth, MaxDay, sep = " ", remove = FALSE) %>%
  unite(Dates, StartDate, EndDate, sep = "-") %>%
  unite(Dates, Dates, Year, sep = ", ") %>%
  left_join(sci.names) %>%
  select(22, 23, 4, 7, 8, 16, 1, 17) %>%
  renameCol('COMMON_NAME', 'Common Name') %>%
  renameCol('SCIENTIFIC_NAME', 'Scientific Name') %>%
  renameCol('catch', '# caught') %>%
  renameCol('minL', 'min(length) mm') %>%
  renameCol('maxL', 'max(length) mm') %>%
  renameCol('survey', 'Water & Location') 
  


Wisconsin.permit <- Wisconsin %>%
  select(-lengthfish) %>% 
  rbind(neuston.catch) %>%
  mutate(County = 'Ashland, Bayfield, Douglas')



###export to Excel file
sheetname <- list('Sheet1' = Michigan.permit, 'Sheet2' = Wisconsin.permit, 'Sheet3' = Michigan, 'Sheet4' = Wisconsin) 
openxlsx::write.xlsx(sheetname, here('Plots and Tables/Collection Permits','PermitReport.xlsx'), row.names=FALSE)





##############################################################
###Compare RVCAT N to Sum of EXP_N in RVCAT_LENGTHS

##load the raw RVCAT data file
##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

raw.data<-read.csv(here('Data','RVCAT.csv'))

raw.data$SPECIES<-as.factor(raw.data$SPECIES) 

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data <- raw.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Date = dmy(paste(Day, Month, YEAR, sep = "-"))) %>%
  select(-c('OP_DATE')) 


###Calculate mid lat and long for each trawl

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

raw.data$YearClass<-raw.data$YEAR-1

##add country based on states
raw.data <- raw.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'E'  ~ "Canada",
    STATE == 'W'  ~ "Canada"))


##Strip out effort fields
rvcat.effort <- raw.data %>%
  select(1, 3, 34, 4, 6, 7, 10) %>%
  distinct(OP_ID, .keep_all = TRUE)
  

rvcat.sum.cnt <- raw.data %>%
  group_by(OP_ID, SPECIES) %>%
  summarize(rvcatfish = sum(NUM)) %>%
  ungroup()

rvcat.sum.cnt2 <- raw.data %>%
  group_by(OP_ID) %>%
  summarize(rvcatfish = sum(NUM)) %>%
  ungroup()

###########################
##load Fish Lengths file into R
raw.lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.lengths<-subset(raw.lengths, EXP_N>0)
raw.lengths$SPECIES<-as.factor(raw.lengths$SPECIES) 

##Join SPECIES names & Effort data
lengths.sum.cnt <- raw.lengths %>%
  group_by(OP_ID, SPECIES) %>%
  summarize(lengthfish=round(sum(EXP_N))) %>%
  ungroup() 

##Join SPECIES names & Effort data
lengths.sum.cnt2 <- raw.lengths %>%
  group_by(OP_ID) %>%
  summarize(lengthfish=round(sum(EXP_N))) %>%
  ungroup() 

#################
##Compare RVCAT vs. LENGTHS sum N

compare <- lengths.sum.cnt %>%
  full_join(rvcat.sum.cnt) %>%
  mutate(Match = case_when(
    rvcatfish == lengthfish  ~ "Yes", 
    rvcatfish != lengthfish  ~ "No")) %>%
  mutate(difference = abs(rvcatfish - lengthfish)) %>%
  full_join(rvcat.effort) #%>%

compare["Match"][is.na(compare["Match"])] <- "No"

compare.sum <- compare %>%
  group_by(Match) %>%
  tally() %>%
  ungroup()



compare2 <- lengths.sum.cnt2 %>%
  full_join(rvcat.sum.cnt2) %>%
  mutate(Match = case_when(
    rvcatfish == lengthfish  ~ "Yes", 
    rvcatfish != lengthfish  ~ "No")) %>%
  mutate(difference = abs(rvcatfish - lengthfish)) %>%
  full_join(rvcat.effort) 

compare2["Match"][is.na(compare2["Match"])] <- "No"


compare.sum2 <- compare2 %>%
  group_by(Match) %>%
  tally() %>%
  ungroup()


###export to Excel file
sheetname <- list('Sheet1' = compare, 'Sheet2' = compare2, 'Sheet3' = compare.sum, 'Sheet4' = compare.sum2) 
openxlsx::write.xlsx(sheetname, here('Plots and Tables/Collection Permits','RVCAT.compare.xlsx'), row.names=FALSE)



