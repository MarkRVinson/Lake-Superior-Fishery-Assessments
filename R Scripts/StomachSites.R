
##load data 
Prey <- read_excel(here('Data',"LSBS_Stomach_Contents.xlsx"), sheet ="RawData") 

#select needed columns
Prey1 <- select(Prey, FishRecord, Station, Date, Year, Gear, BegDepth_m, EndDepth_m, Predator, PRLength_mm, PRWeight_g, Prey1, Prey2, Prey3, Prey4, LifeStage, PreyCount, PreySize_mm, PreyWT_g)
Prey2 <- Prey1 %>%
  filter(Year=="2016") %>%
  
  
  ##load the raw RVCAT data file
  ##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
  ##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
  raw.data<-read.csv(here('Data','RVCAT.csv'))
raw.data$SPECIES<-as.factor(raw.data$SPECIES)

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

sites <- raw.data %>%
  select(LOCATION,Mid.Lat.DD,Mid.Long.DD ) %>%
  filter(Mid.Lat.DD >30) %>%
  group_by(LOCATION) %>%
  summarise(Longitude = mean(Mid.Long.DD),Latitude=mean(Mid.Lat.DD)) %>%
  renameCol('LOCATION','Station') %>%
  inner_join(Prey2)
openxlsx::write.xlsx(sites, here('Plots and Tables/Stomachs','CSMIStomachs_2016.xlsx'), row.names=FALSE)

