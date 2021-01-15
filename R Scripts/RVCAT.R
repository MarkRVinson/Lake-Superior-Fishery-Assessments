
##library
library(ggplot2)
library(rlang)
library(tidyverse)
library(doBy)
library(readxl)
library(plotrix)
library(psych)
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
library(ggforce)
library(here)
library(scales)
library(scatterpie)
library(tidyquant) ##to calculate skewness


##load the raw RVCAT data file
##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
raw.data<-read.csv(here('Data','RVCAT.csv'))
raw.data$SPECIES<-as.factor(raw.data$SPECIES)


## get rid of records with no SPECIES code
raw.data <- raw.data %>%
  drop_na(SPECIES)
  

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

##Replace zeros which are known to be wrong to na for all variables other than SPECIES, NUM, WEIGHT
raw.data$TIME <- replace(raw.data$TIME, raw.data$TIME == 0, NA)             
raw.data$TOW_TIME <- replace(raw.data$TOW_TIME, raw.data$TOW_TIME == 0, NA)             
raw.data$FISHING_DEPTH <- replace(raw.data$FISHING_DEPTH, raw.data$FISHING_DEPTH == 0, NA)
raw.data$SURF_TEMP <- replace(raw.data$SURF_TEMP, raw.data$SURF_TEMP == 0, NA)
raw.data$BEG_SURF <- replace(raw.data$BEG_SURF, raw.data$BEG_SURF == 0, NA)
raw.data$END_SURF <- replace(raw.data$END_SURF, raw.data$END_SURF == 0, NA)
raw.data$BEG_BOTTOM <- replace(raw.data$BEG_BOTTOM, raw.data$BEG_BOTTOM == 0, NA)
raw.data$END_BOTTOM <- replace(raw.data$END_BOTTOM, raw.data$END_BOTTOM == 0, NA)
raw.data$TEMP_BEG_BOTTOM <- replace(raw.data$TEMP_BEG_BOTTOM, raw.data$TEMP_BEG_BOTTOM == 0, NA)

raw.data <- raw.data %>%
  group_by(LOCATION) %>%
  mutate(BEG_LATITUDE_DD = case_when(is.na(BEG_LATITUDE_DD) ~ mean(BEG_LATITUDE_DD, na.rm=TRUE),
                                      TRUE ~ as.numeric(BEG_LATITUDE_DD))) %>%
  mutate(BEG_LONGITUDE_DD = case_when(is.na(BEG_LONGITUDE_DD) ~ mean(BEG_LONGITUDE_DD, na.rm=TRUE),
                                     TRUE ~ as.numeric(BEG_LONGITUDE_DD))) %>%
  mutate(END_LATITUDE_DD = case_when(is.na(END_LATITUDE_DD) ~ mean(END_LATITUDE_DD, na.rm=TRUE),
                                      TRUE ~ as.numeric(END_LATITUDE_DD))) %>%
  mutate(END_LONGITUDE_DD = case_when(is.na(END_LONGITUDE_DD) ~ mean(END_LONGITUDE_DD, na.rm=TRUE),
                                     TRUE ~ as.numeric(END_LONGITUDE_DD))) %>%
  ungroup()

raw.site.lat.long <- raw.data %>%
  select(LOCATION, BEG_LATITUDE_DD, END_LATITUDE_DD, BEG_LONGITUDE_DD, 
         END_LONGITUDE_DD) %>%
  group_by(LOCATION) %>%
  summarise(BEG_LATITUDE_DD = mean(BEG_LATITUDE_DD,na.rm=TRUE),
            END_LATITUDE_DD = mean(END_LATITUDE_DD,na.rm=TRUE),
            BEG_LONGITUDE_DD = mean(BEG_LONGITUDE_DD,na.rm=TRUE),
            END_LONGITUDE_DD = mean(END_LONGITUDE_DD,na.rm=TRUE)) %>%
  ungroup()

##calculate the mid-point (average) latitude and longitude for each trawl where beg and end lat/longs are known
raw.data$Mid.Lat.DD <- rowMeans(raw.data[,c("BEG_LATITUDE_DD","END_LATITUDE_DD")], na.rm=TRUE)
raw.data$Mid.Long.DD <- rowMeans(raw.data[,c("BEG_LONGITUDE_DD","END_LONGITUDE_DD")], na.rm=TRUE)

##Calculate an average Surface and bottom temp
raw.data$Surface.Temp <- rowMeans(raw.data[,c("SURF_TEMP", "BEG_SURF", "END_SURF")], na.rm = TRUE)
raw.data$Bottom.Temp <- rowMeans(raw.data[,c("BEG_BOTTOM", "END_BOTTOM", "TEMP_BEG_BOTTOM", "TEMP_END_BOTTOM")], na.rm = TRUE)

raw.data <- raw.data %>%
  mutate(Mid.Lat.DD = ifelse(is.nan(Mid.Lat.DD), NA, Mid.Lat.DD),
         Mid.Long.DD = ifelse(is.nan(Mid.Long.DD), NA, Mid.Long.DD),
         Surface.Temp = ifelse(is.nan(Surface.Temp), NA, Surface.Temp),
         Bottom.Temp = ifelse(is.nan(Bottom.Temp), NA, Bottom.Temp))

##Replace FISHING_DEPTH with END_DEPTH for bottom trawls and keep as is for mid-water trawls
raw.data <- raw.data %>%
  mutate(FISHING_DEPTH = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ END_DEPTH, 
     TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ FISHING_DEPTH))
    
##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))

###################################################################################
## to calculate NOHA and KGHA, need to subset out different vessels and trawl designs,
## as they have different conversion factors for area sampled
## TR_DESIGNS == 4 - 39 foot bottom trawl, chain foot gear
## TR_DESIGNS == 5 - 32 foot bottom trawl
## TR_DESIGNS == 21 - 47 foot mid-water trawl
## TR_DESIGNS == 22 - 54 foot mid-water trawl
## TR_DESIGNS == 25 - 39 foot bottom trawl, roller foot gear
## TR_DESIGNS == 26 - 18 foot bottom trawl, chain foot gear, Chequamegon Bay trawl
## TR_DESIGNS == 27 - 3 meter naturualist trawl
## TR_DESIGNS == 28 - 50 foot mid-water trawl
## TR_DESIGNS == 41 - 100 foot bag seine
## TR_DESIGNS == 44 - 10 foot beam trawl, bottom trawl
## TR_DESIGNS == 45 - 50 foot bag seine

## for KGHA, weight is converted from grams to kg and then KG per ha are calculated

##r/v siscowet from start to 1976 **note, conversion factor only verified from 1973-1976, estimates for years prior
##r/v siscowet prior to 1977
siscowet_76_tr4 <- raw.data %>%
  subset(VESSEL==1 & YEAR<1977 & TR_DESIGN == 4 | VESSEL==1 & YEAR<1977 & TR_DESIGN == 25) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.01, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

siscowet_76_tr5 <- raw.data %>%
  subset(VESSEL==1 & YEAR<1977 & TR_DESIGN == 5) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.01,
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

##r/v siscowet after 1976
siscowet_77_tr4 <- raw.data %>%
  subset(VESSEL==1 & YEAR>1976 & TR_DESIGN == 4 | VESSEL==1 & YEAR>1976 & TR_DESIGN == 25) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45,
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

siscowet_77_tr5 <- raw.data %>%
  subset(VESSEL==1 & YEAR>1976 & TR_DESIGN == 5) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

siscowet_tr21 <- raw.data %>%
  subset(VESSEL==1 & TR_DESIGN == 21) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

siscowet_tr22 <- raw.data %>%
  subset(VESSEL==1 & TR_DESIGN == 22) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

siscowet_tr27 <- raw.data %>%
  subset(VESSEL==1 & TR_DESIGN == 27) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)


##r/v grayling 1973-1999 (all years)
grayling <- raw.data %>%
  subset(VESSEL==11) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.05, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)


##r/v coaster, USGS, 1988-1992 and 1994-present?; 2004  and 1993 serials>120
coaster_88a <- raw.data %>%
  subset(VESSEL==99 & YEAR==1993 & SERIAL>120) %>%
  mutate(HA_SWEPT = (TOW_TIME/60)*0.6097, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

coaster_88 <- raw.data %>%
  subset(VESSEL==99) %>%
  subset(YEAR != 1993) %>%
  bind_rows(coaster_88a) %>%
  mutate(HA_SWEPT = (TOW_TIME/60)*0.785, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

##r/v coaster, USGS, 1993 serials 1-120
coaster_93 <- raw.data %>% 
subset(VESSEL==99 & YEAR==1993 & SERIAL<121) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*0.6097, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)


##r/v coaster/shoveler USFWS 2009 (calling it shoveler to differentiate from coaster above)
shoveler <- raw.data %>% 
  subset(VESSEL==95) %>% 
  mutate(HA_SWEPT = ((DISTANCE*5280)*14.76)/107639.1, 
         NOHA = NUM/HA_SWEPT, 
         KGHA = (WT*.001)/HA_SWEPT)
         

##r/v whitefish
whitefish <- raw.data %>% 
  subset(VESSEL==50) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*1.174, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)


##jonboat
jonboat_tr26 <- raw.data %>% 
  subset(VESSEL==92 & TR_DESIGN == 26) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*0.6568, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

jonboat_seine <- raw.data %>% 
  subset(VESSEL==92 & TR_DESIGN == 41 | VESSEL==92 & TR_DESIGN == 45) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*0.6568, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

##r/v kaho **note, don't have a conversion factor for this ship yet, using the conversion factor for the siscowet for now
kaho <- raw.data %>% 
  subset(VESSEL==4) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)


##r/v kiyi - 39 foot bottom trawls
kiyi_tr4_25 <- raw.data %>% 
  subset(VESSEL==25 & TR_DESIGN == 4 |  VESSEL==25 & TR_DESIGN == 25) %>% 
  mutate(HA_SWEPT = ((DISTANCE*5280)*25.49)/107639.1, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)
         
##r/v kiyi - beam trawl
kiyi_tr44 <- raw.data %>% 
  subset(VESSEL==25 & TR_DESIGN == 44) %>% 
  mutate(HA_SWEPT = ((DISTANCE*5280)*10)/107639.1, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

##r/v kiyi - midwater trawls
kiyi_tr21_28 <- raw.data %>% 
  subset(VESSEL==25 & TR_DESIGN == 28 | VESSEL==25 & TR_DESIGN == 21) %>% 
  mutate(HA_SWEPT = ((DISTANCE*5280)*32.8)/107639.1, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

##Daphnia **just a guess on conversion factor based on coaster constant
daphnia <- raw.data %>% 
  subset(VESSEL==9) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*0.7, 
         NOHA = NUM/HA_SWEPT,
         KGHA = (WT*.001)/HA_SWEPT)

########################################################################################
##bind all data frames together now that they have HA_SWEPT, NOHA and KGHA

all.data <- bind_rows(siscowet_76_tr4,
            siscowet_76_tr5,
            siscowet_77_tr4,
            siscowet_77_tr5,
            siscowet_tr21,
            siscowet_tr22,
            siscowet_tr27,
            grayling,
            coaster_88,
            coaster_93,
            shoveler,
            whitefish,
            jonboat_tr26,
            jonboat_seine,
            kaho,
            kiyi_tr4_25,
            kiyi_tr44,
            kiyi_tr21_28,
            daphnia)

##check that the number of rows in all.data matches the number in the raw.data input to make sure no data got lost along the way
##if data were lost, were any additional vessels used that are not listed above? any vessel codes changed or recorded incorrectly?

####################################################################################

##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=20, family='serif'), 
                 axis.title=element_text(size=20, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=18, family='serif'),
                 plot.caption=element_text(size=16, family='serif'), 
                 legend.position=c(0.1,0.7),
                 legend.title=element_text(size=20, family='serif'),
                 legend.text=element_text(size=16, family='serif'))

plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=16, family='serif'),
                  axis.title=element_text(size=16, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=16, family='serif'),
                  legend.title=element_text(size=18, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=20, family='serif'),
                  plot.subtitle=element_text(size=16, family='serif'),
                  plot.caption=element_text(size=14, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=16, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0'

####################################################################################################################NEARSHORE DATA####
###Miscellaneous small data requests
##sl <- subset(all.data, SPECIES == 5)

##pig<-all.data %>%
##  distinct(all.data, OP_ID)


##ggplot(sl, aes(x=YEAR, y=NUM))+
##  geom_bar(stat='identity', fill='grey75', color='black')+
##  scale_x_continuous(breaks = pretty_breaks()) +
##  scale_y_continuous(expand=c(0,0)) +
##  plot_theme+
##  labs(x='Year', y='Count',caption=ann_data_access,
##       title='Lake Superior Sea Lamprey Catches',
##       subtitle='USGS bottom trawl assessments') 

##ggsave(here('Plots and Tables/RVCAT','PigPlot.png'), dpi = 300, width = 40, height = 20, units = "cm") 


###########################################################################################
###########################################################################################
##DATA EXPORT NEARSHORE AND OFFSHORE DATA ALL SPECIES, YEARS, SITES
###########################################################################################
##############################################################################
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

export.ns.os <- all.data %>%
  select(OP_ID,OP_DATE,YEAR,TIME,TARGET,TR_DESIGN,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,NUM,NOHA,KGHA) %>%
  subset(YEAR >= 1974) %>% 
  subset(TARGET==2 | TARGET==118 | TARGET==117 & YEAR>2010 & END_DEPTH>84) %>% 
  subset(TR_DESIGN==25 | TR_DESIGN==4) %>%
#  subset(M_UNIT == "WI2") %>% 
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore"))) %>%
  left_join(sci.names)

export.Catch.NoZeros <- export.ns.os %>%
  select(OP_ID,survey,OP_DATE,TIME,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM,NOHA,KGHA) %>%
  mutate(TRAWL_TYPE = "Bottom trawl") 

##If weights were NOT taken replace KGHA with 'na'
export.Catch.NoZeros$KGHA[export.Catch.NoZeros$KGHA == 0 & export.Catch.NoZeros$NUM >0] <- NA

##Pull out all individual trawls
export.trawls<-export.ns.os  %>%
  select(OP_ID,OP_DATE,TIME,survey, YEAR,LOCATION,TOW_TIME,DISTANCE,HA_SWEPT,
         BEG_DEPTH,END_DEPTH,Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp) %>%
  distinct(OP_ID, .keep_all = TRUE)

###Add zeros to NUM for fish species that were not caught
export.trawls.zeros.allspecies <- export.ns.os  %>%
  select(OP_ID,SPECIES, NUM, NOHA, KGHA) 

export.trawls.zeros.allspecies <-complete(export.trawls.zeros.allspecies, OP_ID, SPECIES, 
                                          fill=list(NUM=0, NOHA=0, KGHA=0)) 

export.tfish.trawls <- export.trawls.zeros.allspecies %>%
  group_by(OP_ID) %>%
  summarize(Tfish = sum(NUM))

##get rid of trawls where SPECIES == 0 (NO FISH CAUGHT) and where fish were actually caught
export.trawls.zeros.allspecies <- export.trawls.zeros.allspecies %>%
  left_join(export.tfish.trawls) %>%
#  subset(Tfish > 0 & SPECIES != 0 | Tfish == 0 & SPECIES == 0)  %>%
  left_join(export.trawls) %>% 
  left_join(sci.names) 

export.Catch.Zeros <- export.trawls.zeros.allspecies %>%
  select(OP_ID,survey,OP_DATE,TIME,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM,NOHA,KGHA) %>%
  mutate(TRAWL_TYPE = "Bottom trawl")

##If weights were NOT taken replace KGHA with 'na'
export.Catch.Zeros$KGHA[export.Catch.Zeros$KGHA == 0 & export.Catch.Zeros$NUM >0] <- NA

##Get rid of records where SPECIES == 0 and fish were caught in that trawl
export.Catch.Zeros <- export.Catch.Zeros %>%
  left_join(export.tfish.trawls) %>%
  subset(SPECIES == 0 & Tfish ==0 | Tfish >0 & SPECIES !=0) %>%
  select(OP_ID,survey,OP_DATE,TIME,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM,NOHA,KGHA,TRAWL_TYPE)

##Calculate total number of species caught that year
export.annual.total.fish <- export.trawls.zeros.allspecies %>% 
  subset(NUM>0) %>%
  group_by(YEAR, survey) %>%
  distinct(SPECIES) %>%
  summarise(fishes.total = n())

##Sum KGHA and NOHA across all species to get total biomass and total number per ha
export.trawls.sum<- export.ns.os %>%
  select(OP_ID,NUM,NOHA,KGHA) %>%
  group_by(OP_ID) %>%
  summarise(KGHA = sum(KGHA), NUM=sum(NUM), NOHA = SUM(NOHA), Fishes = n()) %>%
  left_join(export.trawls) %>% 
  mutate(Fishes = replace(Fishes, NOHA == 0, 0))

##calculate summary stats by year
export.annual.sum <- export.trawls.sum %>% 
  group_by(YEAR, survey) %>% 
  summarise(locations=n(), 
            mean.dist.km=mean(DISTANCE)*1.60934, median.dist.km = median(DISTANCE*1.60934), 
            min.dist.km=min(DISTANCE)*1.60934, max.dist.km=max(DISTANCE)*1.60934,
            mean.towtime=mean(TOW_TIME),  median.towtime=median(TOW_TIME), 
            min.towtime=min(TOW_TIME), max.towtime=max(TOW_TIME),
            median.begdepth=median(BEG_DEPTH), min.begdepth=min(BEG_DEPTH), max.begdepth=max(BEG_DEPTH),
            median.enddepth=median(END_DEPTH), min.enddepth=min(END_DEPTH), max.enddepth=max(END_DEPTH),
            mean.surface.temp=mean(Surface.Temp), min.surface.temp=min(Surface.Temp), max.surface.temp=max(Surface.Temp),
            mean.bottom.temp=mean(Bottom.Temp), min.bottom.temp=min(Bottom.Temp), max.bottom.temp=max(Bottom.Temp),
            mean.kg=mean(KGHA), median.kg = median(KGHA), sd.kg = sd(KGHA), 
            std.error.kg = std.error(KGHA), skewness.kg=skewness(KGHA),
            min.kg=min(KGHA), max.kg = max(KGHA),
            mean.num=mean(NOHA), median.num = median(NOHA), sd.num = sd(NOHA), 
            std.error.num = std.error(NOHA), skewness.num=skewness(NOHA),
            sum.num=sum(NUM),
            fishes.mean = mean(Fishes), fishes.median = median(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) %>%
  left_join(export.annual.total.fish) %>%
  ungroup()

export.annual.zerofish <- export.trawls.sum %>%
  subset(NUM == 0) %>%
  group_by(YEAR, survey) %>% 
  summarise(sites.zerofish=n()) %>%
  ungroup() %>%
  select(YEAR, survey, sites.zerofish) 

export.annual.sum <- export.annual.sum %>%
  left_join(export.annual.zerofish) 

export.annual.sum$sites.zerofish[is.na(export.annual.sum$sites.zerofish)] <- 0 

##calculate annual nearshore summary stats by station, year, and species
export.annual.sum.by.species <- export.trawls.zeros.allspecies %>%
  group_by(YEAR, SPECIES, survey) %>%
  summarise(mean.kg = mean(KGHA), median.kg = median(KGHA), std.error.kg = std.error(KGHA), 
            mean.num = mean(NOHA), median.num = median(NOHA), std.error.num = std.error(NOHA)) 

##Add fish common names to file
sci.names$SPECIES<-as.factor(sci.names$SPECIES)
export.annual.sum.by.species <- export.annual.sum.by.species %>%
  left_join(sci.names)

##Nearshore Biomass Table for annual report
export.annual.ns.species.sum <- export.trawls.zeros.allspecies %>%
  select(OP_ID,YEAR,survey,SPECIES,COMMON_NAME, KGHA) %>%
  subset(survey == 'nearshore') %>%
  subset(SPECIES == 109 |
           SPECIES == 202 |
           SPECIES == 203 |
           SPECIES == 204 |
           SPECIES == 307 |
           SPECIES == 317 |
           SPECIES == 127 |
           SPECIES == 902 |
           SPECIES == 903 |
           SPECIES == 904) %>%
  group_by(YEAR,COMMON_NAME) %>% 
  summarise(mean.kg=mean(KGHA)) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = mean.kg, values_fill = 0) %>%
  ungroup()

export.annual.ns.table <- export.annual.sum %>%
  subset(survey == 'nearshore') %>%
  left_join(export.annual.ns.species.sum) %>%
select(YEAR, locations, sites.zerofish, mean.kg, median.kg, fishes.mean, fishes.total,
         "Bloater", "Cisco", "Lake Whitefish", "Rainbow Smelt", 
         "hatchery Lake Trout", "lean Lake Trout", "Burbot",
         "Slimy Sculpin", "Spoonhead Sculpin", "Deepwater Sculpin") %>%
  ungroup() %>%
  mutate(all_sculpins = rowSums(.[15:17])) %>%
  mutate(misc_species = mean.kg - rowSums(.[8:17])) 

##Offshore annual Biomass summary Table for export
export.annual.os.species.sum <- export.trawls.zeros.allspecies %>%
  select(OP_ID,YEAR,survey,SPECIES,COMMON_NAME, KGHA) %>%
  subset(survey == 'offshore') %>%
  subset(SPECIES == 206 |
           SPECIES == 308 |
           SPECIES == 904) %>%
  group_by(YEAR,COMMON_NAME) %>% 
  summarise(mean.kg=mean(KGHA)) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = mean.kg) 

export.annual.os.table <- export.annual.sum %>%
  subset(survey == 'offshore') %>%
  left_join(export.annual.os.species.sum) %>%
  select(YEAR, locations, sites.zerofish, mean.kg, fishes.mean, fishes.total,
         "Kiyi", "siscowet Lake Trout", "Deepwater Sculpin")


##list of species caught and the number of individuals for a particular year, usually max(YEAR)
export.annual.catch <- export.trawls.zeros.allspecies %>%
  select(OP_ID, YEAR, survey, SPECIES, NUM) %>%
  subset(YEAR == max(YEAR) & NUM >0) %>%
  group_by(SPECIES,survey) %>% 
  summarise(catch = sum(NUM)) %>% 
  pivot_wider(names_from = survey, values_from = catch, values_fill = 0) %>%
  ungroup() %>%
  left_join(sci.names) %>%
  select(COMMON_NAME,SCIENTIFIC_NAME, nearshore, offshore) 
  
  
##### Age-1 abundance table ################################################################
############################################################################################
##Lengths file to evaluate Age-1 densities##################################################
#sci.names$SPECIES<-as.numeric(sci.names$SPECIES)

lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) 

lengths$SPECIES<-as.factor(lengths$SPECIES)

export.age1.ns.os <- export.trawls %>%
  full_join(lengths) %>% 
  subset(SPECIES == '109' & LENGTH <101 & survey == "nearshore" |
           SPECIES == '202' & LENGTH <141 & survey == "nearshore" |
           SPECIES == '204' & LENGTH <131 & survey == "nearshore" |
           SPECIES == '203' & LENGTH <161 & survey == "nearshore" |
           SPECIES == '317' & LENGTH <226 & survey == "nearshore" |
           SPECIES == '308' & LENGTH <226 & survey == "offshore" |
           SPECIES == '206' & LENGTH <131 & survey == "offshore") %>%
  select(OP_ID,SPECIES,EXP_N) %>%
  group_by(OP_ID,SPECIES) %>%
  summarise(NUM = sum(EXP_N)) %>%
  ungroup()

export.age1.zeros <-complete(export.age1.ns.os, OP_ID, SPECIES, 
                                          fill=list(NUM=0))

export.age1.complete <- export.trawls %>%
  left_join(export.age1.zeros) %>%
  mutate(NOHA=NUM/HA_SWEPT) 

export.age1.table <- export.age1.complete %>% 
  group_by(YEAR, survey, SPECIES) %>% 
  summarise(age1.mean = mean(NOHA)) %>%
  ungroup() %>%
  select(YEAR, survey, SPECIES, age1.mean) %>%
  subset(!is.na(SPECIES)) %>%
  subset(SPECIES == '109' & survey == "nearshore" |
           SPECIES == '202' & survey == "nearshore" |
           SPECIES == '204' & survey == "nearshore" |
           SPECIES == '203' & survey == "nearshore" |
           SPECIES == '317' & survey == "nearshore" |
           SPECIES == '308' & survey == "offshore" | 
           SPECIES == '206' & survey == "offshore") %>%
  select(YEAR, SPECIES, age1.mean) %>%
  left_join(sci.names) %>%
  mutate('Year class' = YEAR-1) %>%
  select(YEAR, 'Year class', COMMON_NAME, age1.mean) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = age1.mean) %>%
  round(1) 
  

##Export to Excel File
library(openxlsx)
metadata<-read.xlsx(here('Data','ns_os_all_MetaData.xlsx'))
list.sheets<-list('Catch_NoZeros'=export.Catch.NoZeros, 
                  'Catch_Zeros'= export.Catch.Zeros,
                  'AnnualSummary'=export.annual.sum,
                  'NS_table' = export.annual.ns.table, 
                  'OS_table' = export.annual.os.table,
                  'Age1_Fish' = export.age1.table, 
                  'Catch_table' = export.annual.catch,
                  'MetaData'= metadata)

openxlsx::write.xlsx(list.sheets, here('Plots and Tables/RVCAT','export_ns_os_all.xlsx'))

##############################################################################################
##############################################################################################
##############################################################################################

##pull out nearshore, TARGET = 2 data for a normal full survey year

ns<-subset(all.data, TARGET==2 & YEAR >1977)

##ns<-subset(all.data, TARGET==2 & YEAR >1973 &  M_UNIT == "WI2")


###If you want to compare data only from a partcular Managemen Unit or State do it now
## ns<-filter(all.data, TARGET == 2 & YEAR>1977 & STATE== 'MN')
##ns<-subset(all.data, TARGET == 2 & YEAR>1977 & M_UNIT == "WI2")

##Pull out all individual nearshore trawls
ns.trawls<-ns %>%
  select(OP_ID,SERIAL,TARGET,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,
         TR_DESIGN,DISTANCE,TOW_TIME,HA_SWEPT) %>%
  distinct(OP_ID, .keep_all = TRUE)


###Add zeros to NUM for fish species that were not caught
ns.trawls.all.species <- ns %>%
  select(OP_ID,SPECIES,NUM, NOHA, KGHA) 

ns.trawls.all.species <-complete(ns.trawls.all.species, OP_ID, SPECIES, 
                                 fill=list(NUM=0, NOHA=0, KGHA=0)) %>%
  left_join(ns.trawls) 

##Calculate total number of species caught that year
ns.annual.total.fish <- ns.trawls.all.species %>% 
  subset(NUM>0) %>%
  group_by(YEAR) %>%
  distinct(SPECIES) %>%
  summarise(fishes.total = n())

##Sum KGHA and NOHA across all species to get total biomass and total number per ha
ns.trawls.sum<- ns %>%
  select(OP_ID,TARGET,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,NOHA,KGHA,
         TR_DESIGN,DISTANCE,TOW_TIME,HA_SWEPT) %>%
  group_by(OP_ID) %>%
  summarise(KGHA = sum(KGHA), NOHA = SUM(NOHA), Fishes = n()) %>%
  left_join(ns.trawls) %>% 
  mutate(Fishes = replace(Fishes, NOHA == 0, 0))


##calculate summary stats for nearshore total biomass by year
ns.annual.sum <- ns.trawls.sum %>% 
  group_by(YEAR) %>% 
  summarise(locations=n(), mean=mean(KGHA), median = median(KGHA), sd = sd(KGHA), 
            std.error = std.error(KGHA), skewness=skewness(KGHA),
            fishes.mean = mean(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) %>%
  left_join(ns.annual.total.fish) %>%
  mutate(FCO = ifelse(mean <7, "No", "Yes"))

##############################################################################
##calculate annual nearshore summary stats by station, year, and species

##Get mean annual values for each species
ns.annual.sum.by.species <- ns.trawls.all.species %>%
  group_by(YEAR, SPECIES) %>%
  summarise(mean = mean(KGHA), 
            median = median(KGHA), std.error = std.error(KGHA)) 

##Add fish common names to file
sci.names$SPECIES<-as.factor(sci.names$SPECIES)
ns.annual.sum.by.species <- ns.annual.sum.by.species %>%
  left_join(sci.names)


#############################################################################
#############################################################################
#############################################################################
####Offshore data - TARGETs =117 and 118 and depths >84 m

##pull out offshore suvery trawls
os<-subset(all.data, TARGET==118|TARGET==117 & YEAR>2010) %>%
  filter(TR_DESIGN==25|TR_DESIGN==4) %>%
  filter(END_DEPTH>84)

###If you want to compare data only from a particular Management Unit or State do it now
## os<-subset(filter(os, STATE== 'MN')
## os<-subset(os, M_UNIT == "WI2")

##Pull out all individual nearshore trawls
os.trawls<-os %>%
  select(OP_ID,SERIAL,TARGET,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,
         TR_DESIGN,DISTANCE,TOW_TIME,HA_SWEPT) %>%
  distinct(OP_ID, .keep_all = TRUE)


###Add zeros to NUM for fish species that were not caught
os.trawls.all.species <- os %>%
  select(OP_ID,SPECIES,NUM, NOHA, KGHA) 

os.trawls.all.species <-complete(os.trawls.all.species, OP_ID, SPECIES, 
                                 fill=list(NUM=0, NOHA=0, KGHA=0)) %>%
  left_join(os.trawls) 

##Calculate total number of species caught that year
os.annual.total.fish <- os.trawls.all.species %>% 
  subset(NUM>0) %>%
  group_by(YEAR) %>%
  distinct(SPECIES) %>%
  summarise(fishes.total = n())

##Sum KGHA and NOHA across all species to get total biomass and total number per ha
os.trawls.sum<- os %>%
  select(OP_ID,TARGET,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,NOHA,KGHA,
         TR_DESIGN,DISTANCE,TOW_TIME,HA_SWEPT) %>%
  group_by(OP_ID) %>%
  summarise(KGHA = sum(KGHA), NOHA = SUM(NOHA), Fishes = n()) %>%
  left_join(os.trawls) %>% 
  mutate(Fishes = replace(Fishes, NOHA == 0, 0))


##calculate summary stats for nearshore total biomass by year
os.annual.sum <- os.trawls.sum %>% 
  group_by(YEAR) %>% 
  summarise(locations=n(), mean=mean(KGHA), median = median(KGHA), 
            sd = sd(KGHA), std.error = std.error(KGHA),
            skewness=skewness(KGHA), fishes.mean = mean(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) 

##############################################################################
##calculate annual nearshore summary stats by year, and species
os.annual.sum.by.species <- os.trawls.all.species %>%
  group_by(YEAR, SPECIES) %>%
  summarise(mean = mean(KGHA), 
            median = median(KGHA), std.error = std.error(KGHA)) 

##Add fish common names to file
sci.names$SPECIES<-as.factor(sci.names$SPECIES)
os.annual.sum.by.species <- os.annual.sum.by.species %>%
  left_join(sci.names)



#############################################################################
#subset previous years to plot averages over different time frames
ns.5ymean<-subset(ns.annual.sum,  YEAR>=(max(YEAR)-4))
ns.10ymean<-subset(ns.annual.sum, YEAR > (max(YEAR)-9))
ns.15ymean<-subset(ns.annual.sum, YEAR>=(max(YEAR)-14))
ns.20ymean<-subset(ns.annual.sum, YEAR>=(max(YEAR)-19))
ns.30ymean<-subset(ns.annual.sum, YEAR>=(max(YEAR)-29))
ns.40ymean<-subset(ns.annual.sum, YEAR>=(max(YEAR)-39))
ns.1985_1995mean<-subset(ns.annual.sum, YEAR >=1985 & YEAR <= 1995)
ns.2000_2010mean<-subset(ns.annual.sum, YEAR >=2000 & YEAR <= 2010)


###############################################################################
##Nearshore data plots
##turn on/off the geom_segment(...) lines to change which mean lines you want to show

ggplot(ns.annual.sum, aes(x=YEAR, y=mean)) +
  geom_bar(stat='identity', fill='grey75', color='black') +
  geom_errorbar(data=ns.annual.sum, aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4)+
  geom_segment(x=(max(ns.annual.sum$YEAR)-4), xend=max(ns.annual.sum$YEAR), y=mean(ns.5ymean$mean), #5 year mean
               yend=mean(ns.5ymean$mean), size=1.2, color='black')+
  geom_segment(x=(max(ns.annual.sum$YEAR)-9), xend=max(ns.annual.sum$YEAR), y=mean(ns.10ymean$mean), #10 year mean
               yend=mean(ns.10ymean$mean), size=1.2, color='black')+
  geom_segment(x=(max(ns.annual.sum$YEAR)-19), xend=max(ns.annual.sum$YEAR), y=mean(ns.20ymean$mean), #20 year mean
               yend=mean(ns.20ymean$mean), size=1.2, color='black')+
  geom_segment(x=(max(ns.annual.sum$YEAR)-29), xend=max(ns.annual.sum$YEAR), y=mean(ns.30ymean$mean), #30 year mean
               yend=mean(ns.30ymean$mean), size=1.2, color='black')+
  geom_segment(x=(max(ns.annual.sum$YEAR)-39), xend=max(ns.annual.sum$YEAR), y=mean(ns.40ymean$mean), #40 year mean
               yend=mean(ns.40ymean$mean), size=1.2, color='black') +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme +
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Total Fish Biomass',
       subtitle='USGS bottom trawl assessment') 
  
ggsave(here('Plots and Tables/RVCAT','ns_annual_totalbiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##plot mean nearshore biomass no mean lines
ggplot(ns.annual.sum, aes(x=YEAR, y=mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(data=ns.annual.sum, aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4)+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Total Fish Biomass',
       subtitle='USGS bottom trawl assessment')
  
##save plot to the folder you assigned as the working directory
ggsave(here('Plots and Tables/RVCAT','ns_annual_totalbiomass_nomeans.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##lollpop with potential FCO 
ns.annual.sum.2020 <- ns.annual.sum %>%
  subset(YEAR <2020)

ggplot(ns.annual.sum.2020, aes(x=YEAR, y=mean)) + 
  aes(x=YEAR, y = mean, fill = FCO) + 
  geom_hline(yintercept=7, size = 1.5, colour = 'sienna2') +
  geom_point(shape = 21, size=6)+
  geom_segment(aes(x=YEAR, xend=YEAR, y=0, yend=mean), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  plot_theme+
  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
  theme(legend.box='horizontal',
        legend.position = c(0.8, 0.8),
        legend.title.align=0.5) +
  labs(x='Year Class', y='Mean biomass (kg per ha)', caption=ann_data_access,
       title='Lake Superior Total Fish Biomass',
       subtitle='USGS bottom trawl assessment')
ggsave(here('Plots and Tables/RVCAT','ns_totalbiomass_FCO.png'), dpi = 300, width = 40, height = 20, units = "cm")



##ns mean biomass with 10 year chunk means
ggplot(ns.annual.sum, aes(x=YEAR, y=mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(data=ns.annual.sum, aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4)+
  geom_rect(xmin=1985, xmax=1995, ymin=(mean(ns.1985_1995mean$mean)-0.5), 
            ymax=(mean(ns.1985_1995mean$mean+0.5)), fill='deeppink', alpha=0.03)+
  geom_rect(xmin=2000, xmax=2010, ymin=(mean(ns.2000_2010mean$mean)-0.5), 
            ymax=(mean(ns.2000_2010mean$mean+0.5)), fill='deeppink', alpha=0.03)+
  geom_rect(xmin=max(ns.annual.sum$YEAR-10), xmax=max(ns.annual.sum$YEAR), 
            ymin=(mean(ns.10ymean$mean)-0.5), 
            ymax=(mean(ns.10ymean$mean)+0.5), fill='deeppink', alpha=0.03)+
  geom_hline(yintercept=mean(ns.annual.sum$mean), size = 1.5, colour = 'sienna2') +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Total Fish Biomass',
       subtitle='USGS bottom trawl assessment') 
  
ggsave(here('Plots and Tables/RVCAT','ns_annual_totalbiomass_10yrmeans.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##ns skewness plot------------------------------------------------------------------------------------------------------SKEWNESS-----
ggplot(ns.annual.sum, aes(x=YEAR, y=skewness))+
  geom_point(color='black', size=3)+
  geom_line(size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(), limits=c(0,max(ns.annual.sum$skewness))) +
  geom_hline(yintercept=mean(ns.annual.sum$skewness)) +
  plot_theme +
  labs(x='Year', y='Skewness in total biomass across locations',caption=ann_data_access,
       title='Lake Superior Nearshore Survey',
       subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','ns_annual_totalbiomass_skewness.png'), dpi = 300, width = 40, height = 20, units = "cm") 


####################################################################################################################
####################################################################################################

##plot mean nearshore biomass for each species of interest

#########################################################################################
###Cisco and Bloater
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Cisco" | COMMON_NAME == "Bloater")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0),breaks = breaks_extended())+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Cisco and Bloater Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1'))+
  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_cisco_bloater.png'), dpi = 300, width = 40, height = 20, units = "cm") 


###Cisco, Bloater, LWF
ggplot(subset(ns.annual.sum.by.species, 
              COMMON_NAME == "Cisco" |
              COMMON_NAME == "Bloater" |
              COMMON_NAME == "Lake Whitefish")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Coregonus Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_cisco_bloater_lwf.png'), dpi = 300, width = 40, height = 20, units = "cm") 


#####Individual Species
###Cisco
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Cisco")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Cisco Biomass',
       subtitle='USGS bottom trawl assessment') 
  
ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_Cisco.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Bloater
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Bloater")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Bloater Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_Bloater.png'), dpi = 300, width = 40, height = 20, units = "cm") 
    
###Lake Whitefish
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Lake Whitefish")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Lake Whitefish Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_LWF.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Longnose Sucker
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Longnose Sucker")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Longnose Sucker Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_LongnoseSucker.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Rainbow Smelt
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Rainbow Smelt")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Rainbow Smelt Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_RainbowSmelt.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Slimy Sculpin
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Slimy Sculpin")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Slimy Sculpin Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_SlimySculpin.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Spoonhead Sculpin
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Spoonhead Sculpin")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Spoonhead Sculpin Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_SpoonheadSculpin.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Deepwater Sculpin
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Deepwater Sculpin")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Deepwater Sculpin Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_DeepwaterSculpin.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Slimy, Spoons, and Deepwater
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Slimy Sculpin" | 
                COMMON_NAME == "Spoonhead Sculpin" |
                COMMON_NAME == "Deepwater Sculpin")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Sculpin Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_Sculpin.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Trout-Perch, Ninespine
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Trout-Perch" |
                COMMON_NAME == "Ninespine Stickleback")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Trout-Perch and Ninespine Stickleback Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1'))+
  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_TroutPerch_NineSpine.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Pygmy Whitefish
ggplot(subset(ns.annual.sum.by.species, COMMON_NAME == "Pygmy Whitefish")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Pygmy Whitefish Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_PWF.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Miscellaneous species on one plot
ggplot(subset(ns.annual.sum.by.species, 
                COMMON_NAME == "Alewife" | 
                COMMON_NAME == "Burbot" |
                COMMON_NAME == "Longnose Sucker" |
                COMMON_NAME == "Ninespine Stickleback" |
                COMMON_NAME == "Round Whitefish" |
                COMMON_NAME == "Ruffe" |
                COMMON_NAME == "Threespine Stickleback" |
                COMMON_NAME == "Trout-Perch")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Miscellaneous Species Biomass',
       subtitle='USGS bottom trawl assessment')+
  facet_wrap(vars(COMMON_NAME), ncol=2,scales='free') 

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_MiscSpecies.png'), dpi = 300, width = 40, height = 20, units = "cm") 


#################################################################################################
##biomass at individual nearshore stations for the current sampling year, ranked by biomass
##Stand alone plot, saved to a file in black and white
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA) +
  geom_bar(stat='identity', fill='grey75', color='black') +
  geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) + 
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
     caption=ann_data_access,
     title='Lake Superior Fish Biomass at Nearshore Stations',
     subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  
  
ggsave(here('Plots and Tables/RVCAT','ns_biomass_by_station_bw.png'), dpi = 300, width = 40, height = 20, units = "cm")

## Color gradient ars to match colors on map
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_biomass_by_station_color.png'), dpi = 300, width = 40, height = 20, units = "cm")

##################################################################################################
##inset skewness plot into station biomass plot, black and white
###Skewness plot for insetting into individual station biomass plot
ns.skew<-ggplot(ns.annual.sum, aes(x=YEAR, y=skewness))+
  geom_point(color='black', size=3)+
  geom_line(size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(), limits=c(0,max(ns.annual.sum$skewness))) +
  geom_hline(yintercept=mean(ns.annual.sum$skewness)) +
  plot_theme +
  labs(x='Year', y='Skewness')

ns.skew


ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
       aes(x=reorder(LOCATION, KGHA), y=KGHA) +
  geom_bar(stat='identity', fill='grey75', color='black') +
  geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(ns.skew), ymin=20, ymax=45, xmax=60, xmin=3) + 
      ##controls placement of the inset plot if need to change
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)')+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_biomass_by_station_skewness_bw.png'), dpi = 300, width = 40, height = 20, units = "cm")

## Color gradient ars to match colors on map
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  annotation_custom(ggplotGrob(ns.skew), ymin=20, ymax=45, xmax=60, xmin=3) + 
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_biomass_by_station_skewness_color.png'), dpi = 300, width = 40, height = 20, units = "cm")


#############################################################################################
##nearshore map color coded by station total biomass, for inset into station biomass plot
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##create map with stations color coded by biomass
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(ns.trawls.sum, YEAR ==max(ns.trawls.sum$YEAR)),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=KGHA), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84')) + 
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude',
        caption=ann_data_access, 
        title='Lake Superior Nearshore Station Fish Biomass',
        subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_sites_biomass_map.png'), dpi = 300, width = 30, height = 16, units = "cm")


##nearshore map color coded by station total biomass, for inset into station biomass plot----------------------------------------
ns.current.biomass.map<-ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(ns.trawls.sum, YEAR ==max(ns.trawls.sum$YEAR)),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=KGHA), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84')) + 
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude')

ns.current.biomass.map

##inset above map into station biomass bar graph, with corresponding color scales
## Color gradient ars to match colors on map
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(ns.current.biomass.map), ymin=10, ymax=49, xmax=55, xmin=3)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_sites_biomass_map2.png'), dpi = 300, width = 40, height = 20, units = "cm")


##create map with stations color coded by fishes
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR)), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=Fishes), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Fish species') +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84')) + 
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude',
       caption=ann_data_access, 
       title='Lake Superior Nearshore Fish Species Richness',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_sites_fishes_map.png'), dpi = 300, width = 30, height = 16, units = "cm")


######################################################################################################
######################################################################################################
######################################################################################################
##OFFSHORE plots #####################################################################

####################################################################################################################NEARSHORE DATA####
##plot mean offshore biomass
ggplot(os.annual.sum, aes(x=YEAR, y=mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(data=os.annual.sum, aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4)+
  geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  plot_theme +
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Total Fish Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','os_annual_totalbiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##plot mean nearshore biomass no mean lines
ggplot(os.annual.sum, aes(x=YEAR, y=mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(data=os.annual.sum, aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4)+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Total Fish Biomass',
       subtitle='USGS bottom trawl assessment')

##save plot to the folder you assigned as the working directory
ggsave(here('Plots and Tables/RVCAT','os_annual_totalbiomass_nomeans.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##os skewness plot----------------SKEWNESS-----
ggplot(os.annual.sum, aes(x=YEAR, y=skewness))+
  geom_point(color='black', size=3)+
  geom_line(size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  geom_hline(yintercept=mean(os.annual.sum$skewness)) +
  plot_theme +
  labs(x='Year', y='Skewness in total biomass across locations',caption=ann_data_access,
       title='Lake Superior Offshore Survey',
       subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','os_annual_totalbiomass_skewness.png'), dpi = 300, width = 40, height = 20, units = "cm") 


####################################################################################################################
####################################################################################################

##plot mean offshore biomass for each species of interest

#########################################################################################
###Kiyi, DWS, Siscowet
ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "Kiyi" |
                       COMMON_NAME == "siscowet Lake Trout" | 
                       COMMON_NAME == "Deepwater Sculpin")) +
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Deepwater Sculpin, Kiyi, and Siscowet Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  theme(legend.position = c(0.8,0.95))

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_kiyi_DWS_siscowet_stack.png'), dpi = 300, width = 40, height = 20, units = "cm") 


ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "Kiyi" |
                       COMMON_NAME == "siscowet Lake Trout" | 
                       COMMON_NAME == "Deepwater Sculpin")) +
       aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
#  geom_hline(y=intercept = mean(os.annual.sum.by.species$mean)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Deepwater Sculpin, Kiyi, and Siscowet Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  facet_grid(.~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_kiyi_DWS_siscowet_facet.png'), dpi = 300, width = 40, height = 20, units = "cm") 


#####Individual Species
###Kiyi
ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "Kiyi")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Kiyi Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_Kiyi.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###DWS
ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "Deepwater Sculpin")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Deepwater Sculpin Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_DWS.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###siscowet Lake Trout
ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "siscowet Lake Trout")) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore siscowet Lake Trout Biomass',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_siscowet.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###########################################################################################
##biomass at individual offshore stations for the current sampling year, ranked by biomass
##Stand alone plot, saved to a file in black and white
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) +
  aes(x=reorder(LOCATION, KGHA), y=KGHA) +
  geom_bar(stat='identity', fill='grey75', color='black') +
  geom_hline(yintercept=mean(os.trawls.sum$KGHA)) + 
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_biomass_by_station_bw.png'), dpi = 300, width = 40, height = 20, units = "cm")


## Color gradient to match colors on map
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_biomass_by_station_color'), dpi = 300, width = 40, height = 20, units = "cm")


## Color gradient ars to match colors on map
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) +
  aes(x=reorder(LOCATION, Fishes), y=Fishes, fill = Fishes) +
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  geom_hline(yintercept=mean(os.trawls.sum$Fishes)) +
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Fish species collected',
       caption=ann_data_access,
       title='Lake Superior Fish Species Richness at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(os.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_fishes_by_station_color.png'), dpi = 300, width = 40, height = 20, units = "cm")

##################################################################################################
##inset skewness plot into station biomass plot, black and white
###Skewness plot for insetting into individual station biomass plot
os.skew<-ggplot(os.annual.sum, aes(x=YEAR, y=skewness))+
  geom_point(color='black', size=3)+
  geom_line(size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(), 
                     limits=c(0,max(os.annual.sum$skewness))) +
  geom_hline(yintercept=mean(os.annual.sum$skewness)) +
  plot_theme +
  labs(x='Year', y='Skewness')

os.skew

ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA) +
  geom_bar(stat='identity', fill='grey75', color='black') +
  geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(os.skew), ymin=20, ymax=45, xmax=60, xmin=3) + 
  ##controls placement of the inset plot if need to change
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)')+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(os.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_biomass_by_station_skewness_bw.png'), dpi = 300, width = 40, height = 20, units = "cm")

ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill=KGHA) +
  geom_bar(stat='identity') +
  geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(os.skew), ymin=10, ymax=22, xmax=30, xmin=1) + 
  ##controls placement of the inset plot if need to change
  scale_fill_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)')+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(os.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_biomass_by_station_skewness_color.png'), dpi = 300, width = 40, height = 20, units = "cm")


#############################################################################################
##offshore map color coded by station total biomass, for inset into station biomass plot
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##create map with stations color coded by biomass
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR)), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=KGHA), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84')) + 
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude',
       caption=ann_data_access, 
       title='Lake Superior Offshore Station Total Fish Biomass',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_sites_biomass_map.png'), dpi = 300, width = 30, height = 16, units = "cm")

##create map with stations color coded by fishes
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR)) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=Fishes), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Fish species') +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84')) + 
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude',
       caption=ann_data_access, 
       title='Lake Superior Offshore Fish Species Richness',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_sites_fishes_map.png'), dpi = 300, width = 30, height = 16, units = "cm")

##offshore map color coded by station total biomass, for inset into station biomass plot----------------------------------------
os.current.biomass.map<-ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR)), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=KGHA), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_y_continuous() +
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84')) + 
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude')

os.current.biomass.map

##inset above map into station biomass bar graph, with corresponding color scales
## Color gradient ars to match colors on map
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(os.current.biomass.map), ymin=10, ymax=49, xmax=55, xmin=3)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment,', max(os.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','os_current_sites_biomass_map2.png'), dpi = 300, width = 40, height = 20, units = "cm")

###############################################################################################
###############################################################################################
##sample site map########################################################################################################MAP##########
##NOTE: need to have the shapefiles folder in your working directory for these to work

ls_poly <- readOGR(dsn =here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##subset out the current year, and nearshore and offshore targets (if you want Cheq Bay, can add that target code)
##Nearshore and offshore and CBay sites
all.sites.current<-all.data %>% 
  select(OP_ID,OP_DATE,YEAR,TARGET,LOCATION, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
         BEG_LATITUDE_DD, END_LATITUDE_DD, Mid.Lat.DD, Mid.Long.DD, 
         BEG_DEPTH, END_DEPTH,Surface.Temp, Bottom.Temp) %>%
  filter(YEAR==max(YEAR)) %>% 
  filter(TARGET==2|TARGET==117|TARGET==118|TARGET==106) %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                              "118" = "offshore", "106" = "Chequamegon Bay")))
 
#Map of all sites visited in the current year
ggplot(all.sites.current, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(all.sites.current, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=survey), size=6, stroke=1.5)+
  scale_color_manual(values=c('salmon','cadetblue2'), 
                     #scale_color_manual(values=c('salmon','cadetblue2', 'palegreen3'), 
                                        name='Survey', labels=c('Nearshore','Offshore'))+
#  name='Survey', labels=c('Chequamegon Bay','Nearshore','Offshore'))+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Stations Sampled',
       subtitle=(paste('USGS bottom trawl assessments,', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_all_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")

#Nearshore and offshore trawl paths
ggplot(subset(all.sites.current, survey == "nearshore" | survey == "offshore")) +
  aes(Mid.Long.DD, Mid.Lat.DD) + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_segment(aes(x=BEG_LONGITUDE_DD, xend=END_LONGITUDE_DD, 
              y=BEG_LATITUDE_DD, yend=END_LATITUDE_DD, color=survey), size=2)+
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  scale_color_manual(values=c('salmon','cadetblue2'), name='Survey', 
                     labels=c('Nearshore','Offshore'))+
  theme_bw() +
  map_theme+
  labs(caption=ann_data_access,
       title='Lake Superior Bottom Trawl Paths',
       subtitle=(paste('USGS bottom trawl assessment,', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_ns_os_trawlpaths.png'), dpi = 300, width = 40, height = 20, units = "cm")


#Nearshore and offshore sites as simple points
ggplot(subset(all.sites.current, survey == "nearshore" | survey == "offshore")) +
  aes(Mid.Long.DD, Mid.Lat.DD) + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(aes(Mid.Long.DD, Mid.Lat.DD, color=survey), size=6, stroke=1.5)+
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  scale_color_manual(values=c('salmon','cadetblue2'), name='Survey', 
                     labels=c('Nearshore','Offshore'))+
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Stations Sampled',
       subtitle=(paste('USGS bottom trawl assessment,', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_ns_os_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


##Chequamegon Bay trawl map--------------------------------------------------------------------------------------Cheq Bay Map---------
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(subset(all.sites.current, survey == "Chequamegon Bay")) +
  geom_segment(aes(x=BEG_LONGITUDE_DD, xend=END_LONGITUDE_DD, 
                   y=BEG_LATITUDE_DD, yend=END_LATITUDE_DD), 
                   color='blue3', lineend='round', size=2) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1)+
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  theme_bw() +
  map_theme+
  labs(caption=ann_data_access,
       title='Chequamegon Bay Bottom Trawl Paths',
       subtitle=(paste('USGS bottom trawl assessment,', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_cbay_trawlpaths.png'), dpi = 300, width = 35, height = 25, units = "cm")


##All Cheq Bay samples
cbay.trawl.sites <- cbay.trawls %>%
  unite('spot', YEAR:SERIAL, remove=FALSE)

ggplot(cbay.trawl.sites, aes(Mid.Long.DD, Mid.Lat.DD)) + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
#  geom_point(aes(Mid.Long.DD, Mid.Lat.DD), shape = 21size=10, stroke=1.5)+
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  geom_text(aes(label=spot)) +
#  geom_text(aes(label=OP_ID)) +
  theme_bw() +
  map_theme+
  labs(caption=ann_data_access,
       title='Chequamegon Bay Bottom Trawl Sites, TARGET = 106',
 #      subtitle='Locations marked with OP_ID')
       subtitle='Locations marked with Year and SERIAL number')

ggsave(here('Plots and Tables/RVCAT','cbay_alltrawls.png'), dpi = 300, width = 35, height = 25, units = "cm")


##Miscellaneous Map to check sample locations - a lat/long test
#Map of all sites visited in the current year
pig <- all.data %>%
  subset(TARGET == 2) %>%
  subset(YEAR == 1978) 

pig <- distinct(pig, OP_ID, .keep_all = TRUE)

ggplot(pig, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=pig, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=YEAR), size=1, stroke=1.5)+
  scale_color_continuous(high='red',low='deepskyblue2',name=expression(underline('Year')), 
                        breaks=pretty_breaks()) +
  geom_text(aes(label=OP_ID)) +
  map_theme+
  labs(caption=ann_data_access,
       title='Lake Superior USGS Trawl Locations, 1983')
#,     subtitle='Sites labeled with OP_ID')

ggsave(here('Plots and Tables/RVCAT','LSBS_Misc_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")

ggplot(pig, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=pig, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=YEAR), size=1, stroke=1.5)+
  scale_color_continuous(high='red',low='deepskyblue2',name=expression(underline('Year')), 
                         breaks=pretty_breaks()) +
  geom_text(aes(label=OP_ID)) +
  map_theme+
  labs(caption=ann_data_access,
       title='Lake Superior USGS Trawl Locations, 1983')
#,     subtitle='Sites labeled with OP_ID')

ggsave(here('Plots and Tables/RVCAT','LSBS_Misc_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")



###############################################################################################
###############################################################################################
##animated plot of nearshore biomass at each station across years-----------

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ns_map1<-ggplot(ns.trawls.sum, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(ns.trawls.sum, mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), limits=c(0,30), breaks=seq(0,30, by=5))+
  scale_color_continuous(high='red',low='deepskyblue2',name=expression(underline('Biomass (kg per ha)')), limits=c(0,30), 
                         breaks=seq(0,30, by=5))+
  map_theme+
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Nearshore Total Fish Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

ns_map1<-ns_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), size=8, family='serif')+
  transition_manual(YEAR)

ns_map1_gif<-animate(ns_map1, fps = 3, end_pause = 10, width = 1000, height = 500,renderer = gifski_renderer(loop=F))
ns_map1_gif
anim_save(here('Plots and Tables/RVCAT','Animated_ns_station_biomass_gif.gif'))


ns_bars1<-ggplot(ns.annual.sum, aes(x=YEAR, y=mean, fill=mean))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

ns_bars1_gif<-animate(ns_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(ns_map1_gif)
q_mgif<-image_read(ns_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:43){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_ns_ann_biomass_map_bars.gif'))



###############################################################################################
###############################################################################################
##animated plot of offshore biomass at each station across years-----------

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

os_map1<-ggplot(os.trawls.sum, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(os.trawls.sum, mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), limits=c(0,30), breaks=seq(0,30, by=5))+
  scale_color_continuous(high='red',low='deepskyblue2',name=expression(underline('Biomass (kg per ha)')), limits=c(0,30), 
                         breaks=seq(0,30, by=5))+
  map_theme+
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Offshore Total Fish Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

os_map1<-os_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

os_map1_gif<-animate(os_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
os_map1_gif
anim_save(here('Plots and Tables/RVCAT','Animated_os_station_biomass_gif.gif'))


os_bars1<-ggplot(os.annual.sum, aes(x=YEAR, y=mean, fill=mean))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

os_bars1_gif<-animate(os_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(os_map1_gif)
q_mgif<-image_read(os_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:43){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_os_ann_biomass_map_bars.gif'))


#########################################################################################################
##Pie maps showing biomass of cisoes, lwf, and sculpins and lake trout #########
#########################################################################################################
###Extract nearshore and offshore data for selected species
ns.os.pies <- ns.trawls.all.species %>%
  bind_rows(os.trawls.all.species) %>%
  left_join(sci.names) %>%
  select(OP_ID,YEAR,LOCATION,COMMON_NAME,KGHA,Mid.Lat.DD, Mid.Long.DD) %>%
  subset(COMMON_NAME == 'Bloater' |
           COMMON_NAME == 'Cisco' |
           COMMON_NAME == 'Kiyi' |
           COMMON_NAME == 'Lake Whitefish' |
           COMMON_NAME == 'Pygmy Whitefish' |
           COMMON_NAME == 'Slimy Sculpin' |
           COMMON_NAME == 'Spoonhead Sculpin' |
           COMMON_NAME == 'Deepwater Sculpin' |
           COMMON_NAME == 'lean Lake Trout' |
           COMMON_NAME == 'siscowet Lake Trout') %>%
  pivot_wider(names_from = COMMON_NAME, values_from = KGHA, values_fill = 0) %>%
  subset(Mid.Lat.DD >0)

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


##Biomass pie map for bloater, cisco, kiyi for current year
ggplot(subset(ns.os.pies, YEAR == max(ns.os.pies$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(ns.os.pies, YEAR == max(ns.os.pies$YEAR)),
                  cols= c("Bloater", "Cisco", "Kiyi"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Ciscoe Distributions',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.os.pies$YEAR))), 
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','CurrentYear_ciscoe_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')

####################################################################################
##Animated figure showing each year at a time
biomass.pie.map1<-ggplot(ns.os.pies, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=ns.os.pies,
                  cols= c("Bloater", "Cisco", "Kiyi"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Ciscoe Distributions',
       subtitle='USGS bottom trawl assessment', 
       caption=ann_data_access)

transition_manual(YEAR)

biomass.pie.map2<-biomass.pie.map1+geom_text(aes(x=-85, y=48.75, 
                                           label=paste('Year:', YEAR, sep='\n')), 
                                           size=8, family='serif') +
  transition_manual(YEAR)

a1p_gif<-animate(biomass.pie.map2, fps = .5, end_pause = 15, nframes=57, ##NOTE: as more years are added, need to increase nframes to the # years
                 width = 900, height = 500, renderer = gifski_renderer(loop=T))
a1p_gif
anim_save(here('Plots and Tables/RVCAT','Animated_ciscoe_pies.gif')) 

####################################################################################
##Animated figure showing ciscoes and lake whitefish
biomass.pie.map1<-ggplot(ns.os.pies, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=ns.os.pies,
                  cols= c("Bloater", "Cisco", "Kiyi", "Lake Whitefish"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Whitefish Distributions',
       subtitle='USGS bottom trawl assessment', 
       caption=ann_data_access)

transition_manual(YEAR)

biomass.pie.map2<-biomass.pie.map1+geom_text(aes(x=-85, y=48.75, 
                                                 label=paste('Year:', YEAR, sep='\n')), 
                                             size=8, family='serif') +
  transition_manual(YEAR)

a1p_gif<-animate(biomass.pie.map2, fps = .5, end_pause = 15, nframes=57, ##NOTE: as more years are added, need to increase nframes to the # years
                 width = 900, height = 500, renderer = gifski_renderer(loop=T))
a1p_gif
anim_save(here('Plots and Tables/RVCAT','Animated_ciscoeLWF_pies.gif')) 

###########################################################################################
##Animated figure showing ciscoes, lake whitefish, and pygmy whitefish
biomass.pie.map1<-ggplot(ns.os.pies, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=ns.os.pies,
                  cols= c("Bloater", "Cisco", "Kiyi", "Lake Whitefish", "Pygmy Whitefish"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Whitefish Distributions',
       subtitle='USGS bottom trawl assessment', 
       caption=ann_data_access)

transition_manual(YEAR)

biomass.pie.map2<-biomass.pie.map1+geom_text(aes(x=-85, y=48.75, 
                                                 label=paste('Year:', YEAR, sep='\n')), 
                                             size=8, family='serif') +
  transition_manual(YEAR)

a1p_gif<-animate(biomass.pie.map2, fps = .5, end_pause = 15, nframes=57, ##NOTE: as more years are added, need to increase nframes to the # years
                 width = 900, height = 500, renderer = gifski_renderer(loop=T))
a1p_gif
anim_save(here('Plots and Tables/RVCAT','Animated_whitefish_pies.gif')) 

###############################################################################
###Sculpins - Slimy, Spoonhead, Deep

ggplot(subset(ns.os.pies, YEAR == max(ns.os.pies$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(ns.os.pies, YEAR == max(ns.os.pies$YEAR)),
                  cols= c("Slimy Sculpin", "Spoonhead Sculpin", "Deepwater Sculpin"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Sculpin Distributions',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.os.pies$YEAR))), 
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','CurrentYear_sculpin_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')

####################################################################################
##Animated figure showing each year at a time
biomass.pie.map1<-ggplot(ns.os.pies, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=ns.os.pies,
                  cols= c("Slimy Sculpin", "Spoonhead Sculpin", "Deepwater Sculpin"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Sculpin Distributions',
       subtitle='USGS bottom trawl assessment', 
       caption=ann_data_access)

transition_manual(YEAR)

biomass.pie.map2<-biomass.pie.map1+geom_text(aes(x=-85, y=48.75, 
                                               label=paste('Year:', YEAR, sep='\n')), size=8, family='serif') +
  transition_manual(YEAR)

a1p_gif<-animate(biomass.pie.map2, fps = .5, end_pause = 15, nframes=57, ##NOTE: as more years are added, need to increase nframes to the # years
                 width = 900, height = 500, renderer = gifski_renderer(loop=T))
a1p_gif
anim_save(here('Plots and Tables/RVCAT','Animated_sculpin_pies.gif')) 


#########################################################################
###Lake trout - lean and siscowet

ggplot(subset(ns.os.pies, YEAR == max(ns.os.pies$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(ns.os.pies, YEAR == max(ns.os.pies$YEAR)),
                  cols= c("lean Lake Trout", "siscowet Lake Trout"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Lake Trout Distributions',
       subtitle=(paste('USGS bottom trawl assessment,', max(ns.os.pies$YEAR))), 
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','CurrentYear_laketrout_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')

####################################################################################
##Animated figures showing changes in biomass across time for individual species 
ns.os.species.maps <- ns.trawls.all.species %>%
  bind_rows(os.trawls.all.species) %>%
  left_join(sci.names) %>%
  select(OP_ID,YEAR,LOCATION,COMMON_NAME,KGHA,NOHA,Mid.Lat.DD, Mid.Long.DD) %>%
  subset(COMMON_NAME == 'Bloater' |
           COMMON_NAME == 'Cisco' |
           COMMON_NAME == 'Kiyi' |
           COMMON_NAME == 'Lake Whitefish' |
           COMMON_NAME == 'Pygmy Whitefish' |
           COMMON_NAME == 'Slimy Sculpin' |
           COMMON_NAME == 'Spoonhead Sculpin' |
           COMMON_NAME == 'Deepwater Sculpin' |
           COMMON_NAME == 'lean Lake Trout' |
           COMMON_NAME == 'siscowet Lake Trout') %>%
  subset(Mid.Lat.DD >0)

###################################################################
pig<-ns.os.species.maps %>%
  subset(COMMON_NAME == 'Bloater')

##Cisco
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Cisco')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Cisco'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        limits=c(0,50), breaks=seq(0,50, by=10))+
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         limits=c(0,50), breaks=seq(0,50, by=10))+
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Cisco Biomss',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_Cisco_station_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Cisco')) +
  aes(x=YEAR, y=mean, fill=mean)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

sp_bars1_gif<-animate(sp_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(sp_map1_gif)
q_mgif<-image_read(sp_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))

for(i in 2:48){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Cisco_ann_biomass_map_bars.gif'))


##Bloater
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Bloater')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Bloater'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        limits=c(0,50), breaks=seq(0,50, by=10))+
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         limits=c(0,50), breaks=seq(0,50, by=10))+
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Bloater Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_Bloater_station_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Bloater')) +
  aes(x=YEAR, y=mean, fill=mean)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

sp_bars1_gif<-animate(sp_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(sp_map1_gif)
q_mgif<-image_read(sp_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))

for(i in 2:48){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Bloater_ann_biomass_map_bars.gif'))


##Lake Whitefish
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Lake Whitefish')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Lake Whitefish'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        limits=c(0,50), breaks=seq(0,50, by=10))+
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         limits=c(0,50), breaks=seq(0,50, by=10))+
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Lake Whitefish Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_LWF_station_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Lake Whitefish')) +
  aes(x=YEAR, y=mean, fill=mean)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

sp_bars1_gif<-animate(sp_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(sp_map1_gif)
q_mgif<-image_read(sp_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))

for(i in 2:48){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_LWF_ann_biomass_map_bars.gif'))


##Pygmy Whitefish
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Pygmy Whitefish')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Pygmy Whitefish'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        limits=c(0,15), breaks=seq(0,15, by=3))+
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         limits=c(0,15), breaks=seq(0,15, by=3))+
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Pygmy Whitefish Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_PWF_station_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Pygmy Whitefish')) +
  aes(x=YEAR, y=mean, fill=mean)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

sp_bars1_gif<-animate(sp_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(sp_map1_gif)
q_mgif<-image_read(sp_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))

for(i in 2:48){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_PWF_ann_biomass_map_bars.gif'))

##############################################################################
############################################################################
##Chequamegon Bay#############################################Cheq Bay#####
######################################################################################################
####TARGET = 106

##pull out CBay suvery trawls
cbay<-subset(all.data, TARGET==106)

##Pull out all individual nearshore trawls
cbay.trawls<-cbay %>%
  select(OP_ID,SERIAL,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp) %>%
  distinct(OP_ID, .keep_all = TRUE)


###Add zeros to NUM for fish species that were not caught
cbay.trawls.all.species <- cbay %>%
  select(OP_ID,SPECIES,NUM, NOHA) 

cbay.trawls.all.species <-complete(cbay.trawls.all.species, OP_ID, SPECIES, 
                                 fill=list(NUM=0, NOHA=0)) %>%
  left_join(cbay.trawls) 

##Calculate total number of species caught that year
cbay.annual.total.fish <- cbay.trawls.all.species %>% 
  subset(NUM>0) %>%
  group_by(YEAR) %>%
  distinct(SPECIES) %>%
  summarise(fishes.total = n())

##Sum KGHA and NOHA across all species to get total biomass and total number per ha
cbay.trawls.sum<- cbay %>%
  select(OP_ID,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,NOHA) %>%
  group_by(OP_ID) %>%
  summarise(NOHA = SUM(NOHA), Fishes = n()) %>%
  left_join(cbay.trawls) %>% 
  mutate(Fishes = replace(Fishes, NOHA == 0, 0))


##calculate summary stats for nearshore total biomass by year
cbay.annual.sum <- cbay.trawls.sum %>% 
  group_by(YEAR) %>% 
  summarise(locations=n(), mean=mean(NOHA), median = median(NOHA), 
            sd = sd(NOHA), std.error = std.error(NOHA),
            skewness=skewness(NOHA), fishes.mean = mean(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) %>%
  left_join(cbay.annual.total.fish) 

##############################################################################
##calculate annual summary stats by station, year, and species
cbay.annual.sum.by.species <- cbay.trawls.all.species %>%
  group_by(YEAR, SPECIES) %>%
  summarise(mean = mean(NOHA), 
            median = median(NOHA), std.error = std.error(NOHA)) 

##Add fish common names to file
sci.names$SPECIES<-as.factor(sci.names$SPECIES)
cbay.annual.sum.by.species <- cbay.annual.sum.by.species %>%
  left_join(sci.names)



####################################################################################################################NEARSHORE DATA####
##plot mean CBay biomass
ggplot(cbay.annual.sum, aes(x=YEAR, y=mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(data=cbay.annual.sum, aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4)+
  geom_hline(yintercept=mean(cbay.annual.sum$mean)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  plot_theme +
  labs(x='Year', y='Mean number per ha',
       caption=ann_data_access,
       title='Chequamegon Bay Total Fish Abundance',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','cbay_annual_totalabundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##plot CBay fish species
ggplot(cbay.annual.sum, aes(x=YEAR, y=fishes.total))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_hline(yintercept=mean(cbay.annual.sum$fishes.total)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  plot_theme +
  labs(x='Year', y='Total number of species collected',
       caption=ann_data_access,
       title='Chequamegon Bay Fish Species Richness',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','cbay_annual_totalfishes.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##plot CBay fish species
ggplot(cbay.annual.sum, aes(x=YEAR, y=fishes.mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_hline(yintercept=mean(cbay.annual.sum$fishes.mean)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  plot_theme +
  labs(x='Year', y='Mean number of species collected per site',
       caption=ann_data_access,
       title='Chequamegon Bay Fish Species Richness',
       subtitle='USGS bottom trawl assessment') 

ggsave(here('Plots and Tables/RVCAT','cbay_annual_meanfishes.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Yellow Perch, Trout-Perch, ninespine 
ggplot(subset(cbay.annual.sum.by.species, 
              COMMON_NAME == "Ninespine Stickleback" |
                COMMON_NAME == "Yellow Perch" |
                COMMON_NAME == "Trout-Perch")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  labs(x='Year', y='Mean abundance (number per ha)',caption=ann_data_access,
       title='Chequamegon Bay Fish Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','cbay_annual_biomass_yp_9spine_tp.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###############################################################################################
##animated plot of Chequamegon Bay total abundance at each station across years-----------

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

cbay_map1<-ggplot(cbay.trawls.sum, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), 
                     labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(cbay.trawls.sum, mapping=aes(Mid.Long.DD, Mid.Lat.DD, 
                      size=NOHA, color=NOHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Abundance (number per ha)')), 
                        limits=c(0,30000), breaks=seq(0,30000, by=5000))+
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Abundance (number per ha)')), 
                         limits=c(0,30000), 
                         breaks=seq(0,30000, by=5000))+
  map_theme+
  theme(legend.box='horizontal', 
        legend.position = c(-91, 46.7))+
  guides(size=guide_legend(reverse=T, keywidth=5000), color=guide_legend(reverse=T, keywidth=5000))+
  labs(title='Lake Superior Chequamegon Bay Total Fish Abundance',
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

cbay_map1<-cbay_map1+geom_text(aes(x=-90.7, y=46.7, 
                               label=paste('Year:',YEAR, sep='\n')), 
                               size=8, family='serif')+
  transition_manual(YEAR)

cbay_map1_gif<-animate(cbay_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
cbay_map1_gif
anim_save(here('Plots and Tables/RVCAT','Animated_cbay_station_biomass_gif.gif'))


cbay_bars1<-ggplot(cbay.annual.sum, aes(x=YEAR, y=mean, fill=mean))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean abundance (number per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks())+
  transition_manual(YEAR, cumulative=T)

cbay_bars1_gif<-animate(cbay_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(cbay_map1_gif)
q_mgif<-image_read(cbay_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:45){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_cbay_ann_biomass_map_bars.gif'))


#####Age-1 analyses#########################################################################
############################################################################################
############################################################################################

##Lengths file to evaluate Age-1 densities##################################################
##pull out nearshore, TARGET = 2 data for a normal full survey year

ns<-subset(all.data, TARGET==2 & YEAR >1977)

##ns<-subset(all.data, TARGET==2 & YEAR >1973 &  M_UNIT == "WI2")

lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) 

age1.ns <- ns.trawls %>%
  select(OP_ID) %>% 
  left_join(lengths) %>% 
  subset(SPECIES == '109' & LENGTH <101 |
           SPECIES == '202' & LENGTH <141 |
           SPECIES == '204' & LENGTH <131 |
           SPECIES == '203' & LENGTH <161 |
           SPECIES == '317' & LENGTH <226) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) %>%
  full_join(ns.trawls) %>% 
  select(OP_ID,SPECIES,LENGTH,EXP_N) %>%
  group_by(OP_ID,SPECIES) %>%
  summarise(NUM = sum(EXP_N)) 

age1.ns.zeros <- age1.ns %>%
  pivot_wider(names_from = SPECIES, values_from = NUM) %>%
  select(c(1:6))  %>%
  pivot_longer(2:6, names_to = 'SPECIES', values_to = 'NUM') 

age1.ns.zeros$NUM[is.na(age1.ns.zeros$NUM)] <- 0 

age1.ns <- age1.ns.zeros %>% 
  left_join(ns.trawls) %>%
  mutate(NOHA=NUM/HA_SWEPT)


###################################################################################
##Offshore age-1 kiyi and siscowet
age1.os <- os.trawls %>%
  select(OP_ID) %>% 
  left_join(lengths) %>% 
  subset(SPECIES == 206 & LENGTH < 131 | SPECIES == 308 & LENGTH < 226) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) %>%
  full_join(os.trawls) %>% 
  select(OP_ID,SPECIES,LENGTH,EXP_N) %>%
  group_by(OP_ID,SPECIES) %>%
  summarise(NUM = sum(EXP_N)) 

age1.os.zeros <- age1.os %>%
  pivot_wider(names_from = SPECIES, values_from = NUM) %>%
  select(c(1:3))  %>%
  pivot_longer(2:3, names_to = 'SPECIES', values_to = 'NUM') 

age1.os.zeros$NUM[is.na(age1.os.zeros$NUM)] <- 0

age1.os <- age1.os.zeros %>% 
  left_join(os.trawls) %>%
  mutate(NOHA=NUM/HA_SWEPT)


############################################################################
##Combine nearshore and offshore fishes
age1 <- age1.ns %>%
  bind_rows(age1.os) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore"))) %>%
  left_join(sci.names)


#############################################################################
##calculate lakewide age1 density by YEAR, TARGET, SPECIES 
age1.annual.sum <- age1 %>% 
  group_by(YEAR, TARGET, COMMON_NAME) %>% 
  summarise(age1.mean = mean(NOHA)) %>%
  ungroup() %>%
  select(YEAR,COMMON_NAME, age1.mean) %>%
  mutate(FCO = ifelse(COMMON_NAME == 'Lake Whitefish' & age1.mean <4.5, "No",
                      ifelse(COMMON_NAME == 'Bloater' & age1.mean <10, "No", 
                      ifelse(COMMON_NAME == 'Cisco' & age1.mean <10, "No", "Yes"))))


#####################################################################################################
##Create age-1 summary table for export
age1.table<-age1.annual.sum %>%
  pivot_wider(names_from = COMMON_NAME, values_from = age1.mean) %>%
  mutate('Year class' = YEAR-1) %>%
  select(YEAR, 'Year class', c(2:9)) %>%
  distinct(YEAR, .keep_all = TRUE) %>%
  round(1) 

write.xlsx(age1.table, here('Plots and Tables/RVCAT','export_age1_annual_summary.xlsx'), row.names = F)



#################################################################################################
##Age -1 plots

##plot of recent age-1 ciscoe densities
#################################################################################################
age1.annual.sum.2020 <- age1.annual.sum %>%
  subset(YEAR<2020)


ggplot(subset(age1.annual.sum, COMMON_NAME=='Bloater'|COMMON_NAME=='Cisco'|COMMON_NAME=='Kiyi')) + 
  aes(x=YEAR-1, y = age1.mean) + 
  geom_point(size=6)+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  facet_grid(COMMON_NAME~., scales='free_y')+
  plot_theme+
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Ciscoe Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  theme(panel.spacing=unit(2,'lines'))+
  coord_cartesian(clip='off')+
  geom_hline(yintercept=0, color='black', size=1)

ggsave(here('Plots and Tables/RVCAT','ns_os_Age1_ciscoes.png'), dpi = 300, width = 40, height = 20, units = "cm")


##Plot of age-1 Cisco

ggplot(subset(age1.annual.sum.2020, COMMON_NAME == "Cisco")) + 
  aes(x=YEAR-1, y = age1.mean, fill = FCO) + 
  geom_hline(yintercept=0, size = 1, colour = 'black') +
  geom_hline(yintercept=10, size = 1.5, colour = 'sienna2') +
  geom_point(shape = 21, size=6)+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  geom_hline(yintercept=100, size = 1.5, colour = 'sienna2') +
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0.05, .1)))+
  plot_theme+
  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
  theme(legend.box='horizontal',
        legend.position = c(0.8, 0.8),
        legend.title.align=0.5) +
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
     title='Lake Superior Age-1 Cisco Abundance',
     subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','ns_Age1_cisco.png'), dpi = 300, width = 40, height = 20, units = "cm")


#################################################################################################
#################################################################################################
##Plot of age-1 Bloater

ggplot(subset(age1.annual.sum, COMMON_NAME == "Bloater")) + 
  aes(x=YEAR-1, y = age1.mean, fill = FCO) + 
  geom_point(shape = 21, size=6)+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  geom_hline(yintercept=10, size = 1.5, colour = 'sienna2') +
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  plot_theme+
  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
  theme(legend.box='horizontal',
        legend.position = c(0.8, 0.8), 
        legend.title.align=0.5) +
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Bloater Abundance',
       subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','ns_age1_bloater.png'), dpi = 300, width = 40, height = 20, units = "cm")

#################################################################################################
##Plot of age-1 Lake Whitefish

ggplot(subset(age1.annual.sum.2020, COMMON_NAME == "Lake Whitefish")) + 
  aes(x=YEAR-1, y = age1.mean, fill = FCO) + 
  geom_hline(yintercept=4.5, size = 1.5, colour = 'sienna2') +
  geom_point(shape = 21, size=6)+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  plot_theme+
  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
  theme(legend.box='horizontal',
        legend.position = c(0.8, 0.8), 
        legend.title.align=0.5) +
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Lake Whitefish Abundance',
       subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','ns_age1_lwf.png'), dpi = 300, width = 40, height = 20, units = "cm")

#################################################################################################
##Lean and Siscowet Age-1 densities

ggplot(subset(age1.annual.sum, COMMON_NAME=='lean Lake Trout'| 
                COMMON_NAME=='siscowet Lake Trout')) + 
  aes(x=YEAR-1, y = age1.mean) + 
  geom_point(size=6)+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  facet_grid(COMMON_NAME~., scales='free_y')+
  plot_theme+
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Lake Trout Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  theme(panel.spacing=unit(2,'lines'))+
  coord_cartesian(clip='off')+
  geom_hline(yintercept=0, color='black', size=1)

ggsave(here('Plots and Tables/RVCAT','ns_os_age1_laketrout.png'), dpi = 300, width = 40, height = 20, units = "cm")


#################################################################################################
#################################################################################################
##map of age-1 cisco and bloater densities

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

age1.cisco.map <- ggplot(subset(age1, COMMON_NAME == "Cisco")) + 
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_point(subset(age1, COMMON_NAME == "Cisco"), mapping=aes(Mid.Long.DD, Mid.Lat.DD, 
               color=log(NOHA), size=log(NOHA)))+ 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(age1, COMMON_NAME == "Cisco"), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=NOHA, color=NOHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('log(number per ha)')), 
                        limits=c(0,5), breaks=seq(0,5, by=2))+
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('log(number per ha)')), 
                         limits=c(0,5), breaks=seq(0,5, by=2))+
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal',
        legend.position = c(0.2, 0.8))+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Nearshore Age-1 Cisco Abundance',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

age1.cisco.map<-age1.cisco.map+geom_text(aes(x=-85, y=48.75, 
                label=paste('Year class:',YEAR-1, sep='\n')), size=7, family='serif')+
  transition_manual(YEAR)

age1.cisco.map_gif<-animate(age1.cisco.map, fps = 3, end_pause = 10, 
                            width = 1000, height = 500,renderer = gifski_renderer(loop=F))
age1.cisco.map_gif
anim_save(here('Plots and Tables/RVCAT','Animated_ns_cisco_age1_map.gif'))


age1.cisco.map_bars1<-ggplot(subset(age1.annual.sum, COMMON_NAME == "Cisco")) +
  aes(x=YEAR-1, y=age1.mean, fill=age1.mean)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme+
  theme(legend.position = "none") +
  labs(x='Year class', y='Abundance (number per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  transition_manual(YEAR, cumulative=T)

age1.cisco.map_bars1_gif<-animate(age1.cisco.map_bars1, fps = 3, 
              end_pause = 10, width = 350, height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(age1.cisco.map_gif)
q_mgif<-image_read(age1.cisco.map_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:43){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_ns_cisco_age1_map_bars.gif'))


#############################################################################################
#############################################################################################
##Occurrence of age-1 cisco, bloater, kiyi

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##create map with stations colored by age-1 cisco presence - absence
age1.pa <- age1 %>%
  mutate(present = ifelse(NOHA > 0, "Present", "Absent")) %>%
  mutate(station = as.factor(LOCATION))

##Current year Present- absence map  
ggplot(subset(age1.pa, 
              COMMON_NAME=='Cisco' & YEAR == max(age1$YEAR) |
              COMMON_NAME=='Bloater' & YEAR == max(age1$YEAR) |
              COMMON_NAME=='Kiyi' & YEAR == max(age1$YEAR)
              )) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(age1.pa, 
                    COMMON_NAME=='Cisco' & YEAR == max(age1$YEAR) |
                    COMMON_NAME=='Bloater' & YEAR == max(age1$YEAR) |
                    COMMON_NAME=='Kiyi' & YEAR == max(age1$YEAR)
                    ),
  mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=present), size=6, stroke=1.5)+
  scale_color_manual(values=c('gray85','red'), name='Age-1 occurrence')+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-1 Ciscoe Occurrence',
       subtitle=(paste('USGS bottom trawl assessment,', max(age1.pa$YEAR))), 
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','ns_age1_sites_present_absent_map.png'), 
       dpi = 300, width = 40, height = 20, units = "cm")


####
a1.map.inset<-ggplot(subset(age1.pa, 
                   COMMON_NAME=='Cisco' & YEAR == max(age1$YEAR) |
                   COMMON_NAME=='Bloater' & YEAR == max(age1$YEAR) |
                   COMMON_NAME=='Kiyi' & YEAR == max(age1$YEAR)
                   )) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(age1.pa, 
                    COMMON_NAME=='Cisco' & YEAR == max(age1$YEAR) |
                    COMMON_NAME=='Bloater' & YEAR == max(age1$YEAR) |
                    COMMON_NAME=='Kiyi' & YEAR == max(age1$YEAR)
                    ),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=present), size=6, stroke=1.5)+
  scale_color_manual(values=c('gray85','red'), name='Age-1 occurrence')+
  map_theme+
  geom_text(aes(label=LOCATION))

####stacked bar plot for age-1 bloater, ciscoe, kiyi
ggplot(subset(age1.pa, 
              COMMON_NAME=='Cisco' & YEAR == max(age1$YEAR) |
              COMMON_NAME=='Bloater' & YEAR == max(age1$YEAR) |
              COMMON_NAME=='Kiyi' & YEAR == max(age1$YEAR) |
              COMMON_NAME=='Lake Whitefish' & YEAR == max(age1$YEAR)   
              )) +
  aes(x=reorder(LOCATION, NOHA), y = NOHA, fill=COMMON_NAME) + 
#  aes(x=LOCATION, y = NOHA, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
##  aes(x=fct_reorder(station))+
## annotation_custom(ggplotGrob(a1.map.inset), xmin=20, xmax=100, ymin=35, ymax=240)+
  plot_theme+
  theme(legend.position='bottom',
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=12))+
  labs(x='Station', 
       y='Abundance (number per ha)',
       title='Lake Superior Age-1 Ciscoe Occurrence',
       subtitle=(paste('USGS bottom trawl assessment,', max(age1.pa$YEAR))), 
       caption=ann_data_access) +
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'deepskyblue2', 'lightpink3'))+
  theme(legend.position = "none")
#  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_age1_ciscoes_bydate.png'), height=20, width=40, dpi=300, units='cm')


#########################################################################################################
##Pie map of age-1 bloater, cisco, and kiyi  #########
#########################################################################################################
#################################################################################################
age1.pie <- age1 %>%
  select(OP_ID,YEAR,LOCATION,COMMON_NAME,NOHA,Mid.Lat.DD, Mid.Long.DD) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = NOHA, values_fill = 0) %>%
  subset(Mid.Lat.DD >0) 

##%>%
  ##subset(YEAR>1988)

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


##Age-1 pie map for bloater, cisco, kiyi for current year
ggplot(subset(age1.pie, YEAR == max(age1.pie$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  scale_x_continuous(breaks = pretty_breaks(), limits = c(-91.2, -90.2), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(46.5, 47.2), name="Latitude")+
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(age1.pie, YEAR == max(age1.pie$YEAR)),
                  cols= c("Bloater", "Cisco", "Kiyi", 'Lake Whitefish'),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'deepskyblue2', 'lightpink3'))+
#  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
        title='Lake Superior Age-1 Ciscoe Distributions',
        subtitle=(paste('USGS bottom trawl assessment,', max(age1.pa$YEAR))), 
        caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','CurrentYear_age1_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')

####################################################################################
##Animated figure showing each year at a time
age1.pie.map1<-ggplot(age1.pie, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=age1.pie,
                  cols= c("Bloater", "Cisco", "Kiyi"),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-1 Ciscoe Distributions',
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)


  transition_manual(YEAR-1)

age1.pie.map2<-age1.pie.map1+geom_text(aes(x=-85, y=48.75, 
            label=paste('Year class:', YEAR-1, sep='\n')), size=8, family='serif') +
  transition_manual(YEAR-1)

a1p_gif<-animate(age1.pie.map2, fps = .5, end_pause = 15, nframes=57, ##NOTE: as more years are added, need to increase nframes to the # years
                 width = 900, height = 500, renderer = gifski_renderer(loop=T))
a1p_gif
anim_save(here('Plots and Tables/RVCAT','Animated_age1_pies.gif')) ##saves just map, before adding other plots


####################################################################################
####################################################################################
##lollipop 5, 10, 20 year means comparisons#########################################

##calculate 5, 10, and 20 year means for each species
ns.spp5mean<-ns.annual.sum.by.species %>%
  subset(YEAR>=max(YEAR)-4) %>% 
  subset(COMMON_NAME == "Cisco" | 
           COMMON_NAME == "Bloater" |
           COMMON_NAME == "Longnose Sucker" |
           COMMON_NAME == "Ninespine Stickleback" |
           COMMON_NAME == "Lake Whitefish" | 
           COMMON_NAME == "Pygmy Whitefish" | 
           COMMON_NAME == "Rainbow Smelt" | 
           COMMON_NAME == "Burbot" | 
           COMMON_NAME == "Slimy Sculpin" | 
           COMMON_NAME == "Spoonhead Sculpin" | 
           COMMON_NAME == "Deepwater Sculpin" | 
           COMMON_NAME == "Trout-Perch") %>%
  group_by(COMMON_NAME) %>%
  summarise(mean = mean(mean)) %>%
  mutate(period = "5 year mean")

ns.spp10mean<-ns.annual.sum.by.species %>%
  subset(YEAR>=max(YEAR)-9) %>% 
  subset(COMMON_NAME == "Cisco" | 
           COMMON_NAME == "Bloater" |
           COMMON_NAME == "Longnose Sucker" |
           COMMON_NAME == "Ninespine Stickleback" |
           COMMON_NAME == "Lake Whitefish" | 
           COMMON_NAME == "Pygmy Whitefish" | 
           COMMON_NAME == "Rainbow Smelt" | 
           COMMON_NAME == "Burbot" | 
           COMMON_NAME == "Slimy Sculpin" | 
           COMMON_NAME == "Spoonhead Sculpin" | 
           COMMON_NAME == "Deepwater Sculpin" | 
           COMMON_NAME == "Trout-Perch") %>%
  group_by(COMMON_NAME) %>%
  summarise(mean = mean(mean)) %>%
  mutate(period = "10 year mean")

ns.spp20mean<-ns.annual.sum.by.species %>%
  subset(YEAR>=max(YEAR)-19) %>% 
  subset(COMMON_NAME == "Cisco" | 
           COMMON_NAME == "Bloater" |
           COMMON_NAME == "Longnose Sucker" |
           COMMON_NAME == "Ninespine Stickleback" |
           COMMON_NAME == "Lake Whitefish" | 
           COMMON_NAME == "Pygmy Whitefish" | 
           COMMON_NAME == "Rainbow Smelt" | 
           COMMON_NAME == "Burbot" | 
           COMMON_NAME == "Slimy Sculpin" | 
           COMMON_NAME == "Spoonhead Sculpin" | 
           COMMON_NAME == "Deepwater Sculpin" | 
           COMMON_NAME == "Trout-Perch") %>%
  group_by(COMMON_NAME) %>%
  summarise(mean = mean(mean)) %>%
  mutate(period = "20 year mean")

ns.period.means<-bind_rows(ns.spp5mean, ns.spp10mean, ns.spp20mean)


##calculate the mean for the last two years of sampling that will be used to compare to the 5, 10, and 20 year means
ns.spp2mean<-ns.annual.sum.by.species %>%
  subset(YEAR>=max(YEAR)-2) %>% 
  subset(COMMON_NAME == "Cisco" | 
           COMMON_NAME == "Bloater" |
           COMMON_NAME == "Longnose Sucker" |
           COMMON_NAME == "Ninespine Stickleback" |
           COMMON_NAME == "Lake Whitefish" | 
           COMMON_NAME == "Pygmy Whitefish" | 
           COMMON_NAME == "Rainbow Smelt" | 
           COMMON_NAME == "Burbot" | 
           COMMON_NAME == "Slimy Sculpin" | 
           COMMON_NAME == "Spoonhead Sculpin" | 
           COMMON_NAME == "Deepwater Sculpin" | 
           COMMON_NAME == "Trout-Perch") %>%
  group_by(COMMON_NAME) %>%
  summarise(mean2yr = mean(mean))

ns.period.means <- ns.period.means %>%
  left_join(ns.spp2mean)


##percent change calculations
ns.period.means <- ns.period.means %>%
  mutate(percentchange = ((mean2yr/mean)*100)-100) %>%
  mutate(direction=if_else(percentchange>0,'positive','negative')) %>%
  mutate(period_f=factor(period, levels=c('20 year mean','10 year mean','5 year mean')))


##plot
ggplot(ns.period.means, aes(x=percentchange, y=COMMON_NAME, fill=direction))+
  geom_segment(aes(x=0, xend=percentchange, y=COMMON_NAME, yend=COMMON_NAME))+
  geom_point(size=8, shape=21)+
  scale_fill_manual(values=c('red', 'deepskyblue2'), guide=F)+
  scale_y_discrete(limits=c('Longnose Sucker', 'Ninespine Stickleback','Trout-perch',
                            'Spoonhead Sculpin','Slimy Sculpin','Deepwater Sculpin',
                            'Burbot', 'Rainbow Smelt',
                            'Pygmy Whitefish','Lake Whitefish','Bloater','Cisco'))+
  plot_theme+
  facet_grid(.~period_f)+
  geom_vline(xintercept=0, size=2, color='black')+
  labs(x='Percent change in 2019-20 mean biomass (kg per ha) as compared to previous time periods', y='', 
       title='Lake Superior Nearshore Fish Biomass Trends',
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','ns_periods_comparison.png'), dpi = 300, width = 40, height = 20, units = "cm")


########################################################################################
##Animated map of sites sampled in the current year and corresponding fish biomass and diversity#####################################
########################################################################################

animate.map.catch <- all.data%>%
##  subset(YEAR >=2009) %>%
  subset(YEAR==max(YEAR-1)) %>%
  subset(TARGET==2|TARGET==117|TARGET==118) %>%
         #|TARGET ==106) %>%
  select(OP_ID, OP_DATE, TIME, YEAR, TARGET, LOCATION, Mid.Long.DD, Mid.Lat.DD, SPECIES, NUM, WT, NOHA, KGHA) %>%
  arrange(OP_DATE, TIME)

animate.map.species <-animate.map.catch[match(unique(animate.map.catch$SPECIES), animate.map.catch$SPECIES),]
animate.map.species <-aggregate(animate.map.species$SPECIES, by=list(OP_ID=animate.map.species$OP_ID), FUN=length)%>%
  renameCol('x','New.Spp') 

animate.map.catch.sum <- animate.map.catch %>%
  group_by(OP_ID) %>%
  mutate(Catch_spp = n(), 
         Catch.num = sum(NUM), 
         Catch.wt = sum(WT), 
         Catch.NOHA = sum(NOHA), 
         Catch.KGHA = sum(KGHA)) %>%
  ungroup() %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  arrange(OP_DATE, TIME) %>%
  left_join(animate.map.species) 

animate.map.catch.sum$New.Spp[is.na(animate.map.catch.sum$New.Spp)] <- 0 
  
animate.map.catch.sum <- animate.map.catch.sum %>%
  mutate(cumCatch.spp = cumsum(lag(New.Spp,  default = 0)),
         cumCatch.num = cumsum(lag(Catch.num, default = 0)),
         cumCatch.wt = cumsum(lag(Catch.wt,  default = 0))) %>% 
  mutate(Order = 1:nrow(animate.map.catch.sum)) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                          "118" = "offshore", "106" = "Chequamegon Bay"))) %>%
  mutate(Fish.Caught = 'Fish Caught')



ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

p<-ggplot(animate.map.catch.sum, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ ##path for lake outline
  geom_path(data=animate.map.catch.sum, mapping=aes(Mid.Long.DD, Mid.Lat.DD), size=1)+ ##path for lines connecting sites
  geom_point(aes(group=seq_along(Order), color=survey), size=6, alpha=0.6)+ 
  theme_bw() + 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  map_theme +
#  scale_color_viridis(discrete=T, labels=c('Chequamegon Bay', 'Nearshore Cruise', 'Offshore Cruise'), 
  scale_color_viridis(discrete=T, labels=c('Nearshore Cruise', 'Offshore Cruise'), 
                      name='', begin=0,end=0.6, alpha = 0.6) +
  labs(title='Lake Superior Fish Collections',
       subtitle=(paste('USGS bottom trawl assessment,', max(animate.map.catch.sum$YEAR))), 
##       subtitle='USGS bottom trawl assessment, 2009-2020', 
       caption=ann_data_access)+ ##change title to current year
  transition_reveal(Order)+
  theme(legend.position=c(0.1,0.9),
        legend.title = element_blank())

r<-p+geom_text(aes(x=-85, y=49, label=OP_DATE), size=8, family='serif')+
  transition_reveal(Order)

p_gif<-animate(r, fps = 5, end_pause = 60, nframes=150, 
               width = 1000, height = 500, renderer = gifski_renderer(loop=T))

anim_save(here('Plots and Tables/RVCAT','Animated_CurrentYear_map.gif')) ##saves just map, before adding other plots


##########################################################################
## Cummulative number of individuals collected

anim_bar1 <- ggplot(data=animate.map.catch.sum) +
  geom_tile(aes(x = Fish.Caught, y = (cumCatch.num + Catch.num/2),
            height = Catch.num,
            fill = survey), width = 0.9) +
  scale_fill_viridis(discrete=T, begin = 0,end = 0.6, alpha = 0.6)+
  scale_y_continuous(labels = scales::comma, 
                     expand=c(0,0), 
                     breaks = pretty_breaks(5)) +
  scale_x_discrete(expand=c(0,0), labels=c('Fish\nCaught'))+
  theme(legend.position = "none")+
  plot_theme+
  labs(x='',y='Running total: fish caught',title=' ')+
  transition_time(Order)+
  shadow_mark(past=T, future=F)

#animate(anim_bar, fps = 20)
q_gif<-animate(anim_bar1, fps = 5, end_pause = 60, nframes=150, 
               width = 200, height = 500, renderer = gifski_renderer(loop=T))  

##anim_save(here('Plots and Tables/RVCAT','Animated_CurrentYear_CatchPlot.gif')) ##saves just map, before adding other plots

q_gif

##########################################################################
## Cummulative number of new species collected

anim_bar2 <- ggplot(data=animate.map.catch.sum) +
  geom_tile(aes(x = Fish.Caught, y = (cumCatch.spp + New.Spp/2),
                height = New.Spp,
                fill = survey), width = 0.9) +
  scale_fill_viridis(discrete=T, begin = 0,end = 0.6, alpha = 0.6)+
  scale_y_continuous(expand=c(0,0), 
                     breaks = pretty_breaks(5)) +
  scale_x_discrete(expand=c(0,0), labels=c('Species\nCaught'))+
  theme(legend.position = "none")+
  plot_theme+
  labs(x='',y='Running total: unique species caught',title=' ')+
  transition_time(Order)+
  shadow_mark(past=T, future=F)


#animate(anim_bar, fps = 20)
u_gif<-animate(anim_bar2, fps = 5, end_pause = 60, nframes=150, 
               width = 150, height = 500, renderer = gifski_renderer(loop=T)) 

##anim_save(here('Plots and Tables/RVCAT','Animated_CurrentYear_SpeciesPlot.gif')) ##saves just map, before adding other plots

u_mgif<-image_read(u_gif)
p_mgif<-image_read(p_gif)
q_mgif<-image_read(q_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1], u_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(p_mgif[i], q_mgif[i], u_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif,here('Plots and Tables/RVCAT','Animated_CurrentYear_Catch_map.gif'))


############################################################################################
##animate bar plots of cumulative annual mean biomass for nearshore and offshore cruises----
animate.map.catch.sum.ns <- animate.map.catch.sum %>%
  subset(survey == 'nearshore') %>%
  arrange(OP_DATE, TIME) %>%
  mutate(cummean.KGHA = cummean(Catch.KGHA))

animate.map.catch.sum.os <- animate.map.catch.sum %>%
  subset(survey == 'offshore') %>%
  arrange(OP_DATE, TIME) %>%
  mutate(cummean.KGHA = cummean(Catch.KGHA))

animate.map.catch.mean <- animate.map.catch.sum.ns %>%
  bind_rows(animate.map.catch.sum.os) %>%
  mutate(Order2 = Order)

r<-ggplot(animate.map.catch.mean, aes(x = survey, y = cummean.KGHA)) +
  geom_jitter(data=animate.map.catch.mean, aes(x=survey, y= Catch.KGHA), 
            fill = 'black', shape = 21, size = 8) +
  geom_tile(data = animate.map.catch.mean, aes(y = cummean.KGHA / 2, height = cummean.KGHA, 
            fill=survey), 
            width = 0.9, alpha = 0.6) +
  scale_fill_viridis(discrete=T, begin=0,end=0.6, alpha =0.6)+
  theme(legend.position = "none")+
  plot_theme+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), 
                     breaks = pretty_breaks()) +
  labs( x='', y='Lakewide mean biomass (kg per ha)', title='  ', caption='  ') +
  transition_reveal(Order) +
  shadow_wake(0.8)
  
r_gif<-animate(r, nframes = 150, fps = 2, end_pause = 60, width = 500, height = 525)

r_gif

anim_save(here('Plots and Tables/RVCAT','Animated_CurrentYear_MeanBar.gif')) ##saves just barplot, before adding map

p<-ggplot(animate.map.catch.mean, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_path(data=animate.map.catch.mean, mapping=aes(Mid.Long.DD, Mid.Lat.DD), size=1) + 
  geom_point(aes(group=seq_along(Order), color= survey), size=6, alpha=0.6) + 
  theme_bw() + 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  map_theme +
  scale_color_viridis(discrete=T, labels=c('Nearshore Cruise', 'Offshore Cruise'), 
                      name='', begin=0,end=0.6, alpha = 0.6) +
  labs(title='Lake Superior Fish Collections',
       subtitle=(paste('USGS bottom trawl assessment,', max(animate.map.catch.mean$YEAR))), 
       caption=ann_data_access)+ 
  transition_reveal(Order)+
  theme(legend.title = element_blank(), 
        legend.position = "none")

t<-p+geom_text(aes(x=-85, y=49, label=OP_DATE), size=8, family='serif')+
  transition_reveal(Order)

t_gif<-animate(t, fps = 2, end_pause = 60, nframes=150, 
               width = 1000, height = 500, renderer = gifski_renderer(loop=T))

r_mgif<-image_read(r_gif)
t_mgif<-image_read(t_gif)

new_gif<-image_append(c(t_mgif[1], r_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(t_mgif[i], r_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif,here('Plots and Tables/RVCAT','Animated_ns_os_cumulating_biomass.gif'))  

########################################################################################
##animate bar plots of cumulative annual mean biomass for nearshore survey
##Bar chart race, ns biomass--------------------------------------------------------------------------------------------------------------------------

ns.bcr <- export.annual.sum.by.species %>%
  subset(survey == 'nearshore') %>%
  ungroup() %>%
  select(YEAR, COMMON_NAME, mean.kg) %>%
  subset(COMMON_NAME == 'Rainbow Smelt' |
           COMMON_NAME == 'Cisco' |
           COMMON_NAME == 'Bloater' |
           COMMON_NAME == 'Lake Whitefish' |
           COMMON_NAME == 'lean Lake Trout' |
           COMMON_NAME == 'hatchery Lake Trout' |
           COMMON_NAME == 'Burbot' |
           COMMON_NAME == 'Slimy Sculpin' |
           COMMON_NAME == 'Spoonhead Sculpin' |
           COMMON_NAME == 'Deepwater Sculpin')  %>%
  group_by(YEAR) %>% 
  mutate(
    rank = min_rank(-mean.kg) * 1,
    Value_rel = mean.kg / mean.kg[rank == 1],
    Value_lbl = paste0(" ", mean.kg)
  ) %>%
  subset(rank <= 10) %>% 
  ungroup() %>%
  arrange(YEAR, mean.kg) 

p<-ggplot(ns.bcr, aes(rank, group = COMMON_NAME))+ 
  geom_tile(aes(y=mean.kg/2, height=mean.kg, width=0.9, fill=COMMON_NAME))+
  geom_text(aes(y=0, label=paste(COMMON_NAME, "  ")), size=5, vjust=0.2, hjust=1)+
#  geom_text(aes(y=mean.kg, label = Value_lbl), size = 5, hjust=0) +
  coord_flip(clip='off', expand=F)+
  scale_y_continuous(expand=c(0,0), 
                     breaks = pretty_breaks()) +
  scale_x_reverse() +
  scale_fill_viridis(discrete=T, guide=F)+  
  labs(title='Lake Superior Annual Nearshore Fish Biomass', y='Mean biomass (kg/hectare)', x=' ',
       subtitle='{round(frame_time,0)}',
       caption=ann_data_access)+ 
  plot_theme+
  theme(plot.margin = margin(1,1,1,6, 'cm'),
        panel.background = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title=element_text(size=20),
        axis.text.x=element_text(size=18, color='black'),
        axis.title.x=element_text(size=18, color='black'),
        axis.line=element_line(size=1),
        plot.subtitle = element_text(size=18),
        plot.caption=element_text(size=15), 
        legend.position = "none") +
  transition_time(YEAR)+
  ease_aes('cubic-in-out')

animate(p, fps = 4, nframes=200, width = 1024, height = 512, 
        renderer=gifski_renderer(loop=T), end_pause=20) ##nframes =n years x2-1

anim_save(here("Plots and Tables/RVCAT",'Animated_ns_bar_race.gif'))

########################################################################################
########################################################################################
##   Sample Density Distribution plots of species depths and temperatures   #########
########################################################################################

depths<-subset(all.data, YEAR >= 2011) %>% 
  distinct(OP_ID, .keep_all = TRUE) %>% 
  select(OP_ID,YEAR,SERIAL,TARGET,TR_DESIGN, LOCATION,Mid.Lat.DD, Mid.Long.DD, 
         BEG_DEPTH, END_DEPTH, FISHING_DEPTH, Surface.Temp, Bottom.Temp) %>% 
  mutate(Range.Depth = abs(END_DEPTH-BEG_DEPTH), 
         trawl = case_when(
           TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
             TR_DESIGN == 27 | TR_DESIGN == 44 ~ "Bottom trawl", 
           TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
             TR_DESIGN == 45  ~ "Mid-water trawl")) %>%
  filter(Range.Depth<=10) 
    
dslimy<-subset(all.data, SPECIES==902) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Slimy Sculpin')

dspoon<-subset(all.data, SPECIES==903) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Spoonhead Sculpin')

ddeep<-subset(all.data, SPECIES==904) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Deepwater Sculpin')

dcisco<-subset(all.data, SPECIES==202) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Cisco')

dbloater<-subset(all.data, SPECIES==204) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Bloater')

dkiyi<-subset(all.data, SPECIES==206) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Kiyi')

dbfc<-subset(all.data, SPECIES==207) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Blackfin Cisco')

dsjc<-subset(all.data, SPECIES==208) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Shortjaw Cisco')

dlwf<-subset(all.data, SPECIES==203) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Lake Whitefish')

dpwf<-subset(all.data, SPECIES==211) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Pygmy Whitefish')

dslt<-subset(all.data, SPECIES==308) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Siscowet Lake Trout')

dllt<-subset(all.data, SPECIES==317) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Lean Lake Trout')

drbs<-subset(all.data, SPECIES==109) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Rainbow Smelt')

dnss<-subset(all.data, SPECIES==130) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Ninespine Stickleback')

dtp<-subset(all.data, SPECIES==131) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Trout-Perch')

dlns<-subset(all.data, SPECIES==404) %>% 
  select(OP_ID, NOHA) %>%
  renameCol('NOHA','Longnose Sucker')

dfishy1<-left_join(depths, dslimy) 
dfishy1<-left_join(dfishy1, dspoon) 
dfishy1<-left_join(dfishy1, ddeep) 
dfishy1<-left_join(dfishy1, dcisco) 
dfishy1<-left_join(dfishy1, dbloater) 
dfishy1<-left_join(dfishy1, dkiyi) 
dfishy1<-left_join(dfishy1, dbfc) 
dfishy1<-left_join(dfishy1, dsjc) 
dfishy1<-left_join(dfishy1, dlwf) 
dfishy1<-left_join(dfishy1, dpwf) 
dfishy1<-left_join(dfishy1, dslt) 
dfishy1<-left_join(dfishy1, dllt) 
dfishy1<-left_join(dfishy1, drbs) 
dfishy1<-left_join(dfishy1, dnss) 
dfishy1<-left_join(dfishy1, dtp) 
dfishy1<-left_join(dfishy1, dlns) 

dfishy1[is.na(dfishy1)] <- 0 

dfishy1<-pivot_longer(dfishy1, 16:31, names_to = "FISH", values_to = "Fdensity")

dfishy1$avgdepth = rowMeans(dfishy1[,c("BEG_DEPTH", "END_DEPTH")], na.rm = TRUE)

##abundance weighting 
dfishy1 <- dfishy1 %>%
  group_by(FISH) %>%
  mutate(Fdensity.grp = sum(Fdensity)) %>%
  ungroup()

##Get mean depths
dfishy1.sum <- dfishy1 %>%
  group_by(FISH, trawl) %>%
  summarise(cdepth.mean=sum(FISHING_DEPTH*Fdensity)/sum(Fdensity),
            bdepth.mean=sum(avgdepth*Fdensity)/sum(Fdensity), 
            temp.mean=sum(Bottom.Temp*Fdensity)/sum(Fdensity)) %>%
  ungroup()


###########################################################################################  
##Single Species Depth plot for bottom trawl

ggplot(subset(dfishy1, FISH == "Kiyi" & trawl == "Bottom trawl")) +
  aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp) + 
  geom_density(alpha=0.4, fill = 'deepskyblue2') +
  geom_vline(data = subset(dfishy1.sum, FISH == "Kiyi" & trawl == "Bottom trawl"),
             aes(xintercept=bdepth.mean), color='black', size=1.2, show.legend = FALSE) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  labs( x='Fish depth of capture (m)', y='Relative density',
        title='Lake Superior Kiyi Depth Distribution',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','kiyi_BottomTrawlCaptures.png'), height=20, width=40, dpi=300, units='cm')


#######################################################################################
##Single species showing midwater and bottom trawl catches

ggplot(subset(dfishy1, FISH == "Kiyi")) +
  aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=trawl, fill=trawl) + 
  geom_density(alpha=0.4) +
  geom_vline(data = subset(dfishy1.sum, FISH == "Kiyi" & trawl == "Mid-water trawl"),
             aes(xintercept=cdepth.mean), color= "black", size=1.2, show.legend = FALSE) +
  geom_vline(data = subset(dfishy1.sum, FISH == "Kiyi" & trawl == "Bottom trawl"),
             aes(xintercept=bdepth.mean), color='black', size=1.2, show.legend = FALSE) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.position = c(.8, .8), 
        legend.title=element_blank()) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Fish depth of capture (m)', y='Relative density',
        title='Lake Superior Kiyi Depth Distribution',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','kiyi_CaptureDepths_byTrawl.png'), height=20, width=40, dpi=300, units='cm')

#######################################################################################
##Multi species showing midwater and bottom trawl catches
##Get mean depths
ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi")), 
       aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position = c(.9, .8), 
        legend.title=element_blank()) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Fish depth of capture (m)', y='Relative density',
        title='Lake Superior Ciscoe Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  facet_grid(.~trawl, scales="free")

ggsave(here('Plots and Tables/RVCAT','ciscoe_CaptureDepths_byTrawl.png'), height=20, width=40, dpi=300, units='cm')

#####################
ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi", 
                                   "Rainbow Smelt")), 
       aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position = c(.9, .8), 
        legend.title=element_blank()) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Fish depth of capture (m)', y='Relative density',
        title='Lake Superior Ciscoe and Rainbow Smelt Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  facet_grid(.~trawl, scales="free" )

ggsave(here('Plots and Tables/RVCAT','ciscoe_RBS_CaptureDepths_byTrawl.png'), height=20, width=40, dpi=300, units='cm')
###########################################################################################  

###########################################################################################  
##Single Species Temperature plot

ggplot(subset(dfishy1, FISH == "Kiyi" & trawl == "Bottom trawl")) +
  aes(x=Bottom.Temp, y = ..scaled.., weight=Fdensity/Fdensity.grp) + 
  geom_density(alpha=0.4, fill = 'deepskyblue2') +
  geom_vline(data = subset(dfishy1.sum, FISH == "Kiyi" & trawl == "Bottom trawl"),
             aes(xintercept=temp.mean), color='black', size=1.2, show.legend = FALSE) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0), limits = c(0, 25), breaks=c(0, 5, 10, 15, 20, 25), 
                     labels=c('0', '5', '10', '15', '20', '25'))+
  scale_y_continuous(expand=c(0,0))+
  labs( x='Fish temperature of capture (m)', y='Relative density',
        title='Lake Superior Kiyi Temperature Distribution',
        subtitle=' ',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/RVCAT','kiyi_BottomTrawlCaptures.png'), height=20, width=40, dpi=300, units='cm')

##Multi-species temperature plot
ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi")), 
       aes(x=Bottom.Temp, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.8)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 25), breaks=c(0, 5, 10, 15, 20, 25), 
                     labels=c('0', '5', '10', '15', '20', '25'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Capture Temperature (C)', y='Relative density',
        title='Lake Superior Ciscoe Temperature Distribution',
        subtitle=' ',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/RVCAT','ciscoe_temps.png'), height=20, width=40, dpi=300, units='cm')

###########################################################################################  
##Ciscoe capture depths

ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi")), 
       aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Capture depth (m)', y='Relative density',
      title='Lake Superior Ciscoe Depth Distributions',
      subtitle=' ',
      caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','ciscoe_cdepths.png'), height=20, width=40, dpi=300, units='cm')

####Cisco and Kiyi capture depths
ggplot(subset(dfishy1, FISH %in% c("Cisco", "Kiyi")), 
       aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Capture depth (m)', y='Relative density',
        title='Lake Superior Cisco and Kiyi Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','cisco_kiyi_cdepths.png'), height=20, width=40, dpi=300, units='cm')

################################################################################################
##Ciscoe and RBS bathymetric depth

ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi")), 
       aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Bathymetric depth (m)', y='Relative density',
        title='Lake Superior Ciscoe Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','ciscoe_bathydepths.png'), height=20, width=40, dpi=300, units='cm')


###########################################################################################  
##Ciscoe and RBS depths of capture

ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi", "Rainbow Smelt")), 
       aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Depth of capture (m)', y='Relative density',
        title='Lake Superior Ciscoe and Rainbow Smelt Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','ciscoe_RBS_capturedepths.png'), height=20, width=40, dpi=300, units='cm')


###########################################################################################  
##Sculpin depths

ggplot(subset(dfishy1, FISH %in% c("Slimy Sculpin", "Spoonhead Sculpin", "Deepwater Sculpin")), 
       aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Bathymetric depth (m)', y='Relative density',
        title='Lake Superior Sculpin Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','sculpin_depths.png'), height=20, width=40, dpi=300, units='cm')


##############################################################################################
##Lake Whitefish and Pygmy Whitefish depths

ggplot(subset(dfishy1, FISH %in% c("Pygmy Whitefish", "Lake Whitefish")), 
       aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Bathymetric depth (m)', y='Relative density',
        title='Lake Superior Lake and Pygmy Whitefish Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','lwf_pwf_depths.png'), height=20, width=40, dpi=300, units='cm')


##############################################################################################
##Lake Trout depths

ggplot(subset(dfishy1, FISH %in% c("Lean Lake Trout", "Siscowet Lake Trout")), 
       aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Bathymetric depth (m)', y='Relative density',
        title='Lake Superior Lake Trout Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','laketrout_depths.png'), height=20, width=40, dpi=300, units='cm')

##Ninespine Stickleback and Trout Perch depths

ggplot(subset(dfishy1, FISH %in% c("Ninespine Stickleback", "Trout-Perch")), 
       aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank()) +
  theme(legend.position=c(0.8,0.72)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Bathymetric depth (m)', y='Relative density',
        title='Lake Superior Ninespine Stickleback and Trout-Perch Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','9spineTP_depths.png'), height=20, width=40, dpi=300, units='cm')

########################################################################################
########################################################################################
####################################################################################
##Sankey Diagram Nearshore Offshore Fish Collections#########################################
## USING network3D Package
####################################################################################

library(networkD3)
library(htmltools)

##Based on number of individuals
current.yr.all<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)
current.yr.all$TARGET_f<-as.factor(current.yr.all$TARGET)

##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))

spp.counts<-aggregate(current.yr.all$NUM, by=list(SPECIES=current.yr.all$SPECIES, TARGET=current.yr.all$TARGET), FUN=sum)%>%
  renameCol('x','SUM')
spp.counts<-merge.data.frame(spp.counts, sci.names)
spp.counts<-select(spp.counts, c(4,5,2,3))
spp.counts2<-cast(spp.counts, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
spp.counts2<-spp.counts2%>%
  renameCol('COMMON_NAME','Fish')%>%
  renameCol('SCIENTIFIC_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
spp.counts2[is.na(spp.counts2)]<-0 

catch.yr <- spp.counts2 %>% 
  select (1,3,4) 

catch.yr <- tidyr::gather(catch.yr, Survey, Count, -Fish)

# create nodes dataframe
Fish <- unique(as.character(catch.yr$Fish))
nodes <- data.frame(node = c(0:17), 
                    name = c(Fish, "Nearshore", "Offshore"))

#create links dataframe
catch.yr <- merge(catch.yr, nodes, by.x = "Fish", by.y = "name")
catch.yr <- merge(catch.yr, nodes, by.x = "Survey", by.y = "name")
links <- catch.yr[ , c("node.x", "node.y", "Count")]
colnames(links) <- c("source", "target", "value")

# draw sankey network
networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                            Source = 'source', 
                            Target = 'target', 
                            Value = 'value', 
                            NodeID = 'name',
                            units = 'Count',
                         fontFamily = "serif",
                         fontSize = 20,
                         height = 800, width = 1000)


##############################################################################################
##Based on total biomass
current.yr.all<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)
current.yr.all$TARGET_f<-as.factor(current.yr.all$TARGET)

##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))

spp.biomass<-aggregate(current.yr.all$WT/1000, by=list(SPECIES=current.yr.all$SPECIES, TARGET=current.yr.all$TARGET), FUN=sum)%>%
  renameCol('x','SUM')
spp.biomass<-merge.data.frame(spp.biomass, sci.names)
spp.biomass<-select(spp.biomass, c(4,5,2,3))
spp.biomass2<-cast(spp.biomass, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
spp.biomass2<-spp.biomass2%>%
  renameCol('COMMON_NAME','Fish')%>%
  renameCol('SCIENTIFIC_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
spp.biomass2[is.na(spp.biomass2)]<-0 

catch.yr <- spp.biomass2 %>% 
  select (1,3,4) 

catch.yr <- tidyr::gather(catch.yr, Survey, Count, -Fish)

# create nodes dataframe
Fish <- unique(as.character(catch.yr$Fish))
nodes <- data.frame(node = c(0:26), 
                    name = c(Fish, "Nearshore", "Offshore"))
#create links dataframe
catch.yr <- merge(catch.yr, nodes, by.x = "Fish", by.y = "name")
catch.yr <- merge(catch.yr, nodes, by.x = "Survey", by.y = "name")
links <- catch.yr[ , c("node.x", "node.y", "Count")]
colnames(links) <- c("source", "target", "value")

# draw sankey network
networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'Count', 
                         fontFamily = "serif",
                         fontSize = 20)

##############################################################################################
##############################################################################################
##Based on average biomass
current.yr.kgha<-all.data %>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)

current.yr.kgha<- current.yr.kgha %>%
  select (5,8,29:31,34,35) %>%
  complete(TARGET, LOCATION, SPECIES, fill=list(KGHA=0)) %>%
  filter(TARGET==2 & LOCATION<500 | TARGET==117 & LOCATION>500 | TARGET==118 & LOCATION>500)


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))

spp.kgha<-aggregate(current.yr.kgha$KGHA, by=list(SPECIES=current.yr.kgha$SPECIES, TARGET=current.yr.kgha$TARGET), FUN=mean, na.rm=TRUE)%>%
  renameCol('x','SUM')

spp.kgha<-merge.data.frame(spp.kgha, sci.names)
spp.kgha<-select(spp.kgha, c(4,5,2,3))
spp.kgha<-cast(spp.kgha, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
spp.kgha2<-spp.kgha%>%
  renameCol('COMMON_NAME','Fish')%>%
  renameCol('SCIENTIFIC_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
##spp.kgha2[is.na(spp.kgha2)]<-0 

catch.yr <- spp.kgha2 %>% 
  select (1,3,4) 

catch.yr <- tidyr::gather(catch.yr, Survey, mean.biom, -Fish) %>%
   filter(mean.biom>0) 
  
# create nodes dataframe
Fish <- unique(as.character(catch.yr$Fish))
nodes <- data.frame(node = c(0:26), 
                    name = c(Fish, "Nearshore", "Offshore"))
#create links dataframe
catch.yr <- merge(catch.yr, nodes, by.x = "Fish", by.y = "name")
catch.yr <- merge(catch.yr, nodes, by.x = "Survey", by.y = "name")
links <- catch.yr[ , c("node.x", "node.y", "mean.biom")]
colnames(links) <- c("source", "target", "value")

# draw sankey network
networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'mean.biom', 
                         fontFamily = "serif",
                         fontSize = 16,
                         height = 800, width = 1000)


