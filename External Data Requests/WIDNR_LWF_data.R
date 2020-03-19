##library
library(tidyverse)
library(doBy)
library(readxl)
library(ggplot2)
library(plotrix) 
library(psych)
library(rlang)
library(dplyr)
library(purrr)
library(forcats)
library(viridis)
library(reshape)
library(rgdal)
library(xlsx)
library(lubridate)
library(plyr)
library(gganimate)
library(magick)
library(grid)
library(ggforce)
library(here)

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

##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))

##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=20, family='serif'), 
                 axis.title=element_text(size=20, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=18, family='serif'),
                 plot.caption=element_text(size=16, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=16, family='serif'))

plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=20, family='serif'),
                  axis.title=element_text(size=20, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=16, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=16, family='serif'),
                  plot.caption=element_text(size=16, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=16, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0'

##to calculate NOHA and KGHA, need to subset out different vessels, as they have different conversion factors for area sampled

##r/v siscowet from start to 1976 **note, conversion factor only verified from 1973-1976, an estimate for years prior
siscowet76<-subset(raw.data, VESSEL==1 & YEAR<1977)
siscowet76$NOHA<-((siscowet76$NUM*60)/siscowet76$TOW_TIME)/2.01
siscowet76$KGHA<-((siscowet76$WT*.06)/siscowet76$TOW_TIME)/2.01
siscowet76$HA_SWEPT<-(siscowet76$TOW_TIME/60)*2.01

##r/v siscowet from 1977-1999
siscowet77.99<-subset(raw.data, VESSEL==1 & YEAR>1976 & YEAR<2000)
siscowet77.99$NOHA<-((siscowet77.99$NUM*60)/siscowet77.99$TOW_TIME)/2.45
siscowet77.99$KGHA<-((siscowet77.99$WT*.06)/siscowet77.99$TOW_TIME)/2.45
siscowet77.99$HA_SWEPT<-(siscowet77.99$TOW_TIME/60)*2.45

##r/v grayling 1973-1999 (all years)
grayling<-subset(raw.data, VESSEL==11)
grayling$NOHA<-((grayling$NUM*60)/grayling$TOW_TIME)/2.05
grayling$KGHA<-((grayling$WT*.06)/grayling$TOW_TIME)/2.05
grayling$HA_SWEPT<-(grayling$TOW_TIME/60)*2.05

##r/v coaster, USGS, 1993 serials 1-120
coaster93<-subset(raw.data, VESSEL==99 & YEAR==1993 & SERIAL<121)
coaster93$NOHA<-((coaster93$NUM*60)/coaster93$TOW_TIME)/.6097
coaster93$KGHA<-((coaster93$WT*.06)/coaster93$TOW_TIME)/.6097
coaster93$HA_SWEPT<-(coaster93$TOW_TIME/60)*.6097

##r/v coaster, USGS, 1988-1992 and 1994-2004  and 1993 serials>120
coasterUSGS1<-subset(raw.data, VESSEL==99 & YEAR<1993)
coasterUSGS2<-subset(raw.data, VESSEL==99 & YEAR>1993)
coasterUSGS3<-subset(raw.data, VESSEL==99 & YEAR==1993 & SERIAL>120)
coasterUSGS<-rbind(coasterUSGS1, coasterUSGS2, coasterUSGS3)
coasterUSGS$NOHA<-((coasterUSGS$NUM*60)/coasterUSGS$TOW_TIME)/.785
coasterUSGS$KGHA<-((coasterUSGS$WT*.06)/coasterUSGS$TOW_TIME)/.785
coasterUSGS$HA_SWEPT<-(coasterUSGS$TOW_TIME/60)*.785

##r/v coaster/shoveler USFWS 2009 (calling it shoveler to differentiate from coaster above)
shoveler<-subset(raw.data, VESSEL==95)
shoveler$NOHA<-(shoveler$NUM/(((shoveler$DISTANCE*5280)*14.76)/107639.1))
shoveler$KGHA<-(shoveler$WT*.001/(((shoveler$DISTANCE*5280)*14.76)/107639.1))
shoveler$HA_SWEPT<-((shoveler$DISTANCE*5280)*14.76)/107639.1

##r/v whitefish
whitefish<-subset(raw.data, VESSEL==50)
whitefish$NOHA<-((whitefish$NUM*60)/whitefish$TOW_TIME)/1.174
whitefish$KGHA<-((whitefish$WT*.06)/whitefish$TOW_TIME)/1.174
whitefish$HA_SWEPT<-(whitefish$TOW_TIME/60)*1.174

##johnboat
johnboat<-subset(raw.data, VESSEL==92)
johnboat$NOHA<-((johnboat$NUM*60)/johnboat$TOW_TIME)/.6568
johnboat$KGHA<-((johnboat$WT*.06)/johnboat$TOW_TIME)/.6568
johnboat$HA_SWEPT<-(johnboat$TOW_TIME/60)*.6568

##r/v kaho **note, don't have a conversion factor for this ship yet, using the conversion factor for the siscowet for now
kaho<-subset(raw.data, VESSEL==4)
kaho$NOHA<-((kaho$NUM*60)/kaho$TOW_TIME)/2.45
kaho$KGHA<-((kaho$WT*.06)/kaho$TOW_TIME)/2.45
kaho$HA_SWEPT<-(kaho$TOW_TIME/60)*2.45

##r/v kiyi
kiyi<-subset(raw.data, VESSEL==25)
kiyi$NOHA<-(kiyi$NUM/(((kiyi$DISTANCE*5280)*25.49)/107639.1))
kiyi$KGHA<-(kiyi$WT*.001/(((kiyi$DISTANCE*5280)*25.49)/107639.1))
kiyi$HA_SWEPT<-(((kiyi$DISTANCE*5280)*25.49)/107639.1)

##Daphnia **note, no bottom trawls done off this ship, so doesn't have all the same data as the other vessels
daphnia<-subset(raw.data, VESSEL==9)
daphnia$NOHA<-NA
daphnia$KGHA<-NA
daphnia$HA_SWEPT<-NA

##bind all data frames together now that they have NOHA and KGHA
all.data<-rbind(kiyi, siscowet76, siscowet77.99, grayling, coaster93, coasterUSGS, whitefish, johnboat, kaho, shoveler, daphnia)
##check that the number of rows in all.data matches the number in the raw.data input to make sure no data got lost along the way
##if data were lost, were any additional vessels used that are not listed above? any vessel codes changed or recorded incorrectly?

##SUMMARIZED LWF CATCH------------------------------------------------------------------------
lwf<-all.data%>%
  filter(SPECIES==203)%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)%>%
  filter(YEAR>1977)%>%
  filter(M_UNIT=='WI2  '|M_UNIT=='WI1  ')

sites.surveyed<-all.data%>%
  filter(YEAR>1977)%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)%>%
  filter(M_UNIT=='WI2  '|M_UNIT=='WI1  ')%>%
  group_by(YEAR)%>%
  distinct(OP_ID)

sites.surveyed2<-all.data%>%
  filter(YEAR>1977)%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)%>%
  filter(M_UNIT=='WI2  '|M_UNIT=='WI1  ')

site.details<-select(sites.surveyed2, c(1:28, 32,33,36))
site.details<-distinct(site.details)
lwf.catch<-select(lwf, c('OP_ID','YEAR','NUM','WT', 'NOHA','KGHA'))
lwf.allsites<-merge.data.frame(lwf.catch, sites.surveyed, all=T)
lwf.allsites[is.na(lwf.allsites)]<-0

lwf.all<-merge.data.frame(site.details,lwf.allsites, all = F, by=c('OP_ID','YEAR'))

lwf.export<-select(lwf.all, c('OP_ID','YEAR','OP_DATE','TARGET','VESSEL','LOCATION','M_UNIT',
                              'TR_DESIGN','BEG_DEPTH','END_DEPTH','Mid.Lat.DD','Mid.Long.DD',
                              'HA_SWEPT','NUM','WT','NOHA','KGHA'))
write.xlsx(lwf.export, here('External Data Requests/WI Lake Whitefish for WIDNR', 'all_lwf.xlsx'),
                            row.names=F)

##LAKE WHITEFISH LENGTHS-----------------------------------------------------------------
lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
lengths<-lengths%>%
  filter(SPECIES==203)%>%
  filter(YEAR>1977)

lengths2<-merge.data.frame(site.details, lengths, all=F)
lengths.export<-select(lengths2,c('OP_ID','YEAR','OP_DATE','TARGET','VESSEL','LOCATION','M_UNIT',
                                  'TR_DESIGN','BEG_DEPTH','END_DEPTH','Mid.Lat.DD','Mid.Long.DD',
                                  'HA_SWEPT','LENGTH','EXP_N'))

write.xlsx(lengths.export, here('External Data Requests/WI Lake Whitefish for WIDNR', 'all_lengths.xlsx'),
           row.names=F)

##AGE-1 DENSITIES-------------------------------------------------------------------------
a1<-lengths.export%>%
  filter(LENGTH<161)
a1<-aggregate(a1$EXP_N,by=list(OP_ID=a1$OP_ID), FUN=sum)%>%
  renameCol('x','N.Age1')
a1.details<-merge.data.frame(site.details, a1, all=T)

a1.details<-select(a1.details,c('OP_ID','YEAR','OP_DATE','TARGET','VESSEL','LOCATION','M_UNIT',
                                  'TR_DESIGN','BEG_DEPTH','END_DEPTH','Mid.Lat.DD','Mid.Long.DD',
                                  'HA_SWEPT','N.Age1'))
a1.details[c('N.Age1')][is.na(a1.details[c('N.Age1')])]<-0

a1.details$NOHA<-a1.details$N.Age1/a1.details$HA_SWEPT

write.xlsx(a1.details, here('External Data Requests/WI Lake Whitefish for WIDNR',
                            'age1_densities.xlsx'), row.names=F)
