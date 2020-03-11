
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

####################################################################################################################NEARSHORE DATA####
##summarize nearshore data
ns<-subset(all.data, TARGET==2 & YEAR>1977)

##calculate mean biomass by year, start by getting total kgha for each station
ns.ann.station.sum<-aggregate(ns$KGHA, by=list(YEAR=ns$YEAR, Station=ns$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')


##calculate summary statistics
ns.summary <- ns.ann.station.sum%>% 
  group_by(YEAR) %>% 
  summarise_all(funs(mean,median,sd,std.error))%>%
  select(c(1,3,5,7,9))

#subset last 5 years and 10 years so means can be added to plot
ns.yr5mean<-subset(ns.summary, YEAR>=(max(YEAR)-4))
ns.yr10mean<-subset(ns.summary, YEAR>=(max(YEAR)-9))
ns.yr20mean<-subset(ns.summary, YEAR>=(max(YEAR)-19))
ns.yr30mean<-subset(ns.summary, YEAR>=(max(YEAR)-29))
ns.yr40mean<-subset(ns.summary, YEAR>=(max(YEAR)-39))


##plot mean nearshore biomass
##turn on/off the geom_segment(...) lines to change which mean lines you want to show
ggplot(ns.summary, aes(x=YEAR, y=StationKGHA_mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  geom_errorbar(data=ns.summary, aes(x=YEAR, ymin=StationKGHA_mean-StationKGHA_std.error, ymax=StationKGHA_mean+StationKGHA_std.error),
                width=0.4)+
  scale_y_continuous(expand=c(0,0), breaks=c(5,10,15,20,25), labels=c('5','10','15','20','25'))+
  geom_segment(x=(max(ns.summary$YEAR)-4), xend=max(ns.summary$YEAR), y=mean(ns.yr5mean$StationKGHA_mean), #5 year mean
               yend=mean(ns.yr5mean$StationKGHA_mean), size=1.5, color='mediumorchid4')+
  geom_segment(x=(max(ns.summary$YEAR)-9), xend=max(ns.summary$YEAR), y=mean(ns.yr10mean$StationKGHA_mean), #10 year mean
               yend=mean(ns.yr10mean$StationKGHA_mean), size=1.5, color='darkolivegreen4')+
  geom_segment(x=(max(ns.summary$YEAR)-19), xend=max(ns.summary$YEAR), y=mean(ns.yr20mean$StationKGHA_mean), #20 year mean
               yend=mean(ns.yr20mean$StationKGHA_mean), size=1.5, color='cyan4')+
  geom_segment(x=(max(ns.summary$YEAR)-29), xend=max(ns.summary$YEAR), y=mean(ns.yr30mean$StationKGHA_mean), #30 year mean
               yend=mean(ns.yr30mean$StationKGHA_mean), size=1.5, color='lightpink3')+
  geom_segment(x=(max(ns.summary$YEAR)-39), xend=max(ns.summary$YEAR), y=mean(ns.yr40mean$StationKGHA_mean), #40 year mean
               yend=mean(ns.yr40mean$StationKGHA_mean), size=1.5, color='darkorange')

##save plot to the folder you assigned as the working directory
ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##plot mean nearshore biomass no mean lines
ggplot(ns.summary, aes(x=YEAR, y=StationKGHA_mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  geom_errorbar(data=ns.summary, aes(x=YEAR, ymin=StationKGHA_mean-StationKGHA_std.error, ymax=StationKGHA_mean+StationKGHA_std.error),
                width=0.4)+
  scale_y_continuous(expand=c(0,0), breaks=c(5,10,15,20,25), labels=c('5','10','15','20','25'), limits=c(0,28))

##save plot to the folder you assigned as the working directory
ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_nomeans.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##to plot ns biomass of RAINBOW SMELT ONLY
##calculate mean biomass by year, start by getting total kgha for each station
ns.ann.station.sum.spp5<-subset(ns, SPECIES==109)
ns.ann.station.sum.spp5<-aggregate(ns.ann.station.sum.spp5$KGHA, by=list(YEAR=ns.ann.station.sum.spp5$YEAR, 
                                                                         Station=ns.ann.station.sum.spp5$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')

ns.summary2 <- ns.ann.station.sum.spp5%>% 
  group_by(YEAR) %>% 
  summarise_all(funs(mean,median,sd,std.error))%>%
  select(c(1,3,5,7,9))

ns.yr5meansmelt<-subset(ns.summary2, YEAR>=(max(YEAR)-4))
ns.yr10meansmelt<-subset(ns.summary2, YEAR>=(max(YEAR)-9))
ns.yr20meansmelt<-subset(ns.summary2, YEAR>=(max(YEAR)-19))
ns.yr30meansmelt<-subset(ns.summary2, YEAR>=(max(YEAR)-29))
ns.yr40meansmelt<-subset(ns.summary2, YEAR>=(max(YEAR)-39))

ggplot(ns.summary2, aes(x=YEAR, y=StationKGHA_mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Rainbow Smelt Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  geom_errorbar(data=ns.summary2, aes(x=YEAR, ymin=StationKGHA_mean-StationKGHA_std.error, ymax=StationKGHA_mean+StationKGHA_std.error),
                width=0.4)+
  scale_y_continuous(expand=c(0,0), breaks=c(5,10,15,20,25), labels=c('5','10','15','20','25'), limits=c(0,6))+
  geom_segment(x=(max(ns.summary2$YEAR)-4), xend=max(ns.summary2$YEAR), y=mean(ns.yr5meansmelt$StationKGHA_mean), #5 year mean
               yend=mean(ns.yr5meansmelt$StationKGHA_mean), size=1.5, color='mediumorchid4')+
  geom_segment(x=(max(ns.summary2$YEAR)-9), xend=max(ns.summary2$YEAR), y=mean(ns.yr10meansmelt$StationKGHA_mean), #10 year mean
               yend=mean(ns.yr10meansmelt$StationKGHA_mean), size=1.5, color='darkolivegreen4')+
  geom_segment(x=(max(ns.summary2$YEAR)-19), xend=max(ns.summary2$YEAR), y=mean(ns.yr20meansmelt$StationKGHA_mean), #20 year mean
               yend=mean(ns.yr20meansmelt$StationKGHA_mean), size=1.5, color='cyan4')+
  geom_segment(x=(max(ns.summary2$YEAR)-29), xend=max(ns.summary2$YEAR), y=mean(ns.yr30meansmelt$StationKGHA_mean), #30 year mean
               yend=mean(ns.yr30meansmelt$StationKGHA_mean), size=1.5, color='lightpink3')+
  geom_segment(x=(max(ns.summary2$YEAR)-39), xend=max(ns.summary2$YEAR), y=mean(ns.yr40meansmelt$StationKGHA_mean), #40 year mean
               yend=mean(ns.yr40meansmelt$StationKGHA_mean), size=1.5, color='darkorange')


ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_smelt.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##calculate summary statistics WITH SPECIES
ns.ann.station.sum.spp<-aggregate(ns$KGHA, by=list(YEAR=ns$YEAR, Station=ns$LOCATION, Species=ns$SPECIES), FUN=sum)%>%
  renameCol('x','StationKGHA')
ns.ann.station.sum.spp<-cast(ns.ann.station.sum.spp, YEAR+Station~Species)
ns.ann.station.sum.spp[is.na(ns.ann.station.sum.spp)]<-0
ns.ann.station.sum.spp<-melt(ns.ann.station.sum.spp, id=c('YEAR','Station'))
ns.ann.station.sum.spp<-renameCol(ns.ann.station.sum.spp,'value','KGHA')

ns.spp.categories<-data.frame(Spp=c('Cisco','Lake Whitefish','Bloater'), Species=c(202,203,204)) ##change the species you want to single out here
ns.ann.station.sum.spp<-merge.data.frame(ns.ann.station.sum.spp, ns.spp.categories, all=T)
ns.ann.station.sum.spp<- ns.ann.station.sum.spp%>% mutate(Spp=fct_explicit_na(Spp, na_level='Other')) ##species not specified in the lines above = other

#ns.ann.station.sum.spp$Spp<-if_else(ns.ann.station.sum.spp$Species==202|ns.ann.station.sum.spp$Species==204,'Cisco & Bloater','Other')
ns.ann.station.sum.spp2<-aggregate(ns.ann.station.sum.spp$KGHA, by=list(Spp=ns.ann.station.sum.spp$Spp, Station=ns.ann.station.sum.spp$Station,
                                                                       YEAR=ns.ann.station.sum.spp$YEAR), FUN=sum)%>%
  renameCol('x','KGHA')


ns.ann.station.sum.spp2<-aggregate(ns.ann.station.sum.spp2$KGHA, by=list(YEAR=ns.ann.station.sum.spp2$YEAR, Spp=ns.ann.station.sum.spp2$Spp), FUN=mean)%>%
  renameCol('x','meanKGHA')

##plot mean nearshore biomass with bloater, cisco, and lake whitefish separate
ggplot(ns.ann.station.sum.spp2, aes(x=YEAR, y=meanKGHA, fill=Spp))+
  geom_bar(stat='identity', position='stack', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  scale_y_continuous(expand=c(0,0), breaks=c(5,10,15,20,25), labels=c('5','10','15','20','25'), limits=c(0,28))+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1','lightblue3','lightpink3'))+
  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_coregonus.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##bloater and cisco together, all other species as other
ns.ann.station.sum.spp$Spp2<-if_else(ns.ann.station.sum.spp$Spp=='Bloater'|ns.ann.station.sum.spp$Spp=='Cisco','Bloater & Cisco','Other')
ns.ann.station.sum.spp3<-aggregate(ns.ann.station.sum.spp$KGHA, by=list(Spp2=ns.ann.station.sum.spp$Spp2, Station=ns.ann.station.sum.spp$Station,
                                                                        YEAR=ns.ann.station.sum.spp$YEAR), FUN=sum)%>%
  renameCol('x','KGHA')


ns.ann.station.sum.spp3<-aggregate(ns.ann.station.sum.spp3$KGHA, by=list(YEAR=ns.ann.station.sum.spp3$YEAR, Spp=ns.ann.station.sum.spp3$Spp2), FUN=mean)%>%
  renameCol('x','meanKGHA')

ggplot(ns.ann.station.sum.spp3, aes(x=YEAR, y=meanKGHA, fill=Spp))+
  geom_bar(stat='identity', position='stack', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  scale_y_continuous(expand=c(0,0), breaks=c(5,10,15,20,25), labels=c('5','10','15','20','25'), limits=c(0,28))+
  scale_fill_manual( name=' ', values=c('palegreen3','lightpink3'))+
  theme(legend.position=c(0.8,0.8))
ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_ciscobloater.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##bloater and cisco together, lake whitefish separate
ns.spp.categories2<-data.frame(Spp=c('Cisco','Bloater','Lake Whitefish','Other'),Spp3=c('Bloater & Cisco','Bloater & Cisco','Lake Whitefish','Other'))
ns.ann.station.sum.spp<-merge.data.frame(ns.ann.station.sum.spp, ns.spp.categories2)

ns.ann.station.sum.spp4<-aggregate(ns.ann.station.sum.spp$KGHA, by=list(Spp3=ns.ann.station.sum.spp$Spp3, Station=ns.ann.station.sum.spp$Station,
                                                                        YEAR=ns.ann.station.sum.spp$YEAR), FUN=sum)%>%
  renameCol('x','KGHA')


ns.ann.station.sum.spp4<-aggregate(ns.ann.station.sum.spp4$KGHA, by=list(YEAR=ns.ann.station.sum.spp4$YEAR, Spp=ns.ann.station.sum.spp4$Spp3), FUN=mean)%>%
  renameCol('x','meanKGHA')

ggplot(ns.ann.station.sum.spp4, aes(x=YEAR, y=meanKGHA, fill=Spp))+
  geom_bar(stat='identity', position='stack', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  scale_y_continuous(expand=c(0,0), breaks=c(5,10,15,20,25), labels=c('5','10','15','20','25'), limits=c(0,28))+
  scale_fill_manual(name=' ', values=c('palegreen3', 'lightblue3','lightpink3'))+
  theme(legend.position=c(0.8,0.8))
ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_ciscobloater_lwf.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##biomass at individual stations for the current sampling year, ranked by biomass--------------------------------BIOMASS BY STATION-----
##summarize nearshore data
ns<-subset(all.data, TARGET==2 & YEAR>1977)

##calculate mean biomass by year, start by getting total kgha for each station
ns.ann.station.sum<-aggregate(ns$KGHA, by=list(YEAR=ns$YEAR, Station=ns$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')

ns.current.yr<-ns.ann.station.sum%>%
  filter(YEAR==max(YEAR))
ns.current.yr$Station<-as.factor(ns.current.yr$Station)

##plot biomass at each station for nearshore cruise
nsst<-ggplot(ns.current.yr, aes(x=Station, y=StationKGHA))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  theme( axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
  plot_theme+
  aes(x=fct_reorder(Station, StationKGHA))+
  labs(x='Nearshore station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle=(paste('USGS bottom trawl assessment, ', ns.current.yr$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  geom_hline(yintercept=mean(ns.current.yr$StationKGHA))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'))

##save plot to the folder you assigned as the working directory
ggsave(plot=nsst,here('Plots and Tables/RVCAT','ns_biomass_bystation.png'), dpi = 300, width = 40, height = 20, units = "cm")

##ns skewness plot------------------------------------------------------------------------------------------------------SKEWNESS-----
##summarize nearshore data
ns<-subset(all.data, TARGET==2 & YEAR>1977)

##calculate mean biomass by year, start by getting total kgha for each station
ns.ann.station.sum<-aggregate(ns$KGHA, by=list(YEAR=ns$YEAR, Station=ns$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')

ns.skewness<-ns.ann.station.sum%>%
  group_by(YEAR)%>%
  do(describe(.$StationKGHA))

skew<-ggplot(ns.skewness, aes(x=YEAR, y=skew))+
  geom_point(color='black', size=3)+
  geom_line(size=1, color='black')+
  plot_theme+
  labs(x='Year', y='Skewness')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'),
                     limits=c(1977, 2020))+
  scale_y_continuous(expand=c(0,0), limits=c(0,9), breaks=c(1,2,3,4,5,6,7,8,9),
                     labels=c('1','2','3','4','5','6','7','8','9'))+
  geom_hline(data=ns.skewness, yintercept=mean(ns.skewness$skew))

##inset skewness plot into station biomass plot
skewstation<-ggplot(ns.current.yr, aes(x=Station, y=StationKGHA, fill=StationKGHA))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='cadetblue2', high='red', name='Biomass\n(kg per ha)')+
  annotation_custom(ggplotGrob(skew), ymin=20, ymax=45, xmax=60, xmin=3)+ ##controls placement of the inset plot if need to change
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        legend.position=c(0.9,0.8))+
  plot_theme+
  aes(x=fct_reorder(Station, StationKGHA))+
  labs(x='Nearshore station identifier', y='Biomass (kg per ha)',
       title='Lake Superior Fish Biomass at Nearshore Stations and Annual Skewness',
       caption=ann_data_access,
       subtitle=(paste('USGS bottom trawl assessment, ', ns.current.yr$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'))+
  geom_hline(yintercept = mean(ns.current.yr$StationKGHA), size=1.5)

ggsave(plot=skewstation, here('Plots and Tables/RVCAT','ns_station_skewness.png'), dpi = 300, width = 40, height = 20, units = "cm")

##nearshore map color coded by station total biomass, for inset into station biomass plot----------------------------------------
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##subset out the current year for nearshore
ns<-subset(all.data, TARGET==2 & YEAR==max(YEAR))

##calculate mean biomass by year, start by getting total kgha for each station
ns.ann.station.sum<-aggregate(ns$KGHA, by=list(YEAR=ns$YEAR, Station=ns$LOCATION,
                                                   BEG_LONGITUDE_DD=ns$BEG_LONGITUDE_DD,
                                                   BEG_LATITUDE_DD=ns$BEG_LATITUDE_DD), FUN=sum)%>%
  renameCol('x','StationKGHA')

##create map with stations color coded by biomass
nsbiomap2<-ggplot(ns.ann.station.sum, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=ns.ann.station.sum, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=StationKGHA), size=6, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Biomass\n(kg per ha)')+
  map_theme+
  geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Nearshore Station Fish Biomass',
        subtitle=(paste('USGS bottom trawl assessment, ', ns.current.yr$YEAR)),
        caption=ann_data_access)

ggsave(plot=nsbiomap2,here('Plots and Tables/RVCAT','ns_sites_biomass_map2.png'), dpi = 300, width = 30, height = 16, units = "cm")

nsbiomap<-ggplot(ns.ann.station.sum, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=ns.ann.station.sum, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=StationKGHA), size=6, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Biomass\n(kg per ha)')+
  map_theme+
  geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude')

ggsave(plot=nsbiomap,here('Plots and Tables/RVCAT','ns_sites_biomass_map.png'), dpi = 300, width = 30, height = 16, units = "cm")

##inset above map into station biomass bar graph, with corresponding color scales
mapstation<-ggplot(ns.current.yr, aes(x=Station, y=StationKGHA, fill=StationKGHA))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='cadetblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  annotation_custom(ggplotGrob(nsbiomap), ymin=10, ymax=49, xmax=55, xmin=3)+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=14))+
  plot_theme+
  aes(x=fct_reorder(Station, StationKGHA))+
  labs(x='Nearshore station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle=(paste('USGS bottom trawl assessment, ', ns.current.yr$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'))+
  geom_hline(yintercept = mean(ns.current.yr$StationKGHA), size=1)

ggsave(plot=mapstation,here('Plots and Tables/RVCAT','ns_station_biomass_map_bars.png'), dpi = 300, width = 40, height = 20, units = "cm")

##plot comparison of multiple species biomasses from nearshore cruise----------------------------------------NEARSHORE SPP BIOMASS----
##summarize nearshore data
ns<-subset(all.data, TARGET==2 & YEAR>1977)

ns.spp.compare<-subset(ns, SPECIES==317|SPECIES==308|SPECIES==307)%>% ##input the species codes you want
  select(YEAR, LOCATION, SPECIES, KGHA)

##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year
ns.sites.byyear<-ns%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
ns.sites.byyear$SITECHECK<-'SURVEYED'

ns.spp.compare2<-merge.data.frame(ns.spp.compare, ns.sites.byyear, all=TRUE)
ns.spp.compare2<-cast(ns.spp.compare2, YEAR+LOCATION+SITECHECK~SPECIES, value='KGHA', fun.aggregate='mean')%>%
  select(c(1,2,4:6))
ns.spp.compare2[is.na(ns.spp.compare2)]<-0
ns.spp.compare3<-melt.data.frame(ns.spp.compare2, id.vars=c('YEAR','LOCATION'))%>%
  renameCol('value','KGHA')%>%
  renameCol('variable','SPECIES')

##now find the mean of each species for each year
ns.spp.compare.mean<-aggregate(ns.spp.compare3$KGHA, by=list(Year=ns.spp.compare3$YEAR, SPECIES=ns.spp.compare3$SPECIES), FUN=mean)%>%
  renameCol('x', 'meanKGHA')

##merge to get species names
ns.spp.compare.mean<-merge.data.frame(ns.spp.compare.mean, sci.names, all=F)

##plot trends in selected species
ggplot(ns.spp.compare.mean, aes(x=Year, y=meanKGHA, color=COMMON_NAME))+
  geom_line(size=1)+
  geom_point(size=2)+
  theme(legend.position='top') +
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Nearshore Lake Trout Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'),
                     limits=c(1977, 2022))+
  scale_y_continuous(expand=c(0,0), breaks=c(0, .1, .2,.3,.4,.5,.6,.7,.8,.9,1),
                     labels=c('0', '0.1', '0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9',1), limits=c(0,1))+
  scale_color_viridis(discrete=TRUE, name='')

ggsave(here('Plots and Tables/RVCAT','ns_multi_spp_biomass_lines.png'), dpi = 300, width = 40, height = 20, units = "cm")

##OFFSHORE plots and data##################################################################################################OFFSHORE###
os<-subset(all.data, TARGET==118|TARGET==117 & YEAR>2010)%>%
  filter(TR_DESIGN==25|TR_DESIGN==4)%>%
  filter(END_DEPTH>84)

##calculate mean biomass by year, start by getting total kgha for each station
os.ann.station.sum<-aggregate(os$KGHA, by=list(YEAR=os$YEAR, Station=os$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')

##calculate summary statistics
os.summary <- os.ann.station.sum%>% 
  group_by(YEAR) %>% 
  summarise_all(funs(mean,median,sd,std.error))%>%
  select(c(1,3,5,7,9))

##plot mean offshore abundance
ggplot(os.summary, aes(x=YEAR, y=StationKGHA_mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Offshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(2011,2013,2015,2017,2019,2021,2023), 
                     labels=c('2011','2013','2015','2017','2019','2021','2023'))+
  geom_errorbar(data=os.summary, aes(x=YEAR, ymin=StationKGHA_mean-StationKGHA_std.error, ymax=StationKGHA_mean+StationKGHA_std.error),
                width=0.4)+
  scale_y_continuous(expand=c(0,0), breaks=c(2,4,6,8,10), labels=c('2','4','6','8','10'))

ggsave(here('Plots and Tables/RVCAT','os_biomass_annual.png'), dpi = 300, width = 40, height = 20, units = "cm")

##biomass by station-------------------------------------------------------------------------------------------------------------
os<-subset(all.data, TARGET==118|TARGET==117 & YEAR>2010)%>%
  filter(TR_DESIGN==25|TR_DESIGN==4)%>%
  filter(END_DEPTH>84)
##calculate mean biomass by year, start by getting total kgha for each station
os.ann.station.sum<-aggregate(os$KGHA, by=list(YEAR=os$YEAR, Station=os$LOCATION, Species=os$SPECIES, 
                                               EndDepth=os$END_DEPTH), FUN=sum)%>%
  renameCol('x','StationKGHA')
simplifyspp<-data.frame('Species'=c(109,127,130,204,206,211,308,317,902,903,904),
                        'Label'=c('Other', 'Other','Other','Other','Kiyi','Other','siscowet Lake Trout', 'Other','Other',
                                  'Other','Deepwater Sculpin'))
os.ann.station.sum<-merge.data.frame(os.ann.station.sum, simplifyspp)

os.current.yr<-os.ann.station.sum%>%
  filter(YEAR==max(YEAR))
os.current.yr$Station<-as.factor(os.current.yr$Station)

##plot biomass at each station for offshore cruise
os.current.yr$EndDepth<-round(os.current.yr$EndDepth,0)
os.current.yr$Station.Depth<-paste(os.current.yr$Station, os.current.yr$EndDepth, sep=', ')
ggplot(os.current.yr, aes(x=Station.Depth, y=StationKGHA))+
  geom_bar(stat='identity', color='black', aes(fill=Label))+
  scale_fill_viridis(discrete=T, name='', breaks=c('Deepwater Sculpin','Kiyi','siscowet Lake Trout', 'Other'),
                     begin=1, end=0)+
  theme(legend.position=c(0.15,0.7),
        legend.direction = 'vertical',
        axis.text.x = element_text(angle=-90, vjust=0.5))+
  plot_theme+
  aes(x=fct_reorder(Station.Depth, EndDepth))+
  labs(x='Offshore station, ordered by depth (m)', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Offshore Fish Biomass',
       subtitle=(paste('USGS bottom trawl assessment, ', os.current.yr$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'), limits=c(0,29))+
  geom_hline(yintercept=mean(os.summary$StationKGHA_mean), size=1)

##save plot to the folder you assigned as the working directory
ggsave(here('Plots and Tables/RVCAT','os_biomass_bystation.png'), dpi = 300, width = 40, height = 20, units = "cm")

ggplot(os.current.yr, aes(x=Station.Depth, y=StationKGHA))+
  geom_bar(stat='identity', color='black', aes(fill=Label))+
  geom_point(aes(x=Station, y=27, color=EndDepth), size=12, shape=15)+
  scale_color_continuous(low='cyan', high='black', name='Depth (m)')+
  scale_fill_viridis(discrete=T, name='', breaks=c('Deepwater Sculpin','Kiyi','siscowet Lake Trout', 'Other'),
                     begin=1, end=0)+
  theme(legend.position=c(0.15,0.7),
        legend.direction = 'vertical',
        axis.text.x = element_text(angle=-90, vjust=0.5))+
  guides(color=guide_colourbar(direction='horizontal', order=1, barheight=2, barwidth=12))+
  plot_theme+
  aes(x=fct_reorder(Station, EndDepth))+
  labs(x='Offshore station, ordered by depth', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Offshore Fish Biomass',
       subtitle=(paste('USGS bottom trawl assessment, ', os.current.yr$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'), limits=c(0,29))+
  geom_hline(yintercept=mean(os.summary$StationKGHA_mean), size=1)

##save plot to the folder you assigned as the working directory
ggsave(here('Plots and Tables/RVCAT','os_biomass_bystation2.png'), dpi = 300, width = 40, height = 20, units = "cm")

##skewness plot-----------------------------------------------------------------------------------------------------OS-SKEWNESS-----
##summarize offshore data

os<-subset(all.data, TARGET==118|TARGET==117 & YEAR>2010)%>%
  filter(TR_DESIGN==25|TR_DESIGN==4)%>%
  filter(END_DEPTH>84)
##calculate mean biomass by year, start by getting total kgha for each station
os.ann.station.sum<-aggregate(os$KGHA, by=list(YEAR=os$YEAR, Station=os$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')

os.skewness<-os.ann.station.sum%>%
  group_by(YEAR)%>%
  do(describe(.$StationKGHA))

skew2<-ggplot(os.skewness, aes(x=YEAR, y=skew))+
  geom_point(color='black', size=3)+
  geom_line(size=1, color='black')+
  plot_theme+
  labs(x='Year', y='Skewness')+
  scale_x_continuous(expand=c(0,0), breaks=c(2010,2012,2014,2016,2018,2020,2022), 
                     labels=c('2010','2012','2014','2016','2018','2020','2022'),
                     limits=c(2011, 2020))+
  scale_y_continuous(expand=c(0,0), limits=c(0,3))+
  geom_hline(data=os.skewness, yintercept=mean(os.skewness$skew))

##inset skewness plot into station biomass bar graph
os.ann.station.sum<-aggregate(os$KGHA, by=list(YEAR=os$YEAR, Station=os$LOCATION), FUN=sum)%>%
  renameCol('x','StationKGHA')
os.current.yr<-os.ann.station.sum%>%
  filter(YEAR==max(YEAR))
os.current.yr$Station2<-as.factor(os.current.yr$Station)

skewstation2<-ggplot(os.current.yr, aes(x=Station2, y=StationKGHA, fill=StationKGHA))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='cadetblue2', high='red', name='Biomass\n(kg per ha)')+
  annotation_custom(ggplotGrob(skew2), ymin=10, ymax=25, xmax=25, xmin=1)+ #change these values if inset needs repositioning
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), 
        legend.position=c(0.8,0.8))+
  plot_theme+
  aes(x=fct_reorder(Station2, StationKGHA))+
  labs(x='Offshore station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations with Annual Skewness',
       subtitle=(paste('USGS bottom trawl assessment, ', os.current.yr$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'))+
  geom_hline(yintercept=mean(os.current.yr$StationKGHA))

ggsave(plot=skewstation2,here('Plots and Tables/RVCAT','os_station_skewness.png'), dpi = 300, width = 40, height = 20, units = "cm")


##offshore map with sites coded by biomass for inset into bar plot
os.ann.station.sum<-aggregate(os$KGHA, by=list(YEAR=os$YEAR, Station=os$LOCATION,
                                                     BEG_LONGITUDE_DD=os$BEG_LONGITUDE_DD,
                                                     BEG_LATITUDE_DD=os$BEG_LATITUDE_DD), FUN=sum)%>%
  renameCol('x','StationKGHA')
os.ann.station.sum2<-os.ann.station.sum%>%
  filter(YEAR==max(YEAR))
os.ann.station.sum2$Station<-as.factor(os.ann.station.sum2$Station)

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##map of offshore sites color ranked by biomass
osbiomap2<-ggplot(os.ann.station.sum2, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw()+
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude', breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=os.ann.station.sum2, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=StationKGHA), size=6, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Biomass\n(kg per ha)')+
  map_theme+
  geom_text(aes(label=Station))+
  labs(title='Lake Superior offshore Station Fish Biomass',
       caption=ann_data_access,
       subtitle=(paste('USGS bottom trawl assessment, ', os.ann.station.sum2$YEAR)))

ggsave(plot=osbiomap2,here('Plots and Tables/RVCAT','os_sites_biomass_map2.png'), dpi = 300, width = 30, height = 16, units = "cm")

osbiomap<-ggplot(os.ann.station.sum2, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw()+
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude', breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=os.ann.station.sum2, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=StationKGHA), size=6, stroke=1.5)+
  scale_color_gradient(low='cadetblue2', high='red', name='Biomass\n(kg per ha)')+
  map_theme+
  geom_text(aes(label=Station))

ggsave(plot=osbiomap,here('Plots and Tables/RVCAT','os_sites_biomass_map.png'), dpi = 300, width = 30, height = 16, units = "cm")

##inset color map into bar graph of stations with corresponding colors
osbiomapbars<-ggplot(os.ann.station.sum2, aes(x=Station, y=StationKGHA, fill=StationKGHA))+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='cadetblue2', high='red', name='Fish biomass (kg per ha)', guide=F)+
  annotation_custom(ggplotGrob(osbiomap), ymin=7, ymax=26, xmax=23, xmin=1)+ #change these values if inset needs repositioning
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))+
  plot_theme+
  aes(x=fct_reorder(Station, StationKGHA))+
  labs(x='Offshore station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Offshore Stations',
       subtitle=(paste('USGS bottom trawl assessment, ', os.ann.station.sum2$YEAR)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70),
                     labels=c('0','5','10','15','20','25','30','35','40','45','50','55','60','65','70'))+
  geom_hline(yintercept = mean(os.ann.station.sum2$StationKGHA), size=1)

ggsave(plot=osbiomapbars,here('Plots and Tables/RVCAT','os_station_biomass_map_bars.png'), dpi = 300, width = 40, height = 20, units = "cm")

##animated plot of offshore biomass at each station across years---------------------------------------------------------------------
os.ann.station.sum<-aggregate(os$KGHA, by=list(YEAR=os$YEAR, Station=os$LOCATION,
                                               BEG_LONGITUDE_DD=os$BEG_LONGITUDE_DD,
                                               BEG_LATITUDE_DD=os$BEG_LATITUDE_DD), FUN=sum)%>%
  renameCol('x','StationKGHA')
os.ann.station.sum2<-os.ann.station.sum%>%
  filter(YEAR==max(YEAR))
os.ann.station.sum2$Station<-as.factor(os.ann.station.sum2$Station)

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

os_map1<-ggplot(os.ann.station.sum, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=os.ann.station.sum, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, size=StationKGHA, color=StationKGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), limits=c(0,30), breaks=seq(0,30, by=5))+
  scale_color_continuous(high='red',low='cadetblue2',name=expression(underline('Biomass (kg per ha)')), limits=c(0,30), 
                         breaks=seq(0,30, by=5))+
  map_theme+
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Offshore Fish Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

os_map1<-os_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), size=8, family='serif')+
  transition_manual(YEAR)

os_map1_gif<-animate(os_map1, fps = 3, end_pause = 10, width = 1000, height = 500,renderer = gifski_renderer(loop=F))
os_map1_gif
anim_save(here('Plots and Tables/RVCAT','Animated_os_station_biomass_gif.gif'))

os.ann.mean<-aggregate(os.ann.station.sum$StationKGHA, by=list(YEAR=os.ann.station.sum$YEAR), FUN=mean)%>%
  renameCol('x','Mean.KGHA')

os_bars1<-ggplot(os.ann.mean, aes(x=YEAR, y=Mean.KGHA))+
  geom_bar(stat='identity', fill='grey80')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks=seq(2011,2020,by=2))+
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

os_bars1_gif<-animate(os_bars1, fps = 3, end_pause = 10, width = 350, height = 500,renderer = gifski_renderer(loop=F))

p_mgif<-image_read(os_map1_gif)
q_mgif<-image_read(os_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:9){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_os_ann_biomass_map_bars.gif'))

##to plot siscowet, kiyi and deepwater sculpin biomasses---------------------------------------------------OFFSHORE SPP BIOMASS-----
os<-subset(all.data, TARGET==118|TARGET==117 & YEAR>2010)%>%
  filter(TR_DESIGN==25|TR_DESIGN==4)%>%
  filter(END_DEPTH>84)

os.spp.compare<-subset(os, SPECIES==206|SPECIES==308|SPECIES==904)%>% 
  select(YEAR, LOCATION, SPECIES, KGHA)

##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year
os.sites.byyear<-os%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
os.sites.byyear$SITECHECK<-'SURVEYED'

os.spp.compare2<-merge.data.frame(os.spp.compare, os.sites.byyear, all=TRUE)
os.spp.compare2<-cast(os.spp.compare2, YEAR+LOCATION+SITECHECK~SPECIES, value='KGHA', fun.aggregate='mean')%>%
  select(c(1,2,4:6))
os.spp.compare2[is.na(os.spp.compare2)]<-0
os.spp.compare3<-melt.data.frame(os.spp.compare2, id.vars=c('YEAR','LOCATION'))%>%
  renameCol('value','KGHA')%>%
  renameCol('variable','SPECIES')

##means and st error calculations
os.spp.compare.mean<-os.spp.compare3%>%
  group_by(YEAR, SPECIES)%>%
  do(describe(.$KGHA))

##merge to get species names
os.spp.compare.mean<-merge.data.frame(os.spp.compare.mean, codes.to.names, all=F)

##spp means
os.means<-os.spp.compare.mean%>%
  group_by(COMMON_NAME)%>%
  dplyr::summarize(spp.mean=mean(mean))

##plot trends in selected species
ggplot(os.spp.compare.mean, aes(x=YEAR, y=mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_hline(data=os.means, aes(yintercept=spp.mean), size=1.5)+
  facet_grid(.~COMMON_NAME)+ ##in future years if this needs to be flipped so the facets are stacked vertically: (COMMON_NAME~.)
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)', caption=ann_data_access,
       title='Lake Superior Offshore Fish Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(2011,2013,2015,2017,2019,2021,2023), 
                     labels=c('2011','2013','2015','2017','2019','2021','2023'),
                     limits=c(2010, 2020))+
  scale_y_continuous(expand=c(0,0), breaks=c(0,1,2,3,4,5),
                     labels=c('0', '1', '2','3','4','5'), limits=c(0,5))+
  geom_errorbar(data=os.spp.compare.mean, aes(x=YEAR, ymin=mean-se, ymax=mean+se), width=0.4)

ggsave(here('Plots and Tables/RVCAT','os_spp_biomass_annual.png'), dpi = 300, width = 40, height = 20, units = "cm")

##Chequamegon Bay######################################################################################################Cheq Bay#####
cheq<-subset(all.data, TARGET==106)

##calculate mean biomass by year, start by getting total kgha for each station
cheq.ann.station.sum<-aggregate(cheq$NOHA, by=list(YEAR=cheq$YEAR, Station=cheq$LOCATION), FUN=sum)%>%
  renameCol('x','StationNOHA')

##calculate summary statistics
cheq.summary <- cheq.ann.station.sum%>% 
  group_by(YEAR) %>% 
  summarise_all(funs(mean,median,sd,std.error))%>%
  select(c(1,3,5,7,9))

##plot mean cheq bay abundance
ggplot(cheq.summary, aes(x=YEAR, y=StationNOHA_mean))+
  geom_bar(stat='identity', fill='grey75', color='black')+
  plot_theme+
  labs(x='Year', y='Mean abundance (n per ha)',
       caption=ann_data_access,
       title='Chequamegon Bay Annual Fish Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), breaks=c(1975, 1980,1985,1990,1995,2000,2005,2010,2015,2020,2025), 
                     labels=c('1975', '1980','1985','1990','1995','2000','2005','2010','2015','2020','2025'))+
  geom_errorbar(data=cheq.summary, aes(x=YEAR, ymin=StationNOHA_mean-StationNOHA_std.error, 
                                     ymax=StationNOHA_mean+StationNOHA_std.error),
                width=0.4)+
  scale_y_continuous(expand=c(0,0))

ggsave(here('Plots and Tables/RVCAT','cb_abundance_annual.png'), dpi = 300, width = 40, height = 20, units = "cm")

##Cheq Bay Species Trends-------------------------------------------------------------------------------Cheq Bay Species Trends-----
cheq<-subset(all.data, TARGET==106)
cheq.spp.compare<-subset(cheq, SPECIES==130|SPECIES==508|SPECIES==801)%>%  ##choose the species you want here
  select(YEAR, LOCATION, SPECIES, NOHA)

##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year
cheq.sites.byyear<-cheq%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
cheq.sites.byyear$SITECHECK<-'SURVEYED'

cheq.spp.compare2<-merge.data.frame(cheq.spp.compare, cheq.sites.byyear, all=TRUE)
cheq.spp.compare2<-cast(cheq.spp.compare2, YEAR+LOCATION+SITECHECK~SPECIES, value='NOHA', fun.aggregate='mean')%>%
  select(c(1,2,4:6)) ##note, if you change the # of spp being compared, you will need to change the selection of cols here 
#(2 spp=1,2,4,5), 3 spp= 1,2,4:6, 4 spp= 1,2,4:7, etc.
cheq.spp.compare2[is.na(cheq.spp.compare2)]<-0
cheq.spp.compare3<-melt.data.frame(cheq.spp.compare2, id.vars=c('YEAR','LOCATION'))%>%
  renameCol('value','NOHA')%>%
  renameCol('variable','SPECIES')

##means and st error calculations
cheq.spp.compare.mean<-cheq.spp.compare3%>%
  group_by(YEAR, SPECIES)%>%
  do(describe(.$NOHA))

##merge to get species names
cheq.spp.compare.mean<-merge.data.frame(cheq.spp.compare.mean, codes.to.names, all=F)

##plot trends in selected species
ggplot(cheq.spp.compare.mean, aes(x=YEAR, y=mean, color=COMMON_NAME))+
  geom_point()+
  geom_line()+
  theme(legend.position='top')+
  plot_theme+
  labs(x='Year', y='Mean abundance (n per ha)',
       caption=ann_data_access,
       title='Chequamegon Bay Annual Fish Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), limits=c(1973, 2021))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_viridis(discrete=TRUE, name='')

ggsave(here('Plots and Tables/RVCAT','cb_spp_abundance_lines.png'), dpi = 300, width = 40, height = 20, units = "cm")


##sample site map########################################################################################################MAP##########
##NOTE: need to have the shapefiles folder in your working directory for these to work

ls_poly <- readOGR(dsn =here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##subset out the current year, and nearshore and offshore targets (if you want Cheq Bay, can add that target code)
all.current.yr<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)
all.current.yr$TARGET_f<-as.factor(all.current.yr$TARGET)

ggplot(all.current.yr, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=all.current.yr, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=TARGET_f), size=6, stroke=1.5)+
  scale_color_manual(values=c('salmon','cadetblue2'), name='Survey', labels=c('Nearshore','Offshore'))+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Stations Sampled',
       subtitle=(paste('USGS bottom trawl assessment, ', all.current.yr$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


##Chequamegon Bay trawl map--------------------------------------------------------------------------------------Cheq Bay Map---------
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

cheq<-subset(all.data, TARGET==106)

cheq.current.yr<-cheq %>%
  filter(YEAR==max(YEAR))

ggplot(cheq.current.yr, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 1)+
  geom_segment(data=cheq.current.yr, aes(x=BEG_LONGITUDE_DD, xend=END_LONGITUDE_DD, 
                                         y=BEG_LATITUDE_DD, yend=END_LATITUDE_DD), size=2, color='sienna1', lineend='round')+
  map_theme+
  labs(caption=ann_data_access,
       title='Chequamegon Bay Bottom Trawl Locations',
       subtitle=(paste('USGS bottom trawl assessment, ', cheq.current.yr$YEAR)))

ggsave(here('Plots and Tables/RVCAT','cb_CurrentYear_Sites.png'), dpi = 300, width = 35, height = 25, units = "cm")

##summary data tables############################################################################################summary tables#####
all.current.yr<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)
all.current.yr$TARGET_f<-as.factor(all.current.yr$TARGET)

all.spp.counts<-aggregate(all.current.yr$NUM, by=list(SPECIES=all.current.yr$SPECIES, TARGET=all.current.yr$TARGET), FUN=sum)%>%
  renameCol('x','SUM')
all.spp.counts<-merge.data.frame(all.spp.counts, sci.names)
all.spp.counts<-select(all.spp.counts, c(4,5,2,3))
all.spp.counts2<-cast(all.spp.counts, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
all.spp.counts2<-all.spp.counts2%>%
  renameCol('COMMON_NAME','Common name')%>%
  renameCol('SCI_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
all.spp.counts2[is.na(all.spp.counts2)]<-0

write.xlsx(all.spp.counts2, here('Plots and Tables/RVCAT','Current_yr_catch_totals.xlsx'), row.names=F)

##to make summary of chequamegon bay catch totals by species
cheq.current.yr<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==106)
cheq.current.yr$TARGET_f<-as.factor(cheq.current.yr$TARGET)

cheq.spp.counts<-aggregate(cheq.current.yr$NUM, by=list(SPECIES=cheq.current.yr$SPECIES, TARGET=cheq.current.yr$TARGET), FUN=sum)%>%
  renameCol('x','SUM')
cheq.spp.counts<-merge.data.frame(cheq.spp.counts, sci.names)
cheq.spp.counts<-select(cheq.spp.counts, c(4,5,2,3))
cheq.spp.counts2<-cast(cheq.spp.counts, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
cheq.spp.counts2<-cheq.spp.counts2%>%
  renameCol('COMMON_NAME','Common name')%>%
  renameCol('SCI_NAME','Scientific name')%>%
  renameCol('106','Chequamegon Bay')
cheq.spp.counts2[is.na(cheq.spp.counts2)]<-0

write.xlsx(cheq.spp.counts2, here('Plots and Tables/RVCAT','Current_yr_catch_totals_chequamegon.xlsx'), row.names=F)

##table of mean annual nearshore bottom trawl biomass for common fishes---------------------------------------all yrs summary table----
ns<-subset(all.data, TARGET==2 & YEAR>1977)

ns.sites.byyear<-ns%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
ns.sites.byyear$SITECHECK<-'SURVEYED'

ns.summary<-select(ns, c(4,8,29,35))
ns.unique.sites<-ns.sites.byyear%>% ##to calculate the number of sites sampled during the nearshore cruise each year
  group_by(YEAR)%>%
  dplyr::count('LOCATION')%>%
  select(c(1,3))%>%
  renameCol('n','Sites')
ns.total.biomass<-aggregate(ns.summary$KGHA, by=list(YEAR=ns.summary$YEAR, LOCATION=ns.summary$LOCATION), FUN=sum)%>% 
  renameCol('x', 'SiteSum')
ns.zero.sites<-subset(ns.total.biomass, SiteSum==0) ##to calculate the number of sites where no fish were caught
ns.zero.sites<-ns.zero.sites%>%
  group_by(YEAR)%>%
  dplyr::count()%>%
  renameCol('n','No fish sites')
ns.total.biomass2<-aggregate(ns.total.biomass$SiteSum, by=list(YEAR=ns.total.biomass$YEAR), FUN=mean)%>% ##mean total biomass
  renameCol('x','Total biomass')
ns.median.biomass<-aggregate(ns.total.biomass$SiteSum, by=list(YEAR=ns.total.biomass$YEAR), FUN=median)%>% ##median total biomass
  renameCol('x','Median biomass')
ns.spp.count<-merge.data.frame(ns.summary, sci.names) ##to calculate the mean biomass of various common species
ns.spp.count<-select(ns.spp.count, c(2:5))%>%
  renameCol('COMMON_NAME','SPECIES')
ns.spp.count<-merge.data.frame(ns.spp.count, ns.sites.byyear, by=c('YEAR','LOCATION'), all=T)
ns.spp.count<-cast(ns.spp.count, YEAR+LOCATION+SITECHECK~SPECIES, value='KGHA', fun.aggregate='mean')
ns.spp.count[is.na(ns.spp.count)]<-0
ns.spp.count<-select(ns.spp.count, c(1, 4:48))
ns.spp.count2<-aggregate(.~YEAR, ns.spp.count, FUN=mean)

##to change which species you calculate average biomass for, change in the below lines
##note, sculpins are summed together, as are misc. spp.--this can be changed by moving those species into the ns.spp.common data frame
ns.spp.common<-select(ns.spp.count2, c('YEAR','Rainbow Smelt','Cisco','Lake Whitefish','Bloater','hatchery Lake Trout',
                                       'lean Lake Trout','siscowet Lake Trout','Burbot'))
ns.sculpins<-select(ns.spp.count2, c('YEAR','Slimy Sculpin','Spoonhead Sculpin','Deepwater Sculpin'))
ns.sculpins<-ns.sculpins%>%
  mutate(Sculpins = select(., c(2:4))%>%
           rowSums(na.rm=F))%>%
  select(c(1,5))
ns.misc.spp<-select(ns.spp.count2, c('YEAR', 'Ninespine Stickleback','Trout-perch','Kiyi','Shortjaw Cisco','Pygmy Whitefish',
                                     'Round Whitefish','Longnose Sucker'))
ns.misc.spp<-ns.misc.spp%>%
  mutate(Misc.Spp = select(., c(2:8))%>%
  rowSums(na.rm=F))%>%
  select(c(1,9))
ns.num<-select(ns, c(4,8,29,30))
ns.tot.spp.caught<-subset(ns.num, NUM>0) ##calculate the total number of species caught each year
ns.tot.spp.caught$spp2<-as.numeric(as.character(ns.tot.spp.caught$SPECIES))
ns.tot.spp.caught<-subset(ns.tot.spp.caught, spp2<999)
ns.tot.spp.caught<-select(ns.tot.spp.caught, c(1,3))
ns.tot.spp.caught<-unique(ns.tot.spp.caught)
ns.tot.spp.caught<-aggregate(ns.tot.spp.caught$SPECIES, by=list(YEAR=ns.tot.spp.caught$YEAR), FUN=length)%>%
  renameCol('x','Total species collected')

##merge all of the variables calculated above together into one data frame to export
ns.table<-merge.data.frame(ns.unique.sites,ns.zero.sites, all=T)
ns.table[is.na(ns.table)]<-0
ns.table<-merge.data.frame(ns.table, ns.tot.spp.caught)
ns.table<-merge.data.frame(ns.table, ns.total.biomass2)
ns.table<-merge.data.frame(ns.table, ns.median.biomass)
ns.table<-merge.data.frame(ns.table, ns.spp.common)
ns.table<-merge.data.frame(ns.table, ns.sculpins)
ns.table<-merge.data.frame(ns.table, ns.misc.spp)
ns.table<-renameCol(ns.table, 'YEAR','Year')
mean(ns.table$`Total biomass`)
ns.table<-round(ns.table[,c(1:16)], 2)

write.xlsx(ns.table, here('Plots and Tables/RVCAT','ns_biomass_summary.xlsx'), row.names=F)

##Lengths file to evaluate Age-1 densities###########################################################################Age-1 Densities###

lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
op.id.details<-select(all.data, c(1,2,5,8,11,17,36))
lengths2<-merge.data.frame(lengths, op.id.details, by='OP_ID', all=F)
lengths3<-unique(lengths2)


##subset age 1 fish by species. use nearshore catches for all species except kiyi
rbs.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==109)%>%
  filter(LENGTH<101)%>%
  filter(TARGET==2)
cisco.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==202)%>%
  filter(LENGTH<141)%>%
  filter(TARGET==2)
bloater.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==204)%>%
  filter(LENGTH<131)%>%
  filter(TARGET==2)
kiyi.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==206)%>%
  filter(LENGTH<131)%>%
  filter(TARGET==118|TARGET==117)%>%
  filter(END_DEPTH>84)
siscowet.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==308)%>%
  filter(LENGTH<226)%>%
  filter(TARGET==2)
lean.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==317)%>%
  filter(LENGTH<226)%>%
  filter(TARGET==2)
lwf.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==203)%>%
  filter(LENGTH<161)%>%
  filter(TARGET==2)

age1<-rbind(rbs.a1, cisco.a1, bloater.a1, siscowet.a1, lean.a1, lwf.a1) ##bind all species evaluated using nearshore data
##need to process kiyi separate from the nearshore species so you can cast in zeros for nearshore sites where age 1 fish were not caught
age1.station.sum<-aggregate(age1$EXP_N, by=list(YEAR=age1$YEAR, LOCATION=age1$LOCATION, SPECIES=age1$SPECIES,
                                                HA_SWEPT=age1$HA_SWEPT), FUN=sum)%>%
  renameCol('x','N.Age1')

ns<-subset(all.data, TARGET==2 & YEAR>1976)
##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year
ns.sites.byyear<-ns%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
ns.sites.byyear$SITECHECK<-'SURVEYED'
os<-subset(all.data, TARGET==118|TARGET==117 & YEAR>2010)%>%
  filter(TR_DESIGN==25|TR_DESIGN==4)%>%
  filter(END_DEPTH>84)
##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year. offshore=for kiyi calculations
os.sites.byyear<-os%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
os.sites.byyear$SITECHECK<-'SURVEYED'

age1.station.sum<-merge.data.frame(age1.station.sum, ns.sites.byyear, all=T)
age1.station.sum$NOHA<-age1.station.sum$N.Age1/age1.station.sum$HA_SWEPT
age1.cast<-cast(age1.station.sum, YEAR+LOCATION~SPECIES, value='NOHA', fun.aggregate = 'mean')
age1.cast[is.na(age1.cast)]<-0
age1.cast<-select(age1.cast, c(1:8))
age1.cast<-melt(age1.cast, id=c('YEAR', 'LOCATION')) ##use this file later to map species densities
age1.ann.mean<-aggregate(age1.cast$value, by=list(Year=age1.cast$YEAR, SPECIES=age1.cast$SPECIES), FUN=mean)%>%
  renameCol('x','Mean.NOHA')

##now calculate kiyi (and any other offshore fishes if added later)
kiyi.a1<-aggregate(kiyi.a1$EXP_N,  by=list(YEAR=kiyi.a1$YEAR, LOCATION=kiyi.a1$LOCATION, SPECIES=kiyi.a1$SPECIES,
                                          HA_SWEPT=kiyi.a1$HA_SWEPT), FUN=sum)%>%
  renameCol('x','N.Age1')
kiyi.a1<-merge.data.frame(kiyi.a1, os.sites.byyear, all=T)
kiyi.a1$NOHA<-kiyi.a1$N.Age1/kiyi.a1$HA_SWEPT
kiyi.age1.cast<-cast(kiyi.a1, YEAR+LOCATION~SPECIES, value='NOHA', fun.aggregate = 'mean')
kiyi.age1.cast[is.na(kiyi.age1.cast)]<-0
kiyi.age1.cast<-select(kiyi.age1.cast, c(1:3))
kiyi.age1.cast<-melt(kiyi.age1.cast, id=c('YEAR', 'LOCATION'))

kiyi.a1.ann.mean<-aggregate(kiyi.age1.cast$value, by=list(Year=kiyi.age1.cast$YEAR, SPECIES=kiyi.age1.cast$SPECIES), FUN=mean)%>%
  renameCol('x','Mean.NOHA')

##bind together nearshore and offshore calculations and format for table export 
age1.ann.mean<-rbind(age1.ann.mean, kiyi.a1.ann.mean)
age1.ann.mean<-merge.data.frame(age1.ann.mean, sci.names, all=F)%>%
  select(c(2,3,4))
age1.ann.mean2<-cast(age1.ann.mean, Year~COMMON_NAME, value='Mean.NOHA')
age1.ann.mean2$'Year Class'<-age1.ann.mean2$Year-1
  
age1.table<-select(age1.ann.mean2, c('Year','Year Class','Rainbow Smelt','Cisco','Bloater','Lake Whitefish','Kiyi','lean Lake Trout',
                                     'siscowet Lake Trout')) ##if any spp added in future, select them in the order you want here
age1.table<-round(age1.table, 2) ##if you want more or less decimal places, adjust the numeric value here to the number you want
age1.table<-age1.table%>%
  filter(Year>1977) ##select the most recent 40 years, or whatever time period is desired
age1.table[is.na(age1.table)]<-'N/A' ##kiyi will have N/A values for years prior to the start of the offshore cruise (2011)

write.xlsx(age1.table, here('Plots and Tables/RVCAT','ns_Age1_summary.xlsx'), row.names = F)

##plot of recent age-1 ciscoe densities
a1.ciscoes<-age1.ann.mean%>%
  filter(COMMON_NAME=='Bloater'|COMMON_NAME=='Cisco'|COMMON_NAME=='Kiyi')%>%
  filter(Year>1977) ##NOTE: if you select any years before 2011, kiyi will not have data bc it's calculated w/offshore sites

ggplot(a1.ciscoes, aes(x=(Year-1), y=Mean.NOHA))+
  geom_bar(stat='identity', fill='grey75',color='black')+
  facet_grid(COMMON_NAME~., scales='free')+
  plot_theme+
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Ciscoe Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_color_viridis(discrete=T, name='')

ggsave(here('Plots and Tables/RVCAT','ns_Age1_ciscoes.png'), dpi = 300, width = 40, height = 20, units = "cm")

ggplot(a1.ciscoes, aes(x=(Year-1), y=Mean.NOHA))+
  geom_point(size=6)+
  geom_segment(aes(x=Year-1, xend=Year-1, y=0, yend=Mean.NOHA), size=1, color='black')+
  facet_grid(COMMON_NAME~., scales='free_y')+
  plot_theme+
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Ciscoe Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), limits=c(1977,max(a1.ciscoes$Year)), breaks=seq(1977,max(a1.ciscoes$Year), by=5))+
  scale_y_continuous(expand=c(0,0),breaks = scales::pretty_breaks(5))+
  theme(panel.spacing=unit(2,'lines'))+
  coord_cartesian(clip='off')+
  geom_hline(yintercept=0, color='black', size=1)

ggsave(here('Plots and Tables/RVCAT','ns_Age1_ciscoes_lollipop.png'), dpi = 300, width = 40, height = 20, units = "cm")

a1.bloater.cisco<-a1.ciscoes%>%
  filter(COMMON_NAME=='Bloater'| COMMON_NAME=='Cisco')
ggplot(a1.bloater.cisco, aes(x=(Year-1), y=Mean.NOHA))+
  geom_point(size=6)+
  geom_segment(aes(x=Year-1, xend=Year-1, y=0, yend=Mean.NOHA), size=1, color='black')+
  facet_grid(COMMON_NAME~., scales='free_y')+
  plot_theme+
  labs(x='Year Class', y='Age-1 Abundance (number/hectare)', caption=ann_data_access,
       title='Lake Superior Age-1 Bloater and Cisco Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(expand=c(0,0), limits=c(1977,max(a1.ciscoes$Year)), breaks=seq(1977,max(a1.ciscoes$Year), by=5))+
  scale_y_continuous(expand=c(0,0),breaks = scales::pretty_breaks(5))+
  theme(panel.spacing=unit(2,'lines'))+
  coord_cartesian(clip='off')+
  geom_hline(yintercept=0, color='black', size=1)

ggsave(here('Plots and Tables/RVCAT','ns_Age1_bl_ci_lollipop.png'), dpi = 300, width = 40, height = 20, units = "cm")

##map of age-1 cisco and bloater densities
a1.cisco<-age1.cast%>%
  filter(SPECIES==202|SPECIES==204)%>% ##enter other species codes here if you want to map a different species
  filter(YEAR>1977) ##select the years you want to map here
a1.cisco<-aggregate(a1.cisco$value, by=list(YEAR=a1.cisco$YEAR, LOCATION=a1.cisco$LOCATION), FUN=sum)%>%
  renameCol('x','value')
latlong<-select(all.data, c(4,5,8,12,13))
latlong<-latlong%>%
  filter(TARGET==2)
latlong<-latlong[!duplicated(latlong$LOCATION),] ##get coordinates for each site 
latlong<-select(latlong, c(2:5))

a1.cisco2<-merge.data.frame(a1.cisco, latlong, by='LOCATION')
a1.cisco2$YrClass<-a1.cisco2$YEAR-1

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

cisco<-ggplot(a1.cisco2, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ ##path for lake outline
  geom_point(data=a1.cisco2, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=log(value), size=log(value)))+ 
  theme_bw() + 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  map_theme +
  theme(legend.box='horizontal',
        legend.position=c(0.12,0.72))+
  guides(size=guide_legend(reverse=T, keywidth = 5),
         color=guide_legend(reverse=T, keywidth = 5))+
  scale_color_continuous(high='red',low='cadetblue2', limits=c(-1,10), breaks=seq(-1,10, by=1.5)) +
  scale_size_continuous(limits=c(-1,10), breaks=seq(-1,10, by=1.5))+
  labs(title='Lake Superior Age-1 Cisco and Bloater Abundance', 
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access,
       size=expression(underline('Abundance (ln(n per ha))')), 
       color=expression(underline('Abundance (ln(n per ha))')))+
  transition_manual(YrClass)

cisco<-cisco+geom_text(aes(x=-85, y=48.75, label=paste('Year Class:',YrClass, sep='\n')), size=8, family='serif')+
  transition_manual(YrClass)

a1c_gif<-animate(cisco, fps = .5, end_pause = 15, nframes=57, ##NOTE: as more years are added, need to increase nframes to the # years
               width = 900, height = 500, renderer = gifski_renderer(loop=T))
a1c_gif
anim_save(here('Plots and Tables/RVCAT','Animated_A1Cisco_map.gif')) ##saves just map, before adding other plots

a1.cisco3<-aggregate(a1.cisco2$value, by=list(YrClass=a1.cisco2$YrClass), FUN=mean)%>%
  renameCol('x','value')
a1c_bars<-ggplot(a1.cisco3, aes(x=YrClass, y=value))+
  geom_col(fill='gray80', color='black')+
  geom_point(size=3, color='black', shape=18)+
  geom_hline(yintercept=0, size=1)+
  plot_theme+
  theme(legend.position=c(0.8,0.8),
        legend.title.align = 0.5)+
  scale_y_continuous(limits=c(-1,800))+
  labs(y='Lakewide mean abundance (n per ha)', x='Year Class', title=' ', 
       caption='  ', subtitle='  ')+
  transition_manual(YrClass, cumulative=T)
a1c_bars_gif<-animate(a1c_bars, fps=.5, nframes=57, end_pause=15, renderer = gifski_renderer(loop=T),
                      width=350, height=500) ##NOTE: as more years are added, need to increase nframes to the # years
a1c_bars_gif

p_mgif<-image_read(a1c_gif)
q_mgif<-image_read(a1c_bars_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:57){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_ns_Age1_cisco_map_bars.gif'))


##lollipop plot of bloater and cisco age-1 densities
ggplot(a1.cisco3, aes(x=YrClass, y=value, fill=value))+
  geom_hline(yintercept=0, color='black',size=1)+
  geom_segment(aes(x=YrClass, xend=YrClass, y=0, yend=value), size=1, color='black')+
  geom_point(size=6, shape=21)+
  scale_fill_continuous(high='red',low='cadetblue2', name='Abundance\n(n per ha)')+
  plot_theme+
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)',
       title='Lake Superior Age-1 Cisco and Bloater Abundance',
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  theme(axis.line.x=element_blank(),
        legend.position=c(0.9,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_Age1_bloater_cisco_lollipop.png'), dpi = 300, width = 40, height = 20, units = "cm")


##age-1 ciscoes by station for the current year
a1.current<-rbind(age1.cast, kiyi.age1.cast)
a1.current<-a1.current%>%
  filter(SPECIES==202|SPECIES==204|SPECIES==206)%>%
  filter(YEAR==max(YEAR))
dates<-all.data%>%
  filter(YEAR==max(YEAR))
dates<-select(dates, c(2,8, 12, 13))
dates<-dates[!duplicated(dates$LOCATION),]
a1.current<-merge.data.frame(a1.current, dates, all=F)
a1.current<-merge.data.frame(a1.current, sci.names)
a1.current$station<-as.factor(a1.current$LOCATION)
a1.current$a1presence<-if_else(a1.current$value>0,'present','absent')

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##create map with stations color coded by biomass
a1map<-ggplot(a1.current, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=a1.current, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=a1presence), size=6, stroke=1.5)+
  scale_color_manual(values=c('gray85','red'), name='Age-1 occurrence')+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs( x='Longitude', y='Latitude')

ggsave(plot=a1map,here('Plots and Tables/RVCAT','ns_Age1_sites_abundance_map.png'), dpi = 300, width = 30, height = 16, units = "cm")

a1map2<-ggplot(a1.current, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=a1.current, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD, color=a1presence), size=6, stroke=1.5)+
  scale_color_manual(values=c('gray85','red'), name='Age-1 occurrence')+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Age-1 Ciscoe Occurrence',
        subtitle=(paste('USGS bottom trawl assessment, ', a1.current$YEAR)),
        caption=ann_data_access)

ggsave(plot=a1map2,here('Plots and Tables/RVCAT','ns_Age1_sites_abundance_map2.png'), dpi = 300, width = 30, height = 16, units = "cm")

ggplot(a1.current, aes(x=station, y=value, fill=COMMON_NAME))+
  geom_bar(position='stack', stat='identity')+
  aes(x=fct_reorder(station, OP_DATE))+
  annotation_custom(ggplotGrob(a1map), xmin=20, xmax=100, ymin=35, ymax=240)+
  plot_theme+
  theme(legend.position='bottom',
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=12))+
  labs(x='Station, ordered by sampling date', y='Abundance (n per ha)', 
       title='Lake Superior Age-1 Ciscoe Abundance',
       subtitle=(paste('USGS bottom trawl assessment, ', a1.current$YEAR)),
       caption=ann_data_access)+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values=c('darkorchid4','turquoise','darkorange1'), name=' ')

ggsave(here('Plots and Tables/RVCAT','ns_Age1_ciscoes_bydate.png'), height=20, width=40, dpi=300, units='cm')

##Animated map of sites sampled in the current year and corresponding fish biomass and diversity#####################################

all.current.yr<-all.data%>%
  filter(YEAR==max(YEAR))%>% ##if you want a year other than the most recent, change this to YEAR==desired year (ie YEAR==2010)
  filter(TARGET==2|TARGET==117|TARGET==118)
all.current.yr$TARGET_f<-as.factor(all.current.yr$TARGET)
all.current.yr.totals<-aggregate(all.current.yr$NUM, by=list(BEG_LATITUDE_DD=all.current.yr$BEG_LATITUDE_DD,
                                                         BEG_LONGITUDE_DD=all.current.yr$BEG_LONGITUDE_DD,
                                                         TARGET=all.current.yr$TARGET_f,
                                                         DATE=all.current.yr$OP_DATE,
                                                         TIME=all.current.yr$TIME), FUN=sum)%>%
  renameCol('x','NUM')
all.current.yr.totals<-all.current.yr.totals[order(all.current.yr.totals$DATE),]
all.current.yr.totals$Order<-1:nrow(all.current.yr.totals)

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

p<-ggplot(all.current.yr.totals, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ ##path for lake outline
  geom_path(data=all.current.yr.totals, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD), size=1)+ ##path for lines connecting sites
  geom_point(aes(group=seq_along(Order), color=TARGET), size=6, alpha=0.6)+ 
  theme_bw() + 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  map_theme +
  scale_color_viridis(discrete=T, labels=c('Nearshore Cruise', 'Offshore Cruise'), name='', begin=0,end=0.6) +
  labs(title='Lake Superior Fish Collections',
       subtitle='USGS bottom trawl assessment, 2019',
       caption=ann_data_access)+ ##change title to current year
  transition_reveal(Order)+
  theme(legend.position=c(0.1,0.9))

r<-p+geom_text(aes(x=-85, y=49, label=DATE), size=8, family='serif')+
  transition_reveal(Order)

p_gif<-animate(r, fps = 5, end_pause = 60, nframes=150, 
               width = 1000, height = 500, renderer = gifski_renderer(loop=T))
anim_save(here('Plots and Tables/RVCAT','Animated_CurrentYear_map.gif')) ##saves just map, before adding other plots

all.current.yr.totals$Fish.Caught<-'Fish Caught'

all.current.yr.totals %>%
  group_by(Fish.Caught) %>%
  mutate(space_below = cumsum(lag(NUM, default = 0))) %>%
  ungroup() %>%
  
ggplot() +
  geom_tile(aes(x=Fish.Caught, y=(space_below + NUM/2),  
                height = NUM,
                fill = TARGET), width = 0.9) +
  scale_fill_viridis(discrete=T, begin=0,end=0.6)+
  scale_y_continuous(labels = scales::comma, expand=c(0,0), limits=c(0,40000))+ ##need to adjust limits on the y axis depending on catch
  scale_x_discrete(expand=c(0,0), labels=c('Fish\nCaught'))+
  theme(legend.position = "none")+
  plot_theme+
  labs(x='',y='Running total: fish caught',title=' ')+
  transition_time(Order)+
  shadow_mark(past=T, future=F)->   anim_bar

#animate(anim_bar, fps = 20)
q_gif<-animate(anim_bar, fps = 5, end_pause = 60, nframes=150, 
               width = 200, height = 500, renderer = gifski_renderer(loop=T))  
q_gif
##for first occurrence of each species
unique.spp<-all.current.yr[match(unique(all.current.yr$SPECIES), all.current.yr$SPECIES),]
unique.spp<-aggregate(unique.spp$SPECIES, by=list(BEG_LATITUDE_DD=unique.spp$BEG_LATITUDE_DD,
                                                  BEG_LONGITUDE_DD=unique.spp$BEG_LONGITUDE_DD), FUN=length)%>%
  renameCol('x','New.Spp.Caught')

all.current.yr.totals2<-merge.data.frame(all.current.yr.totals, unique.spp, all=T)
all.current.yr.totals2[is.na(all.current.yr.totals2)]<-0
all.current.yr.totals2<-all.current.yr.totals2[order(all.current.yr.totals2$Order),]

all.current.yr.totals2 %>%
  group_by(Fish.Caught) %>%
  mutate(space_below = cumsum(lag(New.Spp.Caught, default = 0))) %>%
  ungroup() %>%
  
  ggplot() +
  geom_tile(aes(x=Fish.Caught, y=(space_below + New.Spp.Caught/2),  
                height = New.Spp.Caught,
                fill = TARGET), width = 0.9) +
  scale_fill_viridis(discrete=T, begin=0,end=0.6)+
  scale_y_continuous(expand=c(0,0), limits=c(0,30))+ ##need to adjust limits on y axis depending on catch
  scale_x_discrete(expand=c(0,0), labels=c('Species\nCaught'))+
  theme(legend.position = "none")+
  plot_theme+
  labs(x='',y='Running total: unique species caught',title=' ')+
  transition_time(Order)+
  shadow_mark(past=T, future=F)->   anim_bar

#animate(anim_bar, fps = 20)
u_gif<-animate(anim_bar, fps = 5, end_pause = 60, nframes=150, 
               width = 150, height = 500, renderer = gifski_renderer(loop=T)) 
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


##animate bar plots of cumulative annual mean biomass for nearshore and offshore cruises---------------------------------------------
os.cumulate<-subset(all.current.yr, TARGET==117|TARGET==118)
os.cumulate<-aggregate(os.cumulate$KGHA, by=list(DATE=os.cumulate$OP_DATE, LOCATION=os.cumulate$LOCATION), FUN=sum)%>%
  renameCol('x','mean.kgha')
os.cumulate<-os.cumulate[order(os.cumulate$DATE),]
os.cumulate$Order<-1:nrow(os.cumulate)
os.cumulate$cum.mean<-cummean(os.cumulate$mean.kgha)
os.cumulate$Target<-'Offshore'
os.cumulate$Order<-os.cumulate$Order+76

ns.cumulate<-subset(all.current.yr, TARGET==2)
ns.cumulate<-aggregate(ns.cumulate$KGHA, by=list(DATE=ns.cumulate$OP_DATE, LOCATION=ns.cumulate$LOCATION), FUN=sum)%>%
  renameCol('x','mean.kgha')
ns.cumulate<-ns.cumulate[order(ns.cumulate$DATE),]
ns.cumulate$Order<-1:nrow(ns.cumulate)
ns.cumulate$cum.mean<-cummean(ns.cumulate$mean.kgha)
ns.cumulate$Target<-'Nearshore'

cumulate<-rbind(ns.cumulate, os.cumulate)

r<-ggplot(cumulate, aes(x = Target, y = cum.mean, fill=Target)) +
  geom_tile(aes(y = cum.mean / 2, height = cum.mean), width = 0.9) +
  scale_fill_viridis(discrete=T, begin=0,end=0.6, guide=F)+
  transition_reveal(Order) +
  theme(legend.position = "none")+
  plot_theme+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs( x='', y='Lakewide mean biomass (kg per ha)', title='  ', caption='  ')

r_gif<-animate(r, nframes = 150, fps = 5, end_pause = 60, width = 500, height = 525)

r_gif

p<-ggplot(all.current.yr.totals, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ ##path for lake outline
  geom_path(data=all.current.yr.totals, mapping=aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD), size=1)+ ##path for lines connecting sites
  geom_point(aes(group=seq_along(Order), color=TARGET), size=6, alpha=0.6)+ 
  theme_bw() + 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  map_theme +
  scale_color_viridis(discrete=T, labels=c('Nearshore Cruise', 'Offshore Cruise'), name='', begin=0,end=0.6) +
  labs(title='Lake Superior Fish Collections',
       subtitle='USGS bottom trawl assessment, 2019',
       caption=ann_data_access)+ ##change title to current year
  transition_reveal(Order)+
  theme(legend.position=c(0.1,0.9))

t<-p+geom_text(aes(x=-85, y=49, label=DATE), size=8, family='serif')+
  transition_reveal(Order)

t_gif<-animate(t, fps = 5, end_pause = 60, nframes=150, 
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

##effort summary table############################################################################################effort table#####
effort<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118|TARGET==106)
effort$Depth_Range<-abs(effort$END_DEPTH-effort$BEG_DEPTH)
effort.unique<-effort[!duplicated(effort[,c('OP_ID')]),]

effort.trawldist<-effort.unique%>%
  group_by(TARGET)%>%
  do(describe(.$DISTANCE, ranges=T))
effort.trawldist$vars<-'TR_Distance'

effort.trawltime<-effort.unique%>%
  group_by(TARGET)%>%
  do(describe(.$TOW_TIME, ranges=T))
effort.trawltime$vars<-'TR_Duration'

effort.begdepth<-effort.unique%>%
  group_by(TARGET)%>%
  do(describe(.$BEG_DEPTH, ranges=T))
effort.begdepth$vars<-'BEG_DEPTH'

effort.enddepth<-effort.unique%>%
  group_by(TARGET)%>%
  do(describe(.$END_DEPTH, ranges=T))
effort.enddepth$vars<-'END_DEPTH'

effort.depthrange<-effort.unique%>%
  group_by(TARGET)%>%
  do(describe(.$Depth_Range, ranges=T))
effort.depthrange$vars<-'RANGE_DEPTH'

effort.station.sum<-aggregate(effort$KGHA, by=list(TARGET=effort$TARGET, LOCATION=effort$LOCATION), FUN=sum)%>%
  renameCol('x','KGHA')
effort.kgha<-effort.station.sum%>%
  group_by(TARGET)%>%
  do(describe(.$KGHA, ranges=T))
effort.kgha$vars<-'KGHA'

effort$SPECIES2<-as.numeric(as.character(effort$SPECIES))
effort.spp0<-subset(effort, SPECIES2==0)
effort.spp0<-select(effort.spp0, 'TARGET','LOCATION')
effort.spp0$SPP<-0
effort.spp1<-subset(effort, SPECIES2>0)
effort.spp.sum<-aggregate(effort.spp1$SPECIES, by=list(TARGET=effort.spp1$TARGET, LOCATION=effort.spp1$LOCATION), FUN=length)%>%
  renameCol('x','SPP')
effort.spp.sum<-rbind(effort.spp.sum, effort.spp0)
effort.spp<-effort.spp.sum%>%
  group_by(TARGET)%>%
  do(describe(.$SPP, ranges=T))
effort.spp$vars<-'N.SPP'


effort.all<-rbind(effort.trawldist, effort.trawltime, effort.begdepth, effort.enddepth, effort.depthrange, effort.spp, effort.kgha)
effort.all<-select(effort.all, c(1:4, 6, 9:10))
effort.all<-effort.all[order(effort.all$TARGET),]
effort.all<-effort.all%>%
  mutate_if(is.numeric, round, 2)
effort.all<-as.data.frame(effort.all)
write.xlsx(effort.all, here('Plots and Tables/RVCAT','Effort_CurrentYear.xlsx'), row.names=FALSE)

effort.nfish<-aggregate(effort$NUM, by=list(TARGET=effort$TARGET), FUN=sum)%>%
  renameCol('x','N_Fish')

effort.unique.spp<-effort[!duplicated(effort$SPECIES2),]
effort.unique.spp<-effort.unique.spp%>%
  filter(TARGET==2|TARGET==117|TARGET==118)%>%
  filter(SPECIES2>0)

##lollipop 5, 10, 20 year means comparisons##########################################################################################
ns<-all.data%>%
  filter(TARGET==2)%>%
  filter(YEAR>1977)

ns.spp.compare<-subset(ns, SPECIES==202|SPECIES==204|SPECIES==211|SPECIES==130|
                         SPECIES==131|SPECIES==902|SPECIES==903|SPECIES==904|SPECIES==109|
                         SPECIES==404|SPECIES==127|SPECIES==203)%>% ##input the species codes you want
  select(YEAR, LOCATION, SPECIES, KGHA)

##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year
ns.sites.byyear<-ns%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
ns.sites.byyear$SITECHECK<-'SURVEYED'

ns.spp.compare2<-merge.data.frame(ns.spp.compare, ns.sites.byyear, all=TRUE)
ns.spp.compare2<-cast(ns.spp.compare2, YEAR+LOCATION+SITECHECK~SPECIES, value='KGHA', fun.aggregate='mean')%>%
  select(c(1,2,4:15))
ns.spp.compare2[is.na(ns.spp.compare2)]<-0
ns.spp.compare<-melt.data.frame(ns.spp.compare2, id.vars=c('YEAR','LOCATION'))%>%
  renameCol('value','KGHA')%>%
  renameCol('variable','SPECIES')

##now find the mean of each species for each year
ns.spp.compare.mean<-aggregate(ns.spp.compare$KGHA, by=list(Year=ns.spp.compare$YEAR, SPECIES=ns.spp.compare$SPECIES), FUN=mean)%>%
  renameCol('x', 'meanKGHA')

##merge to get species names
ns.spp.compare.mean<-merge.data.frame(ns.spp.compare.mean, sci.names, all=F)

##calculate 5, 10, and 20 year means for each species
ns.spp5mean<-ns.spp.compare.mean%>%
  filter(Year>=max(Year)-4)
ns.spp5mean<-aggregate(ns.spp5mean$meanKGHA, by=list(Spp=ns.spp5mean$COMMON_NAME), FUN=mean)%>%
  renameCol('x','meanKGHA')
ns.spp5mean$period<-'5 year mean'
ns.spp10mean<-ns.spp.compare.mean%>%
  filter(Year>=max(Year)-9)
ns.spp10mean<-aggregate(ns.spp10mean$meanKGHA, by=list(Spp=ns.spp10mean$COMMON_NAME), FUN=mean)%>%
  renameCol('x','meanKGHA')
ns.spp10mean$period<-'10 year mean'
ns.spp20mean<-ns.spp.compare.mean%>%
  filter(Year>=max(Year)-19)
ns.spp20mean<-aggregate(ns.spp20mean$meanKGHA, by=list(Spp=ns.spp20mean$COMMON_NAME), FUN=mean)%>%
  renameCol('x','meanKGHA')
ns.spp20mean$period<-'20 year mean'

ns.periodmeans<-rbind(ns.spp5mean, ns.spp10mean, ns.spp20mean)

##calculate the mean for the last two years of sampling that will be used to compare to the 5, 10, and 20 year means
ns.spp2mean<-ns.spp.compare.mean%>%
  filter(Year>=max(Year)-1)
ns.spp2mean<-aggregate(ns.spp2mean$meanKGHA, by=list(Spp=ns.spp2mean$COMMON_NAME), FUN=mean)%>%
  renameCol('x','present2KGHA')

##percent change calculations
ns.periodmeans<-merge.data.frame(ns.periodmeans, ns.spp2mean, all=T)
ns.periodmeans$percentchange<-((ns.periodmeans$present2KGHA/ns.periodmeans$meanKGHA)*100)-100
ns.periodmeans$posneg<-if_else(ns.periodmeans$percentchange>0,'positive','negative')
ns.periodmeans$period_f<-factor(ns.periodmeans$period, levels=c('20 year mean','10 year mean','5 year mean'))

##plot
ggplot(ns.periodmeans, aes(x=percentchange, y=Spp, fill=posneg))+
  geom_segment(aes(x=0, xend=percentchange, y=Spp, yend=Spp))+
  geom_point(size=6, shape=21)+
  scale_fill_manual(values=c('red', 'cadetblue2'), guide=F)+
  scale_y_discrete(limits=c('Longnose Sucker','Rainbow Smelt','Ninespine Stickleback','Lake Whitefish','Burbot','Trout-perch', 
                            'Spoonhead Sculpin','Pygmy Whitefish','Slimy Sculpin','Deepwater Sculpin', 'Bloater','Cisco'))+
  plot_theme+
  facet_grid(.~period_f)+
  geom_vline(xintercept=0, size=2, color='black')+
  labs(x='Percent change in 2018-19 mean biomass (kg per ha) as compared time period', y='', 
       title='Lake Superior Nearshore Fish Biomass Trends',
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','ns_periods_comparison.png'), dpi = 300, width = 40, height = 20, units = "cm")

##Bar chart race, ns biomass--------------------------------------------------------------------------------------------------------------------------
library(reshape2)
ns.bcr<-read_excel(here('Plots and Tables/RVCAT','ns_biomass_summary.xlsx')) ##uses the biomass table made earlier. make sure this file is updated first
ns.bcr<-select(ns.bcr, c(1, 7:15))
ns.bcr2<-reshape2::melt(ns.bcr, id.vars='Year')
ns.bcr2<-ns.bcr2%>%
  renameCol('variable','Species')%>%
  renameCol('value','kg.ha')

anim_table <- ns.bcr2 %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(
    rank = min_rank(-kg.ha) * 1,
    Value_rel = kg.ha / kg.ha[rank == 1],
    Value_lbl = paste0(" ", kg.ha)
  ) %>%
  dplyr::filter(rank <= 10) %>%
  dplyr::ungroup()

ns.bcr3<-ns.bcr2
ns.bcr3<-ns.bcr3[order(ns.bcr3[,'Year'], ns.bcr3[,'kg.ha']),]
ns.bcr3$rank<-9:1




p<-ggplot(ns.bcr3, aes(rank, frame=Year))+
  geom_tile(aes(y=kg.ha/2, height=kg.ha, width=0.9, fill=Species))+
  geom_text(aes(y=0, label=paste(Species, "  ")), size=5, vjust=0.2, hjust=1)+
  coord_flip(clip='off', expand=F)+
  scale_y_continuous(labels=scales::comma)+
  scale_x_reverse()+
  scale_fill_viridis(discrete=T, guide=F)+
  labs(title='Lake Superior Annual Nearshore Fish Biomass', y='Mean biomass (kg/hectare)', x=' ',
       subtitle='{round(frame_time,0)}',
       caption=ann_data_access)+
  theme(plot.margin = margin(1,1,1,6, 'cm'),
        panel.background = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title=element_text(size=20),
        axis.text.x=element_text(size=18, color='black'),
        axis.title.x=element_text(size=18, color='black'),
        axis.line=element_line(size=1),
        plot.subtitle = element_text(size=18),
        plot.caption=element_text(size=15))+
  transition_time(Year)+
  ease_aes('cubic-in-out')
animate(p, fps = 4, nframes=200, width = 1024, height = 512, renderer=gifski_renderer(loop=T), end_pause=20) ##nframes =n years x2-1
anim_save(here("Plots and Tables/RVCAT",'Animated_ns_bar_race.gif'))


####################################################################################
##Sankey Diagram Nearshore Offshore Fish Collections#########################################
## USING network3D Package
library(networkD3)
library(htmltools)

##Based on number of individuals
current.yr.all<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)
current.yr.all$TARGET_f<-as.factor(current.yr.all$TARGET)

##load the species names file for when needed
codes.to.names<-read_xlsx('Species_Taxonomy.xlsx')
sci.names<-select(codes.to.names, c(2:4))

spp.counts<-aggregate(current.yr.all$NUM, by=list(SPECIES=current.yr.all$SPECIES, TARGET=current.yr.all$TARGET), FUN=sum)%>%
  renameCol('x','SUM')
spp.counts<-merge.data.frame(spp.counts, sci.names)
spp.counts<-select(spp.counts, c(4,5,2,3))
spp.counts2<-cast(spp.counts, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
spp.counts2<-spp.counts2%>%
  renameCol('COMMON_NAME','Fish')%>%
  renameCol('SCI_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
spp.counts2[is.na(spp.counts2)]<-0 

catch.yr <- spp.counts2 %>% 
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
  renameCol('SCI_NAME','Scientific name')%>%
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
  renameCol('SCI_NAME','Scientific name')%>%
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



######################################################################################
######################################################################################
######################################################################################
######################################################################################
##Make data table with NUM, WT, NOHA, KGHA for Targets = 2,117, 118
survey.yr.fish<-all.data %>%
  filter(TARGET==2 & YEAR>1977 |
           TARGET==118 |
           TARGET==117 & END_DEPTH>84) %>%
  filter(TR_DESIGN==25|TR_DESIGN==4) %>%
  select (4,5,8,29:31,34,35)

survey.yr.sites<-all.data %>%
  group_by(YEAR,TARGET)%>%
  distinct(LOCATION)
survey.yr.sites$SITECHECK<-'SURVEYED' 

survey.yr<-merge.data.frame(survey.yr.fish, survey.yr.sites, all=TRUE) %>%
  filter(TARGET==2 | TARGET==118 | TARGET==117)
  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
###STOPPED##########  
  
  survey.yr.all<-cast(survey.yr, YEAR+LOCATION+SITECHECK~SPECIES, value='KGHA', fun.aggregate='mean')


%>%
    select(c(1,2,4:6))
  spp.compare.ns2[is.na(spp.compare.ns2)]<-0
  spp.compare<-melt.data.frame(spp.compare.ns2, id.vars=c('YEAR','LOCATION'))%>%
    renameCol('value','KGHA')%>%
    renameCol('variable','SPECIES')
  
  
  
   complete(YEAR, TARGET, SPECIES) %>%
  filter(TARGET==2 & LOCATION <500 | 
           TARGET==117 & LOCATION>500 |
           TARGET==118 & LOCATION>500)
 survey.yr[is.na(survey.yr)]<-0

 
#############################
 nearshore.sites.byyear<-nearshore%>%
   group_by(YEAR)%>%
   distinct(LOCATION)
 nearshore.sites.byyear$SITECHECK<-'SURVEYED'
 
 spp.compare.ns2<-merge.data.frame(spp.compare.ns, nearshore.sites.byyear, all=TRUE)
 spp.compare.ns2<-cast(spp.compare.ns2, YEAR+LOCATION+SITECHECK~SPECIES, value='KGHA', fun.aggregate='mean')%>%
   select(c(1,2,4:6))
 spp.compare.ns2[is.na(spp.compare.ns2)]<-0
 spp.compare<-melt.data.frame(spp.compare.ns2, id.vars=c('YEAR','LOCATION'))%>%
   renameCol('value','KGHA')%>%
   renameCol('variable','SPECIES')
 
 ##now find the mean of each species for each year
 spp.compare.ns.mean<-aggregate(spp.compare$KGHA, by=list(Year=spp.compare$YEAR, SPECIES=spp.compare$SPECIES), FUN=mean)%>%
   renameCol('x', 'meanKGHA')
 
 ##merge to get species names
 spp.compare.ns.mean<-merge.data.frame(spp.compare.ns.mean, codes.to.names, all=F)
 
 
 
############################## 
survey.yr.sum<-aggregate(survey.yr$KGHA, by=list(survey.yr=survey.yr$YEAR, 
               survey.yr=survey.yr$TARGET, survey.yr=survey.yr$SPECIES), FUN=mean) %>%
  renameCol(1,'YEAR') %>%
  renameCol(2,'SURVEY') %>%
  renameCol(3,'SPECIES') %>%
  renameCol(4,'KGHA') 

survey.yr.sum<-subset(survey.yr.sum, SURVEY==2 |
         SURVEY==117 & YEAR==2011 | SURVEY==117 & YEAR==2016 |
         SURVEY==118 & YEAR>2011 & YEAR!=2011+5)

pig<-aggregate(survey.yr.sum$KGHA, by=list(survey.yr.sum=survey.yr.sum$YEAR,
         survey.yr.sum=survey.yr.sum$SURVEY), FUN=sum)



##Make data table with NUM, WT, NOHA, KGHA; CURRENT YEAR
current.yr<-all.data %>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118) %>%
  select (5,8,29:31,34,35) %>%
  complete(TARGET, LOCATION, SPECIES) %>%
  filter(TARGET==2 & LOCATION <500 | TARGET==117 & LOCATION>500 | TARGET==118 & LOCATION>500)
current.yr[is.na(current.yr)]<-0

cy1<-aggregate(current.yr$NUM, by=list(current.yr=current.yr$SPECIES,current.yr=current.yr$TARGET), FUN=sum)%>%
  renameCol(1,'SPECIES') %>%
  renameCol(2,'TARGET') %>%
  renameCol(3,'NUM') 
cy2<-aggregate(current.yr$WT, by=list(current.yr=current.yr$SPECIES,current.yr=current.yr$TARGET), FUN=sum)%>%
  renameCol(1,'SPECIES') %>%
  renameCol(2,'TARGET') %>%
  renameCol(3,'WT') 
cy3<-aggregate(current.yr$NOHA, by=list(current.yr=current.yr$SPECIES,current.yr=current.yr$TARGET), FUN=mean)%>%
  renameCol(1,'SPECIES') %>%
  renameCol(2,'TARGET') %>%
  renameCol(3,'NOHA') 
cy4<-aggregate(current.yr$KGHA, by=list(current.yr=current.yr$SPECIES,current.yr=current.yr$TARGET), FUN=mean)%>%
  renameCol(1,'SPECIES') %>%
  renameCol(2,'TARGET') %>%
  renameCol(3,'KGHA') 

current.yr.sum<- cbind(cy1[1:3],cy2[3],cy3[3],cy4[3]) 
current.yr.names<-merge.data.frame(current.yr.sum, sci.names) 


filter(NUM>0)
renameCol('COMMON_NAME','Common name')%>%
  renameCol('SCI_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
spp.counts2[is.na(spp.counts2)]<-0








#########################################################################
###Sankey using ggalluvial package


ggplot(catch.yr,
       aes(weight = Count, axis1 = Fish, axis2 = Survey)) +
  geom_alluvium(aes(fill = Survey, color = Survey), 
                width = 1/12, alpha = alpha, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("Category", "Response"))     +
  scale_fill_manual(values  = c(A_col, B_col, C_col)) +
  scale_color_manual(values = c(A_col, B_col, C_col)) +
  ggtitle("Relevance of Facebook Custom List Advertising") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  )

labs(title='Lake Superior Nearshore and Offshore Survey',
     subtitle='USGS bottom trawl assessment',
     caption=ann_data_access)


ggsave(p,'Plots and Tables/Plots_RVCAT/ns_os_sankey.png', dpi = 300, width = 40, height = 20, units = "cm")

#########################################

setwd("~/R/Scripts/RVCAT")

current.yr.all<-all.data%>%
  filter(YEAR==max(YEAR))%>%
  filter(TARGET==2|TARGET==117|TARGET==118)
current.yr.all$TARGET_f<-as.factor(current.yr.all$TARGET)

##load the species names file for when needed
codes.to.names<-read_xlsx('Species_Taxonomy.xlsx')
sci.names<-select(codes.to.names, c(2:4))


spp.counts<-aggregate(current.yr.all$NUM, by=list(SPECIES=current.yr.all$SPECIES, TARGET=current.yr.all$TARGET), FUN=sum)%>%
  renameCol('x','SUM')
spp.counts<-merge.data.frame(spp.counts, sci.names)
spp.counts<-select(spp.counts, c(4,5,2,3))
spp.counts2<-cast(spp.counts, COMMON_NAME+SCI_NAME~TARGET, value="SUM")
spp.counts2<-spp.counts2%>%
  renameCol('COMMON_NAME','Fish')%>%
  renameCol('SCI_NAME','Scientific name')%>%
  renameCol('2','Nearshore')%>%
  renameCol('118','Offshore')
spp.counts2[is.na(spp.counts2)]<-0 

catch.yr <- spp.counts2 %>% 
  select (1,3,4) 

catch.yr <- tidyr::gather(catch.yr, Survey, Count, -Fish)

# create nodes dataframe
Fish <- unique(as.character(catch.yr$Fish))
nodes <- data.frame(node = c(0:25), 
                    name = c(Fish, "Nearshore", "Offshore"))
#create links dataframe
catch.yr <- merge(catch.yr, nodes, by.x = "Fish", by.y = "name")
catch.yr <- merge(catch.yr, nodes, by.x = "Survey", by.y = "name")
links <- catch.yr[ , c("node.x", "node.y", "Count")]
colnames(links) <- c("source", "target", "value")

# draw sankey network
p<-networkD3::sankeyNetwork(Links = links, Nodes = nodes, 
                         Source = 'source', 
                         Target = 'target', 
                         Value = 'value', 
                         NodeID = 'name',
                         units = 'Count')
labs(title='Lake Superior Nearshore and Offshore Survey',
     subtitle='USGS bottom trawl assessment',
     caption=ann_data_access)


ggsave(p,'Plots and Tables/Plots_RVCAT/ns_os_sankey.png', dpi = 300, width = 40, height = 20, units = "cm")
