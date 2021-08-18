
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
library(ggforce)
library(here)
library(scales)
library(ggrepel)


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

covid1<-subset(all.data, TARGET == 2 | TARGET == 118 | TARGET == 117) 

covid2<-covid1 %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  group_by(LOCATION) %>%
  summarise(Mid.Lat.DD=mean(Mid.Lat.DD, na.rm=TRUE), Mid.Long.DD=mean(Mid.Long.DD, na.rm=TRUE), END_DEPTH=mean(END_DEPTH, na.rm=TRUE)) %>%
  subset(LOCATION != 12) %>%
  ungroup() 


##maps----------------------------------------------------------------------------------------------------------------------------
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


#################################################################################
#################################################################################

ggplot(covid2, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ ##path for lake outline
  geom_point(data=covid2, mapping=aes(Mid.Long.DD, Mid.Lat.DD), fill = "grey", shape=21, size=12,stroke=1.5)+ ##for the current site in the animation
 # geom_point(data=covid2, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color = "End_Depth"), shape=21, size=12,stroke=1.5)+ ##for the current site in the animation
  theme_bw() + 
  geom_text(aes(label=LOCATION), size=4)+
##  geom_text(aes(label=round(END_DEPTH, digits=0)), size=3)+
  scale_y_continuous(name='Latitude', breaks=seq(46.6,47, by=.2),limits=c(46.56,47.12), labels=c(46.6, 46.8, 47.0))+
  scale_x_continuous(name='Longitude', limits=c(-91.5,-90))+
  theme(axis.text=element_text(size=24, family='serif'), 
        axis.title=element_text(size=24, family='serif'), 
        plot.title=element_text(size=24, family='serif'),
        plot.subtitle=element_text(size=20, family='serif'),
        plot.caption=element_text(size=20, family='serif'), 
        legend.text=element_text(size=20, family='serif'),
        legend.title=element_text(size=20, family='serif'),
        legend.position=c(.28, .75)) +
  scale_color_gradient(low='cadetblue2', high='red', name='Depth, m')+
  labs(title='Research Vessel Kiyi COVID-19 Sampling Locations, July 2020',
       caption = 'U.S. Geological Survey, Lake Superior Biological Station')

ggsave(here('Plots and Tables/RVCAT','COVID_2020_SamplingLocations.png'), dpi=300, width=40, height=20, units='cm')



