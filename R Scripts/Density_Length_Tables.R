##library
library(tidyverse)
library(doBy)
library(readxl)
library(psych)
library(rlang)
library(reshape)
library(xlsx)
library(lubridate)
library(grid)
library(here)

################################################################################################
##Fill in the information you want here, then run the following code

species<-308 ##enter species code of interest

##Commonly used species codes:
##Alewife = 106, Blackfin Cisco = 207, Bloater = 204, Burbot = 127, Cisco = 202, 
##Deepwater Sculpin = 904, hatchery Lake Trout = 307, Kiyi = 206, Lake Whitefish = 203, 
##lean Lake Trout = 317, Longnose Sucker = 404, Ninespine Stickleback = 130, 
##Pygmy Whitefish = 211, Rainbow Smelt =109, Round Whitefish = 212, Ruffe = 805, 
##Shortjaw Cisco = 210, siscowet Lake Trout = 308, Slimy Sculpin = 902, Spoonhead Sculpin = 903, 
##Spottail Shiner = 508, Threespine Stickleback, 129, Trout-perch = 131, Yellow Perch = 801

##survey<-c(2) ##enter target codes

survey<-c(117, 118) ##enter target codes

##Common target codes: 2 = nearshore, 117 = CSMI, 118 = offshore, 
##for present-day Offshore Survey use both 117, 118 and minimum year = 2011 and
##minimum depth to >84, 106 = Chequamegon Bay

sizebreaks<-c(0,200,300,400,500,600,2000) ##enter the size cutoffs you want to calculate for
sizelabels<-c('<200 mm','200-300 mm','300-400 mm','400-500 mm','500-600 mm','>600 mm') ##labels
##need to start with 0, smallest category will be less than the second number entered
##need to end with a number larger than any of the fish you may be analyzing
##there should be one less label category than there are breaks
##Potentially useful break points: age1 sizes - Cisco <140 mm, Bloater <130 mm, 
##Lake Whitefish <160mm, Rainbow Smelt <100 mm. Young Lake Trout , < 226 mm ~<age-3

##depthgreater<-0 ##if you have a "greater than" depth cutoff, if not, put 0

##For offshore cruise
depthgreater<-84 ##if you have a "greater than" depth cutoff, if not, put 0

depthless<-400 ##if you have a "less than" depth cutoff, if not, put 400

yearmax<-2100 ##max year you want included, if no max, use 2100

##yearmin<-1900 ##min year you want included, if no min, use 1900

##For offshore cruise
yearmin<-2011 ##min year you want included, if no min, use 1900


gear<-c(4,25) ##define gears (TR_DESIGN)
##4 = 39' bottom trawl, 25 = 39' roller trawl. Use both codes for Nearshore and Offshore Surveys. 
##26 = Chequamegon Bay small boat trawl 

##Now just run the code below, you should not need to make ANY changes, it will incorporate the 
##parameters you defined above
################################################################################################



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
all.data$AVG_DEPTH<-(all.data$BEG_DEPTH+all.data$END_DEPTH)/2

###
lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
op.id.details<-select(all.data, c(1,2,5,8,11,17,36,37))
lengths2<-merge.data.frame(lengths, op.id.details, by='OP_ID', all=F)

lengths.sub<-lengths2%>%
  filter(SPECIES==species,
         TARGET%in%survey,
         AVG_DEPTH>depthgreater&AVG_DEPTH<depthless,
         YEAR>=yearmin & YEAR<=yearmax,
         TR_DESIGN%in%gear)

lengths3<-unique(lengths.sub)
lengths3$SizeBin<-cut(lengths3$LENGTH, breaks=sizebreaks, labels=sizelabels)

n.fish.bin<-aggregate(lengths3$EXP_N,  by=list(YEAR=lengths3$YEAR, LOCATION=lengths3$LOCATION, 
                                                  HA_SWEPT=lengths3$HA_SWEPT,
                                               SizeBin=lengths3$SizeBin), FUN=sum)%>%
  renameCol('x','N.fish.bin')
n.fish.bin$NOHA.bin<-n.fish.bin$N.fish.bin/n.fish.bin$HA_SWEPT

site.details<-select(all.data, c(4,5,8,11,37))
site.details<-distinct(site.details)
site.details$SurveyCheck<-'SURVEYED'

sites.by.parameters<-site.details%>%
  filter(AVG_DEPTH>depthgreater&AVG_DEPTH<depthless)%>%
  filter(YEAR>=yearmin & YEAR<=yearmax)%>%
  filter(TR_DESIGN%in%gear)%>%
  filter(TARGET%in%survey)

n.fish.bin.allsites<-merge.data.frame(n.fish.bin, sites.by.parameters, all=T)
n.fish.bin.allsites<-select(n.fish.bin.allsites, c(1,2,4,6))
n.fish.bin.allsites2<-cast(n.fish.bin.allsites, YEAR+LOCATION~SizeBin, value='NOHA.bin', fun.aggregate = 'mean')
n.fish.bin.allsites2<-select(n.fish.bin.allsites2, -c(2))
n.fish.bin.allsites2[is.na(n.fish.bin.allsites2)]<-0
n.fish.bin.means<-aggregate(.~YEAR, n.fish.bin.allsites2, FUN=mean)

write.xlsx(n.fish.bin.means,
           here('Plots and Tables/Length Bin Densities',paste(species,'length_bin.xlsx', sep='_')),
           row.names = F)
