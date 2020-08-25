
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


##load the raw RVCAT data file
raw.data<-read.csv(here('Data','RVCAT.csv'))

##change date into usable form
raw.data$date<-dmy(raw.data$OP_DATE)

###Calculate mid lat and long for each trawl

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

raw.data$YearClass<-raw.data$YEAR-1

##Select minimum number of fields of interest
data1<-select(raw.data,1,32,4,5,8,33,34,35)



###########################
##load Fish Lengths file into R
raw.data<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.data<-subset(raw.data, EXP_N>0)
reprows<-rep(1:nrow(raw.data), raw.data$EXP_N)
data2 <- raw.data[reprows,] %>%
  as.data.frame()
data2 <-select(data2, 1,4:6)


###JOIN TRAWL EFFORT TO LENGTH DATA
data3 <- inner_join(data2,unique(data1))

describe(data3)

#########################################################################################
#########################################################################################
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

###Subset data for a particular species, target, location, year, etc...
sumdata1 <- data3 %>%
  filter(SPECIES=="202",TARGET==2, YEAR >=1989)


###########Length Frequency Joy Plots
sumdata1$YEARf<-as.factor(sumdata1$YEAR)

ggplot(sumdata1, aes(x = LENGTH, y = YEARf)) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,500)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
  theme_joy(font_size = 16, grid = TRUE) +
  labs(title='Lake Superior Cisco Length Frequency',
     subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
     caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
     x = "Total length (mm)",
     y = "Year")

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Cisco_Joyplot.png'), dpi = 300, width = 40, height = 20, units = "cm")


#################################################################################################
#################################################################################################
#################################################################################################
##Annual mean lengths MULTIPLE SPECIES - Nearshore - offshore Ciscoes
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="204" & TARGET==2 & YEAR >=1989 |
           SPECIES=="206" & TARGET==118 & YEAR >=1989 |
           SPECIES=="206" & TARGET==117 & YEAR >=1989 |
           SPECIES=="202" & TARGET==2 & YEAR >=1989)

sumdata2$YEARf<-as.factor(sumdata2$YEAR)
sumdata2$SPECIES[sumdata2$SPECIES == "202"] <- "Cisco"
sumdata2$SPECIES[sumdata2$SPECIES == "204"] <- "Bloater"
sumdata2$SPECIES[sumdata2$SPECIES == "206"] <- "Kiyi"

sumdata3 <-sumdata2 %>%
  group_by(YEAR, SPECIES) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH), min_L=min(LENGTH), max_L=max(LENGTH))

sumdata4 <-sumdata3 %>%
  group_by(SPECIES) %>%
  summarize(median_L=median(median_L), mean_L=mean(mean_L), min_L=min(min_L),max_L=max(max_L), )

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(data=sumdata4, aes(yintercept=mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,350)) +
  plot_theme +
  labs(title='Lake Superior Ciscoes Mean Annual Lengths',
       subtitle='Near- and Offshore bottom trawl survey collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  facet_grid(SPECIES ~.)+
  scale_x_discrete(breaks=seq(1989,2019, by=2))


ggsave(here('Plots and Tables/Lengths','ns_Lengths_Ciscoe_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  

#################################################################################################
##Annual mean lengths MULTIPLE SPECIES - Nearshore SCULPINS
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="902" & TARGET==2 & YEAR >=1989 |
           SPECIES=="903" & TARGET==2 & YEAR >=1989 |
           SPECIES=="904" & TARGET==2 & YEAR >=1989)

sumdata2$YEARf<-as.factor(sumdata2$YEAR)
sumdata2$SPECIES[sumdata2$SPECIES == "902"] <- "Slimy Sculpin"
sumdata2$SPECIES[sumdata2$SPECIES == "903"] <- "Spoonhead Sculpin"
sumdata2$SPECIES[sumdata2$SPECIES == "904"] <- "Deepwater Sculpin"

sumdata3 <-sumdata2 %>%
  group_by(YEAR, SPECIES) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH), min_L=min(LENGTH), max_L=max(LENGTH))

sumdata4 <-sumdata3 %>%
  group_by(SPECIES) %>%
  summarize(median_L=median(median_L), mean_L=mean(mean_L), min_L=min(min_L),max_L=max(max_L), )

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(data=sumdata4, aes(yintercept=mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,150)) +
  plot_theme +
  labs(title='Lake Superior Sculpin Mean Annual Lengths',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  facet_grid(SPECIES ~.)+
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Sculpins_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  


#################################################################################################
##Annual mean lengths MULTIPLE SPECIES - Nearshore SCULPINS
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="130" & TARGET==2 & YEAR >=1989 |
           SPECIES=="131" & TARGET==2 & YEAR >=1989)

sumdata2$YEARf<-as.factor(sumdata2$YEAR)
sumdata2$SPECIES[sumdata2$SPECIES == "130"] <- "Ninespine Stickleback"
sumdata2$SPECIES[sumdata2$SPECIES == "131"] <- "Trout-Perch"

sumdata3 <-sumdata2 %>%
  group_by(YEAR, SPECIES) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH), min_L=min(LENGTH), max_L=max(LENGTH))

sumdata4 <-sumdata3 %>%
  group_by(SPECIES) %>%
  summarize(median_L=median(median_L), mean_L=mean(mean_L), min_L=min(min_L),max_L=max(max_L), )

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(data=sumdata4, aes(yintercept=mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,150)) +
  plot_theme +
  labs(title='Lake Superior Ninespine Stickleback and Trout-Perch Mean Annual Lengths',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  facet_grid(SPECIES ~.)+
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_TP-NS_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  

#################################################################################################
##Annual mean lengths SINGLE SPECIES Rainbow Smelt
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="109" & TARGET==2 & YEAR >=1989)

sumdata3 <-sumdata2 %>%
  group_by(YEAR, SPECIES) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH), min_L=min(LENGTH), max_L=max(LENGTH))

sumdata4 <-sumdata3 %>%
  group_by(SPECIES) %>%
  summarize(median_L=median(median_L), mean_L=mean(mean_L), min_L=min(min_L),max_L=max(max_L), )

sumdata2$YEARf<-as.factor(sumdata2$YEAR)

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(yintercept=mean(sumdata3$mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,250)) +
  plot_theme +
  labs(title='Lake Superior Rainbow Smelt Mean Annual Length',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_RBS_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  


##Annual mean lengths SINGLE SPECIES Burbot
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="127" & TARGET==2 & YEAR >=1989)

sumdata3 <-sumdata2 %>%
  group_by(YEAR, SPECIES) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH), min_L=min(LENGTH), max_L=max(LENGTH))

sumdata4 <-sumdata3 %>%
  group_by(SPECIES) %>%
  summarize(median_L=median(median_L), mean_L=mean(mean_L), min_L=min(min_L),max_L=max(max_L), )

sumdata2$YEARf<-as.factor(sumdata2$YEAR)

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(yintercept=mean(sumdata3$mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,750)) +
  plot_theme +
  labs(title='Lake Superior Burbot Mean Annual Length',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Burbot_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  


##Annual mean lengths SINGLE SPECIES Longnose Sucker
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="404" & TARGET==2 & YEAR >=1989)

sumdata3 <-sumdata2 %>%
  group_by(YEAR, SPECIES) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH), min_L=min(LENGTH), max_L=max(LENGTH))

sumdata4 <-sumdata3 %>%
  group_by(SPECIES) %>%
  summarize(median_L=median(median_L), mean_L=mean(mean_L), min_L=min(min_L),max_L=max(max_L), )

sumdata2$YEARf<-as.factor(sumdata2$YEAR)

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(yintercept=mean(sumdata3$mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,500)) +
  plot_theme +
  labs(title='Lake Superior Longnose Sucker Mean Annual Length',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_LNS_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  


##Annual mean lengths SINGLE SPECIES Siscowet Lake Trout
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="308" & TARGET==118 & YEAR >=2011 |
         SPECIES=="308" & TARGET==117 & YEAR >=1989)

sumdata3 <-sumdata2 %>%
  group_by(YEAR) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH))

sumdata2$YEARf<-as.factor(sumdata2$YEAR)

ggplot(sumdata2, aes(x=YEARf,LENGTH)) +
  stat_summary(fun.y = mean, geom = "point", size=2) + 
  geom_line() + 
  geom_hline(yintercept=mean(sumdata3$mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,750)) +
  plot_theme +
  labs(title='Lake Superior Siscowet Lake Trout Mean Annual Length',
       subtitle='Offshore summer bottom trawl collections, 2011-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(2011,2019, by=1))

ggsave(here('Plots and Tables/Lengths','os_Lengths_Siscowet_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  



###############################################################################################
###############################################################################################
###############################################################################################
##vertical histograms 
##CISCO--------------------------------------------------------------------------------------------------------------------
cisco <- data3 %>%
  filter(SPECIES=="202",TARGET==2, YEAR >=1989)
##filter(SPECIES=="202", YEAR >=1960)
ggplot(cisco, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Cisco Length Frequency',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=140, size=1)

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Cisco_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")  

##Bloater-----------------------------------------------------------------------------------------------------------
bloater <- data3 %>%
  filter(SPECIES=="204",TARGET==2, YEAR >=1989)
ggplot(bloater, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Bloater Length Frequency',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=130, size=1)

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Bloater_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##Lake Whitefish----------------------------------------------------------------------------------------------------
lwf <- data3 %>%
  filter(SPECIES=="203",TARGET==2, YEAR >=1989)%>%
  filter(LENGTH<501) ##included this bc there are some misentered data for lwf lengths, need to decide the best cutoff length
ggplot(lwf, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Lake Whitefish (<500 mm) Length Frequency',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=160, size=1)+
  scale_x_continuous(breaks=seq(0,600,by=100))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_LWF_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##rainbow smelt------------------------------------------------------------------------------------------------------
rbs <- data3 %>%
  filter(SPECIES=="109",TARGET==2, YEAR >=1989)
ggplot(rbs, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Rainbow Smelt Length Frequency',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=100, size=1)+
  scale_x_continuous(breaks=seq(0,300, by=50), limits=c(0,NA), expand=c(0,0))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_RBS_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##longnose sucker--------------------------------------------------------------------------
lns <- data3 %>%
  filter(SPECIES=="404",TARGET==2, YEAR >=1989)
ggplot(lns, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Longnose Sucker Length Frequency',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
 ## geom_vline(xintercept=100, size=1)+
  scale_x_continuous(breaks=seq(0,500, by=50), limits=c(0,NA), expand=c(0,0))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_LNS_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##kiyi----------------------------------------------------------------------------------------------------------------
kiyi <- data3 %>%
  filter(SPECIES=="206",TARGET==117|TARGET==118, YEAR >=2011)
ggplot(kiyi, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Kiyi Length Frequency',
       subtitle='Offshore summer bottom trawl collections, 2011-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=130, size=1)

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Kiyi_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##SISCOWET--------------------------------------------------------------------------------------------------------------------
siscowet <- data3 %>%
  filter(SPECIES=="308",TARGET==117|TARGET==118, YEAR >=2011) %>%
  subset(LENGTH<=800)
ggplot(siscowet, aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=12))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Siscowet Length Frequency',
       subtitle='Offshore summer bottom trawl collections, 2011-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL)+
  ##  geom_vline(xintercept=130, size=1)
  
  ggsave(here('Plots and Tables/Lengths','ns_Lengths_Siscowet_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")



###############################################################################################
#################################################################################################

###Miscellaneous plots below

###############################################################################################
###############################################################################################
cisco <- data3 %>%
  filter(SPECIES=="202",TARGET==2, YEAR >=1989)
ggplot(cisco, aes(x=date, y=LENGTH))+
  geom_point()  +
  plot_theme+
  #facet_grid(.~YEAR, switch='both', scales='free')+
  #coord_flip()+
#  geom_hline(yintercept=0, color='black', size=.5)+
 # theme(strip.placement = 'inside',
    #    strip.background = element_blank(),
    #    strip.text=element_text(size=12))+
  labs(x = 'Year', y='Total Length (mm)',
       title='Lake Superior Cisco Length Frequency',
       subtitle='Nearshore spring bottom trawl collections, 1989-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL) 
#  geom_vline(xintercept=140, size=1)


##Simple length frequency histogram all individuals
ggplot(sumdata1, aes(x = LENGTH)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0,350), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  theme(axis.line=element_line(size=1),
        panel.background=element_blank(),
        axis.text=element_text(size=16, family='serif'), 
        axis.title=element_text(size=16, family='serif'), 
        plot.title=element_text(size=20, family='serif'),
        plot.subtitle=element_text(size=20, family='serif'), 
        plot.caption=element_text(size=12, family='serif'), 
        legend.text=element_text(size=20, family='serif'),
        legend.title=element_text(size=20, family='serif')) +
    labs(title='Lake Superior Bloater Length Frequency',
         subtitle='Nearshore spring bottom trawl collections, 1998-2019', 
         caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
         x = "Total length (mm)",
         y = "Count")

ggsave(here('Plots and Tables/Lengths','ns_lengthfreq.png'),dpi = 300, width = 40, height = 20, units = "cm")

################################################################################
##Annual length plots with all fish - jittered
sumdata2 <- data3 %>%
  filter(SPECIES=="308" & TARGET==118 & YEAR >=2011 |
           SPECIES=="308" & TARGET==117 & YEAR >=2011)

sumdata3 <-sumdata2 %>%
  group_by(YEAR) %>%
  summarize(median_L=median(LENGTH), mean_L=mean(LENGTH))

sumdata2$YEARf<-as.factor(sumdata2$YEAR)

ggplot(sumdata2, aes(x=YEAR,y = LENGTH)) +
  geom_jitter(alpha=.1) +
##  geom_point(mean(LENGTH)) +
  geom_hline(yintercept=mean(sumdata3$mean_L), color='black', size=.5)+
  scale_y_continuous(limits = c(0,750)) +
  scale_x_discrete(breaks=seq(2011,2019, by=1)) +
  plot_theme +
  labs(title='Lake Superior Siscowet Lake Trout Lengths',
       subtitle='Offshore summer bottom trawl collections, 2011-2019', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
       x = "Year",
       y = "Total length (mm)")

ggsave(here('Plots and Tables/Lengths','ns_annual_lengths.png'), dpi = 300, width = 40, height = 20, units = "cm")



###################################################################################################
###################################################################################################
###Excel annual summary table of lengths by species and targets (2-nearshore, 117/118-offshore)

##library
library(tidyverse)
library(doBy)
library(readxl)
library(xlsx)
library(lubridate)
library(here)


##load the raw RVCAT data file
raw.data<-read.csv(here('Data','RVCAT.csv'))

##change date into usable form
raw.data$date<-dmy(raw.data$OP_DATE)

###Calculate mid lat and long for each trawl

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

raw.data$YearClass<-raw.data$YEAR-1

##Select minimum number of fields of interest
data1<-select(raw.data,1,32,4,5,8,33,34,35)

###########################
##load Fish Lengths file into R
raw.data<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.data<-subset(raw.data, EXP_N>0)
reprows<-rep(1:nrow(raw.data), raw.data$EXP_N)
data2 <- raw.data[reprows,] %>%
  as.data.frame()
data2 <-select(data2, 1,4:6)

###########################
###JOIN TRAWL EFFORT TO LENGTH DATA
data3 <- inner_join(data2,unique(data1))

###########################
##Filter targets and summarize by SPECIES, YEAR, TARGET
##Write Excel file
sumtable1 <- data3 %>%
  filter(TARGET==2 & YEAR >=1978 | TARGET==118 & YEAR >=2011 | TARGET==117 & YEAR >=2011 | TARGET==106 & YEAR >=1973) 

sumtable2 <- sumtable1 %>%
  group_by(SPECIES,YEAR,TARGET) %>%
  summarize(fish_count=n(), min_L_mm=mean(LENGTH), max_L_mm=max(LENGTH), median_L_mm=median(LENGTH), mean_L_mm=mean(LENGTH))

openxlsx::write.xlsx(sumtable2, here('Plots and Tables/Lengths','Length_AnnualSumry_byTarget_1978-present.xlsx'), row.names=FALSE)

