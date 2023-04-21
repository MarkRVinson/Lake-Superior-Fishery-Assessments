
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
sci.names<-select(codes.to.names, c(1:3,8,10))
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
  mutate(Country = when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'E'  ~ "Canada",
    STATE == 'W'  ~ "Canada"))


##Select minimum number of fields of interest
data1<-select(raw.data,1, 34, 32, 33, 3, 37, 4, 7, 10, 35, 36)

##data1<-subset(data1, TARGET==2 & YEAR >1973 &  M_UNIT == "WI2")

##data1<-subset(data1, TARGET==2 & YEAR >1977 &  Country == "USA")


###########################
##load Fish Lengths file into R
raw.lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.lengths<-subset(raw.lengths, EXP_N>0)
reprows<-rep(1:nrow(raw.lengths), raw.lengths$EXP_N)
data2 <- raw.lengths[reprows,] %>%
  as.data.frame()
data2 <-select(data2, 1,4:6)

##Join SPECIES names
data2$SPECIES<-as.factor(data2$SPECIES) 

data2 <- data2 %>%
  left_join(sci.names)

###JOIN TRAWL EFFORT TO LENGTH DATA
data3 <- inner_join(data2,unique(data1)) 
  

describe(data3)

#########################################################################################
#########################################################################################
plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=24, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=26, family='serif'),
                  plot.subtitle=element_text(size=24, family='serif'),
                  plot.caption=element_text(size=18, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=24, family='serif'))


ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/P9XVOLR1'

#########################################################################################
#########################################################################################
###########Length Frequency Joy Plots

ggplot(subset(data3, SPECIES == "202" & YEAR > 1989), aes(x = LENGTH, y = as.factor(YEAR))) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,600)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
  theme_joy(font_size = 16, grid = TRUE) +
  labs(title='Lake Superior Cisco Length Frequency',
       subtitle='Collections occurrred April-November', 
       caption=ann_data_access, 
       x = "Total length (mm)",
       y = "Year")

ggsave(here('Plots and Tables/Lengths','CiscoLength_Joyplot.png'), dpi = 300, width = 20, height = 20, units = "cm")

ggplot(subset(data3, SPECIES == "204" & YEAR > 1989), aes(x = LENGTH, y = as.factor(YEAR))) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,350)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
  theme_joy(font_size = 16, grid = TRUE) +
  labs(title='Lake Superior Bloater Length Frequency',
       subtitle='Collections occurrred April-November', 
       caption=ann_data_access, 
       x = "Total length (mm)",
       y = "Year")

ggsave(here('Plots and Tables/Lengths','BloaterLength_Joyplot.png'), dpi = 300, width = 20, height = 20, units = "cm")

ggplot(subset(data3, SPECIES == "206" & YEAR > 1989), aes(x = LENGTH, y = as.factor(YEAR))) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,300)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
  theme_joy(font_size = 16, grid = TRUE) +
  labs(title='Lake Superior Kiyi Length Frequency',
       subtitle='Collections occurrred April-November', 
       caption=ann_data_access, 
       x = "Total length (mm)",
       y = "Year")

ggsave(here('Plots and Tables/Lengths','KiyiLength_Joyplot.png'), dpi = 300, width = 20, height = 20, units = "cm")

ggplot(subset(data3, SPECIES == "202" & YEAR > 1984 & YEAR < 2021 |
                SPECIES == "204" & YEAR > 1984  & YEAR < 2021 |
                SPECIES == "206" & YEAR > 1984 & YEAR < 2021 ), 
       aes(x = LENGTH, y = as.factor(YEAR-1))) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,350)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
#  theme_joy(font_size = 16, grid = TRUE) +
  theme(axis.text=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        plot.title=element_text(size=24, family='serif'),
        plot.subtitle=element_text(size=22, family='serif'),
        plot.caption=element_text(size=22, family='serif'),
        strip.text=element_text(size=22, family='serif')) +
  labs(title='Lake Superior Ciscoe (Bloater, Cisco, and Kiyi) Length Frequency',
       subtitle='Collections occurrred April-November', 
       caption=ann_data_access, 
       x = "Total length (mm)",
       y = "Year-class")

ggsave(here('Plots and Tables/Lengths','CiscoeLength_Joyplot.png'), dpi = 300, width = 30, height = 40, units = "cm")


### RBS Joy plot
ggplot(subset(data3, SPECIES == "109" & YEAR > 1983), 
       aes(x = LENGTH, y = as.factor(YEAR))) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
  #  theme_joy(font_size = 16, grid = TRUE) +
  theme(axis.text=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        plot.title=element_text(size=24, family='serif'),
        plot.subtitle=element_text(size=22, family='serif'),
        plot.caption=element_text(size=22, family='serif'),
        strip.text=element_text(size=22, family='serif')) +
  labs(title='Lake Superior Rainbow Smelt Length Frequency',
       subtitle='Collections occurrred April-November', 
       caption=ann_data_access, 
       x = "Total length (mm)",
       y = "Year")

ggsave(here('Plots and Tables/Lengths','RBSLength_Joyplot.png'), dpi = 300, width = 30, height = 40, units = "cm")


##Cisco joyplot for 2022 report
ggplot(subset(data3, SPECIES == "202" & YEAR > 1983 & YEAR < 2021), 
       aes(x = LENGTH, y = as.factor(YEAR))) +
  geom_joy_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(limits = c(0,500)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  plot_theme +
  #  theme_joy(font_size = 16, grid = TRUE) +
  theme(axis.text=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        plot.title=element_text(size=24, family='serif'),
        plot.subtitle=element_text(size=22, family='serif'),
        plot.caption=element_text(size=22, family='serif'),
        strip.text=element_text(size=22, family='serif')) +
  labs(title='Lake Superior Cisco Length Frequency',
       subtitle='Collections occurrred April-November', 
       caption=ann_data_access, 
       x = "Total length (mm)",
       y = "Year")

ggsave(here('Plots and Tables/Lengths','CiscoLength_Joyplot.Report.png'), dpi = 300, width = 30, height = 40, units = "cm")



#################################################################################################
#################################################################################################
##############################################################################################
##vertical histograms 
##CISCO--------------------------------------------------------------------------------------------------------------------

ggplot(subset(data3, SPECIES=="202" & YEAR-1 >=1984), aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=8))+
  labs(y='Year-class', x='Total Length (mm)',
       title='Lake Superior Cisco Length Frequency',
  #     subtitle='Data from all collections made throughout the year', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL)+
#  xlim(0,520) +
  geom_vline(xintercept=140, size=1, linetype="dotted") +
  geom_vline(xintercept=0, size=1, linetype="solid") 
  
ggsave(here('Plots and Tables/Lengths','Lengths_Cisco_Vhistogram.png'), dpi = 300, width = 40, height = 25, units = "cm")  

##Bloater-----------------------------------------------------------------------------------------------------------

ggplot(subset(data3, SPECIES=="204" & YEAR >=1984), aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=8))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Bloater Length Frequency',
   #    subtitle='Data from all collections made throughout the year', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL)+
  xlim(0,500) +
  geom_vline(xintercept=130, size=1, linetype="dotted") +
  geom_vline(xintercept=0, size=1, linetype="solid") 


ggsave(here('Plots and Tables/Lengths','Lengths_Bloater_Vhistogram.png'), dpi = 300, width = 40, height = 25, units = "cm") 

##Kiyi-----------------------------------------------------------------------------------------------------------

ggplot(subset(data3, SPECIES=="206" & YEAR >=1984), aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=8))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Kiyi Length Frequency',
  #     subtitle='Data from all collections made throughout the year', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL)+
  xlim(0,350) +
  geom_vline(xintercept=140, size=1, linetype="dotted") +
  geom_vline(xintercept=0, size=1, linetype="solid") 

ggsave(here('Plots and Tables/Lengths','Lengths_Kiyi_Vhistogram.png'), dpi = 300, width = 40, height = 25, units = "cm") 


##Export to Excel File to use in Grand Island Kiyi analysis
##library(openxlsx)
list.sheets<-list('kiyi.lengths'= kiyi)

openxlsx::write.xlsx(list.sheets, here('Plots and Tables/Lengths','kiyi.lengths.xlsx'))


##Lake Whitefish----------------------------------------------------------------------------------------------------
ggplot(subset(data3, SPECIES=="203" & YEAR >=1984), aes(x=LENGTH))+
  geom_histogram(binwidth = 10)+
  plot_theme+
  facet_grid(.~YEAR, switch='both', scales='free')+
  coord_flip()+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(strip.placement = 'inside',
        strip.background = element_blank(),
        strip.text=element_text(size=8))+
  labs(y='Year', x='Total Length (mm)',
       title='Lake Superior Lake Whitefish (<500 mm) Length Frequency',
   #    subtitle= 'All collections made throughout the year', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=160, size=1, linetype="dotted") +
  geom_vline(xintercept=0, size=1, linetype="solid") 

ggsave(here('Plots and Tables/Lengths','ns_Lengths_LWF_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##rainbow smelt------------------------------------------------------------------------------------------------------

ggplot(subset(data3, SPECIES=="109" & YEAR >=1984), aes(x=LENGTH))+
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
       subtitle='Collections made April through November', 
       caption=ann_data_access)+ 
  scale_x_continuous(limits=c(0,200), expand=c(0,0)) +
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=100, size=1, linetype="dotted") +
  geom_vline(xintercept=0, size=1, linetype="solid") 

ggsave(here('Plots and Tables/Lengths','ns_Lengths_RBS_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##longnose sucker--------------------------------------------------------------------------

ggplot(subset(data3, SPECIES=="404" & YEAR >=1984), aes(x=LENGTH))+
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
      # subtitle='All collections made throughout the year', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=0, size=1, linetype="solid") 
scale_x_continuous(breaks=seq(0,500, by=50), limits=c(0,NA), expand=c(0,0))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_LNS_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##kiyi----------------------------------------------------------------------------------------------------------------

ggplot(subset(data3, SPECIES=="206" & TARGET==117|TARGET==118 & YEAR >=2011), aes(x=LENGTH))+
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
       subtitle='All collections made throughout the year', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL)+
  geom_vline(xintercept=130, size=1, linetype="dotted") +
  geom_vline(xintercept=0, size=1, linetype="solid") 

ggsave(here('Plots and Tables/Lengths','ns_Lengths_Kiyi_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")

##SISCOWET--------------------------------------------------------------------------------------------------------------------
ggplot(subset(data3, SPECIES=="308" & TARGET==117|TARGET==118 & YEAR >=2011), aes(x=LENGTH))+
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
       subtitle='Offshore summer bottom trawl collections', 
       caption=ann_data_access)+
  scale_y_continuous(breaks=NULL) +
  geom_vline(xintercept=0, size=1, linetype="solid") 

  ggsave(here('Plots and Tables/Lengths','ns_Lengths_Siscowet_Vhistogram.png'), dpi = 300, width = 40, height = 20, units = "cm")



###############################################################################################
#################################################################################################
### CSMI Length frequencies for ciscoes

###Subset data for a particular species, target, location, year, etc...
csmi1 <- data3 %>%
  subset(SPECIES=="202" & TARGET==117 |
           SPECIES=="204" & TARGET==117 |
           SPECIES=="206" & TARGET==117 |
           SPECIES=="217" & TARGET==117) %>%
  mutate(gear = case_when(
    TR_DESIGN == '4'  ~ "Bottom Trawl", 
    TR_DESIGN == '25'  ~ "Bottom Trawl",   
    TR_DESIGN == '28'  ~ "Midwater Trawl")) %>%
  mutate(COMMON_NAME2 = case_when(
      YEAR != 2022 ~ COMMON_NAME,
      YEAR == 2022 & Month >7 & LENGTH <130 ~ 'Unidentified Coregonid', 
      YEAR == 2022 & Month >7 & LENGTH >130 ~ COMMON_NAME, 
      YEAR == 2022 & Month <=7 ~ COMMON_NAME)) 


ggplot(csmi1, aes(x=LENGTH, fill = COMMON_NAME2))+
  geom_histogram(binwidth = 5) +
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(legend.title = element_blank(), 
        legend.position = c(0.5, 0.88)) +
  labs(y='Number collected', x='Total Length (mm)',
       title='Lake Superior Ciscoe Length Frequency',
       subtitle='CSMI sites sampled in 2011, 2016, and 2022', 
       caption=ann_data_access)+
  #  scale_x_continuous() +
  scale_y_continuous() +
  scale_fill_brewer(palette = 'Pastel1') +
  #xlim(0, 520) +
  facet_grid(YEAR ~ gear)

ggsave(here('Plots and Tables/CSMI','csmi_Lengths_Ciscoes.png'), dpi = 300, width = 40, height = 40, units = "cm")  
ggsave(here('Plots and Tables/Lengths','csmi_Lengths_Ciscoes.png'), dpi = 300, width = 40, height = 40, units = "cm")  


## stacked bar histogram by week of the year
csmi.22 <- csmi1 %>%
  subset(Month >= 8) %>%
  subset(YEAR == 2022) %>%
  subset(LENGTH <= 150) %>%
  group_by(Week, COMMON_NAME) %>%
  tally()

ggplot(csmi.22, aes(x=as.factor(Week), weights = n, fill = COMMON_NAME))+
  geom_bar() + 
  plot_theme+
  geom_hline(yintercept=0, color='black', size=.5)+
  theme(legend.title = element_blank(), 
        legend.position = c(0.25, 0.88)) +
  labs(y='Number collected', x='Week',
       title='Lake Superior Age-0 Ciscoe Collections in Summer 2022',
       subtitle=' ', 
       caption=ann_data_access)

ggsave(here('Plots and Tables/Lengths','2022CSMI_Age0byWeek.png'), dpi = 300, width = 40, height = 20, units = "cm")  



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
       subtitle='Near- and Offshore bottom trawl survey collections, May-July', 
       caption=ann_data_access, 
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
       subtitle='Near- and Offshore bottom trawl survey collections, May-July', 
       caption=ann_data_access, 
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
       subtitle='Nearshore spring bottom trawl collections', 
       caption=ann_data_access, 
       x = "Year",
       y = "Mean length (mm)") +
  facet_grid(SPECIES ~.)+
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_TP-NS_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  

#################################################################################################
##Annual mean lengths SINGLE SPECIES Rainbow Smelt
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="109" & TARGET==2 & YEAR >=1984)

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
       subtitle='Nearshore spring bottom trawl collections', 
       caption=ann_data_access, 
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(1984,2022, by=2))

ggsave(here('Plots and Tables/Lengths','ns_Lengths_RBS_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  

##Annual mean lengths SINGLE SPECIES Kiyi
###Subset data for a particular species, target, location, year, etc...
sumdata2 <- data3 %>%
  filter(SPECIES=="206" & YEAR >=1989)

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
  scale_y_continuous() +
  plot_theme +
  labs(title='Lake Superior Kiyi Mean Annual Length',
       subtitle='Collections made 1989-2019', 
       caption=ann_data_access, 
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(1989,2019, by=2))

ggsave(here('Plots and Tables/Lengths','Lengths_Kiyi_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  


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
       subtitle='Nearshore spring bottom trawl collections', 
       caption=ann_data_access, 
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
       subtitle='Nearshore spring bottom trawl collections', 
       caption=ann_data_access, 
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
       subtitle='Offshore summer bottom trawl collections', 
       caption=ann_data_access, 
       x = "Year",
       y = "Mean length (mm)") +
  scale_x_discrete(breaks=seq(2011,2019, by=1))

ggsave(here('Plots and Tables/Lengths','os_Lengths_Siscowet_Means.png'), dpi = 300, width = 40, height = 20, units = "cm")  



###############################################################################################

###Miscellaneous plots below
###############################################################################################
cisco <- data3 %>%
  filter(SPECIES=="202",TARGET==2, YEAR >=1989)
ggplot(cisco, aes(x=OP_DATE, y=LENGTH))+
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
       subtitle='Nearshore spring bottom trawl collections', 
       caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0')+
  scale_y_continuous(breaks=NULL) 
#  geom_vline(xintercept=140, size=1)


##Simple length frequency histogram all individuals
ggplot(kiyi, aes(x = LENGTH)) +
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
         subtitle='Nearshore spring bottom trawl collections', 
         caption = 'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0',
         x = "Total length (mm)",
         y = "Count")

ggsave(here('Plots and Tables/Lengths','ns_lengthfreq.png'),dpi = 300, width = 40, height = 20, units = "cm")

##Simple length frequency histogram all individuals
ggplot(kiyi, aes(x = LENGTH)) +
  geom_freqpoly() +
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
       subtitle='Nearshore spring bottom trawl collections', 
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
       subtitle='Offshore summer bottom trawl collections', 
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

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data <- raw.data %>%
  mutate(Day = day(OP_DATE),
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

##Select minimum number of fields of interest
data1<-select(raw.data,1,33, 3, 4, 7, 10, 34,35,36)

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
  group_by(SPECIES,YEAR,TARGET, TR_DESIGN) %>%
  summarize(fish_count=n(), min_L_mm=mean(LENGTH), max_L_mm=max(LENGTH), median_L_mm=median(LENGTH), mean_L_mm=mean(LENGTH))

openxlsx::write.xlsx(sumtable2, here('Plots and Tables/Lengths','Length_AnnualSumry_byTarget_1978-present.xlsx'), row.names=FALSE)

###########################
##Make a file of lengths for a high school student project, age-1
##Write Excel file
##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3,8,10))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

data3$SPECIES<-as.factor(data3$SPECIES)

length1.misc <- data3 %>%
  filter(TARGET==2 & YEAR >=1995)  %>%
  subset(SPECIES == 109 & LENGTH <= 125 |
           SPECIES == 202 & LENGTH <= 175 |
           SPECIES == 203 & LENGTH <= 250|
           SPECIES == 204 & LENGTH <= 175) %>%
  left_join(sci.names) %>%
  group_by(COMMON_NAME,YEAR) %>%
  summarize(fish_count=n(), min_L_mm=mean(LENGTH), max_L_mm=max(LENGTH), median_L_mm=median(LENGTH), mean_L_mm=mean(LENGTH))

length2.misc <- data3 %>%
  filter(TARGET==2 & YEAR >=1995)  %>%
  subset(SPECIES == 109 |
           SPECIES == 202 |
           SPECIES == 203 |
           SPECIES == 204) %>%
  left_join(sci.names) %>%
  group_by(COMMON_NAME,YEAR) %>%
  summarize(fish_count=n(), min_L_mm=mean(LENGTH), max_L_mm=max(LENGTH), median_L_mm=median(LENGTH), mean_L_mm=mean(LENGTH))

length3.misc <- data3 %>%
  filter(TARGET==2 & YEAR >=1995)  %>%
  subset(SPECIES == 109 |
           SPECIES == 202 |
           SPECIES == 203 |
           SPECIES == 204) %>%
  left_join(sci.names) %>%
  select(OP_ID, OP_DATE, YEAR, LOCATION, Mid.Lat.DD, Mid.Long.DD, COMMON_NAME, LENGTH, EXP_N)


list.sheets<-list('Age1Length' = length1.misc, 
                  'LengthSum'= length2.misc, 
                  'Lengthd'= length3.misc)

openxlsx::write.xlsx(list.sheets, here('Plots and Tables/Lengths','MiscFishLengths.xlsx'))

