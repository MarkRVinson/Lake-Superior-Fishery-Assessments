
library(tidyverse)
library(doBy)
library(readxl)
library(ggplot2)
library(purrr)
library(viridis)
library(reshape)
library(readxl)
library(lubridate)
library(gganimate)
library(ggimage)
library(here)
library(grid)
library(scales)


plot_theme<-theme(axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=24, family='serif'),
                  legend.title=element_text(size=24, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=20, family='serif'),
                  plot.caption=element_text(size=20, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=24, family='serif'))

plot_theme2<-theme(axis.line=element_line(size=1, color='black'),
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



##ICE COVER_____________________________________________________________________________________________________________________________ICE COVER__________
##ice data, access the most recent data from: https://www.glerl.noaa.gov/data/ice/#historical -> Daily averages by lake -> Lake Superior 
##save file as '~/R/Scripts/RVCAT/sup_ice_recent'

raw.ice<-read_xlsx(here('Data','LS_GLERL_Ice.xlsx'), sheet = 'WideIce')

##change date format into usable form
raw.ice$date<-as.character(raw.ice$date)
raw.ice$date<-parse_date(ymd(raw.ice$date, format='%d-%b-%y')) 


  
ice <- raw.ice %>%
  pivot_longer(2:51, names_to = 'ice.year', values_to = 'ice.cover', names_transform = list(ice.year = as.numeric)) %>% 
  mutate(Day = day(date),
         Month = month(date), 
         Jday = yday(date)) %>%
  mutate(Year = case_when(
    Jday > 250 ~ ice.year - 1,
    Jday < 250 ~ ice.year)) %>%
  select(-date) %>%
  mutate(Date = make_date(Year, Month, Day)) %>%
  mutate(jday2 = case_when(
    Jday < 250 ~ Jday*1,
    Jday > 250 ~ (Jday*-1 + 366) * -1)) 

  
##facet of all years
ggplot(subset(ice, ice.year > 1976), aes(x=Jday, y=ice.cover, color=ice.cover))+
  geom_line(size=2)+
  scale_color_gradient(low='cyan',high='navy', name='Percent')+
  scale_x_continuous(limits=c(0,150), breaks=c(0,31,60,91,122), labels=c('Jan','Feb','Mar','Apr','May'))+
  coord_polar()+
  labs( title='Lake Superior Mean Daily Ice Concentration', 
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical',
       x='',y='')+
  plot_theme2 +
  theme_bw()  +
  theme(legend.position=c(0.7,0.05), 
        legend.direction="horizontal",
        legend.title = element_text()) +
  facet_wrap(~ice.year) 

ggsave(here('Plots and Tables/Ice_Temp','LS_Annual_Ice.png'), dpi = 300, width = 30, height = 35, units = "cm") 


##to calculate means by jday
icejdaymean<-aggregate(ice$ice.cover, by=list(Jday=ice$Jday), FUN=mean)%>%
  renameCol('x','meanjdayice')
icejdaymean<-icejdaymean[complete.cases(icejdaymean),]
ice2<-merge.data.frame(ice, icejdaymean)


##pre and post 1998 el nino means
iceprenino<-ice2 %>%
  subset(ice.year<1998)
iceprenino<-aggregate(iceprenino$ice.cover, by=list(Jday=iceprenino$Jday), FUN=mean)%>%
  renameCol('x','PreNino.jdaymean')

icepostnino<- ice2 %>%
  subset(ice.year>1997)
icepostnino<-aggregate(icepostnino$ice.cover, by=list(Jday=icepostnino$Jday), FUN=mean)%>%
  renameCol('x','PostNino.jdaymean')

ice3<-merge.data.frame(ice2, iceprenino)
ice3<-merge.data.frame(ice3, icepostnino) %>%
  distinct(Date, .keep_all = TRUE)


##animation
p<-ggplot(ice3, aes(x=Jday, y=ice.cover, color=ice.cover))+
  geom_line(size=2)+
  scale_color_gradient(low='cyan',high='navy', name='Percent\nIce Cover')+
  coord_polar()+
  transition_manual(frames=ice.year, cumulative=F)+
  scale_x_continuous(limits=c(0,140), breaks=c(0,31,60,91,122), labels=c('Jan','Feb','Mar','Apr','May'))+
  labs( title='Lake Superior Mean Daily Ice Concentrationr', 
        subtitle='Year: {current_frame}',
        caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical',
        x='Green line: Pre-1998 el Ni?o mean\nOrange line: Post-1998 el Ni?o mean',y='')+
  plot_theme +
  theme_bw()  +
  theme(legend.position=c(0.2,0.06), 
        legend.direction="horizontal")  +
  geom_line(aes(x=Jday, y=PreNino.jdaymean), color='seagreen', size=1) + ##turn off this line if you don't want the mean lines 
  geom_line(aes(x=Jday, y=PostNino.jdaymean), color='orange1', size=1)  ##turn off this line if you don't want the mean lines

animate(p, end_pause=10, duration=70)
anim_save(here('Plots and Tables/Ice_Temp','Animated_LS_Polar_Ice.gif'))

iceprenino$nino<-'pre'
icepostnino$nino<-'post'

iceprenino2<-iceprenino
iceprenino2<-renameCol(iceprenino2,'PreNino.jdaymean','mean.ice')
icepostnino2<-icepostnino
icepostnino2<-renameCol(icepostnino2, 'PostNino.jdaymean','mean.ice')

ice.nino<-rbind(iceprenino2, icepostnino2)
pad<-10

ggplot(ice.nino, aes(x=Jday, y=mean.ice, color=nino))+
  geom_point(size=2)+
  geom_segment(aes(x=Jday, xend=Jday, y=0, yend=mean.ice), size=1)+
  coord_polar()+
  scale_x_continuous(limits=c(0,140), breaks=c(0,31,60,91,122), labels=c('Jan','Feb','Mar','Apr','May'))+
  labs( title='Lake Superior Mean Daily Ice Cover', 
        caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical',
        x='',y='')+
  plot_theme +
  theme_bw()  +
  theme(legend.position=c(0.2,0.06), 
        legend.direction="horizontal") 
 # ylim(-.5*pad, max(ice.nino$ice)+4)



##area plot of daily ice cover across period of record
ggplot(subset(ice, Year >= 1977), aes(x=Date, y=ice.cover))+
  geom_area()+
  geom_smooth()+
  plot_theme2+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  labs(x='Date', y='Percent lakewide ice cover', title='Lake Superior Daily Ice Concentration', 
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')

ggsave(here('Plots and Tables/Ice_temp','LS_Daily_IceFit.png'), dpi = 300, width = 20, height = 10, units = "cm") 



##ice 'degree day' style plot
##n days >20% by year

icedays<-ice3 %>%
  subset(ice.cover >= 20) %>%
  group_by(ice.year) %>%
  summarise(Ice20count = n())
 
age1<-read_xlsx(here('Data','export_age1_annual_summary.xlsx'), sheet = 'Sheet 1') %>%
  renameCol('Year class', 'ice.year') %>%
    subset(survey == 'nearshore') %>%
    select(ice.year, survey, Bloater, Cisco) %>%
    left_join(icedays)

#age1$recruitment<-cut(age1$Cisco, breaks=c(-1,10,50,100,200,1000), labels=c('0-10','10-50','50-100','100-200','>200'))
age1$recruitment<-cut(age1$Cisco, breaks=c(-1,5,50,1000), labels=c('0-5','5-50','>50'))


ggplot(age1, aes(x=ice.year, y=Ice20count, fill=recruitment))+
  geom_bar(stat='identity')+
  plot_theme2+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x='Ice year', y='Number of days with >20% ice cover', title='Lake Superior Cumulative Annual Ice Days',
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')+
  scale_fill_viridis(discrete=TRUE, name='Age-1 cisco abundance\n(number/ha)')

ggsave(here('Plots and Tables/Ice_Temp','LS_20PercIceDays_RecruitmentByYear.png'), dpi = 300, width = 20, height = 10, units = "cm") 

ggplot(age1, aes(x=Ice20count, y=Cisco))+
  geom_point(size=7)+
  plot_theme2+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x='Number of days with >20% ice cover', 
       y= 'Age-1 Cisco Abundance (number/ha)',
       title='Lake Superior Cumulative Annual Ice Days',
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')

ggsave(here('Plots and Tables/Ice_Temp','LS_20PercIceDays_Recruitment.png'), dpi = 300, width = 20, height = 10, units = "cm") 


######################################################################################################################
###Annual Ice Cover for today (JDAY) for each year
######################################################################################################################

##go to https://www.glerl.noaa.gov/res/glcfs/glcfs-ice.php?lake=s&type=N&hr=00 and get today's ice concentration
##put this value in for IceCover = in the add_row function below
##in add_row function change Year and Yr fields as needed

##Today's date as JDay or you could change this to any day of the ice season you care about
x <-Sys.Date()
jdaytoday = day(x)

###manually subset data for any JDay
#jdaytoday = 6

###Subset the data to only that day - JDay and then add data for this year
icetoday<-subset(ice, Jday == jdaytoday)

##Add row for this year's data that might not be in GLERL data set
icetoday2<-add_row(icetoday, Jday = jdaytoday, Year = 2022, ice.year = 2022, ice.cover = 1.4)

image = here('Plots and Tables/Ice_Temp','snowflake.png')

ggplot(icetoday2, aes(x=Year, y=ice.cover)) +
  #geom_image(aes(image=image), size= 0.05) +
  geom_segment(aes(x=Year, xend=Year, y=0, yend=ice.cover), size=1, color='black')+
  geom_smooth(se = FALSE, size = 1.5, span = 0.5) +
  geom_smooth(method=lm, se = FALSE, colour='red') +
  plot_theme2 +
  labs(x='Ice year', y='% of total lake area', title='Lake Superior Historical Ice Cover for December 22',
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')+
  scale_x_continuous(expand=c(0,0), limits=c(1970,max(icetoday2$Year+1)), breaks=seq(1970,max(icetoday2$Year+1), by=5))+
  scale_y_continuous(expand=c(0,0),breaks = scales::pretty_breaks(4), limits=c(0,100)) +
  coord_cartesian(clip='off')+
  geom_hline(yintercept=0, color='black', size=1)

  ggsave(here('Plots and Tables/Ice_Temp','LS_Todays_Ice.png'), dpi = 300, width = 30, height = 15, units = "cm") 

  ######################################################################################################################
  ###Date of Maximum Annual Ice Cover
  ######################################################################################################################
  
  ##go to https://www.glerl.noaa.gov/res/glcfs/glcfs-ice.php?lake=s&type=N&hr=00 and get today's ice concentration
  ##put this value in for IceCover = in the add_row function below
  ##in add_row function change Year and Yr fields as needed
  
  icemax<-ice %>%
    group_by(Year) %>%
    slice(which.max(ice.cover))
  
  
  ##Add row for this year's data that might not be in GLERL data set
#  icetoday2<-add_row(icetoday, jday = jdaytoday, Year = 'X2020', IceCover = 1.4, Yr = 2020)
  
#  image = here('Plots and Tables/Ice_Temp','snowflake.png')
  
  ggplot(icemax, aes(x=Year, y=Jday)) +
#    geom_image(aes(image=image), size= 0.05) +
    geom_segment(aes(x=Year, xend=Year, y=0, yend=Jday), size=1, color='black')+
    geom_smooth(se = FALSE, size = 1.5, span = 0.5) +
    geom_smooth(method=lm, se = FALSE, colour='red') +
    plot_theme2 +
    labs(x='Ice year', y='Date of maximum ice concentration (Julian day)', title='Lake Superior Day of Maximum Ice Cover',
         caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')+
    scale_x_continuous(expand=c(0,0), limits=c(1970,max(icetoday2$Year+1)), breaks=seq(1970,max(icetoday2$Year+1), by=5))+
    scale_y_continuous(expand=c(0,0),breaks = scales::pretty_breaks(4), limits=c(0,100)) +
    coord_cartesian(clip='off')+
    geom_hline(yintercept=0, color='black', size=1)
  
  ggsave(here('Plots and Tables/Ice_Temp','LS_MaxIce_Day.png'), dpi = 300, width = 30, height = 15, units = "cm") 
  

##Max annual ice plot
    ggplot(icemax, aes(x=ice.year, y=ice.cover)) +
#    geom_image(aes(image=image), size= 0.05) +
    geom_segment(aes(x=ice.year, xend=ice.year, y=0, yend=ice.cover), size=1, color='black')+
    geom_smooth(se = FALSE, size = 1.5, span = 0.5) +
    geom_smooth(method=lm, se = FALSE, colour='red') +
    plot_theme2 +
    labs(x='Ice year', y='% of total lake area', title='Lake Superior Maximum Annual Ice Concentration',
         caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')+
    scale_x_continuous(expand=c(0,0), limits=c(1970,max(icemax$ice.year+1)), breaks=seq(1970,max(icemax$ice.year+1), by=5))+
    scale_y_continuous(expand=c(0,0),breaks = scales::pretty_breaks(4), limits=c(0,100)) +
    coord_cartesian(clip='off')+
    geom_hline(yintercept=0, color='black', size=1)
    
    ggsave(here('Plots and Tables/Ice_Temp','LS_Annual_MaxIce.png'), dpi = 300, width = 30, height = 15, units = "cm") 
    

    
##################################################################################################################################
###################################WATER TEMPERATURE
##################################################################################################################################
    
##Data from https://coastwatch.glerl.noaa.gov/statistic/statistic.html 
##need to download data for current year and append to Excel file

temp<-read_xlsx(here('Data','LS_GLERL_WTemp.xlsx'), sheet = 'GLERL')

##change date format into usable form
temp$date<-as.character(temp$date)
temp$date<-parse_date(ymd(temp$date))

temp <- temp %>%
  mutate(Day = day(date),
         Week = week(date), 
         Month = month(date))

 temp <- temp %>%
  pivot_longer(4:9, names_to = "lake", values_to = "temperature") 
 
 temp.LS <- temp %>%
  filter(lake == 'Superior') 
 
####CSMI Years plot
temp.csmi.dates <- temp.LS %>%
  subset(year == 2003 |
           year == 2009 |
           year == 2011 |
           year == 2014 |
           year == 2016 |
           year == 2022) %>%
  mutate(Month.short = month.abb[Month]) %>%
  unite(Date1, Month.short, Day, sep = ' ', remove = FALSE) %>%
  distinct(Date1, .keep_all = TRUE)

temp.csmi <- temp.LS %>%
  subset(year == 2003 |
           year == 2009 |
           year == 2011 |
           year == 2014 |
           year == 2016 |
           year == 2022) %>%
  mutate(Month.short = month.abb[Month]) %>%
  unite(Date1, Month.short, Day, sep = ' ', remove = FALSE) 


ggplot(temp.csmi) +
  aes(x=as.Date(jday, origin = as.Date("2011-01-01")), y = temperature, group = as.factor(year), color = as.factor(year)) + 
  geom_line(size=2)+
  scale_x_date(date_labels = "%b", breaks=pretty_breaks(12)) +
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_brewer(palette = 'Pastel1') +
  theme_bw()  +
  plot_theme +
  labs( title='Lake Superior Average Daily Surface Water Temperature', 
        caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html', 
        x='Date',y='Surface water temperature') +
  theme(legend.title = element_blank(), 
        legend.position=c(0.1,0.8)) 
  
ggsave(here('Plots and Tables/Ice_Temp','LS_CSMItemps.png'), height=20, width=40, units='cm')
ggsave(here('Plots and Tables/CSMI','LS_CSMItemps.png'), height=20, width=40, units='cm')



ggplot(temp.LS, aes(x=jday, y=temperature, color=temperature))+
  geom_line(size=2)+
  scale_color_gradient(low='cyan',high='red', name='Surface\nTemperature (C)')+
  coord_polar()+
  scale_x_continuous(limits=c(0,365), breaks=c(0,31,60,91,122,152,183,214,244,275,305,336), labels=c('Jan','Feb','Mar','Apr','May',
                                                                                                     'June','July','Aug','Sep',
                                                                                                     'Oct','Nov','Dec'))+
  labs( title='Lake Superior Average Daily Surface Water Temperature', 
        caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html',
        x='',y='') +
  plot_theme2 +
  theme_bw()  +
  theme(legend.position=c(0.18,-0.05), 
        legend.direction="horizontal") +
  facet_wrap(~year)

ggsave(here('Plots and Tables/Ice_Temp','LS_Annual_Temps.png'), height=30, width=25, units='cm')



###################################################################################################
##animation of daily temperatures by year 
temp.prenino <-temp.LS %>%
  filter(year<1998)
temp.prenino<-aggregate(temp.prenino$temperature, by=list(jday=temp.prenino$jday), FUN=mean)%>%
  renameCol('x','mean.prenino')

temp.postnino <-temp.LS %>%
  filter(year>1997)
temp.postnino<-aggregate(temp.postnino$temperature, by=list(jday=temp.postnino$jday), FUN=mean)%>%
  renameCol('x','mean.postnino')

temp.LS<-merge.data.frame(temp.LS, temp.prenino)
temp.LS<-merge.data.frame(temp.LS, temp.postnino)


p<-ggplot(temp.LS, aes(x=jday, y=temperature, color=temperature))+
  geom_line(size=2)+
  scale_color_gradient(low='cyan',high='red', name='Temperature (C)')+
  coord_polar()+
    transition_manual(frames=year, cumulative=F)+
  scale_x_continuous(limits=c(0,365), breaks=c(0,31,60,91,122,152,183,214,244,275,305,336), labels=c('Jan','Feb','Mar','Apr','May',
                                                                                                     'June','July','Aug','Sep',
                                                                                                     'Oct','Nov','Dec'))+
  labs( title='Lake Superior Average Daily Surface Water Temperature', 
        subtitle='Year: {current_frame}',
        caption='Data: NOAA, coastwatch.glerl.noaa.gov/statistic/statistic.html\nGreen line: pre-1998 el Ni?o mean\nOrange line: post-1998 el Ni?o mean',x='',y='')+
  plot_theme +
  theme_bw()  +
  theme(legend.position=c(0.15,-0.08), 
        legend.direction="horizontal", 
        legend.title = element_blank()) +
  geom_line(aes(x=jday, y=mean.prenino), color='seagreen', size=1)+
  geom_line(aes(x=jday, y=mean.postnino), color='orange1', size=1)

animate(p, end_pause=10, duration=70)

anim_save(here('Plots and Tables/Ice_Temp','Animated_LS_Polar_Temps.gif'))




################Experimantal Not yet pretty plot
temp.prenino$nino<-'pre'
temp.postnino$nino<-'post'

temp.prenino2<-temp.prenino
temp.prenino2<-renameCol(temp.prenino2,'mean.prenino','temperature')
temp.postnino2<-temp.postnino
temp.postnino2<-renameCol(temp.postnino2, 'mean.postnino','temperature')

temp.nino<-rbind(temp.prenino2, temp.postnino2)
pad<-10

ggplot(temp.nino, aes(x=jday, y=temperature, color=nino))+
  geom_point(size=2)+
  geom_segment(aes(x=jday, xend=jday, y=0, yend=temperature), size=1)+
  coord_polar()+
  scale_x_continuous(limits=c(0,365), breaks=c(0,31,60,91,122,152,183,214,244,275,305,336), labels=c('Jan','Feb','Mar','Apr','May',
                                                                                                     'June','July','Aug','Sep',
                                                                                                     'Oct','Nov','Dec'))+
  labs( title='Lake Superior Average Daily Surface Water Temperature', 
        caption='Data: NOAA, https://coastwatch.glerl.noaa.gov/statistic/statistic.html',
        x='',y='')+
  plot_theme +
  theme_bw()  +
  theme(legend.position=c(0.1,-0.1), 
        legend.direction="horizontal") +
  ylim(-.5*pad, max(temp.nino$temperature)+4)
