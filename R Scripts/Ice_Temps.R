
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


plot_theme<-theme(axis.text=element_text(size=10, family='serif'),
                  axis.title=element_text(size=12, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=16, family='serif'),
                  legend.title=element_text(size=8, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=18, family='serif'),
                  plot.subtitle=element_text(size=14, family='serif'),
                  plot.caption=element_text(size=10, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=14, family='serif'))

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
ice<-read_xlsx(here('Data','LS_GLERL_Ice.xlsx'), sheet = 'WideIce') %>%
  replace(is.na(.), 0) %>%
  pivot_longer(2:50, names_to = 'ice.year', values_to = 'ice.cover', names_transform = list(ice.year = as.numeric)) %>% 
  mutate(Date = as.Date.character(Date)) %>% 
  mutate(Jday = yday(Date)) %>%
  mutate(Year = case_when(
    Jday > 250 ~ ice.year - 1,
    Jday < 250 ~ ice.year)) %>%
  mutate(month = month(Date), day = day(Date)) %>%
  select(2:7) %>%
  mutate(Date = make_date(Year, month, day)) %>% 
  drop_na(Date) %>%
  mutate(jday2 = case_when(
    Jday < 250 ~ Jday*1,
    Jday > 250 ~ (Jday*-1 + 366) * -1)) %>%


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
icejdaymean<-ice %>%
  group_by(ice.year) %>%
  
  renameCol('x','meanjdayice')
icejdaymean<-icejdaymean[complete.cases(icejdaymean),]
icenoaa4<-merge.data.frame(icenoaa4, icejdaymean)


icejdaymean<-aggregate(icenoaa4$IceCover, by=list(jday=icenoaa4$jday), FUN=mean)%>%
  renameCol('x','meanjdayice')
icejdaymean<-icejdaymean[complete.cases(icejdaymean),]
icenoaa4<-merge.data.frame(icenoaa4, icejdaymean)

##pre and post 1998 el nino means
iceprenino<-subset(icenoaa4, ice.yr<1998)
iceprenino<-aggregate(iceprenino$IceCover, by=list(jday=iceprenino$jday), FUN=mean)%>%
  renameCol('x','PreNino.jdaymean')

icepostnino<-subset(icenoaa4, ice.yr>1997)
icepostnino<-aggregate(icepostnino$IceCover, by=list(jday=icepostnino$jday), FUN=mean)%>%
  renameCol('x','PostNino.jdaymean')

icenoaa4<-merge.data.frame(icenoaa4, iceprenino)
icenoaa4<-merge.data.frame(icenoaa4, icepostnino)

##animation
p<-ggplot(ic, aes(x=Jday, y=ice.coverr, color=ice.cover))+
  geom_line(size=2)+
  scale_color_gradient(low='cyan',high='navy', name='Percent\nIce Cover')+
  coord_polar()+
  transition_manual(frames=ice.yr, cumulative=F)+
  scale_x_continuous(limits=c(0,140), breaks=c(0,31,60,91,122), labels=c('Jan','Feb','Mar','Apr','May'))+
  labs( title='Lake Superior Mean Daily Ice Concentrationr', 
        subtitle='Year: {current_frame}',
        caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical',
        x='Green line: Pre-1998 el Ni?o mean\nOrange line: Post-1998 el Ni?o mean',y='')+
  plot_theme +
  theme_bw()  +
  theme(legend.position=c(0.2,0.06), 
        legend.direction="horizontal")  ##+
#  geom_line(aes(x=Jday, y=PreNino.jdaymean), color='seagreen', size=1)+ ##turn off this line if you don't want the mean lines 
#  geom_line(aes(x=Jday, y=PostNino.jdaymean), color='orange1', size=1)  ##turn off this line if you don't want the mean lines

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

ggplot(ice.nino, aes(x=jday, y=mean.ice, color=nino))+
  geom_point(size=2)+
  geom_segment(aes(x=jday, xend=jday, y=0, yend=mean.ice), size=1)+
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
icedays<-aggregate(icenoaa4$IceCover, by=list(ice.yr=icenoaa4$ice.yr), FUN=sum)%>%
  renameCol('x','IceAnnSum')

age1<-read.xlsx(here('Plots and Tables/RVCAT','ns_Age1_summary.xlsx'), sheetIndex = 1)
age1$ice.yr<-age1$Year.Class
age1<-select(age1, c(4,10))

icedays<-merge.data.frame(icedays, age1)
icedays$recruitment<-cut(icedays$Cisco, breaks=c(-1,10,50,100,200,1000), labels=c('0-10','10-50','50-100','100-200','>200'))

ggplot(icedays, aes(x=ice.yr, y=IceAnnSum, fill=recruitment))+
  geom_bar(stat='identity')+
  plot_theme2+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  labs(x='Ice year', y='Total annual ice cover', title='Lake Superior Cumulative Annual Ice Days',
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')+
  scale_fill_viridis(discrete=TRUE, name='Age-1 cisco abundance\n(number/ha)')

ggsave(here('Plots and Tables/Ice_Temp','LS_CummIce_Recruitment.png'), dpi = 300, width = 20, height = 10, units = "cm") 



##n days >20% by year
age1<-read_xlsx(here('Plots and Tables/RVCAT','export_age1_annual_summary.xlsx'), sheet = 1) %>%
  mutate(ice.year = year.class) %>%
  select(c(2,4, 10))


ice20<-subset(ice, ice.cover>=20) ##if you want a number other than 20%, change it here
ice20<-aggregate(ice20$Jday, by=list(ice.year=ice20$ice.year), FUN=length)%>%
  renameCol('x','n.days')

ice20<-merge.data.frame(ice20, age1)
ice20$recruitment<-cut(ice20$Cisco, breaks=c(-1,10,50,100,200,1000), labels=c('0-10','10-50','50-100','100-200','>200'))
ggplot(ice20, aes(x=ice.year, y=n.days, fill=recruitment))+
  geom_bar(stat='identity')+
  plot_theme2+
  theme_bw()+
  labs(x='Ice year', y='Number of days with ice cover >20%', title='Lake Superior Days With Ice Cover Greater Than 20%',
       caption='Data: NOAA GLERL https://www.glerl.noaa.gov/data/ice/#historical')+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_viridis(discrete=T, name='Age-1 cisco abundance\n(number/ha)')

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
jdaytoday = 40

###Subset the data to only that day - JDay and then add data for this year
icetoday<-subset(ice, Jday == jdaytoday)

##Add row for this year's data that might not be in GLERL data set
icetoday2<-add_row(icetoday, Jday = jdaytoday, Year = 2022, IceCover = 1.4)

image = here('Plots and Tables/Ice_Temp','snowflake.png')

ggplot(icetoday2, aes(x=Year, y=IceCover)) +
  geom_image(aes(image=image), size= 0.05) +
  geom_segment(aes(x=Year, xend=Year, y=0, yend=IceCover), size=1, color='black')+
  geom_smooth(se = FALSE, size = 1.5, span = 0.5) +
  geom_smooth(method=lm, se = FALSE, colour='red') +
  plot_theme2 +
  labs(x='Ice year', y='% of total lake area', title='Lake Superior Historical Ice Cover for January',
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
  
  image = here('Plots and Tables/Ice_Temp','snowflake.png')
  
  ggplot(icemax, aes(x=Year, y=Jday)) +
    geom_image(aes(image=image), size= 0.05) +
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
    geom_image(aes(image=image), size= 0.05) +
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

 temp <- temp %>%
  pivot_longer(3:8, names_to = "lake", values_to = "temperature") 
 
 temp1 <- temp %>%
  filter(lake == 'Superior')  
 
ggplot(temp1, aes(x=jday, y=temperature, color=temperature))+
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
temp.prenino <-temp1 %>%
  filter(year<1998)
temp.prenino<-aggregate(temp.prenino$temperature, by=list(jday=temp.prenino$jday), FUN=mean)%>%
  renameCol('x','mean.prenino')

temp.postnino <-temp1 %>%
  filter(year>1997)
temp.postnino<-aggregate(temp.postnino$temperature, by=list(jday=temp.postnino$jday), FUN=mean)%>%
  renameCol('x','mean.postnino')

temp2<-merge.data.frame(temp1, temp.prenino)
temp3<-merge.data.frame(temp2, temp.postnino)


p<-ggplot(temp3, aes(x=jday, y=temperature, color=temperature))+
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
