

##install packages if needed, using install.package
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
library(gganimate)
library(magick)
library(grid)
library(ggforce)
library(lubridate)
library(ggridges)
library(here)

##load data 
data1 <- read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTData") %>%
  as.data.frame() 

data2 <-read_excel(here('Data',"LSBS_BTdata.xlsx"), sheet="BTSample") %>%
  as.data.frame() 

data2<-select(data2,1,4,5,11,14,15,17)

data2$Date<-format(as.POSIXct(data2$Date,format='%Y/%m/%d'),format='%Y/%m/%d')
##data2$Month<-format(as.POSIXct(data2$Date,format='%Y/%m/%d'),format='%m')
data2$Month<-month(ymd(data2$Date), label = TRUE, abbr = FALSE)
data2$Year<-format(as.POSIXct(data2$Date,format='%Y/%m/%d'),format='%Y')
data2$Time<-format(as.POSIXct(data2$Time,format='%Y/%m/%d %H:%M:%S'),format='%H:%M:%S')

data2$Longitude<-abs(data2$Longitude)*-1

data2 <- data1 %>%
  group_by(Sample) %>%
  right_join(data2) 

data1<-data2[order(data2$Date),] 
data1$Order<-1:nrow(data2)


###Subset data for most recent years
Sumdata1 <- data1 %>%
  subset(Target == 2 & Month == "June" & Year >2009 & Depth <101 |
         Target == 117 & Month == "July" & Year >2009 |
         Target == 118 & Month == "July" & Year >2009) 


#################################################################################
plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=16, family='serif'),
                  axis.title=element_text(size=20, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=16, family='serif'),
                  plot.caption=element_text(size=16, family='serif'),
                  legend.background = element_blank(),
                  legend.text=element_text(size=20, family='serif'),
                  legend.title=element_text(size=20, family='serif'),
  ##                legend.key = element_blank(),
                  strip.text=element_text(size=20, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0'
#############
########Heatmap - geom_gitter column show-all-data style
  facet_label <- c(June = "June - Nearshore Assessment" , July = "July - Offshore Assessment") 
  Sumdata1 %>% 
  ggplot(aes(x = Year, y = Depth, color=Temperature)) +
  geom_jitter() +
  scale_y_reverse() +
  scale_x_discrete(breaks=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  scale_color_gradient(low = "blue", high = "red", "Temperature") +
  labs(title = "Lake Superior Water Temperatures",
       subtitle = "USGS bottom trawl assessments",
       caption = ann_data_access,
       y = "Depth (m)", x = "Year") +
  guides(fill = guide_legend(nrow = 1))+
  plot_theme +
  theme(axis.text.x=element_text(size=16), 
         legend.position=c(0.1,-0.1), 
        legend.direction="horizontal") +
  facet_wrap(~Month, scales = "free_y", labeller = labeller(Month=facet_label)) 
  
ggsave(here('Plots and Tables/Ice_Temp','ns_os_wtemps1.png'), dpi = 300, width = 40, height = 20, units = "cm") 

str(Sumdata1)
#################################################################################################
#############
###Mean Temperature at Depth Plot - lines for each year
###Standard temperature at Depth plot
#####
facet_label <- c(June = "June - Nearshore Assessment" , July = "July - Offshore Assessment") 

Sumdata2 <-aggregate(Sumdata1$Temperature, by=list(Year=Sumdata1$Year, Month=Sumdata1$Month, Depth=Sumdata1$Depth), FUN=mean)%>%
  renameCol('x','mean_T')
Sumdata2$Year<-as.numeric(as.character(Sumdata2$Year))
Sumdata2<-subset(Sumdata2, Year>=(max(Year)-5))

Sumdata3 <-aggregate(Sumdata1$Temperature, by=list(Month=Sumdata1$Month, Depth=Sumdata1$Depth), FUN=mean)%>%
  renameCol('x','mean_T')
Sumdata3$Common<-'Mean'

ggplot(Sumdata2, aes(x=mean_T, y=Depth, color =as.factor(Year))) +
  geom_point() +
  scale_color_manual(values=c('mediumaquamarine','sienna1','mediumpurple1','orchid2','darkseagreen4','gold1','black'), name='Year')+
  geom_line(data=Sumdata3, aes(color=Common), size = 1.5) +
  scale_y_reverse() +
  scale_x_continuous(limits=c(0, NA), breaks=seq(13), expand=c(0,0)) +
  labs(title = "Lake Superior Water Temperatures",
       subtitle = "USGS bottom trawl assessments",
       caption = ann_data_access,
       y = "Depth (m)", x = "Mean water temperature (C)") +
  plot_theme + 
  guides(colour = guide_legend(override.aes = list(shape=c(16,16,16,16,16,16,NA),size = 4, linetype=c(0,0,0,0,0,0,1)))) +
  theme(legend.position=c(0.9,0.5)) +
  facet_wrap(~Month, scales = "free", labeller = labeller(Month=facet_label))

ggsave(here('Plots and Tables/Ice_Temp','ns_os_wtemps2.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##Summary Min, Max, Mean Temps for Report
Sumdata3 <-Sumdata1 %>%
  group_by(Month, Depth)%>%
  summarize(mean=mean(Temperature),min=min(Temperature),max=max(Temperature))

Sumdata4 <-Sumdata1 %>%
  group_by(Year, Month, Depth)%>%
  summarize(mean=mean(Temperature),min=min(Temperature),max=max(Temperature))


###################################################################################
##Ridges plot of June - July surface water temperatures
###################################################################################
facet_label <- c(June = "June - Nearshore Assessment" , July = "July - Offshore Assessment") 

Sumdata2 <- Sumdata1 %>% 
 subset(Depth <= 2) 

Sumdata3 <-Sumdata2 %>%
  group_by(Month) %>%
  summarize(median_T=median(Temperature), mean_T=mean(Temperature))

##ggridges plot with leopard look, median lines inside distribution not very visible
ggplot(Sumdata2, aes(x = Temperature, y = Year, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01,
  quantile_lines  = TRUE, quantiles = 2,    
  gradient_lwd = 0, jittered_points = TRUE, point_size = 1)  +
  scale_fill_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  geom_vline(data=Sumdata3, mapping = aes(xintercept=median_T), size = 1.2, color="red") +
  plot_theme + 
  theme(axis.text.x=element_text(size=16),
        legend.position=c(0.5,0.78),
        legend.title.align = 0.5, 
        legend.text.align = 0.8, 
        legend.direction="vertical",
        legend.text=element_text(size=20, family='serif')) +
  labs(title = "Lake Superior Near Surface Water Temperatures",
       subtitle = "USGS bottom trawl assessments",
       caption = "Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0",
       y = "Year", x = "Water temperature, <3 m deep (C)") +
  facet_wrap(~Month, scales = "free_x", labeller = labeller(Month=facet_label)) 

ggsave(here('Plots and Tables/Ice_Temp','ns_os_wtemps3a.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##ggridges plot with points below distribution, red lines are 10 year median & 1st, 2nd, 3rd quartiles in individual years
ggplot(Sumdata2, aes(x = Temperature, y = Year, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.2, rel_min_height = 0.01,
       quantile_lines  = TRUE,     
       gradient_lwd = 0, jittered_points = TRUE, point_size = 1,
       vline_size = 1.5, vline_color = "red", 
       position = position_raincloud(adjust_vlines = TRUE)) +
  scale_fill_viridis(name = "Temperature (C)", option = "C", alpha=0.6) +
  geom_vline(data=Sumdata3, mapping = aes(xintercept=median_T), size = 1.2, color="red") +
  plot_theme + 
  theme(axis.text.x=element_text(size=16),
        legend.position=c(0.5,0.78),
        legend.title.align = 0.5, 
        legend.text.align = 0.8, 
        legend.direction="vertical",
        legend.text=element_text(size=20, family='serif')) +
  labs(title = "Lake Superior Near Surface Water Temperatures",
       subtitle = "USGS bottom trawl assessments",
       caption = "Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0",
       y = "Year", x = "Water temperature, <3 m deep (C)") +
  facet_wrap(~Month, scales = "free_x", labeller = labeller(Month=facet_label)) 

ggsave(here('Plots and Tables/Ice_Temp','ns_os_wtemps3b.png'), dpi = 300, width = 40, height = 20, units = "cm") 



#################################################################################
##Map with geom_text date label

#######################################################################
##maps----------------------------------------------------------------------------------------------------------------------------
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


p<-ggplot(Sumdata1, aes(Longitude, Latitude)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ ##path for lake outline
  geom_path(data=Sumdata1, mapping=aes(Longitude,Latitude), size=1)+ ##path for lines connecting sites
  geom_jitter(data=Sumdata1, mapping=aes(Longitude, Latitude, color=Year), size=8,stroke=1.5)+ ##for the current site in the animation
  theme_bw() + 
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  theme(axis.text=element_text(size=20, family='serif'), 
        axis.title=element_text(size=20, family='serif'), 
        plot.title=element_text(size=24, family='serif'),
        plot.subtitle=element_text(size=20, family='serif'),
        plot.caption=element_text(size=16, family='serif'), 
        legend.position="none", 
        legend.text=element_blank(),
        legend.title=element_blank()) +
  labs(title='R/V Kiyi`s Annual Lake Superior Fish Collections, 2000-2019', 
       caption = 'U.S. Geological Survey, Lake Superior Biological Station') +
   ##    color='Water surface \ntemperature, C')+
    transition_reveal(Order)

q<-p +geom_jitter(aes(group=seq_along(Order), color=Year), size=6)+ ##to leave marks at past sites
  transition_reveal(Order)
r<-q+geom_text(aes(x=-91.75, y=49.05, label=Date), size=8, family='serif')+
  transition_reveal(Order)

p_gif<-animate(r, fps = 5, end_pause = 60, nframes=150, 
     width = 1000, height = 500, renderer = gifski_renderer(loop=F))
anim_save(here('Plots and Tables/Ice_Temp','RV_KiyiMap.gif'))

##

###Sidebar cumulative fish processing numbers
##THIS SECTION CURRENTLY DOESN'T DO ANYTHING, WOULD NEED TO UPLOAD FISH DATA IF YOU WANT THIS TO WORK, OR ADJUST FOR APPLICATION TO TEMP DATA IF THAT'S
##THE GOAL. THE WORKING VERSION OF THIS WITH FISH DATA IS IN THE RVCAT FILE
data4<-aggregate(data3$Fish, by=list(YEAR=data3$YEAR), FUN=sum)%>%
  renameCol('x','YrSum')
data4$Order<-1:nrow(data4)
data4$Fish.Caught<-'Total'

data4 %>%
  group_by(Fish.Caught) %>%
  mutate(space_below = cumsum(lag(YrSum, default = 0))) %>%
  ungroup() %>%
  
  ggplot() +
  geom_tile(aes(x=Fish.Caught, y=(space_below + YrSum/2), 
                width = 0.9, height = YrSum,
                fill = Order)) +
  scale_fill_gradientn(colours=rainbow(20))+
  scale_y_continuous(limits = c(0, 1500000),labels = scales::comma, expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  theme(axis.text.y=element_text(size=20, family='serif'), 
        axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=20, family='serif'), 
        legend.position = "none", 
        plot.title=element_text(size=20, family='serif'),
        plot.caption = element_text(size=20, family='serif'),
        axis.line=element_line(size=1, color='black'),
        panel.background=element_rect(NA),
        plot.margin = margin(.5,.5,.5,.5,"cm"))+
  labs(x='',y='Running Total: Fish Caught',title=' ')+
  transition_time(Order)+
  shadow_mark()->   anim_bar

#animate(anim_bar, fps = 20)
q_gif<-animate(anim_bar, fps = 4, end_pause = 60, nframes=150, 
               width = 200, height = 500, renderer = gifski_renderer(loop=F))  
q_gif
p_mgif<-image_read(p_gif)
q_mgif<-image_read(q_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif,'KiyiTrips.gif')  
  


