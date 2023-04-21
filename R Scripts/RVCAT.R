
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
library(broom)
library(maptools)
library(ggsn)


##load the raw RVCAT data file
##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
raw.data<-read.csv(here('Data','RVCAT.csv'))
raw.data$SPECIES<-as.factor(raw.data$SPECIES)
ShoreDistance <- read_xlsx(here('Data','ShoreDistance.xlsx'))


## get rid of records with no SPECIES code
raw.data <- raw.data %>%
  drop_na(SPECIES) %>%
  left_join(ShoreDistance)


##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)

raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data <- raw.data %>%
  mutate(Day = day(OP_DATE),
         Month = month(OP_DATE),
         Date = as.Date(OP_DATE, "%d-%m-%Y")) %>%
  arrange(Date) 

##Replace zeros which are known to be wrong to na for all variables other than SPECIES, NUM, WEIGHT
raw.data$TIME <- replace(raw.data$TIME, raw.data$TIME == 0, NA)             
raw.data$TOW_TIME <- replace(raw.data$TOW_TIME, raw.data$TOW_TIME == 0, NA)             
raw.data$FISHING_DEPTH <- replace(raw.data$FISHING_DEPTH, raw.data$FISHING_DEPTH == 0, NA)
raw.data$SURF_TEMP <- replace(raw.data$SURF_TEMP, raw.data$SURF_TEMP == 0, NA)
raw.data$BEG_SURF <- replace(raw.data$BEG_SURF, raw.data$BEG_SURF == 0, NA)
raw.data$END_SURF <- replace(raw.data$END_SURF, raw.data$END_SURF == 0, NA)
raw.data$BEG_BOTTOM <- replace(raw.data$BEG_BOTTOM, raw.data$BEG_BOTTOM == 0, NA)
raw.data$END_BOTTOM <- replace(raw.data$END_BOTTOM, raw.data$END_BOTTOM == 0, NA)

raw.data$STATE <- replace(raw.data$STATE, raw.data$STATE == 'E', 'ONT.E')
raw.data$STATE <- replace(raw.data$STATE, raw.data$STATE == 'W', 'ONT.W')
raw.data$STATE <- str_trim(raw.data$STATE)
raw.data$M_UNIT <- str_trim(raw.data$M_UNIT)

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
raw.data$Mid.Depth <- rowMeans(raw.data[,c("BEG_DEPTH", "END_DEPTH")], na.rm = TRUE)


raw.data <- raw.data %>%
  mutate(Mid.Lat.DD = ifelse(is.nan(Mid.Lat.DD), NA, Mid.Lat.DD),
         Mid.Long.DD = ifelse(is.nan(Mid.Long.DD), NA, Mid.Long.DD),
         Surface.Temp = ifelse(is.nan(Surface.Temp), NA, Surface.Temp),
         Bottom.Temp = ifelse(is.nan(Bottom.Temp), NA, Bottom.Temp),
         Mid.Depth = ifelse(is.nan(Mid.Depth), NA, Mid.Depth))

##Replace FISHING_DEPTH with END_DEPTH for bottom trawls and keep as is for mid-water trawls
raw.data <- raw.data %>%
  mutate(FISHING_DEPTH = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ END_DEPTH, 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ FISHING_DEPTH))

##add country based on states
raw.data <- raw.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'ONT.E'  ~ "Canada",
    STATE == 'ONT.W'  ~ "Canada"))

##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3,8,10))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)


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

## Vessel codes in the dataset
## Vessel == 1 - Siscowet
## Vessel == 4 - Kaho
## Vessel == 11 - Grayling
## Vessel == 99 - Coaster
## Vessel == 92 - Jon Boat
## Vessel == 50 - Whitefish
## Vessel == 25 - Kiyi
## Vessel == 9 - Daphnia
## Vessel == 95 - Northern Shoveler, FWS boat
## Vessel == 37 - Arcticus 

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
#  mutate(HA_SWEPT = (TOW_TIME/60)*2.05, 
#      Old speed factor was incorrect, changed to 2.45 mph, March 2022 
grayling <- raw.data %>%
  subset(VESSEL==11) %>% 
  mutate(HA_SWEPT = (TOW_TIME/60)*2.45, 
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

##r/v Arcticus - 39 foot bottom trawls
arcticus_tr4_25 <- raw.data %>% 
  subset(VESSEL==37 & TR_DESIGN == 4 |  VESSEL==37 & TR_DESIGN == 25) %>% 
  mutate(HA_SWEPT = ((DISTANCE*5280)*25.49)/107639.1, 
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
                      daphnia, 
                      arcticus_tr4_25)

##check that the number of rows in all.data matches the number in the raw.data input to make sure no data got lost along the way
##if data were lost, were any additional vessels used that are not listed above? any vessel codes changed or recorded incorrectly?

####################################################################################

##set default themes for all plots and maps
map_theme<-theme(strip.text=element_text(size=24, family='serif'),
                 axis.text=element_text(size=24, family='serif'),
                 axis.title=element_text(size=24, family='serif'),
                 legend.text=element_text(size=24, family='serif'),
                 legend.title=element_text(size=24, family='serif'),
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=22, family='serif'),
                 plot.caption=element_text(size=20, family='serif'),
                 legend.position=c(0.1,0.7))

plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  strip.text=element_text(size=24, family='serif'),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  legend.text=element_text(size=24, family='serif'),
                  legend.title=element_text(size=24, family='serif'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=22, family='serif'),
                  plot.caption=element_text(size=20, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/P9XVOLR1'


#########################################################################################################
#########################################################################################################
#########################################################################################################
##Animated travel map with fish collected bar
##Map with geom_text date label

trips.fish1 <- all.data %>%
  select(OP_ID, SPECIES, NUM) %>%
  group_by(OP_ID) %>%
  summarise(Fish = n(),
            TFish = sum(NUM)) %>%
  ungroup

trips.fish2 <- all.data %>%
  select(OP_ID, Date, YEAR, Mid.Lat.DD, Mid.Long.DD, VESSEL, TR_DESIGN, Mid.Depth) %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  left_join(trips.fish1) %>%
  arrange(Date) %>%
  subset(YEAR >= 2000) 

trips.fish2$Order<-1:nrow(trips.fish2) ##for the order that the points will appear on the map

#######################################################################
##maps----------------------------------------------------------------------------------------------------------------------------
##Map outline
ls_poly <- readOGR(dsn =here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


p<-ggplot(trips.fish2, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
#  geom_path(data=trips.fish2, mapping=aes(Mid.Long.DD, Mid.Lat.DD), size=1)+ 
  geom_jitter(data=trips.fish2, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=YEAR), size=8, stroke=1.5)+ 
  scale_color_gradientn(colours=rainbow(10), name = 'Year') +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  plot_theme +
  map_theme +
  theme(legend.position=c(0.15,0.75),
        axis.text=element_text(size=28, family='serif'),
        axis.title=element_text(size=28, family='serif'),
        plot.title=element_text(size=28, family='serif'),
        legend.text=element_text(size=28, family='serif'),
        legend.title=element_text(size=28, family='serif'),
        plot.caption=element_text(size=20, family='serif')) +
#  geom_text(aes(Mid.Long.DD, Mid.Lat.DD, label=OP_ID)) +
  labs(title='U.S. Geological Survey Lake Superior Fish Assessments',
       caption=ann_data_access) +
  transition_reveal(Order)

q<-p + geom_jitter(aes(group=seq_along(Order), color=YEAR), size=6)+ ##to leave marks at past sites
  transition_reveal(Order)
r<-q + geom_text(aes(x=-85, y=49, label=Date), size=12, family='serif') +
  geom_text(aes(x=-86.2, y=49, label='Date:'), size=12, family='serif') +
  transition_reveal(Order)

p_gif<-animate(r, fps = 5, end_pause = 60, nframes=150, 
               width = 1000, height = 500, renderer = gifski_renderer(loop=F))
anim_save('KiyiTrips.gif')

###################
trips.fish3 <- trips.fish2 %>%
  group_by(YEAR) %>%
  summarise(YrSum = sum(TFish)) %>%
  mutate(Fish.Caught = 'Total')

trips.fish3$Order<-1:nrow(trips.fish3)

trips.fish3 <- trips.fish3 %>% 
  group_by(Fish.Caught) %>%
  mutate(space_below = cumsum(lag(YrSum, default = 0))) %>%
  ungroup() 

################################
ggplot(trips.fish3) +
  geom_tile(aes(x=Fish.Caught, y=(space_below + YrSum/2), 
                width = 0.9, height = YrSum,
                fill = Order)) +
  scale_fill_gradientn(colours=rainbow(10))+
  scale_y_continuous(labels = scales::comma, expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  theme(axis.text.y=element_text(size=28, family='serif'), 
        axis.text.x=element_blank(),
        axis.title.x=element_blank(), 
        axis.title.y=element_text(size=28, family='serif'), 
        legend.position = "none", 
        plot.title=element_text(size=20, family='serif'),
        plot.caption = element_text(size=20, family='serif'),
        axis.line=element_line(size=1, color='black'),
        panel.background=element_rect(NA),
        plot.margin = margin(.5,.5,.5,.5,"cm"))+
  labs(x='',
       y='Total Fish Caught',
       title='')+
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_LSBSFish.gif'))


###########################################################################################################################
###CSMI DATA
###########################################################################################
sci.names<-select(codes.to.names, c(1:3,8,10))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

csmi <- all.data %>%
  subset(TARGET==117) %>%
  select(OP_ID, Date, TIME, YEAR, LOCATION, TARGET, TR_DESIGN, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, 
         ShoreDistance, BEG_DEPTH, END_DEPTH, FISHING_DEPTH, Surface.Temp, Bottom.Temp, TOW_TIME, 
         DISTANCE, HA_SWEPT, SPECIES, NUM, WT, NOHA, KGHA) %>%
  mutate(survey = 'CSMI',
         Depth.Zone = case_when(
           Mid.Depth <=30 ~ "<30 m",
           Mid.Depth >30 & Mid.Depth <= 100 ~ "30-100 m",
           Mid.Depth >100 & Mid.Depth <= 200 ~ "100-200 m", 
           Mid.Depth >200 ~ ">200 m"),
         Gear = case_when(
           TR_DESIGN == 4 ~ "Bottom trawl",
           TR_DESIGN == 25 ~ "Bottom trawl",
           TR_DESIGN == 28 ~ "Mid-water trawl"),
         Assemblage = case_when(
           TR_DESIGN == 4 ~ "Benthic fish", 
           TR_DESIGN == 25 ~ "Benthic fish",
           TR_DESIGN == 28 ~ "Pelagic fish")) %>%
  left_join(ShoreDistance)

csmi$SPECIES[csmi$SPECIES == 213] <- 217
csmi$SPECIES[csmi$SPECIES == 218] <- 217

csmi <- csmi %>%
  left_join(sci.names)
  

csmi.locations <- csmi %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  select(LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, ShoreDistance, Depth.Zone) 


csmi.Catch.NoZeros <- csmi %>%
  select(OP_ID, survey, Date, TIME, YEAR, LOCATION,
         Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Depth.Zone, Gear, Assemblage, 
         ShoreDistance, BEG_DEPTH, END_DEPTH, FISHING_DEPTH, Surface.Temp, Bottom.Temp, TOW_TIME, 
         DISTANCE, HA_SWEPT,SPECIES, COMMON_NAME,SCIENTIFIC_NAME, NUM, WT, NOHA, KGHA) 

##If weights were NOT taken replace KGHA with 'na'
csmi.Catch.NoZeros$KGHA[csmi.Catch.NoZeros$KGHA == 0 & csmi.Catch.NoZeros$NUM >0] <- NA

##Pull out all individual trawls
csmi.trawls<-csmi  %>%
  select(OP_ID, survey, Date, TIME, YEAR, LOCATION,
         Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Depth.Zone, Gear, Assemblage, 
         ShoreDistance, BEG_DEPTH, END_DEPTH, FISHING_DEPTH, Surface.Temp, Bottom.Temp, TOW_TIME, 
         DISTANCE, HA_SWEPT) %>%
  distinct(OP_ID, .keep_all = TRUE)

###Add zeros to NUM for fish species that were not caught
csmi.trawls.zeros.allspecies <- csmi  %>%
  select(OP_ID, survey, SPECIES, NUM, WT, NOHA, KGHA) 

csmi.trawls.zeros.allspecies <-complete(csmi.trawls.zeros.allspecies, OP_ID, survey, SPECIES, 
                                        fill=list(NUM=0, WT=0, NOHA=0, KGHA=0)) 

csmi.tfish.trawls <- csmi.trawls.zeros.allspecies %>%
  group_by(OP_ID) %>%
  summarize(Tfish = sum(NUM)) %>%
  ungroup()

##get rid of trawls where SPECIES == 0 (NO FISH CAUGHT) and where fish were actually caught
csmi.trawls.zeros.allspecies <- csmi.trawls.zeros.allspecies %>%
  left_join(csmi.tfish.trawls) %>%
  #  subset(Tfish > 0 & SPECIES != 0 | Tfish == 0 & SPECIES == 0)  %>%
  left_join(csmi.trawls) %>% 
  left_join(sci.names) 

csmi.Catch.Zeros <- csmi.trawls.zeros.allspecies %>%
  select(OP_ID, survey, Date, TIME, YEAR, LOCATION,
         Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Depth.Zone, Gear, Assemblage, 
         ShoreDistance, BEG_DEPTH, END_DEPTH, FISHING_DEPTH, Surface.Temp, Bottom.Temp, TOW_TIME, 
         DISTANCE, HA_SWEPT, SPECIES, COMMON_NAME, SCIENTIFIC_NAME, NUM, WT, NOHA, KGHA) 

##If weights were NOT taken replace KGHA with 'na'
csmi.Catch.Zeros$KGHA[csmi.Catch.Zeros$KGHA == 0 & csmi.Catch.Zeros$NUM >0] <- NA

##Get rid of records where SPECIES == 0 and fish were caught in that trawl
csmi.Catch.Zeros <- csmi.Catch.Zeros %>%
  left_join(csmi.tfish.trawls) %>%
  subset(SPECIES == 0 & Tfish ==0 | Tfish >0 & SPECIES !=0) %>%
  select(OP_ID, survey, Date, TIME, YEAR, LOCATION,
         Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Depth.Zone, Gear, Assemblage, 
         ShoreDistance, BEG_DEPTH, END_DEPTH, FISHING_DEPTH, Surface.Temp, Bottom.Temp, TOW_TIME, 
         DISTANCE, HA_SWEPT, SPECIES, COMMON_NAME, SCIENTIFIC_NAME, NUM, WT, NOHA, KGHA) 


#######################################################################
#################
##NOT NEEDED
##Calculate total number of species caught that year
#csmi.annual.total.fish <- csmi.trawls.zeros.allspecies %>% 
#subset(NUM>0) %>%
#  group_by(YEAR, Gear) %>%
#  distinct(SPECIES) %>%
#  summarise(fishes.total = n()) %>%
#  ungroup() 

##Sum KGHA for native and exotic species
#csmi.trawls.sum.type<- csmi %>%
#  select(OP_ID,TYPE, KGHA) %>%
#  group_by(OP_ID, TYPE) %>%
#  summarise(TYPE.KGHA = sum(KGHA)) %>%
#  pivot_wider(names_from = TYPE, values_from = TYPE.KGHA, values_fill = 0) %>%
#  renameCol('native', 'native.KGHA') %>%
#  renameCol('exotic', 'exotic.KGHA') %>%
#  ungroup() 

##Sum KGHA for prey and predator
#csmi.trawls.sum.diet<- csmi %>%
#  select(OP_ID,DIET, KGHA) %>%
#  group_by(OP_ID, DIET) %>%
#  summarise(DIET.KGHA = sum(KGHA)) %>%
#  pivot_wider(names_from = DIET, values_from = DIET.KGHA, values_fill = 0) %>%
#  renameCol('prey', 'prey.KGHA') %>%
#  renameCol('predator', 'predator.KGHA')  %>%
#  ungroup()


##Sum KGHA for prey and predator by native and exotic
#csmi.trawls.sum.type.diet<- csmi %>%
#  select(OP_ID, TYPE, DIET, KGHA) %>%
#  mutate(type_diet = paste(TYPE, DIET, sep = ".")) %>%
#  group_by(OP_ID, type_diet) %>%
#  summarise(TYPE.DIET.KGHA = sum(KGHA)) %>%
#  pivot_wider(names_from = type_diet, values_from = TYPE.DIET.KGHA, values_fill = 0) %>%
#  ungroup() %>%
#  renameCol('native.prey', 'native.prey.KGHA') %>%
#  renameCol('native.predator', 'native.predator.KGHA') %>%
#  renameCol('exotic.prey', 'exotic.prey.KGHA') %>%
#  renameCol('exotic.predator', 'exotic.predator.KGHA')

##Sum KGHA and NOHA across all species to get total biomass and total number per ha
#csmi.trawls.sum<- csmi %>%
#  select(OP_ID,NUM,NOHA,KGHA) %>%
#  group_by(OP_ID) %>%
#  summarise(KGHA = sum(KGHA), NUM=sum(NUM), NOHA = SUM(NOHA), Fishes = n()) %>%
#  left_join(csmi.trawls) %>% 
#  left_join(csmi.trawls.sum.type) %>%
#  left_join(csmi.trawls.sum.diet) %>%
#  left_join(csmi.trawls.sum.type.diet) %>%
#  mutate(Fishes = replace(Fishes, NOHA == 0, 0)) %>%
#  ungroup() 


#csmi.zerofish <- csmi.trawls.sum %>%
#  subset(NUM == 0) %>%
#  group_by(YEAR, Gear) %>% 
#  summarise(sites.zerofish=n()) %>%
#  ungroup() %>%
#  select(YEAR, Gear, sites.zerofish) 

#csmi.zerofish$sites.zerofish[is.na(csmi.zerofish$sites.zerofish)] <- 0 
 
###########################################################################################
###########################################################################################
#####################################################################################################################


#################################################################################################################
##Add CSMI Acoustics  data
##No midwater trawl data from sites 2059 and 2132 in 2022, so fill in effort from previous years for acoustic collections in 2022
###########################################################################################
#Build file with all CSMI data - bottom trawl, mid-water trawl, acoustics
###CSMI acoustics effort

csmi.acoustics.effort <- csmi %>%
  subset(TARGET==117) %>%
  subset(TR_DESIGN==28) %>% 
  select(OP_ID, Date, TIME, YEAR, LOCATION, Mid.Lat.DD, Mid.Long.DD,  ShoreDistance, TR_DESIGN, 
         Mid.Depth, FISHING_DEPTH, DISTANCE, TOW_TIME, Surface.Temp, Bottom.Temp) %>%
  mutate(Depth.Zone = case_when(
    Mid.Depth <=30 ~ "<30 m",
    Mid.Depth >30 & Mid.Depth <= 100 ~ "30-100 m",
    Mid.Depth >100 & Mid.Depth <= 200 ~ "100-200 m", 
    Mid.Depth >200 ~ ">200 m")) %>%
  distinct(LOCATION, YEAR, .keep_all = TRUE) %>%
  mutate(Gear = "Acoustics") %>%
  mutate(Assemblage = "Pelagic fish")


##No midwater trawl data from sites 2059 and 2132 in 2022, so fill in effort from previous years
csmi.acoustics.2022.miss_sites <- csmi.acoustics.effort %>%
  subset(LOCATION == 2059 |
           LOCATION == 2132) %>% 
  group_by(LOCATION) %>%
  summarise(Mid.Lat.DD = mean(Mid.Lat.DD),
            Mid.Long.DD = mean(Mid.Long.DD),
            Mid.Depth = mean(Mid.Depth),
            FISHING_DEPTH = mean(FISHING_DEPTH)) %>%
  ungroup() %>% 
  mutate(Depth.Zone = case_when(
    Mid.Depth <=30 ~ "<30 m",
    Mid.Depth >30 & Mid.Depth <= 100 ~ "30-100 m",
    Mid.Depth >100 & Mid.Depth <= 200 ~ "100-200 m", 
    Mid.Depth >200 ~ ">200 m"),
    YEAR = 2022,
    Gear = 'Acoustics',
    Assemblage = 'Pelagic fish', 
    Date = case_when(
      LOCATION == 2059 ~ '2022-9-30', 
      LOCATION == 2132 ~ '2022-9-19'),
    Surface.Temp = case_when(
      LOCATION == 2132 ~ 17.2),
    Bottom.Temp = case_when(
      LOCATION == 2132 ~ 3.7),
    ShoreDistance = case_when(
      LOCATION == 2059 ~ 6380.55, 
      LOCATION == 2132 ~ 12712.09)) 


csmi.acoustics.2022.miss_sites$Date<-as.POSIXct(csmi.acoustics.2022.miss_sites$Date, format="%Y-%m-%d") 


csmi.acoustics.effort <- csmi.acoustics.effort %>%
  bind_rows(csmi.acoustics.2022.miss_sites)


##Get CSMI acoustics fish
csmi.acoustics.fish <- read_excel(here('Data', 'CSMIacoustics.xlsx'))  %>%
  left_join(csmi.acoustics.effort) %>%
  select(OP_ID, COMMON_NAME, NOHA, KGHA, Date, TIME, YEAR, LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, FISHING_DEPTH,
         ShoreDistance, Depth.Zone, Gear, Assemblage,
         Surface.Temp, Bottom.Temp) 

##Clean up trawl fish to match acoustics fields
csmi.nocatch <- csmi.Catch.Zeros %>%
  group_by(COMMON_NAME) %>%
  summarise(TCatch = sum(NUM))


csmi.Catch.Zeros <- csmi.Catch.Zeros %>%
  left_join(csmi.nocatch) %>%
  subset(TCatch > 0 | SPECIES == 0) 

csmi.trawl.fish <- csmi.Catch.Zeros %>%
  select(OP_ID, COMMON_NAME, NUM, WT, NOHA, KGHA, Date, TIME, YEAR, LOCATION, 
         Mid.Lat.DD, Mid.Long.DD, Mid.Depth, FISHING_DEPTH,
         ShoreDistance, Depth.Zone, Gear, Assemblage,
         TOW_TIME, DISTANCE, Surface.Temp, Bottom.Temp)

##Combine files to include all fish- bottom trawl, mid-water trawl, acoustics
csmi.all.fish.all <- csmi.trawl.fish %>%
  bind_rows(csmi.acoustics.fish)


############################################################################################################################
##implenent rules to select single best gear for each species 
csmi.fish.rules <- csmi.all.fish.all %>%
  group_by(COMMON_NAME, Gear) %>%
  summarise(catch.total = sum(NUM)) %>%
  ungroup() %>%
  pivot_wider(names_from = Gear, values_from = catch.total, values_fill = 0) %>%
  mutate(Acoustics = ifelse(is.na(Acoustics), 0, Acoustics)) %>%
  mutate(Catch.total = rowSums(across(c(2:4)))) %>%
  mutate(COMMON_NAME2 = case_when(
    Catch.total <= 500 ~ 'Other', 
    Catch.total > 500 ~ COMMON_NAME)) %>%
  mutate(COMMON_NAME2 = case_when(
    COMMON_NAME == 'ciscoe YOY' ~ COMMON_NAME,
    COMMON_NAME != 'ciscoe YOY' ~ COMMON_NAME2)) %>%
  pivot_longer(names_to = "Gear", cols = c("Acoustics", "Bottom trawl", "Mid-water trawl")) %>%
  select(COMMON_NAME, COMMON_NAME2, Gear) %>%
  mutate(Keep.fish = case_when(
    COMMON_NAME == 'Bloater' & Gear == 'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Burbot' & Gear == 'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Cisco' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'ciscoe YOY' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'Chinook Salmon' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'Coho Salmon' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'Deepwater Sculpin' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'hatchery Lake Trout' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Johnny Darter' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Kiyi' & Gear == 'Acoustics' ~ 'Y',
    COMMON_NAME == 'Lake Whitefish' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'lean Lake Trout' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Longnose Sucker' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Ninespine Stickleback' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Pygmy Whitefish' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Rainbow Smelt' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'Ruffe' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Sea Lamprey' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Shortjaw Cisco' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'siscowet Lake Trout' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Slimy Sculpin' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Splake' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'Spoonhead Sculpin' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Threespine Stickleback' & Gear ==  'Acoustics' ~ 'Y',
    COMMON_NAME == 'Trout-Perch' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Unidentified Coregonid' & Gear ==  'Bottom trawl' ~ 'Y',
    COMMON_NAME == 'Walleye' & Gear ==  'Acoustics' ~ 'Y')) %>%
  replace_na(list(Keep.fish = 'N')) %>%
  mutate(Assemblage = case_when(
    Gear == 'Bottom trawl' ~ 'Benthic fish',
    Gear == 'Mid-water trawl' ~ 'Pelagic fish', 
    Gear == 'Acoustics' ~ 'Pelagic fish')) %>%  
  select(Keep.fish, COMMON_NAME, COMMON_NAME2, Gear, Assemblage)

####################################################################################################################
##Make file for analyses based on above rules

csmi.all.fish.keep <- csmi.all.fish.all %>%
    left_join(csmi.fish.rules)
    

csmi.all.fish <- csmi.all.fish.keep %>%
  select(COMMON_NAME, COMMON_NAME2, Gear, Assemblage, Keep.fish, Date, YEAR, 
         LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Depth.Zone, 
         ShoreDistance, FISHING_DEPTH, TOW_TIME, DISTANCE, Surface.Temp, Bottom.Temp,
         NUM, WT, NOHA, KGHA)


##CSMI effort from all gears all years
csmi.all.effort <- csmi.all.fish %>%
  distinct(YEAR, LOCATION, Gear, .keep_all = TRUE) %>%
  select(Date, YEAR, LOCATION, Gear, Assemblage, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, Depth.Zone, 
         ShoreDistance, FISHING_DEPTH, TOW_TIME, DISTANCE, Surface.Temp, Bottom.Temp)


##Summarise by year and gear
na.rm=TRUE
csmi.all.effort.sum <- csmi.all.effort %>%
  distinct(YEAR, LOCATION, Gear, .keep_all = TRUE) %>%
  group_by(YEAR, Gear) %>%
  summarise(locations=n(), 
            min.Date=min(Date), max.Date=max(Date),
            min.year=min(YEAR), max.year=max(YEAR), 
            mean.dist.km=mean(DISTANCE)*1.60934, median.dist.km = median(DISTANCE*1.60934), 
            min.dist.km=min(DISTANCE)*1.60934, max.dist.km=max(DISTANCE)*1.60934,
            mean.towtime=mean(TOW_TIME),  median.towtime=median(TOW_TIME), 
            min.towtime=min(TOW_TIME), max.towtime=max(TOW_TIME),
            median.Mid.Depth = median(Mid.Depth), min.Mid.Depth=min(Mid.Depth), max.Mid.Depth=max(Mid.Depth),
            min.fishingdepth=min(FISHING_DEPTH), max.fishingdepth=max(FISHING_DEPTH), 
            median.fishingdepth=median(FISHING_DEPTH), 
            mean.fishingdepth=mean(FISHING_DEPTH),
            mean.surface.temp=mean(Surface.Temp), min.surface.temp=min(Surface.Temp), max.surface.temp=max(Surface.Temp),
            mean.bottom.temp=mean(Bottom.Temp), min.bottom.temp=min(Bottom.Temp), max.bottom.temp=max(Bottom.Temp)) %>%
  ungroup()


##calculate annual summary by location by year and gear
csmi.annual.location.sum <- csmi.all.fish %>%
  filter(Keep.fish == 'Y') %>%
  group_by(YEAR, LOCATION, Gear) %>%
  summarise(catch.total = sum(NUM), 
            NOHA = sum(NOHA), 
            KGHA = sum(KGHA)) %>%
  ungroup() %>%
  left_join(csmi.all.effort)


##calculate annual summary stats by year and gear
csmi.annual.sum <- csmi.annual.location.sum %>%
  group_by(YEAR, Gear) %>%
  summarise(NOHA.max = max(NOHA), NOHA.min = min(NOHA), NOHA.mean = mean(NOHA), NOHA.median = median(NOHA), NOHA.se = std.error(NOHA),
            KGHA.max = max(KGHA), KGHA.min = min(KGHA), KGHA.mean = mean(KGHA), KGHA.median = median(KGHA), KGHA.se = std.error(KGHA)) %>%
  ungroup() 

##calculate annual summary stats by depth zone by year and gear
csmi.annual.dz.sum <- csmi.annual.location.sum %>%
  group_by(YEAR, Gear, Depth.Zone) %>%
  summarise(NOHA.max = max(NOHA), NOHA.min = min(NOHA), NOHA.mean = mean(NOHA), NOHA.median = median(NOHA), NOHA.se = std.error(NOHA),
            KGHA.max = max(KGHA), KGHA.min = min(KGHA), KGHA.mean = mean(KGHA), KGHA.median = median(KGHA), KGHA.se = std.error(KGHA)) %>%
  ungroup() 


##calculate annual summary stats by species by year and gear
csmi.annual.species.sum <- csmi.all.fish %>%
  group_by(YEAR, Gear, COMMON_NAME) %>%
  summarise(sites.occur =  sum(NOHA>0), 
            catch.total = sum(NUM), catch.max = max(NUM), catch.min = min(NUM), catch.mean = mean(NUM), catch.median = median(NUM),
            NOHA.max = max(NOHA), NOHA.min = min(NOHA), NOHA.mean = mean(NOHA), NOHA.median = median(NOHA), NOHA.se = std.error(NOHA),
            KGHA.max = max(KGHA), KGHA.min = min(KGHA), KGHA.mean = mean(KGHA), KGHA.median = median(KGHA), KGHA.se = std.error(KGHA)) %>%
  ungroup() %>%
  left_join(csmi.fish.rules) 

##calculate annual summary stats by species by depth zone and year and gear
csmi.annual.species.dz.sum <- csmi.all.fish %>%
  group_by(YEAR, Depth.Zone, Gear, COMMON_NAME) %>%
  summarise(sites.occur =  sum(NOHA>0), 
            catch.total = sum(NUM), catch.max = max(NUM), catch.min = min(NUM), catch.mean = mean(NUM), catch.median = median(NUM),
            NOHA.max = max(NOHA), NOHA.min = min(NOHA), NOHA.mean = mean(NOHA), NOHA.median = median(NOHA), NOHA.se = std.error(NOHA),
            KGHA.max = max(KGHA), KGHA.min = min(KGHA), KGHA.mean = mean(KGHA), KGHA.median = median(KGHA), KGHA.se = std.error(KGHA)) %>%
  ungroup() %>%
  left_join(csmi.fish.rules) %>%
  mutate(TFish = case_when(
    Depth.Zone == '<30 m' ~ NOHA.mean * 387000,
    Depth.Zone == '30-100 m' ~ NOHA.mean * 1647200,  
    Depth.Zone == '100-200 m' ~ NOHA.mean * 3726900,  
    Depth.Zone == '>200 m' ~ NOHA.mean * 2287600)) %>%
  mutate(TBiomass = case_when(
    Depth.Zone == '<30 m' ~ (KGHA.mean * 387000) * 0.001,
    Depth.Zone == '30-100 m' ~ (KGHA.mean * 1647200) * 0.001,  
    Depth.Zone == '100-200 m' ~ (KGHA.mean * 3726900) * 0.001,  
    Depth.Zone == '>200 m' ~ (KGHA.mean * 2287600) * 0.001)) 


##Lakewide total abundance and biomass
csmi.fish.lakewide <- csmi.annual.species.dz.sum %>%
  filter(Keep.fish == 'Y') %>%
  group_by(YEAR, COMMON_NAME) %>%
  summarize(TFish = sum(TFish),
            TBiomass = sum(TBiomass)) %>%
  ungroup() %>%
  left_join(csmi.fish.rules) %>% 
  filter(Keep.fish == 'Y') %>%
  mutate(GearHabitat = case_when(
    Assemblage == 'Benthic fish' ~ 'Benthic fish - bottom trawls',
    Assemblage == 'Pelagic fish' ~ 'Pelagic fish - acoustics'))

 
##Lakewide total abundance and biomass by depth zone
csmi.fish.lakewide.dz <- csmi.annual.species.dz.sum %>%
  filter(Keep.fish == 'Y') %>%
  group_by(YEAR, Gear, Depth.Zone, COMMON_NAME) %>%
  summarize(TFish = sum(TFish),
            TBiomass = sum(TBiomass)) %>%
  ungroup() %>%
  left_join(csmi.fish.rules) %>% 
  filter(Keep.fish == 'Y') 

#############################################################################################################
##Table for table in report

##Add total fish and total abundance to species annual sum
csmi.total.catch <- csmi.annual.species.dz.sum  %>%
  group_by(YEAR, COMMON_NAME) %>%
  summarize(TFish = sum(TFish),
            TBiomass = sum(TBiomass)) %>%
  ungroup() %>%
  right_join(csmi.annual.species.sum) %>%
  left_join(sci.names) %>%
  select(COMMON_NAME, SCIENTIFIC_NAME, YEAR, Gear, sites.occur, catch.total, TFish, TBiomass) %>%
  pivot_wider(names_from = c(Gear, YEAR), values_from = c(sites.occur, catch.total, TFish, TBiomass), values_fill = 0) %>%
  renameCol('sites.occur_Acoustics_2011', 'AC.sites.2011') %>%
  renameCol('sites.occur_Mid-water trawl_2011', 'MWT.sites.2011') %>%
  renameCol('sites.occur_Bottom trawl_2011', 'BT.sites.2011') %>%
  renameCol('sites.occur_Acoustics_2016', 'AC.sites.2016') %>%
  renameCol('sites.occur_Mid-water trawl_2016', 'MWT.sites.2016') %>%
  renameCol('sites.occur_Bottom trawl_2016', 'BT.sites.2016') %>%
  renameCol('sites.occur_Acoustics_2022', 'AC.sites.2022') %>%
  renameCol('sites.occur_Mid-water trawl_2022', 'MWT.sites.2022') %>%
  renameCol('sites.occur_Bottom trawl_2022', 'BT.sites.2022') %>%
  renameCol('catch.total_Mid-water trawl_2011', 'MWT.catch.2011') %>%
  renameCol('catch.total_Bottom trawl_2011', 'BT.catch.2011') %>%
  renameCol('catch.total_Mid-water trawl_2016', 'MWT.catch.2016') %>%
  renameCol('catch.total_Bottom trawl_2016', 'BT.catch.2016') %>%
  renameCol('catch.total_Mid-water trawl_2022', 'MWT.catch.2022') %>%
  renameCol('catch.total_Bottom trawl_2022', 'BT.catch.2022') %>%
  select(COMMON_NAME, SCIENTIFIC_NAME,
         AC.sites.2011,
         MWT.sites.2011,
         BT.sites.2011,
         AC.sites.2016,
         MWT.sites.2016,
         BT.sites.2016,
         AC.sites.2022,
         MWT.sites.2022,
         BT.sites.2022,
         MWT.catch.2011,
         BT.catch.2011,
         MWT.catch.2016,
         BT.catch.2016,
         MWT.catch.2022,
         BT.catch.2022) %>%
  mutate_if(is.numeric, round, 0)



###Lakewide summary table for report with best gear estimates
csmi.annual.species.lakewide.table <- csmi.annual.species.sum %>%
  select(YEAR, Gear, COMMON_NAME, NOHA.mean, KGHA.mean) 
  
csmi.fish.lakewide.table <- csmi.fish.lakewide %>% 
  left_join(csmi.annual.species.lakewide.table) %>%
  pivot_wider(names_from = c(YEAR), values_from = c(NOHA.mean, KGHA.mean, TFish, TBiomass), values_fill = 0)
 
csmi.annual.sum.table <- csmi.fish.lakewide.dz %>%
  group_by(YEAR, Gear) %>%
  summarise(TFish = sum(TFish),
            TBiomass =sum(TBiomass)) %>%
  ungroup() 

csmi.annual.sum <- csmi.annual.sum %>%
  left_join(csmi.annual.sum.table)


#############################################################################################################################
###Sampling location map by depth zone
####
##Get nearshore and CSMI site locations for a site map
csmi.ns.sites <- all.data %>%
  subset(TARGET==117 |
           TARGET == 2 & YEAR == 2022) %>% 
  select(OP_ID, Date, YEAR, TARGET, LOCATION, Mid.Lat.DD, Mid.Long.DD,  ShoreDistance, Mid.Depth) %>%
  distinct(LOCATION, .keep_all = TRUE) %>% 
  mutate(survey = case_when(
    TARGET == 2 ~ 'Nearshore',
    TARGET == 117 ~ 'CSMI')) %>%
  mutate(Depth.Zone = case_when(
    Mid.Depth <=30 ~ "<30 m",
    Mid.Depth >30 & Mid.Depth <= 100 ~ "30-100 m",
    Mid.Depth >100 & Mid.Depth <= 200 ~ "100-200 m", 
    Mid.Depth >200 ~ ">200 m"))

##Map outline
ls_poly <- readOGR(dsn =here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


####2022 nearshore and CSMI Site map
ggplot(csmi.ns.sites, 
       aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.ns.sites, TARGET == 117), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, fill = Depth.Zone), 
             shape = 21, size = 6, stroke=1.5) +
  geom_point(data=subset(csmi.ns.sites, TARGET == 2), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
             color = 'black', size = 3, stroke=1.5) +
  map_theme +
  scale_fill_brewer(palette = 'Pastel1', limits=c("<30 m","30-100 m", "100-200 m", ">200 m"), name = 'CSMI\nDepth zone (m)') +
  labs(caption=ann_data_access,
       title='USGS Lake Superior Nearshore and CSMI Surveys') +
  theme(legend.position=c(0.15,0.75))

ggsave(here('Plots and Tables/CSMI','CSMI.NS.Sites.ByDepthZone.Map.png'), dpi = 300, width = 40, height = 20, units = "cm") 

####2022 CSMI Site map
##Map of sites with Nearshore, Offshore, and CSMI indicated
img <- image_read(here('Data/NorthArrow.jpg'))
narrow <- rasterGrob(img)

ggplot(subset(csmi.ns.sites, survey = "CSMI"), 
       aes(Mid.Long.DD, Mid.Lat.DD)) +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.ns.sites, TARGET == 117), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, fill = Depth.Zone), 
             shape = 21, size = 6, stroke=1.5) +
  theme_bw() +
  map_theme +
  scale_fill_brewer(palette = 'Pastel1', limits=c("<30 m","30-100 m", "100-200 m", ">200 m"), name = 'Depth zone (m)') +
  annotation_custom(grob = narrow, xmin = -85.6, xmax = -84.45, ymin = 48.5, ymax = 49) +
  labs(caption=ann_data_access,
       title='U.S. Geological Survey - U.S. Environmental Protection Agency Lake Superior CSMI Survey') +
  theme(legend.position=c(0.15,0.75))

ggsave(here('Plots and Tables/CSMI','CSMI.Sites.ByDepthZone.Map.png'), dpi = 300, width = 40, height = 20, units = "cm") 


############################################################################################################################################
############################################################################################################################################
###Plot benthic and pelagic fish
#############
##Overall catch in 2022
breaks = 1000**(1:10)

ggplot(subset(csmi.fish.lakewide,  YEAR == 2022 & TFish > 0)) +
  aes(x=reorder(COMMON_NAME, desc(TFish)), y = TFish, fill = GearHabitat) + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
  geom_hline(yintercept=10000000, size = 2) + 
#  geom_segment(y = 10000000, x = 8.6, yend = 10000001, xend = 18, size = 2) +
  annotate("text", label = "Other\nfish", x = 13, y = 160000000, family = "serif", size = 14) +
  geom_vline(xintercept=8.6, size = 2) + 
  coord_flip() +
  scale_y_log10(breaks = breaks, labels = comma(breaks, accuracydigits = 1), name = 'Estimated Lakewide Fish') +
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = 'top',
        legend.direction = 'horizontal', 
        legend.title = element_blank(),
        axis.title.y = element_blank()) +
  labs(title='Lake Superior 2022 CSMI Fish Survey')

ggsave(here('Plots and Tables/CSMI','CSMI.2022.TotalAbundanceByGear.png'), dpi = 300, width = 40, height = 30, units = "cm") 


#######################################################################################################################################
##Total biomass and abundance by depth
ggplot(subset(csmi.annual.location.sum, 
              Gear == 'Bottom trawl' |
                Gear == 'Acoustics'   ), 
       aes(x=Mid.Depth, y=KGHA, fill = Assemblage))+
  geom_point(shape = 21, size = 5) +
#  geom_bar(stat="identity", position = "stack") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(4)) +
  plot_theme+
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.8,0.58),
        legend.title = element_blank()) +
  labs(x='Bathymetric depth (m)', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass',
       subtitle='USGS CSMI assessments') +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Biomass.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm") 

ggplot(subset(csmi.annual.location.sum, 
              Gear == 'Bottom trawl' |
                Gear == 'Acoustics'   ), 
       aes(x=Mid.Depth, y=NOHA, fill = Assemblage))+
  geom_point(shape = 21, size = 5) +
  #  geom_bar(stat="identity", position = "stack") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(4)) +
  plot_theme+
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.8,0.58),
        legend.title = element_blank()) +
  labs(x='Bathymetric depth (m)', y='Abundance (fish per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Abundance',
       subtitle='USGS CSMI assessments') +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Abundance.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm") 



ggplot(subset(csmi.annual.location.sum, Gear == 'Bottom trawl'), aes(x=Mid.Depth, y=NOHA))+
  geom_point(stat='identity', fill='plum3', color='plum3', size = 5)+
  geom_segment(aes(x=Mid.Depth, xend=Mid.Depth, y=0, yend=NOHA), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(4)) +
  plot_theme+
  labs(x='Bathymetric depth (m)', y='Abundance (fish per ha)',
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.Abundance.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Total biomass by depth
ggplot(subset(csmi.annual.location.sum, Gear == 'Bottom trawl'), aes(x=Mid.Depth, y=KGHA))+
  geom_point(stat='identity', fill='plum3', color='plum3', size = 5)+
  geom_segment(aes(x=Mid.Depth, xend=Mid.Depth, y=0, yend=KGHA), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(3)) +
  plot_theme+
  labs(x='Bathymetric depth (m)', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.Biomass.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm") 


ggplot(subset(csmi.annual.location.sum, Gear == 'Acoustics'), aes(x=Mid.Depth, y=NOHA))+
  geom_point(stat='identity', fill='plum3', color='plum3', size = 5)+
  geom_segment(aes(x=Mid.Depth, xend=Mid.Depth, y=0, yend=NOHA), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(4)) +
  plot_theme+
  labs(x='Bathymetric depth (m)', y='Abundance (fish per ha)',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.Abundance.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Total biomass by depth
ggplot(subset(csmi.annual.location.sum, Gear == 'Acoustics'), aes(x=Mid.Depth, y=KGHA))+
  geom_point(stat='identity', fill='plum3', color='plum3', size = 5)+
  geom_segment(aes(x=Mid.Depth, xend=Mid.Depth, y=0, yend=KGHA), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(3)) +
  plot_theme+
  labs(x='Bathymetric depth (m)', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.Biomass.Depth.png'), dpi = 300, width = 40, height = 20, units = "cm") 


########################################################################################################################################
##Map outline
ls_poly <- readOGR(dsn =here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


####Biomass map
ggplot(subset(csmi.annual.location.sum, Gear = 'Bottom trawl'), 
       aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.annual.location.sum, Gear = 'Bottom trawl'), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, 
                         size = KGHA, color = KGHA), stroke=1.5) +
  map_theme +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_size_continuous(name='Biomass\n(kg per ha)') +
  guides(color= guide_legend(), size=guide_legend()) +
  labs(caption=ann_data_access,
       title='Lake Superior Benthic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') +
  theme(legend.position=c(0.1,0.55)) +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.Biomass.Map.png'), dpi = 300, width = 40, height = 40, units = "cm") 


ggplot(subset(csmi.annual.location.sum, Gear = 'Acoustics'), 
       aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.annual.location.sum, Gear = 'Acoustics'), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, 
                         size = KGHA, color = KGHA), stroke=1.5) +
  map_theme +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_size_continuous(name='Biomass\n(kg per ha)') +
  guides(color= guide_legend(), size=guide_legend()) +
  labs(caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI acoustic assessments') +
  theme(legend.position=c(0.1,0.55)) +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.Biomass.Map.png'), dpi = 300, width = 40, height = 40, units = "cm") 



###NOHA maps
ggplot(subset(csmi.annual.location.sum, Gear = 'Bottom trawl'), 
       aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.annual.location.sum, Gear = 'Bottom trawl'), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, 
                         size = NOHA, color = NOHA), stroke=1.5) +
  map_theme +
  scale_color_gradient(low='deepskyblue2', high='red', name='Abundance\n(fish per ha)') +
  scale_size_continuous(name='Abundance\n(fish per ha)') +
  guides(color= guide_legend(), size=guide_legend()) +
  labs(caption=ann_data_access,
       title='Lake Superior Benthic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') +
  theme(legend.position=c(0.1,0.55)) +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.Abundance.Map.png'), dpi = 300, width = 40, height = 40, units = "cm") 



ggplot(subset(csmi.annual.location.sum, Gear = 'Acoustics'), 
       aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.annual.location.sum, Gear = 'Acoustics'), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, 
                         size = NOHA, color = NOHA), stroke=1.5) +
  map_theme +
  scale_color_gradient(low='deepskyblue2', high='red', name='Abundance\n(fish per ha)') +
  scale_size_continuous(name='Abundance\n(fish per ha)') +
  guides(color= guide_legend(), size=guide_legend()) +
  labs(caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI acoustic assessments') +
  theme(legend.position=c(0.1,0.55)) +
  facet_wrap(~YEAR, nrow=3)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.Abundance.Map.png'), dpi = 300, width = 40, height = 40, units = "cm") 

##############################################################################################################################
##Benthic species mean biomass by YEAR, bottom trawls
ggplot(subset(csmi.annual.species.sum, 
              Assemblage == 'Benthic fish' & Keep.fish == 'Y')) + 
  aes(x=as.factor(YEAR), y = KGHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean biomass (kg per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.MeanBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Benthic species mean abundance by YEAR, bottom trawls
ggplot(subset(csmi.annual.species.sum, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = NOHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean abundance (fish per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5, 0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.MeanAbundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Pelagic species mean biomass by YEAR, acoustics
ggplot(subset(csmi.annual.species.sum, 
              Assemblage == 'Pelagic fish' & Keep.fish == 'Y')) + 
  aes(x=as.factor(YEAR), y = KGHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean biomass (kg per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI acoustic trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.MeanBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Pelagic species mean abundance by YEAR, acoustics
ggplot(subset(csmi.annual.species.sum, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = NOHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean abundance (fish per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5, 0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI acoustic assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.MeanAbundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###############################################################################################################################
##Benthic species biomass by YEAR and Depth Zone
csmi.annual.species.dz.sum$Depth.Zone <- 
  factor(csmi.annual.species.dz.sum$Depth.Zone, levels=c("<30 m", "100-200 m", "30-100 m", ">200 m"))

ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = KGHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean biomass (kg per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.DepthZone.MeanBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##Benthic species abundance by YEAR and Depth Zone
csmi.annual.species.dz.sum$Depth.Zone <- 
  factor(csmi.annual.species.dz.sum$Depth.Zone, levels=c("<30 m", "100-200 m", "30-100 m", ">200 m"))

ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = NOHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Abundance (fish per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.DepthZone.MeanAbundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Pelagic species biomass by YEAR and Depth Zone
csmi.annual.species.dz.sum$Depth.Zone <- 
  factor(csmi.annual.species.dz.sum$Depth.Zone, levels=c("<30 m", "100-200 m", "30-100 m", ">200 m"))

ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = KGHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean biomass (kg per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.4,0.85),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI acoustics assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)
#facet_wrap(.~Depth.Zone, nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.MeanBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##Pelagic species abundance by YEAR and Depth Zone
csmi.annual.species.dz.sum$Depth.Zone <- 
  factor(csmi.annual.species.dz.sum$Depth.Zone, levels=c("<30 m", "100-200 m", "30-100 m", ">200 m"))

ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = NOHA.mean, fill = COMMON_NAME2) + 
  geom_bar(stat="identity", position = "stack") +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Abundance (fish per ha)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.65,0.85),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI acoustic assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.MeanAbundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###############################################################################################################################
###Lakewide abundance and biomass by best gear
caption2 <-'Data: U.S. Geological Survey, doi.org/10.5066/P9XVOLR1\nOther includes fish with <10 million estimated individuals'

csmi.fish.lakewide.dz$Depth.Zone <- 
  factor(csmi.fish.lakewide.dz$Depth.Zone, levels=c("<30 m", "100-200 m", "30-100 m", ">200 m"))


##Benthic and Pelagic Total Abundance
ggplot(subset(csmi.fish.lakewide, 
              Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TFish, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  geom_hline(yintercept = 0) + 
#  scale_y_continuous(expand=c(0,0), labels = label_number(scale = 1e-9, accuracy = 1), name = 'Fish, billions')+
  scale_y_continuous(labels = label_number(scale = 1e-9, accuracy = 1), name = 'Fish, billions')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.75),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=caption2,
       title='Lake Superior Fish Abundance',
       subtitle='USGS CSMI bottom trawl and hydroacoustic - mid-water trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.TotalAbundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##Benthic and Pelagic Total Biomass
ggplot(subset(csmi.fish.lakewide, 
              Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  geom_hline(yintercept = 0) + 
  scale_y_continuous(labels = label_number(scale = 1e-3), name = 'Biomass, thousand metric tons')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.75),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=caption2,
       title='Lake Superior Fish Biomass',
       subtitle='USGS CSMI bottom trawl and hydroacoustic - mid-water trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.TotalBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Lakewide Benthic Total abundance 
ggplot(subset(csmi.fish.lakewide, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TFish, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(labels = label_number(scale = 1e-9, accuracy = 1), name = 'Fish, billion')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.TotalFish.png'), dpi = 300, width = 40, height = 20, units = "cm") 


###Lakewide benthic total biomass
ggplot(subset(csmi.fish.lakewide, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(breaks = pretty_breaks(), name = 'Biomass (metric tons)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.TotalBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 


###Lakewide pelagic Total abundance 
ggplot(subset(csmi.fish.lakewide, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TFish, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(labels = label_number(scale = 1e-9, accuracy = 1), name = 'Fish, billions')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI acoustic assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.TotalFish.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Lakewide pelagic total biomass
ggplot(subset(csmi.fish.lakewide, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(breaks = pretty_breaks(), name = 'Biomass (metric tons)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI acoustic assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.TotalBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 


###Lakewide benthic abundance By depth zone
ggplot(subset(csmi.fish.lakewide.dz, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TFish, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1), name = 'Fish, thousands')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Abundance',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)


ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.DepthZone.TotalFish.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Lakewide benthic biomass by depth zone
csmi.annual.species.dz.sum$Depth.Zone <- 
  factor(csmi.annual.species.dz.sum$Depth.Zone, levels=c("<30 m", "100-200 m", "30-100 m", ">200 m"))

ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Benthic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(breaks = pretty_breaks(), name = 'Biomass (metric tons)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.85),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Benthic Fish Biomass',
       subtitle='USGS CSMI bottom trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Benthic.DepthZone.TotalBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 



###Pelagic Lakewide abundance by depth zone
ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TFish, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 1), name = 'Fish, millions')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI acoustic assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.TotalFish.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Pelagic Lakewide biomass by depth zone
ggplot(subset(csmi.annual.species.dz.sum, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(breaks = pretty_breaks(), name = 'Biomass (metric tons)')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.15,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI acoustic assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.TotalBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 



###############################################################################################################################
###Lakewide abundance in millions of fish
ggplot(subset(csmi.fish.lakewide.dz, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TFish, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(labels = label_number(scale = 1e-9, accuracy = 1), name = 'Fish, billions')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.65,0.85),
        #    legend.position = "bottom",  
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Abundance',
       subtitle='USGS CSMI hydroacoustic and mid-water trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.TotalFish.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Lakewide biomass
ggplot(subset(csmi.fish.lakewide.dz, 
              Assemblage == 'Pelagic fish' & Keep.fish == "Y")) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = COMMON_NAME2) + 
  geom_bar(stat='identity', position='stack')+
  scale_y_continuous(labels = label_number(scale = 1e-3), name = 'Biomass, thousand metric tons')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.4,0.85),
        #legend.position = "bottom",  
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Pelagic Fish Biomass',
       subtitle='USGS CSMI hydroacoustic and mid-water trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.TotalBiomass.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##############################################################################################################

##############################################################################################################################
####################################################################################################################
###Compare Kiyi and siscowet bottom trawl v. acoustics 
csmi.kiyi.siscowet <- csmi.annual.species.dz.sum %>%
  filter(COMMON_NAME == 'Kiyi' | COMMON_NAME == 'siscowet Lake Trout') %>%
  group_by(YEAR, Depth.Zone, Gear, COMMON_NAME) %>%
  summarize(TFish = sum(TFish),
            TBiomass = sum(TBiomass)) %>%
  ungroup() %>%
  left_join(csmi.fish.rules) %>%
  subset(Gear != 'Mid-water trawl')



ggplot(subset(csmi.kiyi.siscowet, COMMON_NAME == 'Kiyi')) + 
  aes(x=as.factor(YEAR), y = TFish, fill = Assemblage) + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
#  scale_y_continuous(labels = label_number(scale = 1e-3, accuracy = 1), name = 'Fish, thousands')+
  scale_y_continuous(name = 'Fish')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.1,0.89),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Kiyi Abundance',
       subtitle='USGS CSMI bottom trawl versus hydroacoustic and mid-water trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.BenthicPelagic.DepthZone.Abundance.Kiyi.png'), dpi = 300, width = 40, height = 20, units = "cm") 

ggplot(subset(csmi.kiyi.siscowet, COMMON_NAME == 'Kiyi')) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = Assemblage) + 
  geom_bar(stat="identity", width=.5, position = "dodge")+
  scale_y_continuous(name = 'Biomass, thousand metric tons')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.1,0.89),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior Kiyi Biomass',
       subtitle='USGS CSMI bottom trawl versus hydroacoustic and mid-water trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.BenthicPelagic.DepthZone.Biomass.Kiyi.png'), dpi = 300, width = 40, height = 20, units = "cm") 

####################################################################################################################

###Compare siscowet bottom trawl v. acoustics 
ggplot(subset(csmi.kiyi.siscowet, COMMON_NAME == 'siscowet Lake Trout')) + 
  aes(x=as.factor(YEAR), y = TFish, fill = Assemblage) + 
  geom_bar(stat="identity", width=.5, position = "dodge") +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 1), name = 'Fish, millions')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.3,0.89),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior siscowet Lake Trout Abundance',
       subtitle='USGS CSMI bottom trawl versus hydroacoustic and mid-water trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.BenthicPelagic.DepthZone.Abundance.Siscowet.png'), dpi = 300, width = 40, height = 20, units = "cm") 

ggplot(subset(csmi.kiyi.siscowet, COMMON_NAME == 'siscowet Lake Trout')) + 
  aes(x=as.factor(YEAR), y = TBiomass, fill = Assemblage) + 
  geom_bar(stat="identity", width=.5, position = "dodge")+
  scale_y_continuous(name = 'Biomass, thousand metric tons')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.3,0.89),
        legend.title = element_blank()) +
  labs(x = 'Year',
       caption=ann_data_access,
       title='Lake Superior siscowet Lake Trout Biomass',
       subtitle='USGS CSMI bottom trawl versus hydroacoustic and mid-water trawl assessments') +
  facet_wrap(.~Depth.Zone, scales = "free_y", nrow = 2)

ggsave(here('Plots and Tables/CSMI','CSMI.BenthicPelagic.DepthZone.Biomass.Siscowet.png'), dpi = 300, width = 40, height = 20, units = "cm") 

####################################################################################################################


#####################################################################################################################################
###YOY Coregonine abundance

Depth.Zone_order <- c("<30 m", "30-100 m", "100-200 m", ">200 m")

ggplot(subset(csmi.annual.species.dz.sum, 
                Assemblage == 'Pelagic fish' & COMMON_NAME == 'ciscoe YOY')) +
  aes(x=Depth.Zone,  y = TFish) + 
  geom_col(fill = 'olivedrab')  +  
  scale_x_discrete(limits = Depth.Zone_order) +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 1), name = 'Fish, millions')+
  plot_theme +
  scale_fill_brewer(palette = 'Pastel1') +
  theme(legend.position = c(0.4,0.85),
        legend.title = element_blank()) +
  labs(x = 'Depth zone',
       caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI hydroacoustic and mid-water trawl assessments') 

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.DepthZone.CiscoeYOYAbundance.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Age-0 Ciscoe map
ggplot(subset(csmi.acoustics.fish, COMMON_NAME == 'ciscoe YOY')) +
  scale_y_continuous(name='Latitude', breaks = pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks = pretty_breaks()) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(csmi.acoustics.fish, COMMON_NAME == 'ciscoe YOY'), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size = NOHA, color = NOHA), stroke=1.5) +
  theme_bw() +
  map_theme +
  scale_color_gradient(low='deepskyblue2', high='red', name='Abundance\n(fish per ha)') +
  scale_size_continuous(name='Abundance\n(fish per ha)', range = c(1,10)) +
  guides(color= guide_legend(), size=guide_legend()) +
  labs(caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI hydroacoustic and mid-water trawl assessments') +
  theme(legend.position=c(0.15,0.8)) 

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.CiscoeYOYAbundance.Map.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Age-0 ciscoes by date

ggplot(subset(csmi.acoustics.fish, COMMON_NAME == 'ciscoe YOY' & YEAR == 2022)) +
  geom_point(aes(x= as.Date(Date), y = NOHA), color = 'lightskyblue', size = 6, stroke=1.5) + 
  geom_smooth(aes(x= as.Date(Date), y = NOHA), se = FALSE) +
  scale_x_date(date_labels = "%b %d", name = "Date", breaks = pretty_breaks(6)) +
  scale_y_continuous(name='Abundance (fish per ha)', breaks = pretty_breaks()) +
  theme_bw() +
  plot_theme +
  labs(caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI hydroacoustic and mid-water trawl assessments')  

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.CiscoeYOYAbundance.Date.png'), dpi = 300, width = 40, height = 20, units = "cm") 

##Age-0 ciscoes by distance from shore

ggplot(subset(csmi.acoustics.fish, COMMON_NAME == 'ciscoe YOY' & YEAR == 2022)) +
  geom_point(aes(x= ShoreDistance/1000, y = NOHA, fill = as.Date(Date)), shape = 21, size = 6, stroke=1.5) + 
  geom_smooth(aes(x= ShoreDistance/1000, y = NOHA), se = FALSE) +
  scale_x_continuous(name = "Distance from shore (km)", breaks = pretty_breaks()) +
  scale_y_continuous(name='Abundance (fish per ha)', breaks = pretty_breaks()) +
  scale_fill_date(date_breaks = "14 days", 
                  date_labels = "%b %d",
                  low = 'red', high = 'lightskyblue',
                  name = 'Date') +
  theme_bw() +
  map_theme +
  theme(legend.position=c(0.15,0.8)) + 
  labs(caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI hydroacoustic and mid-water trawl assessments')  

ggsave(here('Plots and Tables/CSMI','CSMI.Pelagic.CiscoeYOYAbundance.ShoreDistance.png'), dpi = 300, width = 40, height = 20, units = "cm") 


###################################################

####################################################################################################################NEARSHORE DATA####
##Neuston data
##Neuston data 2014-2022

codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

neuston.data<-read.csv(here('Data','2014-2022_Neuston_data.csv'))
neuston.data$SPECIES<-as.factor(neuston.data$SPECIES) 

##Get shore distance
ShoreDistance<-read_xlsx(here('Data','ShoreDistance.xlsx'))


##change date format into usable form
neuston.data$OP_DATE<-as.character(neuston.data$OP_DATE)
neuston.data$OP_DATE<-parse_date(neuston.data$OP_DATE, format='%d-%b-%y')

##Add date fields and shore distance
neuston.data <- neuston.data %>%
  mutate(Day = day(OP_DATE),
         Week = week(OP_DATE), 
         Month = month(OP_DATE),
         Month.short = month.abb[Month],
         Month.long = month.name[Month],
         Year = year(OP_DATE), 
         Date = dmy(paste(Day, Month, Year, sep = "-"))) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, Day, sep=" ", remove = FALSE) %>%
  left_join(sci.names) %>%
  left_join(ShoreDistance)


neuston.data[is.na(neuston.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

neuston.data[is.na(neuston.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
neuston.data[is.na(neuston.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- neuston.data[is.na(neuston.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

neuston.data$Mid.Lat.DD<-(neuston.data$BEG_LATITUDE_DD+neuston.data$END_LATITUDE_DD)/2
neuston.data$Mid.Long.DD<-(neuston.data$BEG_LONGITUDE_DD+neuston.data$END_LONGITUDE_DD)/2
neuston.data$Mid.Depth <- rowMeans(neuston.data[,c("BEG_DEPTH", "END_DEPTH")], na.rm = TRUE)


neuston.effort <- neuston.data %>%
  select(TARGET, Date, Date1, Day, Week, Month, Month.short, Month.long, Year, LOCATION, Mid.Lat.DD, Mid.Long.DD, Mid.Depth, ShoreDistance) %>%
  distinct(Date, LOCATION, .keep_all = TRUE) 


##Composite the two nets into one sample
##Subset data frame to SPECIES = 217, UNIDENTIFIED Coregonus or SPECIES = 0 for no fish tows
##Composite the two nets into one sample
##Calculate fish density - number per hectare 
## Sampling area in square feet = DISTANCE (in miles) * 5280 (feet per mile) * 6.56168 (Net Width in feet, two 1 m wide nets)
## Sampling area in hectares = area in square ft / 107639
## Divide the fish count by the sampling area in hectares = number of fish per hectare

neuston.1 <- neuston.data %>% 
  group_by(Date, LOCATION, SPECIES) %>%
  summarise(Fish = sum(N)) %>%
  ungroup() %>%
  left_join(neuston.data) %>%
  distinct(Date, LOCATION, SPECIES, .keep_all = TRUE) %>%
  mutate(fish_ha = Fish/(((DISTANCE*5280)*6.56168)/107639.1)) %>%
  select(OP_ID, Date, Date1, Week, Day, Month, Month.short, Month.long, Year, LOCATION, Mid.Lat.DD, Mid.Long.DD, 
         ShoreDistance, SPECIES, COMMON_NAME, Fish, fish_ha) %>%
  replace(is.na(.), 0)


##Pull out ciscoes and zero catch samples to make table with ciscoe (217) densities by date and location
neuston.ciscoe.densities <- neuston.1 %>%
  subset(SPECIES == 217) %>%
  select(Date, LOCATION, SPECIES, Fish, fish_ha) %>%
  right_join(neuston.effort) %>%
  mutate(SPECIES2 = 217,
         COMMON_NAME = 'ciscoe larvae') %>% 
  select(-SPECIES) %>%
  replace(is.na(.), 0) 


##########################################################################################################
##Larval densities by year all data including CSMI shallow sites

ggplot(subset(neuston.ciscoe.densities, Month <8 & Year != 2021)) + 
  aes(x=Year, y = fish_ha) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'olivedrab', alpha = .8) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI abundance (fish/ha)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') 

ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.95CI.png'), dpi = 300, width = 40, height = 20, units = "cm") 
ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.95CI.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##By month with 95% CI
ggplot(subset(neuston.ciscoe.densities, Month <8 & Year != 2021)) + 
  aes(x=Year, y = fish_ha) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'olivedrab', alpha = .8) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI abundance (fish/ha)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=3, scales="free_y")


ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.ByMonth.95CI.png'), dpi = 300, width = 40, height = 40, units = "cm") 
ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.ByMonth.95CI.png'), dpi = 300, width = 40, height = 40, units = "cm") 



###########################################################################################################################
####With Standard Errors all data including CSMI shallow sites
neuston.ciscoe.densities.sum <- neuston.ciscoe.densities %>%
  subset(Month <8) %>%
  subset(Year !=2021) %>%
  group_by(Year) %>%
  summarize(n = n(), 
            mean.fish_ha = mean(fish_ha),
            sd.fish_ha = sd(fish_ha), 
            se.fish_ha = sd.fish_ha/sqrt(n)) %>%
  ungroup() 


##with SE by Year
ggplot(neuston.ciscoe.densities.sum) + 
  aes(x=Year, y = mean.fish_ha) + 
  geom_bar(stat = "identity", fill = 'olivedrab', alpha = .8) +
  geom_errorbar(data=neuston.ciscoe.densities.sum, aes(x=Year, ymin=mean.fish_ha-se.fish_ha, ymax=mean.fish_ha+se.fish_ha), 
                width = 0.4, size = 1.5) +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean abundance & SE (fish per ha)')+
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') 

ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.SE.png'), dpi = 300, width = 40, height = 20, units = "cm") 
ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.SE.png'), dpi = 300, width = 40, height = 20, units = "cm") 


####Larval densities by month with SE
neuston.ciscoe.densities.month.sum <- neuston.ciscoe.densities %>%
  subset(Month <8) %>%
  subset(Year !=2021) %>%
  group_by(Year, Month) %>%
  summarize(n = n(), 
            mean.fish_ha = mean(fish_ha),
            sd.fish_ha = sd(fish_ha), 
            se.fish_ha = sd.fish_ha/sqrt(n)) %>%
  ungroup() %>%
  mutate(Month.long = month.name[Month])


neuston.ciscoe.densities.month.sum <- neuston.ciscoe.densities.month.sum %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))


ggplot(neuston.ciscoe.densities.month.sum) + 
  aes(x=Year, y = mean.fish_ha) + 
  geom_bar(stat = "identity", fill = 'olivedrab', alpha = .8) +
  geom_errorbar(data=neuston.ciscoe.densities.month.sum, aes(x=Year, ymin=mean.fish_ha-se.fish_ha, ymax=mean.fish_ha+se.fish_ha), 
                width = 0.4, size = 1.5) +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean abundance & SE (fish per ha)')+
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=3, scales="free_y")


ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.ByMonth.SE.png'), dpi = 300, width = 40, height = 40, units = "cm") 
ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.ByMonth.SE.png'), dpi = 300, width = 40, height = 40, units = "cm") 


######################################################################################################################
##Larval densities by year for nearshore and offshore - shallow CSMI sites not included from 2016 and 2022
neuston.ciscoe.densities.noCSMI <- neuston.ciscoe.densities %>%
  subset(TARGET==2 | TARGET==118 | TARGET==117 & Year>2013 & Mid.Depth >80) 


ggplot(subset(neuston.ciscoe.densities.noCSMI, Month <8 & Year !=2021)) + 
  aes(x=Year, y = fish_ha) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'olivedrab', alpha = .8) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI abundance (fish/ha)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') 

ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.95CI.noCSMI.png'), dpi = 300, width = 40, height = 20, units = "cm") 
ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.95CI.noCSMI.png'), dpi = 300, width = 40, height = 20, units = "cm") 


neuston.ciscoe.densities.noCSMI <- neuston.ciscoe.densities.noCSMI %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))


ggplot(subset(neuston.ciscoe.densities.noCSMI, Month <8 & Year !=2021)) + 
  aes(x=Year, y = fish_ha) + 
  stat_summary(fun = "mean", geom = "bar", fill = 'olivedrab', alpha = .8) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = 0.5,
               size = 1.5) +
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year') +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean & 95% CI abundance (fish/ha)')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=3, scales="free_y")

ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.ByMonth.noCSMI.95CI.png'), dpi = 300, width = 40, height = 40, units = "cm") 
ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.ByMonth.noCSMI.95CI.png'), dpi = 300, width = 40, height = 40, units = "cm") 


#################################################################################
##############
##With SE instead of 95% CI
neuston.ciscoe.densities.sum.noCSMI <- neuston.ciscoe.densities.noCSMI %>%
  subset(Month <8) %>%
  subset(Year !=2021) %>%
  group_by(Year) %>%
  summarize(n = n(), 
            mean.fish_ha = mean(fish_ha),
            sd.fish_ha = sd(fish_ha), 
            se.fish_ha = sd.fish_ha/sqrt(n)) %>%
  ungroup() 


##with SE by Year
ggplot(neuston.ciscoe.densities.sum.noCSMI) + 
  aes(x=Year, y = mean.fish_ha) + 
  geom_bar(stat = "identity", fill = 'olivedrab', alpha = .8) +
  geom_errorbar(data=neuston.ciscoe.densities.sum.noCSMI, aes(x=Year, ymin=mean.fish_ha-se.fish_ha, ymax=mean.fish_ha+se.fish_ha), 
                width = 0.4, size = 1.5) +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean abundance & SE (fish per ha)')+
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') 

ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.noCSMI.SE.png'), dpi = 300, width = 40, height = 20, units = "cm") 
ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.noCSMI.SE.png'), dpi = 300, width = 40, height = 20, units = "cm") 




####Larval densities by month with SE
neuston.ciscoe.densities.month.sum.noCSMI <- neuston.ciscoe.densities.noCSMI %>%
  subset(Month <8) %>%
  subset(Year !=2021) %>%
  group_by(Year, Month) %>%
  summarize(n = n(), 
            mean.fish_ha = mean(fish_ha),
            sd.fish_ha = sd(fish_ha), 
            se.fish_ha = sd.fish_ha/sqrt(n)) %>%
  ungroup() %>%
  mutate(Month.long = month.name[Month])


neuston.ciscoe.densities.month.sum.noCSMI <- neuston.ciscoe.densities.month.sum.noCSMI %>%
  mutate(across(Month.long, factor, levels = c("May", "June", "July", "August", "September")))


ggplot(neuston.ciscoe.densities.month.sum.noCSMI) + 
  aes(x=Year, y = mean.fish_ha) + 
  geom_bar(stat = "identity", fill = 'olivedrab', alpha = .8) +
  geom_errorbar(data=neuston.ciscoe.densities.month.sum.noCSMI, aes(x=Year, ymin=mean.fish_ha-se.fish_ha, ymax=mean.fish_ha+se.fish_ha), 
                width = 0.4, size = 1.5) +
  scale_y_continuous(breaks = pretty_breaks(), name = 'Mean abundance & SE (fish per ha)')+
  scale_x_continuous(breaks = pretty_breaks(), name = 'Year')+
  plot_theme +
  theme(legend.position = c(0.5,0.8),
        legend.title = element_blank()) +
  labs(x = 'Year', 
       caption=ann_data_access,
       title='Lake Superior Larval Ciscoe Abundance',
       subtitle='USGS Surface Trawl Surveys, May-July') +
  facet_wrap(~Month.long, nrow=3, scales="free_y")


ggsave(here('Plots and Tables/CSMI','Annual.Larval.Densities.ByMonth.noCSMI.SE.png'), dpi = 300, width = 40, height = 40, units = "cm") 
ggsave(here('Plots and Tables/RVCAT','Annual.Larval.Densities.ByMonth.noCSMI.SE.png'), dpi = 300, width = 40, height = 40, units = "cm") 





##########################################################################################################################################
###Annual summary table
neuston.ciscoe.annual <- neuston.ciscoe.densities.noCSMI %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  subset(Month <8) %>%
  group_by(Year) %>%
  summarise(n = n(), 
            FishSum = sum(Fish), 
            FishMean = mean(fish_ha)) %>%
  ungroup()

###Annual summary table
neuston.ciscoe.month <- neuston.ciscoe.densities.noCSMI %>%
  distinct(Date, LOCATION, .keep_all = TRUE) %>%
  subset(Month <8) %>%
  group_by(Year, Month) %>%
  summarise(n = n(), 
            FishSum = sum(Fish), 
            FishMean = mean(fish_ha)) %>%
  ungroup()


#########################################################################################################################

##Combine Age-0 acoustics and larval neuston data
csmi.2022.larval <- neuston.ciscoe.densities %>%
  subset(Year == 2022) %>%
  select(Date, COMMON_NAME, fish_ha) %>%
  renameCol('fish_ha', 'NOHA')

csmi.2022.age0 <- csmi.all.fish %>%
  subset(COMMON_NAME == 'ciscoe YOY' & YEAR == 2022) %>%
  select(Date, COMMON_NAME, NOHA) %>%
  bind_rows(csmi.2022.larval) %>%
  mutate(Gear = case_when(
    COMMON_NAME == 'ciscoe larvae' ~ 'Surface trawl',
    COMMON_NAME == 'ciscoe YOY' ~ 'Mid-water trawl & acoustics'))


csmi.2022.age0.dailysum <- csmi.2022.age0 %>%
  group_by(Date, Gear) %>%
  summarize(n = n(), 
            mean.NOHA = mean(NOHA), 
            sd.NOHA = sd(NOHA), 
            se.NOHA = sd.NOHA/sqrt(n)) %>%
  ungroup() 


ggplot(csmi.2022.age0) +
  geom_point(aes(x= as.Date(Date), y = NOHA, colour = Gear), size = 6, stroke=1.5) + 
  geom_smooth(aes(x= as.Date(Date), y = NOHA, colour = Gear), size = 2, se = FALSE) +
  scale_x_date(date_labels = "%b %d", name = "Date", breaks = pretty_breaks(8)) +
  scale_y_continuous(name='Abundance (fish per ha)', breaks = pretty_breaks()) +
  scale_colour_brewer(palette = 'Pastel1') +
  theme_bw() +
  map_theme +
  theme(legend.position=c(0.25,0.9),
        legend.title = element_blank()) + 
  labs(caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI surface trawl and hydroacoustic and mid-water trawl assessments')  

ggsave(here('Plots and Tables/CSMI','CSMI.SurfaceTrawl.Acoustic.CiscoeYOYAbundance.ByDate.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Daily average
ggplot(csmi.2022.age0.dailysum) +
  geom_point(aes(x= as.Date(Date), y = mean.NOHA, colour = Gear), size = 6, stroke=1.5) + 
  geom_smooth(aes(x= as.Date(Date), y = mean.NOHA, colour = Gear), se = FALSE, size = 2) +
  scale_x_date(date_labels = "%b %d", name = "Date", breaks = pretty_breaks(6)) +
  scale_y_continuous(name='Mean daily abundance (fish per ha)', breaks = pretty_breaks()) +
  scale_colour_brewer(palette = 'Pastel1') +
  theme_bw() +
  map_theme +
  theme(legend.position=c(0.75,0.9),
        legend.title = element_blank()) + 
  labs(caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI surface trawl and hydroacoustic and mid-water trawl assessments')  

ggsave(here('Plots and Tables/CSMI','CSMI.SurfaceTrawl.Acoustic.Ciscoe.YOYAbundance.MeanDaily.png'), dpi = 300, width = 40, height = 20, units = "cm") 

ggplot(csmi.2022.age0.dailysum) +
  geom_point(aes(x= as.Date(Date), y = mean.NOHA, colour = Gear), size = 6, stroke=1.5) + 
  geom_smooth(aes(x= as.Date(Date), y = mean.NOHA, colour = Gear), se = FALSE, size = 2) +
  geom_errorbar(data=csmi.2022.age0.dailysum, aes(x=as.Date(Date), ymin=mean.NOHA-se.NOHA, ymax=mean.NOHA+se.NOHA, colour = Gear),
                size = 1.5) +
  scale_x_date(date_labels = "%b %d", name = "Date", breaks = pretty_breaks(6)) +
  scale_y_continuous(name='Mean daily abundance (fish per ha)', breaks = pretty_breaks()) +
  scale_colour_brewer(palette = 'Pastel1') +
  theme_bw() +
  map_theme +
  theme(legend.position=c(0.75,0.9),
        legend.title = element_blank()) + 
  labs(caption=ann_data_access,
       title='Lake Superior YOY Ciscoe Abundance',
       subtitle='USGS CSMI surface trawl and hydroacoustic and mid-water trawl assessments')  

ggsave(here('Plots and Tables/CSMI','CSMI.SurfaceTrawl.Acoustic.Ciscoe.YOYAbundance.Mean_SE_Daily.png'), dpi = 300, width = 40, height = 20, units = "cm") 



#################################################################################################################
##csmi fish. assemblage summary for export 
###############################################################################################################
##Combine trawls and acoustics to summarize by year and gear for large summary table of just about everything
sci.names<-select(codes.to.names, c(1:3,8,10))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

##Calculate total number of species caught that year
csmi.summary.all.fish <- csmi.all.fish.all %>%
  left_join(sci.names) %>%
  left_join(csmi.fish.rules) %>%
  subset(Keep.fish == 'Y')

csmi.summary.total.fish <- csmi.summary.all.fish %>% 
  group_by(YEAR, LOCATION, Gear) %>%
  summarise(Fishes = sum(NOHA > 0)) %>%
  ungroup() 

##Sum KGHA and NOHA for native and exotic species
csmi.summary.type<- csmi.summary.all.fish %>%
  select(YEAR, LOCATION, Gear, TYPE, NOHA, KGHA) %>%
  group_by(YEAR, LOCATION, Gear, TYPE) %>%
  summarise(TYPE.NOHA = sum(NOHA),
            TYPE.KGHA = sum(KGHA)) %>%
  pivot_wider(names_from = TYPE, values_from = c(TYPE.NOHA, TYPE.KGHA), values_fill = 0) %>%
  renameCol('TYPE.NOHA_native', 'NOHA.native') %>%
  renameCol('TYPE.NOHA_exotic', 'NOHA.exotic') %>%
  renameCol('TYPE.KGHA_native', 'KGHA.native') %>%
  renameCol('TYPE.KGHA_exotic', 'KGHA.exotic') %>%
  ungroup() 

##Sum KGHA and NOHA for prey and predator
csmi.summary.diet<- csmi.summary.all.fish  %>%
  select(YEAR, LOCATION, Gear, DIET, NOHA, KGHA) %>%
  group_by(YEAR, LOCATION, Gear, DIET) %>%
  summarise(DIET.NOHA = sum(NOHA),
            DIET.KGHA = sum(KGHA)) %>%
  pivot_wider(names_from = DIET, values_from = c(DIET.NOHA, DIET.KGHA), values_fill = 0) %>%
  renameCol('DIET.NOHA_prey', 'NOHA.prey') %>%
  renameCol('DIET.NOHA_predator', 'NOHA.predator') %>%
  renameCol('DIET.KGHA_prey', 'KGHA.prey') %>%
  renameCol('DIET.KGHA_predator', 'KGHA.predator') %>%
  ungroup()


##Sum KGHA for prey and predator by native and exotic
csmi.summary.type.diet<- csmi.summary.all.fish  %>%
  select(YEAR, LOCATION, Gear, TYPE, DIET, NOHA, KGHA) %>%
  mutate(type_diet = paste(TYPE, DIET, sep = ".")) %>%
  group_by(YEAR, LOCATION, Gear, type_diet) %>%
  summarise(TYPE.DIET.NOHA = sum(NOHA),
            TYPE.DIET.KGHA = sum(KGHA)) %>%
  pivot_wider(names_from = type_diet, values_from = c(TYPE.DIET.NOHA, TYPE.DIET.KGHA), values_fill = 0) %>%
  ungroup() %>%
  renameCol('TYPE.DIET.NOHA_native.prey', 'NOHA.native.prey') %>%
  renameCol('TYPE.DIET.NOHA_native.predator', 'NOHA.native.predator') %>%
  renameCol('TYPE.DIET.NOHA_exotic.prey', 'NOHA.exotic.prey') %>%
  renameCol('TYPE.DIET.NOHA_exotic.predator', 'NOHA.exotic.predator') %>%
  renameCol('TYPE.DIET.KGHA_native.prey', 'KGHA.native.prey') %>%
  renameCol('TYPE.DIET.KGHA_native.predator', 'KGHA.native.predator') %>%
  renameCol('TYPE.DIET.KGHA_exotic.prey', 'KGHA.exotic.prey') %>%
  renameCol('TYPE.DIET.KGHA_exotic.predator', 'KGHA.exotic.predator') 

   
##Sum KGHA and NOHA across all species to get total biomass and total number per ha
csmi.summary.all <- csmi.summary.all.fish  %>%
  select(YEAR, LOCATION, Gear,NUM,NOHA,KGHA) %>%
  group_by(YEAR, LOCATION, Gear) %>%
  summarise(KGHA = sum(KGHA), 
            NUM=sum(NUM), 
            NOHA = SUM(NOHA)) %>%
  ungroup() %>%
  left_join(csmi.all.effort) %>% 
  left_join(csmi.summary.type) %>%
  left_join(csmi.summary.diet) %>%
  left_join(csmi.summary.type.diet) %>%
  left_join(csmi.summary.total.fish)


csmi.summary.zerofish <- csmi.annual.location.sum %>%  
  group_by(YEAR, Gear) %>% 
  summarise(sites.zerofish=sum(NOHA == 0)) %>%
  ungroup() 

################################
na.rm=TRUE
csmi.summary <- csmi.summary.all %>% 
  group_by(YEAR, Gear) %>% 
  summarise(locations=n(), 
            min.Date=min(Date), max.Date=max(Date),
            min.year=min(YEAR), max.year=max(YEAR), 

            mean.kgha=mean(KGHA), median.kgha = median(KGHA), 
            min.kgha=min(KGHA), max.kgha = max(KGHA),
            sd.kgha = sd(KGHA), std.error.kgha = std.error(KGHA), skewness.kgha=skewness(KGHA),

            mean.noha=mean(NOHA), median.noha = median(NOHA), 
            min.kgha=min(NOHA), max.kgha = max(NOHA),
            sd.noha = sd(NOHA), std.error.noha = std.error(NOHA), skewness.noha=skewness(NOHA),
            
            mean.native.noha = mean(NOHA.native), median.native.noha = median(NOHA.native),
            min.native.noha = min(NOHA.native), max.native.noha = max(NOHA.native),
            mean.exotic.noha = mean(NOHA.exotic), median.exotic.noha = median(NOHA.exotic),
            min.exotic.noha = min(NOHA.exotic), max.exotic.noha = max(NOHA.exotic),
            
            mean.native.kgha = mean(KGHA.native), median.native.kgha = median(KGHA.native),
            min.native.kgha = min(KGHA.native), max.native.kgha = max(KGHA.native),
            mean.exotic.kgha = mean(KGHA.exotic), median.exotic.kgha = median(KGHA.exotic),
            min.exotic.kgha = min(KGHA.exotic), max.exotic.kgha = max(KGHA.exotic),
            
            mean.prey.noha = mean(NOHA.prey), median.prey.noha = median(NOHA.prey),
            min.prey.noha = min(NOHA.prey), max.prey.noha = max(NOHA.prey),
            mean.predator.noha = mean(NOHA.predator), median.predator.noha = median(NOHA.predator),
            min.predator.noha = min(NOHA.predator), max.predator.noha = max(NOHA.predator),
            
            mean.prey.kgha = mean(KGHA.prey), median.prey.kgha = median(KGHA.prey),
            min.prey.kgha = min(KGHA.prey), max.prey.kgha = max(KGHA.prey),
            mean.predator.kgha = mean(KGHA.predator), median.predator.kgha = median(KGHA.predator),
            min.predator.kgha = min(KGHA.predator), max.predator.kgha = max(KGHA.predator),
            
            mean.native.prey.noha=mean(NOHA.native.prey), mean.exotic.prey.noha=mean(NOHA.exotic.prey), 
            mean.native.predator.noha=mean(NOHA.native.predator), mean.exotic.predator.noha=mean(NOHA.exotic.predator), 
            mean.native.prey.kgha=mean(KGHA.native.prey), mean.exotic.prey.kgha=mean(KGHA.exotic.prey), 
            mean.native.predator.kgha=mean(KGHA.native.predator), mean.exotic.predator.kgha=mean(KGHA.exotic.predator), 
 
            sum.num=sum(NUM),
            fishes.mean = mean(Fishes), fishes.median = median(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) %>%
  ungroup() %>%
  left_join(csmi.summary.zerofish)
  


#########################################################################################################
##Export CSMI data tables to Excel
csmi.tables<-list('all.fish' = csmi.all.fish,
                  'csmi.total.annual.sum' = csmi.total.catch, 
                  'csmi.total.lakewide.annual.sum' = csmi.fish.lakewide.table, 
                  'csmi.fish.rules' = csmi.fish.rules,
                  'csmi.effort'= csmi.all.effort,
                  'csmi.effort.sum' = csmi.all.effort.sum, 
                  'csmi.fish.lakewide' = csmi.fish.lakewide, 
                  'csmi.fish.lakewide.depthzone' = csmi.fish.lakewide.dz, 
                  'csmi.annual.location.sum' = csmi.annual.location.sum, 
                  'csmi.annual.sum' = csmi.annual.sum,
                  'csmi.annual.depthzone.sum' = csmi.annual.dz.sum,
                  'CSMI.annual.species'= csmi.annual.species.sum,
                  'CSMI.annual.species.depthzone' = csmi.annual.species.dz.sum,
                  'csmi.fish.summary' = csmi.summary)

openxlsx::write.xlsx(csmi.tables, here('Plots and Tables/CSMI','CSMI.DataTables.xlsx'))


########
##Calculate the number of age-0 ciscoe caught in mid-water trawls in 2022
csmi.yoy.lengths <- read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  right_join(csmi.trawls) %>%
  select(OP_ID,YEAR, Gear,LOCATION,SPECIES,LENGTH,EXP_N) %>%
  subset(YEAR = 2022) %>%
  subset(SPECIES == 202 | SPECIES == 204 | SPECIES == 206 | SPECIES == 217) %>%
  subset(LENGTH <= 130) %>%
  subset(Gear == 'Mid-water trawl') %>%
  mutate(SPECIES2 = 217) %>%
  group_by(LOCATION, SPECIES2) %>%
  summarise(fishcnt = sum(EXP_N)) %>%
  ungroup()


csmi.yoy.sum <- csmi.yoy.lengths %>%
  group_by(SPECIES2) %>%
  summarise(locations=n(),
            fishes = sum(fishcnt)) %>%
  ungroup()

csmi.adult.lengths <- read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  right_join(csmi.trawls) %>%
  select(OP_ID,YEAR, Gear,LOCATION,SPECIES,LENGTH,EXP_N) %>%
  subset(YEAR == 2022) %>%
  subset(SPECIES == 202 | SPECIES == 204 | SPECIES == 206 | SPECIES == 217) %>%
  subset(LENGTH > 130) %>%
  subset(Gear == 'Mid-water trawl') %>%
  group_by(LOCATION, SPECIES) %>%
  summarise(fishcnt = sum(EXP_N)) %>%
  ungroup()


csmi.adult.sum <- csmi.adult.lengths %>%
  group_by(SPECIES) %>%
  summarise(locations=n(),
            fishes = sum(fishcnt)) %>%
  ungroup()

############################################################################################################


###########################################################################################################
##################################################################################################################
##################################################################################################################
###Miscellaneous small data requests

ns.sites <- all.data %>%
  subset(YEAR >=2010 & TARGET == 2) %>%
  select(OP_ID, OP_DATE, YEAR, LOCATION, BEG_LATITUDE_DD, END_LATITUDE_DD, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
         BEG_DEPTH, END_DEPTH, M_UNIT) %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  group_by(LOCATION) %>%
  summarise(Beg.Lat = mean(BEG_LATITUDE_DD), Beg.Long = mean(BEG_LONGITUDE_DD), 
            End.Lat = mean(END_LATITUDE_DD), End.Long = mean(END_LONGITUDE_DD),
            Beg.Depth_m = mean(BEG_DEPTH), End.Depth_m = mean(END_DEPTH), 
            Min.Beg.Lat = min(BEG_LATITUDE_DD), Min.Beg.Long = min(BEG_LONGITUDE_DD), 
            Min.End.Lat = min(END_LATITUDE_DD), Min.End.Long = min(END_LONGITUDE_DD),
            Max.Beg.Lat = max(BEG_LATITUDE_DD), Max.Beg.Long = max(BEG_LONGITUDE_DD), 
            Max.End.Lat = max(END_LATITUDE_DD), Max.End.Long = max(END_LONGITUDE_DD),
            Diff.Beg.Lat = Max.Beg.Lat-Min.Beg.Lat, Diff.Beg.Long = Max.Beg.Long-Min.Beg.Long, 
            Diff.End.Lat = Max.End.Lat-Min.End.Lat, Diff.End.Long = Max.End.Long-Min.End.Long)

openxlsx::write.xlsx(ns.sites, here('Plots and Tables/RVCAT','export_ns_sites.xlsx'))


#sites1 <- all.data %>%
#  subset(LOCATION == 454 & YEAR >=2010 & TARGET == 2) %>%
#  distinct(OP_ID, .keep_all = TRUE) %>%
#  select(OP_ID, OP_DATE, YEAR, LOCATION, BEG_LATITUDE_DD, END_LATITUDE_DD, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
#       BEG_DEPTH, END_DEPTH, M_UNIT)
 
 
##Single species collections, best for rare species
ggplot(subset(all.data, SPECIES == 127), aes(x=YEAR, y=NUM))+
  geom_bar(stat='identity', fill='plum3', color='plum3')+
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks()) +
  plot_theme+
  labs(x='Year', y='Count',caption=ann_data_access,
       title='Lake Superior Burbot Catches',
       subtitle='USGS trawl assessments, 1963-2022') 

ggsave(here('Plots and Tables/RVCAT','LS.Burbot.Collections.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Single species collection map, best for rare species
ls_poly <- readOGR(dsn =here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


ggplot(subset(all.data, SPECIES == 127), aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(color='plum3', size=4, stroke=1.5)+
  map_theme+
#  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Burbot Collections',
       subtitle='USGS trawl assessments, 1963-2022') 

  ggsave(here('Plots and Tables/RVCAT','LS.Burbot.Collections.Map.png'), dpi = 300, width = 40, height = 20, units = "cm") 
  

  ##Single Species Data Export - no Zeros
export.singlespecies <- all.data %>%
  subset(SPECIES == 127) %>%
  left_join(sci.names) %>%
  mutate(Trawl_Type = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ "Bottom trawl", 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ "Mid-water trawl")) %>%
  select(OP_ID, OP_DATE,TIME, YEAR, M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         Trawl_Type, BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM,NOHA,KGHA) %>%

  write.csv(here('Plots and Tables/RVCAT','LS.Burbot.Collections.csv'))


ggplot(subset(all.data, SPECIES == 127), aes(x=END_DEPTH, y=NUM)) +
#  geom_point(stat='identity', fill='plum3', color='plum3', size = 5)+
  geom_bar(stat='identity', fill='plum3', color='plum3')+
  scale_x_continuous(breaks = pretty_breaks(10)) +
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks()) +
  plot_theme+
  labs(x='Depth of capture (m)', y='Count',caption=ann_data_access,
       title='Lake Superior Burbot Catches',
       subtitle='USGS bottom trawl assessments, 1963-2022') 

ggsave(here('Plots and Tables/RVCAT','LS.Burbot.DepthOfcapture.png'), dpi = 300, width = 40, height = 20, units = "cm") 



##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
###Coregonus PVA data request
sci.names<-select(codes.to.names, c(1:3,8:10))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

export.pva <- all.data %>%
  select(OP_ID,OP_DATE,YEAR,TIME,TARGET,TR_DESIGN,M_UNIT, STATE, Country,LOCATION,Mid.Lat.DD,Mid.Long.DD,
       BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
       DISTANCE,HA_SWEPT,SPECIES,NUM,NOHA,KGHA) %>%
  subset(YEAR >= 1978) %>% 
  subset(TARGET==2 | TARGET==118 | TARGET==117 & YEAR>2010) %>% 
  subset(TR_DESIGN==25 | TR_DESIGN==4) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "CSMI", 
                                            "118" = "offshore"))) %>%
  left_join(sci.names)

export.station.survey.pva <-export.pva  %>%
  select(LOCATION,survey) %>%
  distinct(LOCATION, .keep_all = TRUE)


export.Catch.NoZeros.pva <- export.pva %>%
  select(OP_ID,survey,OP_DATE,TIME, STATE, Country,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,TYPE,DIET,NUM,NOHA,KGHA) %>%
  mutate(TRAWL_TYPE = "Bottom trawl") 

##If weights were NOT taken replace KGHA with 'na'
export.Catch.NoZeros.pva$KGHA[export.Catch.NoZeros.pva$KGHA == 0 & export.Catch.NoZeros.pva$NUM >0] <- NA

##Pull out all individual trawls
export.trawls.pva<-export.pva  %>%
  select(OP_ID,OP_DATE,TIME,survey, YEAR, STATE, Country,M_UNIT,LOCATION,TOW_TIME,DISTANCE,HA_SWEPT,
         BEG_DEPTH,END_DEPTH,Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp) %>%
  distinct(OP_ID, .keep_all = TRUE)

###Add zeros to NUM for fish species that were not caught
export.trawls.zeros.allspecies.pva <- export.pva  %>%
  select(OP_ID,SPECIES, NUM, NOHA, KGHA) 

export.trawls.zeros.allspecies.pva <-complete(export.trawls.zeros.allspecies.pva, OP_ID, SPECIES, 
                                          fill=list(NUM=0, NOHA=0, KGHA=0)) 

export.tfish.trawls.pva <- export.trawls.zeros.allspecies.pva %>%
  group_by(OP_ID) %>%
  summarize(Tfish = sum(NUM))

##get rid of trawls where SPECIES == 0 (NO FISH CAUGHT) and where fish were actually caught
export.trawls.zeros.allspecies.pva <- export.trawls.zeros.allspecies.pva %>%
  left_join(export.tfish.trawls.pva) %>%
  #  subset(Tfish > 0 & SPECIES != 0 | Tfish == 0 & SPECIES == 0)  %>%
  left_join(export.trawls.pva) %>% 
  left_join(sci.names) 

export.Catch.Zeros.pva <- export.trawls.zeros.allspecies.pva %>%
  select(OP_ID,survey,OP_DATE,TIME, STATE, Country,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,TYPE,DIET,NUM,NOHA,KGHA) %>%
  mutate(TRAWL_TYPE = "Bottom trawl")

##If weights were NOT taken replace KGHA with 'na'
export.Catch.Zeros.pva$KGHA[export.Catch.Zeros.pva$KGHA == 0 & export.Catch.Zeros.pva$NUM >0] <- NA

##Get rid of records where SPECIES == 0 and fish were caught in that trawl
export.Catch.Zeros.pva <- export.Catch.Zeros.pva %>%
  left_join(export.tfish.trawls.pva) %>%
  subset(SPECIES == 0 & Tfish ==0 | Tfish >0 & SPECIES !=0) %>%
  select(OP_ID,survey,OP_DATE,TIME, STATE, Country, M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,TYPE,DIET,NUM,NOHA,KGHA,TRAWL_TYPE) 


pva1 <- export.Catch.Zeros.pva %>%
  select(OP_ID,survey,OP_DATE,TIME, STATE, Country,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM,NOHA,KGHA) %>%
  subset(SPECIES == 202 | SPECIES == 203 | SPECIES == 204 | SPECIES == 206 | SPECIES == 210) %>%
  group_by(survey, SPECIES) %>%
  summarize(Samples = n(), Present = sum(NUM > 0, na.rm = TRUE), 
            minLat = min(Mid.Lat.DD), maxLat = max(Mid.Lat.DD),
            minLong = min(Mid.Long.DD), maxLong = max(Mid.Long.DD),
            minDepth = min(BEG_DEPTH), maxDepth = max(END_DEPTH), meanDepth = mean((END_DEPTH+BEG_DEPTH)/2),
            medianDepth = median((END_DEPTH+BEG_DEPTH)/2), sdDepth = sd((END_DEPTH+BEG_DEPTH)/2)) %>%
  mutate(Ppresent = Present / Samples) %>%
  ungroup()


##Depths for both surveys for larval surveys
pva3 <- export.Catch.Zeros.pva %>%
  select(OP_ID,survey,OP_DATE,TIME, STATE, Country,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM,NOHA,KGHA) %>%
  subset(SPECIES == 202 | SPECIES == 203 | SPECIES == 204 | SPECIES == 206 | SPECIES == 210) %>%
  group_by(SPECIES) %>%
  summarize(Samples = n(), Present = sum(NUM > 0, na.rm = TRUE), 
            minLat = min(Mid.Lat.DD), maxLat = max(Mid.Lat.DD),
            minLong = min(Mid.Long.DD), maxLong = max(Mid.Long.DD),
            minDepth = min(BEG_DEPTH), maxDepth = max(END_DEPTH), meanDepth = mean((END_DEPTH+BEG_DEPTH)/2),
            medianDepth = median((END_DEPTH+BEG_DEPTH)/2), sdDepth = sd((END_DEPTH+BEG_DEPTH)/2)) %>%
  mutate(Ppresent = Present / Samples) %>%
  ungroup()


##PVA Length information

export.pva2<-read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) 

export.pva2$SPECIES<-as.factor(export.pva2$SPECIES)


##Expanded age-1 length cutoffs to account for faster growth observed in 2021
pva2 <- export.trawls.pva %>%
  select(OP_ID, survey) %>% 
  full_join(export.pva2) %>% 
  subset(SPECIES == 202 | SPECIES == 203 | SPECIES == 204 | SPECIES == 206 | SPECIES == 210) %>%
  group_by(survey, SPECIES) %>%
  summarise(meanL = mean(LENGTH), sdL = sd(LENGTH),
            count = n(), maxL = max(LENGTH), minL = min(LENGTH)) %>%
  ungroup()



###########################################################################################
###########################################################################################
##DATA EXPORT NEARSHORE AND OFFSHORE DATA ALL SPECIES, YEARS, SITES
###########################################################################################
##############################################################################
sci.names<-select(codes.to.names, c(1:3,8:10))
sci.names$SPECIES<-as.factor(sci.names$SPECIES)

export.ns.os <- all.data %>%
  select(OP_ID,OP_DATE,YEAR,TIME, VESSEL,TARGET,TR_DESIGN,M_UNIT,STATE,Country,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,NUM,NOHA,KGHA) %>%
  subset(YEAR >= 1978) %>% 
  subset(TARGET==2 | TARGET==118 | TARGET==117 & YEAR>2010 & END_DEPTH>84) %>% 
  subset(TR_DESIGN==25 | TR_DESIGN==4) %>% 
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore"))) %>%
  left_join(sci.names)

##  subset(M_UNIT == "WI2") %>% 
##   subset(Country == "USA") %>% 
##  subset(STATE == "ONT.E") %>% 

export.station.survey <-export.ns.os %>%
  select(LOCATION,survey) %>%
  distinct(LOCATION, .keep_all = TRUE)


export.Catch.NoZeros <- export.ns.os %>%
  select(OP_ID,survey,OP_DATE,TIME, VESSEL,STATE, Country,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,TYPE,DIET,NUM,NOHA,KGHA) %>%
  mutate(TRAWL_TYPE = "Bottom trawl") 

##If weights were NOT taken replace KGHA with 'na'
export.Catch.NoZeros$KGHA[export.Catch.NoZeros$KGHA == 0 & export.Catch.NoZeros$NUM >0] <- NA

##Pull out all individual trawls
export.trawls<-export.ns.os  %>%
  select(OP_ID,OP_DATE,TIME, VESSEL,survey, YEAR, STATE, Country,M_UNIT,LOCATION,TOW_TIME,DISTANCE,HA_SWEPT,
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
  select(OP_ID,survey,OP_DATE,TIME, YEAR, VESSEL, STATE, Country,M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,TYPE,DIET,NUM,NOHA,KGHA) %>%
  mutate(TRAWL_TYPE = "Bottom trawl")

##If weights were NOT taken replace KGHA with 'na'
export.Catch.Zeros$KGHA[export.Catch.Zeros$KGHA == 0 & export.Catch.Zeros$NUM >0] <- NA

##Get rid of records where SPECIES == 0 and fish were caught in that trawl
export.Catch.Zeros <- export.Catch.Zeros %>%
  left_join(export.tfish.trawls) %>%
  subset(SPECIES == 0 & Tfish ==0 | Tfish >0 & SPECIES !=0) %>%
  select(OP_ID,survey,OP_DATE,TIME, YEAR, VESSEL, STATE, Country, M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,TYPE,DIET,NUM,NOHA,KGHA,TRAWL_TYPE) 

##Calculate total number of species caught that year
export.annual.total.fish <- export.trawls.zeros.allspecies %>% 
  subset(NUM>0) %>%
  group_by(YEAR, survey) %>%
  distinct(SPECIES) %>%
  summarise(fishes.total = n())

##Sum KGHA for native and exotic species
export.trawls.sum.type<- export.ns.os %>%
  select(OP_ID,TYPE, KGHA) %>%
  group_by(OP_ID, TYPE) %>%
  summarise(TYPE.KGHA = sum(KGHA)) %>%
  pivot_wider(names_from = TYPE, values_from = TYPE.KGHA, values_fill = 0) %>%
  renameCol('native', 'native.KGHA') %>%
  renameCol('exotic', 'exotic.KGHA') 
  
##Sum KGHA for prey and predator
export.trawls.sum.diet<- export.ns.os %>%
  select(OP_ID,DIET, KGHA) %>%
  group_by(OP_ID, DIET) %>%
  summarise(DIET.KGHA = sum(KGHA)) %>%
  pivot_wider(names_from = DIET, values_from = DIET.KGHA, values_fill = 0) %>%
  renameCol('prey', 'prey.KGHA') %>%
  renameCol('predator', 'predator.KGHA') 
  

##Sum KGHA for prey and predator by native and exotic
export.trawls.sum.type.diet<- export.ns.os %>%
  select(OP_ID, TYPE, DIET, KGHA) %>%
  mutate(type_diet = paste(TYPE, DIET, sep = ".")) %>%
  group_by(OP_ID, type_diet) %>%
  summarise(TYPE.DIET.KGHA = sum(KGHA)) %>%
  pivot_wider(names_from = type_diet, values_from = TYPE.DIET.KGHA, values_fill = 0) %>%
  renameCol('native.prey', 'native.prey.KGHA') %>%
  renameCol('native.predator', 'native.predator.KGHA') %>%
  renameCol('exotic.prey', 'exotic.prey.KGHA') %>%
  renameCol('exotic.predator', 'exotic.predator.KGHA')

##Sum KGHA and NOHA across all species to get total biomass and total number per ha
export.trawls.sum<- export.ns.os %>%
  select(OP_ID,NUM,NOHA,KGHA) %>%
  group_by(OP_ID) %>%
  summarise(KGHA = sum(KGHA), NUM=sum(NUM), NOHA = SUM(NOHA), Fishes = n()) %>%
  left_join(export.trawls) %>% 
  left_join(export.trawls.sum.type) %>%
  left_join(export.trawls.sum.diet) %>%
  left_join(export.trawls.sum.type.diet) %>%
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
            mean.kgha=mean(KGHA), median.kgha = median(KGHA), 
            sd.kgha = sd(KGHA), std.error.kgha = std.error(KGHA), skewness.kgha=skewness(KGHA),
            min.kgha=min(KGHA), max.kgha = max(KGHA),
            mean.native.kgha=mean(native.KGHA), mean.exotic.kgha=mean(exotic.KGHA), 
            mean.prey.kgha=mean(prey.KGHA), mean.predator.kgha=mean(predator.KGHA), 
            mean.native.prey.kgha=mean(native.prey.KGHA), mean.exotic.prey.kgha=mean(exotic.prey.KGHA), 
            mean.native.predator.kgha=mean(native.predator.KGHA), mean.exotic.predator.kgha=mean(exotic.predator.KGHA), 
            mean.noha=mean(NOHA), median.noha = median(NOHA), sd.noha = sd(NOHA), 
            std.error.noha = std.error(NOHA), skewness.noha=skewness(NOHA),
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
  select(OP_ID,YEAR,survey,SPECIES,COMMON_NAME, KGHA, NOHA) %>%
  subset(survey == 'nearshore') %>%
  subset(SPECIES == 109 |
           SPECIES == 202 |
           SPECIES == 203 |
           SPECIES == 204 |
           SPECIES == 307 |
           SPECIES == 308 |
           SPECIES == 317 |
           SPECIES == 127 |
           SPECIES == 902 |
           SPECIES == 903 |
           SPECIES == 904 | 
           SPECIES == 130 | 
           SPECIES == 131 | 
           SPECIES == 404 |
           SPECIES == 211 |
           SPECIES == 212 |
           SPECIES == 206 |
           SPECIES == 207 |
           SPECIES == 210 |
           SPECIES == 217 |
           SPECIES == 106 |
           SPECIES == 805 |
           SPECIES == 129 |
           SPECIES == 508) %>%
  group_by(YEAR,COMMON_NAME) %>% 
  summarise(mean.kgha=mean(KGHA),
            mean.noha=(mean(NOHA))) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = c(mean.kgha, mean.noha), values_fill = 0) %>%
  ungroup()

export.annual.ns.table <- export.annual.sum %>%
  subset(survey == 'nearshore') %>%
  left_join(export.annual.ns.species.sum) %>%
  select(YEAR, locations, sites.zerofish, mean.kgha, median.kgha, fishes.mean, fishes.total,
       "Bloater", "Cisco", "Lake Whitefish", "Rainbow Smelt", 
       "hatchery Lake Trout", "lean Lake Trout", "siscowet Lake Trout", "Burbot",
       "Slimy Sculpin", "Spoonhead Sculpin", "Deepwater Sculpin", "Ninespine Stickleback", 
       "Trout-Perch", "Longnose Sucker", "Pygmy Whitefish", "Round Whitefish", "Kiyi", 
       "Blackfin Cisco", "Shortjaw Cisco", "Unidentified Coregonid", 
       "Alewife", "Ruffe", "Threespine Stickleback", "Spottail Shiner") %>%
  mutate(all_sculpins = rowSums(.[16:18])) %>%
  mutate(other_species = mean.kgha - rowSums(.[8:30]) - all_sculpins) 

##Offshore annual Biomass summary Table for export
export.annual.os.species.sum <- export.trawls.zeros.allspecies %>%
  select(OP_ID,YEAR,survey,SPECIES,COMMON_NAME, KGHA) %>%
  subset(survey == 'offshore') %>%
  subset(SPECIES == 206 |
           SPECIES == 308 |
           SPECIES == 904) %>%
  group_by(YEAR,COMMON_NAME) %>% 
  summarise(mean.kgha=mean(KGHA)) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = mean.kgha) 

export.annual.os.table <- export.annual.sum %>%
  subset(survey == 'offshore') %>%
  left_join(export.annual.os.species.sum) %>%
  select(YEAR, locations, sites.zerofish, mean.kgha, fishes.mean, fishes.total,
         "Kiyi", "siscowet Lake Trout", "Deepwater Sculpin")


##list of species caught and the number of individuals for a particular year, usually max(YEAR)
export.annual.catch <- export.Catch.Zeros %>%
  select(OP_ID, YEAR, survey, SPECIES, NUM, NOHA, KGHA) %>%
  subset(YEAR == max(YEAR)) %>%
  group_by(SPECIES,survey) %>% 
  summarise(sites = sum(NUM>0),
            catch = sum(NUM),
            NOHA = mean(NOHA),
            KGHA = mean(KGHA)) %>% 
  ungroup() %>%
  filter(catch >0) %>%
  pivot_wider(names_from = survey, values_from = c(sites, catch, NOHA, KGHA), values_fill = 0) %>%
  left_join(sci.names) %>%
  select(-TYPE, -DIET) %>%
  select(SPECIES, COMMON_NAME, SCIENTIFIC_NAME, sites_nearshore, catch_nearshore, NOHA_nearshore, KGHA_nearshore,
         sites_offshore, catch_offshore, NOHA_offshore, KGHA_offshore)


##list of species caught by survey including number of individuals, NOHA, and KGHA for a particular year, usually max(YEAR)
export.annual.stats.byspecies <- export.trawls.zeros.allspecies %>%
  select(OP_ID, YEAR, survey, SPECIES, NUM, NOHA, KGHA) %>%
  subset(YEAR == max(YEAR)) %>%
  group_by(SPECIES,survey) %>% 
  summarise(sites.occur =  sum(NUM>=1), 
            catch.total = sum(NUM), catch.max = max(NUM), catch.min = min(NUM), catch.mean = mean(NUM),
            NOHA.max = max(NOHA), NOHA.min = min(NOHA), NOHA.mean = mean(NOHA),
            KGHA.max = max(KGHA), KGHA.min = min(KGHA), KGHA.mean = mean(KGHA)) %>%
  ungroup() %>%
  subset(catch.total >0)  %>%
  left_join(sci.names)
  

##Biomass Summary By Nearshore and Offshore Stations for period of record
export.station.sum <- export.trawls.sum %>%
  group_by(LOCATION) %>% 
  summarise(Years=n(), 
            min.year=min(YEAR), max.year=max(YEAR), 
            median.begdepth=median(BEG_DEPTH), min.begdepth=min(BEG_DEPTH), max.begdepth=max(BEG_DEPTH),
            median.enddepth=median(END_DEPTH), min.enddepth=min(END_DEPTH), max.enddepth=max(END_DEPTH),
            mean.surface.temp=mean(Surface.Temp), min.surface.temp=min(Surface.Temp), max.surface.temp=max(Surface.Temp),
            mean.bottom.temp=mean(Bottom.Temp), min.bottom.temp=min(Bottom.Temp), max.bottom.temp=max(Bottom.Temp),
            mean.kgha=mean(KGHA), median.kgha = median(KGHA), 
            sd.kgha = sd(KGHA), std.error.kgha = std.error(KGHA), skewness.kgha=skewness(KGHA),
            min.kgha=min(KGHA), max.kgha = max(KGHA),
            mean.native.kgha=mean(native.KGHA), mean.exotic.kgha=mean(exotic.KGHA), 
            mean.prey.kgha=mean(prey.KGHA), mean.predator.kgha=mean(predator.KGHA), 
            mean.native.prey.kgha=mean(native.prey.KGHA), mean.exotic.prey.kgha=mean(exotic.prey.KGHA), 
            mean.native.predator.kgha=mean(native.predator.KGHA), mean.exotic.predator.kgha=mean(exotic.predator.KGHA), 
            mean.noha=mean(NOHA), median.noha = median(NOHA), sd.noha = sd(NOHA), 
            std.error.noha = std.error(NOHA), skewness.noha=skewness(NOHA),
            sum.num=sum(NUM),
            fishes.mean = mean(Fishes), fishes.median = median(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) %>%
  left_join(export.station.survey) %>%
  ungroup()


##### Age-1 abundance table ################################################################
############################################################################################
##Lengths file to evaluate Age-1 densities##################################################
#sci.names$SPECIES<-as.numeric(sci.names$SPECIES)

lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) 

lengths$SPECIES<-as.factor(lengths$SPECIES)

########################################################################################
########################################################################################
##Expanded age-1 length cutoffs if you want to account for faster growth 
##Adjust lengths and then don't run traditional code below
#export.age1.ns.os <- export.trawls %>%
#  full_join(lengths) %>% 
#  subset(SPECIES == '109' & LENGTH <101 & survey == "nearshore" |
#           SPECIES == '202' & LENGTH <141 & survey == "nearshore" |
#           SPECIES == '204' & LENGTH <131 & survey == "nearshore" |
#           SPECIES == '203' & LENGTH <191 & survey == "nearshore" |
#           SPECIES == '317' & LENGTH <226 & survey == "nearshore" |
#           SPECIES == '308' & LENGTH <226 & survey == "offshore" |
#           SPECIES == '206' & LENGTH <161 & survey == "offshore") %>%
#  select(OP_ID,SPECIES,EXP_N) %>%
#  group_by(OP_ID,SPECIES) %>%
#  summarise(NUM = sum(EXP_N)) %>%
#  ungroup()
########################################################################################
########################################################################################


###Traditional Age-1 length cutoffs
export.age1.ns.os <- export.trawls %>% 
  select(OP_ID,survey,OP_DATE,YEAR,LOCATION) %>% 
  left_join(lengths) %>% 
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
  ungroup() %>%
  right_join(export.trawls) %>%
  select(OP_ID, SPECIES, NUM)


###Add zeros to NUM for fish species that were not caught
export.age1.zeros <-complete(export.age1.ns.os, OP_ID, SPECIES, 
                                          fill=list(NUM=0))

export.age1.complete <- export.age1.zeros %>%
  left_join(export.trawls) %>%
  left_join(sci.names) %>%
  select(OP_ID,survey,OP_DATE,YEAR,TIME, VESSEL, STATE, Country, M_UNIT,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH,Surface.Temp,Bottom.Temp, TOW_TIME, 
         DISTANCE,HA_SWEPT,SPECIES,COMMON_NAME,SCIENTIFIC_NAME,NUM) %>%
  subset(SPECIES == '109' & survey == "nearshore" |
           SPECIES == '202' & survey == "nearshore" |
           SPECIES == '204' & survey == "nearshore" |
           SPECIES == '203' & survey == "nearshore" |
           SPECIES == '317' & survey == "nearshore" |
           SPECIES == '308' & survey == "offshore" | 
           SPECIES == '206' & survey == "offshore") %>%
  mutate(NOHA=NUM/HA_SWEPT) 

export.age1.table <- export.age1.complete %>% 
  group_by(YEAR, survey, SPECIES) %>% 
  summarise(age1.mean = mean(NOHA), sites = n()) %>%
  ungroup() %>%
  select(YEAR, survey, sites, SPECIES, age1.mean) %>%
  subset(!is.na(SPECIES)) %>%
  subset(SPECIES == '109' & survey == "nearshore" |
           SPECIES == '202' & survey == "nearshore" |
           SPECIES == '204' & survey == "nearshore" |
           SPECIES == '203' & survey == "nearshore" |
           SPECIES == '317' & survey == "nearshore" |
           SPECIES == '308' & survey == "offshore" | 
           SPECIES == '206' & survey == "offshore") %>%
  select(YEAR, sites, SPECIES, age1.mean) %>%
  left_join(sci.names) %>%
  mutate('year.class' = YEAR-1) %>%
  select(YEAR, 'year.class', sites, COMMON_NAME, age1.mean) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = age1.mean) %>%
  round(2) 


##########################
#Age-1 indexes by State

export.age1.state.table <- export.age1.complete %>% 
  group_by(YEAR, survey, STATE, SPECIES) %>% 
  summarise(age1.mean = mean(NOHA), sites = n()) %>%
  ungroup() %>%
  select(YEAR, survey, STATE, sites, SPECIES, age1.mean) %>%
  subset(!is.na(SPECIES)) %>%
  subset(SPECIES == '109' & survey == "nearshore" |
           SPECIES == '202' & survey == "nearshore" |
           SPECIES == '204' & survey == "nearshore" |
           SPECIES == '203' & survey == "nearshore" |
           SPECIES == '317' & survey == "nearshore" |
           SPECIES == '308' & survey == "offshore" | 
           SPECIES == '206' & survey == "offshore") %>%
  select(YEAR, STATE, sites, SPECIES, age1.mean) %>%
  subset(SPECIES == 203) %>%
  mutate('year.class' = YEAR-1) %>%
  round(2)

openxlsx::write.xlsx(export.age1.state.table, here('Plots and Tables/RVCAT','export_Age1_States.xlsx'))


##list of nearshore and offshore sites
export.ns.os.sites <- all.data %>%
  select(OP_ID,OP_DATE,YEAR,TARGET,TR_DESIGN,M_UNIT, STATE, Country,LOCATION,Mid.Lat.DD,Mid.Long.DD,
         BEG_DEPTH,END_DEPTH) %>%
  subset(YEAR >= 1978) %>% 
  subset(TARGET==2 | TARGET==118 | TARGET==117 & YEAR>2010 & END_DEPTH>84) %>% 
  subset(TR_DESIGN==25 | TR_DESIGN==4) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore"))) %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  group_by(LOCATION) %>%
  summarise(Latitude = mean(Mid.Lat.DD), Longitude = mean(Mid.Long.DD)) %>%
  left_join(all.data) %>%
  select(LOCATION, M_UNIT,  STATE, Country, Latitude, Longitude) %>%
  distinct(LOCATION, .keep_all = TRUE)
  

##Export to Excel File
library(openxlsx)
metadata<-read.xlsx(here('Data','ns_os_all_MetaData.xlsx'))
vesseldata<-read.xlsx(here('Data','VesselData.xlsx'))
list.sheets<-list('Catch_Zeros'= export.Catch.Zeros,
                  'Catch_NoZeros'=export.Catch.NoZeros,
                  'AnnualSummary'=export.annual.sum,
                  'NS_table' = export.annual.ns.table, 
                  'OS_table' = export.annual.os.table,
                  'Age1_Fish_Zeros' = export.age1.complete, 
                  'Age1_Fish_Sum' = export.age1.table, 
                  'Catch_table' = export.annual.catch,
                  'Catch_BySpecies' = export.annual.stats.byspecies,
                  'NS_KGHA_NOHA' = export.annual.ns.species.sum,
                  'ByStations' = export.station.sum, 
                  'Locations' = export.ns.os.sites,
                  'Species' = sci.names,
                  'MetaData'= metadata,
                  'VesselData'= vesseldata)

openxlsx::write.xlsx(list.sheets, here('Plots and Tables/RVCAT','export_ns_os_all.xlsx'))


###########################################################################################
##DATA EXPORT NEARSHORE AND OFFSHORE DATA FOR LSTC FCO ANALYSES
###########################################################################################

##calculate summary stats by year
export.fco.annual.sum <- export.annual.sum %>% 
  select(YEAR, survey, locations,
         mean.kgha, 
         mean.native.kgha,
         mean.exotic.kgha, 
         mean.prey.kgha,
         mean.predator.kgha, 
         mean.native.prey.kgha,
         mean.exotic.prey.kgha, 
         mean.native.predator.kgha,
         mean.exotic.predator.kgha) 
         

export.fco.age1.cisco <- export.age1.table %>% 
  select(YEAR, Cisco)

export.annual.catch.fco <- export.trawls.zeros.allspecies %>%
  select(OP_ID, YEAR, survey, SPECIES, NUM) %>%
  subset(NUM > 0) %>%
  subset(SPECIES != 999) %>%
  group_by(SPECIES,survey) %>% 
  summarise(catch = sum(NUM)) %>% 
  pivot_wider(names_from = survey, values_from = catch, values_fill = 0) %>%
  ungroup() %>%
  left_join(sci.names) %>%
  select(COMMON_NAME,SCIENTIFIC_NAME, TYPE,DIET,nearshore, offshore) %>%
  arrange(desc(nearshore))
  


##Export to Excel File
##library(openxlsx)
metadata<-read.xlsx(here('Data','LSTC_FCO_MetaData.xlsx'))
list.sheets<-list('Biomass'=export.fco.annual.sum,
                  'Age1_Cisco_Density' = export.fco.age1.cisco,
                  'Catch' = export.annual.catch.fco)

openxlsx::write.xlsx(list.sheets, here('Plots and Tables/RVCAT','LSTC_FCO.xlsx'))

##############################################################################################
##############################################################################################

###If you want to compare data only from a partcular Managemen Unit or State or Country do it now
## ns<-filter(all.data, TARGET == 2 & YEAR>1977 & STATE== 'MN')
##ns<-subset(all.data, TARGET == 2 & YEAR>1977 & M_UNIT == "WI2")

##pull out nearshore, TARGET = 2 data for a normal full survey year

##ns<-subset(all.data, TARGET==2 & YEAR >1973 &  M_UNIT == "WI2")
##ns<-subset(all.data, TARGET==2 & YEAR >1977 &  STATE == "ONT.E")


ns<-subset(all.data, TARGET==2 & YEAR >1977)


##Pull out all individual nearshore trawls
ns.trawls<-ns %>%
  select(OP_ID,SERIAL,TARGET,OP_DATE, YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
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
  select(OP_ID,TARGET,OP_DATE, YEAR, LOCATION,BEG_DEPTH,END_DEPTH,
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
##os<-subset(os, Country == 'USA')
##os<-subset(os, STATE == "ONT.E")


##Pull out all individual offshore trawls
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


##calculate summary stats for offshore total biomass by year
os.annual.sum <- os.trawls.sum %>% 
  group_by(YEAR) %>% 
  summarise(locations=n(), mean=mean(KGHA), median = median(KGHA), 
            sd = sd(KGHA), std.error = std.error(KGHA),
            skewness=skewness(KGHA), fishes.mean = mean(Fishes), 
            fishes.min = min(Fishes), fishes.max = max(Fishes)) 

##############################################################################
##calculate annual offshore summary stats by year, and species
os.annual.sum.by.species <- os.trawls.all.species %>%
  group_by(YEAR, SPECIES) %>%
  summarise(mean = mean(KGHA), 
            median = median(KGHA), std.error = std.error(KGHA)) 

##Add fish common names to file
sci.names$SPECIES<-as.factor(sci.names$SPECIES)
os.annual.sum.by.species <- os.annual.sum.by.species %>%
  left_join(sci.names)

############################################################################
############################################################################
##Combine nearshore and offshore surveys for calculating depth zone weighted biomass
ns.os <- all.data %>%
  subset(TARGET==2 & YEAR >1977 | TARGET==118| TARGET==117 & YEAR>2010) %>%
  filter(TR_DESIGN==25|TR_DESIGN==4) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore"))) 

ns.os.trawls<-ns.os %>%
  select(OP_ID,SERIAL,TARGET,OP_DATE, YEAR, LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,
         TR_DESIGN,DISTANCE,TOW_TIME,HA_SWEPT) %>%
  distinct(OP_ID, .keep_all = TRUE)

ns.os.trawls.all.species <- ns.os %>%
  select(OP_ID,SPECIES, KGHA) 

##add zeros for species not collected
ns.os.trawls.all.species <-complete(ns.os.trawls.all.species, OP_ID, SPECIES, 
                                 fill=list(KGHA=0)) %>%
  left_join(ns.os.trawls) %>%
  left_join(sci.names) %>%
  mutate(Depth.Zone = case_when(
           END_DEPTH <= 50 ~ "0-50 m", 
           END_DEPTH > 50 & END_DEPTH <= 100 ~ "50-100 m",
           END_DEPTH > 100 & END_DEPTH <= 150 ~ "100-150 m",
           END_DEPTH > 150 & END_DEPTH <= 200 ~ "150-200 m",
           END_DEPTH > 200 ~ ">200 m")) %>%
  mutate(Depth.Zone.Area = case_when(
  Depth.Zone == "0-50 m" ~ 1120599, 
  Depth.Zone == "50-100 m" ~ 1065318, 
  Depth.Zone == "100-150 m" ~ 1541944, 
  Depth.Zone == "150-200 m" ~ 2210986, 
  Depth.Zone == ">200 m" ~ 2158177)) 


##get mean biomass for all species within depth zones 
ns.os.dzone.species.sum <- ns.os.trawls.all.species %>%
  select(OP_ID, TARGET, YEAR, SPECIES, COMMON_NAME, KGHA, Depth.Zone, Depth.Zone.Area) %>% 
  group_by(YEAR, Depth.Zone, SPECIES) %>%
  summarise(mean = mean(KGHA), locations = n()) %>%
  ungroup()

##get rid of predators, sum prey species to get total prey fish biomass
ns.os.dzone.prey <- ns.os.trawls.all.species %>%
  select(OP_ID, TARGET, YEAR, SPECIES, COMMON_NAME, KGHA, Depth.Zone, Depth.Zone.Area) %>% 
  group_by(OP_ID) %>% 
   ##remove piscivorus Lake trout and burbot
  subset(SPECIES != 317 &  SPECIES != 307 & SPECIES != 308 & SPECIES != 127) %>% 
  summarise(KGHA.Total = sum(KGHA)) %>%
  ungroup() %>%
  left_join(ns.os.trawls) %>%
  mutate(Depth.Zone = case_when(
    END_DEPTH <= 50 ~ "0-50 m", 
    END_DEPTH > 50 & END_DEPTH <= 100 ~ "50-100 m",
    END_DEPTH > 100 & END_DEPTH <= 150 ~ "100-150 m",
    END_DEPTH > 150 & END_DEPTH <= 200 ~ "150-200 m",
    END_DEPTH > 200 ~ ">200 m")) 

ns.os.dzone.preysum <- ns.os.dzone.prey %>%
  group_by(YEAR, Depth.Zone) %>%
  summarise(KGHA.mean = mean(KGHA.Total), KGHA.se = std.error(KGHA.Total), locations = n()) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(Depth.Zone.Area = case_when(
    Depth.Zone == "0-50 m" ~ 1120599, 
    Depth.Zone == "50-100 m" ~ 1065318, 
    Depth.Zone == "100-150 m" ~ 1541944, 
    Depth.Zone == "150-200 m" ~ 2210986, 
    Depth.Zone == ">200 m" ~ 2158177)) %>%
  mutate(KGHA.AreaWT = KGHA.mean * Depth.Zone.Area) %>%
  mutate(KGHA.se.AreaWT = KGHA.se * Depth.Zone.Area)

ns.os.dzone.preysum2 <- ns.os.dzone.preysum %>%
  group_by(YEAR) %>%
  summarize(KG.mean.sum = sum(KGHA.AreaWT), KG.se.sum = sum(KGHA.se.AreaWT), ) %>%
  ungroup() %>%
  mutate(KGHA.WTmean = KG.mean.sum/8097024) %>%
  mutate(KGHA.WTse = KG.se.sum/8097024)

## Reorder Depth Zones so thay plot logically
ns.os.dzone.preysum$Depth.Zone <- factor(ns.os.dzone.preysum$Depth.Zone, 
                                         levels = c("0-50 m", "50-100 m", "100-150 m", "150-200 m", ">200 m"))

ggplot(ns.os.dzone.preysum, aes(x=YEAR, y=KGHA.mean)) + 
  geom_point(shape = 21, size=3, fill = 'black')+
  geom_line(size = 1) +
  geom_errorbar(data=ns.os.dzone.preysum, aes(x=YEAR, ymin=KGHA.mean-KGHA.se, ymax=KGHA.mean+KGHA.se),
                width=0.4)+
  #geom_segment(aes(x=YEAR, xend=YEAR, y=0, yend=KGHA.mean), size=1, color='black')+
  plot_theme+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) + 
  ##, 
  ##                   expand = expansion(mult = c(0, .1)))+
  labs(x='Year', y='Mean prey fish biomass (kg per ha)', 
       caption=ann_data_access,
       title='Lake Superior Prey Fish Biomass by Depth Zone',
       subtitle='USGS nearshore and offshore bottom trawl assessments') +  
  facet_wrap(vars(Depth.Zone), ncol=2, scales='free_y') 

ggsave(here('Plots and Tables/RVCAT','ns_os_totalbiomass_depthzones.png'), dpi = 300, width = 40, height = 20, units = "cm")


ggplot(subset(ns.os.dzone.preysum2, YEAR>=2011), aes(x=YEAR, y=KGHA.WTmean)) + 
  geom_point(shape = 21, size=6, fill = 'black')+
  geom_errorbar(data=subset(ns.os.dzone.preysum2, YEAR>=2011), aes(x=YEAR, ymin=KGHA.WTmean-KGHA.WTse, ymax=KGHA.WTmean+KGHA.WTse),
                width=0.4)+
  geom_line(size = 1) +
  plot_theme+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(limits=c(0, NA), breaks = pretty_breaks())+
  labs(x='Year', y='Mean depth zone weighted biomass (kg per ha)', 
       caption=ann_data_access,
       title='Lake Superior Prey Fish Biomass',
       subtitle='USGS bottom trawl assessment')   

ggsave(here('Plots and Tables/RVCAT','ns_os__depthzone_weighted_totalbiomass.png'), dpi = 300, width = 40, height = 20, units = "cm")

##All years
ggplot(ns.os.dzone.preysum2, aes(x=YEAR, y=KGHA.WTmean)) + 
  geom_point(shape = 21, size=6, fill = 'black')+
  geom_errorbar(data=ns.os.dzone.preysum2, aes(x=YEAR, ymin=KGHA.WTmean-KGHA.WTse, ymax=KGHA.WTmean+KGHA.WTse),
                width=0.4)+
  geom_line(size = 1) +
  plot_theme+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(limits=c(0, NA), breaks = pretty_breaks())+
  labs(x='Year', y='Mean depth zone weighted biomass (kg per ha)', 
       caption=ann_data_access,
       title='Lake Superior Prey Fish Biomass',
       subtitle='USGS bottom trawl assessment')   


##### adjustable depth zones 
##get rid of predators, sum prey species to get total prey fish biomass
ns.os <- all.data %>%
  subset(TARGET==2 & YEAR >1977 | TARGET==118| TARGET==117 & YEAR>2010) %>%
  filter(TR_DESIGN==25|TR_DESIGN==4) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore"))) 

ns.os.trawls<-ns.os %>%
  select(OP_ID,SERIAL,TARGET,OP_DATE,YEAR,LOCATION,BEG_DEPTH,END_DEPTH,
         Mid.Lat.DD, Mid.Long.DD,Surface.Temp,Bottom.Temp,
         TR_DESIGN,DISTANCE,TOW_TIME,HA_SWEPT) %>%
  distinct(OP_ID, .keep_all = TRUE)

ns.os.trawls.all.species.x <- ns.os %>%
  select(OP_ID,SPECIES, KGHA) 

##add zeros for species not collected
ns.os.trawls.all.species.x <-complete(ns.os.trawls.all.species.x, OP_ID, SPECIES, 
                                    fill=list(KGHA=0)) %>%
  left_join(ns.os.trawls) %>%
  left_join(sci.names) %>%
  mutate(Depth.Zone = case_when(
    END_DEPTH <= 100 ~ "0-100 m", 
    END_DEPTH > 100 ~ ">100 m")) %>%
mutate(Depth.Zone.Area = case_when(
  Depth.Zone == "0-100 m" ~ 2185917, 
  Depth.Zone == ">100 m" ~ 5911107)) 
  
ns.os.dzone.prey.x <- ns.os.trawls.all.species.x %>%
  select(OP_ID, TARGET, YEAR, SPECIES, COMMON_NAME, KGHA, Depth.Zone, Depth.Zone.Area) %>% 
  group_by(OP_ID) %>% 
  ##remove piscivorus Lake trout and burbot
  subset(SPECIES != 317 &  SPECIES != 307 & SPECIES != 308 & SPECIES != 127) %>% 
  summarise(KGHA.Total = sum(KGHA)) %>%
  ungroup() %>%
  left_join(ns.os.trawls) %>%
  mutate(Depth.Zone = case_when(
    END_DEPTH <= 100 ~ "0-100 m", 
    END_DEPTH > 100 ~ ">100 m")) 

ns.os.dzone.preysum.x <- ns.os.dzone.prey.x %>%
  group_by(YEAR, Depth.Zone) %>%
  summarise(KGHA.mean = mean(KGHA.Total), KGHA.se = std.error(KGHA.Total), locations = n()) %>%
  ungroup() %>%
  drop_na() %>%
  mutate(Depth.Zone.Area = case_when(
    Depth.Zone == "0-100 m" ~ 2185917, 
    Depth.Zone == ">100 m" ~ 5911107)) %>%
  mutate(KGHA.AreaWT = KGHA.mean * Depth.Zone.Area) %>%
  mutate(KGHA.se.AreaWT = KGHA.se * Depth.Zone.Area)

ns.os.dzone.preysum2.x <- ns.os.dzone.preysum.x %>%
  group_by(YEAR) %>%
  summarize(KG.mean.sum = sum(KGHA.AreaWT), KG.se.sum = sum(KGHA.se.AreaWT), ) %>%
  ungroup() %>%
  mutate(KGHA.WTmean = KG.mean.sum/8097024) %>%
  mutate(KGHA.WTse = KG.se.sum/8097024)

## Reorder Depth Zones so thay plot logically
ns.os.dzone.preysum.x$Depth.Zone <- factor(ns.os.dzone.preysum.x$Depth.Zone, 
                                         levels = c("0-100 m", ">100 m"))

ggplot(ns.os.dzone.preysum.x, aes(x=YEAR, y=KGHA.mean)) + 
  geom_point(shape = 21, size=3, fill = 'black')+
  geom_line(size = 1) +
  geom_errorbar(data=ns.os.dzone.preysum.x, aes(x=YEAR, ymin=KGHA.mean-KGHA.se, ymax=KGHA.mean+KGHA.se),
                width=0.4)+
  #geom_segment(aes(x=YEAR, xend=YEAR, y=0, yend=KGHA.mean), size=1, color='black')+
  plot_theme+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(limits = c(0, NA), breaks = pretty_breaks()) + 
  ##, 
  ##                   expand = expansion(mult = c(0, .1)))+
  labs(x='Year', y='Mean prey fish biomass (kg per ha)', 
       caption=ann_data_access,
       title='Lake Superior Prey Fish Biomass by Depth Zone',
       subtitle='USGS nearshore and offshore bottom trawl assessments') +  
  facet_wrap(vars(Depth.Zone), ncol=2, scales='free_y') 

ggsave(here('Plots and Tables/RVCAT','ns_os_totalbiomass_depthzones_x.png'), dpi = 300, width = 40, height = 20, units = "cm")

 
ggplot(subset(ns.os.dzone.preysum2.x, YEAR>=2011), aes(x=YEAR, y=KGHA.WTmean)) + 
  geom_point(shape = 21, size=6, fill = 'black')+
  geom_errorbar(data=subset(ns.os.dzone.preysum2.x, YEAR>=2011), aes(x=YEAR, ymin=KGHA.WTmean-KGHA.WTse, ymax=KGHA.WTmean+KGHA.WTse),
                width=0.4)+
  geom_line(size = 1) +
  plot_theme+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(limits=c(0, NA), breaks = pretty_breaks())+
  labs(x='Year', y='Mean depth zone weighted biomass (kg per ha)', 
       caption=ann_data_access,
       title='Lake Superior Prey Fish Biomass',
       subtitle='USGS bottom trawl assessment')   

ggsave(here('Plots and Tables/RVCAT','ns_os__depthzone_weighted_totalbiomassa_x.png'), dpi = 300, width = 40, height = 20, units = "cm")

##All years
ggplot(ns.os.dzone.preysum2.x, aes(x=YEAR, y=KGHA.WTmean)) + 
  geom_point(shape = 21, size=6, fill = 'black')+
  geom_errorbar(data=ns.os.dzone.preysum2.x, aes(x=YEAR, ymin=KGHA.WTmean-KGHA.WTse, ymax=KGHA.WTmean+KGHA.WTse),
                width=0.4)+
  geom_line(size = 1) +
  plot_theme+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(limits=c(0, NA), breaks = pretty_breaks())+
  labs(x='Year', y='Mean depth zone weighted biomass (kg per ha)', 
       caption=ann_data_access,
       title='Lake Superior Prey Fish Biomass',
       subtitle='USGS bottom trawl assessment')   



#############################################################################
#subset previous years to plot averages over different time frames
ns.10ymean<-subset(ns.annual.sum,  YEAR>=(max(YEAR)-9))
ns.20ymean<-subset(ns.annual.sum, YEAR < (max(YEAR)-10) & YEAR > (max(YEAR)-19))
ns.30ymean<-subset(ns.annual.sum, YEAR < (max(YEAR)-20) & YEAR > (max(YEAR)-29))
ns.40ymean<-subset(ns.annual.sum, YEAR < (max(YEAR)-30) & YEAR > (max(YEAR)-39))
ns.50ymean<-subset(ns.annual.sum, YEAR < (max(YEAR)-40) & YEAR > (max(YEAR)-49))

mean.ns.kgha.currentyear<-ns.trawls.sum %>%
  subset(YEAR == max(ns.annual.sum$YEAR)) 

mean.os.kgha.currentyear<-os.trawls.sum %>%
  subset(YEAR == max(os.annual.sum$YEAR)) 

###############################################################################
##Nearshore data plots
##turn on/off the geom_segment(...) lines to change which mean lines you want to show

ggplot(subset(ns.annual.sum, YEAR != 2020 & YEAR != 2021), aes(x=YEAR, y=mean)) +
  geom_bar(stat='identity', fill='grey75', color='black') +
  geom_errorbar(data=subset(ns.annual.sum, YEAR != 2020 & YEAR != 2021), 
                aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),
                width=0.4) +
  geom_segment(x=(max(ns.annual.sum$YEAR)-9), xend=max(ns.annual.sum$YEAR), 
               y=mean(ns.10ymean$mean), yend=mean(ns.10ymean$mean), size=1.2, color='black') + #last 10 year mean
  geom_segment(x=(max(ns.annual.sum$YEAR)-10), xend=(max(ns.annual.sum$YEAR)-19), 
               y=mean(ns.20ymean$mean), yend=mean(ns.20ymean$mean), size=1.2, color='black') + #20 years ago mean
  geom_segment(x=(max(ns.annual.sum$YEAR)-20), xend=(max(ns.annual.sum$YEAR)-29), 
               y=mean(ns.30ymean$mean), yend=mean(ns.30ymean$mean), size=1.2, color='black') + #20 years ago mean
  geom_segment(x=(max(ns.annual.sum$YEAR)-30), xend=(max(ns.annual.sum$YEAR)-39), 
               y=mean(ns.40ymean$mean), yend=mean(ns.40ymean$mean), size=1.2, color='black') + #20 years ago mean
  geom_segment(x=(max(ns.annual.sum$YEAR)-40), xend=min(ns.annual.sum$YEAR), 
               y=mean(ns.50ymean$mean), yend=mean(ns.50ymean$mean), size=1.2, color='black') + #20 years ago mean
#  scale_x_continuous(breaks = pretty_breaks(),limits = c(1978, 2022)) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme +
  labs(x='Year', y='Mean biomass (kg per ha)', caption=ann_data_access,
       title='Lake Superior Fish Biomass',
       subtitle='USGS nearshore bottom trawl assessment')

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
##ns.annual.sum.2020 <- ns.annual.sum %>%
##  subset(YEAR <2020)

ggplot(ns.annual.sum, aes(x=YEAR, y=mean)) + 
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

##plot naive - exotic biomass

annual.sum.bytype <- export.annual.sum %>%
  select(YEAR, survey, mean.native.kgha, mean.exotic.kgha) %>%
  subset(survey == 'nearshore') %>%
  pivot_longer(cols = 3:4, names_to = 'type', values_to = 'kgha')
  

ggplot(subset(annual.sum.bytype, survey == 'nearshore')) +
  aes(x=YEAR, y = kgha, fill=type) + 
  geom_col(position = "fill") +
#  geom_bar(stat='identity', position='stack') +
  scale_fill_manual(labels = c('Exotic', 'Native'), values=c('palegreen3','sienna1')) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = scales::percent) +
#  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  plot_theme+
  labs(x='Year', y='Mean biomass proportion',caption=ann_data_access,
       title='Lake Superior Exotic and Native Fish Biomass',
       subtitle='USGS nearshore bottom trawl assessment')+
  geom_text(x = 2000, y = 95, label = "Exotic fish") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_native_exotic.png'), dpi = 300, width = 40, height = 20, units = "cm") 



#########################################################################################
###Cisco, Bloater, LWF, RBS, and other
ns.annual.sum.by.otherspecies <- ns.annual.sum.by.species %>%
  group_by(YEAR) %>%
  summarise(Total = sum(mean))

ns.annual.sum.cisco.bloater.lwf.other <- ns.annual.sum.by.species %>%
  subset(COMMON_NAME == "Cisco" |
           COMMON_NAME == "Bloater" |
           COMMON_NAME == "Lake Whitefish" |
           COMMON_NAME == "Rainbow Smelt") %>%
  select(YEAR, COMMON_NAME, mean) %>% 
  pivot_wider(names_from = COMMON_NAME, values_from = mean) %>%
  left_join(ns.annual.sum.by.otherspecies) %>%
  renameCol('Lake Whitefish','LWF') %>%
  renameCol('Rainbow Smelt','RBS') %>%
  mutate(Cisco.Bloater = Cisco + Bloater) %>% 
  mutate(CBLWF = Cisco.Bloater + LWF + RBS ) %>%
  mutate(Other = Total - CBLWF) %>%
  renameCol('LWF', 'Lake Whitefish') %>%
  renameCol('RBS', 'Rainbow Smelt') %>%
  select(YEAR, Cisco, Bloater, 'Lake Whitefish', 'Rainbow Smelt', Other) %>%
  pivot_longer(col = c(2:6), names_to = "COMMON_NAME", values_to = "mean")

ggplot(subset(ns.annual.sum.cisco.bloater.lwf.other,  
              COMMON_NAME == "Cisco" |
                COMMON_NAME == "Bloater" |
                COMMON_NAME == "Lake Whitefish" |
                COMMON_NAME == "Rainbow Smelt" |
                COMMON_NAME == "Other")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0),breaks = pretty_breaks())+
  plot_theme+
  theme(axis.text=element_text(size=20, family='serif'),
        axis.title=element_text(size=20, family='serif'),
        legend.text=element_text(size=20, family='serif'),
        legend.title=element_text(size=20, family='serif'),
        plot.title=element_text(size=20, family='serif'),
        plot.subtitle=element_text(size=18, family='serif')) +
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Fish Biomass',
       subtitle='USGS nearshore bottom trawl assessment')+
  scale_fill_manual(name = '', labels = c('Bloater','Cisco', 'Lake Whitefish', 'Rainbow Smelt', 'Other'), 
                    values=c('palegreen3','sienna1', 'lightpink3', 'skyblue3', 'yellow2')) +
  theme(legend.position = c(0.6,0.9))

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_cisco_bloater_lwf_rbs_other_stackbar.png'), dpi = 300, width = 40, height = 20, units = "cm") 



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


###Cisco, Bloater, LWF - stacked bar
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

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_cisco_bloater_lwf_stackbar.png'), dpi = 300, width = 40, height = 20, units = "cm") 

###Cisco, Bloater, LWF - facet
fmean <- ns.annual.sum.by.species %>%
  subset(COMMON_NAME == "Cisco" | 
           COMMON_NAME == "Bloater" |
           COMMON_NAME == "Lake Whitefish") %>%
  group_by(COMMON_NAME) %>%
  summarise(mean = mean(mean))

ggplot(subset(ns.annual.sum.by.species, 
              COMMON_NAME == "Cisco" |
                COMMON_NAME == "Bloater" |
                COMMON_NAME == "Lake Whitefish")) + 
  aes(x=YEAR, y = mean, fill=COMMON_NAME) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  geom_hline(data = (subset(fmean, 
                            COMMON_NAME == "Cisco" |
                            COMMON_NAME == "Bloater" |
                            COMMON_NAME == "Lake Whitefish")), 
             aes(yintercept=mean), linetype = "dashed") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Nearshore Coregonus Biomasss',
       subtitle='USGS bottom trawl assessment, 1978-2022') +
  facet_grid(COMMON_NAME ~., scales="free_y")

ggsave(here('Plots and Tables/RVCAT','ns_annual_biomass_cisco_bloater_lwf_facet.png'), dpi = 300, width = 40, height = 20, units = "cm") 


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
#pig <- ns.annual.sum.by.species %>%
#  subset(YEAR <= 2019)

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
  geom_hline(yintercept=mean(mean.ns.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) + 
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
  geom_hline(yintercept=mean(mean.ns.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
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
  geom_hline(yintercept=mean(mean.ns.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(ns.skew), ymin=2, ymax=20, xmax=65, xmin=3) + 
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
  geom_hline(yintercept=mean(mean.ns.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(ns.trawls.sum$KGHA)) +
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  annotation_custom(ggplotGrob(ns.skew), ymin=2, ymax=20, xmax=65, xmin=3) + 
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
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION), size =4)+
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
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=KGHA), size=10, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Biomass\n(kg per ha)') +
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  theme_bw() +
  map_theme+
  theme(axis.text=element_text(size=28, family='serif'),
        axis.title=element_text(size=28, family='serif'),
        legend.text=element_text(size=22, family='serif'),
        legend.title=element_text(size=22, family='serif'),
        plot.title=element_text(size=28, family='serif'),
        plot.subtitle=element_text(size=28, family='serif'),
        plot.caption=element_text(size=25, family='serif')
  ) +
  geom_text(aes(label=LOCATION), size =4)+
  labs(x='Longitude', y='Latitude')

ns.current.biomass.map

##inset above map into station biomass bar graph, with corresponding color scales
## Color gradient ars to match colors on map

ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  geom_hline(yintercept=mean(mean.ns.kgha.currentyear$KGHA)) +
  
##  geom_hline(yintercept=filter(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR)), mean(ns.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(ns.current.biomass.map), ymin=5, ymax=20, xmax=65, xmin=2)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  plot_theme +
  theme(axis.text=element_text(size=28, family='serif'),
        axis.title=element_text(size=28, family='serif'),
        legend.text=element_text(size=22, family='serif'),
        legend.title=element_text(size=22, family='serif'),
        plot.title=element_text(size=28, family='serif'),
        plot.subtitle=element_text(size=28, family='serif'),
        plot.caption=element_text(size=25, family='serif')
  ) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=28, family='serif')) + 
  labs(x='Station identifier', y='Biomass (kg per ha)',
       caption=ann_data_access,
       title='Lake Superior Fish Biomass at Nearshore Stations',
       subtitle='USGS bottom trawl assessment, 2022')  

  ##subtitle=(paste('USGS bottom trawl assessment,', max(ns.trawls.sum$YEAR))))  

ggsave(here('Plots and Tables/RVCAT','ns_current_sites_biomass_map2.png'), dpi = 300, width = 60, height = 30, units = "cm")


##create map with stations color coded by fishes
ggplot(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(ns.trawls.sum, YEAR == max(ns.trawls.sum$YEAR)), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=Fishes), size=6, stroke=1.5) +
  scale_color_gradient(low='deepskyblue2', high='red', name='Fish species') +
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
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

os.annual.sum.big3 <- os.annual.sum.by.species %>%
  subset(COMMON_NAME == "Kiyi" & YEAR != 2020 |
           COMMON_NAME == "siscowet Lake Trout"  & YEAR != 2020| 
           COMMON_NAME == "Deepwater Sculpin" & YEAR != 2020) %>%
  subset(YEAR !=2020) %>%
  group_by(COMMON_NAME) %>%
  summarise(mean=mean(mean)) %>%
  ungroup()


####Stackbar
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


##Standard plot with DWS, Kiyi, siscowet
ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "Kiyi" & YEAR != 2020 |
                       COMMON_NAME == "siscowet Lake Trout"  & YEAR != 2020 | 
                       COMMON_NAME == "Deepwater Sculpin" & YEAR != 2020)) +
       aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  geom_hline(data = os.annual.sum.big3, aes(yintercept=mean), size = 1.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  theme(axis.text.x=element_text(size=16, family='serif')) +
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Deepwater Sculpin, Kiyi, and siscowet Lake Trout Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  facet_grid(.~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_kiyi_DWS_siscowet_facet.png'), dpi = 300, width = 40, height = 20, units = "cm") 


##Standard plot with just DWS and Kiyi
ggplot(subset(os.annual.sum.by.species, COMMON_NAME == "Kiyi" & YEAR != 2020 |
                COMMON_NAME == "Deepwater Sculpin" & YEAR != 2020)) +
  aes(x=YEAR, y = mean) + 
  geom_bar(stat='identity', fill='grey75', color='black')+
  geom_errorbar(aes(x=YEAR, ymin=mean-std.error, ymax=mean+std.error),width=0.4) +
  geom_hline(data = subset(os.annual.sum.big3, COMMON_NAME == "Kiyi" |
                             COMMON_NAME == "Deepwater Sculpin"), 
             aes(yintercept=mean), size = 1.2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  plot_theme+
  theme(axis.text.x=element_text(size=16, family='serif')) +
  labs(x='Year', y='Mean biomass (kg per ha)',caption=ann_data_access,
       title='Lake Superior Offshore Deepwater Sculpin and Kiyi Biomass',
       subtitle='USGS bottom trawl assessment')+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
  facet_grid(.~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/RVCAT','os_annual_biomass_kiyi_DWS_facet.png'), dpi = 300, width = 40, height = 20, units = "cm") 

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
  geom_hline(yintercept=mean(mean.os.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(os.trawls.sum$KGHA)) + 
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
  geom_hline(yintercept=mean(mean.os.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
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
  geom_hline(yintercept=mean(mean.os.kgha.currentyear$Fishes)) +
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
  geom_hline(yintercept=mean(mean.os.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(os.skew), ymin=5, ymax=18, xmax=60, xmin=3) + 
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
  geom_hline(yintercept=mean(mean.os.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(os.skew), ymin=5, ymax=18, xmax=60, xmin=3) + 
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
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
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
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
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
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  theme_bw() +
  map_theme+
  theme(legend.position = 'none') + 
  geom_text(aes(label=LOCATION)) +
  labs(x='Longitude', y='Latitude')

os.current.biomass.map

##inset above map into station biomass bar graph, with corresponding color scales
## Color gradient ars to match colors on map
ggplot(subset(os.trawls.sum, YEAR == max(os.trawls.sum$YEAR))) + 
  aes(x=reorder(LOCATION, KGHA), y=KGHA, fill = KGHA) +
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red', name='Fish biomass\n(kg per ha)', guide=F)+
  geom_hline(yintercept=mean(mean.os.kgha.currentyear$KGHA)) +
  ##geom_hline(yintercept=mean(os.trawls.sum$KGHA)) +
  annotation_custom(ggplotGrob(os.current.biomass.map), ymin=6.5, ymax=19, xmax=28, xmin=2)+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=15, family='serif')) + 
  plot_theme +
  theme(legend.position = 'bottom') + 
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
##Nearshore and offshore sites typically sampled

ns.os.planned.means<-all.data %>% 
  select(OP_ID, YEAR, TARGET, LOCATION, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
         BEG_LATITUDE_DD, END_LATITUDE_DD, Mid.Lat.DD, Mid.Long.DD, 
         BEG_DEPTH, END_DEPTH,Surface.Temp, Bottom.Temp) %>%
  filter(TARGET==2|TARGET==117|TARGET==118|TARGET==106) %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  group_by(LOCATION) %>%
  summarise(last.sampled = max(YEAR), 
            mean.BEG_LONGITUDE_DD = mean(BEG_LONGITUDE_DD), 
            mean.END_LONGITUDE_DD = mean(END_LONGITUDE_DD), 
            mean.BEG_LATITUDE_DD = mean(BEG_LATITUDE_DD), 
            mean.BEG_LATITUDE_DD = mean(BEG_LATITUDE_DD), 
            mean.Mid.Lat.DD = mean(Mid.Lat.DD), 
            mean.Mid.Long.DD = mean(Mid.Long.DD), 
            mean.BEG_DEPTH = mean(BEG_DEPTH), 
            mean.END_DEPTH = mean(END_DEPTH),
            mean.Surface.Temp = mean(Surface.Temp),
            mean.Bottom.Temp = mean(Bottom.Temp),
  ) %>%
  ungroup() 

ns.os.planned<-all.data %>% 
  select(OP_ID,OP_DATE,YEAR,TARGET,LOCATION, M_UNIT, STATE) %>%
  filter(TARGET==2|TARGET==117|TARGET==118|TARGET==106) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore", "106" = "Chequamegon Bay"))) %>%
  mutate(Country = case_when(
    STATE == "MI" ~ 'USA', 
    STATE == "MN" ~ 'USA', 
    STATE == "WI" ~ 'USA', 
    STATE != "MI" | STATE != "MN" | STATE != "WI" ~ 'Canada')) %>%
  left_join(ns.os.planned.means) %>%
  mutate(Current = case_when(
    last.sampled >= 2018 ~ 'Current', 
    last.sampled <2018 ~ 'Extinct'))

#Map of all sites typically sampled in a non-CSMI year
ns.os.planned <- ns.os.planned %>%
  filter(survey == 'nearshore') %>%
  filter(Current == 'Current') 


ggplot(ns.os.planned, aes(mean.Mid.Long.DD, mean.Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(color='deepskyblue', size=6, stroke=1.5)+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior 2022 Nearshore Trawl Stations')

ggsave(here('Plots and Tables/RVCAT','Planned_USANearshoreSites.png'), dpi = 300, width = 40, height = 20, units = "cm")


ggplot(ns.os.planned, aes(mean.Mid.Long.DD, mean.Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(ns.os.planned, mapping=aes(mean.Mid.Long.DD, mean.Mid.Lat.DD, color=Country), size=6, stroke=1.5)+
  scale_color_manual(values=c('palegreen3', 'cadetblue2'), 
                     name='Country', labels=c('Canada','USA'))+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
         title='Lake Superior Nearshore Bottom Trawl Stations')

ggsave(here('Plots and Tables/RVCAT','Planned_NearshoreSites.png'), dpi = 300, width = 40, height = 20, units = "cm")


##subset out the current year, and nearshore and offshore targets (if you want Cheq Bay, can add that target code)
##Nearshore and offshore and CBay sites
all.sites.current<-all.data %>% 
  select(OP_ID, OP_DATE, YEAR, TARGET, LOCATION, STATE, Country, 
         BEG_LATITUDE_DD, END_LATITUDE_DD, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
         Mid.Lat.DD, Mid.Long.DD, 
         BEG_DEPTH, END_DEPTH, Mid.Depth, 
         Surface.Temp, Bottom.Temp) %>%
  filter(YEAR==max(YEAR)) %>% 
  filter(TARGET==2|TARGET==117|TARGET==118|TARGET==106) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  mutate(survey.csmi = case_when(
    TARGET == 2 ~ "Nearshore", 
    TARGET == 118 ~ "Offshore",
    TARGET == 117 & END_DEPTH <=84 ~ "CSMI",
    TARGET == 117 & END_DEPTH >84 ~ "CSMI & Offshore"),
    survey = case_when(
      TARGET == 2 ~ "Nearshore", 
      TARGET == 118 ~ "Offshore",
      TARGET == 117 & END_DEPTH <=84 ~ "No",
      TARGET == 117 & END_DEPTH >84 ~ "Offshore"),
    survey.shape = case_when(
      TARGET == 2 ~ "Nearshore", 
      TARGET == 118 ~ "Offshore",
      TARGET == 117 ~ "CSMI"))
 
#Map of all sites visited in the current year
ggplot(all.sites.current, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(all.sites.current, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color = survey), size=5, stroke=1.5) +
  scale_color_manual(values=c('palegreen3','salmon', 'cadetblue2'), name='Survey')+
  scale_y_continuous(breaks = pretty_breaks(), name='Latitude') +
  scale_x_continuous(breaks = pretty_breaks(), name='Longitude') +
  theme_bw() +
  map_theme+
  theme(legend.title.align = 0.5, 
        legend.position = c(.15, .8),
        legend.text=element_text(size=16, family='serif')) +
  geom_text(aes(label=LOCATION), size =3)+
  labs(caption=ann_data_access,
       title='Lake Superior Fish Community Survey Sampling Stations',
       subtitle=(paste('USGS bottom trawl assessments,', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_all_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


##Map of sites with Nearshore, Offshore, and CSMI indicated
img <- image_read(here('Data/NorthArrow.jpg'))
narrow <- rasterGrob(img)

star <- image_read(here('Data/BlackStar.png'))
mapstar <- rasterGrob(star)
                   


##Typical CSMI Nearshore Year
ggplot(all.sites.current, aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(all.sites.current, mapping=aes(Mid.Long.DD, Mid.Lat.DD, shape = survey.csmi, fill = survey.csmi), 
             size=5, stroke=1.5)+
  scale_y_continuous(breaks = pretty_breaks(), name='Latitude') +
  scale_x_continuous(breaks = pretty_breaks(), name='Longitude') +
  scale_fill_manual(name="Survey", 
                     labels = c("CSMI", "CSMI & Offshore", "Nearshore"), 
                     values=c("plum1", "purple", "skyblue")) + 
  scale_shape_manual(name="Survey", 
                     labels = c("CSMI", "CSMI & Offshore", "Nearshore"), 
                     values = c(24, 24, 21)) + 
  theme_bw() +
  map_theme +
  annotation_custom(grob = narrow, xmin = -85.6, xmax = -84.45, ymin = 48.5, ymax = 49) +
  theme(legend.title.align = 0.5, 
        legend.position = c(.15, .82)) +
  labs(caption=ann_data_access,
       title='Lake Superior Fish Community Assessments',
       subtitle=(paste('USGS trawling and hydroacoustic sampling locations in', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_NearOffCSMI_sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


###Near and offshore sites, no shallow CSMI sites
ggplot(subset(all.sites.current, survey != "No"), aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(subset(all.sites.current, survey != "No"), mapping=aes(Mid.Long.DD, Mid.Lat.DD, shape = survey, fill = survey), 
              size=6, stroke=1.5)+
  scale_y_continuous(breaks = pretty_breaks(), name='Latitude') +
  scale_x_continuous(breaks = pretty_breaks(), name='Longitude') +
  scale_fill_manual(name="Survey", 
                    labels = c("Nearshore", "Offshore"), 
                    values=c("skyblue", "purple")) + 
  scale_shape_manual(name="Survey", 
                     labels = c("Nearshore", "Offshore"), 
                     values = c(21, 21)) + 
  theme_bw() +
  map_theme +
  annotation_custom(grob = narrow, xmin = -85.6, xmax = -84.45, ymin = 48.5, ymax = 49) +
  theme(legend.title.align = 0.5, 
        legend.position = c(.15, .82)) +
  labs(caption=ann_data_access,
       title='Lake Superior Fish Community Assessments',
       subtitle=(paste('U.S. Geological Survey nearshore and offshore sampling locations in', all.sites.current$YEAR)))

ggsave(here('Plots and Tables/RVCAT','CurrentYear_NearOff_Sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


###Near and offshore sites same symbol, Ashland starred on map
ggplot(subset(all.data, YEAR == 2016), aes(Mid.Long.DD, Mid.Lat.DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(subset(all.sites.current, survey != "No"), mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
              shape = 21, fill = 'grey70', color = 'black', size=6, stroke=1.5) +
  scale_y_continuous(breaks = pretty_breaks(), name='Latitude') +
  scale_x_continuous(breaks = pretty_breaks(), name='Longitude') +
  theme_bw() +
  map_theme +
  annotation_custom(grob = narrow, xmin = -85.6, xmax = -84.45, ymin = 48.5, ymax = 49) +
  annotation_custom(grob = mapstar, xmin = -90.5, xmax = -91.1, ymin = 46.5, ymax = 46.7) + 
  annotate("text", label = "Ashland", x = -90.8, y = 46.45, size = 8, colour = "black", family = "serif") +
  theme(legend.position = "none") +
  labs(title='U.S. Geological Survey Lake Superior Annual Fish, Zooplankton, and Limnological Sampling Locations',
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','NearOffSites.png'), dpi = 300, width = 40, height = 20, units = "cm")



#Nearshore and offshore trawl paths
ggplot(subset(all.sites.current, survey == "nearshore" | survey == "offshore")) +
  aes(Mid.Long.DD, Mid.Lat.DD) + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_segment(aes(x=BEG_LONGITUDE_DD, xend=END_LONGITUDE_DD, 
              y=BEG_LATITUDE_DD, yend=END_LATITUDE_DD, color=survey), size=2)+
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude',breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  scale_color_manual(values=c('palegreen3','salmon', 'cadetblue2'), name='Survey')+
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
#  geom_point(aes(Mid.Long.DD, Mid.Lat.DD), shape = 21, size=10, stroke=1.5)+
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
  subset(TARGET == 2 | TARGET == 118 | TARGET == 117 ) %>%
  subset(YEAR == 2016) 

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


##LS management units with sampling locations for a typical CSMI year
all.sites.current<-all.data %>% 
  select(OP_ID,OP_DATE,YEAR,TARGET,LOCATION,  STATE, Country, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
         BEG_LATITUDE_DD, END_LATITUDE_DD, Mid.Lat.DD, Mid.Long.DD, 
         BEG_DEPTH, END_DEPTH,Surface.Temp, Bottom.Temp) %>%
  filter(YEAR==2016) %>% 
  filter(TARGET==2 | TARGET == 118 | TARGET == 117) %>%
#  filter(TARGET==2 | TARGET == 118 | TARGET == 117 | TARGET == 106) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore", "106" = "Chequamegon Bay")))


ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "ls_lktrout_mgmt_units_proj_new_ONT")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- tidy(ls_poly)

ggplot(subset(all.sites.current, survey == "nearshore" | survey == "offshore")) +
  aes(Mid.Long.DD, Mid.Lat.DD) + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(aes(Mid.Long.DD, Mid.Lat.DD, color=survey), size=6, stroke=1.5)+
  geom_polygon(data=ls_poly.fort, aes(long, lat, group = group), colour = "black", fill = NA, size = 0.5)+
#  geom_text(data=ls_poly.fort$M_units, aes(label=ls_poly.fort$id)) +
  scale_y_continuous(breaks = pretty_breaks(), name='Latitude') +
  scale_x_continuous(breaks = pretty_breaks(), name='Longitude' )+
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Fish Collection Locations',
       #subtitle=(paste('USGS bottom trawl assessment,', all.sites.current$YEAR)))
       subtitle='USGS bottom trawl assessments')
       
ggsave(here('Plots and Tables/RVCAT','LSBS.PlannedSites.byMunits.png'), dpi = 300, width = 40, height = 20, units = "cm")


##LS management units with sampling locations for an upcoming survey year
all.sites.current<-all.data %>% 
  select(OP_ID,OP_DATE,YEAR,TARGET,LOCATION,  STATE, Country, BEG_LONGITUDE_DD, END_LONGITUDE_DD, 
         BEG_LATITUDE_DD, END_LATITUDE_DD, Mid.Lat.DD, Mid.Long.DD, 
         BEG_DEPTH, END_DEPTH,Surface.Temp, Bottom.Temp) %>%
  filter(YEAR== 2016) %>% 
##  filter(YEAR==max(YEAR)) %>% 
  filter(TARGET==2|TARGET==117|TARGET==118) %>%
##  filter(TARGET==2|TARGET==117|TARGET==118|TARGET==106) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%

  mutate(survey = str_replace_all(TARGET, c("2" = "nearshore", "117" = "offshore", 
                                            "118" = "offshore", "106" = "Chequamegon Bay")))

library(gpclib)

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "ls_lktrout_mgmt_units_proj_new_ONT")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- tidy(ls_poly, region = "MAN_DIST")
lapply(ls_poly.fort, class)
mnames <- aggregate(cbind(long, lat) ~ id, data = ls_poly.fort, FUN=mean) 


ggplot(subset(all.sites.current, survey == "nearshore" | survey == "offshore")) +
  aes(Mid.Long.DD, Mid.Lat.DD) + 
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  #  geom_point(aes(Mid.Long.DD, Mid.Lat.DD, color=survey), size=6, stroke=1.5)+
  geom_polygon(data=ls_poly.fort, aes(long, lat, group = group), colour = "black", fill = NA, size = 0.5)+
  geom_point(aes(Mid.Long.DD, Mid.Lat.DD), shape =21, colour = "black", fill = "white", size=8, stroke=1.5)+
  #  geom_text(data = mnames, aes(x = long, y = lat, label = id), size = 4) +
  scale_y_continuous(breaks = pretty_breaks(), name='Latitude') +
  scale_x_continuous(breaks = pretty_breaks(), name='Longitude' )+
  theme_bw() +
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(caption=ann_data_access,
       title='Lake Superior Stations Sampled',
       subtitle='USGS bottom trawl assessment, 2022')
#subtitle=(paste('USGS bottom trawl assessment,', all.sites.current$YEAR+1)))

ggsave(here('Plots and Tables/RVCAT','LSBS_Munits_SitestoSample.png'), dpi = 300, width = 40, height = 20, units = "cm")


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
           COMMON_NAME == 'Rainbow Smelt' |
           COMMON_NAME == 'Longnose Sucker' |
           COMMON_NAME == 'lean Lake Trout' |
           COMMON_NAME == 'siscowet Lake Trout') %>%
  subset(Mid.Lat.DD >0)


###################################################################
##Cisco
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Cisco')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Cisco'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Cisco Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_Cisco_biomass.gif'))


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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Cisco_biomass_map_bars.gif'))


####################################################################
##Bloater
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Bloater')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Bloater'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
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

anim_save(here('Plots and Tables/RVCAT','Animated_Bloater_biomass.gif'))


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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Bloater_biomass_map_bars.gif'))


####################################################################
##Lake Whitefish
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Lake Whitefish')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Lake Whitefish'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
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

anim_save(here('Plots and Tables/RVCAT','Animated_LWF_biomass.gif'))


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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_LWF_biomass_map_bars.gif'))


####################################################################
##Pygmy Whitefish
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Pygmy Whitefish')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Pygmy Whitefish'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
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

anim_save(here('Plots and Tables/RVCAT','Animated_PWF_biomass.gif'))


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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_PWF_biomass_map_bars.gif'))


####################################################################
##Rainbow Smelt
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Rainbow Smelt')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Rainbow Smelt'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Rainbow Smelt Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_RBS_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Rainbow Smelt')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_RBS_biomass_map_bars.gif'))


####################################################################
##Longnose Sucker
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Longnose Sucker')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'Longnose Sucker'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Longnose Sucker Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_LongnoseSucker_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Longnose Sucker')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_LongnoseSucker_biomass_map_bars.gif'))


####################################################################
##Lean Lake Trout
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'lean Lake Trout')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(ns.os.species.maps, COMMON_NAME == 'lean Lake Trout'),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior lean Lake Trout Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_leanLT_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'lean Lake Trout')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_leanLT_biomass_map_bars.gif'))

####################################################################
##siscowet Lake Trout
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'siscowet Lake Trout')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(subset(ns.os.species.maps, COMMON_NAME == 'siscowet Lake Trout'),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior siscowet Lake Trout Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_siscowetLT_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'siscowet Lake Trout')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_siscowetLT_biomass_map_bars.gif'))


####################################################################
##Slimy Sculpin
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Slimy Sculpin')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(subset(ns.os.species.maps, COMMON_NAME == 'Slimy Sculpin'),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Slimy Sculpin Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_SlimySculpin_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Slimy Sculpin')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_SlimySculpin_biomass_map_bars.gif'))


####################################################################
##Spoonhead Sculpin
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Spoonhead Sculpin')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(subset(ns.os.species.maps, COMMON_NAME == 'Spoonhead Sculpin'),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Spoonhead Sculpin Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_SpoonheadSculpin_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Spoonhead Sculpin')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_SpoonheadSculpin_biomass_map_bars.gif'))


####################################################################
##Deepwater Sculpin
sp_map1<-ggplot(subset(ns.os.species.maps,
                       COMMON_NAME == 'Deepwater Sculpin')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude')+
  scale_x_continuous(name='Longitude')+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_jitter(subset(ns.os.species.maps, COMMON_NAME == 'Deepwater Sculpin'),
              mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=KGHA, color=KGHA), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Biomass (kg per ha)')), 
                        breaks = pretty_breaks(n=8)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Biomass (kg per ha)')), 
                         breaks = pretty_breaks(n=8)) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal')+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Deepwater Sculpin Biomass',subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)+
  transition_manual(YEAR)

sp_map1<-sp_map1+geom_text(aes(x=-85, y=48.75, label=paste('Year:',YEAR, sep='\n')), 
                           size=8, family='serif')+
  transition_manual(YEAR)

sp_map1_gif<-animate(sp_map1, fps = 3, end_pause = 10, width = 1000, 
                     height = 500,renderer = gifski_renderer(loop=F))
sp_map1_gif

anim_save(here('Plots and Tables/RVCAT','Animated_DeepwaterSculpin_biomass.gif'))


sp_bars1<-ggplot(subset(ns.annual.sum.by.species,
                        COMMON_NAME == 'Deepwater Sculpin')) +
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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_DeepwaterSculpin_biomass_map_bars.gif'))

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
  left_join(cbay.trawls) %>%
  left_join(sci.names) 

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
                      size=log(NOHA), color=log(NOHA)), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Log(number per ha)')), 
                        breaks = pretty_breaks(n=6)) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Log(number per ha)')), 
                         breaks = pretty_breaks(n=6)) +
  map_theme+
  theme(legend.box='horizontal', 
        legend.position = c(0.15, .75))+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
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
anim_save(here('Plots and Tables/RVCAT','Animated_Cbay_abundance_map.gif'))


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
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Cbay_abundance_map_bars.gif'))


############################################################################################
##C-Bay single species annimated plots
##Rainbow Smelt
cbay_map1<-ggplot(subset(cbay.trawls.all.species,
                       COMMON_NAME == 'Rainbow Smelt')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), 
                     labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(cbay.trawls.all.species, COMMON_NAME == 'Rainbow Smelt'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=log(NOHA), color=log(NOHA)), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Log(number per ha)')), 
                        breaks = pretty_breaks()) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Log(number per ha)')), 
                         breaks = pretty_breaks()) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal', 
        legend.position = c(0.15, .75))+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Chequamegon Bay Rainbow Smelt Abundance',
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

anim_save(here('Plots and Tables/RVCAT','Animated_Cbay_RBS_abundance.gif'))


cbay_bars1<-ggplot(subset(cbay.annual.sum.by.species,
                        COMMON_NAME == 'Rainbow Smelt')) +
  aes(x=YEAR, y=mean, fill=mean)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean abundance (number per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0), breaks = pretty_breaks(), labels = scales::comma) +
  transition_manual(YEAR, cumulative=T)

cbay_bars1_gif<-animate(cbay_bars1, fps = 3, end_pause = 10, width = 350, 
                      height = 500,renderer = gifski_renderer(loop=F))
cbay_bars1

p_mgif<-image_read(cbay_map1_gif)
q_mgif<-image_read(cbay_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))

for(i in 2:48){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Cbay_RBS_abundance_map_bars.gif'))

#####################################################################################
##Yellow Perch
cbay_map1<-ggplot(subset(cbay.trawls.all.species,
                         COMMON_NAME == 'Yellow Perch')) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), 
                     labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(cbay.trawls.all.species, COMMON_NAME == 'Yellow Perch'),
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, size=log(NOHA), color=log(NOHA)), stroke=1.5)+
  scale_size_continuous(name=expression(underline('Log(number per ha)')), 
                        breaks = pretty_breaks()) +
  scale_color_continuous(high='red',low='deepskyblue2',
                         name=expression(underline('Log(number per ha)')), 
                         breaks = pretty_breaks()) +
  theme_bw() +
  map_theme +
  theme(legend.box='horizontal', 
        legend.position = c(0.15, .75))+
  guides(size=guide_legend(reverse=T, keywidth=5), color=guide_legend(reverse=T, keywidth=5))+
  labs(title='Lake Superior Chequamegon Bay Yellow Perch Abundance',
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

anim_save(here('Plots and Tables/RVCAT','Animated_Cbay_YPerch_abundance.gif'))


cbay_bars1<-ggplot(subset(cbay.trawls.all.species,
                          COMMON_NAME == 'Yellow Perch')) +
  aes(x=YEAR, y=NOHA, fill=NOHA)+
  geom_bar(stat='identity')+
  scale_fill_gradient(low='deepskyblue2', high='red')+
  geom_point(size=3, color='black', shape=18)+
  plot_theme +
  theme(legend.position = "none") +
  labs(x='Year', y='Mean abundance (number per ha)',
       title=' ', caption='  ', subtitle='  ')+
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(expand=c(0,0))+
  transition_manual(YEAR, cumulative=T)

cbay_bars1_gif<-animate(cbay_bars1, fps = 3, end_pause = 10, width = 350, 
                        height = 500,renderer = gifski_renderer(loop=F))
cbay_bars1

p_mgif<-image_read(cbay_map1_gif)
q_mgif<-image_read(cbay_bars1_gif)

new_gif<-image_append(c(p_mgif[1], q_mgif[1]))

for(i in 2:48){ ##NOTE: as more years are added, need to increase the # frames to the # years (ie, if 50 years-> i in 2:50)
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}
new_gif
image_write(new_gif, here('Plots and Tables/RVCAT','Animated_Cbay_YPerch_abundance_map_bars.gif'))

#####Age-1 analyses#########################################################################
############################################################################################
############################################################################################

##Lengths file to evaluate Age-1 densities##################################################
##pull out nearshore, TARGET = 2 data for a normal full survey year

ns<-subset(all.data, TARGET==2 & YEAR >1977)

##ns<-subset(all.data, TARGET==2 & YEAR >1977 & Country == "USA")

lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv')) %>%
  select(OP_ID,SPECIES,LENGTH,EXP_N) 


###Expanded age-1 length cutoffs to deal with faster observed growth in 2021
#age1.ns <- ns.trawls %>%
#  select(OP_ID) %>% 
#  left_join(lengths) %>% 
#  subset(SPECIES == '109' & LENGTH <101 |
#           SPECIES == '202' & LENGTH <141 |
#           SPECIES == '204' & LENGTH <131 |
#           SPECIES == '203' & LENGTH <161 |
#           SPECIES == '317' & LENGTH <226) %>%
#  select(OP_ID,SPECIES,LENGTH,EXP_N) %>%
#  full_join(ns.trawls) %>% 
#  select(OP_ID,SPECIES,LENGTH,EXP_N) %>%
#  group_by(OP_ID,SPECIES) %>%
#  summarise(NUM = sum(EXP_N)) %>%
#  ungroup() 


###Traditional age-1 length cutoffs
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
  summarise(NUM = sum(EXP_N)) %>%
  ungroup() 


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
  summarise(NUM = sum(EXP_N)) %>%
  ungroup()

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
  group_by(YEAR, survey, COMMON_NAME) %>% 
  summarise(age1.mean = mean(NOHA), sites = n()) %>%
  select(YEAR, survey, sites, COMMON_NAME, age1.mean) %>%
  ungroup()


#####################################################################################################
##Create age-1 summary table for export
age1.table<-age1.annual.sum %>%
  pivot_wider(names_from = COMMON_NAME, values_from = age1.mean) %>%
  mutate('Year class' = YEAR-1) %>%
  mutate(across(c(
              'Bloater', 
              'Cisco',
              'Lake Whitefish',
              'Rainbow Smelt',
              'lean Lake Trout',
              'Kiyi',
              'siscowet Lake Trout'),
              round, 2))

openxlsx::write.xlsx(age1.table, here('Plots and Tables/RVCAT','export_age1_annual_summary.xlsx'))

openxlsx::write.xlsx(age1.table, here('Data','export_age1_annual_summary.xlsx'))


#################################################################################################
##Age -1 plots

#################################################################################################
age1.annual.sum.2020 <- age1.annual.sum %>%
  subset(YEAR<2020)

##Plot of age-1 Bloater, Cisco, Kiyi
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


##Plot of age-1 Bloater, Cisco, Lake whitefish
ggplot(subset(age1.annual.sum, COMMON_NAME=='Bloater'|COMMON_NAME=='Cisco'|COMMON_NAME=='Lake Whitefish')) + 
  aes(x=YEAR-1, y = age1.mean) + 
  geom_point(size=6)+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  facet_grid(COMMON_NAME~., scales='free_y')+
  plot_theme+
  theme(axis.text=element_text(size=20, family='serif'),
          axis.title=element_text(size=20, family='serif'),
        strip.text=element_text(size=20, family='serif')
        ) +
  labs(x='Year Class', y='Lakewide mean abundance (fish per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Bloater, Cisco, and Lake Whitefish Abundance',
       subtitle='USGS bottom trawl assessment')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  theme(panel.spacing=unit(2,'lines'))+
  coord_cartesian(clip='off')+
  geom_hline(yintercept=0, color='black', size=1)

ggsave(here('Plots and Tables/RVCAT','ns_os_Age1_ciscoes_LWF.png'), dpi = 300, width = 40, height = 30, units = "cm")

##Plot of age-1 Cisco
ggplot(subset(age1.annual.sum, COMMON_NAME == "Cisco")) + 
  aes(x=YEAR-1, y = age1.mean) + 
  geom_hline(yintercept=0, size = 1, colour = 'black') +
#  geom_hline(yintercept=10, size = 1.5, colour = 'sienna2') +
  geom_point(shape = 21, size=6, fill = 'gray35')+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  #geom_hline(yintercept=100, size = 1.5, colour = 'sienna2') +
  scale_x_continuous(breaks = pretty_breaks())+
#  scale_y_continuous(trans = 'log10', breaks = pretty_breaks(5), 
#                     expand = expansion(mult = c(0.05, .1)))+
#  scale_y_continuous(trans = 'log10', breaks = pretty_breaks(4)) +
 # scale_y_log10() +
  plot_theme+
#  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
#  theme(legend.box='horizontal',
#        legend.position = c(0.8, 0.8),
#        legend.title.align=0.5) +
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
     title='Lake Superior Age-1 Cisco Abundance',
     subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','ns_Age1_cisco.png'), dpi = 300, width = 40, height = 20, units = "cm")


#################################################################################################
#################################################################################################
##Plot of age-1 Bloater

ggplot(subset(age1.annual.sum, COMMON_NAME == "Bloater")) + 
  aes(x=YEAR-1, y = age1.mean) + 
  geom_point(shape = 21, size=6, fill = 'gray35')+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
#  geom_hline(yintercept=10, size = 1.5, colour = 'sienna2') +
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  plot_theme+
#  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
#  theme(legend.box='horizontal',
#        legend.position = c(0.8, 0.8), 
#        legend.title.align=0.5) +
  labs(x='Year Class', y='Lakewide mean abundance (n per ha)', caption=ann_data_access,
       title='Lake Superior Age-1 Bloater Abundance',
       subtitle='USGS bottom trawl assessment')

ggsave(here('Plots and Tables/RVCAT','ns_age1_bloater.png'), dpi = 300, width = 40, height = 20, units = "cm")

#################################################################################################
##Plot of age-1 Lake Whitefish

ggplot(subset(age1.annual.sum, COMMON_NAME == "Lake Whitefish")) + 
  aes(x=YEAR-1, y = age1.mean) + 
#  geom_hline(yintercept=4.5, size = 1.5, colour = 'sienna2') +
  geom_point(shape = 21, size=6, fill = 'gray35')+
  geom_segment(aes(x=YEAR-1, xend=YEAR-1, y=0, yend=age1.mean), size=1, color='black')+
  scale_x_continuous(breaks = pretty_breaks())+
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  plot_theme+
  scale_fill_manual(name='FCO\nMetric Met?', values=c('deeppink1','deepskyblue2'))+
#  theme(legend.box='horizontal',
#        legend.position = c(0.8, 0.8), 
#        legend.title.align=0.5) +
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
       title='Lake Superior Juvenile (<age-3) Lake Trout Abundance',
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
##Presence - Absence of age-1 cisco, bloater, kiyi

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

##create map with stations colored by age-1 bloater, cisco, kiyi presence - absence
age1.ciscoes.pa <- age1 %>%
  select(OP_ID, OP_DATE, YEAR, LOCATION, Mid.Lat.DD, Mid.Long.DD, SPECIES, COMMON_NAME, NUM, NOHA) %>%
  subset(COMMON_NAME=='Cisco' | 
           COMMON_NAME=='Bloater' |
           COMMON_NAME=='Kiyi') %>% 
  group_by(OP_ID) %>%
  summarise(ciscoes = sum(NUM)) %>%
  mutate(present = ifelse(ciscoes > 0, "Present", "Absent")) %>%
  left_join(age1) %>%
  select(OP_ID, YEAR, LOCATION, Mid.Lat.DD, Mid.Long.DD, ciscoes, present) %>%
  mutate(station = as.factor(LOCATION)) %>%
  distinct(OP_ID, .keep_all = TRUE) %>%
  ungroup()
  
ggplot(subset(age1.ciscoes.pa, YEAR == max(age1.ciscoes.pa$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(subset(age1.ciscoes.pa, YEAR == max(age1.ciscoes.pa$YEAR)), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=present), size=6, stroke=1.5)+
  scale_color_manual(values=c('gray50','red'), name='Age-1 occurrence')+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-1 Bloater, Cisco, and Kiyi Occurrence',
       subtitle=(paste('USGS bottom trawl assessment,', max(age1.ciscoes.pa$YEAR))), 
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','ns_age1_sites_present_absent_map.png'), 
       dpi = 300, width = 40, height = 20, units = "cm")



####
a1.map.inset<-ggplot(subset(age1.ciscoes.pa, YEAR == max(age1.ciscoes.pa$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(subset(age1.ciscoes.pa, YEAR == max(age1.ciscoes.pa$YEAR)), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=present), size=6, stroke=1.5) +
  scale_color_manual(values=c('gray50','red'), name='Age-1 occurrence') +
  theme_bw() +
  map_theme +
  geom_text(aes(label=LOCATION))


#########################################################################################################
##Pie map of age-1 bloater, cisco, Lake Whitefish and kiyi  #########
#########################################################################################################
#################################################################################################
age1.pie <- age1 %>%
  select(OP_ID,YEAR,LOCATION,COMMON_NAME,NOHA,Mid.Lat.DD, Mid.Long.DD) %>%
  pivot_wider(names_from = COMMON_NAME, values_from = NOHA, values_fill = 0) %>%
  subset(Mid.Lat.DD >0) %>%
  subset

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


##Age-1 pie map for bloater, cisco, kiyi for current year
ggplot(subset(age1.pie, YEAR == max(age1.pie$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  geom_point(all.sites.current, mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
             shape=21, fill='white', colour='black', size=3, stroke=1.5, show.legend = FALSE)+
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(age1.pie, YEAR == max(age1.pie$YEAR)),
                  cols= c("Bloater", "Cisco", "Kiyi", 'Lake Whitefish'),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'deepskyblue2', 'lightpink3'))+
#  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85)) +
  labs(x='Longitude', y='Latitude',
        title='Lake Superior Age-1 Bloater, Cisco, Kiyi, and Lake Whitefish Occurrence',
        subtitle=(paste('USGS bottom trawl assessment,', max(age1.pie$YEAR))), 
        caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','CurrentYear_age1_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')


##Age-1 pie map for bloater, cisco, Lake Whitefish for current year
a1b.map.inset<-ggplot(subset(age1.pie, YEAR == max(age1.pie$YEAR))) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  geom_point(all.sites.current, mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
             shape=21, fill='white', colour='black', size=3, stroke=1.5, show.legend = FALSE)+
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(age1.pie, YEAR == max(age1.pie$YEAR)),
                  cols= c("Bloater", "Cisco", 'Lake Whitefish'),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3'))+
#    scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude',
       title='Lake Superior Age-1 Bloater, Cisco, and Kiyi Distributions',
       subtitle=(paste('USGS bottom trawl assessment,', max(age1.pie$YEAR))), 
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','CurrentYear_age1_piemap.png'), 
       height=20, width=40, dpi=300, units='cm')


##Age-1 pie map for bloater, cisco, Lake Whitefish for current year for inset into stacked bar plot below
a1c.map.inset<-ggplot(subset(age1.pie, 
                             YEAR == max(age1.pie$YEAR) 
                                         & age1.pie$LOCATION <500)) +
  aes(Mid.Long.DD, Mid.Lat.DD) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  scale_x_continuous(breaks = pretty_breaks(), name="Longitude") +
  scale_y_continuous(breaks = pretty_breaks(), name="Latitude")+
  geom_point(subset(age1.pie, 
                    YEAR == max(age1.pie$YEAR) & age1.pie$LOCATION <500), 
                    mapping=aes(Mid.Long.DD, Mid.Lat.DD), 
                    shape=21, fill='white', colour='black', size=4, stroke=1.5, show.legend = FALSE)+
  geom_scatterpie(aes(x=Mid.Long.DD, y=Mid.Lat.DD), 
                  data=subset(age1.pie, YEAR == max(age1.pie$YEAR) & age1.pie$LOCATION <500),
                  cols= c("Bloater", "Cisco", 'Lake Whitefish'),
                  pie_scale = 0.75) +
  theme_bw() +
  map_theme +
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3', 'steelblue'))+
  #    scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.15, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(x='Longitude', y='Latitude')

ggsave(here('Plots and Tables/RVCAT','CurrentYear_age1_piemapc.png'), 
       height=20, width=40, dpi=300, units='cm')


####stacked bar plot for age-1 bloater, ciscoe, Lake Whitefish
ggplot(subset(age1, 
              COMMON_NAME=='Cisco' & YEAR == max(age1$YEAR) |
                COMMON_NAME=='Bloater' & YEAR == max(age1$YEAR) |
              #  COMMON_NAME=='Kiyi' & YEAR == max(age1$YEAR) |
                COMMON_NAME=='Lake Whitefish' & YEAR == max(age1$YEAR) 
              )) +
  aes(x=reorder(LOCATION, NOHA), y = NOHA, fill=COMMON_NAME) + 
  #  aes(x=LOCATION, y = NOHA, fill=COMMON_NAME) + 
  geom_bar(stat='identity', position='stack')+
  ##  aes(x=fct_reorder(station))+
  annotation_custom(ggplotGrob(a1c.map.inset), xmin=1, xmax=60,  ymin=10, ymax=100)+
  plot_theme+
  theme(legend.position='bottom',
        axis.title=element_text(size=22),
        axis.text.y=element_text(size=22),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=22))+
  labs(x='Station', 
       y='Abundance (number per ha)',
       title='Lake Superior Age-1 Bloater, Cisco, and Lake Whitefish Abundance',
       subtitle='USGS bottom trawl assessment', 

      ## subtitle=(paste('USGS bottom trawl assessment,', max(age1$YEAR))), 
       caption=ann_data_access) +
  scale_y_continuous(breaks = pretty_breaks(5), 
                     expand = expansion(mult = c(0, .1)))+
  scale_fill_manual( name=' ', values=c('palegreen3','sienna1', 'lightpink3', 'steelblue'))+
  theme(legend.position = "none")
#  theme(legend.position = c(0.8,0.8))

ggsave(here('Plots and Tables/RVCAT','ns_age1_bloater_cisco_LWF_bySite.png'), height=20, width=40, dpi=300, units='cm')

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
       title='Lake Superior Age-1 Bloater, Cisco, and Kiyi Distributions',
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
##lollipop last 5, last 10, 10-20 years ago, & 20-30 years ago means comparisons#########################################

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
  mutate(period = "Last 5 years")

ns.spp10mean<-ns.annual.sum.by.species %>%
  subset(YEAR < (max(YEAR)-9)) %>% 
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
  mutate(period = "Last 10 years")

ns.spp20mean<-ns.annual.sum.by.species %>%
  subset(YEAR < (max(YEAR)-10) & YEAR > (max(YEAR)-19)) %>%
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
  mutate(period = "10-20 years ago")

ns.spp30mean<-ns.annual.sum.by.species %>%
  subset(YEAR < (max(YEAR)-20) & YEAR > (max(YEAR)-29)) %>%
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
  mutate(period = "20-30 years ago")

ns.period.means<-bind_rows(ns.spp5mean, ns.spp10mean, ns.spp20mean, ns.spp30mean)


##calculate the mean for the last two years of sampling that will be used to compare to the 5, 10, and 20 year means
ns.spp2mean<-ns.annual.sum.by.species %>%
#  subset(YEAR>=max(YEAR)-2) %>% 
  subset(YEAR>=max(YEAR)) %>% 
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
  mutate(period_f=factor(period, levels=c('20-30 years ago','10-20 years ago','Last 10 years','Last 5 years')))
#mutate(period_f=factor(period, levels=c('20 year mean','10 year mean','5 year mean')))


##plot
ggplot(ns.period.means, aes(x=percentchange, y=COMMON_NAME, fill=direction))+
  geom_segment(aes(x=0, xend=percentchange, y=COMMON_NAME, yend=COMMON_NAME))+
  geom_point(size=8, shape=21)+
  scale_fill_manual(values=c('red', 'deepskyblue2'), guide="none")+
  scale_y_discrete(limits=c('Longnose Sucker', 'Ninespine Stickleback','Trout-Perch',
                            'Spoonhead Sculpin','Slimy Sculpin','Deepwater Sculpin',
                            'Burbot', 'Rainbow Smelt',
                            'Pygmy Whitefish','Lake Whitefish','Bloater','Cisco'))+
  plot_theme+
  facet_grid(.~period_f)+
  geom_vline(xintercept=0, size=2, color='black')+
  labs(x='Percent change in 2022 mean biomass (kg per ha) as compared to previous time periods', y='', 
       title='Lake Superior Nearshore Fish Biomass Trends',
       subtitle='USGS bottom trawl assessment',
       caption=ann_data_access)

ggsave(here('Plots and Tables/RVCAT','ns_periods_comparison.png'), dpi = 300, width = 40, height = 20, units = "cm")


########################################################################################
##Animated map of sites sampled in the current year and corresponding fish biomass and diversity#####################################
########################################################################################

animate.map.catch <- all.data%>%
##  subset(YEAR >=2009) %>%
  subset(YEAR==max(YEAR)) %>%
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
##       subtitle='USGS bottom trawl assessment, 2009-2022', 
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
#  geom_text(aes(y=mean.kgha, label = Value_lbl), size = 5, hjust=0) +
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


plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=24, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=20, family='serif'),
                  plot.caption=element_text(size=20, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  legend.title=element_text(size=24, family='serif'),
                  strip.text=element_text(size=20, family='serif'))


###########################################################################################  
##Single Species Depth plot for bottom trawl

ggplot(subset(dfishy1, FISH == "Kiyi" & trawl == "Bottom trawl")) +
  aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp) + 
  geom_density(alpha=0.4, fill = 'deepskyblue2') +
  geom_vline(data = subset(dfishy1.sum, FISH == "Kiyi" & trawl == "Bottom trawl"),
             aes(xintercept=bdepth.mean), color='black', size=1.5, linetype=3, show.legend = FALSE) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.position = "none") +
#  scale_x_continuous(limits = c(0, 330)) +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  labs( x='Bathymetric depth (m)', y='Proportional density',
       # title='Lake Superior Kiyi Depth Distribution',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=8, family='serif') +
  annotate(geom="text", x=385, y=0.12, label="atop(Maximum, depth)", 
           size=8, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','kiyi_BottomTrawlCaptures.png'), height=16, width=35, dpi=300, units='cm')

ggplot(subset(dfishy1, FISH == "Bloater" & trawl == "Bottom trawl")) +
  aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp) + 
  geom_density(alpha=0.4, fill = 'deepskyblue2') +
  geom_vline(data = subset(dfishy1.sum, FISH == "Bloater" & trawl == "Bottom trawl"),
             aes(xintercept=bdepth.mean), color='black', size=1.2, show.legend = FALSE) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  labs( x='Fish bathymetric depth (m)', y='Relative density',
        title='Lake Superior Bloater Depth Distribution',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','bloater_BottomTrawlCaptures.png'), height=20, width=40, dpi=300, units='cm')

##Rainbow Smelt
ggplot(subset(dfishy1, FISH == "Rainbow Smelt" & trawl == "Bottom trawl")) +
  aes(x=avgdepth, y = ..scaled.., weight=Fdensity/Fdensity.grp) + 
  geom_density(alpha=0.4, fill = 'deepskyblue2') +
  geom_vline(data = subset(dfishy1.sum, FISH == "Rainbow Smelt" & trawl == "Bottom trawl"),
             aes(xintercept=bdepth.mean), color='black', size=1.2, show.legend = FALSE) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.position = "none") +
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  labs( x='Fish bathymetric depth (m)', y='Relative density',
        title='Lake Superior Rainbow Smelt Distribution',
        subtitle='USGS Bottom and Mid-water Trawling, 2011-2022',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=6, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=6, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','RBS_BathyDepths.png'), height=20, width=40, dpi=300, units='cm')

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
        title='Lake Superior Coregonines and Rainbow Smelt Depth Distributions',
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

ggsave(here('Plots and Tables/RVCAT','kiyi_BottomTrawlTemperature.png'), height=20, width=40, dpi=300, units='cm')

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

ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi", "Rainbow Smelt")), 
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
library(ggtext)

ggplot(subset(dfishy1, FISH %in% c("Bloater", "Cisco", "Kiyi", "Rainbow Smelt")), 
       aes(x=FISHING_DEPTH, y = ..scaled.., weight=Fdensity/Fdensity.grp, group=FISH, fill=FISH)) + 
  geom_density(alpha=0.4) +
  geom_segment(aes(x=406,xend=406,y=0,yend=0.1), color='black', size=3) +
  geom_segment(aes(x=min(dfishy1$avgdepth), xend=max(dfishy1$avgdepth), y=0.05, yend=0.05), size=3, color='black') +
  plot_theme + 
  theme(legend.title=element_blank(),
        legend.position=c(0.8,0.72),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_markdown(size=24, family='serif'), 
        plot.subtitle = element_markdown(size=24, family='serif'),
        plot.caption = element_markdown(hjust=0, size=24, family='serif'), 
        axis.text=element_text(size=24, family='serif'),
        axis.title=element_text(size=24, family='serif'),
        legend.text=element_text(size=24, family='serif')) + 
  scale_x_continuous(expand=c(0,0), limits = c(0, 410), breaks=c(0,100, 200, 300, 400), 
                     labels=c('0', '100', '200', '300', '400'))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_brewer(palette="Dark2") +
  labs( x='Depth of capture (m)', y='Relative density',
        title='Lake Superior Coregonines and Rainbow Smelt Depth Distributions',
        subtitle=' ',
        caption=ann_data_access) +
  annotate(geom="text", x=295, y=0.1, label="Sampled depths", size=8, family='serif') +
  annotate(geom="text", x=390, y=0.12, label="atop(Maximum, depth)", 
           size=8, family='serif', parse = TRUE) 

ggsave(here('Plots and Tables/RVCAT','ciscoe_RBS_capturedepths.png'), height=20, width=40, dpi=600, units='cm')


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


