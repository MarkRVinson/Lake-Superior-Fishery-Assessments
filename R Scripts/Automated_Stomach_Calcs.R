library(tidyverse)
library(ggplot2)
library(readxl)
library(tibble)
library(quantreg)
library(plotrix)
library(doBy)
library(viridis)
library(reshape)
library(xlsx)
library(spaa)
library(scales)
library(rgdal)
library(cowplot)
library(ggExtra)
library(here)

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

##run from here through line 53, then define parameters
##load data 
Prey <- read_excel(here('Data',"LSBS_Stomach_Contents.xlsx"), sheet ="RawData") 

##fill in missing prey weights using the average for the species
Prey$AvgPreyWt<-Prey$PreyWT_g/Prey$PreyCount
Prey2<-select(Prey, 'Prey4','AvgPreyWt')
Prey2<-na.omit(Prey2)

##calculate mean for each prey type, change here if you want to calculate based on a diff. level of 
##classification (ie. Prey1-4)
Mean.Prey.Wts<-aggregate(Prey2$AvgPreyWt, by=list(Prey4=Prey2$Prey4), FUN=mean)%>%
  renameCol('x','PreyItem.WT')

Prey<-merge.data.frame(Prey, Mean.Prey.Wts, all=T)

replace1<-subset(Prey, is.na(PreyWT_g))
good.data<-subset(Prey, !is.na(PreyWT_g))

replace1$PreyWT_g<-with(replace1, PreyCount*PreyItem.WT)

Prey.Complete<-rbind(good.data, replace1)

Prey<-select(Prey.Complete, c(1:38))


##Fill in the parameters you want for the analysis--species, prey types, etc. Can list one or multiple
Predators<-c('siscowet lake trout')
Years<-c(2016,2017,2018,2019)
Months<-c(1:12)
PredSizeGreater<-0 ##if you want a minimum size for predators (ie. set at 300, size will be >300 mm)
PredSizeLess<-800 ##if you want a maximum size for predators
AvgDepthGreater<-0 ##if you want a minimum depth of capture (ie. if 50, will subset fish caught at >50 m)
AvgDepthLess<-400 ##if you want a maximum depth of capture
filename<-'sisLT_2016-2019' ##how do you want export files names

##Now just run the below code, outputs will save to RVCAT/Plots and Tables/Stomachs
Prey$AvgDepth_m<-(Prey$BegDepth_m+Prey$EndDepth_m)/2

Stomachs<-Prey%>%
  filter(Year%in%Years)%>%
  filter(Predator%in%Predators)%>%
  filter(PRLength_mm>PredSizeGreater & PRLength_mm<PredSizeLess)%>%
  filter(Month%in%Months)%>%
  filter(AvgDepth_m>AvgDepthGreater & AvgDepth_m<AvgDepthLess)


sample.size<-aggregate(Stomachs$PreyWT_g, by=list(FishRecord=Stomachs$FishRecord,
                                                  Predator=Stomachs$Predator), FUN=sum)%>%
  renameCol('x','Sum')
sample.size<-aggregate(sample.size$FishRecord, by=list(Predator=sample.size$Predator), FUN=length)%>%
  renameCol('x','SampleSize')

n.empty<-Stomachs%>%
  filter(Empty=='Y')
n.empty<-aggregate(n.empty$Specimen, by=list(Predator=n.empty$Predator), FUN=length)%>%
  renameCol('x','Number.Empty')

summary.stats<-merge.data.frame(sample.size, n.empty)
write.xlsx(summary.stats, sheet='samples', append=T,
           here('Plots and Tables/Stomachs', paste(filename,'StomachData.xlsx', sep='_')),
           row.names = F)

biomass<-Stomachs%>%
  filter(Empty=='N')%>%
  select(c('FishRecord','Predator','Prey3','PreyWT_g'))

biomass2<-cast(biomass, FishRecord+Predator~Prey3, value='PreyWT_g', fun.aggregate = 'sum')
biomass2[is.na(biomass2)]<-0
biomass3<-melt(biomass2, id=c('FishRecord','Predator'))
biomass3<-renameCol(biomass3, 'value','PreyWT_g')

ind.totals<-aggregate(biomass3$PreyWT_g, by=list(FishRecord=biomass3$FishRecord), FUN=sum) %>%
  renameCol('x','Ind.Total')

biomass4<-merge.data.frame(biomass3, ind.totals)
biomass4$Ind.Prop<-(biomass4$PreyWT_g/biomass4$Ind.Total)*100
biomass4<-biomass4[complete.cases(biomass4),]

biomass.props<-aggregate(biomass4$Ind.Prop, by=list(Predator=biomass4$Predator, 
                                                    Prey=biomass4$Prey3), FUN=mean)%>%
  renameCol('x','Mean.Percent')

biomass.props2<-cast(biomass.props, Prey~Predator, value='Mean.Percent')

write.xlsx(biomass.props2, sheet='biomass', append=T,
           here('Plots and Tables/Stomachs',paste(filename,'StomachData.xlsx',sep='_')),
                                row.names = F)

##Percent Occurrence with Prey1
perc.occ1<-Stomachs%>%
  select('FishRecord','Predator','Prey1','Prey2','Prey3','Prey4','PreyWT_g')
perc.occ1<-subset(perc.occ1, PreyWT_g>0)
perc.occ<-aggregate(perc.occ1$PreyWT_g, by=list(FishRecord=perc.occ1$FishRecord, Predator=perc.occ1$Predator,
                                                Prey.Types=perc.occ1$Prey1), FUN=sum)
perc.occ<-aggregate(perc.occ$FishRecord, by=list(Predator=perc.occ$Predator,
                                                  Prey.Types=perc.occ$Prey.Types), FUN=length)%>%
  renameCol('x','N.fish.wpreytype')

perc.occ<-merge.data.frame(perc.occ, sample.size)
perc.occ<-merge.data.frame(perc.occ, n.empty)
perc.occ$N.stom.full<-perc.occ$SampleSize-perc.occ$Number.Empty
perc.occ$Percent.Occurrence<-(perc.occ$N.fish.wpreytype/perc.occ$N.stom.full)*100
perc.occ<-cast(perc.occ, Prey.Types~Predator, value='Percent.Occurrence')
perc.occ[is.na(perc.occ)]<-0

write.xlsx(perc.occ, sheet='occurrence_prey1', append=T,
           here('Plots and Tables/Stomachs',paste(filename,'StomachData.xlsx', sep='_')),
           row.names = F)

##Percent Occurrence Prey2##
perc.occ2<-aggregate(perc.occ1$PreyWT_g, by=list(FishRecord=perc.occ1$FishRecord, Predator=perc.occ1$Predator,
                                                Prey.Types=perc.occ1$Prey2), FUN=sum)
perc.occ2<-aggregate(perc.occ2$FishRecord, by=list(Predator=perc.occ2$Predator,
                                                 Prey.Types=perc.occ2$Prey.Types), FUN=length)%>%
  renameCol('x','N.fish.wpreytype')

perc.occ2<-merge.data.frame(perc.occ2, sample.size)
perc.occ2<-merge.data.frame(perc.occ2, n.empty)
perc.occ2$N.stom.full<-perc.occ2$SampleSize-perc.occ2$Number.Empty
perc.occ2$Percent.Occurrence<-(perc.occ2$N.fish.wpreytype/perc.occ2$N.stom.full)*100
perc.occ2<-cast(perc.occ2, Prey.Types~Predator, value='Percent.Occurrence')
perc.occ2[is.na(perc.occ2)]<-0

write.xlsx(perc.occ2, sheet='occurrence_prey2', append=T,
           here('Plots and Tables/Stomachs',paste(filename,'StomachData.xlsx', sep='_')),
           row.names = F)

##Percent Occurrence Prey3##
perc.occ3<-aggregate(perc.occ1$PreyWT_g, by=list(FishRecord=perc.occ1$FishRecord, Predator=perc.occ1$Predator,
                                                 Prey.Types=perc.occ1$Prey3), FUN=sum)
perc.occ3<-aggregate(perc.occ3$FishRecord, by=list(Predator=perc.occ3$Predator,
                                                   Prey.Types=perc.occ3$Prey.Types), FUN=length)%>%
  renameCol('x','N.fish.wpreytype')

perc.occ3<-merge.data.frame(perc.occ3, sample.size)
perc.occ3<-merge.data.frame(perc.occ3, n.empty)
perc.occ3$N.stom.full<-perc.occ3$SampleSize-perc.occ3$Number.Empty
perc.occ3$Percent.Occurrence<-(perc.occ3$N.fish.wpreytype/perc.occ3$N.stom.full)*100
perc.occ3<-cast(perc.occ3, Prey.Types~Predator, value='Percent.Occurrence')
perc.occ3[is.na(perc.occ3)]<-0

write.xlsx(perc.occ3, sheet='occurrence_prey3', append=T,
           here('Plots and Tables/Stomachs',paste(filename,'StomachData.xlsx', sep='_')),
           row.names = F)


##Percent Occurrence Prey4##
perc.occ4<-aggregate(perc.occ1$PreyWT_g, by=list(FishRecord=perc.occ1$FishRecord, Predator=perc.occ1$Predator,
                                                 Prey.Types=perc.occ1$Prey4), FUN=sum)
perc.occ4<-aggregate(perc.occ4$FishRecord, by=list(Predator=perc.occ4$Predator,
                                                   Prey.Types=perc.occ4$Prey.Types), FUN=length)%>%
  renameCol('x','N.fish.wpreytype')

perc.occ4<-merge.data.frame(perc.occ4, sample.size)
perc.occ4<-merge.data.frame(perc.occ4, n.empty)
perc.occ4$N.stom.full<-perc.occ4$SampleSize-perc.occ4$Number.Empty
perc.occ4$Percent.Occurrence<-(perc.occ4$N.fish.wpreytype/perc.occ4$N.stom.full)*100
perc.occ4<-cast(perc.occ4, Prey.Types~Predator, value='Percent.Occurrence')
perc.occ4[is.na(perc.occ4)]<-0

write.xlsx(perc.occ4, sheet='occurrence_prey4', append=T,
           here('Plots and Tables/Stomachs',paste(filename,'StomachData.xlsx', sep='_')),
           row.names = F)
##############################################
##Table of prey consumed with counts and mean and maximum prey lengths
##############################################

prey4 <- Stomachs %>%
  group_by(Predator,Prey4) %>%
  summarise(prey_cnt = n(), max_length=max(PreySize_mm,  na.rm=TRUE),
            mean_length=mean(PreySize_mm, na.rm=TRUE)) %>%
  ungroup()
prey4<-as.data.frame(prey4)

write.xlsx(prey4, sheet="PreyCnt", append=TRUE, 
           here('Plots and Tables/Stomachs',paste(filename,'StomachData.xlsx',sep='_')),
           row.names = F)

##############################################

##############################################
##Methods Stats
##############################################
Stomachs2<-Stomachs[match(unique(Stomachs$FishRecord), Stomachs$FishRecord),]

n.sites<-data.frame(Metric=c('N.Sites'), Value=c(length(unique(Stomachs2$Station))))
first.date<-data.frame(Metric=c('First.Date'),Value=c(min(Stomachs2$Date)))
last.date<-data.frame(Metric=c('Last.Date'),Value=c(max(Stomachs2$Date)))
min.depth<-data.frame(Metric=c('Min.Depth'), Value=c(min(Stomachs2$AvgDepth_m)))
max.depth<-data.frame(Metric=c('Max.Depth'), Value=c(max(Stomachs2$AvgDepth_m)))
mean.depth<-data.frame(Metric=c('Mean.Depth'), Value=c(mean(Stomachs2$AvgDepth_m)))
median.depth<-data.frame(Metric=c('Median.Depth'), Value=c(median(Stomachs2$AvgDepth_m)))
n.trawl<-subset(Stomachs2, Gear=='BTR'|Gear=='MTR')
n.trawl<-data.frame(Metric=c('N.Trawl.Samples'), Value=c(length(n.trawl$FishRecord)))
n.gn<-subset(Stomachs2, Gear=='GN')
n.gn<-data.frame(Metric=c('N.Gillnet.Samples'), Value=c(length(n.gn$FishRecord)))
min.pred.length<-data.frame(Metric=c('Min.Pred.Length'), Value=c(min(Stomachs2$PRLength_mm)))
max.pred.length<-data.frame(Metric=c('Max.Pred.Length'), Value=c(max(Stomachs2$PRLength_mm)))
mean.pred.length<-data.frame(Metric=c('Mean.Pred.Length'), Value=c(mean(Stomachs2$PRLength_mm)))
sd.pred.length<-data.frame(Metric=c('StanDev.Pred.Length'), Value=c(sd(Stomachs2$PRLength_mm)))
median.pred.length<-data.frame(Metric=c('Median.Pred.Length'), Value=c(median(Stomachs2$PRLength_mm)))
n.prey<-aggregate(Stomachs$PreyWT_g, by=list(FishRecord=Stomachs$FishRecord, Prey.Types=Stomachs$Prey4),
                  FUN=sum)
n.prey<-aggregate(n.prey$Prey.Types, by=list(FishRecord=n.prey$FishRecord), FUN=length)
max.prey<-data.frame(Metric=c('Max.Prey.Groups'), Value=c(max(n.prey$x)))
mean.prey<-data.frame(Metric=c('Mean.Prey.Groups'), Value=c(mean(n.prey$x)))
median.prey<-data.frame(Metric=c('Median.Prey.Groups'), Value=c(median(n.prey$x)))
n.month<-aggregate(Stomachs2$FishRecord, by=list(Month=Stomachs2$Month), FUN=length)
n.month$Month<-paste('n.month',n.month$Month, sep='_')
n.month<-n.month%>%
  renameCol('Month','Metric')%>%
  renameCol('x','Value')
n.month<-as.data.frame(n.month)

methods.stats<-rbind(n.sites,min.depth, max.depth, mean.depth, median.depth,
                     n.trawl, n.gn, min.pred.length, max.pred.length, mean.pred.length,
                     sd.pred.length, median.pred.length, max.prey, mean.prey, median.prey, n.month)
methods.stats$Value<-round(methods.stats$Value, 2)

write.xlsx(methods.stats, append=T, sheet='Methods.Stats',
           here('Plots and Tables/Stomachs',
                paste(filename,'StomachData.xlsx', sep='_')), row.names = F)

##to add collection location stats to the excel file
coords<- read_excel(here('Data',"LSBS_Stomach_Contents.xlsx"), sheet ="Locations")
coords<-select(coords, c('Agency','Station','Latitude','Longitude'))
coords<-coords[match(unique(coords$Station),coords$Station),]

collections<-merge.data.frame(Stomachs2, coords, by=c('Agency', 'Station'))

collection.stats<-aggregate(collections$FishRecord, by=list(Agency=collections$Agency,
                                                            Station=collections$Station,
                                                            Latitude=collections$Latitude,
                                                            Longitude=collections$Longitude),
                            FUN=length)%>% renameCol('x','N.Fish.Collected')
write.xlsx(collection.stats, append=T, sheet='Collections',here('Plots and Tables/Stomachs',
                                                                paste(filename,'StomachData.xlsx',sep='_')),
           row.names = F)

############################################################
##PLOTS
############################################################

ggplot(Stomachs, aes(x=PRLength_mm, y=PreySize_mm, color=Prey2))+
  geom_point(size=3)+
  plot_theme+
  labs(x='Predator length (mm)',y='Prey length (mm)', color='Prey', title=filename)+
  scale_y_continuous(expand=c(0,0), limits=c(0, max(Stomachs$PreySize_mm+10)),
                     breaks=pretty_breaks(n=6))+
  scale_x_continuous(expand=c(0,0), limits=c(0,max(Stomachs$PRLength_mm+10)),
                     breaks=pretty_breaks(n=8))
ggsave(here('Plots and Tables/Stomachs',paste(filename, 'prey_length.png', sep='_')),
       height=20, width=40, units='cm')

ggplot(Stomachs, aes(x=PRLength_mm, y=PreySize_mm, color=Prey3))+
  geom_point(size=3)+
  plot_theme+
  labs(x='Predator length (mm)',y='Prey length (mm)', color='Prey', title=filename)+
  scale_y_continuous(expand=c(0,0), limits=c(0, max(Stomachs$PreySize_mm+10)),
                     breaks=pretty_breaks(n=6))+
  scale_x_continuous(expand=c(0,0), limits=c(0,max(Stomachs$PRLength_mm+10)),
                     breaks=pretty_breaks(n=8))
ggsave(here('Plots and Tables/Stomachs',paste(filename, 'prey3_length.png', sep='_')),
       height=20, width=40, units='cm')

ggplot(Stomachs2, aes(x=PRLength_mm))+
  geom_bar(stat='count', binwidth = 10, fill='grey25')+
  plot_theme+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0), breaks=pretty_breaks(n=8), limits=c(0,max(Stomachs$PRLength_mm+10)))+
  labs(x='Predator length (mm), 10 mm bins', y='Count', title=filename)
ggsave(here('Plots and Tables/Stomachs', paste(filename, 'length_freq.png',sep='_')),
       height=20, width=40, units='cm')


map_theme<-theme(axis.text=element_text(size=20, family='serif'), 
                 axis.title=element_text(size=20, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=18, family='serif'),
                 plot.caption=element_text(size=16, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=16, family='serif'))

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ggplot(collections, aes(Longitude, Latitude)) +
  theme_bw() +
  scale_y_continuous()+
  scale_x_continuous(breaks=c(-93,-92,-91,-90,-89,-88,-87,-86,-85,-84), 
                     labels=c('-93','-92','-91','-90','-89','-88','-87','-86','-85','-84'))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=collections, mapping=aes(Longitude, Latitude), size=6, stroke=1.5, color='darkslategray2')+
  map_theme+
  geom_text(aes(label=Station))+
  labs( x='Longitude', y='Latitude',
        title=filename)
ggsave(here('Plots and Tables/Stomachs',paste(filename,'collection_map.png', sep='_')),
       height=20, width=40, units='cm')


##for area plot
fishlengths<-select(Stomachs2,c('FishRecord','PRLength_mm'))
fishlengths<-merge.data.frame(fishlengths, biomass4)
fishlengths$bins<-cut(fishlengths$PRLength_mm, breaks=seq(0,max(fishlengths$PRLength_mm), by=50),
                      labels=seq(51,max(fishlengths$PRLength_mm), by=50))

categ.other<-aggregate(fishlengths$Ind.Prop, by=list(Prey3=fishlengths$Prey3), FUN=mean)%>%
  renameCol('x','overall.mean')
categ.other$Labels<-if_else(categ.other$overall.mean<5,'other',paste(categ.other$Prey))

fishlengths<-merge.data.frame(fishlengths, categ.other)
fishlengths<-filter(fishlengths, Labels !='other')
ind.total2<-aggregate(fishlengths$PreyWT_g, by=list(FishRecord=fishlengths$FishRecord), FUN=sum)%>%
  renameCol('x','Ind.Total.g.no-other')
fishlengths<-merge.data.frame(fishlengths, ind.total2)
fishlengths$Ind.Prop<-(fishlengths$PreyWT_g/fishlengths$`Ind.Total.g.no-other`)*100
fishlengths2<-cast(fishlengths, FishRecord+bins~Labels, value='Ind.Prop', fun.aggregate = 'sum')
fishlengths2<-melt(fishlengths2, id=c('FishRecord','bins'))

bin.avgs<-aggregate(fishlengths2$value, by=list(Prey=fishlengths2$Labels, LengthBin=fishlengths2$bins),
                    FUN=mean)%>%
  renameCol('x','Perc.Biomass')

fishlengths3<-fishlengths2[match(unique(fishlengths2$FishRecord), fishlengths2$FishRecord),]

bin.sample.sizes<-aggregate(fishlengths3$FishRecord, by=list(LengthBin=fishlengths3$bins), FUN='length')%>%
  renameCol('x','bin.sample.size')
bin.avgs<-merge.data.frame(bin.avgs, bin.sample.sizes)

fishlengths3<-fishlengths3%>%
  renameCol('bins','LengthBin')%>%
  renameCol('value','Perc.Biomass')

samples<-ggplot(bin.sample.sizes, aes(LengthBin, bin.sample.size))+
  geom_bar(stat='identity', fill='grey70')+
  plot_theme+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))+
  labs(title=filename)+
  theme(axis.text.x=element_blank(),
        plot.margin = unit(c(0,0,0,0),'cm'),
        axis.text.y=element_blank(),
        axis.line.y=element_blank(),
        axis.ticks=element_blank(),
        axis.line.x=element_line(size=0.25),
        axis.title=element_blank())
  
areas<-ggplot()+
  geom_area(inherit.aes=F, data=bin.avgs, aes(x=LengthBin, y=Perc.Biomass, fill=Prey, group=Prey))+
  plot_theme+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), limits=c(0,100))+
  scale_fill_brewer(palette='Paired', name='')+
  labs(x='Predator length (mm)', y='Mean percent biomass')+
  theme(legend.position='bottom',
        plot.margin = unit(c(0,1,0,0),'cm'),
        plot.title = element_blank())+
  guides(fill=guide_legend(nrow=1))

  #ggMarginal(areas,type='histogram', data=fishlengths3, x=LengthBin,
  #           y=Perc.Biomass, margins='x')
  #annotation_custom(ggplotGrob(samples), ymin=100,ymax=110)

plots<-plot_grid(samples, areas, align='v',rel_heights=c(3/16, 13/16), nrow=2)
plots
save_plot(here('Plots and Tables/Stomachs',paste(filename, 'prey_areaplot.png',sep='_')), plots,
          base_height=8, base_width=15)
