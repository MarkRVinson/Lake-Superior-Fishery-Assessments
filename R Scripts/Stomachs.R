
## Load packages
## ===========================================================
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

options(scipen=999)


##load data 
Prey <- read_excel(here('Data',"LSBS_Stomach_Contents.xlsx"), sheet ="RawData") 

#select needed columns
Prey1 <- select(Prey, Predator, Sex, Year, Prey1, Prey2, Prey3, PRLength_mm, PRWeight_g, PreySize_mm, PreyWT_g)
Prey1<-renameCol(Prey1, 'Prey1','PreyClass')
Prey1<-renameCol(Prey1, 'Prey3','PreyGroup')
Prey1$SizeGroup<-cut(Prey1$PRLength_mm, breaks=c(0,99,199,299,399,499,599,699,1000), labels=c('<100','100-200','200-300','300-400','400-500','500-600',
                                                                                              '600-700','>700'))
Prey1$empty<-if_else(Prey1$PreyClass=='empty', 'empty', 'prey.present')

##proportion empty stomachs by fish size
empty.summary<-aggregate(Prey1$Predator, by=list(Predator=Prey1$Predator, SizeGroup=Prey1$SizeGroup, n.empty=Prey1$empty), FUN=length)%>%
  renameCol('x','n')

empty.summary<-cast(empty.summary, SizeGroup+Predator~n.empty, value='n')
empty.summary$total.n<-empty.summary$empty + empty.summary$prey.present
empty.summary$perc.empty<-(empty.summary$empty/empty.summary$total.n)*100

empty.predators<-empty.summary%>%
  subset(Predator=='lean lake trout'|Predator=='siscowet lake trout'|Predator=='burbot')

ggplot(empty.predators, aes(x=SizeGroup, y=perc.empty))+
  geom_bar(stat='identity',position='dodge')+
  plot_theme+
  labs(x='Size group (mm)', y='Percent empty stomachs', title='Prevalence of empty stomachs Lake Superior native predator fishes',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_y_continuous(limits=c(0,25), expand=c(0,0))+
  scale_fill_viridis(discrete=T)+
  facet_grid(Predator~.)
ggsave(here('Plots and Tables/Stomachs','no_os_empty_predators.png'), height=20, width=40, units='cm')

##proportion empty stomachs trends across years
Prey2<-subset(Prey1, PRLength_mm>200&PRLength_mm<700)
empty.summary2<-aggregate(Prey2$Predator, by=list(Predator=Prey2$Predator, Year=Prey2$Year, n.empty=Prey2$empty), FUN=length)%>%
  renameCol('x','n')

empty.summary2<-cast(empty.summary2, Year+Predator~n.empty, value='n')
empty.summary2[is.na(empty.summary2)]<-0
empty.summary2$total.n<-empty.summary2$empty + empty.summary2$prey.present
empty.summary2$perc.empty<-(empty.summary2$empty/empty.summary2$total.n)*100

empty.predators2<-empty.summary2%>%
  subset(Predator=='lean lake trout'|Predator=='siscowet lake trout'|Predator=='burbot')

ggplot(empty.predators2, aes(x=Year, y=perc.empty))+
  geom_bar(stat='identity',position='dodge')+
  plot_theme+
  labs(x='Year', y='Percent empty stomachs', title='Prevalence of empty stomachs Lake Superior native predator fishes size 200-700 mm',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_y_continuous(limits=c(0,NA), expand=c(0,0))+
  scale_x_continuous(breaks=seq(from=2012, to=max(empty.predators2$Year), by=1))+
  scale_fill_viridis(discrete=T)+
  facet_grid(Predator~., scales='free')
ggsave(here('Plots and Tables/Stomachs','no_os_empty_predators_byyear.png'), height=20, width=40, units='cm')


##remove empty stomachs and calculate prey proportions
spp2<-Prey1[!is.na(Prey1$PreyGroup),]
spp5<-select(spp2,'Year','Predator','PRLength_mm','PRWeight_g', 'PreyGroup','Fish')
spp3<-select(spp2, 'Fish','PreyGroup','PRWeight_g')
spp3<-cast(spp3, Fish~PreyGroup, fun.aggregate='sum')
spp4<-melt(spp3, id.vars='Fish')%>%
  renameCol('value','PreyWT_g')

simple_prey<-data.frame(Simple=c('other', 'other','other','fish','invertebrate','invertebrate','fish', 'Diporeia','fish',
                                 'Mysis','other','fish','fish','fish','fish','invertebrate','other'), PreyGroup=c('adult insect',
                                  'aquatic invertebrate','aquatic vegetation','burbot','calanoida','cladocera','coregonus','diporeia','fish other','mysis',
                                    'other','rainbow smelt','salmonid','sculpin','stickleback','terrestrial insect','terrestrial vegetation'))

spp4<-merge.data.frame(spp4, simple_prey)

sum.prey<-aggregate(spp4$PreyWT_g, by=list(Fish=spp4$Fish, PreyGroup=spp4$Simple), FUN=sum)%>%
  renameCol('x','PreyGroup_g')
sum.fish<-aggregate(spp4$PreyWT_g, by=list(Fish=spp4$Fish), FUN=sum)%>%
  renameCol('x', 'TotalPrey_g')

prey.prop<-merge.data.frame(sum.prey, sum.fish)
prey.prop$PreyProp<-prey.prop$PreyGroup_g/prey.prop$TotalPrey_g

fish.details<-select(spp2, 'Predator','Fish','SizeGroup')
fish.details<-unique(fish.details[,1:3])
prey.prop<-merge.data.frame(prey.prop, fish.details)
prey.prop<-prey.prop[complete.cases(prey.prop),]

mean.prop.size<-aggregate(prey.prop$PreyProp, by=list(Predator=prey.prop$Predator, SizeGroup=prey.prop$SizeGroup, PreyGroup=prey.prop$PreyGroup), FUN=mean)%>%
  renameCol('x','Mean.Prop.bySize')

mean.props.preds<-mean.prop.size%>%
  subset(Predator=='lean lake trout'|Predator=='siscowet lake trout'|Predator=='burbot')

ggplot(mean.props.preds, aes(x=SizeGroup, y=Mean.Prop.bySize, fill=PreyGroup, group=PreyGroup))+
  geom_area()+
  plot_theme+
  scale_y_continuous(expand=c(0,0))+
  labs(x='Size group (mm)',y='Mean proportion', title='Lake Superior native predator fish stomach contents by size',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_fill_brewer(palette='Paired', name='Prey')+
  facet_grid(Predator~.)

ggsave(here('Plots and Tables/Stomachs','ns_os_prey_summary.png'), height=20, width=40, units='cm', dpi=300)

##prey proportions by season
prey.seas<-select(Prey, Predator, Fish, Month, Prey1, Prey3, PRLength_mm, PRWeight_g, PreySize_mm, PreyWT_g)
prey.seas<-renameCol(prey.seas, 'Prey1','PreyClass')
prey.seas<-renameCol(prey.seas, 'Prey3','PreyGroup')
prey.seas<-prey.seas%>%
  filter(Predator=='siscowet lake trout')

prey.seas$SizeGroup<-cut(prey.seas$PRLength_mm, breaks=c(0,199,499,800), labels=c('<200','200-500','>500'))
prey.seas$Season<-cut(prey.seas$Month, breaks=c(3,5,8,11), labels=c('Spring','Summer','Fall'))

prey.seas1<-prey.seas[!is.na(prey.seas$PreyGroup),]
prey.seas2<-select(prey.seas1, 'Fish','PreyGroup','PreyWT_g')
prey.seas2<-cast(prey.seas2, Fish~PreyGroup, fun.aggregate='sum')
prey.seas3<-melt(prey.seas2, id.vars='Fish')%>%
  renameCol('value','PreyWT_g')

sum.prey2<-aggregate(prey.seas3$PreyWT_g, by=list(Fish=prey.seas3$Fish, PreyGroup=prey.seas3$PreyGroup), FUN=sum)%>%
  renameCol('x','PreyGroup_g')
sum.fish2<-aggregate(prey.seas3$PreyWT_g, by=list(Fish=prey.seas3$Fish), FUN=sum)%>%
  renameCol('x', 'TotalPrey_g')

prey.prop2<-merge.data.frame(sum.prey2, sum.fish2)
prey.prop2$PreyProp<-prey.prop2$PreyGroup_g/prey.prop2$TotalPrey_g

fish.details2<-select(prey.seas1, 'Fish','SizeGroup','Season')
fish.details2<-unique(fish.details2[,1:3])
prey.prop2<-merge.data.frame(prey.prop2, fish.details2)
prey.prop2<-prey.prop2[complete.cases(prey.prop2),]

mean.prop.size2<-aggregate(prey.prop2$PreyProp, by=list(SizeGroup=prey.prop2$SizeGroup, PreyGroup=prey.prop2$PreyGroup, 
                                                        Season=prey.prop2$Season), FUN=mean)%>%
  renameCol('x','Mean.Prop.bySize')

mean.prop.size2<-merge.data.frame(mean.prop.size2, simple_prey)

ggplot(mean.prop.size2, aes(x=SizeGroup, y=Mean.Prop.bySize, fill=Simple))+
  geom_bar(position='stack', stat='identity')+
  plot_theme+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(.~Season)+
  labs(x='Size group (mm)', y='Mean proportion of biomass',title='Lake Superior siscowet Lake Trout seasonal prey consumption',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_fill_brewer(palette='Paired', name='Prey')

ggsave(here('Plots and Tables/Stomachs','ns_os_SCA_season.png'), height=20, width=40, units='cm')


##table with prey proportions for burbot, leans, and siscowets
Prey1 <- select(Prey, Predator, Fish, Prey1, Prey3, PRLength_mm, PRWeight_g, PreySize_mm, PreyWT_g)
Prey1<-renameCol(Prey1, 'Prey1','PreyClass')
Prey1<-renameCol(Prey1, 'Prey3','PreyGroup')

#filter to only include siscowets
sisc<-Prey1%>%
  filter(Predator=='siscowet lake trout')
sisc$SizeGroup<-1
##<-cut(sisc$PRLength_mm, breaks=c(0,300,1500), labels=c('<300 mm', '>300 mm')) ##replace the above definition of SizeGroup with this if you want size 
##group analysis. Change the breaks and labels depending on what classes you want. Same with the lines below for leans and burbot.
##SizeGroup 1 is a placeholder so if you decide to run it with size groups, the code below will work as written

lean<-Prey1%>%
  filter(Predator=='lean lake trout')
lean$SizeGroup<-1
##-cut(lean$PRLength_mm, breaks=c(0,400,1500), labels=c('<400 mm', '>400 mm'))

burb<-Prey1%>%
  filter(Predator=='burbot')
burb$SizeGroup<-1
##<-cut(burb$PRLength_mm, breaks=c(0,300,1500), labels=c('<300 mm','>300 mm'))


spp<-rbind(sisc, lean, burb)


spp2<-spp[!is.na(spp$PreyGroup),]
spp3<-select(spp2, 'Predator', 'Fish','PreyGroup','PreyWT_g')
spp3<-cast(spp3, Fish+Predator~PreyGroup, fun.aggregate='sum')
spp4<-melt(spp3, id.vars=c('Fish','Predator'))%>%
  renameCol('value','PreyWT_g')

spp4<-merge.data.frame(spp4, simple_prey)

sum.prey<-aggregate(spp4$PreyWT_g, by=list(Fish=spp4$Fish, PreyGroup=spp4$PreyGroup), FUN=sum)%>% 
  renameCol('x','PreyGroup_g')
sum.fish<-aggregate(spp4$PreyWT_g, by=list(Fish=spp4$Fish), FUN=sum)%>%
  renameCol('x', 'TotalPrey_g')

prey.prop<-merge.data.frame(sum.prey, sum.fish)
prey.prop$PreyProp<-prey.prop$PreyGroup_g/prey.prop$TotalPrey_g

fish.details<-select(spp2, 'Predator','Fish','SizeGroup')
fish.details<-unique(fish.details[,1:3])
prey.prop<-merge.data.frame(prey.prop, fish.details)
prey.prop<-prey.prop[complete.cases(prey.prop),]

mean.prop.size<-aggregate(prey.prop$PreyProp, by=list(Predator=prey.prop$Predator, SizeGroup=prey.prop$SizeGroup, PreyGroup=prey.prop$PreyGroup), FUN=mean)%>%
  renameCol('x','Mean.Prop.bySize')

mean.prop.size$Mean.Prop.bySize<-round(mean.prop.size$Mean.Prop.bySize,3)*100

prop.table1<-cast(mean.prop.size, PreyGroup~Predator+SizeGroup)
prop.table1<-prop.table1%>%
  renameCol('PreyGroup','Prey')%>%
  renameCol(2,'Burbot')%>%
  renameCol(3,'lean Lake Trout')%>%
  renameCol(4,'siscowet Lake Trout')
write.xlsx(prop.table1, here('Plots and Tables/Stomachs','prey_table.xlsx'), row.names = F)

##trends in prey over time
prey.time<-Prey%>%
  select(c(2,12,13,18,19,20,28,30,39))
prey.time<-prey.time[!is.na(prey.time$PreyWT_g),]
prey.time<-prey.time[!is.na(prey.time$PRWeight_g),]
prey.time<-aggregate(prey.time$PreyWT_g, by=list(Fish=prey.time$Fish, Year=prey.time$Year, Month=prey.time$Month, Predator=prey.time$Predator, 
                                                 PRLength_mm=prey.time$PRLength_mm,
                                                 PRWeight_g=prey.time$PRWeight_g, Prey3=prey.time$Prey3
                                                 ), FUN=sum)%>%
  renameCol('x','PreyWT_g')

prey.time$prey.perc.g<-(prey.time$PreyWT_g/prey.time$PRWeight_g)*100
prey.time2<-prey.time%>%
  filter(Prey3=='rainbow smelt'|Prey3=='mysis'|Prey3=='coregonus'|Prey3=='sculpin')%>%
  filter(Predator=='siscowet lake trout'|Predator=='lean lake trout')

ggplot(prey.time2, aes(x=Year, y=prey.perc.g))+
  geom_jitter(size=2)+
  plot_theme+
  facet_grid(Prey3+Predator~.)+
  scale_y_continuous(limits=c(0,15))+
  labs(x='Year',y='(Prey weight/Predator weight)*100',
       title='Lake Superior lean and siscowet Lake Trout common prey',
       caption='Data: USGS and WI-DNR stomach content analyses')

ggsave(here('Plots and Tables/Stomachs','ns_os_preytrends.png'), height=40, width=40, units='cm')

prey.time3<-aggregate(prey.time2$prey.perc.g, by=list(Predator=prey.time2$Predator, Prey3=prey.time2$Prey3, Year=prey.time2$Year), FUN=mean)%>%
  renameCol('x','mean.prey.perc.g')

ggplot(prey.time3, aes(x=Year, y=mean.prey.perc.g))+
  geom_bar(stat='identity')+
  plot_theme+
  facet_grid(Prey3+Predator~.)+
  scale_y_continuous(limits=c(0,15))

prey.time4<-aggregate(prey.time2$PreyWT_g, by=list(Fish=prey.time2$Fish, Year=prey.time2$Year, PRWeight_g=prey.time2$PRWeight_g,
                                                   Predator=prey.time2$Predator), FUN=sum)%>%
  renameCol('x','Tot.Prey.Fish.g')

prey.time4$prey.perc.g<-(prey.time4$Tot.Prey.Fish.g/prey.time4$PRWeight_g)*100
prey.time4$Prey3<-'total prey'
prey.time4<-select(prey.time4, c(1:4, 6,7))


smelt<-prey.time2%>%
  filter(Prey3=='rainbow smelt')%>%
  select(c(1,2,6,4,9))
smelt$Prey3<-'rainbow smelt'

smeltcompare<-rbind(prey.time4, smelt)
smeltcompare<-smeltcompare%>%
  filter(Predator=='lean lake trout')


raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, size = 16),
  plot.subtitle = element_text(size=14),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", trim = TRUE, position=position_nudge(x = .2, y = 0)
                             ,scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )


##with zeros for fish that had food in guts but no rainbow smelt
smeltcomp3<-cast(smeltcompare, Fish+Year~Prey3, value='prey.perc.g', fun.aggregate = 'sum')
smeltcomp3<-melt(smeltcomp3, id.vars=c(Fish, Year))

labels1<-list('rainbow smelt'='Rainbow Smelt', 'total prey'='Total prey')
labeller1<-function(variable, value){return(labels1[value])}

ggplot(smeltcomp3, aes(x=Year, y=value, group=Year))+
  #geom_flat_violin(trim=F, alpha=0.75, fill='cyan') +
  #geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.8, color='grey40')+
  geom_boxplot(color='red', outlier.alpha = 0.4, size=1)+
  geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.2, color='grey40')+
  facet_grid(Prey3~., labeller=labeller1)+
  plot_theme+
  labs(y='(prey weight/predator weight)*100', title='Lake Superior lean Lake Trout prey fish consumption',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_x_continuous(breaks=seq(2012,2020,1))+
  scale_y_continuous(limits=c(0,8))

ggsave(here('Plots and Tables/Stomachs','ns_os_leanLT_rbs_preytot.png'), height=20, width=40, units='cm')

##just rainbow smelt consumption by lean lake trout, includes zeros for fish that had food in guts but no rbs
smelt4<-subset(smeltcomp3, Prey3=='rainbow smelt')

ggplot(smelt4, aes(x=Year, y=value, group=Year))+
  #geom_flat_violin(trim=F, alpha=0.75, fill='cyan') +
  geom_boxplot(color='red', outlier.alpha = 0.4, size=1)+
  geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.2, color='grey40')+
  facet_grid(Prey3~., labeller=labeller1)+
  plot_theme+
 # coord_flip()+
  labs(y='(prey weight/predator weight)*100', title='Lake Superior lean Lake Trout prey fish consumption',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_x_continuous(breaks=seq(2012,2020,1))+
  scale_y_continuous(limits=c(0,10))

ggsave(here('Plots and Tables/Stomachs','ns_os_leanLT_rbs.png'), height=20, width=40, units='cm')

##coregonus and sculpins in siscowet lake trout_________________________________________________________________________________________________________
coreg<-prey.time2%>%
  filter(Prey3=='coregonus'|Prey3=='sculpin')%>%
  select(c(1,2,6,4,9,7))


coregcompare<-rbind(prey.time4, coreg)
coregcompare<-coregcompare%>%
  filter(Predator=='siscowet lake trout')

coregcomp3<-cast(coregcompare, Fish+Year~Prey3, value='prey.perc.g', fun.aggregate = 'sum')
coregcomp3<-melt(coregcomp3, id.vars=c(Fish, Year))

labels2<-list('coregonus'='Coregonus', 'sculpin'='Sculpin', 'total prey'='Total prey')
labeller2<-function(variable, value){return(labels2[value])}

ggplot(coregcomp3, aes(x=Year, y=value, group=Year))+
  #geom_flat_violin(trim=F, alpha=0.75, fill='cyan') +
  geom_boxplot(color='red', outlier.alpha = 0.4, size=1)+
  geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.2, color='grey40')+
  facet_grid(Prey3~., labeller=labeller2)+
  plot_theme+
  labs(y='(prey weight/predator weight)*100', title='Lake Superior siscowet Lake Trout prey fish consumption',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_x_continuous(breaks=seq(2012,2020,1))+
  scale_y_continuous(limits=c(0,5))

ggsave(here('Plots and Tables/Stomachs','ns_os_siscowetLT_coreg-sculp_preytot.png'), height=20, width=40, units='cm')


coreg2<-subset(coregcomp3, Prey3=='coregonus')
ggplot(coreg2, aes(x=Year, y=log(value,10), group=Year))+
  #geom_flat_violin(trim=F, alpha=0.75, fill='cyan') +
  #geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.8, color='grey40')+
  geom_boxplot(color='red', outlier.alpha = 0.4, size=1)+
  geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.2, color='grey40')+
  facet_grid(Prey3~., labeller=labeller2)+
  plot_theme+
  #coord_flip()+
  labs(y='log((prey weight/predator weight)*100)', title='Lake Superior siscowet Lake Trout prey fish consumption',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_x_continuous(breaks=seq(2012,2020,1))

ggsave(here('Plots and Tables/Stomachs','ns_os_siscowetLT_coreg.png'), height=20, width=40, units='cm')


sculp2<-subset(coregcomp3, Prey3=='sculpin')
ggplot(sculp2, aes(x=Year, y=value, group=Year))+
  #geom_flat_violin(trim=F, alpha=0.75, fill='cyan') +
  #geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.8, color='grey40')+
  geom_boxplot(color='red', outlier.alpha = 0.4, size=1)+
  geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.2, color='grey40')+
  facet_grid(Prey3~., labeller=labeller2)+
  plot_theme+
 # coord_flip()+
  labs(y='(prey weight/predator weight)*100', title='Lake Superior siscowet Lake Trout prey fish consumption',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_x_continuous(breaks=seq(2012,2020,1))

ggsave(here('Plots and Tables/Stomachs','ns_os_siscowetLT_sculp.png'), height=20, width=40, units='cm')


coregcomp3$Predator<-'siscowet Lake Trout'
smeltcomp3$Predator<-'lean Lake Trout'
ltcomp<-rbind(coregcomp3, smeltcomp3)
ltcomp<-subset(ltcomp, Prey3=='total prey')

ggplot(ltcomp, aes(x=Year, y=value, group=Year))+
  #geom_flat_violin(trim=F, alpha=0.75, fill='cyan') +
  #geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.8, color='grey40')+
  geom_boxplot(color='red', outlier.alpha = 0.4, size=1)+
  geom_point(position = position_jitter(width = .15),size = 1, alpha = 0.2, color='grey40')+
  facet_grid(Predator~.)+
  plot_theme+
  labs(y='(prey weight/predator weight)*100', title='Lake Superior Lake Trout total prey fish consumption by morphotype',
       caption='Data: USGS and WI-DNR stomach content analyses')+
  scale_x_continuous(breaks=seq(2012,2020,1))+
  scale_y_continuous(limits=c(0,15))

ggsave(here('Plots and Tables/Stomachs','ns_os_LT_allprey.png'), height=20, width=40, units='cm')


##Fulton's K calculations and plot
spp2.fish<-distinct(spp5, Fish,.keep_all=TRUE)
spp2.fish$l3<-spp2.fish$PRLength_mm^3
spp2.fish$fultonsk<-100000*(spp2.fish$PRWeight_g/spp2.fish$l3)
spp2.fish<-subset(spp2.fish, Predator=='lean lake trout'|Predator=='siscowet lake trout')
spp2.fish<-spp2.fish[complete.cases(spp2.fish),]
#spp2.fish<-aggregate(spp2.fish$fultonsk, by=list(Predator=spp2.fish$Predator, Year=spp2.fish$Year), FUN=mean)%>%
#  renameCol('x','fultonsk.mean')

ggplot(spp2.fish, aes(x=Year, y=fultonsk, group=Year))+
  geom_boxplot()+
  plot_theme+
  facet_grid(Predator~.)+
  scale_x_continuous(breaks=seq(from=2012, to=max(spp2.fish$Year), by=1))+
  scale_y_continuous()+
  labs(y='Fultons K= 100,000*(W/L^3)', title='Lake Superior Lake Trout body condition',
       caption='Data: USGS and WI-DNR stomach content analyses')

ggsave(here('Plots and Tables/Stomachs','ns_os_LT_fultonsK.png'), height=20, width=40, units='cm')

##THIS STUFF NOT WORKING RN##
##consumption wheel attempt
prop.table1<-as.data.frame(prop.table1)
prop.table2<-melt(prop.table1, id.vars=c('Prey'))
prop.table2<-prop.table2%>%
  renameCol('variable', 'Species')%>%
  renameCol('value','Proportion')

classify<-data.frame(Prey=c('adult insect','aquatic invertebrate','aquatic vegetation','burbot','calanoida','cladocera','coregonus','diporeia',
                            'fish other','mysis','other','rainbow smelt','salmonid','sculpin','stickleback','terrestrial insect','terrestrial vegetation'),
                     Classify=c('Invertebrate','Invertebrate',NA,'Fish','Invertebrate','Invertebrate','Fish','Invertebrate','Fish',
                                'Invertebrate',NA,'Fish','Fish','Fish','Fish', 'Invertebrate',NA))
prop.table3<-merge.data.frame(prop.table2, classify)
prop.table3<-prop.table3[complete.cases(prop.table3),]
prop.table4<-merge.data.frame(prop.table1, classify)
prop.lean<-prop.table4%>%
  select(c(1,3,5))
prop.sisc<-prop.table4%>%
  select(c(1,4,5))
prop.burb<-prop.table4%>%
  select(c(1,2,5))


  
