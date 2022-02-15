library(maptools)
library(spdep)
library(MASS)
library(msm)
library(tigris)
library(ggplot2)
library(sf)
library(tidyr)
library(purrr)
library(plyr)

## read in population and pm rate data ##
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
load('bw_adat.RData')

## put in wide format to merge with the shapefile ##
adat_wide<-merge(adat[which(adat$race=='white'),],adat[which(adat$race=='black'),],by='GEOID',suffixes=c('_w','_b'))

## extract CT shapefile ##
ma_shp<-tracts(state = 'MA',year=2010)

## merge with pop and rate data ##
ma_shp<-merge(ma_shp,adat_wide,by.x='GEOID10',by.y='GEOID',all.x=T)

## put in sf format to use nice mapping packages ##
ma_shp<-st_as_sf(ma_shp)

## ??
## remove CTs with NA ce_exp counts ##
ma_shp<-ma_shp[which(ma_shp$exp_counts_ce_w!="NA" | ma_shp$exp_counts_ce_b!="NA"),]

## long form by race ##
shp_plot<-gather(ma_shp,'race','exp_count',
                 c(exp_counts_ce_w,exp_counts_ce_b),factor_key = T)

shp_plot$race<-mapvalues(shp_plot$race,
                              from=c('exp_counts_ce_w','exp_counts_ce_b'),
                              to=c('NHW','Black'))

## get quantiles of expected counts for maps ##
ma_q<-c(0,0.1,0.5,1,5,10,15,max(shp_plot$exp_count,na.rm = T)+1)

shp_plot$exp_factor<-cut(shp_plot$exp_count,breaks=ma_q,labels=c('<0.1','[0.1,0.5)','[0.5,1)','[1,5)','[5,10)','[10,15)','15+'),right=F)

## race-stratified expected counts, MA ##
ma_maps<-ggplot(shp_plot, aes(fill = exp_factor)) +
  geom_sf(colour=NA) +
  scale_fill_brewer(palette = "YlOrRd")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        legend.title=element_blank(),text=element_text(size=18))+
  facet_wrap(~ race)

## read in boston shapefiles ##
bos_shp<-st_read('boston_ct/Census_2010_Tracts.shp')
bos_shp<-merge(bos_shp,adat_wide,by.x='GEOID10',by.y='GEOID',all.x=T)

bos_bbox<-as.numeric(st_bbox(bos_shp))

xlim<-c(bos_bbox[1], bos_bbox[1]+.4*(bos_bbox[3]-bos_bbox[1]))
ylim<-c(bos_bbox[2], bos_bbox[2]+.8*(bos_bbox[4]-bos_bbox[2]))

## remove CTs with zero population (these are the islands off the MA coast) ##
bos_shp<-bos_shp[which(bos_shp$exp_counts_ce_w!="NA" | bos_shp$exp_counts_ce_b!="NA"),]

## long form by race ##
shp_plot<-gather(bos_shp,'race','exp_count',
                 c(exp_counts_ce_w,exp_counts_ce_b),factor_key = T)

shp_plot$race<-mapvalues(shp_plot$race,
                         from=c('exp_counts_ce_w','exp_counts_ce_b'),
                         to=c('NHW','Black'))

## get quantiles of expected counts for maps ##
ma_q<-c(0,0.1,0.5,1,5,10,15,max(shp_plot$exp_count,na.rm = T)+1)

shp_plot$exp_factor<-cut(shp_plot$exp_count,breaks=ma_q,labels=c('<0.1','[0.1,0.5)','[0.5,1)','[1,5)','[5,10)','[10,15)','15+'),right=F)

## race-stratified expected counts, BOS ##  
bos_maps<-ggplot(shp_plot, aes(fill = exp_factor)) +
  geom_sf(colour=NA) +
  coord_sf(xlim=xlim,ylim=ylim)+
  scale_fill_brewer(palette = "YlOrRd")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        legend.title=element_blank(),text=element_text(size=18))+
  facet_wrap(~ race)

m<-cowplot::plot_grid(ma_maps,bos_maps,ncol=1)

## save to pdf ##
pdf('maps_expected_counts.pdf',width=7,height=6)
print(m)
dev.off()

