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
library(rstudioapi)

## read in population and pm rate data ##
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))


load("simnum4.RData")
dp0527_fit <- cars
load("simnum5.RData")
dp_fit <- cars
load("simnum6.RData")
ce_fit <- cars #100*2877

dp_fit_mean <- apply(dp_fit, 2, mean)
dp_smr <- dp_fit_mean/adat$exp_counts_dp
dp0527_fit_mean <- apply(dp0527_fit, 2, mean)
dp0527_smr <- dp0527_fit_mean/adat$exp_counts_dp0527
ce_fit_mean <- apply(ce_fit, 2, mean)
ce_smr <- ce_fit_mean/adat$exp_counts_ce

dp0527_fit_mape <- abs((ce_smr - dp0527_smr)/ce_smr) #2877
dp_fit_mape <- abs((ce_smr - dp_smr)/ce_smr) #2877



##################################################################
#######   MAPE map for dp ########################################
##################################################################
load('bw_adat.RData')
adat <- cbind(adat, dp_fit_mape)[,c(1, 2, 8)] #2877*4


## put in wide format to merge with the shapefile ##
adat_wide<-merge(adat[which(adat$race=='white'),],adat[which(adat$race=='black'),],by='GEOID',suffixes=c('_w','_b'))
#  1418*7

## extract CT shapefile ##
ma_shp<-tracts(state = 'MA',year=2010)

## merge with pop and rate data ##
ma_shp<-merge(ma_shp,adat_wide,by.x='GEOID10',by.y='GEOID',all.x=T)

## put in sf format to use nice mapping packages ##
ma_shp<-st_as_sf(ma_shp)  #1478*21

## ?? leave NA 
## remove CTs with NA ce_exp counts ##
# ma_shp<-ma_shp[which(ma_shp$exp_counts_ce_w!="NA" | ma_shp$exp_counts_ce_b!="NA"),]  #1418*27


## long form by race ##
shp_plot<-gather(ma_shp,'race','fit_mape',
                 c(dp_fit_mape_w,dp_fit_mape_b),factor_key = T)

shp_plot$race<-mapvalues(shp_plot$race,
                              from=c('dp_fit_mape_w','dp_fit_mape_b'),
                              to=c('NHW','Black'))


# histogram to find the quantiles shp_plot$fit_mape
quantile(shp_plot$fit_mape, na.rm = T)

ma_q<-c(0,0.001,0.01,0.05,0.1,0.15,0.2,max(shp_plot$fit_mape,na.rm = T)+1)

shp_plot$exp_factor<-cut(shp_plot$fit_mape,breaks=ma_q,
                         labels=c('<0.001','[0.001,0.01)','[0.01,0.05)','[0.05,0.1)','[0.1,0.15)',
                                  '[0.15,0.2)','0.2+'),right=F)

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
#bos_shp<-bos_shp[which(bos_shp$exp_counts_ce_w!="NA" | bos_shp$exp_counts_ce_b!="NA"),]

## long form by race ##
shp_plot<-gather(bos_shp,'race','fit_mape',
                 c(dp_fit_mape_w,dp_fit_mape_b),factor_key = T)

shp_plot$race<-mapvalues(shp_plot$race,
                         from=c('dp_fit_mape_w','dp_fit_mape_b'),
                         to=c('NHW','Black'))

shp_plot$exp_factor<-cut(shp_plot$fit_mape,breaks=ma_q,
                         labels=c('<0.001','[0.001,0.01)','[0.01,0.05)','[0.05,0.1)','[0.1,0.15)',
                                  '[0.15,0.2)','0.2+'),right=F)


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
pdf('maps_mape.pdf',width=7,height=6)
print(m)

#################################################################################
#################################################################################

#### MAPE0527 plot
load('bw_adat.RData')
adat <- cbind(adat, dp0527_fit_mape)[,c(1, 2, 8)] #2877*4


## put in wide format to merge with the shapefile ##
adat_wide<-merge(adat[which(adat$race=='white'),],adat[which(adat$race=='black'),],by='GEOID',suffixes=c('_w','_b'))
#  1418*7

## extract CT shapefile ##
ma_shp<-tracts(state = 'MA',year=2010)

## merge with pop and rate data ##
ma_shp<-merge(ma_shp,adat_wide,by.x='GEOID10',by.y='GEOID',all.x=T)

## put in sf format to use nice mapping packages ##
ma_shp<-st_as_sf(ma_shp)  #1478*21

## ?? leave NA 
## remove CTs with NA ce_exp counts ##
# ma_shp<-ma_shp[which(ma_shp$exp_counts_ce_w!="NA" | ma_shp$exp_counts_ce_b!="NA"),]  #1418*27




shp_plot0527<-gather(ma_shp,'race','fit0527_mape',
                 c(dp0527_fit_mape_w,dp0527_fit_mape_b),factor_key = T)

shp_plot0527$race<-mapvalues(shp_plot0527$race,
                         from=c('dp0527_fit_mape_w','dp0527_fit_mape_b'),
                         to=c('NHW','Black'))

#ma_q<-c(0,0.01,0.05,0.1,0.2,1,1.5,max(shp_plot0527$fit0527_mape,na.rm = T)+1)


shp_plot0527$exp_factor<-cut(shp_plot0527$fit0527_mape,breaks=ma_q,
                             labels=c('<0.001','[0.001,0.01)','[0.01,0.05)','[0.05,0.1)','[0.1,0.15)',
                                      '[0.15,0.2)','0.2+'),right=F)

## race-stratified expected counts, MA ##
ma_maps0527<-ggplot(shp_plot0527, aes(fill = exp_factor)) +
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
#bos_shp<-bos_shp[which(bos_shp$exp_counts_ce_w!="NA" | bos_shp$exp_counts_ce_b!="NA"),]

## long form by race ##
shp_plot0527<-gather(bos_shp,'race','fit0527_mape',
                 c(dp0527_fit_mape_w,dp0527_fit_mape_b),factor_key = T)

shp_plot0527$race<-mapvalues(shp_plot0527$race,
                         from=c('dp0527_fit_mape_w','dp0527_fit_mape_b'),
                         to=c('NHW','Black'))

shp_plot0527$exp_factor<-cut(shp_plot0527$fit0527_mape,breaks=ma_q,
                             labels=c('<0.001','[0.001,0.01)','[0.01,0.05)','[0.05,0.1)','[0.1,0.15)',
                                      '[0.15,0.2)','0.2+'),right=F)

## race-stratified expected counts, BOS ##  
bos_maps0527<-ggplot(shp_plot0527, aes(fill = exp_factor)) +
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

m0527<-cowplot::plot_grid(ma_maps0527,bos_maps0527,ncol=1)

## save to pdf ##
pdf('maps_mape0527.pdf',width=7,height=6)
print(m0527)










