###########################################################################################
##   Create a dataset combining percentage of black population for each CT in MA & GA    ##
###########################################################################################

library(tidyverse)
library(tidycensus)
library(tigris)
library(sp)
library(stringr)
library(haven)
library(reshape2)
library(rstudioapi)
library(dplyr)
library(RColorBrewer)
library(purrr)
library(sf)

## set working directory to wherever this file is located ##
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))


##################
##  CENSUS DATA ##
##################

## census 2010 data ##
## variable names ##
#v10 <- load_variables(2010, "sf1")
v_black_total <- c("P008001", "P008004")
##extract using tidycensus ##
ma_black_total <-get_decennial(geography = "tract",
                     variables=v_black_total,
                     state = "MA",year=2010,sumfile='sf1')


# Load MA census tract data
ma_tracts <- st_read("./shp_files/tl_2010_25_tract10/tl_2010_25_tract10.shp")


# make ma_black_total a wide data frame
ma_black_total <- as.data.frame(ma_black_total)
ma_pct_black <- merge(ma_black_total[ma_black_total$variable == "P008004", ], ma_black_total[ma_black_total$variable == "P008001", ], by = c("GEOID", "NAME"))
ma_pct_black <- ma_pct_black[, c("GEOID", "NAME", "value.x", "value.y")]
names(ma_pct_black) <- c("GEOID", "NAME", "black_pop", "total_pop")
ma_pct_black$pct_black <- ma_pct_black$black_pop / ma_pct_black$total_pop *100

## extract CT shapefile ##
ma_shp<-tracts(state = 'MA',year=2010)

## merge with pop and rate data ##
ma_shp<-merge(ma_shp,ma_pct_black,by.x='GEOID10',by.y='GEOID',all.x=T)

## put in sf format to use nice mapping packages ##
ma_shp<-st_as_sf(ma_shp)  #1478*19

#quantile(shp_plot0527$fit0527_mape, na.rm = T)
# 0.1589031 0.3059507 0.5038545 0.6941363 1.7476195 

ma_q <-c(ma_shp$pct_black, ga_shp$pct_black)
quantile(ma_q,na.rm = T)
ma_q<-c(0,2,5, 10, 25,50,75,100)


ma_shp$exp_factor<-cut(ma_shp$pct_black,breaks=ma_q,
                             labels=c('<2','[2,5)','[5,10)','[10,25)','[25,50)',
                                      '[50,75)','75+'),right=F)

## race-stratified expected counts, MA ##
ma_maps_b_pct<-ggplot(ma_shp, aes(fill = exp_factor)) +
  geom_sf(colour=NA) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "white", na.translate = F)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.title=element_blank(),text=element_text(size=18))+
  guides(fill = FALSE)
ma_maps_b_pct

## read in boston shapefiles ##
bos_shp<-st_read('../boston_ct/Census_2010_Tracts.shp')
bos_shp<-merge(bos_shp,ma_pct_black,by.x='GEOID10',by.y='GEOID',all.x=T)

bos_bbox<-as.numeric(st_bbox(bos_shp))

xlim<-c(bos_bbox[1], bos_bbox[1]+.4*(bos_bbox[3]-bos_bbox[1]))
ylim<-c(bos_bbox[2], bos_bbox[2]+.8*(bos_bbox[4]-bos_bbox[2]))

## remove CTs with zero population (these are the islands off the MA coast) ##
#bos_shp<-bos_shp[which(bos_shp$exp_counts_ce_w!="NA" | bos_shp$exp_counts_ce_b!="NA"),]


bos_shp$exp_factor<-cut(bos_shp$pct_black,breaks=ma_q,
                        labels=c('<2','[2,5)','[5,10)','[10,25)','[25,50)',
                                 '[50,75)','75+'),right=F)
## race-stratified expected counts, BOS ##  
bos_maps_b_pct<-ggplot(bos_shp, aes(fill = exp_factor)) +
  geom_sf(colour=NA) +
  coord_sf(xlim=xlim,ylim=ylim)+
  scale_fill_brewer(palette = "YlOrRd", na.value = "white", na.translate = F)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.title=element_blank(),text=element_text(size=18))

bos_maps_b_pct


##########
### GA ###
##########

##extract using tidycensus ##
ga_black_total <-get_decennial(geography = "tract",
                               variables=v_black_total,
                               state = "GA",year=2010,sumfile='sf1')

# make ma_black_total a wide data frame
ga_black_total <- as.data.frame(ga_black_total)
ga_pct_black <- merge(ga_black_total[ga_black_total$variable == "P008004", ], ga_black_total[ga_black_total$variable == "P008001", ], by = c("GEOID", "NAME"))
ga_pct_black <- ga_pct_black[, c("GEOID", "NAME", "value.x", "value.y")]
names(ga_pct_black) <- c("GEOID", "NAME", "black_pop", "total_pop")
ga_pct_black$pct_black <- ga_pct_black$black_pop / ga_pct_black$total_pop *100

## extract CT shapefile ##
ga_shp<-tracts(state = 'GA',year=2010)

## merge with pop and rate data ##
ga_shp<-merge(ga_shp,ga_pct_black,by.x='GEOID10',by.y='GEOID',all.x=T)

## put in sf format to use nice mapping packages ##
ga_shp<-st_as_sf(ga_shp)  #1478*19

#quantile(shp_plot0527$fit0527_mape, na.rm = T)
# 0.1589031 0.3059507 0.5038545 0.6941363 1.7476195 
ma_q<-c(0,2,5, 10, 25,50,75,100)


ga_shp$exp_factor<-cut(ga_shp$pct_black,breaks=ma_q,
                       labels=c('<2','[2,5)','[5,10)','[10,25)','[25,50)',
                                '[50,75)','75+'),right=F)

## race-stratified expected counts, MA ##
ga_maps_b_pct<-ggplot(ga_shp, aes(fill = exp_factor)) +
  geom_sf(colour=NA) +
  scale_fill_brewer(palette = "YlOrRd", na.value = "white", na.translate = F)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.title=element_blank(),text=element_text(size=18))
ga_maps_b_pct








## read in Atlanta shapefiles ##

## extract CT shapefile ##
fulton_shp<-tracts(state = 'GA', county =c("Fulton","Dekalb") , year=2010)

## merge with pop and rate data ##
fulton_shp<-merge(fulton_shp,ga_pct_black,by.x='GEOID10',by.y='GEOID',all.x=T)

## put in sf format to use nice mapping packages ##
fulton_shp<-st_as_sf(fulton_shp)  #1478*21


## manually extract the NAME10 in Atlanta City ##
dekalb_name <- read.table("./shp_files/atl_ct/DC10CT_C13089_CT2MS.txt", header = TRUE,sep = ";")
#dekalb_name <- dekalb_name[c(1:11, 111:112,203),] 2020 DC map
dekalb_name <- dekalb_name[c(1:10),]

fulton_name <- read.table("./shp_files/atl_ct/DC10CT_C13121_CT2MS.txt", header = TRUE,sep = ";")
#fulton_name <- fulton_name[c(1:157,321:325),]
fulton_name <- fulton_name[c(1:114,135, 202,204),]



atl_name <- rbind(dekalb_name, fulton_name)

## remove CTs with zero population (these are the islands off the MA coast) ##
atl_shp<-fulton_shp[fulton_shp$GEOID10 %in% atl_name$CODE,]

atl_bbox<-as.numeric(st_bbox(atl_shp))

xlim<-c(atl_bbox[1], atl_bbox[1]+.4*(atl_bbox[3]-atl_bbox[1]))
ylim<-c(atl_bbox[2], atl_bbox[2]+.8*(atl_bbox[4]-atl_bbox[2]))

xlim<-c(atl_bbox[1], atl_bbox[3])
ylim<-c(atl_bbox[2], atl_bbox[4])

## remove CTs with zero population (these are the islands off the MA coast) ##
#bos_shp<-bos_shp[which(bos_shp$exp_counts_ce_w!="NA" | bos_shp$exp_counts_ce_b!="NA"),]


atl_shp$exp_factor<-cut(atl_shp$pct_black,breaks=ma_q,
                        labels=c('<2','[2,5)','[5,10)','[10,25)','[25,50)',
                                 '[50,75)','75+'),right=F)

## race-stratified expected counts, BOS ##  
atl_maps_b_pct<-ggplot(atl_shp, aes(fill = exp_factor)) +
  geom_sf(colour=NA) +
  coord_sf(xlim=xlim,ylim=ylim)+
  scale_fill_brewer(palette = "YlOrRd", na.value = "white", na.translate = F)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_line(colour = 'transparent'),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.position = "none",
        legend.title=element_blank(),text=element_text(size=18))

atl_maps_b_pct


ma_bos<-cowplot::plot_grid(ma_maps_b_pct,ga_maps_b_pct,
                           bos_maps_b_pct,atl_maps_b_pct,ncol=2)
atl_legend <- get_legend(
  atl_maps_b_pct + 
    guides(fill = guide_legend(nrow = 1))+
    theme(legend.position = "bottom", legend.direction = "horizontal")
)

combined_plot <- plot_grid(ma_bos, atl_legend,ncol=1,rel_heights = c(1, .1))

combined_plot

