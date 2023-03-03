###############################################################################################
## Create a GA version standard.csv   ##
## Author: Yanran Li                  ##
## Date: 02/18/23                     ##
###############################################################################################

library(tidyverse)
library(tidycensus)
library(tigris)
library(sp)
library(stringr)
library(haven)
library(reshape2)
library(rstudioapi)
library(dplyr)

## set working directory to wherever this file is located ##
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))


ga_premort_by_age <- read.csv('./data/ga_premort_by_age.csv',
                    stringsAsFactors = F,header=T)

ga_premort_by_age <- ga_premort_by_age[0:(nrow(ga_premort_by_age)-2),c(2,8)]
names(ga_premort_by_age) <- c("age", "ndeaths_1yr")

ga_premort_by_age$ndeaths_1yr <- as.numeric(gsub(",", "", ga_premort_by_age$ndeaths_1yr))/5
agecat<-paste0('Age',c('0-4','5-9','10-14','15-17','18-19','20-24','25-29','30-34','35-44','45-54','55-64'))
realign_fineage<-c(1,1,2:8,9,9,10,10,11,11)
ga_premort_by_age$age <- realign_fineage
ga_premort_by_age<-merge(ga_premort_by_age,data.frame('age'=1:length(agecat),'agecat'=agecat),by='age')

load("ga_merged_denom_cov_data.RData")

age_strat_dat <- subset(adat,race=='total')

mort_ref_1yr<-aggregate(ce_pop~agecat,data=age_strat_dat,FUN=sum,na.rm=T)
mort_ref_1yr <- merge( ga_premort_by_age, mort_ref_1yr, by="agecat")

mort_ref_1yr <- aggregate(ndeaths_1yr~agecat+ce_pop+age,data=mort_ref_1yr,FUN=sum,na.rm=T)
mort_ref_1yr$ndeaths_1yr <- round(mort_ref_1yr$ndeaths_1yr,0)

mort_ref_1yr <- mort_ref_1yr[c(1,4,2)]
#mort_ref_1yr<-aggregate(cbind(ndeaths_1yr,ce_pop)~agecat,data=subset(adat,race=='total'),FUN=sum,na.rm=T)

names(mort_ref_1yr)[c(2:3)]<-c('stdcount','stdpop')

write.csv(mort_ref_1yr,file='standard_ga.csv',row.names = F)
