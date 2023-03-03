###############################################################################################
## Create a dataset combining census, ACS, and differential private population counts        ##
## in long-form (age/race/sex stratified) for MA census tracts, and merge in mortality data  ##
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
library(RColorBrewer)
library(purrr)

## set working directory to wherever this file is located ##
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#################
## 1. ACS DATA ##
#################

## variable names ##
v12 <- load_variables(2012, "acs5")

##extract using tidycensus ##
ga_acs<- get_acs(geography = "tract",
                 variables = c(## total pop <65
                   paste0('B01001_',str_pad(as.character(c(3:19,27:43)),width=3,side='left',pad='0')),
                   ## non-hispanic white <65
                   paste0('B01001H_',str_pad(as.character(c(3:13,18:28)),width=3,side='left',pad='0')),
                   ## black <65
                   paste0('B01001B_',str_pad(as.character(c(3:13,18:28)),width=3,side='left',pad='0')),
                   ## american indian/alaska native <65
                   paste0('B01001C_',str_pad(as.character(c(3:13,18:28)),width=3,side='left',pad='0')),
                   ## asian <65
                   paste0('B01001D_',str_pad(as.character(c(3:13,18:28)),width=3,side='left',pad='0')),
                   ## native hawaiian/pacific islander
                   paste0('B01001E_',str_pad(as.character(c(3:13,18:28)),width=3,side='left',pad='0')),
                   ## hispanic
                   paste0('B01001I_',str_pad(as.character(c(3:13,18:28)),width=3,side='left',pad='0'))
                 ),
                 state = "GA",year=2012)



# d1101<- get_decennial(geography = "tract",
#                       variables=v12,
#                       state = "MA",year=2010,sumfile='sf1')

## organize ##
ga_acs<-as.data.frame(ga_acs)

## racevar is a variable that tells us what racegroup the row represents ##
ga_acs$racevar<-substr(ga_acs$variable, 1, 7)
## vnum is a variable that tells us the sex and age group the row represents ##
ga_acs$vnum<-as.numeric(substr(ga_acs$variable,nchar(ga_acs$variable)-2,nchar(ga_acs$variable)))

## create a dataset with proper race and sex variables and an age group variable with consistent groupings (called rxsxa) based on the racevar and vnum variables ##
## age ranges ##
agecat<-paste0('Age',c('0-4','5-9','10-14','15-17','18-19','20-24','25-29','30-34','35-44','45-54','55-64'))

rxsxa<-ga_acs[!duplicated(ga_acs[,c('racevar','vnum')]),c('racevar','vnum')]
rxsxa<-rxsxa[order(rxsxa$racevar,rxsxa$vnum),]

realign_fineage<-c(1:5,6,6,6,7,8,9,9,10,10,11,11,11)
rxsxa$age<-c(rep(realign_fineage,2),rep(1:length(agecat),2*(length(unique(rxsxa$racevar))-1)))
rxsxa$sex<-c(rep(c('m','f'),each=length(realign_fineage)),rep(rep(c('m','f'),each=length(agecat)),length(unique(rxsxa$racevar))-1))
rxsxa<-merge(rxsxa,data.frame('age'=1:length(agecat),'agecat'=agecat),by='age')
rxsxa<-merge(rxsxa,data.frame(
  'race'=c('total','white','black','am_indian','asian','pac_islander','hispanic'),
  'racevar'=paste0('B01001',c('_','H','B','C','D','E','I'))),by='racevar')

## merge this new dataset into ma_acs to add proper race, age, sex variables ##
ga_acs<-merge(ga_acs,rxsxa,by=c('racevar','vnum'))

## need to now aggregate population size and MOE where applicable due to the inconsistent age groupings ##
temp1<-aggregate(estimate~GEOID+race+sex,data=ga_acs,
                 FUN=sum,na.rm=T)
names(temp1)[4]<-'acs_pop'

temp2<-aggregate(moe~GEOID+race+sex,data=ga_acs,
                 FUN = function(x) 1.96*(sqrt(sum((x/1.645)^2,na.rm=T))))
names(temp2)[4]<-'acs_moe'

ga_acs<-merge(temp1,temp2,by=c('GEOID','race','sex'))

ga_acs<-ga_acs[order(ga_acs$GEOID),]

## add in the poverty computed from ACS ##
# ice<-read_sas(data_file = 'icemeasures_acs_0812_12_12_18.sas7bdat')
# ice<-as.data.frame(ice)
# ice<-ice[,c('GEOid2','perc_belowpov','ICEracewb','ICEinc','ICEwbinc')]
# names(ice)<-c('GEOID','acs_pov_pct','acs_ice_racewb','acs_ice_inc','acs_ice_raceinc')
# 

## grab the poverty data from 2020 ACS
variables_dict <- tibble::tribble(
    ~var,          ~shortname,      ~desc,
    "B23024_001",  'poverty_denominator', "20-64 year old total population for poverty estimates",
    "B23024_002",  'poverty_count',    "20-64 year old estimate of income below poverty level",
  )

acs_pov_data <- get_acs(
  geography = 'tract',
  #geometry = TRUE,
  state = "GA",
  year = 2012,
  variables = variables_dict$var)

acs_pov_data<-as.data.frame(acs_pov_data)

# pivot to a wide format for renaming, dropping the margin of error data
acs_pov_data <- acs_pov_data %>% dplyr::select(-moe) %>% pivot_wider(names_from = variable, values_from = estimate)

rename_vars <- setNames(variables_dict$var, variables_dict$shortname)
acs_pov_data <- acs_pov_data %>% rename(!!rename_vars)

# calculate percent poverty
acs_pov_data <- acs_pov_data %>%
  mutate(acs_pov_pct = 100*poverty_count  / poverty_denominator) %>%
  dplyr::select(GEOID,poverty_count,poverty_denominator,acs_pov_pct)


ga_acs<-merge(ga_acs,acs_pov_data,by='GEOID')

save(ga_acs,file='ga_acs_data.RData')
rm(list=ls())

####################
## 2. CENSUS DATA ##
####################

## census 2010 data ##
## variable names ##
#v10 <- load_variables(2010, "sf1")
racemg<-data.frame('race'=c('total','white','black','am_indian','asian','pac_islander','hispanic'),'racevar'=paste0('P012',c('0','I','B','C','D','E','H')))
nums65<-str_pad(c(3:19,27:43),width=3,side='left',pad='0')
vnames<-c(paste0('P012',nums65),paste0(rep(racemg$racevar[2:nrow(racemg)],each=length(nums65)),nums65))

##extract using tidycensus ##
ga_ce<-get_decennial(geography = "tract",
                     variables=vnames,
                     state = "GA",year=2010,sumfile='sf1')

## organize ##
ga_ce<-as.data.frame(ga_ce)

## racevar is a variable that tells us what racegroup the row represents ##
ga_ce$racevar<-substr(ga_ce$variable,1,5)
## vnum is a variable that tells us the sex and age group the row represents ##
ga_ce$vnum<-as.numeric(substr(ga_ce$variable,nchar(ga_ce$variable)-2,nchar(ga_ce$variable)))

## create a dataset with proper race and sex variables and an age group variable with consistent groupings (called rxsxa) based on the racevar and vnum variables ##
## age ranges ##
agecat<-paste0('Age',c('0-4','5-9','10-14','15-17','18-19','20-24','25-29','30-34','35-44','45-54','55-64'))

rxsxa<-ga_ce[!duplicated(ga_ce[,c('racevar','vnum')]),c('racevar','vnum')]
rxsxa<-rxsxa[order(rxsxa$racevar,rxsxa$vnum),]

realign_fineage<-c(1:5,6,6,6,7,8,9,9,10,10,11,11,11)
rxsxa$age<-rep(realign_fineage,2*(length(unique(rxsxa$racevar))))
rxsxa$sex<-rep(rep(c('m','f'),each=length(realign_fineage)),length(unique(rxsxa$racevar)))
rxsxa<-merge(rxsxa,data.frame('age'=1:length(agecat),'agecat'=agecat),by='age')
rxsxa<-merge(rxsxa,racemg,by='racevar')

ga_ce<-merge(ga_ce,rxsxa,by=c('racevar','vnum'))

## need to now aggregate population size and MOE where applicable due to the inconsistent age groupings ##
ga_ce<-aggregate(value~GEOID+race+sex,data=ga_ce,
                 FUN=sum,na.rm=T)
names(ga_ce)[4]<-'ce_pop'

## merge in the ice for race from the census (P003002-P003003)/P001001 ##
ice<-get_decennial(geography = "tract",
                   variables=c('P001001','P003002','P003003'),
                   state = "GA",year=2010,sumfile='sf1',output = 'wide')
ice<-as.data.frame(ice)
ice<-data.frame('GEOID'=ice$GEOID,'ce_ice_racewb'=(ice$P003002-ice$P003003)/ice$P001001)

ga_ce<-merge(ga_ce,ice,by='GEOID')

ga_ce<-ga_ce[order(ga_ce$GEOID),]

save(ga_ce,file='ce_data.RData')

rm(list=ls())

################
## 3. DP DATA ##
################

racemg<-data.frame('race'=c('total','white','black','am_indian','asian','pac_islander','hispanic'),'racevar'=paste0('P012',c('0','I','B','C','D','E','H')))
nums65<-str_pad(c(3:19,27:43),width=3,side='left',pad='0')
vnames<-c(paste0('P0120',nums65),paste0(rep(racemg$racevar[2:nrow(racemg)],each=length(nums65)),nums65))

## download/unzip the GA DP data from https://ciser.cornell.edu/data/data-archive/census-2010-dhc-download-center/ ##

## read in the DP data, takes a few mins to read in ##
ga_dp<-read.csv('GA2010DHC.CSV',stringsAsFactors = F)
ga_dp<-read.csv('~/Desktop/Research/Rachel/CT_race_diff_privacy/GA/GA2010DHC.CSV',stringsAsFactors = F)

## only census tract level info ##
ga_dp<-subset(ga_dp,SUMLEV==140)

## create fips code ##
ga_dp$GEOID<-as.character(paste0(ga_dp$STATE,str_pad(ga_dp$COUNTY,width=3,side='left',pad='0'),str_pad(ga_dp$TRACT,width=6,side='left',pad='0')))

## pull out the variables needed to create ice_racewb ##
ice<-ga_dp[,c('GEOID','P0010001','P0030002','P0030003')]

## subset to only variables we will use for denominator ##
ga_dp<-ga_dp[,c('GEOID',vnames)]

## put in long form and organize ##
ga_dp<-melt(data = ga_dp, id.vars = "GEOID", measure.vars = vnames,factorsAsStrings = T)
ga_dp$variable<-as.character(ga_dp$variable)

## racevar is a variable that tells us what racegroup the row represents ##
ga_dp$racevar<-substr(ga_dp$variable,1,5)
## vnum is a variable that tells us the sex and age group the row represents ##
ga_dp$vnum<-as.numeric(substr(ga_dp$variable,nchar(ga_dp$variable)-2,nchar(ga_dp$variable)))

## create a dataset with proper race and sex variables and an age group variable with consistent groupings (called rxsxa) based on the racevar and vnum variables ##
## age ranges ##
agecat<-paste0('Age',c('0-4','5-9','10-14','15-17','18-19','20-24','25-29','30-34','35-44','45-54','55-64'))

rxsxa<-ga_dp[!duplicated(ga_dp[,c('racevar','vnum')]),c('racevar','vnum')]
rxsxa<-rxsxa[order(rxsxa$racevar,rxsxa$vnum),]

realign_fineage<-c(1:5,6,6,6,7,8,9,9,10,10,11,11,11)
rxsxa$age<-rep(realign_fineage,2*(length(unique(rxsxa$racevar))))
rxsxa$sex<-rep(rep(c('m','f'),each=length(realign_fineage)),length(unique(rxsxa$racevar)))
rxsxa<-merge(rxsxa,data.frame('age'=1:length(agecat),'agecat'=agecat),by='age')
rxsxa<-merge(rxsxa,racemg,by='racevar')

ga_dp<-merge(ga_dp,rxsxa,by=c('racevar','vnum'))

## need to now aggregate population size and MOE where applicable due to the inconsistent age groupings ##
ga_dp<-aggregate(value~GEOID+race+sex,data=ga_dp,
                 FUN=sum,na.rm=T)
names(ga_dp)[4]<-'dp_pop'

## merge in the ice for race from the dp data (P0030002-P0030003)/P0010001 ##
ice<-data.frame('GEOID'=ice$GEOID,'dp_ice_racewb'=(ice$P0030002-ice$P0030003)/ice$P0010001)

ga_dp<-merge(ga_dp,ice,by='GEOID')

ga_dp<-ga_dp[order(ga_dp$GEOID),]

save(ga_dp,file='ga_dp_data.RData')

rm(list=ls())

#######################################
## 4. merge ACS, census, and dp data ##
#######################################

## read each processed dataset ##
load('ga_acs_data.RData')

load('ga_ce_data.RData')

load('ga_dp_data.RData')

## merge them by fips code, race, age, and sex ##
mg1<-merge(ga_acs,ga_ce,by=c('GEOID','race','sex'))

adat<-merge(mg1,ga_dp,by=c('GEOID','race','sex'))

save(adat,file='ga_merged_denom_cov_data.RData')
rm(list=ls())

###################################
## 5. merge in mortality counts  ##
###################################

## read/process 2010 MA mortality data ##
mort5yr<-NULL

## read/process 2010 mortality data ##
mort5yr<-read.csv('../data/GA_pre_mort2010.csv',
                  stringsAsFactors = F,header=T)

mort5yr <- as.data.frame(mort5yr[,0:3])

## change column names 
names(mort5yr)[1] <- 'areakey10'
names(mort5yr)[2] <- 'white'
names(mort5yr)[3] <- 'black'


## make "mort5yr" into a long form with 3 columns with name c('areakey10', 'race', 'ndeaths')
long_mort5yr <- mort5yr %>%
  pivot_longer(cols = c(white, black), names_to = "race", values_to = "ndeaths") %>%
  select(areakey10, race, ndeaths) 

long_mort5yr$race <- as.character(long_mort5yr$race)
long_mort5yr$ndeaths <- as.numeric(long_mort5yr$ndeaths)

long_mort5yr <- long_mort5yr %>%
  group_by(areakey10, race) %>%
  summarize(ndeaths = sum(ndeaths)) %>%
  ungroup() %>%
  group_by(areakey10) %>%
  summarize(race = "total", ndeaths = sum(ndeaths), .groups = "drop") %>% # 添加 .groups 参数
  bind_rows(long_mort5yr, .) %>%
  arrange(areakey10)

# delete the last 3 rows
long_mort5yr <- long_mort5yr[0:(nrow(long_mort5yr)-3),]

adat_mort5yr<-process_mort(mort=mort5yr)

names(adat_mort5yr)[grep('ndeaths',names(adat_mort5yr))]<-'ndeaths_5yr'

## merge with the denominator and covariate data ##
load('merged_denom_cov_data.RData')

adat<-merge(adat,adat_mort5yr,by=c('GEOID','race','agecat','sex'),all.x=T)

## missing ndeaths ==> 0 deaths ##
adat$ndeaths_5yr[which(is.na(adat$ndeaths_5yr))]<-0

save(adat,file='merged_pm_denom_cov_data.RData')
