## pseudo-simulations using Vintage 2022-05-27 differential privacy data ##
## Generate data with "true" census pop and use 0316 dp data in models ##
## date: 08/02/22 ##


## load libraries 
library(dplyr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(sp)
library(stringr)
library(haven)
library(reshape2)
library(rstudioapi)
library(RColorBrewer)
library(purrr)
library(maptools)
library(spdep)
library(MASS)
library(msm)
library(CARBayes)
library(epitools)

###########################################
## 1. expected counts and covariate info ##
###########################################

set.seed(2)

## load the pre-prepped MA CT-level ACS/census/dp data ##
load('merged_denom_cov_data5.RData')

# aggregate across the sex variable
agg_sex <- aggregate(ce_pop~GEOID+race+agecat,data=adat, 
                     FUN = sum,na.rm=T)


agg_sex_dp <- aggregate(dp_pop~GEOID+race+agecat,data=adat, 
                        FUN = sum,na.rm=T)

agg_sex_dp0527 <- aggregate(dp0527_pop~GEOID+race+agecat,data=adat, 
                            FUN = sum,na.rm=T)

agg_sex_dp22 <- aggregate(dp22_pop~GEOID+race+agecat,data=adat, 
                          FUN = sum,na.rm=T)

# dimnames
agelabs <- c("0-4", "5-9", "10-14", "15-17", "18-19", "20-24", "25-29", "30-34", "35-44", "45-54", "55-64")

# count
count <- rep(0, length(agelabs))

# stdcount & stdpop
std_pm <- read.csv("standard.csv")
stdcount <- std_pm$stdcount
stdpop <- std_pm$stdpop

# ce matrix
ce_matrix <- matrix(0,ncol = length(unique(adat$GEOID)),
                    nrow=length(unique(adat$race)), 
                    dimnames = list(unique(adat$race),
                                    unique(adat$GEOID)))
for (i in unique(adat$GEOID)){
  for (j in unique(adat$race)){
    count = count
    pop = agg_sex %>% filter(GEOID ==i, race==j) 
    output <- ageadjust.indirect(count, pop$ce_pop, stdcount, stdpop, stdrate = NULL, conf.level = 0.95)
    ce_matrix[j, i] <- output$sir[2]
  }
}

# dp matrix
dp_matrix <- matrix(0,ncol = length(unique(adat$GEOID)),
                    nrow=length(unique(adat$race)), 
                    dimnames = list(unique(adat$race),
                                    unique(adat$GEOID)))
for (i in unique(adat$GEOID)){
  for (j in unique(adat$race)){
    count = count
    pop_dp = agg_sex_dp %>% filter(GEOID ==i, race==j) 
    output <- ageadjust.indirect(count, pop_dp$dp_pop, stdcount, stdpop, stdrate = NULL, conf.level = 0.95)
    dp_matrix[j, i] <- output$sir[2]
  }
}

# 0527 new dp matrix
dp0527_matrix <- matrix(0,ncol = length(unique(adat$GEOID)),
                        nrow=length(unique(adat$race)), 
                        dimnames = list(unique(adat$race),
                                        unique(adat$GEOID)))
for (i in unique(adat$GEOID)){
  for (j in unique(adat$race)){
    count = count
    pop_dp0527 = agg_sex_dp0527 %>% filter(GEOID ==i, race==j) 
    output <- ageadjust.indirect(count, pop_dp0527$dp0527_pop, stdcount, stdpop, stdrate = NULL, conf.level = 0.95)
    dp0527_matrix[j, i] <- output$sir[2]
  }
}

# 0316 new dp matrix
dp22_matrix <- matrix(0,ncol = length(unique(adat$GEOID)),
                      nrow=length(unique(adat$race)), 
                      dimnames = list(unique(adat$race),
                                      unique(adat$GEOID)))
for (i in unique(adat$GEOID)){
  for (j in unique(adat$race)){
    count = count
    pop_dp22 = agg_sex_dp22 %>% filter(GEOID ==i, race==j) 
    output <- ageadjust.indirect(count, pop_dp22$dp22_pop, stdcount, stdpop, stdrate = NULL, conf.level = 0.95)
    dp22_matrix[j, i] <- output$sir[2]
  }
}

# long form 
# merge the census and differential private expected counts
ce_l <- data.frame(t(ce_matrix)) %>% 
  mutate(GEOID = dimnames(data.frame(t(ce_matrix)))[[1]]) %>% 
  gather(race, exp_counts_ce, -GEOID)
dp_l <- data.frame(t(dp_matrix)) %>% 
  mutate(GEOID = dimnames(data.frame(t(dp_matrix)))[[1]]) %>% 
  gather(race, exp_counts_dp, -GEOID)
dp0527_l <- data.frame(t(dp0527_matrix)) %>% 
  mutate(GEOID = dimnames(data.frame(t(dp0527_matrix)))[[1]]) %>% 
  gather(race, exp_counts_dp0527, -GEOID)
dp22_l <- data.frame(t(dp22_matrix)) %>% 
  mutate(GEOID = dimnames(data.frame(t(dp22_matrix)))[[1]]) %>% 
  gather(race, exp_counts_dp22, -GEOID)


exp_l <- ce_l %>% left_join(dp_l, by=c("GEOID", "race")) %>% 
  left_join(dp0527_l, by=c("GEOID", "race")) %>% 
  left_join(dp22_l, by=c("GEOID", "race"))


cov_l <- aggregate(cbind(acs_pov_pct)~GEOID+race,data=adat,
                   FUN = mean)

## merge these aggregated datasets ##
adat<-merge(exp_l,cov_l,by=c('GEOID','race'))

## check CTs' expected counts ##
adat<-subset(adat,exp_counts_ce>0 & exp_counts_dp>0 & 
               exp_counts_dp0527>0& exp_counts_dp22>0)
adat<-adat[order(adat$GEOID),]

# remove all the race groups from the dataset expect black and white
adat <- adat %>% filter(race == 'white' | race == 'black')

## add black & white index
adat$bw_ind<-0
adat$bw_ind[which(adat$race=='black')]<-1

## create the matrices simulate ##
X<-cbind(1,adat$bw_ind,adat$acs_pov_pct)
colnames(X)<-c('intercept','bw','pov')
P<-matrix(adat$exp_counts_dp0527) 
Ptrue<-matrix(adat$exp_counts_ce)

N<-nrow(X)
Nct<-length(unique(adat$GEOID))

###########################################
## 2. adjacency matrix for census tracts ##
###########################################

## extract shapefile ##
#sf_roads <- readShapePoly("/Users/liyanran/Desktop/Research/Rachel/census_diff_privacy-master/sims/tl_2010_25_tract10/tl_2010_25_tract10.shp")
# sf_roads <- readShapePoly("./tl_2010_25_tract10/tl_2010_25_tract10.shp")
# foo = poly2nb(sf_roads, queen=TRUE, row.names=sf_roads@data$GEOID10)
# W=nb2mat(foo,style='B') # large matrix 1478*1478
W <- read.csv("W.csv")
rownames(W) <- W[,1]
W <- W[,-1]


##########################################################################
## 3. align ordering of adjacency matrix and covariates/population data ##
##########################################################################

## census tracts in W with matches in the data ##
tractrm<-which(rownames(W) %in% as.character(adat$GEOID))

W<-W[tractrm,tractrm]

W<-W[order(as.numeric(rownames(W))),order(as.numeric(rownames(W)))]

identical(unique(as.character(adat$GEOID)),rownames(W)) # 1460*1460

########################################
## 4. create simulated data structure ##
########################################

## spatial information ##
Dw<-diag(rowSums(W))
#rho_bds<-eigen(sqrt(solve(Dw))%*%W%*%sqrt(solve(Dw)))$values
#print(1/min(rho_bds))
#print(1/max(rho_bds))
rho<-.2

## lambda=scalar background rate ##
# set the lambda object equal to 1
lambda <- 1

## beta coefficients for covariates ##
beta<-c(-0.5,0.4,0.01)

###################################
## 5. set up storage for results ##
###################################

reps<-100
nburn<-10000
nsamp<-30000
cars<-matrix(NA,nrow=reps,ncol=length(beta)*3+9)
fitted_values<-matrix(NA,nrow=reps,ncol=dim(adat)[1])
thetas<-matrix(NA,nrow=reps,ncol=dim(adat)[1])


########################
## 6. run simulations ##
########################

simnum = 3 #dp20


set.seed(simnum)

W <- as.matrix(W)

for (xx in 1:reps){
  
  ## U (spatial component) ##
  # Simulate from a Multivariate Normal Distribution
  U<-matrix(mvrnorm(n=1,mu=rep(0,Nct),Sigma=solve(Dw-(rho*W))))
  U<-U[as.numeric(as.factor(adat$GEOID)),]
  
  ## V (unsturctured variance component 无结构方差组分) ##
  V<-matrix(rnorm(n=N,mean=0,sd=.5))
  
  ## generate outcome ##
  
  theta<-X%*%beta+U+V
  
  Y<-matrix(rpois(n=N,lambda=lambda*Ptrue*exp(theta))) #Poisson Distribution
  
  ################################################
  ## FIT STANDARD SPATIAL CAR  MODEL 条件自回归 ##
  ################################################
  adat$Y<-Y
  adat$offs<-log(lambda*P)
  results_cars<-S.CARmultilevel(Y~bw_ind+acs_pov_pct+offset(offs),family="poisson",data=adat,
                                ind.area=as.numeric(as.factor(adat$GEOID)),
                                ind.re=as.factor(1:N),
                                W=W,burnin=nburn,n.sample=nsamp)
  
  cars[xx,]<-c(apply(results_cars$samples$beta,2,mean),
               apply(results_cars$samples$beta,2,quantile,.025),
               apply(results_cars$samples$beta,2,quantile,.975),
               apply(results_cars$samples$tau2,2,mean),
               apply(results_cars$samples$tau2,2,quantile,.025),
               apply(results_cars$samples$tau2,2,quantile,.975),
               apply(results_cars$samples$sigma2,2,mean),
               apply(results_cars$samples$sigma2,2,quantile,.025),
               apply(results_cars$samples$sigma2,2,quantile,.975),
               apply(results_cars$samples$rho,2,mean),
               apply(results_cars$samples$rho,2,quantile,.025),
               apply(results_cars$samples$rho,2,quantile,.975))
  fitted_values[xx,]<-fitted(results_cars)
  thetas[xx,] <- exp(theta)
  
}

cars<-as.data.frame(cars)
names(cars)<-c(paste0('beta',0:(length(beta)-1)),paste0('beta',0:(length(beta)-1),'_lo'),
               paste0('beta',0:(length(beta)-1),'_hi'),'tau2','tau2_lo','tau2_hi',
               'sigma2','sigma2_lo','sigma2_hi','rho','rho_lo','rho_hi')
fitted_values<-as.data.frame(fitted_values)
exp_theta <- as.data.frame(thetas)


save(cars,file=paste0('results/0coef',simnum,'.RData'))
save(fitted_values,file=paste0('results/0fitted_value',simnum,'.RData'))
save(exp_theta, file=paste0('results/0exp_theta',simnum,'.RData'))
