# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#####################################################################################
# 03_sightability_simulations.R
# script to simulate sightability models to inform sightabilty survey study design
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 9-Mar-2021
#####################################################################################

# overall process:
#- Run basic mHT sightablity model simulations
# Steinhorst and Samuel (1989) assumptions:
# 1. The population is geographically and demographically closed.
# 2. Groups of animals are independently observed.
# 3. Observed groups are completely enumerated and observed only once.
# 4. The survey design for land units can be specified.
# 5. The probability of observing a group is known or can be estimated

data("obs.m")
data("exp.m")
data("sampinfo.m")

# experimental data frame consists of:
# year (year of test trial)
# observed (binary; 1 if telem animal observed, 0 otherwise)
# voc (covariate; amount of screening cover within 10 m of first animal seen)
# grpsize (group size)
# each row represents an independent sightability trial with observed representing the random variables
exp.m[1:5,]
glimpse(exp.m)
exp.m %>% group_by(year) %>% count(observed) # 39 sightability trials in 2005, 37 in 2006 and 48 in 2007

# operational data frame consists of:
# each row corresponds to an independently sighted group with animal-specific covariates
# subunit is the sample plot identifier (EPU in our case?)
# stratum is the stratum identifier (should take on value of 1 for non-stratified surveys)
obs.m[1:5,]
glimpse(obs.m)

# sampling information data frame consists of:
# number of sampled units nh in each stratum
# population Nh in each stratum
sampinfo.m


# to fit a specified logistic regression and estimate population abundance in a single step use Sight.Est function
# code below models detection probability as a function of visual obstruction
# note that an intercept only model would assume detection probabilities are constant, specified using observed~1
est.2004 <- Sight.Est(observed~voc, odat = subset(obs.m, year==2004),
                        sdat = exp.m, sampinfo = subset(sampinfo.m, year==2004))
print(est.2004)
# Variance component estimates provide useful information for improving future surveys
# VarSamp = sampling uncertainty; reduced by surveying a larger number of aerial plots or implement more efficient sampling design
# VarMod = parameter uncertainty; reduced by conducting more sightability trials

# to fit a specified logistic regression and estimate population abundance in a single step use Sight.Est function
# code below models detection probability as a function of visual obstruction
# note that an intercept only model would assume detection probabilities are constant, specified using observed~1
# below is for bootstrapping when sightability trials are low (i.e., < 100)
est.2004 <- Sight.Est(observed~voc,
                      odat = subset(obs.m, year==2004),
                      sdat = subset(exp.m, year==2005),
                      sampinfo = subset(sampinfo.m, year==2004),
                      method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = FALSE)
print(est.2004)
print(format(round(est.2004$est, 0), big.mark = ","), quote = FALSE)


boot.est <- Sight.Est(observed~voc,
                      odat = subset(obs.m, year==2004),
                      sdat = subset(exp.m, year==2005),
                      sampinfo = subset(sampinfo.m, year==2004),
                      method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
print(format(round(boot.est$est, 0), big.mark = ","), quote = FALSE)

################################################################################################
###--- let's create some simulated data to play with
# use parameters adapted from 2020 survey data and Sechelt Peninsulsa
# Sechelt Peninsula = estimated population 222
# group size = 1-52, mean = 12; 2019 collaring data notes 16 groups with mean = 18, range 1-42
# on average, groups contained 0.3 bulls, 1.4 spikes, 13 cows and 3.3 calves
# calf:cow ratio = 30:100 (range 14-39 : 100)
# yearling:cow ratio = 21:100
# bull:cow ratio = 25:100

#- function to estimate number of animals per age-sex class based on classification ratios

est.sex.age.class.pop <- function(pop.size=222, ratio.value=30){
  round(ratio.value / (ratio.value+100)*pop.size,0)
}

# est.bull.Sechelt <- est.sex.age.class.pop(pop.size = 222, ratio.value = 25)
# est.calf.Sechelt <- est.sex.age.class.pop(pop.size = 222, ratio.value = 30)
# est.spike.Sechelt <- est.sex.age.class.pop(pop.size = 222, ratio.value = 21)
# est.cow.Sechelt <- 222 - (est.bull.Sechelt + est.calf.Sechelt + est.spike.Sechelt)

# for random numbers use rnorm with mean and sd specified for sex/age-class, bound by range of group sizes
# n = number of groups (i.e., sightability trials)
sum(round(rtruncnorm(n=20, a=0, b=42, mean=0.3, sd=1.5),0))

# use these functions together to generate simluated sightability trial dataset


###--- create simulted observation data.frame
# operational data frame consists of:
# each row corresponds to an independently sighted group with animal-specific covariates
# subunit is the sample plot identifier (EPU in our case?)
# stratum is the stratum identifier (should take on value of 1 for non-stratified surveys)
obs.m[1:5,]
glimpse(obs.m)

sim.obs.m.fn <- function(year=c(2020,2019), stratum=1, subunit=1, bull.ratio=25, calf.ratio=30, spike.ratio=21, pop.size=222, covariates="voc", grpsize=c(0,42),
                         bull.grp.m=0.3, bull.grp.sd=1.5, cow.grp.m=13, cow.grp.sd=10.4, calf.grp.m=3.3, calf.grp.sd=1.5, spike.grp.m=1.4, spike.grp.sd=1.0,
                         n.sghtblty.trls=20){
  bull.grp <- est.sex.age.class.pop(pop.size = pop.size, ratio.value = bull.ratio)
  calf.grp <- est.sex.age.class.pop(pop.size = pop.size, ratio.value = calf.ratio)
  spike.grp <- est.sex.age.class.pop(pop.size = pop.size, ratio.value = spike.ratio)
  cow.grp <- pop.size - (bull.grp + calf.grp + spike.grp)

  bulls <- round(rtruncnorm(n=n.sghtblty.trls, a=grpsize[1], b=bull.grp, mean=bull.grp.m, sd=bull.grp.sd),0)
  calves <- round(rtruncnorm(n=n.sghtblty.trls, a=grpsize[1], b=calf.grp, mean=calf.grp.m, sd=calf.grp.sd),0)
  spikes <- round(rtruncnorm(n=n.sghtblty.trls, a=grpsize[1], b=spike.grp, mean=spike.grp.m, sd=spike.grp.sd),0)
  cows <- round(rtruncnorm(n=n.sghtblty.trls, a=grpsize[1], b=cow.grp, mean=cow.grp.m, sd=cow.grp.sd),0)

  sim.obs.m <- as.data.frame(cbind(bulls, calves, spikes, cows))

  # to make more realistic, if 3 or more bulls then a bachelor group
  sim.obs.m <- sim.obs.m %>% mutate(cows = case_when(bulls >= 3 ~ 0,TRUE ~ cows))
  sim.obs.m <- sim.obs.m %>% mutate(calves = case_when(bulls >= 3 ~ 0,TRUE ~ calves))
  sim.obs.m <- sim.obs.m %>% mutate(spikes = case_when(bulls >= 3 ~ 0,TRUE ~ spikes))

  # fill in rest of observation data frame
  sim.obs.m$year <- rep(year, length.out=n.sghtblty.trls)
  sim.obs.m$stratum <- stratum
  sim.obs.m$subunit <- subunit
  sim.obs.m$total <- sim.obs.m$bulls + sim.obs.m$calves + sim.obs.m$spikes + sim.obs.m$cows
  sim.obs.m$grpsize <- sim.obs.m$total

  # create voc category, ranging from 0 to 1 (proportion of veg)
  sim.obs.m$covariate1 <- runif(n.sghtblty.trls, 0, 1)
  colnames(sim.obs.m)[10] <- covariates[1]

  # format to same order as example dataset - only works if only have voc as covariate
  sim.obs.m <- sim.obs.m[c("year", "stratum", "subunit", "total", "cows", "calves", "bulls", "spikes","voc",
                           "grpsize")]
  return(sim.obs.m)

}


#- for Sechelt peninsula can use the default values of simulation for observation data frame

# create 100 datasets of simulated Sechelt observation data
sim.obs.Sechelt <- vector('list', 100)
names(sim.obs.Sechelt) <- paste0('sim.obs.Sechelt', seq_along(sim.obs.Sechelt))
for(i in seq_along(sim.obs.Sechelt)){
  sim.obs.Sechelt.base <- sim.obs.m.fn()
  sim.obs.Sechelt[[i]] <- sim.obs.Sechelt.base
}


###--- create exp.m simulated data frame
# assume if voc is <0.5 and group is >5 then 0.8 prob of seeing elk
# assume if voc is <0.5 and group is <5 then 0.7 prob of seeing elk
# assume if voc is >0.5 and group is >5 then 0.4 prob of seeing elk
# assume if voc is >0.5 and group is <5 then 0.3 prob of seeing elk

glimpse(exp.m)

sim.exp.m.fn <- function(year=c(2020,2019), covariates="voc", grpsize=c(0,42), grpsize.m=16, grpsize.sd=10.4, n.sghtblty.trls=20,
                         cov.value=0.5, prob.sight=c(0.8, 0.7, 0.4, 0.3)){

  sim.exp.m <- as.data.frame(matrix(nrow=n.sghtblty.trls,ncol=4))
  colnames(sim.exp.m) <- c("year", "observed", covariates, "grpsize")
  sim.exp.m$year <- rep(year, length.out=n.sghtblty.trls)
  sim.exp.m$voc <- runif(n.sghtblty.trls, 0,1)
  sim.exp.m$grpsize <-round(rtruncnorm(n=n.sghtblty.trls, a=grpsize[1], b=grpsize[2], mean=grpsize.m, sd=grpsize.sd),0)

  sim.exp.m$observed <- as.integer(1) # default to 1 to set up case_when function
  sim.exp.m <- sim.exp.m %>% mutate(observed = case_when(voc < cov.value & grpsize >5 ~ rbinom(1, size=1, prob.sight[1]),
                                                         voc < cov.value & grpsize <5 ~ rbinom(1, size=1, prob.sight[2]),
                                                         voc > cov.value & grpsize >5 ~ rbinom(1, size=1, prob.sight[3]),
                                                         voc > cov.value & grpsize <5 ~ rbinom(1, size=1, prob.sight[4]), TRUE ~ observed))
  return(sim.exp.m)

}

sim.exp.m.fn()

#- can use the default values of simulation for experimental data frame

# create 100 datasets of simulated experimental (sightability trial) data
sim.exp.trials <- vector('list', 100)
names(sim.exp.trials) <- paste0('sim.exp.trials', seq_along(sim.exp.trials))
for(i in seq_along(sim.exp.trials)){
  sim.exp.trials.base <- sim.exp.m.fn()
  sim.exp.trials[[i]] <- sim.exp.trials.base
}

head(sampinfo.m)
# the inventory survey data
# number of sampled units nh in each stratum
# population Nh in each stratum

sim.sampinfo.m <- as.data.frame(matrix(nrow=1,ncol=4))
colnames(sim.sampinfo.m) <- c("year","stratum","Nh","nh")
sim.sampinfo.m$year <- 2020
sim.sampinfo.m$stratum <- 1
sim.sampinfo.m$Nh <-220
sim.sampinfo.m$nh <- 10


###--- try fitting the mHT
sim.est.2020 <- Sight.Est(observed~voc,
                      odat = subset(sim.obs.m),
                      sdat = subset(sim.exp.m),
                      sampinfo = subset(sim.sampinfo.m),
                      method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = FALSE)
print(sim.est.2020)
print(format(round(sim.est.2020$est, 0), big.mark = ","), quote = FALSE)


boot.sim.est <- Sight.Est(observed~voc,
                      odat = subset(sim.obs.m),
                      sdat = subset(sim.exp.m),
                      sampinfo = subset(sim.sampinfo.m),
                      method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
print(format(round(boot.sim.est$est, 0), big.mark = ","), quote = FALSE)


###--- need to create function that feeds into simulation paramters and then a loop to run 100 simulations per set of params
