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


# so for random numbers use rnorm with mean and sd specified:
# bulls: mean = 0.3
bulls.grp <- round(abs(rnorm(20, mean = 0.3, sd = 1.5)),0)
sum(bulls.grp)
cows.grp <- round(abs(rnorm(20, mean = 13, sd = 10.4)),0)
sum(cows.grp)
calves.grp <- round(abs(rnorm(20, mean = 3.3, sd = 2)),0)
sum(calves.grp)

sim.obs.m <- as.data.frame(cbind(bulls.grp, cows.grp, calves.grp))

# to make more realistic, if 3 bulls then remove any cows or calves from group
sim.obs.m <- sim.obs.m %>% mutate(cows.grp = case_when(bulls.grp == 3 ~ 0,TRUE ~ cows.grp))

# fill in rest of observation data frame
sim.obs.m$year <- 2020  # specify year 2002
sim.obs.m$stratum <- 1  # not stratified so all stratum = 1
sim.obs.m$subunit <- 1  # EPU?
sim.obs.m$total <- sim.obs.m$bulls.grp + sim.obs.m$cows.grp + sim.obs.m$calves.grp
sim.obs.m$grpsize <- sim.obs.m$total

# create voc category, ranging from 0 to 1 (proportion of veg)
sim.obs.m$voc <- runif(20, 0,1)

###--- create exp.m simulated data frame
# assume if voc is <0.5 and group is >5 then 0.8 prob of seeing elk
# assume if voc is <0.5 and group is <5 then 0.7 prob of seeing elk
# assume if voc is >0.5 and group is >5 then 0.5 prob of seeing elk
# assume if voc is >0.5 and group is <5 then 0.4 prob of seeing elk

sim.exp.m <- as.data.frame(matrix(nrow=20,ncol=4))
colnames(sim.exp.m) <- c("year", "observed", "voc", "grpsize")
sim.exp.m$year <- 2020
sim.exp.m$voc <- runif(20, 0,1)
sim.exp.m$grpsize <- round(abs(rnorm(20, mean = 16, sd = 10.4)),0)
sim.exp.m$observed <- as.integer(1)
sim.exp.m <- sim.exp.m %>% mutate(observed = case_when(voc < 0.5 & grpsize >5 ~ rbinom(1, size=1, 0.8),
                                                                voc < 0.5 & grpsize <5 ~ rbinom(1, size=1, 0.7),
                                                                voc > 0.5 & grpsize >5 ~ rbinom(1, size=1, 0.4),
                                                                voc > 0.5 & grpsize <5 ~ rbinom(1, size=1, 0.3), TRUE ~ observed))

glimpse(sim.exp.m)

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
