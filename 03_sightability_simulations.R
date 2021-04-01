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
pop.size <- 222
est.sex.age.class.pop <- function(pop.size=pop.size, ratio.value=30){
  round(ratio.value / (ratio.value+100)*pop.size,0)
}


# for random numbers use rnorm with mean and sd specified for sex/age-class, bound by range of group sizes
# n = number of groups (i.e., # of groups seen during sightability trials)
# sum(round(rtruncnorm(n=20, a=0, b=42, mean=0.3, sd=1.5),0))

# use these functions together to generate simluated sightability trial dataset

###--- create exp.m simulated data frame
# using a function to simulate between 50-60 sightability trials done over 2 years (2019 and 2020)
# using only the covariate "visual obstruction" in the GLM
# considering various sightability probabilities depending on the amount of "voc" or visual obstruction
# used relatively general group size (mean, range, sd) values that could occur in multiple EPUs

sim.exp.m.fn <- function(year=c(2020,2019), covariates="voc", grpsize=c(1,42), grpsize.m=12, grpsize.sd=10, n.sghtblty.trls=sample(50:60,1),
                         cov.value=c(25,50), prob.sight=c(0.95, 0.70, 0.55, 0.45, 0.30)){

  sim.exp.m <- as.data.frame(matrix(nrow=n.sghtblty.trls,ncol=4))
  colnames(sim.exp.m) <- c("year", "observed", covariates, "grpsize")
  sim.exp.m$year <- rep(year, length.out=n.sghtblty.trls)

  # use gamma distribution to create voc - considering nature of surveys, voc more likely to be skewed right (kurtosis)
  # so might be useful to use rgamma (n = 100, shape = 4, scale = 8), rather than runif
  sim.exp.m$voc <- round(rgamma(n = n.sghtblty.trls, shape = 4, scale = 8),0)
  sim.exp.m <- sim.exp.m %>% mutate(voc = case_when(voc>100 ~ 100, TRUE ~ voc))

  sim.exp.m$grpsize <-round(rtruncnorm(n=n.sghtblty.trls, a=grpsize[1], b=grpsize[2], mean=grpsize.m, sd=grpsize.sd),0)


  # assume if <=0.25 voc then 0.95 prob of seeing elk
  # assume if >0.25 voc <=0.50 and group is >=5 then 0.70 prob of seeing elk
  # assume if >0.25 voc <=0.50 and group is <5 then 0.55 prob of seeing elk
  # assume if >0.50 voc and group is >=5 then 0.45 prob of seeing elk
  # assume if >0.50 voc and group is <5 then 0.30 prob of seeing elk

  sim.exp.m$observed <- as.integer(0) # default to 1 to set up case_when function
  sim.exp.m <- sim.exp.m %>% mutate(observed = case_when(voc <= cov.value[1] ~ rbinom(1, size=1, prob.sight[1]),
                                                         voc > cov.value[1] & voc <= cov.value[2] & grpsize >=5 ~ rbinom(1, size=1, prob.sight[2]),
                                                         voc > cov.value[1] & voc <= cov.value[2] & grpsize <5 ~ rbinom(1, size=1, prob.sight[3]),
                                                         voc > cov.value[2] & grpsize >=5 ~ rbinom(1, size=1, prob.sight[4]),
                                                         voc > cov.value[2] & grpsize <5 ~ rbinom(1, size=1, prob.sight[5]), TRUE ~ observed))

  sim.exp.m <- sim.exp.m %>% mutate(across(1:ncol(sim.exp.m), as.integer))

  return(sim.exp.m)

}

# sim.exp.m.fn()

#- can use the default values of simulation for experimental data frame

# create 100 datasets of simulated experimental (sightability trial) data
# first with sightabilty surveys over 2 years, assuming differnces in sighting so between 50-60 groups observed across years
sim.exp.trials50 <- vector('list', 100)
names(sim.exp.trials50) <- paste0('sim.exp.trials50', seq_along(sim.exp.trials50))
for(i in seq_along(sim.exp.trials50)){
  sim.exp.trials.base <- sim.exp.m.fn(n.sghtblty.trls=sample(50:60,1))
  sim.exp.trials50[[i]] <- sim.exp.trials.base
}
str(sim.exp.trials50[1])
# output <- data.frame(lapply(sim.exp.trials50, colSums))

# then assuming differnces in sighting so between 80-100 groups observed across years
sim.exp.trials100 <- vector('list', 100)
names(sim.exp.trials100) <- paste0('sim.exp.trials100', seq_along(sim.exp.trials100))
for(i in seq_along(sim.exp.trials100)){
  sim.exp.trials.base <- sim.exp.m.fn(n.sghtblty.trls=sample(80:100,1))

  sim.exp.trials100[[i]] <- sim.exp.trials.base
}


###--- create simulted observation data.frame
# operational data frame consists of:
# each row corresponds to an independently sighted group with animal-specific covariates
# subunit is the sample plot identifier (EPU in our case?)
# stratum is the stratum identifier (should take on value of 1 for non-stratified surveys)
# to be realistic, create same "observed" category as in sim.exp.trials and only consider observed (1) in obs data frame

sim.obs.m.fn <- function(year=c(2020), stratum=1, subunit=1, bull.ratio=25, calf.ratio=30, spike.ratio=21, pop.size=222, covariates="voc", grpsize=c(0,23),
                         bull.grp.m=1.2, bull.grp.sd=1.6, cow.grp.m=4.3, cow.grp.sd=4.5, calf.grp.m=1.9, calf.grp.sd=2.3, spike.grp.m=0.5, spike.grp.sd=0.8,
                         n.grps.obs=40, cov.value=c(25,50), prob.sight=c(0.95, 0.70, 0.55, 0.45, 0.30)){
  bull.grp <- est.sex.age.class.pop(pop.size = pop.size, ratio.value = bull.ratio)
  calf.grp <- est.sex.age.class.pop(pop.size = pop.size, ratio.value = calf.ratio)
  spike.grp <- est.sex.age.class.pop(pop.size = pop.size, ratio.value = spike.ratio)
  cow.grp <- pop.size - (bull.grp + calf.grp + spike.grp)

  bulls <- round(rtruncnorm(n=n.grps.obs, a=grpsize[1], b=bull.grp, mean=bull.grp.m, sd=bull.grp.sd),0)
  calves <- round(rtruncnorm(n=n.grps.obs, a=grpsize[1], b=calf.grp, mean=calf.grp.m, sd=calf.grp.sd),0)
  spikes <- round(rtruncnorm(n=n.grps.obs, a=grpsize[1], b=spike.grp, mean=spike.grp.m, sd=spike.grp.sd),0)
  cows <- round(rtruncnorm(n=n.grps.obs, a=grpsize[1], b=cow.grp, mean=cow.grp.m, sd=cow.grp.sd),0)

  sim.obs.m <- as.data.frame(cbind(bulls, calves, spikes, cows))

  # to make more realistic, if 3 or more bulls then a bachelor group
  sim.obs.m <- sim.obs.m %>% mutate(cows = case_when(bulls >= 3 ~ 0,TRUE ~ cows))
  sim.obs.m <- sim.obs.m %>% mutate(calves = case_when(bulls >= 3 ~ 0,TRUE ~ calves))
  sim.obs.m <- sim.obs.m %>% mutate(spikes = case_when(bulls >= 3 ~ 0,TRUE ~ spikes))

  # fill in rest of observation data frame
  sim.obs.m$year <- rep(year, length.out=n.grps.obs)
  sim.obs.m$stratum <- stratum
  sim.obs.m$subunit <- subunit
  sim.obs.m$total <- sim.obs.m$bulls + sim.obs.m$calves + sim.obs.m$spikes + sim.obs.m$cows
  sim.obs.m$grpsize <- sim.obs.m$total

  # create voc category, ranging from 0 to 1 (proportion of veg)
  sim.obs.m$covariate1 <- round(rgamma(n = n.grps.obs, shape = 4, scale = 8),0)
  colnames(sim.obs.m)[10] <- covariates[1]
  sim.obs.m <- sim.obs.m %>% mutate(voc = case_when(voc>100 ~ 100, TRUE ~ voc))


  # create obs category, and then subset dataframe to only the observed groups
  # use the same criteria for observing as in exp data frame
  # assume if <=0.25 voc then 0.95 prob of seeing elk
  # assume if >0.25 voc <=0.50 and group is >=5 then 0.70 prob of seeing elk
  # assume if >0.25 voc <=0.50 and group is <5 then 0.55 prob of seeing elk
  # assume if >0.50 voc and group is >=5 then 0.45 prob of seeing elk
  # assume if >0.50 voc and group is <5 then 0.30 prob of seeing elk
  sim.obs.m$observed <- as.integer(0) # default to 1 to set up case_when function
  sim.obs.m <- sim.obs.m %>% mutate(observed = case_when(voc <= cov.value[1] ~ rbinom(1, size=1, prob.sight[1]),
                                                         voc > cov.value[1] & voc <= cov.value[2] & grpsize >=5 ~ rbinom(1, size=1, prob.sight[2]),
                                                         voc > cov.value[1] & voc <= cov.value[2] & grpsize <5 ~ rbinom(1, size=1, prob.sight[3]),
                                                         voc > cov.value[2] & grpsize >=5 ~ rbinom(1, size=1, prob.sight[4]),
                                                         voc > cov.value[2] & grpsize <5 ~ rbinom(1, size=1, prob.sight[5]), TRUE ~ observed))


  sim.obs.m <- sim.obs.m %>% filter(observed==1)


  # format to same order as example dataset - only works if only have voc as covariate
  sim.obs.m <- sim.obs.m[c("year", "stratum", "subunit", "total", "cows", "calves", "bulls", "spikes","voc",
                           "grpsize")]

  sim.obs.m <- sim.obs.m %>% mutate(across(1:ncol(sim.obs.m), as.integer))


  return(sim.obs.m)

}

sim.obs.m.fn()
#- for Sechelt peninsula can use the default values of simulation for observation data frame

# create 100 datasets of simulated Sechelt observation data
sim.obs.Sechelt <- vector('list', 100)
names(sim.obs.Sechelt) <- paste0('sim.obs.Sechelt', seq_along(sim.obs.Sechelt))
for(i in seq_along(sim.obs.Sechelt)){
  sim.obs.Sechelt.base <- sim.obs.m.fn(pop.size = 222, n.grps.obs = 40)
  sim.obs.Sechelt[[i]] <- sim.obs.Sechelt.base
}


# create 100 datasets of simulated Skwawka observation data
# 2020 estimated pop size 68
sim.obs.Skwawka <- vector('list', 100)
names(sim.obs.Skwawka) <- paste0('sim.obs.Skwawka', seq_along(sim.obs.Skwawka))
for(i in seq_along(sim.obs.Skwawka)){
  sim.obs.Skwawka.base <- sim.obs.m.fn(pop.size=68, n.grps.obs = 20)
  sim.obs.Skwawka[[i]] <- sim.obs.Skwawka.base
}

sim.obs.Skwawka_sub <- Filter(function(x) colSums(x) > 0,sim.obs.Skwawka)
str(sim.obs.Skwawka_sub)
# tmp <- sim.obs.Sechelt[sim.obs.Sechelt]
#
# data.frame(lapply(sim.obs.Sechelt_sub, colSums))
#
###--- set the sampling inventory dataframe based on recent EPU inventory surveys
# the inventory survey data
# number of sampled units nh in each stratum
# population Nh in each stratum


# if want to simulate the number of sampled units in each stratum, but might be better to set
# sim.sampinfo.m.fn <- function(year=c(2020,2019), stratum=1, pop.size=222, nh=c(10,20)){
#   sim.sampinfo.m <- as.data.frame(matrix(nrow=length(year),ncol=4))
#   colnames(sim.sampinfo.m) <- c("year","stratum","Nh","nh")
#   sim.sampinfo.m$year <- rep(year, 1)
#   sim.sampinfo.m$stratum <- stratum
#   sim.sampinfo.m$Nh <-pop.size
#   sim.sampinfo.m$nh <- floor(runif(length(year), min=nh[1], max=nh[2]))
#
#   return(sim.sampinfo.m)
# }

# sim.sampinfo.m.fn()
set.sampinfo.Sechelt <- as.data.frame(matrix(NA,1,4))
colnames(set.sampinfo.Sechelt) <- c("year","stratum","nh","Nh")
set.sampinfo.Sechelt$year <- c(2020)
set.sampinfo.Sechelt$stratum <- 1
set.sampinfo.Sechelt$Nh <-c(155)
set.sampinfo.Sechelt$nh <- 100 # set as 100, considering each transect flown as nh
set.sampinfo.Sechelt <- as.data.frame(set.sampinfo.Sechelt %>% mutate(across(1:4, as.integer)))


# sim.sampinfo.m.fn()
set.sampinfo.Skwawka <- as.data.frame(matrix(NA,1,4))
colnames(set.sampinfo.Skwawka) <- c("year","stratum","nh","Nh")
set.sampinfo.Skwawka$year <- c(2020)
set.sampinfo.Skwawka$stratum <- 1
set.sampinfo.Skwawka$Nh <-c(49)
set.sampinfo.Skwawka$nh <- 50 # set as 50, considering each transect flown as nh
set.sampinfo.Skwawka <- as.data.frame(set.sampinfo.Skwawka %>% mutate(across(1:4, as.integer)))

###--- try fitting the boot strapped mHT
# this round has between 50-60 sightability trials, 100 simulations
out.Sechelt50 <- vector('list', 100)
names(out.Sechelt50) <- paste0('sim.Sechelt50_', seq_along(out.Sechelt50))
for(i in seq_along(out.Sechelt50)){
  out.Sechelt50.list <- Sight.Est(observed~voc,
                                  odat = subset(sim.obs.Sechelt[[i]], year==2020),
                                  sdat = sim.exp.trials50[[i]],
                                  sampinfo = subset(set.sampinfo.Sechelt, year == 2020),
                                  method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Sechelt50[[i]] <- out.Sechelt50.list
}

saveRDS(out.Sechelt50,"mHT_rd1_Sechelt50.RDS")

# this round has 80-100 sightability trials, 100 simulations
out.Sechelt100 <- vector('list', 100)
names(out.Sechelt100) <- paste0('out.Sechelt100_', seq_along(out.Sechelt100))
for(i in seq_along(out.Sechelt100)){
  out.Sechelt.list <- Sight.Est(observed~voc,
                                  odat = subset(sim.obs.Sechelt[[i]], year==2020),
                                  sdat = sim.exp.trials100[[i]],
                                  sampinfo = subset(set.sampinfo.Sechelt, year == 2020),
                                  method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Sechelt100[[i]] <- out.Sechelt.list
}

saveRDS(out.Sechelt100,"mHT_rd1_Sechelt100.RDS")

# this round has 50-60 sightability trials, 100 simulations
out.Skwawka50 <- vector('list', 100)
names(out.Skwawka50) <- paste0('out.Skwawka50_', seq_along(out.Skwawka50))
for(i in seq_along(out.Skwawka50)){
  out.Skwawka.list <- Sight.Est(observed~voc,
                                  odat = subset(sim.obs.Skwawka[[i]], year==2020),
                                  sdat = sim.exp.trials50[[i]],
                                  sampinfo = subset(set.sampinfo.Skwawka, year == 2020),
                                  method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Skwawka50[[i]] <- out.Skwawka.list
}

saveRDS(out.Skwawka50,"mHT_rd1_Skwawka50.RDS")


################################################################################################
###--- view simulation output
out.Sechelt100 <- readRDS("mHT_rd1_Sechelt100.RDS")
out.Sechelt50 <- readRDS("mHT_rd1_Sechelt50.RDS")

tau.Sechelt50 <- tau.Sechelt100 <- matrix(NA,100,3)
count <- 1
for(i in 1:nrow(tau.Sechelt50)){
  tau.Sechelt50[count,] <- unlist(summary(out.Sechelt50[[i]]))

for(i in 1:nrow(tau.Sechelt100))
    tau.Sechelt100[count,] <- unlist(summary(out.Sechelt100[[i]]))

count <- count + 1
}


rownames(tau.Sechelt50) <- rownames(tau.Sechelt100) <- 1:100
colnames(tau.Sechelt50) <- colnames(tau.Sechelt100) <- c("tau.hat","LCL","UCL")
(tau.Sechelt50 <- apply(tau.Sechelt50, 1:2,
                   FUN=function(x){as.numeric(gsub(",", "", x, fixed = TRUE))}))
(tau.Sechelt100 <-   (tau.Sechelt50 <- apply(tau.Sechelt50, 1:2,
                                FUN = function(x){as.numeric(gsub(",", "", x, fixed = TRUE))})))


tau.Sechelt50 <- as.data.frame(tau.Sechelt50)
# tau.Sechelt50_use <- tau.Sechelt50[-13,] # removes an outlier for graphing

Sechelt50_simsplot = ggplot(tau.Sechelt50, aes(x = reorder(row.names(tau.Sechelt50),tau.hat), y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt50), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14)) +
  ylim(0,10000)

ggsave(Sechelt50_simsplot, file="out/Sechelt50_simsplots.PNG")


tau.Sechelt50_sub <- tau.Sechelt50 %>% filter(tau.hat <300) # only 16 / 100 with pop estimates < 300
Sechelt50_sub_simsplot = ggplot(tau.Sechelt50_sub, aes(x = reorder(row.names(tau.Sechelt50_sub),tau.hat), y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt50_sub), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
Sechelt50_sub_simsplot

ggsave(Sechelt50_sub_simsplot, file="out/Sechelt50_sub_simsplots.PNG")


###--- for Skwawka
tau.Skwawka50  <- matrix(NA,100,3)
count <- 1
for(i in 1:nrow(tau.Skwawka50)){
  tau.Skwawka50[count,] <- unlist(summary(out.Skwawka50[[i]]))
  count <- count + 1
}


rownames(tau.Skwawka50) <- 1:100
colnames(tau.Skwawka50) <- c("tau.hat","LCL","UCL")
(tau.Skwawka50 <- apply(tau.Skwawka50, 1:2,
                        FUN=function(x){as.numeric(gsub(",", "", x, fixed = TRUE))}))
(tau.Skwawka50 <- apply(tau.Skwawka50, 1:2,
                                             FUN = function(x){as.numeric(gsub(",", "", x, fixed = TRUE))}))


tau.Skwawka50 <- as.data.frame(tau.Skwawka50)
pop.size.Skwawka <- 68

Skwawka50_simsplot = ggplot(tau.Skwawka50, aes(x = reorder(row.names(tau.Skwawka50),tau.hat), y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Skwawka50), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size.Skwawka, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))+
  ylim(0,10000)
Skwawka50_simsplot
ggsave(Skwawka50_simsplot, file="out/Skwawka50_simsplot.PNG")


tau.Skwawka50_sub <- tau.Skwawka50 %>% filter(tau.hat <100) # only 17 /100 with pop estimates < 100 (and none converged / no CI)
Skwawka50_sub_simsplot = ggplot(tau.Skwawka50_sub, aes(x = reorder(row.names(tau.Skwawka50_sub),tau.hat), y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Skwawka50_sub), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size.Skwawka, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
Skwawka50_sub_simsplot
ggsave(Skwawka50_sub_simsplot, file="out/Skwawka50_sub_simsplot.PNG")

# save.image("mHT_sightability_simulations.RData")
# load("mHT_sightability_simulations.RData")

################################################################################################
###--- create similar simulation data for Bayesian model
# from Fieberg README:
# 1.  sight_dat.csv = Sightability survey data:  124 records
# x = visual obstruction measurements associated with the test trial data used to develop the sightability model
# z = detection indicator (1 if the group was observed, 0 otherwise)
# the tilde '~' distinguishes data collected during the sightability trials from those collected during the operational surveys
# currently have sim.exp.trials50 / sim.exp.trials100
str(sim.exp.trials50)

# create a list of the 100 sim.exp.trials50 in the same format as Fieberg's sight_dat csv
sight_dat50 <- vector('list', 100)
names(sight_dat50) <- paste0('sight_dat50_', seq_along(sight_dat50))
for(i in seq_along(sight_dat50)){

  sight_dat50.list <-  sim.exp.trials50[[i]]
  sight_dat50.list <- sight_dat50.list[c("voc", "observed")]
  sight_dat50.list$voc <- sight_dat50.list$voc * 0.01 # change to proportion for consistency with Fieberg code
  colnames(sight_dat50.list) <- c("x.tilde", "z.tilde")

  sight_dat50[[i]] <-sight_dat50.list
}

sight_dat50[[1]] # check - looks the same as Fieberg's sight_dat csv

# 2.  oper_dat.csv:  Operational survey data:  4380 records
# (includes observed and augmented data for the annual surveys in  2006 and 2007 combined).
# Augmented data records have NA (missing) for x, y, q.
# x = visual obstruction measurements
# ym1 = y-1, where y = observed group size
# h = stratum identifier (1, 2, 3 correspond to low, medium and high density strata)
# q = indicator variable that represents whether the group belongs to the study population (equal to 1 for all observed groups and NA for all augmented groups).
# z = detection indicator (equal to 1 if the group was observed during the operational survey and 0 otherwise)
# subunits = unique plot identifier (for all sampled plots).
# yr = year of observation (1 = 2006, 2= 2007)

# currently have sim.obs.Sechelt and sim.obs.Skwawka
# first create very simple oper_dat files for each EPU separately and then combine to see if better power

oper_dat.Sechelt <- vector('list', 100)
names(oper_dat.Sechelt) <- paste0('oper_dat.Sechelt_', seq_along(oper_dat.Sechelt))
for(i in seq_along(oper_dat.Sechelt)){

  # need to add in augmented NA values

  oper_dat.Sechelt.obs <-  sim.obs.Sechelt[[i]]
  oper_dat.Sechelt.obs$q <- 1
  oper_dat.Sechelt.obs$z <- 1
  oper_dat.Sechelt.obs$ym1 <- oper_dat.Sechelt.obs$grpsize - 1
  oper_dat.Sechelt.obs$yr <-1
  oper_dat.Sechelt.obs <- oper_dat.Sechelt.obs[c("voc","ym1","stratum","q","z","yr","subunit")]
  oper_dat.Sechelt.obs$voc <- oper_dat.Sechelt.obs$voc * 0.01 # change to proportion for consistency with Fieberg code
  colnames(oper_dat.Sechelt.obs) <- c("x", "ym1", "h","q","z","yr","subunits")

  oper_dat.Sechelt.aug <-  as.data.frame(matrix(NA, 100-nrow(sim.obs.Sechelt[[1]]),7))
  colnames(oper_dat.Sechelt.aug) <- c("x", "ym1", "h","q","z","yr","subunits")
  oper_dat.Sechelt.aug$h <- 1
  oper_dat.Sechelt.aug$z <- 0
  oper_dat.Sechelt.aug$yr <-1
  oper_dat.Sechelt.aug$subunits <- 1

  oper_dat.Sechelt[[i]] <- rbind(oper_dat.Sechelt.obs, oper_dat.Sechelt.aug)
}

oper_dat.Sechelt[[1]] # check - looks the same as Fieberg's oper_dat csv

# 3.  plot_dat.csv:  Plot-level information data: 77 records (one for each of the plots sampled in either 2006 or 2007)
# h.plots = stratum to which the plot belonged (1, 2, 3 correspond to low, medium and high density strata)
# yr.plots = year the plot was sampled (1 = 2006, 2= 2007)

plot_dat_Sechelt <- as.data.frame(matrix(NA,100,2))
colnames(plot_dat_Sechelt) <- c("h.plots","yr.plots")

# unstratified survey so all h.plots = 1, using only 2020 data so all yr.plots also 1
plot_dat_Sechelt$h.plots <- plot_dat_Sechelt$yr.plots <- 1

# 4.  scalar_dat.csv:  Scalars:
# R = number of sightability trials (124)
# Ngroups = number of observed and augmented groups for the 2006 and 2007 annual operational surveys (4380)
# Nsubunits.yr = total number of plots sampled in 2006 and 2007 combined = 77
# ny1 = number of groups associated with the annual survey in 2006 (year 1) = 2060

###--- for list of scalar_dat for simulations
scalar_dat_Sechelt50  <- as.data.frame(matrix(NA,100,4))
colnames(scalar_dat_Sechelt50) <- c("R","Ngroups","Nsubunits.yr","ny1")

# set the top most number of groups observed between 50-60 for sim.obs.Sechelt so can comfortably augment to 100
scalar_dat_Sechelt50$Ngroups <- 100  # observed and augmented groups for 2020 survey
scalar_dat_Sechelt50$Nsubunits.yr <- set.sampinfo.Sechelt$nh # total number of plots surveyed in 2020

count <- 1
for(i in 1:nrow(scalar_dat_Sechelt50)){
  scalar_dat_Sechelt50[count,1] <- nrow(sim.exp.trials50[[i]]) # number of sightabilty trials = nrow for each sim.exp.trials50
  scalar_dat_Sechelt50[count,4] <- nrow(sim.obs.Sechelt[[i]])  # number of groups associated with inventory survey in 2020
  count <- count + 1
}
scalar_dat_Sechelt50
# scalar_dat_Sechelt50 <- as.data.frame(scalar_dat_Sechelt50 %>% mutate(across(1:4, as.integer)))

#############################################################
###--- RUNNING 3 CHAINS IN PARALLEL USING JAGS IMPLEMENTATION
#
# round(dnorm(0.00000, 0.1)*100,0)
# rgamma(n = 1, shape = 4, scale = 8)
# dgamma(4,8)*100
# dgamma(6,9)

oper_dat.Sechelt[[1]]

dat <- list(x.tilde=sight_dat50[[i]]$x.tilde, z.tilde=sight_dat50[[i]]$z.tilde, #sight_dat
            x=oper_dat.Sechelt[[i]]$x, ym1=oper_dat.Sechelt[[i]]$ym1, h=oper_dat.Sechelt[[i]]$h, q=oper_dat.Sechelt[[i]]$q, z=oper_dat.Sechelt[[i]]$z, yr=oper_dat.Sechelt[[i]]$yr, subunits=oper_dat.Sechelt[[i]]$subunits, # oper_dat
            h.plots=plot_dat_Sechelt$h.plots, yr.plots=plot_dat_Sechelt$yr.plots, # plot_dat
            R=scalar_dat_Sechelt50[i,]$R, Ngroups=scalar_dat_Sechelt50[i,]$Ngroups, Nsubunits.yr=scalar_dat_Sechelt50[i,]$Nsubunits.yr, ny1=scalar_dat_Sechelt50[i,]$ny1) # scalar_dat

# dat.list <- list(C=data$C, nsite=data$nsite, nmonth=data$nmonth, cov1=data$cov1)
#
# win.data.CO0 <- vector('list', 200)
# names(win.data.CO0) <- paste0('win.data', seq_along(win.data.CO0))
# for(i in seq_along(win.data.CO0)){
#   win.data.CO0.list <- list(C=CO.S0[[i]]$C, nsite=CO.S0[[i]]$nsite, nmonth=CO.S0[[i]]$nmonth, cov1=CO.S0[[i]]$cov1)
#   win.data.CO0[[i]] <- win.data.CO0.list
# }


# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# inits <- function()list(mu=runif(1,0,4), beta1=runif(1,-2,2), alpha=runif(24,-2,2), eps=runif(12,-2,2))

# Parameters monitored
params <- c("tau.samp1", "tau.samp2", "bo", "bvoc")
params <- c("tau.samp", "bo", "bvoc")

# MCMC settings
ni <- 40000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 20000 # build to 20000
nc <- 3

test <- jags(dat, inits, params, "beta_binom_model_elksim.txt",
             n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb,
             parallel=TRUE, n.cores=3)

# now need to run 100 simulations using simulated data

# ###--- execute JAGS models
# library(jagsUI)
#
# # 200 runs of base data 24 sites @ 12 months
# jags_out.Sechelt50 <- vector('list', 200)
# names(jags_out.Sechelt50) <- paste0('win.data', seq_along(out.CO0))
# for(i in seq_along(jags_out.Sechelt50)){
#   jags_out.Sechelt50.list <- jags(dat[[i]], inits.CO0, params, "beta_binom_model_elksim.txt",
#                        n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb,
#                        parallel=TRUE, n.cores=3)
#   jags_out.Sechelt50[[i]] <- jags_out.Sechelt50.list
# }
# save("out.CO0",file="Simout.CO0.RData") # MB
#
