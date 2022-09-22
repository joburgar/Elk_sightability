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

# # overall process:
# #- Run basic mHT sightablity model simulations
# # Steinhorst and Samuel (1989) assumptions:
# # 1. The population is geographically and demographically closed.
# # 2. Groups of animals are independently observed.
# # 3. Observed groups are completely enumerated and observed only once.
# # 4. The survey design for land units can be specified.
# # 5. The probability of observing a group is known or can be estimated
#
# data("obs.m")
# data("exp.m")
# data("sampinfo.m")
#
# # experimental data frame consists of:
# # year (year of test trial)
# # observed (binary; 1 if telem animal observed, 0 otherwise)
# # voc (covariate; amount of screening cover within 10 m of first animal seen)
# # grpsize (group size)
# # each row represents an independent sightability trial with observed representing the random variables
# exp.m[1:5,]
# glimpse(exp.m)
# exp.m %>% group_by(year) %>% count(observed) # 39 sightability trials in 2005, 37 in 2006 and 48 in 2007
#
# # operational data frame consists of:
# # each row corresponds to an independently sighted group with animal-specific covariates
# # subunit is the sample plot identifier (EPU in our case?)
# # stratum is the stratum identifier (should take on value of 1 for non-stratified surveys)
# obs.m[1:5,]
# glimpse(obs.m)
#
# # sampling information data frame consists of:
# # number of sampled units nh in each stratum
# # population Nh in each stratum
# sampinfo.m
#
#
# # to fit a specified logistic regression and estimate population abundance in a single step use Sight.Est function
# # code below models detection probability as a function of visual obstruction
# # note that an intercept only model would assume detection probabilities are constant, specified using observed~1
# est.2004 <- Sight.Est(observed~voc, odat = subset(obs.m, year==2004),
#                         sdat = exp.m, sampinfo = subset(sampinfo.m, year==2004))
# print(est.2004)
# # Variance component estimates provide useful information for improving future surveys
# # VarSamp = sampling uncertainty; reduced by surveying a larger number of aerial plots or implement more efficient sampling design
# # VarMod = parameter uncertainty; reduced by conducting more sightability trials
#
# # to fit a specified logistic regression and estimate population abundance in a single step use Sight.Est function
# # code below models detection probability as a function of visual obstruction
# # note that an intercept only model would assume detection probabilities are constant, specified using observed~1
# # below is for bootstrapping when sightability trials are low (i.e., < 100)
# est.2004 <- Sight.Est(observed~voc,
#                       odat = subset(obs.m, year==2004),
#                       sdat = subset(exp.m, year==2005),
#                       sampinfo = subset(sampinfo.m, year==2004),
#                       method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = FALSE)
# print(est.2004)
# print(format(round(est.2004$est, 0), big.mark = ","), quote = FALSE)
#
#
# boot.est <- Sight.Est(observed~voc,
#                       odat = subset(obs.m, year==2004),
#                       sdat = subset(exp.m, year==2005),
#                       sampinfo = subset(sampinfo.m, year==2004),
#                       method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
# print(format(round(boot.est$est, 0), big.mark = ","), quote = FALSE)

################################################################################################
###--- let's create some simulated data to work with

#####################################################################################
# if working in the elk_sightability project, need to point to correct library
version$major
version$minor
R_version <- paste0("R-",version$major,".",version$minor)

.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# 1.1 LOAD PACKAGES ####

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 1.2 Create functions ####
name_fixer <- function(x){
  output <- case_when(
    grepl("Rainy", x, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrows", x, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Deserted", x, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Chehalis", x, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sechelt", x, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", x, ignore.case = TRUE) ~ "Homathko",
    grepl("Haslam", x, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", x, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quatum", x, ignore.case = TRUE) ~ "Quatam",
    grepl("Lillooet", x, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Vancouver", x, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squamish", x, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", x, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", x, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", x, ignore.case = TRUE) ~ "Theo",
    grepl("Mcnab", x, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", x, ignore.case = TRUE) ~ "Bear",
    TRUE ~ x
  )
  return(output)
}

standard_survey <- function(x){
  output <- case_when(
    grepl("incidental", x, ignore.case = TRUE) ~ "Incidental",
    grepl("telemetry", x, ignore.case = TRUE) ~ "Telemetry",
    grepl("transect", x, ignore.case = TRUE) ~ "Inventory",
    grepl("inventory", x, ignore.case = TRUE) ~ "Inventory",
    TRUE ~ "Other")
  return(output)
}


# 1.1 Load subjective sightability data  ####
sub.sight.all <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2022 Summary", range = "A2:AK29", 
                       col_types = NULL) %>%
  type_convert() %>% glimpse()
colnames(sub.sight.all)[1] <- "EPU"
colnames(sub.sight.all)[26:34] <- c("Sub_2014","Sub_2015","Sub_2016","Sub_2017","Sub_2018","Sub_2019","Sub_2020","Sub_2021", "Sub_2022")
sub.sight <- sub.sight.all %>% select(EPU, starts_with("Sub"))

# to be more realistic, assume that each EPU is sampled every 2nd year
index1 <- seq(1,nrow(sub.sight),by=2)
index2 <- seq(2,nrow(sub.sight),by=2)
#Replace with NA
sub.sight$Sub_2014[index2] <- NA
sub.sight$Sub_2015[index1] <- NA
sub.sight$Sub_2016[index2] <- NA
sub.sight$Sub_2017[index1] <- NA
sub.sight$Sub_2018[index2] <- NA
sub.sight$Sub_2019[index1] <- NA
sub.sight$Sub_2020[index2] <- NA
sub.sight$Sub_2021[index1] <- NA

sight.true.N <- sub.sight %>% pivot_longer(cols=starts_with("Sub"), names_to="Year", values_to="N", values_drop_na = T)
sight.true.N$Year <- as.factor(str_replace(sight.true.N$Year, "Sub_", ""))
sight.true.N <- sight.true.N %>% arrange(Year, EPU)
sight.true.N$EPU <- name_fixer(sight.true.N$EPU)

# load real data (2021/2022) for ideas on group size, etc
load("input/mHT_input.Rdata")
eff
sampinfo
exp
obs %>% group_by(year) %>% count(stratum)


###################################################################################
### eff.sim

# subset simulation data to the same EPUs in the real data analysis (19)
sight.true.N <- sight.true.N %>% filter(EPU %in% eff$Unit)

# create the effort table for the simulations
# use the exact same area_surveyed values and just updating year
# arranged in the order of Year, ID (same ID )
eff.sim <- left_join(sight.true.N %>% select(-N), eff %>% select(-year), by=c("EPU"="Unit"))
eff.sim <- eff.sim[c("EPU", "area_surveyed","area_surveyed_km","Year","ID")]
as.data.frame(eff.sim %>% arrange(Year,ID))

###################################################################################
### sampinfo.sim
sampinfo # keeping this consistent between years
sampinfo.sim <- eff.sim %>% select(ID, Year)
sampinfo.sim <- left_join(sampinfo.sim, sampinfo %>% select(-year), by=c("ID"="stratum"))
colnames(sampinfo.sim)[1:2] <- c("stratum","year")
sampinfo.sim <- sampinfo.sim[names(sampinfo)]
sampinfo.sim <- sampinfo.sim %>% arrange(year, stratum)

###################################################################################
### exp.sim

# create the experimental table, this one is for collared animals only
# first pull in the number of collared animals per EPU

#- EPU polygon shapefile
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_NA")
EPU_poly$EPU <- name_fixer(EPU_poly$EPU_Unit_N)
EPU_poly <- EPU_poly %>% st_transform(3005)

# load collar locations
CollarDIR <- "C:/Users/JBURGAR/R/Analysis/Collar_data"
collar_data <- read.csv(paste0(CollarDIR,"/Position-2022-Sep-22_12-01-28.csv"))
glimpse(collar_data)
collar_data <- collar_data %>% filter(Fix.Type=="3D Validated")
collar_data <- st_as_sf(collar_data,coords = c("Longitude.deg.", "Latitude.deg."), crs = 4326) %>% st_transform(3005)
collar_data <- collar_data  %>% st_intersection(EPU_poly)

ggplot()+
  geom_sf(data=EPU_poly %>% filter(EPU %in% c("Rainy-Gray", "Sechelt Peninsula")))+
  geom_sf(data=collar_data)

EPU_poly <- st_join(EPU_poly, collar_data %>% select(Collar.ID))
collars.per.EPU <- EPU_poly %>% filter(EPU %in% eff$Unit) %>% group_by(EPU) %>% count(Collar.ID) %>% st_drop_geometry()
collars.per.EPU <- collars.per.EPU %>% count(EPU)
collars.per.EPU$ID <- eff.sim$ID[match(collars.per.EPU$EPU, eff.sim$EPU)]

eff.sim %>% filter(ID==4) # no collars in the Lower Lilloet? going with it for this analysis
# add in Lower Lillooet as one of the EPUs, but no collar data, arrange in order of ID
collars.per.EPU <- collars.per.EPU %>% add_row(EPU="Lower Lillooet", n=0, ID=4)
collars.per.EPU <- collars.per.EPU %>% arrange(ID)

# next steps:
# 1. associate the collars with grpsize
# 2. associate grpsize and collar with voc
# 3. associate collar with observed 1 or 0 based on voc

# need to make a matrix / dataframe that is the same length (nrow) as number of collars * number of surveys
sum(collars.per.EPU$n)
eff.sim %>% count(Year)
eff.sim %>% filter(Year=="2022")

nrow(eff.sim)
# number of years = 9, 4 sets of surveys plus the last year with 10 EPUs
# so can simply use the sum of collars.per.EPU$n * 4 plus the sum of collars.per.EPU$n in 9th year as length
tmp1 <- sum(collars.per.EPU$n)*4
tmp2 <- as.numeric(collars.per.EPU %>% filter(EPU %in% eff.sim[eff.sim$Year=="2022",]$EPU) %>% summarise(sum(n)))

exp.sim <- as.data.frame(matrix(NA, nrow=tmp1+tmp2, ncol=4))
colnames(exp.sim) <- c("year","observed","voc","grpsize")

# now populate the year, the first set of EPUs surveyed (even years) has 43 collars in total and second set has 32
tmp3 <- as.numeric(collars.per.EPU %>% filter(EPU %in% eff.sim[eff.sim$Year=="2014",]$EPU) %>% summarise(sum(n)))
tmp4 <- as.numeric(collars.per.EPU %>% filter(EPU %in% eff.sim[eff.sim$Year=="2015",]$EPU) %>% summarise(sum(n)))
exp.sim$year <- as.numeric(c(rep(c("2014","2016","2018","2020"), each=tmp3), rep(c("2015","2017","2019","2021"), each=tmp4), rep("2022", each=tmp2)))
exp.sim <- arrange(exp.sim, year)

# plot the voc and total animals observed data, to get a sense for simulations
# plot the observed animals group size by voc
# looks as voc increases to about 10, grp size might have negative effect (weird), 
# but >10 voc associated with higher group size
# ggplot(exp %>% filter(observed==1), aes(grpsize, voc))+
#   geom_point()+
#   geom_smooth()+
#   theme_minimal()

# the exp table is to inform the sightability correction factor and doesn't have to match back with the obs table
# consider the exp from the real data to inform the simulated exp table
# doesn't seem to be a difference in group size if the group is observed or not
# what seems to be really driving observation is voc

obs0.voc.rates <- exp %>% filter(observed==0) %>% summarise(mean.voc = mean(voc, na.rm=T), sd.voc = sd(voc, na.rm=T))
obs1.voc.rates <- exp %>% filter(observed==1) %>% summarise(mean.voc = mean(voc, na.rm=T), sd.voc = sd(voc, na.rm=T))

exp %>% group_by(observed) %>% summarise(mean.grp = mean(grpsize, na.rm=T), sd.grp = sd(grpsize, na.rm=T))
exp %>% summarise(mean.grp = mean(grpsize, na.rm=T), sd.grp = sd(grpsize, na.rm=T))

obs %>% filter(year==2021)
exp %>% filter(year==2021 & observed==1)

hist(abs(rnorm(n=nrow(exp.sim), mean=mean(exp$grpsize, na.rm=T), sd=sd(exp$grpsize, na.rm=T))))
hist(exp$grpsize)
# group size seems to follow a poisson distribution and is similar when obs==1 or 0
# go with rpois(x, 13) to fill in grpsize for each year
theta = mean(exp$grpsize, na.rm=T)
sim.grpsize <- rhalfnorm(n=nrow(exp.sim), theta)
sim.grpsize <- round(sim.grpsize*100)
sim.grpsize[sim.grpsize==0] <- 1
hist(sim.grpsize)
exp.sim$grpsize <- sim.grpsize

# about 48:52 chance of seeing collared elk
exp.sim$observed <- rbinom(nrow(exp.sim), 1, prob=0.48)

# now add in % voc based on if observed
voc0 <- abs(round(rnorm(n=nrow(exp.sim[exp.sim$observed==0,]), mean=obs0.voc.rates$mean.voc, sd=obs0.voc.rates$sd.voc)))
voc1 <- abs(round(rnorm(n=nrow(exp.sim[exp.sim$observed==1,]), mean=obs1.voc.rates$mean.voc, sd=obs1.voc.rates$sd.voc)))

exp.sim <- exp.sim %>% arrange(observed)
exp.sim$voc <- c(voc0, voc1)
exp.sim <- arrange(exp.sim, year)
exp.sim <- exp.sim %>% mutate(voc = case_when(voc>100 ~ 100,TRUE ~ as.numeric(voc))) # to make sure no veg is >100 percent

# now have exp.sim table ready to go
glimpse(exp.sim)
summary(exp.sim)
summary(exp)

exp.sim %>% group_by(observed) %>% summarise(mean.grp = mean(grpsize, na.rm=T), sd.grp = sd(grpsize, na.rm=T))
exp.sim %>% summarise(mean.grp = mean(grpsize, na.rm=T), sd.grp = sd(grpsize, na.rm=T))

# this exp.sim is unrealistic in that it has all EPUs surveyed for sightability in all years.
# makes more sense to sample from it for a reasonable number of sightability trials
# perhaps this can be part of the sensitivity testing
# can test how many sightability trials are necessary / cost-effective

###################################################################################

# no need to include more than grpsize and voc for obs table
# obs.sim is a table for all observed animals during sightability surveys
# this includes collared and uncollared groups
# this is just who is observed and doesn't include collar info
# i.e., don't know from obs table if group had collar in it
obs[is.na(obs)] <- 0
obs %>% summarise(mean.voc = mean(voc, na.rm=T), sd.voc = sd(voc, na.rm=T))
cor(obs[,10:11])

nrow(obs %>% filter(voc<10))/nrow(obs) # ~25% of voc values < 10%
# need to simulate the voc to be heavily inflated (1/4) with <10% values
# the remaining values an even spread from 10-100
inflate.voc <- sample(1:10, nrow(obs.sim)*.20)
normal.voc <- sample(1:100, nrow(obs.sim)*.80)

glimpse(obs)
summary(obs)

#- function to estimate number of animals per age-sex class based on classification ratios
pop.size <- 222
est.sex.age.class.pop <- function(pop.size=pop.size, ratio.value=30){
  round(ratio.value / (ratio.value+100)*pop.size,0)
}

est.sex.age.class.pop(pop.size = 100)

# for random numbers use rnorm with mean and sd specified for sex/age-class, bound by range of group sizes
# n = number of groups (i.e., # of groups seen during sightability trials)
# sum(round(rtruncnorm(n=20, a=0, b=42, mean=0.3, sd=1.5),0))

# use these functions together to generate simluated sightability trial dataset
###--- create exp.m simulated data frame
# using a function to simulate between 19 sightability trials done every 2 years for 8 years
# using only the covariate "visual obstruction" in the GLM
# considering various sightability probabilities depending on the amount of "voc" or visual obstruction
# used relatively general group size (mean, sd) values that could occur based on 2020/2021 read data

sim.exp.m.fn <- function(year=c(1,2), covariates="voc", grpsize=c(1,42), field.obs=obs, n.sghtblty.trls=19,
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


# create the observed table for the simulations
# use the same number of subunits (transects) per stratum
simulate.obs.grps <- obs %>% group_by(stratum) %>% summarise(mean=mean(grpsize, na.rm = T), sd=sd(grpsize, na.rm = T))

eff %>% group_by(ID) %>% summarise(mean(area_surveyed_km))


# when creating the simulated observations
# think about how many groups are encountered (rows) and how many individuals per group (grpsize)
# use the mean and sd grpsize as input for rnorm(mean, sd)


###--- create simulted observation data.frame
# using a function to simulate between 19 sightability trials done every 2 years for 8 years
# using only the covariate "visual obstruction" in the GLM
# considering various sightability probabilities depending on the amount of "voc" or visual obstruction
# used relatively general group size (mean, sd) values that could occur based on 2020/2021 read data


# operational data frame consists of:
# each row corresponds to an independently sighted group with animal-specific covariates
# subunit is the sample plot identifier (EPU in our case)
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



#- can use the default values of simulation for experimental data frame

# create 100 datasets of simulated experimental (sightability trial) data
# first with sightability surveys over 2 years, assuming differences in sighting so between 50-60 groups observed across years
sim.exp.trials50 <- vector('list', 100)
names(sim.exp.trials50) <- paste0('sim.exp.trials50', seq_along(sim.exp.trials50))
for(i in seq_along(sim.exp.trials50)){
  sim.exp.trials.base <- sim.exp.m.fn(n.sghtblty.trls=sample(50:60,1))
  sim.exp.trials50[[i]] <- sim.exp.trials.base
}
str(sim.exp.trials50[1])
# output <- data.frame(lapply(sim.exp.trials50, colSums))

# then assuming differences in sighting so between 80-100 groups observed across years
sim.exp.trials100 <- vector('list', 100)
names(sim.exp.trials100) <- paste0('sim.exp.trials100', seq_along(sim.exp.trials100))
for(i in seq_along(sim.exp.trials100)){
  sim.exp.trials.base <- sim.exp.m.fn(n.sghtblty.trls=sample(80:100,1))
  
  sim.exp.trials100[[i]] <- sim.exp.trials.base
}


sim.exp.trials50 <- sim.exp.m.fn(n.sghtblty.trls=50)
sim.exp.trials50 %>% count(observed) # 39 observed and 11 not observed

sim.exp.trials100 <- sim.exp.m.fn(n.sghtblty.trls=100)
sim.exp.trials100 %>% count(observed) # 83 observed and 17 not observed



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

# sim.obs.Skwawka_sub <- Filter(function(x) colSums(x) > 0,sim.obs.Skwawka)
# str(sim.obs.Skwawka_sub)
# length(sim.obs.Skwawka)


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
# this round has between 50 sightability trials, 100 simulations
out.Sechelt50 <- vector('list', 100)
names(out.Sechelt50) <- paste0('sim.Sechelt50_', seq_along(out.Sechelt50))
for(i in seq_along(out.Sechelt50)){
  out.Sechelt50.list <- Sight.Est(observed~voc,
                                  odat = subset(sim.obs.Sechelt[[i]], year==2020),
                                  sdat = sim.exp.trials50,
                                  sampinfo = subset(set.sampinfo.Sechelt, year == 2020),
                                  method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Sechelt50[[i]] <- out.Sechelt50.list
}

saveRDS(out.Sechelt50,"mHT_rd2_Sechelt50.RDS")

# this round has 100 sightability trials, 100 simulations
out.Sechelt100 <- vector('list', 100)
names(out.Sechelt100) <- paste0('out.Sechelt100_', seq_along(out.Sechelt100))
for(i in seq_along(out.Sechelt100)){
  out.Sechelt.list <- Sight.Est(observed~voc,
                                odat = subset(sim.obs.Sechelt[[i]], year==2020),
                                sdat = sim.exp.trials100,
                                sampinfo = subset(set.sampinfo.Sechelt, year == 2020),
                                method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Sechelt100[[i]] <- out.Sechelt.list
}

saveRDS(out.Sechelt100,"mHT_rd2_Sechelt100.RDS")

# this round has 50 sightability trials, 100 simulations
out.Skwawka50 <- vector('list', 100)
names(out.Skwawka50) <- paste0('out.Skwawka50_', seq_along(out.Skwawka50))
for(i in seq_along(out.Skwawka50)){
  out.Skwawka.list <- Sight.Est(observed~voc,
                                odat = subset(sim.obs.Skwawka[[i]], year==2020),
                                sdat = sim.exp.trials50,
                                sampinfo = subset(set.sampinfo.Skwawka, year == 2020),
                                method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Skwawka50[[i]] <- out.Skwawka.list
}

saveRDS(out.Skwawka50,"mHT_rd2_Skwawka50.RDS")

# this round has 100 sightability trials, 100 simulations
out.Skwawka100 <- vector('list', 100)
names(out.Skwawka100) <- paste0('out.Skwawka100_', seq_along(out.Skwawka100))
for(i in seq_along(out.Skwawka100)){
  out.Skwawka.list <- Sight.Est(observed~voc,
                                odat = subset(sim.obs.Skwawka[[i]], year==2020),
                                sdat = sim.exp.trials100,
                                sampinfo = subset(set.sampinfo.Skwawka, year == 2020),
                                method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
  out.Skwawka100[[i]] <- out.Skwawka.list
}

saveRDS(out.Skwawka100,"mHT_rd2_Skwawka100.RDS")



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
(tau.Sechelt100 <-   (tau.Sechelt100 <- apply(tau.Sechelt100, 1:2,
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
  theme(axis.text.y = element_text(size=14))
Sechelt50_simsplot
ggsave(Sechelt50_simsplot, file="out/Sechelt50_simsplot_rd2.PNG")


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
Sechelt50_sub_simsplot # 28 out of 99 simulations
ggsave(Sechelt50_sub_simsplot, file="out/Sechelt50_sub_simsplot_rd2.PNG")


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
  theme(axis.text.y = element_text(size=14))
Skwawka50_simsplot
ggsave(Skwawka50_simsplot, file="out/Skwawka50_simsplot_rd2.PNG")


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
Skwawka50_sub_simsplot # 22 out of 100
ggsave(Skwawka50_sub_simsplot, file="out/Skwawka50_sub_simsplot_rd2.PNG")

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
  
  sight_dat50.list <-  sim.exp.trials50[i,]
  sight_dat50.list <- sight_dat50.list[c("voc", "observed")]
  sight_dat50.list$voc <- sight_dat50.list$voc * 0.01 # change to proportion for consistency with Fieberg code
  colnames(sight_dat50.list) <- c("x.tilde", "z.tilde")
  
  sight_dat50[[i]] <-sight_dat50.list
}

sight_dat50[[2]] # check - looks the same as Fieberg's sight_dat csv

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

###--- Sechelt Peninsula
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
  
  oper_dat.Sechelt.aug <-  as.data.frame(matrix(NA, 100-nrow(sim.obs.Sechelt[[i]]),7))
  colnames(oper_dat.Sechelt.aug) <- c("x", "ym1", "h","q","z","yr","subunits")
  oper_dat.Sechelt.aug$h <- 1
  oper_dat.Sechelt.aug$z <- 0
  oper_dat.Sechelt.aug$yr <-1
  oper_dat.Sechelt.aug$subunits <- 1
  
  oper_dat.Sechelt[[i]] <- rbind(oper_dat.Sechelt.obs, oper_dat.Sechelt.aug)
}

oper_dat.Sechelt[[2]] # check - looks the same as Fieberg's oper_dat csv

###--- Skwawka
oper_dat.Skwawka <- vector('list', 100)
names(oper_dat.Skwawka) <- paste0('oper_dat.Skwawka_', seq_along(oper_dat.Skwawka))
for(i in seq_along(oper_dat.Skwawka)){
  
  # need to add in augmented NA values
  
  oper_dat.Skwawka.obs <-  sim.obs.Skwawka[[i]]
  oper_dat.Skwawka.obs$q <- 1
  oper_dat.Skwawka.obs$z <- 1
  oper_dat.Skwawka.obs$ym1 <- oper_dat.Skwawka.obs$grpsize - 1
  oper_dat.Skwawka.obs$yr <-1
  oper_dat.Skwawka.obs <- oper_dat.Skwawka.obs[c("voc","ym1","stratum","q","z","yr","subunit")]
  oper_dat.Skwawka.obs$voc <- oper_dat.Skwawka.obs$voc * 0.01 # change to proportion for consistency with Fieberg code
  colnames(oper_dat.Skwawka.obs) <- c("x", "ym1", "h","q","z","yr","subunits")
  
  oper_dat.Skwawka.aug <-  as.data.frame(matrix(NA, 100-nrow(sim.obs.Skwawka[[i]]),7))
  colnames(oper_dat.Skwawka.aug) <- c("x", "ym1", "h","q","z","yr","subunits")
  oper_dat.Skwawka.aug$h <- 1
  oper_dat.Skwawka.aug$z <- 0
  oper_dat.Skwawka.aug$yr <-1
  oper_dat.Skwawka.aug$subunits <- 1
  
  oper_dat.Skwawka[[i]] <- rbind(oper_dat.Skwawka.obs, oper_dat.Skwawka.aug)
}

oper_dat.Skwawka[[2]] # check - looks correct

# 3.  plot_dat.csv:  Plot-level information data: 77 records (one for each of the plots sampled in either 2006 or 2007)
# h.plots = stratum to which the plot belonged (1, 2, 3 correspond to low, medium and high density strata)
# yr.plots = year the plot was sampled (1 = 2006, 2= 2007)

plot_dat_Sechelt <- as.data.frame(matrix(NA,100,2))
colnames(plot_dat_Sechelt) <- c("h.plots","yr.plots")

plot_dat_Skwawka <- as.data.frame(matrix(NA,100,2))
colnames(plot_dat_Skwawka) <- c("h.plots","yr.plots")

# unstratified survey so all h.plots = 1, using only 2020 data so all yr.plots also 1
plot_dat_Sechelt$h.plots <- plot_dat_Sechelt$yr.plots <- 1

plot_dat_Skwawka$h.plots <- plot_dat_Skwawka$yr.plots <- 1

# 4.  scalar_dat.csv:  Scalars:
# R = number of sightability trials (124)
# Ngroups = number of observed and augmented groups for the 2006 and 2007 annual operational surveys (4380)
# Nsubunits.yr = total number of plots sampled in 2006 and 2007 combined = 77
# ny1 = number of groups associated with the annual survey in 2006 (year 1) = 2060

###--- for list of scalar_dat for simulations
# Sechelt
scalar_dat_Sechelt50  <- as.data.frame(matrix(NA,100,4))
colnames(scalar_dat_Sechelt50) <- c("R","Ngroups","Nsubunits.yr","ny1")

# set the top most number of groups observed at 50 for sim.obs.Sechelt so can comfortably augment to 100
scalar_dat_Sechelt50$Ngroups <- 100  # observed and augmented groups for 2020 survey
scalar_dat_Sechelt50$Nsubunits.yr <- set.sampinfo.Sechelt$nh # total number of plots surveyed in 2020

count <- 1
for(i in 1:nrow(scalar_dat_Sechelt50)){
  scalar_dat_Sechelt50[count,1] <- nrow(sim.exp.trials50) # number of sightabilty trials = nrow for each sim.exp.trials50
  scalar_dat_Sechelt50[count,4] <- nrow(sim.obs.Sechelt[[i]])  # number of groups associated with inventory survey in 2020
  count <- count + 1
}
scalar_dat_Sechelt50

# Skwawka
scalar_dat_Skwawka50  <- as.data.frame(matrix(NA,100,4))
colnames(scalar_dat_Skwawka50) <- c("R","Ngroups","Nsubunits.yr","ny1")

# set the top most number of groups observed at 50 for sim.obs.Skwawka so can comfortably augment to 100
scalar_dat_Skwawka50$Ngroups <- 100  # observed and augmented groups for 2020 survey
scalar_dat_Skwawka50$Nsubunits.yr <- set.sampinfo.Sechelt$nh # total number of plots surveyed in 2020

count <- 1
for(i in 1:nrow(scalar_dat_Skwawka50)){
  scalar_dat_Skwawka50[count,1] <- nrow(sim.exp.trials50) # number of sightabilty trials = nrow for each sim.exp.trials50
  scalar_dat_Skwawka50[count,4] <- nrow(sim.obs.Skwawka[[i]])  # number of groups associated with inventory survey in 2020
  count <- count + 1
}
scalar_dat_Skwawka50

#############################################################
###--- RUNNING 3 CHAINS IN PARALLEL USING JAGS IMPLEMENTATION

# Bundle data

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
# params <- c("tau.samp1", "tau.samp2", "bo", "bvoc") # for when there are 2 or more years of data
params <- c("tau.samp", "bo", "bvoc") # for current situation, simulating for only 1 year of data

# MCMC settings
ni <- 40000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 20000 # build to 20000
nc <- 3

# ###--- execute JAGS models
# 100 runs of base data for Sechelt using 50 sightability trials
bundle.data <- vector('list', 100)
names(bundle.data) <- paste0('bundle.data_', seq_along(bundle.data))
for(i in seq_along(bundle.data)){
  bundle.data.list <- list(x.tilde=sight_dat50$x.tilde, z.tilde=sight_dat50$z.tilde, #sight_dat
                           x=oper_dat.Sechelt[[i]]$x, ym1=oper_dat.Sechelt[[i]]$ym1, h=oper_dat.Sechelt[[i]]$h, q=oper_dat.Sechelt[[i]]$q, z=oper_dat.Sechelt[[i]]$z, yr=oper_dat.Sechelt[[i]]$yr, subunits=oper_dat.Sechelt[[i]]$subunits, # oper_dat
                           h.plots=plot_dat_Sechelt$h.plots, yr.plots=plot_dat_Sechelt$yr.plots, # plot_dat
                           R=scalar_dat_Sechelt50[i,]$R, Ngroups=scalar_dat_Sechelt50[i,]$Ngroups, Nsubunits.yr=scalar_dat_Sechelt50[i,]$Nsubunits.yr, ny1=scalar_dat_Sechelt50[i,]$ny1) # scalar_dat
  bundle.data[[i]] <- bundle.data.list
}

jags_Sechelt50 <- vector('list', 100)
names(jags_Sechelt50) <- paste0('jags_Sechelt50_', seq_along(jags_Sechelt50))
for(i in seq_along(jags_Sechelt50)){
  jags_Sechelt50.list <- jagsUI::jags(bundle.data[[i]], inits, params, "beta_binom_model_elksim.txt",
                                      n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb,
                                      parallel=TRUE, n.cores)
  jags_Sechelt50[[i]] <- jags_Sechelt50.list
}

# for(i in seq_along(jags_Sechelt50)){
#   jags_Sechelt50.list <- jags(bundle.data[[i]], inits, params, "beta_binom_model_elksim.txt",
#                                       n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb,
#                                       parallel=TRUE, n.cores=3)
#   jags_Sechelt50[[i]] <- jags_Sechelt50.list
# }
save("jags_Sechelt50",file="Simjags_Sechelt50_rd2.RData") #

summary(jags_Sechelt50) # only the first 18 models ran

# 100 runs of base data for Skwawka using 50 sightability trials
bundle.data <- vector('list', 100)
names(bundle.data) <- paste0('bundle.data_', seq_along(bundle.data))
for(i in seq_along(bundle.data)){
  bundle.data.list <- list(x.tilde=sight_dat50$x.tilde, z.tilde=sight_dat50$z.tilde, #sight_dat
                           x=oper_dat.Skwawka[[i]]$x, ym1=oper_dat.Skwawka[[i]]$ym1, h=oper_dat.Skwawka[[i]]$h, q=oper_dat.Skwawka[[i]]$q, z=oper_dat.Skwawka[[i]]$z, yr=oper_dat.Skwawka[[i]]$yr, subunits=oper_dat.Skwawka[[i]]$subunits, # oper_dat
                           h.plots=plot_dat_Skwawka$h.plots, yr.plots=plot_dat_Skwawka$yr.plots, # plot_dat
                           R=scalar_dat_Skwawka50[i,]$R, Ngroups=scalar_dat_Skwawka50[i,]$Ngroups, Nsubunits.yr=scalar_dat_Skwawka50[i,]$Nsubunits.yr, ny1=scalar_dat_Skwawka50[i,]$ny1) # scalar_dat
  bundle.data[[i]] <- bundle.data.list
}
jags_Skwawka50 <- vector('list', 100)
names(jags_Skwawka50) <- paste0('jags_Skwawka50_', seq_along(jags_Skwawka50))
for(i in seq_along(jags_Skwawka50)){
  jags_Skwawka50.list <- jags(bundle.data[[i]], inits, params, "beta_binom_model_elksim.txt",
                              n.chains=nc, n.thin=nt, n.iter=ni, n.burnin=nb,
                              parallel=TRUE, n.cores=3)
  jags_Skwawka50[[i]] <- jags_Skwawka50.list
}
save("jags_Skwawka50",file="Simjags_Skwawka50_rd2.RData") #
# load("Simjags_Skwawka50_rd2.RData")

summary(jags_Skwawka50) #


# ###--- combining JAGS output into usable format
# load("Simjags_Sechelt50_rd2.RData")
# load("Simjags_Skwawka50_rd2.RData")

tau.Sechelt50_jags <- matrix(NA,46,4)
count <- 1
for(i in 1:nrow(tau.Sechelt50_jags)){
  tau.Sechelt50_jags[count,1] <- jags_Sechelt50[[i]]$mean$tau.samp
  tau.Sechelt50_jags[count,2] <- jags_Sechelt50[[i]]$q2.5$tau.samp
  tau.Sechelt50_jags[count,3] <- jags_Sechelt50[[i]]$q97.5$tau.samp
  tau.Sechelt50_jags[count,4] <- jags_Sechelt50[[i]]$Rhat$tau.samp
  count <- count + 1
}

tau.Sechelt50_jags
colnames(tau.Sechelt50_jags) <- c("mean","LCL","UCL","Rhat")
tau.Sechelt50_jags <- as.data.frame(tau.Sechelt50_jags)

###--- Plotting
tau.Sechelt50_jags_simsplot = ggplot(tau.Sechelt50_jags, aes(x = reorder(row.names(tau.Sechelt50_jags),mean), y=mean))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt50_jags), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
tau.Sechelt50_jags_simsplot
ggsave(tau.Sechelt50_jags_simsplot, file="out/tau.Sechelt50_jags_simsplot.PNG")


tau.Sechelt50_jags_sub <- tau.Sechelt50_jags %>% filter(mean <300) # only 15 / 46 with pop estimates < 300
tau.Sechelt50_jags_sub_simsplot = ggplot(tau.Sechelt50_jags_sub, aes(x = reorder(row.names(tau.Sechelt50_jags_sub),mean), y=mean))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt50_jags_sub), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
tau.Sechelt50_jags_sub_simsplot
ggsave(tau.Sechelt50_jags_simsplot, file="out/tau.Sechelt50_jags_simsplot.PNG")


tau.Sechelt50$model <- "mHT"
tau.Sechelt50$simID <- row.names(tau.Sechelt50)
names(tau.Sechelt50)
tau.Sechelt50_jags$model <- "Bayesian"
names(tau.Sechelt50_jags)
tau.Sechelt50_jags$simID <- row.names(tau.Sechelt50_jags)

Sechelt50 <- rbind(tau.Sechelt50, tau.Sechelt50_jags %>% select(-Rhat) %>% rename(tau.hat=mean))


col.cat <- as.character(c("#2028B2","#B2AA20"))
Sechelt50_sub <- Sechelt50 %>% filter(tau.hat<300)

Sechelt50_sub_simsplot = Sechelt50_sub %>%
  ggplot(aes(x = reorder(simID,tau.hat), y=tau.hat, fill=model))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(simID, ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
Sechelt50_sub_simsplot
ggsave(Sechelt50_sub_simsplot, file="out/Sechelt50_both_sub_simsplot.PNG")

Sechelt50_simsplot = Sechelt50 %>%
  ggplot(aes(x = reorder(rownames(Sechelt50),tau.hat), y=tau.hat, fill=model))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(rownames(Sechelt50), ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))+
  ylim(c(0,20000))
Sechelt50_simsplot
ggsave(Sechelt50_simsplot, file="out/Sechelt50_both_simsplot_simsplot.PNG")
