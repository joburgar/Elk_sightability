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

#####################################################################################
#####################################################################################

###--- create simulated data for mHT and Bayesian analyses

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

# LOAD PACKAGES ####

list.of.packages <- c("tidyverse", "fdrtool","sf", "rgdal","readxl", "Cairo", "rjags","coda", "SightabilityModel","truncnorm",  "xtable", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Create functions ####
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


# Load subjective sightability data  ####
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
# eff
# sampinfo
# exp
# obs %>% group_by(year) %>% count(stratum)


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
# these are technically supposed to be independent sightability trials
# done outside the inventory surveys
# currently creating for all collared animals per EPU per year (overkill)
# this way we can sample from it as part of a sensitivity analysis in the simulations

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
exp.sim$year <- as.numeric(c(rep(c("2014","2016","2018","2020"), each=tmp3), 
                             rep(c("2015","2017","2019","2021"), each=tmp4), 
                             rep("2022", each=tmp2)))
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

# not quite the same as the field data values but similar ball park
# slightly smaller group sizes in sim, less variation

###################################################################################
### obs.sim

# "true" values change each year for EPU
sight.true.N$ID <- eff.sim$ID[match(sight.true.N$EPU, eff.sim$EPU)]

meanN <- sight.true.N %>% group_by(ID) %>% filter(Year %in% c(2020,2021,2022)) %>% summarise(meanN=mean(N))
field.obs <- obs %>% group_by(stratum) %>% summarise(field.obs=sum(grpsize))
field.obs$meanN <- meanN$meanN
field.obs[is.na(field.obs)] <- 0
field.obs$diff <-  field.obs$meanN - field.obs$field.obs
sort(field.obs$diff)
groups.seen <- obs %>% count(stratum)
field.obs$grps.seen <- groups.seen$n

cor(field.obs[,c("meanN","grps.seen")])
summary(lm(grps.seen~meanN, data=field.obs))

# need to sort out how many grps encountered on each survey
summary(lm(field.obs ~ grps.seen, data=field.obs %>% filter(stratum!=9)))
cor(field.obs[-9,2:3]) # removing 9 from here as well, gives a much higher correlation (close to lm)
cor(field.obs[-9,c("field.obs","grps.seen")]) # removing 9 from here as well, gives a much higher correlation (close to lm)
hist(rnorm(100, mean=10, sd=1.6))

###--- create simulted observation data.frame
# using a function to simulate between 19 sightability trials done every 2 years for 8 years
# using only the covariate "visual obstruction" in the GLM
# considering various sightability probabilities depending on the amount of "voc" or visual obstruction
# used relatively general group size (mean, sd) values that could occur based on 2020/2021 read data

# operational data frame consists of:
# each row corresponds to an independently sighted group with animal-specific covariates
# subunit is the sample plot identifier (EPU in our case)
# stratum is the stratum identifier (should take on value of 1 for non-stratified surveys)

# no need to include more than grpsize and voc for obs table
# obs.sim is a table for all observed animals during sightability surveys
# this includes collared and uncollared groups
# this is just who is observed and doesn't include collar info
# i.e., don't know from obs table if group had collar in it


glimpse(obs)
eff.sim %>% count(EPU) %>% filter(n==4)
# 10 EPUs surveyd 5 years
# 9 EPUS surveyed 4 years

sim.obs.m.fn <- function(year=5, eff.sim=eff.sim, field.obs=obs, pop.size=sight.true.N){
  
  # need to figure out how observations per year per EPU
  # first determine the number of EPUs
  EPUs.to.use <- eff.sim %>% count(EPU,ID)
  EPUs.to.use <- EPUs.to.use %>% filter(n==year)
  
  sight.true.N.to.use <- sight.true.N %>% filter(EPU %in% EPUs.to.use$EPU)
  
  sight.true.N.to.use$mult.factor <- rtruncnorm(nrow(sight.true.N.to.use), a=0.01, b=0.99, mean=0.05, sd=0.01)
  sight.true.N.to.use$grps.seen <- round(sight.true.N.to.use$N * sight.true.N.to.use$mult.factor)
  
  df1=sight.true.N.to.use
  df2 <- data.frame(df1[rep(seq_len(dim(df1)[1]),  
                            with(df1, ifelse(grps.seen > 0 & !is.na(grps.seen), grps.seen, 1))), , drop = FALSE], 
                    row.names=NULL)
  obs.sim <- df2 %>% select(Year, ID)
  colnames(obs.sim) <- c("year","stratum")
  obs.sim$subunit <- 1
  
  # nrow(obs %>% filter(voc<10))/nrow(obs) # ~25% of voc values < 10%
  # need to simulate the voc to be heavily inflated (1/4) with <10% values
  # the remaining values an even spread from 10-100
  inflate.voc <- sample(1:10, round(nrow(obs.sim)*.20), replace = T)
  normal.voc <- sample(1:100, round(nrow(obs.sim)*.80), replace = T)
  voc <- c(inflate.voc, normal.voc)
  obs.sim$voc <- sample(voc)
  
  # populate group size using half normal distribution
  field.obs.to.use <- field.obs %>% filter(stratum %in% EPUs.to.use$ID)
  theta = mean(field.obs.to.use$grpsize, na.rm=T)
  sim.grpsize <- rhalfnorm(n=nrow(obs.sim), theta)
  sim.grpsize <- round(sim.grpsize*100)
  sim.grpsize[sim.grpsize==0] <- 1
  obs.sim$total <- sim.grpsize
  
  # summary(obs.sim$grpsize)
  # summary(field.obs.to.use$grpsize)
  
  return(obs.sim)
  
}

# obs.sim.5years <- sim.obs.m.fn(year=5, eff.sim=eff.sim, field.obs=obs, pop.size=sight.true.N)
# obs.sim.4years <- sim.obs.m.fn(year=4, eff.sim=eff.sim, field.obs=obs, pop.size=sight.true.N)
# 
# obs.sim <- rbind(obs.sim.5years, obs.sim.4years)
# obs.sim <- obs.sim %>% arrange(year, stratum)


############################################################################################################
###--- create 30 sets of simulations

# use the same exp, eff and sampinfo data
# have 9 years of exp data, so try with 3, 6, and 9 years
# Set1 = 2014, 2017, 2020
# Set2 = 2014, 2015, 2017, 2018, 2020, 2021
# Set3 = all years
# run each set 10 times
# create new obs data (10 sets to run with each exp set)

glimpse(exp.sim)
exp.sim.Set1 <- exp.sim %>% filter(year %in% c(2014, 2017, 2020))
exp.sim.Set2 <- exp.sim %>% filter(!year %in% c(2016, 2019, 2022))
exp.sim.Set3 <- exp.sim

exp.sim.list <- list(exp.sim.Set1, exp.sim.Set2, exp.sim.Set3)


obs.sim.list <- list()
for(i in 1:10){
  obs.sim.4years <- sim.obs.m.fn(year=4,eff.sim=eff.sim, field.obs=obs, pop.size=sight.true.N)
  obs.sim.5years <- sim.obs.m.fn(year=5,eff.sim=eff.sim, field.obs=obs, pop.size=sight.true.N)
  
  obs.sim <- rbind(obs.sim.5years, obs.sim.4years)
  obs.sim <- obs.sim %>% arrange(year, stratum)
  
  obs.sim.list[[i]] <- obs.sim
}


mHT_input_sim <- list(eff=eff.sim, exp=exp.sim.list, obs=obs.sim.list, sampinfo=sampinfo.sim)
str(mHT_input_sim)
save(mHT_input_sim, file = "input/mHT_input_sim.Rdata")

#####################################################################################
#####################################################################################

###--- run simulated data for mHT
# adapting from Tristen's code

load("input/mHT_input_sim.Rdata")

eff=mHT_input_sim$eff
exp=mHT_input_sim$exp
obs=mHT_input_sim$obs
sampinfo=mHT_input_sim$sampinfo

year.to.use <- 2014:2022
str(mHT_input_sim$obs)

# run the 10 simulations of obs data with Set 1 of exp and the same sampinfo data
# add on the one layer of the extra exp sets and run all 30 at once with nboot at 10000
out.mHT <- vector('list', 3)
for(r in 1:length(mHT_input_sim$exp)){
  
  out.mHT_Set <- vector('list', 10)
  names(out.mHT_Set) <- paste0('sim.Set',r,'_', seq_along(out.mHT_Set))
  for(i in seq_along(out.mHT_Set)){
    tempobs <- mHT_input_sim$obs[[i]]
    out.year <- vector('list', length(year.to.use))
    
    for(yr in 1:length(year.to.use)){
      stratum.to.use <- tempobs %>% filter(year==year.to.use[yr]) %>% count(stratum)
      stratum.to.use <- as.numeric(stratum.to.use$stratum)
      
      tau.hats <- matrix(NA,length(stratum.to.use), 7)
      
      for(st in 1:length(stratum.to.use)) {
        
        
        
        tmp.year <- Sight.Est(observed~voc,
                              odat = tempobs %>% filter(year==year.to.use[yr] & stratum==stratum.to.use[st]),
                              sdat = mHT_input_sim$exp[[r]],
                              sampinfo = mHT_input_sim$sampinfo %>% filter(year==year.to.use[yr] & stratum==stratum.to.use[st]),
                              method = "Wong", logCI = TRUE, alpha = 0.05, Vm.boot = TRUE, nboot = 10000)
        temp.summary <- summary(tmp.year)
        tau.hats[st, 1:5] <- tmp.year$est
        tau.hats[st, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
        tau.hats[st, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
        
        
      }
      out.year[[yr]] <- tau.hats
      
    } 
    out.mHT_Set[[i]] <- out.year
    
  }
  out.mHT[[r]]  <- out.mHT_Set
}

saveRDS(out.mHT,"out/mHT_30sims.RDS")
str(out.mHT)

#####################################################################################
#####################################################################################

###--- adapted simulated data for Bayesian analyses
# adapting from Tristen's code

# BAYESIAN DATA ####

## Main differences between bayesian and mHT datasets:
## 1. VOC is a decimal in Bayesian
## 2. subunit is numbered like stratum in Bayesian


## SIGHT DAT ####

# Sightability survey data:  64 records
# s = habitat indicator ()
# x = visual obstruction measurements associated with the test trial data used to develop the sightability model
# a = activity indicator (0 if bedded, 1 if standing/moving)
# z = detection indicator (1 if the group was observed, 0 otherwise)
# t = group size

sight.dat <- vector('list',3)
for(i in 1:length(exp)){
  tmp.exp <- exp[[i]]
  tmp.sight.dat <- tmp.exp %>%
    mutate(x.tilde = as.double(voc*.01),
           z.tilde = as.double(observed)) %>%
    select(x.tilde, z.tilde)
  
  sight.dat[[i]] <- tmp.sight.dat
}

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

### from Tristen's original work - testing correlations ####
# sight.dat[[3]] %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))
# 
# x.z <- cor.test(sight.dat[[3]]$z.tilde, sight.dat[[3]]$x.tilde, method="pearson")
# t.z <- cor.test(sight.dat[[3]]$z.tilde, sight.dat[[3]]$t, method="pearson")
# 
# Correlation <- as.data.frame(matrix(NA, 2, 3))
# Correlation[1,] <- c("VOC", x.z$estimate, x.z$p.value)
# Correlation[2,] <- c("Group size", t.z$estimate, t.z$p.value)
# colnames(Correlation) <- c("Variable", "Correlation", "p")
# Correlation
# 
# # voc is most significantly correlated with sightability -> select only voc
# sight.dat <- sight.dat %>% select(x.tilde, z.tilde)

## OPER DAT ####

# Operational survey data:  4380 records
# (includes observed and augmented data for the annual surveys from 2014:2022 combined).
# Augmented data records have NA (missing) for x, y, q.
# x = visual obstruction measurements
# ym1 = y-1, where y = observed group size
# h = stratum identifier (all same stratum)
# q = indicator variable that represents whether the group belongs to the study population (equal to 1 for all observed groups and NA for all augmented groups).
# z = detection indicator (equal to 1 if the group was observed during the operational survey and 0 otherwise)
# subunits = unique plot identifier (for all sampled plots).
# yr = year of observation (1 = 2014...9 = 2022)

# non-augmented data
oper.dat <- vector('list',10)
for(i in 1:length(obs)){
  tmp.oper.dat <- obs[[i]] %>%
    transmute(x = round(as.double(voc*.01), 2),
              ym1 = total-1,
              h = as.double(stratum),
              q = 1,
              z = 1,
              yr = recode(year, '2014'=1, '2015'=2, '2016'=3, '2017'=4, '2018'=5, '2019'=6, '2020'=7, '2021'=8, '2022'=9), 
              subunits = as.double(subunit)) %>%
    glimpse()
  oper.dat[[i]] <- tmp.oper.dat
}

str(oper.dat)

# augmented data
# need to determine max m of each h
aug <- vector('list',10)
for(i in 1:length(oper.dat)){
  aug.list <- oper.dat[[i]] %>%
    group_by(yr, h) %>%
    summarize(m = n()) %>%
    ungroup() %>%
    group_by(h) %>%
    summarize(yr = yr,
              m = m,
              m.max = max(m)) %>%
    ungroup() %>%
    mutate(b = 5*m.max,
           aug = b-m)
  aug[[i]] <- aug.list
}

oper.dat.aug <- vector('list',10)
for(i in 1:length(oper.dat)){
tmp.oper.dat.aug <- aug[[i]][rep(1:nrow(aug[[i]]), aug[[i]]$aug),] %>%
  mutate(x = NA, ym1 = NA, h = h, q = NA, z = 0, yr = yr, subunits = h, .keep="none") %>%
  ungroup()
oper.dat.aug[[i]] <- tmp.oper.dat.aug
}

for(i in 1:length(oper.dat)){
oper.dat[[i]] <- rbind(oper.dat[[i]], oper.dat.aug[[i]]) %>%
  arrange(yr, h, q)
}

glimpse(oper.dat) # check

## PLOT DAT ####

# Plot-level information data: 86 records (one for each of the plots sampled between 2014 and 2022)
# h.plots = stratum to which the plot belonged (1, 2, 3 correspond to the EPU)
# yr.plots = year the plot was sampled (1 = 2014, 9= 2022)
# same for each simulation, so just need one (not a list of them)
plot.dat <- oper.dat[[1]] %>%
  select(yr, h) %>%
  distinct() %>%
  mutate(h.plots = h, 
         yr.plots = yr) %>%
  select(h.plots, yr.plots) %>%
  arrange(yr.plots, h.plots)

glimpse(plot.dat)

## SCALAR DAT ####

#   Scalars:
#   R = number of sightability trials (changes depending on simulation set - 3 sets)
# 
#   Ngroups = number of observed and augmented groups for the 2014 to 2022 annual operational surveys
# 
#   Nsubunits.yr = total number of plots sampled between 2014 and 2022  = 86 
# 
#   ny1 = number of groups associated with the annual survey in 2014 (year 1)

scalar.dat.full <- vector('list',3)
for(x in 1:length(sight.dat)){
  
  scalar.dat <- vector('list',10)
  for(y in 1:length(oper.dat)){
    
    tmp.scalar.dat <- as.data.frame(matrix(NA, 1, 3))
    colnames(tmp.scalar.dat) <- c("R", "Ngroups", "Nsubunits.yr")
    
    tmp.scalar.dat <- as.data.frame(matrix(NA, 1, (nrow(plot.dat))))
    for(i in 1:nrow(plot.dat)){
      tmp.scalar.dat[,i] <- as.double(nrow(oper.dat[[y]] %>% filter(yr == plot.dat$yr.plots[i], h == plot.dat$h.plots[i])))
      colnames(tmp.scalar.dat)[i] <- paste("h", plot.dat$h.plots[i], "y", plot.dat$yr.plots[i], sep = "")
    }
    
    tmp.scalar.dat <- tmp.scalar.dat %>%
      mutate(R = as.double(nrow(sight.dat[[x]])),
             Ngroups = as.double(nrow(oper.dat[[y]])),
             Nsubunits.yr = as.double(nrow(plot.dat)))
    
    scalar.dat[[y]]<- tmp.scalar.dat
  }
  scalar.dat.full[[x]] <- scalar.dat
}


# Need scalar.sums to ease modelling
scalar.sums.full <- vector('list',3)
for(x in 1:3){
  scalar.sums <- vector('list',10)
  for(y in 1:10){
    tmp.scalar.sums <- matrix(NA, nrow(plot.dat), 2)
    for (i in 1:nrow(plot.dat)){
      t <- i-1
      tmp.scalar.sums[i, 1] <- sum(scalar.dat.full[[x]][[y]][,0:t], 1)
      tmp.scalar.sums[i, 2] <- sum(scalar.dat.full[[x]][[y]][,0:i])
    }
    scalar.sums[[y]] <- tmp.scalar.sums
  }
  scalar.sums.full[[x]] <- scalar.sums
}

## Save Bayesian data ####
save(list = c("sight.dat", "oper.dat", "plot.dat", "scalar.dat.full", "eff", "scalar.sums.full"), file = "input/jags_30sims_input.Rdata")
rm(list = ls())

#####################################################################################
#####################################################################################

###--- run simulated data for Bayesian analyses
# adapting from Tristen's code

load("input/jags_30sims_input.Rdata")

# RUN MODEL ####
# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("bo", "bvoc", "tau.hat")

# MCMC settings
ni <- 100 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 50  # build to 20000
nc <- 3


# length(sight.dat) #3
# length(oper.dat)  #10
# length(scalar.sums.full) #3 by 10
# length(scalar.dat.full)  #3 by 10

# i <- 1
# 
bundle.hy <- as.character()
  for(i in 1:(ncol(scalar.dat.full[[x]][[y]])-3)){
  bundle.hy[i] <-
    paste(colnames(scalar.dat.full[[x]][[y]])[i],
          "=scalar.dat.full[[x]][[y]]$",
          colnames(scalar.dat.full[[x]][[y]])[i],
          ", ",
          sep = "")
  }
# # run and copy the output to paste into data bundle (don't forget to remove quotes and final comma)
paste0(bundle.hy, collapse = "")


# Bundle data
jags_30sims_output <- vector('list',3)
for(x in 1:3){
  tmp.jags <- vector('list',10)
  for(y in 1:10){
    
    bundle.dat <- list(x.tilde=sight.dat[[x]]$x.tilde, z.tilde=sight.dat[[x]]$z.tilde, #sight.dat
                       x=oper.dat[[y]]$x+.000001, ym1=oper.dat[[y]]$ym1, h=oper.dat[[y]]$h, q=oper.dat[[y]]$q, z=oper.dat[[y]]$z, yr=oper.dat[[y]]$yr, subunits=oper.dat[[y]]$subunits, # oper.dat
                       h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot_dat
                       R=scalar.dat.full[[x]][[y]]$R, Ngroups=scalar.dat.full[[x]][[y]]$Ngroups, Nsubunits.yr=scalar.dat.full[[x]][[y]]$Nsubunits.yr, scalars=scalar.sums.full[[x]][[y]])#,#scalar.dat
                       # h3y1=scalar.dat.full[[x]][[y]]$h3y1, h5y1=scalar.dat.full[[x]][[y]]$h5y1, h7y1=scalar.dat.full[[x]][[y]]$h7y1, h8y1=scalar.dat.full[[x]][[y]]$h8y1, h10y1=scalar.dat.full[[x]][[y]]$h10y1, h11y1=scalar.dat.full[[x]][[y]]$h11y1, h12y1=scalar.dat.full[[x]][[y]]$h12y1, h13y1=scalar.dat.full[[x]][[y]]$h13y1, h14y1=scalar.dat.full[[x]][[y]]$h14y1, h19y1=scalar.dat.full[[x]][[y]]$h19y1, h1y2=scalar.dat.full[[x]][[y]]$h1y2, h2y2=scalar.dat.full[[x]][[y]]$h2y2, h4y2=scalar.dat.full[[x]][[y]]$h4y2, h6y2=scalar.dat.full[[x]][[y]]$h6y2, h9y2=scalar.dat.full[[x]][[y]]$h9y2, h15y2=scalar.dat.full[[x]][[y]]$h15y2, h16y2=scalar.dat.full[[x]][[y]]$h16y2, h17y2=scalar.dat.full[[x]][[y]]$h17y2, h18y2=scalar.dat.full[[x]][[y]]$h18y2, h3y3=scalar.dat.full[[x]][[y]]$h3y3, h5y3=scalar.dat.full[[x]][[y]]$h5y3, h7y3=scalar.dat.full[[x]][[y]]$h7y3, h8y3=scalar.dat.full[[x]][[y]]$h8y3, h10y3=scalar.dat.full[[x]][[y]]$h10y3, h11y3=scalar.dat.full[[x]][[y]]$h11y3, h12y3=scalar.dat.full[[x]][[y]]$h12y3, h13y3=scalar.dat.full[[x]][[y]]$h13y3, h14y3=scalar.dat.full[[x]][[y]]$h14y3, h19y3=scalar.dat.full[[x]][[y]]$h19y3, h1y4=scalar.dat.full[[x]][[y]]$h1y4, h2y4=scalar.dat.full[[x]][[y]]$h2y4, h4y4=scalar.dat.full[[x]][[y]]$h4y4, h6y4=scalar.dat.full[[x]][[y]]$h6y4, h9y4=scalar.dat.full[[x]][[y]]$h9y4, h15y4=scalar.dat.full[[x]][[y]]$h15y4, h16y4=scalar.dat.full[[x]][[y]]$h16y4, h17y4=scalar.dat.full[[x]][[y]]$h17y4, h18y4=scalar.dat.full[[x]][[y]]$h18y4, h3y5=scalar.dat.full[[x]][[y]]$h3y5, h5y5=scalar.dat.full[[x]][[y]]$h5y5, h7y5=scalar.dat.full[[x]][[y]]$h7y5, h8y5=scalar.dat.full[[x]][[y]]$h8y5, h10y5=scalar.dat.full[[x]][[y]]$h10y5, h11y5=scalar.dat.full[[x]][[y]]$h11y5, h12y5=scalar.dat.full[[x]][[y]]$h12y5, h13y5=scalar.dat.full[[x]][[y]]$h13y5, h14y5=scalar.dat.full[[x]][[y]]$h14y5, h19y5=scalar.dat.full[[x]][[y]]$h19y5, h1y6=scalar.dat.full[[x]][[y]]$h1y6, h2y6=scalar.dat.full[[x]][[y]]$h2y6, h4y6=scalar.dat.full[[x]][[y]]$h4y6, h6y6=scalar.dat.full[[x]][[y]]$h6y6, h9y6=scalar.dat.full[[x]][[y]]$h9y6, h15y6=scalar.dat.full[[x]][[y]]$h15y6, h16y6=scalar.dat.full[[x]][[y]]$h16y6, h17y6=scalar.dat.full[[x]][[y]]$h17y6, h18y6=scalar.dat.full[[x]][[y]]$h18y6, h3y7=scalar.dat.full[[x]][[y]]$h3y7, h5y7=scalar.dat.full[[x]][[y]]$h5y7, h7y7=scalar.dat.full[[x]][[y]]$h7y7, h8y7=scalar.dat.full[[x]][[y]]$h8y7, h10y7=scalar.dat.full[[x]][[y]]$h10y7, h11y7=scalar.dat.full[[x]][[y]]$h11y7, h12y7=scalar.dat.full[[x]][[y]]$h12y7, h13y7=scalar.dat.full[[x]][[y]]$h13y7, h14y7=scalar.dat.full[[x]][[y]]$h14y7, h19y7=scalar.dat.full[[x]][[y]]$h19y7, h1y8=scalar.dat.full[[x]][[y]]$h1y8, h2y8=scalar.dat.full[[x]][[y]]$h2y8, h4y8=scalar.dat.full[[x]][[y]]$h4y8, h6y8=scalar.dat.full[[x]][[y]]$h6y8, h9y8=scalar.dat.full[[x]][[y]]$h9y8, h15y8=scalar.dat.full[[x]][[y]]$h15y8, h16y8=scalar.dat.full[[x]][[y]]$h16y8, h17y8=scalar.dat.full[[x]][[y]]$h17y8, h18y8=scalar.dat.full[[x]][[y]]$h18y8, h1y9=scalar.dat.full[[x]][[y]]$h1y9, h8y9=scalar.dat.full[[x]][[y]]$h8y9, h9y9=scalar.dat.full[[x]][[y]]$h9y9, h13y9=scalar.dat.full[[x]][[y]]$h13y9, h14y9=scalar.dat.full[[x]][[y]]$h14y9, h15y9=scalar.dat.full[[x]][[y]]$h15y9, h16y9=scalar.dat.full[[x]][[y]]$h16y9, h17y9=scalar.dat.full[[x]][[y]]$h17y9, h18y9=scalar.dat.full[[x]][[y]]$h18y9, h19y9=scalar.dat.full[[x]][[y]]$h19y9)
    # Run model
    
    glimpse(bundle.dat)
    
    tmp.jags[[y]] <- jags(bundle.dat, inits, params, "input/beta_binom_model_elk2022_updated.txt", nc, ni, nb, nt)
  }
  jags_30sims_input[[i]] <-  tmp.jags
}

# setwd("C:/Users/TBRUSH/R/Elk_sightability/out")

save("jags_output", "scalar.dat", file="out/03_Bayesian_analysis_jags_output.RData")
save("eff",file="out/03_Bayesian_analysis_jags_effort.RData")

rm(list = ls())

