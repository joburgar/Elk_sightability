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
# 04_SMR_simulations.R
# script to simulate SMR models to inform camera survey study design
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 07-Apr-2021
# nimble code provided by Paul van Dam-Bates - 16-Apr-2021
#####################################################################################

#####################################################################################
# if working in the elk_sightability project, need to point to correct library
version$major
version$minor
R_version <- paste0("R-",version$major,".",version$minor)

.libPaths(paste0("C:/Program Files/R/",R_version,"/library")) # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# LOAD PACKAGES ####

list.of.packages <- c("tidyverse","lubridate", "timetk", "sf", "rgdal", "Cairo", "rjags","coda","doParallel",  
                      "xtable", "R2jags","data.table","MCMCvis","PNWColors")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#########################################
###--- Input study area  / camera trap location data
# trap ID for marking occasions - unknown so using camera array (elk marked by completely different process, independent)
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_NA")
# SP_poly <- st_read(dsn=GISDir, layer="EPU_Sechelt_Peninsula")
# st_area(SP_poly)*1e-6

aoi <- EPU_poly %>% filter(EPU_Unit_N=="Sechelt Peninsula")

cam_lcn <- read.csv("data/cam_metadata_correct coordintes.csv", head=TRUE) %>% as_tibble()
glimpse(cam_lcn)
cam_sf <- st_as_sf(cam_lcn,coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=cam_sf)

#  UTM Zone 10N, NAD83 (EPSG:26910)
cam_coords <- st_coordinates(cam_sf %>% st_transform(26910))


###--- have both sets of camera data, start with simulations for 40 sites
camXY <- cam_lcn %>% select(Project_Station_ID)
camXY <- cbind(camXY, cam_coords)
colnames (camXY) <- c("trap.id","x","y")
str(camXY)

# specify how much to buffer sites by (in 1 km units, according to coord.scale)
coord.scale <- 1000
buffer <- 2 # 2 km unit buffer

traplocs <- as.matrix(camXY[,c("x","y")])
X <- traplocs/coord.scale
dim(X)

###--- create xlims and ylims of scaled coordinates
Xl <- min(X[,1] - buffer)
X.scaled <- X[,1] - Xl

Yl <- min(X[,2] - buffer)
Y.scaled <- X[,2] - Yl

xlims.scaled <- c(min(X.scaled)-buffer,max(X.scaled)+buffer); ylims.scaled <- c(min(Y.scaled)-buffer,max(Y.scaled)+buffer)

areakm2.scaled <- xlims.scaled[2]*ylims.scaled[2]
# [1] 837 km2 for 57 cams and 2 km buffer

X2 <- as.matrix(cbind(X.scaled,Y.scaled))
dim(X2) # scaled traploc matrix in 1 km units
summary(X2)

## Sechelt Peninsula 95% KDe = summer 31.1 +/- 16.1 and winter = 17 +/- 2
# consider range from 15 km2 to 45 km2
# Following Royle et. al (2011), and assuming a
# chi-squared distribution with 2 degrees of freedom, the range of sigma is given by
# sqrt(15/pi)/sqrt(5.99)  # min 95 % HR  # 0.89 km
# sqrt(45/pi)/sqrt(5.99)  # max 95% HR   # 1.55 km
# spacing cameras ~2 km should be good (aim for distance of 2*sigma) so range from 1.8-3.2 km apart
# In our grid spacing, 1 unit = 1 km, so our we want a prior with most of the density between 0.89 - 1.55
# qgamma(c(0.001,0.5,0.999),10,10)  #  0.2960520 0.9668715 2.2657373 - variable home ranges from 15-45
# curve(dgamma(x,10,10), col='black',xlim=c(0,5), ylim=c(0,2)) ## home ranges between 15-45 km2

###--- for "elk"
N <- 222
M <- N*2   # to be on the safe side
lambda0 <- 1
sigma <- 1

J <- nrow(X2) # number of traps
K <- 30   # sampling occasions
n.marked <- 9 # number of marked elk (max collared elk in Sechelt Peninsula)
nlocs = K


###--- use real telem locations
telem <- read.csv("data/Collars_Sechelt.csv", stringsAsFactors = TRUE) %>% as_tibble() %>%
  dplyr::select(Collar_ID, SCTS__UTC_, Latitude_d, Longitude_ ,Easting, Northing) %>%
  rename(Date.Time.UTC = SCTS__UTC_, Latitude = Latitude_d, Longitude=Longitude_)

telem$Date.Time.UTC <- ymd_hms(telem$Date.Time.UTC, tz = "UTC")
telem$Date.Time.PST <- with_tz(telem$Date.Time.UTC, tz)
telem <- telem %>% mutate(Year = year(Date.Time.PST), Month = month(Date.Time.PST, label = T), jDay = yday(Date.Time.PST))

telem %>% group_by(Collar_ID) %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
telem %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
# telem location data for Sechelt Peninsula from 2021-06-13 19:01:36 to 2022-06-01 10:19:55 

telem_sf <- st_as_sf(telem %>% filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs=4326)
telem_sf %>% count(Collar_ID) %>% st_drop_geometry()
telem_sf$Collar_ID <- as.factor(telem_sf$Collar_ID)

table(telem_sf$Collar_ID, telem_sf$Month)

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=telem_sf %>% filter(Month %in% c("Feb","Mar","Apr")),aes(fill=Collar_ID, col=Collar_ID))+
  geom_sf(data=cam_sf, cex=2, col="blue")
  
# consider population periods based on seasonality and life history traits
# check with Tristen (Dan / Darryl) to finalise seasonaliy dates
# Goal is to have ~60 day intervals to maximise data while minimising violation of closed population assumption
# have calving / summer season as first option and start of our research. Season 1 = calving = 15 June - 15 Aug
# open hunting season (deer) in Sechelt Sept 1- Nov 30. Season 2 = hunting/rutting = Sept 15 - Nov 15
# then overlap with our surveys and winter. Season 3 = winter = 1 Feb - 31 Mar

telem_sf <- telem_sf %>% mutate(SmpPrd = case_when(Date.Time.UTC %>% between("2021-06-15","2021-08-15") ~ 'Smp1',
                                                   Date.Time.UTC %>% between("2021-09-15","2021-11-15") ~ 'Smp2',
                                                   Date.Time.UTC %>% between("2022-02-01","2022-03-31") ~ 'Smp3'))

telem_sf %>% count(SmpPrd)

ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data=telem_sf %>% filter(SmpPrd=="Smp3"),aes(fill=Collar_ID, col=Collar_ID))+
  geom_sf(data=cam_sf, pch=15)
  
  
### Run for Smp1 to start
telem_sf_smp <- telem_sf %>% filter(SmpPrd=="Smp1") %>% arrange(Collar_ID)
num.reps.collar <- telem_sf_smp %>% count(Collar_ID) %>% select(n) %>% st_drop_geometry()
num.reps.collar <- as.numeric(num.reps.collar$n)

locs <- as.data.frame(st_coordinates(telem_sf_smp %>% st_transform(crs=26910))) # convert to NAD 83 UTM Zone 10 for consistency with trapping grid (m)
colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
plot(locs)
ind <- rep(1:length(num.reps.collar), times=num.reps.collar) # have 9 marked individuals and various locations from each (33 to 104)
length(ind);nrow(telem_sf_smp) # 702 locations for 9 animals (check)

###--- create xlims and ylims of scaled coordinates
locs.scaled <- locs/coord.scale
locs.scaled$Xcoord.scaled <- locs.scaled[,1] - Xl
locs.scaled$Ycoord.scaled <- locs.scaled[,2] - Yl

plot(X2)
points(locs.scaled, col="red")

###############################################################################
################################################################################
### START HERE
###############################################################################
################################################################################

###--- simulate data
dat <- sim.pID.data(N=N, K=K, sigma=sigma, lam0=lambda0, knownID = n.marked,
                    X=X2, xlims=xlims.scaled, ylims=ylims.scaled,
                    obsmod="pois", nmarked='known',
                    tel=n.marked, nlocs=K) # each marked individual has telemetry and each has 1 fix per day (occasion)

glimpse(dat)
sum(dat$n) # number of elk detections (marked and unmarked)
sum(dat$Yknown) # number of marked elk detections

###--- use simulated telemetry locations
# tmp <- dat$locs
# locs <- as.data.frame(rbind(as.data.frame(tmp[1]),
#                             as.data.frame(tmp[2]),
#                             as.data.frame(tmp[3]),
#                             as.data.frame(tmp[4]),
#                             as.data.frame(tmp[5]),
#                             as.data.frame(tmp[5]),
#                             as.data.frame(tmp[6]),
#                             as.data.frame(tmp[7]),
#                             as.data.frame(tmp[8]),
#                             as.data.frame(tmp[9]),
#                             as.data.frame(tmp[10])))
# colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
# plot(locs)
# ind <- rep(1:n.marked, each=K)


## Data on marked guys from resighting occasion
yr.aug <- array(0L, c(M, J))
y2d <- apply(dat$Yobs,c(1,2),sum) # 2-d encounter history 'nind' x 'ntraps'
class(y2d) <- "integer"
dim(y2d)

yr.aug[1:n.marked,] <- y2d
dim(yr.aug)
dim(dat$Yobs)
sum(yr.aug) # should be the same as sum(dat$Yobs)
sum(dat$Yobs)
sum(dat$n)

yr.obs <- rowSums(dat$n)
## ----Simulated Data for SMR-----------------------------------------------------------
# Run JAGS code
cSMR.data <- list(M=M,y=yr.aug, n=yr.obs, x=X2, nMarked=n.marked, J=J,
                  #K=K,
                  nlocs=nlocs, ind=ind, locs=locs,
                  xlim=xlims.scaled, ylim=ylims.scaled, A=areakm2.scaled)

jd1 <- cSMR.data
ji1 <- function() list(z=rep(1,M))
jp1 <- c("psi", "lam0", "sigma", "N", "D")


# run model - without accounting for effort
(start.time <- Sys.time())
cl3 <- makeCluster(3)
clusterExport(cl3, c("jd1","ji1","jp1","M"))

cSMR_JAGS <- clusterEvalQ(cl3, {
  library(rjags)
  jm1 <- jags.model("elk_cSMR.jag", jd1, ji1, n.chains=1, n.adapt=1000)
  jc1 <- coda.samples(jm1, jp1, n.iter=30000)
  return(as.mcmc(jc1))
})

mc.cSMR_JAGS <- mcmc.list(cSMR_JAGS)

(end.time <- Sys.time()) # 4 mins for 1000 iterations
mc.cSMR_JAGS.ET <- difftime(end.time, start.time, units='mins')

save("mc.cSMR_JAGS",file="out/mc.elk.cSMR_30KIt.RData")
save("mc.cSMR_JAGS.ET",file="out/mc.elk.cSMR_30KIt.ET.RData")
stopCluster(cl3)

# load("out/cSMR.data.RData")
# load("out/constants.RData")
# all.data <- c(cSMR.data, constants)


cSMR.data <- list(M=M,y=yr.aug, n=yr.obs, x=X2, nMarked=n.marked, J=J,
                  #K=K,
                  nlocs=nlocs, ind=ind, locs=locs.scaled,
                  xlim=xlims.scaled, ylim=ylims.scaled, A=areakm2.scaled)

jd1 <- cSMR.data
ji1 <- function() list(z=rep(1,M))
jp1 <- c("psi", "lam0", "sigma", "N", "D")


#############################################################################################
###--- for NIMBLE
cSMR.data <- list(locs=locs.scaled,y=yr.aug, n=yr.obs)
constants <- list(M=M, x=X2, nMarked=n.marked, J=J, nlocs=nlocs,
                  ind=ind, xlim=xlims.scaled, ylim=ylims.scaled, A=areakm2.scaled)

# save("cSMR.data",file="out/cSMR.data.RData")
# save("constants",file="out/constants.RData")


# inits <- function() list(z=rep(1,M))	# Initialize at psi = 100% That seems inefficient...

inits <- function(){
  p <- runif(1, 0.1, 0.7)
  K <- rbinom(1, M - n.marked, p) + n.marked	# Make sure we init with at least the marked animals.
  list(
    lam0 = runif(1, 0.1, 2),
    psi = p,
    sigma = runif(1, 0.1, 1),
    s = cbind(runif(M, xlims.scaled[1], xlims.scaled[2]),
              runif(M, ylims.scaled[1], ylims.scaled[2])),
    z = c(rep(1,K), rep(0, M-K))
  )
}

# Parameters to save:
params <- c("psi", "lam0", "sigma", "N", "D")

# Load the first Nimble function:
# I would personally just write the other models in here too.
source('NimbleModels.r')	# Load the model.

# Note for Jo: Clustering with Nimble means running compiling 3 independent times for 3 clusters. For now
# let's see how fast it is on one cpu.
# This is where the foreach loop would have to go I think.
#----------------------------------------------------

# Compile the nimble model:
Rmodel <- nimbleModel(cSMR, constants = constants,
                      data = cSMR.data, inits = inits())
conf <- configureMCMC(Rmodel)
conf$setMonitors(params)

# We will sample s[i,1:2] at the same time. Seems trivial but otherwise the
# the compiler will sample s[1,1] then s[1,2] and that's inefficient.
conf$removeSamplers('s')
for(i in 1:M) conf$addSampler(target = paste0('s[', i, ', 1:2]'), type = 'RW_block')
Rmcmc <- buildMCMC(conf)
Cmodel <- compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

samples <- runMCMC(Cmcmc, 20000, nburnin = 10000,
                   nchains = 3, thin = 10, inits = list(inits(), inits(), inits()))
out <- mcmc.list(list(as.mcmc(samples[[1]]), as.mcmc(samples[[2]]), as.mcmc(samples[[3]])))

###############################################################
###--- View output (JAGS)

################################
###--- view output
mc.cSMR_JAGS.ET # 184 min for 20KIT, 60 cameras, M=2*N; 23:06:33 to 03:27:54 for 30KIT

out <- mc.cSMR_JAGS

summary(window(out, start = 11001))
# summary(window(out.eff,start = 1001))
# summary(window(out.eff.unS,start = 1001))

plot(window(out,start = 1001))
# plot(window(out.eff,start = 11001))
# plot(window(out.eff,start = 11001))

rejectionRate(window(out ,start = 1001))


################################
load("out/mc.elk.sim.cSMR_60cam_9telem_20KIt.RData")

str(out[1])
out1 <- out[1]; out2 <- out[2]; out3<-out[3]

Nout1 <- unlist(out1[,2]); Nout2 <- unlist(out2[,2]); Nout3 <- unlist(out3[,2])
Nall <- c(Nout1, Nout2, Nout3)
length(Nall)


# create 30 datasets of simulated camera data from JAGS output
sim.cSMR_60.Sechelt <- vector('list', 30)
names(sim.cSMR_40.Sechelt) <- paste0('sim.cSMR_60.Sechelt', seq_along(sim.cSMR_60.Sechelt))
for(i in seq_along(sim.cSMR_60.Sechelt)){
  Nsample <- sample(Nall, 10000, replace=FALSE)
  sim.cSMR_60.Sechelt.list <- quantile(Nsample, c(0.05, 0.5, 0.95))
  sim.cSMR_60.Sechelt[[i]] <- sim.cSMR_60.Sechelt.list
}

quantile.cSMR_60 <- matrix(NA,30,4)
count <- 1
for(i in 1:nrow(quantile.cSMR_60)){
  quantile.cSMR_60[count,1] <- sim.cSMR_60.Sechelt[[i]][1]
  quantile.cSMR_60[count,2] <- sim.cSMR_60.Sechelt[[i]][2]
  quantile.cSMR_60[count,3] <- sim.cSMR_60.Sechelt[[i]][3]
  quantile.cSMR_60[count,4] <- "cam60"
  count <- count + 1
}

quantile.cSMR_60 <- as.data.frame(quantile.cSMR_60)
colnames(quantile.cSMR_60) <- c("LCL", "Mean", "UCL","Num.Cam")
pop.size=222

quantile.cSMR <- rbind(quantile.cSMR_40, quantile.cSMR_60)
quantile.cSMR$Count <- 1:nrow(quantile.cSMR)
quantile.cSMR$LCL <- as.numeric(quantile.cSMR$LCL)
quantile.cSMR$Mean <- as.numeric(quantile.cSMR$Mean)
quantile.cSMR$UCL <- as.numeric(quantile.cSMR$UCL)

col.cat <- as.character(c("#2028B2","#B2AA20"))

cSMR_simsplot = quantile.cSMR %>%
  ggplot(aes(x = reorder(rownames(quantile.cSMR),Mean), y=Mean, fill=Num.Cam))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(rownames(quantile.cSMR), ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))+
  ylim(c(0,500))
cSMR_simsplot
ggsave(cSMR_simsplot, file="out/cSMR_simsplot_40cam.PNG")


#######################################################################
######################################
#create function to load files and write output to table

#function to create output table for JAGS output
get_JAGS_output <- function(filename){
  out <- filename
  s <- summary(window(out, start = 11001))
  gd <- gelman.diag(window(out, start = 11001),multivariate = FALSE)
  output_table <- rbind(as.data.frame(t(s$statistics)),
                        as.data.frame(t(s$quantiles)),
                        as.data.frame(t(gd$psrf)))
  return(output_table)
}

#function to return model run time (hours)
get_JAGS_ET <- function(filename){
  ET <- parse_number(filename) # parse function no longer working, not sure why
  return(ET)
}

######################################
getwd()
info <- list.files(paste(getwd(),"/out",sep=""),"*.RData")

info.mc <- unique(gsub('ET.','',info))
info.mc.40 <- info.mc[grepl("40cam",info.mc)]
info.mc.60 <- info.mc[grepl("60cam",info.mc)]

######################################

# create datasets of simulated camera data from JAGS output
# output for 40 cam simulations
sim.cSMR_40.Sechelt <- vector('list', length(info.mc.40))
names(sim.cSMR_40.Sechelt) <- paste0('sim.cSMR_40.Sechelt', seq_along(sim.cSMR_40.Sechelt))
for(i in seq_along(sim.cSMR_40.Sechelt)){
  load(paste("out/",info.mc.40[i],sep=""))
  out <- mc.cSMR_JAGS
  sim.cSMR_40.Sechelt.list <- get_JAGS_output(out)
  sim.cSMR_40.Sechelt[[i]] <- sim.cSMR_40.Sechelt.list
}

quantile.cSMR_40 <- matrix(NA,length(info.mc.40),5)
count <- 1
for(i in 1:nrow(quantile.cSMR_40)){
  quantile.cSMR_40[count,1] <- sim.cSMR_40.Sechelt[[i]][5,"N"]
  quantile.cSMR_40[count,2] <- sim.cSMR_40.Sechelt[[i]][7,"N"]
  quantile.cSMR_40[count,3] <- sim.cSMR_40.Sechelt[[i]][9,"N"]
  quantile.cSMR_40[count,4] <- "cam40"
  quantile.cSMR_40[count,5] <- count
  count <- count + 1
}

quantile.cSMR_40 <- as.data.frame(quantile.cSMR_40)
colnames(quantile.cSMR_40) <- c("LCL", "Mean", "UCL","Num.Cam","Sim")

# output for 60 cam simulations
sim.cSMR_60.Sechelt <- vector('list', length(info.mc.60))
names(sim.cSMR_60.Sechelt) <- paste0('sim.cSMR_40.Sechelt', seq_along(sim.cSMR_60.Sechelt))
for(i in seq_along(sim.cSMR_60.Sechelt)){
  load(paste("out/",info.mc.60[i],sep=""))
  out <- mc.cSMR_JAGS
  sim.cSMR_60.Sechelt.list <- get_JAGS_output(out)
  sim.cSMR_60.Sechelt[[i]] <- sim.cSMR_60.Sechelt.list
}

quantile.cSMR_60 <- matrix(NA,length(info.mc.60),5)
count <- 1
for(i in 1:nrow(quantile.cSMR_60)){
  quantile.cSMR_60[count,1] <- sim.cSMR_60.Sechelt[[i]][5,"N"]
  quantile.cSMR_60[count,2] <- sim.cSMR_60.Sechelt[[i]][7,"N"]
  quantile.cSMR_60[count,3] <- sim.cSMR_60.Sechelt[[i]][9,"N"]
  quantile.cSMR_60[count,4] <- "cam60"
  quantile.cSMR_60[count,5] <- count
  count <- count + 1
}


quantile.cSMR_60 <- as.data.frame(quantile.cSMR_60)
colnames(quantile.cSMR_60) <- c("LCL", "Mean", "UCL","Num.Cam","Sim")


pop.size=222

quantile.cSMR <- rbind(quantile.cSMR_40, quantile.cSMR_60)
quantile.cSMR$LCL <- as.numeric(quantile.cSMR$LCL)
quantile.cSMR$Mean <- as.numeric(quantile.cSMR$Mean)
quantile.cSMR$UCL <- as.numeric(quantile.cSMR$UCL)

col.cat <- as.character(c("#2028B2","#B2AA20"))
quantile.cSMR <- quantile.cSMR %>% arrange(Num.Cam, Mean)
quantile.cSMR$Graph.Order <- 1:nrow(quantile.cSMR)

cSMR_simsplot = quantile.cSMR %>%
  ggplot(aes(x = Graph.Order, y=Mean, fill=Num.Cam))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(Graph.Order, ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
cSMR_simsplot
ggsave(cSMR_simsplot, file="out/cSMR_simsplot_40-60cam_9telem.PNG")

