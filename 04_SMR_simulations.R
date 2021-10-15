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

#########################################
###--- Input study area  / camera trap location data
# trap ID for marking occasions - unknown so using camera array (elk marked by completely different process, independent)
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_NA")
# SP_poly <- st_read(dsn=GISDir, layer="EPU_Sechelt_Peninsula")
# st_area(SP_poly)*1e-6

aoi <- EPU_poly %>% filter(EPU_Unit_N=="Sechelt Peninsula")

smp_lcn <- st_read(dsn ="./data" , layer="SecheltPen_smpln_opts")
smp_lcn %>% count(options) # 83 "available" sampling locations
smp_lcn %>% group_by(options) %>% summarise(across(c(road_dist, FWA_dist, elev, veg_height), list(min=min, mean=mean, max=max)))

elk_kml <- "./data/elk_potential_sites.kml"
smp_elk <- st_read(elk_kml)
smp_elk_cov <- read.csv("data/elk_all_sites.csv")

aerial_actual_kml <- "./data/aerial_actual_elk.kml"
aerial_actual_elk <- st_read(aerial_actual_kml)
aerial_coordinates <- st_coordinates(aerial_actual_elk)
aerial_coordinates <- aerial_coordinates[,1:2]
write.csv(aerial_coordinates, "data/aerial_actual_elk.csv")

actual_cam_lcn <- read.csv("data/camera_actual_location_SP.csv")
actual_cam_lcn <- st_as_sf(actual_cam_lcn,coords = c("Longitude", "Latitude"), crs = 4326)
st_write(actual_cam_lcn %>% st_transform(crs=4326) %>% select(NAME = CAM), "data/actual_cam_lcn.kml", driver = "kml", delete_dsn = TRUE)


smp_elk <- left_join(smp_elk, smp_elk_cov, by=c("Name"="Lcn"))
smp_elk <- smp_elk %>% filter(Options!="exclude") %>% select(-options)
smp_elk %>% count(Options)
smp_elk <- smp_elk %>%  mutate(Longitude = st_coordinates(.)[,1],Latitude = st_coordinates(.)[,2])

ggplot()+
  geom_sf(data = smp_elk, aes(fill=Options, col=Options))


smp_lcn_500m <- st_buffer(smp_elk %>% st_transform(crs=26910), dist=500)
write.csv(smp_elk %>% st_drop_geometry, "out/smp_elk_cov.csv", row.names = FALSE)
smp_elk <- read.csv("out/smp_elk_cov.csv")
smp_elk <- st_as_sf(smp_elk,coords = c("Longitude", "Latitude"), crs = 4326)
st_write(smp_elk %>% st_transform(crs=4326) %>% select(NAME = Lcn), "data/elk_potential_sites.kml", driver = "kml", delete_dsn = TRUE)

st_write(smp_elk %>% filter(Options=="truck") %>% st_transform(crs=4326) %>% select(NAME = Name), "data/elk_truck_sites.kml", driver = "kml", delete_dsn = TRUE)
st_write(smp_elk %>% filter(Options!="truck") %>% st_transform(crs=4326) %>% select(NAME = Name), "data/elk_aerial_atv_sites.kml", driver = "kml", delete_dsn = TRUE)

# st_write(smp_lcn_500m %>% st_transform(crs=4326) %>% select(NAME = Lcn), "data/elk_sites_500mbuff.kml", driver = "kml", delete_dsn = TRUE)
# st_write(smp_lcn %>% filter(options=="available") %>% st_transform(crs=4326) %>% select(NAME = Lcn), "data/elk_potential_sites.kml", driver = "kml", delete_dsn = TRUE)
# write.csv(smp_lcn %>% filter(options=="available") %>% st_drop_geometry(), "data/elk_potential_sites.csv", row.names = FALSE)

# randomly sample 40 and 60 available locations for simulations, make the 40 a subset of the 60
# smp_lcn$Lcn <- paste("SP", str_pad(as.character(rownames(smp_lcn)),3,pad ="0"), sep="")
# smp_lcn_available <- smp_lcn %>% filter(options=="available")
# camXY_60 <- smp_lcn_available[sample(nrow(smp_lcn_available), 60), ]
# camXY_40 <- camXY_60[sample(nrow(camXY_60), 40), ]
# camXY_60_sites <- camXY_60$Lcn
# camXY_40_sites <- camXY_40$Lcn
#
# smp_lcn$Sim40 <- ifelse(smp_lcn$Lcn %in% camXY_40_sites,"Sim40",NA)
# smp_lcn$Sim60 <- ifelse(smp_lcn$Lcn %in% camXY_60_sites,"Sim60",NA)
# st_write(smp_lcn, "data/SecheltPen_smpln_opts.shp", delete_layer = TRUE)

camXY_40 <- smp_lcn %>% filter(Sim40=="Sim40")
camXY_60 <- smp_lcn %>% filter(Sim60=="Sim60")

ggplot()+
  geom_sf(data = aoi) +
  geom_sf(data = smp_lcn %>% filter(options=="available")) +
  geom_sf(data = camXY_60, col="red") +
  geom_sf(data = camXY_40, col="blue")
# ggsave("out/SP_camXY_sim.png",plot=last_plot(), dpi=300)

camXY_40 <- as.data.frame(st_coordinates(camXY_40))
trap.id <- smp_lcn %>% filter(Sim40=="Sim40") %>% select(Lcn) %>% st_drop_geometry()
camXY_40$trap.id <- trap.id$Lcn

camXY_60 <- as.data.frame(st_coordinates(camXY_60))
trap.id <- smp_lcn %>% filter(Sim60=="Sim60") %>% select(Lcn) %>% st_drop_geometry()
camXY_60$trap.id <- trap.id$Lcn

###--- have both sets of camera data, start with simulations for 40 sites
camXY <- camXY_40[c(3,1,2)]
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
# [1] 816 km2 for 40 cams and @ 2 km buffer; 864 km2 for 60 cams and 2 km buffer

X2 <- as.matrix(cbind(X.scaled,Y.scaled))
dim(X2) # scaled traploc matrix in 1 km units
summary(X2)



## ----sim1----------------------------------------------------------------
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
telem <- read.csv("data/Rd1_telem.csv", stringsAsFactors = TRUE)
telem$Latitude <- as.numeric(substr(telem$geometry, 3,10))
telem$Longitude <- as.numeric(substr(telem$X, 2, 9))

#- filter for only Sechelt Peninsula and sample for only K number of  fixes per animal; convert telem_dat to sf
by_Animal.ID <- telem %>% filter(EPU.Fix=="Sechelt Peninsula" & Animal.ID!="Elk020" & Year > 2019) %>% group_by(Animal.ID)
telem_SP <- sample_n(by_Animal.ID, K, replace=TRUE)
# telem_SP %>% filter(is.na(Longitude)) # issues with some latitude ending in 0  not showing up

telem_sf <- st_as_sf(telem_SP %>% filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs=4326)
telem_sf %>% count(Animal.ID)
# st_write(telem_sf, "data/telem_SP_sim.shp")

ggplot()+
  geom_sf(data=telem_sf,aes(fill=Animal.ID, col=Animal.ID))

telem_sf <- telem_sf %>% arrange(Animal.ID)
locs <- as.data.frame(st_coordinates(telem_sf %>% st_transform(crs=26910))) # convert to NAD 83 UTM Zone 10 for consistency with trapping grid (m)
colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
plot(locs)
ind <- rep(1:n.marked, each=K) # have 9 marked individuals and 30 random locations from each

###--- create xlims and ylims of scaled coordinates
locs.scaled <- locs/coord.scale
locs.scaled$Xcoord.scaled <- locs.scaled[,1] - Xl
locs.scaled$Ycoord.scaled <- locs.scaled[,2] - Yl

plot(X2)
points(locs.scaled, col="red")

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

