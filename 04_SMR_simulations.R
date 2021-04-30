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
aoi <- EPU_poly %>% filter(EPU_Unit_N=="Sechelt Peninsula")


smp_lcn <- st_read(dsn ="./data" , layer="SecheltPen_smpln_opts")
smp_lcn %>% count(options) # 83 "available" sampling locations
smp_lcn %>% group_by(options) %>% summarise(across(c(road_dist, FWA_dist, elev, veg_height), list(min=min, mean=mean, max=max)))

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
ggsave("out/SP_camXY_sim.png",plot=last_plot(), dpi=300)

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
buffer <- 1 # 10 km unit buffer

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
# [1] 704 km2 @ 1 km buffer

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
# curve(dgamma(x,10,10), col='black',xlim=c(0,5), ylim=c(0,2)) ## home ranges between 15-45 ha

## for "elk"
N <- 222
M <- N*1.5
lambda0 <- 0.1
sigma <- 1

J <- nrow(X2) # number of traps
K <- 30   # sampling occasions
n.marked <- 10 # number of marked elk
nlocs = K

dat <- sim.pID.data(N=N, K=K, sigma=sigma, lam0=lambda0, knownID = n.marked,
                    X=X2, xlims=xlims.scaled, ylims=ylims.scaled,
                    obsmod="pois", nmarked='known',
                    tel=n.marked, nlocs=K) # each marked individual has telemetry and each has 1 fix per day (occasion)

glimpse(dat)
sum(dat$n) # number of elk detections (marked and unmarked)
sum(dat$Yknown) # number of marked elk detections

# format simulated data for cSMR NIMBLE code
tmp <- dat$locs
locs <- as.data.frame(rbind(as.data.frame(tmp[1]),
                                as.data.frame(tmp[2]),
                                as.data.frame(tmp[3]),
                                as.data.frame(tmp[4]),
                                as.data.frame(tmp[5])))
colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
plot(locs)
ind <- rep(1:n.marked, each=K)

## Data on marked guys from resighting occasion
yr.aug <- array(0L, c(M, J))
y2d <- apply(dat$Yobs,c(1,2),sum) # 2-d encounter history 'nind' x 'ntraps'
class(y2d) <- "integer"
dim(y2d)

yr.aug[1:n.marked,] <- y2d
dim(yr.aug)
dim(dat$Yobs)
sum(yr.aug) # should be the same as sum(dat$Yobs)

yr.obs <- rowSums(dat$n)
## ----Simulated Data for NIMBLE-----------------------------------------------------------

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

#############################################################################################
###--- for NIMBLE
cSMR.data <- list(locs=locs,y=yr.aug, n=yr.obs)
constants <- list(M=M, x=X2, nMarked=n.marked, J=J, nlocs=nlocs,
                  ind=ind, xlim=xlims.scaled, ylim=ylims.scaled, A=areakm2.scaled)

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

samples <- runMCMC(Cmcmc, 30000, nburnin = 10000,
                   nchains = 3, thin = 10, inits = list(inits(), inits(), inits()))
out <- mcmc.list(list(as.mcmc(samples[[1]]), as.mcmc(samples[[2]]), as.mcmc(samples[[3]])))

###############################################################
###--- View output (JAGS)

################################
###--- view output
mc.cSMR_JAGS.ET #

out <- mc.cSMR_JAGS

summary(window(out, start = 101))
# summary(window(out.eff,start = 1001))
# summary(window(out.eff.unS,start = 1001))

plot(window(out,start = 101))
# plot(window(out.eff,start = 11001))
# plot(window(out.eff,start = 11001))

rejectionRate(window(out ,start = 101))

