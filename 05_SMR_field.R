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
list.of.packages <- c("tidyverse","lubridate", "readxl","timetk", "sf", "rgdal", "Cairo", "rjags","coda","doParallel",  
                      "xtable", "R2jags","data.table","MCMCvis","PNWColors", "AHMbook")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


###############################################################################
# LOAD AERIAL DATA ####
aerialDIR <- "//Sfp.idir.bcgov/s140/S40073/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/4. Technical/Data/Aerial_SMR"
list.files(aerialDIR)

# transects
aerialTransects <- st_read(paste(aerialDIR,"Flight3/Path_Flight3.kml", sep="/")) %>% st_transform(26910)
aerialTransects$length <- st_length(aerialTransects)

# need to sort through the transect data - review SCR book for line transect surveys
tmp1 <- st_cast(aerialTransects, "MULTIPOINT")
tmp2 <- st_coordinates(tmp1)
tmp3 <- tmp2[seq(1, nrow(tmp2), 20), ]
tmp4 <- st_cast(aerialTransects %>% filter(grepl("34", Name)), "MULTIPOINT")


dim(tmp4)
plot(tmp3)

ggplot()+
  geom_sf(data=aerialTransects, col="black") +
  # geom_sf(data=tmp4)
  # geom_sf(data=test, col="red") +
  geom_sf(data=obsSMR_sf)
  

# survey data
aerialSMR1 <- read_excel(paste(aerialDIR,"Flight1/2022Mar2_SMR_Data.xlsx", sep="/"), 
                       sheet = "Data", range = "A1:T12", col_types = "text") %>% type_convert()
aerialSMR1$SurveyNum <- 1

aerialSMR2 <- read_excel(paste(aerialDIR,"Flight2/2022Mar15_SMR_Data.xlsx", sep="/"), 
                         sheet = "Data", range = "A1:U13", col_types = "text") %>% type_convert()
aerialSMR2$SurveyNum <- 2

aerialSMR3 <- read_excel(paste(aerialDIR,"Flight3/2022Mar28_SMR_Data.xlsx", sep="/"), 
                         sheet = "Data", range = "A1:U10", col_types = "text") %>% type_convert()
aerialSMR3$SurveyNum <- 3

aerialSMR <- bind_rows(aerialSMR1 %>% select(`Elk Lat & Long`, `Group Size`, `Collar ID`, SurveyNum),
                       aerialSMR2 %>% select(`Elk Lat & Long`, `Group Size`, `Collar ID`, SurveyNum), 
                       aerialSMR3 %>% select(`Elk Lat & Long`, `Group Size`, `Collar ID`, SurveyNum))

aerialSMR$Latitude <- as.numeric(substr(aerialSMR$`Elk Lat & Long`, 1, 9))
aerialSMR$Longitude <- as.numeric(substr(aerialSMR$`Elk Lat & Long`, 12, nchar(aerialSMR$`Elk Lat & Long`)))

aerialSMR <- aerialSMR %>% rename(GrpSze = `Group Size`, CollarID = `Collar ID`)
aerialSMR <- aerialSMR[c("SurveyNum","Latitude","Longitude","CollarID","GrpSze")]

aerialSMR$Collar_ID <- as.numeric(gsub("([^0-9])", "", aerialSMR$CollarID))
aerialSMR %>% filter(!is.na(Collar_ID))

# one group had two collars & two groups had an old collar
# not sure how best to model this
# 1. creating two rows for the observation
# 2. divide the GrpSize between the two
# 3. attribute each duplicated row to just one collar
# 4. ignore the old collars as not unique ID
aerialSMR <- rbind(aerialSMR, aerialSMR[rep(26, times=1),])
# manual changing (I know not great but easy)
aerialSMR[26,]$Collar_ID <- 42256
aerialSMR[33,]$Collar_ID <- 42257

aerialSMR$GrpSze <- case_when(aerialSMR$Collar_ID==42256 ~ ceiling(17/2),
                              aerialSMR$Collar_ID==42257 ~ floor(17/2),
                              TRUE~aerialSMR$GrpSze)

obsSMR_sf <- st_as_sf(aerialSMR, coords=c("Longitude", "Latitude"), crs=4326)


aerialSMR %>% filter(!is.na(Collar_ID))
# 7 active collars seen during 3 surveys: 3 in Survey1, 1 in Survey2, and 4 in Survey3
# 1 collar was seen in two surveys for a total of 8 collared observations

active.collars 

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=aerialTransects %>% filter(grepl(02, Name))) +
  geom_sf(data=obsSMR_sf %>% st_transform(26910)) +
  geom_sf(data=obsSMR_sf %>% filter(!is.na(Collar_ID)) %>% st_transform(26910), col="red")
  


aerialSMR %>% count(Collar_ID)

as.data.frame(aerialSMR)

active.collars
telem_sf %>% count(Collar_ID) %>% st_drop_geometry()

# create lat and long for elk position ('Elk Lat & Long')
# Subset to tibble with Latitude, Longitude, group size (`Group Size`) and collar ID (`Collar ID`)

################################################################################
# ## For simulations
# ## Sechelt Peninsula 95% KDe = summer 31.1 +/- 16.1 and winter = 17 +/- 2
# # consider range from 15 km2 to 45 km2
# # Following Royle et. al (2011), and assuming a
# # chi-squared distribution with 2 degrees of freedom, the range of sigma is given by
# # sqrt(15/pi)/sqrt(5.99)  # min 95 % HR  # 0.89 km
# # sqrt(45/pi)/sqrt(5.99)  # max 95% HR   # 1.55 km
# # spacing cameras ~2 km should be good (aim for distance of 2*sigma) so range from 1.8-3.2 km apart
# 
# ###--- for "elk"
# N <- 222
# M <- N*2   # to be on the safe side
# lambda0 <- 1
# sigma <- 1
# 
# J <- nrow(X2) # number of traps
# K <- 30   # sampling occasions
# n.marked <- 9 # number of marked elk (max collared elk in Sechelt Peninsula)
# nlocs = K


###--- simulate data
# use function from scrbook
# sim.pID.data <- function(N=N, K=K, sigma=sigma, lam0=lam0, knownID=knownID,X=X,
#                          xlims=xlims, ylims=ylims,  obsmod= c("pois", "bern"), nmarked=c("known", "unknown"),rat=1, tel =0, nlocs=0)
# {
#   
#   ###add an error message for when there are more tel guys than nmarked
#   if(tel>knownID) stop ("tel cannot be bigger than knownID")
#   
#   obsmod <- match.arg(obsmod)
#   nmarked <- match.arg(nmarked)
#   
#   # Home range centers
#   npts<-dim(X)[1]
#   sx <- runif(N, xlims[1], xlims[2])
#   sy <- runif(N, ylims[1], ylims[2])
#   S <- cbind(sx, sy)
#   D <- e2dist(S, X)
#   lam <- lam0*exp(-(D*D)/(2*sigma*sigma))
#   Y <- array(NA, c(N, npts, K))
#   for (i in 1:N){
#     for (j in 1: npts){
#       
#       if (identical(obsmod, "bern")){
#         Y[i,j,] <- rbinom(K,1, lam[i,j])
#       } else if (identical(obsmod, "pois"))  {
#         Y[i,j,] <- rpois(K,lam[i,j])
#       }
#     }}
#   
#   n <- apply(Y, c(2,3), sum)
#   
#   Yknown <- Y[1:knownID,,]
#   
#   if (identical(nmarked, "unknown")){
#     iobs<-which(apply(Yknown>0,1,any))
#     Yobs<-Yknown[iobs,,]
#   } else if (identical(nmarked, "known")){
#     Yobs<-Yknown }
#   
#   YknownR<-Yobs
#   counter<-array(0, c(dim(Yobs)[1],dim(X)[1],K ))
#   for (i in 1:dim(Yobs)[1]){
#     for (j in 1: dim(X)[1]){
#       for (k in 1:K){
#         
#         if (identical(obsmod, "bern")){
#           if (YknownR[i,j,k] ==1 ) {
#             IDed<-rbinom(1,1,rat)
#             if (IDed ==0) { 
#               YknownR[i,j,k]<-0
#               counter[i,j,k]<-1} #counter is the number of marked records that cannot be identified to individual level
#           }
#         } else if (identical(obsmod, "Ypois")) {
#           if (Yobs[i,j,k] > 0 ) {
#             
#             IDed<-sum(rbinom(Yobs[i,j,k] ,1,rat))
#             YknownR[i,j,k]<-IDed
#             
#             if (IDed!=Yobs[i,j,k] ) { 
#               counter[i,j,k]<-Yobs[i,j,k]-IDed}
#           }
#         }
#         
#         
#       }}}
#   
#   n<-n-apply(counter, 2:3, sum) #subtract unidentified pictures from n
#   
#   #generate telemetry locations if tel>0
#   if (tel>0) {
#     
#     itel<-sort(sample(1:knownID, tel, replace=F))
#     locs<-list()
#     for (i in 1:tel){
#       lx<-rnorm(nlocs, S[itel[i],1], sigma)
#       ly<-rnorm(nlocs, S[itel[i],2], sigma)
#       locs[[i]]<-cbind(lx, ly)
#     }
#     
#   } else {
#     locs<-NULL
#     itel<-NULL}
#   
#   list(n=n,Y=Y, Yknown=Yknown, Yobs=Yobs, YknownR=YknownR, counter=sum(counter), locs=locs,telID=itel)
#   
# }
# 
# dat <- sim.pID.data(N=N, K=K, sigma=sigma, lam0=lambda0, knownID = n.marked,
#                     X=X2, xlims=xlims.scaled, ylims=ylims.scaled,
#                     obsmod="pois", nmarked='known',
#                     tel=n.marked, nlocs=K) # each marked individual has telemetry and each has 1 fix per day (occasion)
################################################################################
###--- function to organise camera data for SMR

# consider area in ha, unit size is 100 m = 1 unit, buffer in unit lengths
organise_camera_SMR_data <- function(cam_sf=cam_sf, cam_dat=cam_dat, eff=eff, telem_sf=telem_sf, M=400, 
                                     coord.scale=100, buffer=20, start_date="2021-06-15", end_date="2021-09-01"){
  ###--- Camera meta data
  #  UTM Zone 10N, NAD83 (EPSG:26910)
  cam_coords <- st_coordinates(cam_sf %>% st_transform(26910))
  
  camXY <- cam_lcn %>% select(station_id)
  camXY <- cbind(camXY, cam_coords)
  colnames (camXY) <- c("trap.id","x","y")
  
  traplocs <- as.matrix(camXY[,c("x","y")])
  X <- traplocs/coord.scale
  
  ###--- create xlims and ylims of scaled coordinates, and area
  Xl <- min(X[,1] - buffer)
  X.scaled <- X[,1] - Xl
  
  Yl <- min(X[,2] - buffer)
  Y.scaled <- X[,2] - Yl
  
  xlims.scaled <- c(min(X.scaled)-buffer,max(X.scaled)+buffer); ylims.scaled <- c(min(Y.scaled)-buffer,max(Y.scaled)+buffer)
  
  areaha.scaled <- xlims.scaled[2]*ylims.scaled[2]
  
  X2 <- as.matrix(cbind(X.scaled,Y.scaled))
  
  ### creating a data frame of study dates to align with occasions
  study.days <- colnames(eff[2:ncol(eff)])
  study.days <- str_replace(study.days, "X", "")
  study.days <- as.data.frame(study.days)
  study.days$Date <- ymd(study.days$study.days)
  study.days$Year <- year(study.days$Date)
  study.days$Month <- month(study.days$Date)
  study.days$jDay <- yday(study.days$Date)
  study.days$Occ <- rownames(study.days)
  
  study_period_days <- study.days%>% filter(between(Date, as.Date(start_date),as.Date(end_date))) %>% as_tibble
  study_period_days$SP_Occ <- rownames(study_period_days) # get the occasions for the study period, starting at 1
  
  ### creating operability matrix with rownames as station_id (same order as X2) and colnames as occasions
  rownames(eff) <- eff$station_id
  camop <- as.matrix(eff[,2:ncol(eff)])
  colnames(camop) <- 1:ncol(camop)
  SP_Occ_start <- min(as.numeric(study_period_days$Occ))
  SP_Occ_end <- max(as.numeric(study_period_days$Occ))
  
  camop <- camop[,SP_Occ_start:SP_Occ_end]
  
  telem_sf <- telem_sf %>% mutate(SmpPrd = case_when(Date.Time.PST %>% between(start_date, end_date) ~ 'SmpPrd'))
  
  telem_sf_smp <- telem_sf %>% filter(SmpPrd=="SmpPrd") %>% arrange(Collar_ID)
  active.collars <- unique(telem_sf_smp$Collar_ID)
  active.collars <- droplevels(active.collars)
  num.reps.collar <- telem_sf_smp %>% count(Collar_ID) %>% select(n) %>% st_drop_geometry()
  num.reps.collar <- as.numeric(num.reps.collar$n)
  
  locs <- as.data.frame(st_coordinates(telem_sf_smp %>% st_transform(crs=26910))) # convert to NAD 83 UTM Zone 10 for consistency with trapping grid (m)
  colnames(locs) <- c("Xcoord.scaled","Ycoord.scaled")
  nlocs <- nrow(locs)

  locs.scaled <- locs/coord.scale
  locs.scaled$Xcoord.scaled <- locs.scaled[,1] - Xl
  locs.scaled$Ycoord.scaled <- locs.scaled[,2] - Yl
  
  ind <- rep(1:length(num.reps.collar), times=num.reps.collar) #  marked individuals and various locations from each
  
  cam_dat <- cam_dat %>% select(station_id, species, report_names, date_time, collar, collar_tags, event_id, event_duration, event_groupsize, event_observations)
  
  cam_dat$date_time <- ymd_hms(cam_dat$date_time, tz = tz)
  cam_dat$Date <- as.Date(cam_dat$date_time)
  cam_dat <- cam_dat %>% mutate(Year = year(Date), Month = lubridate::month(Date, label = T), jDay = yday(Date))
  
  cam_dat <- cam_dat %>% mutate(SmpPrd = case_when(Date %>% between(start_date, end_date) ~ 'SmpPrd'))
  cam_dat_smp <- cam_dat %>% filter(SmpPrd=="SmpPrd")
  elk_dat <- cam_dat_smp %>% filter(grepl("Cervus", species))
  
  grp_size <- elk_dat %>% summarise(min = min(event_groupsize), mean = mean(event_groupsize), max = max(event_groupsize))
  
  # to  match with telem with individuals
  ind.lookup <- as.data.frame(active.collars)
  colnames(ind.lookup) <- "active.collar"
  ind.lookup$ind <- rep(1:nrow(ind.lookup))
  
  elk_dat$collar_tags <- as.factor(elk_dat$collar_tags)
  elk_dat <- left_join(elk_dat, ind.lookup, by=c("collar_tags"="active.collar"))
  elk_dat <- left_join(elk_dat %>% select(!date_time), 
                       study.days %>% 
                         filter(between(Date, as.Date(start_date), as.Date(end_date))) %>% 
                         select("Occ", "jDay"))
  elk_dat$SP_Occ <- as.numeric(study_period_days$SP_Occ[match(elk_dat$Occ, study_period_days$Occ)])
  
  ind_dat <- elk_dat %>% filter(!is.na(ind)) %>% select(!collar) %>% arrange(ind, SP_Occ)
  
  # dat$Yobs is a matrix nind:J:K
  # rows are the 9 inds, columns are the traps J, and different slices for each occasion
  n.marked = length(active.collars)
  ind_dat$trapID <- rownames(camXY)[match(ind_dat$station_id, camXY$trap.id)]
  Yobs_ind_dat <- ind_dat %>% group_by(trapID, ind) %>% count(SP_Occ) %>% arrange(SP_Occ)
  
  K = nrow(study_period_days) 
  J = nrow(camXY)
  
  # create an empty array with rows as # of individuals, columns as camera traps, and slices as Occasions
  Yobs <- array(0L, c(n.marked,J,K))
  
  for(i in 1:nrow(Yobs_ind_dat)){
    ind.value <- as.numeric(Yobs_ind_dat[i,c("ind")])
    J.value <- as.numeric(Yobs_ind_dat[i,c("trapID")])
    K.value <- as.numeric(Yobs_ind_dat[i,c("SP_Occ")])
    dtn.value <- as.numeric(Yobs_ind_dat[i,c("n")])
    
    Yobs[ind.value,J.value,K.value] <- dtn.value
  }
  
  # sum(Yobs); sum(Yobs_ind_dat$n) # check to make sure the same
  
  ## Data on marked guys from resighting occasion
  yr.aug <- array(0L, c(M, J))
  y2d <- apply(Yobs,c(1,2),sum) # 2-d encounter history 'nind' x 'ntraps' # remove k for faster processing
  class(y2d) <- "integer"
  
  yr.aug[1:n.marked,] <- y2d
  # dim(yr.aug)
  # dim(Yobs)
  # sum(yr.aug) # should be the same as sum(Yobs)
  
  n.tmp <- elk_dat %>% group_by(station_id) %>% count(SP_Occ)
  
  n.tmp$trapID <- rownames(camXY)[match(n.tmp$station_id, camXY$trap.id)]
  n.tmp2 <- n.tmp %>% ungroup() %>% select(-station_id) %>% arrange(trapID)
  
  # create an empty array with rows as # of individuals, columns as camera traps, and slices as Occasions
  n <- array(0L, c(J,K))
  
  for(i in 1:nrow(n.tmp2)){
    J.value <- as.numeric(n.tmp2[i,c("trapID")])
    K.value <- as.numeric(n.tmp2[i,c("SP_Occ")])
    dtn.value <- as.numeric(n.tmp2[i,c("n")])
    
    n[J.value,K.value] <- dtn.value
  }
  
  sum(n.tmp2$n); sum(n); nrow(elk_dat) # check they are all the same
  
  yr.obs <- rowSums(n) # removing k for faster processing
  
  return(list(M=M,yr.aug=yr.aug, yr.obs=yr.obs, n=n, Yobs_ind_dat=Yobs_ind_dat, X2=X2, n.marked=n.marked, J=J,K=K,
              nlocs=nlocs, ind=ind, locs.scaled=locs.scaled, camop = camop, grp_size=grp_size,
              xlims.scaled=xlims.scaled, ylims.scaled=ylims.scaled, areaha.scaled=areaha.scaled))
}


################################################################################
###--- For field data ---###

###--- Input study area  / camera trap location data
# trap ID for marking occasions - unknown so using camera array (elk marked by completely different process, independent)
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_NA")

aoi <- EPU_poly %>% filter(EPU_Unit_N=="Sechelt Peninsula")

cam_lcn <- read.csv("data/SPCS_station_data.csv", head=TRUE) %>% as_tibble()
glimpse(cam_lcn)
cam_lcn %>% count(treatment)
cam_lcn <- cam_lcn %>% mutate(habitat = case_when(treatment=="Forest - Mixed" ~ "Forest_Mixed",
                                                  grepl("Forest - Conifer", treatment) ~ "Forest_Conifer",
                                                  treatment=="Forest - Grass - Road" ~ "Forest_Conifer",
                                                  grepl("Open", treatment) ~ "Open"))

cam_lcn %>% count(habitat)
cam_lcn <- cam_lcn %>% arrange(station_id)
cam_lcn <- cam_lcn %>% filter(station_id != c("SPCS17", "SPCS61")) # remove the two stations without data

cam_sf <- st_as_sf(cam_lcn,coords = c("longitude", "latitude"), crs = 4326)

cam_dat <- read.csv("data/SPCS_30min_Independent.csv", head=TRUE) %>% as_tibble() %>% type_convert()

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=cam_sf, aes(col=habitat))

eff <- read.csv("data/SPCS_operability_matrix.csv", head=TRUE)

telem <- read.csv("data/Collars_Sechelt.csv", stringsAsFactors = TRUE) %>% as_tibble() %>%
  dplyr::select(Collar_ID, SCTS__UTC_, Latitude_d, Longitude_ ,Easting, Northing) %>%
  rename(Date.Time.UTC = SCTS__UTC_, Latitude = Latitude_d, Longitude=Longitude_)

###--- telem locations
telem$Date.Time.UTC <- ymd_hms(telem$Date.Time.UTC, tz = "UTC")
telem$Date.Time.PST <- with_tz(telem$Date.Time.UTC, tz)
telem <- telem %>% mutate(Year = year(Date.Time.PST), Month = lubridate::month(Date.Time.PST, label = T), jDay = yday(Date.Time.PST))

telem %>% group_by(Collar_ID) %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
telem %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
# telem location data for Sechelt Peninsula from 2021-06-13 19:01:36 to 2022-06-01 10:19:55 

telem_sf <- st_as_sf(telem %>% filter(!is.na(Longitude)), coords = c("Longitude", "Latitude"), crs=4326)
telem_sf %>% count(Collar_ID) %>% st_drop_geometry()
telem_sf$Collar_ID <- as.factor(telem_sf$Collar_ID)

telem_sf %>% summarise(min(Date.Time.PST), max(Date.Time.PST)) %>% st_drop_geometry()

table(telem_sf$Collar_ID, telem_sf$Month)

ggplot()+
  geom_sf(data=aoi)+
  geom_sf(data=telem_sf %>% filter(Month %in% c("Feb","Mar","Apr")),aes(fill=Collar_ID, col=Collar_ID))+
  geom_sf(data=cam_sf, cex=2, col="blue")


##########################################
###--- create camera data for different study periods

# consider population periods based on seasonality and life history traits
# check with Tristen (Dan / Darryl) to finalise seasonaliy dates
# open hunting season (deer) in Sechelt Sept 1- Nov 30. Season 2 = hunting/rutting = Sept 15 - Nov 15
# Goal is to have ~90 day intervals to maximise data while minimising violation of closed population assumption
# have calving / summer season as first option and start of our research. Season 1 = calving = 15 June - 1 Sept
# then overlap with our surveys and winter. Season 2 = winter = 1 Jan - 31 Mar


cSMR_smp1 <- organise_camera_SMR_data(cam_sf=cam_sf, cam_dat=cam_dat, eff=eff, telem_sf=telem_sf,
                                      M=400,coord.scale=100, buffer=20, start_date="2021-06-15", end_date="2021-09-01")
  
cSMR_smp2 <- organise_camera_SMR_data(cam_sf=cam_sf, cam_dat=cam_dat, eff=eff, telem_sf=telem_sf,
                                      M=400,coord.scale=100, buffer=20, start_date="2022-01-01", end_date="2022-03-31")


cSMR_smp1$grp_size # 1  2.72    13
cSMR_smp2$grp_size # 1  3.11    10

cSMR_smp1$cam_eff <- rowSums(cSMR_smp1$camop)/cSMR_smp1$K
cSMR_smp2$cam_eff <- rowSums(cSMR_smp2$camop)/cSMR_smp2$K


# #########################################
# cSMR_smp1$grp_size # 1  2.72    13
# cSMR_smp2$grp_size # 1  3.11    10
# for now let's model events, and multiply density by average group size for total animals

## ----Data for SMR-----------------------------------------------------------
# Run JAGS code

cSMR.data <- list(M=cSMR_smp1$M,y=cSMR_smp1$yr.aug, n=cSMR_smp1$yr.obs, x=cSMR_smp1$X2, nMarked=cSMR_smp1$n.marked, 
                  J=cSMR_smp1$J, effort=cSMR_smp1$cam_eff, nlocs=cSMR_smp1$nlocs, ind=cSMR_smp1$ind, locs=cSMR_smp1$locs.scaled,
                  xlim=cSMR_smp1$xlims.scaled, ylim=cSMR_smp1$ylims.scaled, A=cSMR_smp1$areaha.scaled)

cSMR.data <- list(M=cSMR_smp2$M,y=cSMR_smp2$yr.aug, n=cSMR_smp2$yr.obs, x=cSMR_smp2$X2, nMarked=cSMR_smp2$n.marked, 
                  J=cSMR_smp2$J, effort=cSMR_smp2$cam_eff, nlocs=cSMR_smp2$nlocs, ind=cSMR_smp2$ind, locs=cSMR_smp2$locs.scaled,
                  xlim=cSMR_smp2$xlims.scaled, ylim=cSMR_smp2$ylims.scaled, A=cSMR_smp2$areaha.scaled)


M=400

jd1 <- cSMR.data
ji1 <- function() list(z=rep(1,M))
jp1 <- c("psi", "lam0", "sigma", "N", "D")

# run model - accounting for effort
(start.time <- Sys.time())
cl3 <- makeCluster(3)
clusterExport(cl3, c("jd1","ji1","jp1","M"))

cSMR_JAGS <- clusterEvalQ(cl3, {
  library(rjags)
  jm1 <- jags.model("elk_cSMR_trapeff.jag", jd1, ji1, n.chains=1, n.adapt=1000)
  jc1 <- coda.samples(jm1, jp1, n.iter=8000)
  return(as.mcmc(jc1))
})

mc.cSMR_JAGS <- mcmc.list(cSMR_JAGS)

(end.time <- Sys.time()) # 
mc.cSMR_JAGS.ET <- difftime(end.time, start.time, units='mins')

save("mc.cSMR_JAGS",file="out/mc.elk_SmpPrd2_eff_cSMR_8KIt.RData")
save("mc.cSMR_JAGS.ET",file="out/mc.elk_SmpPrd2_effcSMR_8KIt.ET.RData")
stopCluster(cl3)


###############################################################
###--- View output (JAGS)

################################
###--- view output
mc.cSMR_JAGS.ET # 63 mins for 56 cams, M=400, 8000 IT

options(scipen = 999)

out <- mc.cSMR_JAGS
summary(out)
plot(out)
summary(window(out, start = 4001))
# summary(window(out.eff,start = 1001))
# summary(window(out.eff.unS,start = 1001))

plot(window(out,start = 4001))
# plot(window(out.eff,start = 11001))
# plot(window(out.eff,start = 11001))

rejectionRate(window(out ,start = 4001))

######################################
#create function to load files and write output to table

#function to create output table for JAGS output
get_JAGS_output <- function(filename){
  out <- filename
  s <- summary(window(out, start = 8001))
  gd <- gelman.diag(window(out, start = 4001),multivariate = FALSE)
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
###--- output tables

load("out/mc.elk_SmpPrd1_eff_cSMR_8KIt.RData")


out_elk_SmpPrd1_cSMR_8KIt_8KBIN <- get_JAGS_output(out)
write.csv(out_elk_SmpPrd1_cSMR_8KIt_8KBIN, "out/out_elk_SmpPrd1.cSMR_8KIt_8KBIN.csv")


out_elk_SmpPrd2_cSMR_8KIt_8KBIN <- get_JAGS_output(out)
write.csv(out_elk_SmpPrd2_cSMR_8KIt_8KBIN, "out/out_elk_SmpPrd2.cSMR_8KIt_8BIN.csv")


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
  s <- summary(window(out, start = 4001))
  gd <- gelman.diag(window(out, start = 4001),multivariate = FALSE)
  output_table <- rbind(as.data.frame(t(s$statistics)),
                        as.data.frame(t(s$quantiles)),
                        as.data.frame(t(gd$psrf)))
  return(output_table)
}

get_JAGS_output(out)

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

