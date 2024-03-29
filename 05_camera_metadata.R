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
# 05_camera_metadata.R
# script to load camera site and meta data
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 16-Sep-2021
#####################################################################################

.libPaths("C:/Program Files/R/R-4.0.5/library") # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# Load Packages
list.of.packages <- c("tidyverse", "lubridate","bcdata", "bcmaps","sf","nngeo","Cairo", "OpenStreetMap", "ggmap", "units","raster")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################

# keep in mind that WGS84 lat/long espg = 4326; BC Albers espg = 3005; NAD83 / UTM zone 10N espg = 26910

###--- function to retrieve geodata from BCGW

retrieve_geodata_aoi <- function (ID=ID){
  aoi.geodata <- bcdc_query_geodata(ID) %>%
    filter(BBOX(st_bbox(aoi))) %>%
    collect()
  aoi.geodata <- aoi.geodata %>% st_intersection(aoi)
  aoi.geodata$Area_km2 <- st_area(aoi.geodata)*1e-6
  aoi.geodata <- drop_units(aoi.geodata)
  return(aoi.geodata)
}

#####################################################################################
#- EPU polygon shapefile
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_Sechelt_Peninsula")


# load camera locations
cam_metadata <- read.csv("data/SP_deployment_station_data.csv")
cam_metadata <- st_as_sf(cam_metadata,coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(3005)

sp_dtn_sites <- read.csv("data/Cam_focalsp_detections.csv")
sp_dtn_sites[is.na(sp_dtn_sites)] <- 0
colnames(sp_dtn_sites)[1] <- "Deployment.Location.ID"

random_lcn <- st_read(paste0(getwd(),"/data/elk_all_sites.kml")) %>% st_transform(3005)

ggplot()+
  geom_sf(data = cam_metadata)+
  geom_sf(data = random_lcn, col="blue")+
  theme_minimal()

# create initial aoi
aoi <- st_as_sfc(st_bbox(random_lcn))
aoi <- st_buffer(aoi, dist=2000)
aoi <- aoi %>% st_transform(3005)

# check loaded properly
ggplot()+
  geom_sf(data = aoi)+
  geom_sf(data= EPU_poly, col="red")+
  geom_sf(data = cam_metadata)+
  geom_sf(data = random_lcn, col="blue")+
  theme_minimal()


###--- Create Species Detection Maps for Focal Species
# Bear, Deer, Elk
cam_sp_data <- left_join(cam_metadata, sp_dtn_sites)
cam_sp_data <- cam_sp_data %>% filter(!is.na(Bear))
nrow(cam_sp_data)
nrow(cam_metadata)
# check working properly

# bear
Dtn_bear <- ggplot()+
  geom_sf(data= EPU_poly, col="black")+
  geom_sf(data = cam_sp_data[cam_sp_data$Bear!=0,], aes(size = Bear), col="darkblue")+
  geom_sf(data = cam_sp_data[cam_sp_data$Bear==0,], col="cadetblue")+
  # geom_sf(data = cam_metadata, col="blue")+
  theme_minimal()

Cairo(file="out/Dtn_bear.PNG",
      type="png",
      width=2500,
      height=3000,
      pointsize=15,
      bg="white",
      dpi=300)
Dtn_bear
dev.off()

# elk
Dtn_elk <- ggplot()+
  geom_sf(data= EPU_poly, col="black")+
  geom_sf(data = cam_sp_data[cam_sp_data$Elk!=0,], aes(size = Elk), col="darkblue")+
  geom_sf(data = cam_sp_data[cam_sp_data$Elk==0,], col="cadetblue")+
  # geom_sf(data = cam_metadata, col="blue")+
  theme_minimal()

Cairo(file="out/Dtn_elk.PNG",
      type="png",
      width=2500,
      height=3000,
      pointsize=15,
      bg="white",
      dpi=300)
Dtn_elk
dev.off()

# deer
Dtn_deer <- ggplot()+
  geom_sf(data= EPU_poly, col="black")+
  geom_sf(data = cam_sp_data[cam_sp_data$Deer!=0,], aes(size = Deer), col="darkblue")+
  geom_sf(data = cam_sp_data[cam_sp_data$Deer==0,], col="cadetblue")+
  # geom_sf(data = cam_metadata, col="blue")+
  theme_minimal()

Cairo(file="out/Dtn_deer.PNG",
      type="png",
      width=2500,
      height=3000,
      pointsize=15,
      bg="white",
      dpi=300)
Dtn_deer
dev.off()


  ###---
  # want to get average distance / area for centre of each land based cell (i.e., what is on the landscape)
  # and then compare that to what the camera location site average distance / covariate type is
  # to determine if sites achieve stratified sampling or are biased one way or another (i.e., close to roads)

  # do this for
  # biogeoclimatic zone (proportion habitat across landscape vs proportion of sites with that type of habitat)
  # site series (proportion habitat across landscape vs proportion of sites with that type of habitat)
  # roads (distance to primary vs trails)
  # waterbodies (distance)
  ###---

###--- IMPORT SPATIAL (COVARIATE) FILES ---###
###--- load covariates from bcmaps
# digital elevation raster
aoi_raster <- cded_raster(aoi)
aoi.cded <- rasterToPoints(aoi_raster) # convert to points for join
aoi.cded <- st_as_sf(as.data.frame(aoi.cded), coords = c("x","y"), crs = 3005) # create spatial layer

# broad biogeoclimatic data
aoi.BEC <- bec() %>% st_intersection(aoi)
aoi.ECOprov <- ecoprovinces() %>% st_intersection(aoi)
aoi.ECOreg <- ecoregions() %>% st_intersection(aoi)
aoi.ECOsec <- ecosections() %>% st_intersection(aoi)

ggplot()+
  # geom_sf(data=aoi.BEC, aes(fill=MAP_LABEL, col=MAP_LABEL))+
  # geom_sf(data=aoi.ECOprov, aes(fill=ECOPROVINCE_NAME, col=ECOPROVINCE_NAME))+
  # geom_sf(data=aoi.ECOreg, aes(fill=ECOREGION_NAME, col=ECOREGION_NAME))+
  geom_sf(data=aoi.ECOsec, aes(fill=ECOSECTION_NAME, col=ECOSECTION_NAME))+
  geom_sf(data=cam_metadata)

###--- load covariates from bcdata
# using the bc data warehouse option to clip to aoi

# TEM
# WHSE_TERRESTRIAL_ECOLOGY.STE_TEM_ATTRIBUTE_POLYS_SVW
# bcdc_search("TEM", res_format = "wms")
aoi.TEM <- retrieve_geodata_aoi(ID = "0a83163b-a62f-4ce6-a9a1-21c228b0c0a3")

# review report for codes
# https://a100.gov.bc.ca/pub/acat/documents/r35895/tem_4678_rpt_1363697818384_6de51d6c988801e3369406fe9a22713aa4dca27f63de77d54bf44b70a246813b.pdf
# recall that site series labels are duplicated between biogeoclimatic labels so need to go by map cde for plotting

# glimpse(aoi.TEM)
#
# aoi.TEM %>% group_by(BIOGEOCLIMATIC_LBL) %>% summarise(sum(Area_km2)) %>% st_drop_geometry()
# as.data.frame(aoi.TEM %>% group_by(BIOGEOCLIMATIC_LBL) %>% count(SITE_SERIES_LBL_CPNT_1) %>% st_drop_geometry())
# as.data.frame(aoi.TEM %>% group_by(BIOGEOCLIMATIC_LBL,SITE_SERIES_LBL_CPNT_1) %>% summarise(sum(Area_km2)) %>% st_drop_geometry())
#
# aoi.TEM %>% count(SITE_SERIES_MAP_CDE_LBL_CPNT_1) %>% st_drop_geometry()
# aoi.TEM %>% count(ECOSYSTEM_LBL_CPNT_1) %>% st_drop_geometry() # would need to group to make useful
# as.data.frame(aoi.TEM %>% group_by(BIOGEOCLIMATIC_LBL) %>% count(ECOSYSTEM_DECILE_CPNT_1) %>% st_drop_geometry())
glimpse(aoi.TEM)

ggplot()+
  geom_sf(data=aoi.TEM, aes(fill=SITE_SERIES_MAP_CDE_LBL_CPNT_1, col=SITE_SERIES_MAP_CDE_LBL_CPNT_1))+
  geom_sf(data=cam_metadata)


# watercourses layer
# bcdc_search("NTS BC River", res_format = "wms")
aoi.RLW <- retrieve_geodata_aoi(ID = "414be2d6-f4d9-4f32-b960-caa074c6d36b")

ggplot()+
  geom_sf(data=aoi.RLW, aes(fill=DESCRIPTION, col=DESCRIPTION))+
  geom_sf(data=cam_metadata)

# FWA_WATERSHED_GROUPS Freshwater Atlas Watershed Boundaries
# bcdc_search("freshwater", res_format = "wms")
aoi.FWA <- retrieve_geodata_aoi(ID = "ab758580-809d-4e11-bb2c-df02ac5465c9")

ggplot()+
  geom_sf(data=aoi.FWA, aes(fill=WATERSHED_GROUP_CODE, col=WATERSHED_GROUP_CODE))+
  geom_sf(data=cam_metadata)

# transportation layer (Digital Road Atlas)
# bcdc_search("road", res_format = "wms")
aoi.DRA <- retrieve_geodata_aoi(ID = "bb060417-b6e6-4548-b837-f9060d94743e")

ggplot()+
  geom_sf(data=aoi.DRA, aes(fill=ROAD_SURFACE, col=ROAD_SURFACE))+
  geom_sf(data=cam_metadata)
# glimpse(aoi.DRA)
# aoi.DRA %>% count(ROAD_SURFACE)

# approved WHAs & UWRs (GAR Orders)
# bcdc_search("WHA", res_format = "wms")
# aoi.WHA <- retrieve_geodata_aoi(ID = "b19ff409-ef71-4476-924e-b3bcf26a0127")
# just one goshawk WHA in the north-west (not worth noting)
# ggplot()+
#   geom_sf(data=aoi.WHA)+
#   geom_sf(data=cam_metadata)

# bcdc_search("UWR", res_format = "wms")
# aoi.UWR <- retrieve_geodata_aoi(ID = "712bd887-7763-4ed3-be46-cdaca5640cc1")
# no approved UWRs on the SP, plot proposed instead (from kml)
# ggplot()+
#   geom_sf(data=aoi.UWR)+
#   geom_sf(data=cam_metadata)

# vegetation data (VRI)
# bcdc_search("VRI", res_format = "wms")
aoi.VRI <- retrieve_geodata_aoi(ID = "2ebb35d8-c82f-4a17-9c96-612ac3532d55")
aoi.VRI %>% summarise(min(PROJ_HEIGHT_1, na.rm=T), max(PROJ_HEIGHT_1, na.rm=T), mean(PROJ_HEIGHT_1, na.rm=T))

aoi.VRI %>% filter(!is.na(PROJ_HEIGHT_1)) %>%
  summarise(mean = mean(PROJ_HEIGHT_1), min = min(PROJ_HEIGHT_1), max=max(PROJ_HEIGHT_1), sd = sd(PROJ_HEIGHT_1))
aoi.VRI$PROJ_HEIGHT_1_cat <- as.factor(ifelse(aoi.VRI$PROJ_HEIGHT_1 < 10, "H0-10",
                                              ifelse(aoi.VRI$PROJ_HEIGHT_1 < 20, "H10-20",
                                                     ifelse(aoi.VRI$PROJ_HEIGHT_1 < 30, "H20-30",
                                                            ifelse(aoi.VRI$PROJ_HEIGHT_1 < 40, "H30-40",
                                                                   ifelse(aoi.VRI$PROJ_HEIGHT_1 < 50, "H40-50", "H50+"))))))# remove NAs

ggplot()+
  geom_sf(data=aoi.VRI, aes(fill=PROJ_HEIGHT_1_cat, col=PROJ_HEIGHT_1_cat))+
  geom_sf(data=cam_metadata) +
  geom_sf(data=random_lcn, col="blue")

###--- JOIN COVARIATE TO POINT DATA ---###
# run first for actual site locations and then for random locations

###--- function to join covariate type / distance data

retrieve_covariate_dist <- function (point.dat=point.dat, cov.dat=cov.dat, cov.dat.col=cov.dat.col){

  tmp <- cov.dat[,cov.dat.col] %>% st_drop_geometry()
  colnames(tmp)[1] <- "cov.type"

  cov.dist <- st_nn(point.dat %>% st_transform(crs=26910),
                    cov.dat %>% st_transform(crs=26910),
                    k=1, returnDist = T)
  point.dat$cov.dist <- unlist(cov.dist$dist)
  point.dat$cov.type <- unlist(cov.dist$nn)
  point.dat$cov.type <- tmp$cov.type[match(point.dat$cov.type,rownames(tmp))]

  return(point.dat %>% dplyr::select(cov.dist, cov.type) %>% st_drop_geometry())
}


# points <- random_lcn

# how far are camera points from each other
cam.dist <- st_nn(cam_metadata %>% st_transform(crs=26910),
                  cam_metadata %>% st_transform(crs=26910),
                  k=2, returnDist = T)

cam.dist <- unlist(cam.dist$dist)
cam.dist <- cam.dist[cam.dist >0]
min(cam.dist) # 1372.374
mean(cam.dist) # 1913.309
max(cam.dist) # 4002.707
sd(cam.dist)/ sqrt(length(cam.dist)) # se = 67.5

#- elevation
# for some reason it's not working?!?!?
# elev.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.cded, cov.dat.col=1)
# elev.dist.rndm <- retrieve_covariate_dist(point.dat=random_lcn, cov.dat=aoi.cded, cov.dat.col=1)

#- biogeoclimatic (might want proportion of cam points compared to proportion of area on SP)
BEC.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.BEC, cov.dat.col=8)
BEC.cam.tbl <- BEC.dist.cam %>% count(cov.type)

BEC.prop.EPU <- aoi.BEC %>% st_intersection(EPU_poly) %>% st_transform(crs=26910) # for m2
BEC.prop.EPU$Area_km2 <- st_area(BEC.prop.EPU)*1e-6
BEC.prop.EPU <- drop_units(BEC.prop.EPU)
BEC.prop.tbl <- BEC.prop.EPU %>% group_by(MAP_LABEL) %>% summarise(area_km2  = sum(Area_km2)) %>% st_drop_geometry()
BEC.prop.tbl$prop <- BEC.prop.tbl$area_km2 / (st_area(EPU_poly)*1e-6) %>% drop_units()
BEC.prop.tbl$num.site <- BEC.cam.tbl$n[match(BEC.prop.tbl$MAP_LABEL, BEC.cam.tbl$cov.type)]
BEC.prop.tbl$prop.site <- BEC.prop.tbl$num.site / 57
write.csv(BEC.prop.tbl, "./out/BEC.prop.tbl.csv", row.names = F)


ECOprov.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.ECOprov, cov.dat.col=4)
ECO.cam.tbl <- ECOprov.dist.cam %>% count(cov.type)

ECO.prop.EPU <- aoi.ECOprov %>% st_intersection(EPU_poly) %>% st_transform(crs=26910) # for m2
ECO.prop.EPU$Area_km2 <- st_area(ECO.prop.EPU)*1e-6
ECO.prop.EPU <- drop_units(ECO.prop.EPU)
ECO.prop.tbl <- ECO.prop.EPU %>% group_by(ECOPROVINCE_NAME) %>% summarise(area_km2  = sum(Area_km2)) %>% st_drop_geometry()
ECO.prop.tbl$prop <- ECO.prop.tbl$area_km2 / (st_area(EPU_poly)*1e-6) %>% drop_units()
ECO.prop.tbl$num.site <- ECO.cam.tbl$n[match(ECO.prop.tbl$ECOPROVINCE_NAME, ECO.cam.tbl$cov.type)]
ECO.prop.tbl$prop.site <- ECO.prop.tbl$num.site / 57
write.csv(ECO.prop.tbl, "./out/ECO.prop.tbl.csv", row.names = F)

# the proportion is the same for ECOprov, ECOreg and ECOsec.
# ECO (province) Coast and Mountains = (Region) Georgia-Puget Basin = (Section) Southern Pacific Ranges
# compared to Georgia Depression = Lower Mainland = Georgia Lowland
ECOreg.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.ECOreg, cov.dat.col=4)
ECOsec.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.ECOsec, cov.dat.col=4)

#- TEM
colnames(aoi.TEM)[14]
TEM.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.TEM, cov.dat.col=14)

#- roads
names(aoi.DRA)
DRA.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.DRA, cov.dat.col=18)
DRA.dist.random <- retrieve_covariate_dist(point.dat=random_lcn, cov.dat=aoi.DRA, cov.dat.col=18)

#- waterbodies
names(aoi.RLW)
RLW.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.RLW, cov.dat.col=6)
RLW.dist.random <- retrieve_covariate_dist(point.dat=random_lcn, cov.dat=aoi.RLW, cov.dat.col=6)

#- watershed
names(aoi.FWA)
FWA.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.FWA, cov.dat.col=13)
FWA.dist.random <- retrieve_covariate_dist(point.dat=random_lcn, cov.dat=aoi.FWA, cov.dat.col=13)

dist.cam <- cbind(DRA.dist.cam, RLW.dist.cam, FWA.dist.cam)
colnames(dist.cam) <- c("road.dist.cam", "road.surface.cam", "RLW.dist.cam", "RLW.type.cam", "wtrshd.dist.cam", "wtrshd.type.cam")
dist.cam %>% group_by(road.surface.cam) %>% summarise(mean = mean(road.dist.cam))
write.csv(dist.cam, "./out/dist.cam.csv")

dist.random <- cbind(DRA.dist.random, RLW.dist.random, FWA.dist.random)
colnames(dist.random) <- c("road.dist.rndm", "road.surface.rndm", "RLW.dist.rndm", "RLW.type.rndm", "wtrshd.dist.rndm", "wtrshd.type.rndm")
write.csv(dist.random, "./out/dist.rndm.csv")


#- VRI
names(aoi.VRI)
VRI.dist.cam <- retrieve_covariate_dist(point.dat=cam_metadata, cov.dat=aoi.VRI, cov.dat.col=136)
summary(VRI.dist.cam$cov.type, na.rm=TRUE)
VRI.dist.cam %>% filter(!is.na(VRI.dist.cam)) %>%
  summarise(mean = mean(cov), min = min(PROJ_HEIGHT_1), max=max(PROJ_HEIGHT_1), sd = sd(PROJ_HEIGHT_1))
VRI.dist.cam$PROJ_HEIGHT_1_cat <- as.factor(ifelse(VRI.dist.cam$cov.type < 10, "H0-10",
                                              ifelse(VRI.dist.cam$cov.type < 20, "H10-20",
                                                     ifelse(VRI.dist.cam$cov.type < 30, "H20-30",
                                                            ifelse(VRI.dist.cam$cov.type < 40, "H30-40",
                                                                   ifelse(VRI.dist.cam$cov.type < 50, "H40-50", "H50+"))))))# remove NAs
VRI.cam.tbl <- VRI.dist.cam %>% count(PROJ_HEIGHT_1_cat)

VRI.dist.random <- retrieve_covariate_dist(point.dat=random_lcn, cov.dat=aoi.VRI, cov.dat.col=136)
VRI.prop.EPU <- aoi.VRI %>% st_intersection(EPU_poly) %>% st_transform(crs=26910) # for m2
VRI.prop.EPU$Area_km2 <- st_area(VRI.prop.EPU)*1e-6
VRI.prop.EPU <- drop_units(VRI.prop.EPU)
VRI.prop.tbl <- VRI.prop.EPU %>% group_by(PROJ_HEIGHT_1_cat) %>% summarise(area_km2  = sum(Area_km2)) %>% st_drop_geometry()
VRI.prop.tbl$prop <- VRI.prop.tbl$area_km2 / (st_area(EPU_poly)*1e-6) %>% drop_units()
VRI.prop.tbl$num.site <- VRI.cam.tbl$n[match(VRI.prop.tbl$PROJ_HEIGHT_1_cat, VRI.cam.tbl$PROJ_HEIGHT_1_cat)]
VRI.prop.tbl$prop.site <- VRI.prop.tbl$num.site / 57
write.csv(VRI.prop.tbl, "./out/VRI.prop.tbl.csv", row.names = F)

################################################################################
save.image("data/05_camera_metadata.RData")
# load("data/05_camera_metadata.RData")
################################################################################

