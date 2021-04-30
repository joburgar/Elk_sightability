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
# 01_load.R
# script to load elk collar and EPU data
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 17-Feb-2021
#####################################################################################

.libPaths("C:/Program Files/R/R-4.0.5/library") # to ensure reading/writing libraries from C drive
tz = Sys.timezone() # specify timezone in BC

# overall process:
#- Upload collar data
#- Upload Animal metadata and collar metadata (if applicable)
#- Upload EPU metadata (from SBOT and inventory files)

# Load Packages
list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo",
                      "OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "scrbook")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
#####################################################################################
###--- Import spatial files
# Set study region as BC / SC
bc <- bc_bound()
bc_latlon <- st_transform(bc, crs=4326)
st_bbox(bc_latlon)

SC <- nr_districts() %>% filter(ORG_UNIT %in% c("DCK", "DSQ", "DSC"))
SC_latlon <- st_transform(SC, crs=4326)
st_bbox(SC_latlon)

# Import BEC, Ecoprovinces, Ecoregions and Ecosections
BEC.SC <- bec() %>% st_intersection(SC)
ECOprov.SC <- ecoprovinces() %>% st_intersection(SC)
ECOreg.SC <- ecoregions() %>% st_intersection(SC)
ECOsec.SC <- ecosections() %>% st_intersection(SC)

ggplot() +
  geom_sf(data = BEC.SC, aes(fill=MAP_LABEL)) +
  geom_sf(data = SC,  color = "black", fill = NA, lwd=1.5) +
  coord_sf(datum = NA) +
  theme_minimal()

ggplot() +
  geom_sf(data = ECOsec.SC, aes(fill=ECOSECTION_CODE)) +
  geom_sf(data = SC,  color = "black", fill = NA, lwd=1.5) +
  coord_sf(datum = NA) +
  theme_minimal()

cities.SC <- bc_cities() %>% st_intersection(SC)


# Import road layer
# Using the bc data warehouse option to clip to SC aoi
# bcdc_search("road", res_format = "wms")
#
# bcdc_tidy_resources("bb060417-b6e6-4548-b837-f9060d94743e") %>%
#   filter(bcdata_available == "TRUE") %>% select(name, id)
#
# as.data.frame(bcdc_describe_feature("bb060417-b6e6-4548-b837-f9060d94743e"))
# bcdc_query_geodata("0a83163b-a62f-4ce6-a9a1-21c228b0c0a3") %>% filter(BUSINESS_AREA_PROJECT_ID==4678)
#


# # Import elevation data for priority area

# digital elevation raster
SC_raster <- cded_raster(SC)
plot(SC_raster)

#- South Coast roads data from BC Data Warehouse
# transportation layer (Digital Road Atlas)
# bcdc_search("road", res_format = "wms")
# Roads_line <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
#   filter(INTERSECTS(SC)) %>%
#   collect()
# rather large, use only for priority EPUs or stick with old way
BCWData_Dir <- "C:/Users/JBURGAR/R/Analysis/BC_Warehouse_Data/Roads/DRA_DGTL_ROAD_ATLAS_MPAR_SP"
Roads_line <- st_read(dsn=BCWData_Dir, layer = "DRA_MPAR_line")

#- EPU polygon shapefile
GISDir <- "//spatialfiles.bcgov/work/wlap/sry/Workarea/jburgar/Elk"
EPU_poly <- st_read(dsn=GISDir, layer="EPU_NA")


#####################################################################################
###--- Import collar telemetry data
# Read collar position data csv, selecting most recent collar data download
# assumes all collar data downloaded each time, will need to modify if only downloading most recent data
collar_pos_path <-"C:/Users/JBURGAR/R/Analysis/Collar_data/"
tmpshot <- fileSnapshot(collar_pos_path)
recent_file <- rownames(tmpshot$info[which.max(tmpshot$info$mtime),])

# Import collar data with valid fixes, with locations inside BC, selecting only pertinent columns, transforming date field into R formats
collar_pos <- read.csv(paste(collar_pos_path, recent_file, sep=""), header=TRUE, sep=";") %>%
  type.convert() %>%
  filter(Fix.Type=="3D Validated") %>%
  filter(Mortality.Status!="Mortality No Radius")%>%
  filter(between(Longitude..deg., st_bbox(bc_latlon)$xmin, st_bbox(bc_latlon)$xmax)) %>%
  filter(between(Latitude..deg., st_bbox(bc_latlon)$ymin, st_bbox(bc_latlon)$ymax)) %>%
  select(Collar.ID, UTC.Date, Latitude..deg., Longitude..deg., Mortality.Status) %>%
  mutate(Date.Time.UTC = ymd_hms(UTC.Date, truncated = 1, tz="UTC"))


###--- Import collar metadata
# check if these are the latest files and they encapsulate the correct range of celss
cptr_telem <- read_excel("data/Capture and Telemetry_DATABASE_July_28_2020.xls",
                  sheet = 2, range = "A11:BZ202", trim_ws = TRUE, col_types = c("text")) %>% type.convert()
collar_inv <- read_excel("data/Collar Inventory_DATABASE_January_07_2021.xls",
                 sheet= 1, range = "A8:Q158", trim_ws = TRUE, col_types = c("text")) %>% type.convert()

glimpse(cptr_telem)
glimpse(collar_inv)

#####################################################################################
###--- Import original AnimalID values for collars
origID <- read.csv("data/AnimalID_orig.csv", header=T, colClasses=c("character"),
                     stringsAsFactors = TRUE,  na.string=c("","NA")) %>% type_convert()

#####################################################################################
###--- Import EPU metadata (from SBOT and inventory files)
# Read deployment data csv for station covariates
EPU_SBOT <- read.csv("data/Elk_SBOT_data.csv", header=T, colClasses=c("character"),
                     stringsAsFactors = TRUE,  na.string=c("","NA")) %>% type_convert()
EPU_inv <- read.csv("data/EPU_Priority.csv", header=T, colClasses=c("character"),
                    stringsAsFactors = TRUE,  na.string=c("","NA")) %>% type_convert()

glimpse(EPU_SBOT)
glimpse(EPU_inv)

#####################################################################################

###--- MOVE ON TO 02_clean to clean/format data
