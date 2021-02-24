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
# 02_clean.R
# script to collate elk (meta)data for sightability methods, covariates
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 22-Feb-2021
#####################################################################################

#- clean collar inventory and capture / telemetry metadata, then combine with telemetry download data
# collar inventory
glimpse(collar_inv)
elk_inv <- collar_inv %>% filter(grepl("Elk", Species) & grepl("Vectronic",Company))
elk_inv %>% count(`Program (see info files for details)`) # all animals 1 fix/day; rolling +1 hr/ day

# need to format dates for R, acting odd coming from excel
# for excel dates (Windows) treat numeric origin as "1899-12-30" in as.Date for conversion
elk_inv <- droplevels(elk_inv)
elk_inv$DDate <- as.character(elk_inv$`Deployed Date`)
elk_inv$RDate <- as.character(elk_inv$`Recovered Date`)
elk_inv$Date.Deployed <- as.Date(as.numeric(elk_inv$DDate), origin = "1899-12-30")
elk_inv$Date.Recovered <- as.Date(as.numeric(elk_inv$RDate), origin = "1899-12-30")

elk_inv <- elk_inv %>% select(`Serial number`, Status, Date.Deployed, `General location...11`,
                              Date.Recovered, `General location...15`) %>%
  rename(Collar.ID = `Serial number`, Loc.Deployed = `General location...11`, Loc.Recovered = `General location...15`)
elk_inv$Status

# catpure / telemetry metadata
glimpse(cptr_telem)
elk_cptr_telem <- cptr_telem %>% filter(Species=="Roosevelt elk" & Make=="Vectronics") %>%
  select(`WLHID#`, Date, `Serial No.`, `Initial or Recapture`, `Population Unit`,
         `Easting (capture)`, `Northing (capture)`, `Release                       (on site or relocated)`,
         `Release Date`, `Population Unit (release)`, `Easting (release)`, `Northing (release)`, `Age Class`, Sex)

elk_cptr_telem <- droplevels(elk_cptr_telem)
colnames(elk_cptr_telem) <- c("WLHID", "Date.Capture", "Collar.ID", "Initial.Recapture", "Pop.Unit.Capture",
                              "Easting.Capture", "Northing.Capture",
                              "Release.Loc", "Date.Release", "Pop.Unit.Release",
                              "Easting.Release", "Northing.Release", "Age.Class", "Sex")

elk_cptr_telem$Date.Captured <- as.Date(as.numeric(elk_cptr_telem$Date.Capture), origin = "1899-12-30")
elk_cptr_telem$Date.Released <- as.Date(as.numeric(elk_cptr_telem$Date.Release), origin = "1899-12-30")
table(elk_cptr_telem$Pop.Unit.Capture, elk_cptr_telem$Pop.Unit.Release)

elk_cptr_telem$Pop.Unit.Capture <- elk_cptr_telem$Pop.Unit.Capture %>%
  recode("Homathco"="Homathko", "Pitt River*"="Pitt River", "Vancouer River"="Vancouver River", "Haslam"="Haslam-Dodd",
         "Theodosia"="Theo", "Tzoonie-Narrows"="Narrows", "Deserted-Stakawus"="Deserted", "Skwawaka"="Skwawka")
elk_cptr_telem$Pop.Unit.Release <- elk_cptr_telem$Pop.Unit.Release %>%
  recode("Homathco"="Homathko", "Chahalis"="Chehalis","Haslam"="Haslam-Dodd",
         "Theodosia"="Theo", "Tzoonie-Narrows"="Narrows", "Deserted-Stakawus"="Deserted", "Skwawaka"="Skwawka")

elk_cptr_telem$Release.Loc <- elk_cptr_telem$Release.Loc %>%
  recode("On site" = "On.Site", "On Site" = "On.Site", "On SIte" = "On.Site",
         "Translocated"="Off.Site", "Relocated" = "Off.Site")

elk_cptr_telem %>% filter(Release.Loc=="Off.Site") %>% select(Pop.Unit.Capture, Pop.Unit.Release)
elk_cptr_telem <- elk_cptr_telem %>% select(-c(Date.Capture, Date.Release))

as.data.frame(elk_cptr_telem %>% group_by(Collar.ID) %>% count(Date.Released))

unique(elk_cptr_telem$Collar.ID) #62 collars but 65 entries = Collar.ID 15114, 15120, 22587 were used twice
unique(elk_inv$Collar.ID) #63 collars and 63 rows
as.data.frame(elk_inv %>% group_by(Collar.ID) %>% count(Date.Deployed))

elk_cptr_telem %>% filter(Collar.ID %in% c("15114", "15120", "22587")) %>%
  select(Collar.ID, Date.Captured, Pop.Unit.Capture, Date.Released, Pop.Unit.Release, Sex, Age.Class)
elk_inv %>% filter(Collar.ID%in% c("15114", "15120", "22587"))

# Collar.ID 15114 possibly used twice, first translocated to Chehalis in 2015 then to Philips Arm in 2017
# Collar.ID 15120 possibly used twice, first in Sechelt in 2015 then translocated to Philips Arm in 2017
# Collar.ID 22587 used twice, first in Stave in 2017 then in Skwawka in 2018
# 22587     2017-04-11    Stave             2017-04-11    Stave             F     A
# 22587     2018-03-17    Skwawka           2018-03-17    Skwawka           F     A
# March 17 is Julian Day 76

###--- join the two collar metadata files
collar_meta <- left_join(elk_cptr_telem, elk_inv)

# group status values into 3
collar_meta <- collar_meta %>% mutate(Status = case_when(grepl("On hand", Status, ignore.case = TRUE) ~ "Office",
                           grepl("last", Status, ignore.case = TRUE) ~ "Field.NotOperational",
                           TRUE ~ "Field.Operational"))

collar_meta$Status <- as.factor(collar_meta$Status)
as.data.frame(collar_meta %>% filter(Status=="Field.Operational") %>% count(Pop.Unit.Release))

collar_meta %>% filter(is.na(WLHID)) %>% summarise(min(Date.Deployed), max(Date.Deployed))
collar_meta %>% filter(!is.na(WLHID)) %>% summarise(min(Date.Deployed), max(Date.Deployed))
# WLHID associated with all collars from 2017-03-20 onwards; last time collar was deployed without WLHID was 2017-01-24

collar_whlid <- as.data.frame(collar_meta %>% group_by(WLHID) %>% count(Collar.ID))
collar_whlid %>% filter(!is.na(WLHID)) %>% filter(n>1) # if no rows turn up then each WLHID only associated with 1 collar
# can use WHLID to associate original Animal ID and then create new Animal IDs moving forward

#- add animalID to collar meta data
colnames(origID)[2] <- "Collar.ID"
# tmp <- full_join(origID, collar_meta %>% select(Collar.ID, WLHID), by="Collar.ID")
# tmp %>% filter(duplicated(tmp$Collar.ID))
# need to use case_when to deal with collar 22587 being used twice, otherwise should be ok for newer Vectronics collars

# so for Animal.ID will need to separate Elk13 and Elk14 in collar_meta based on WHLID (Elk13 is WLHID 16-8261, Elk14 is 18-11025)
collar_meta <- left_join(collar_meta, origID %>% select(-WLHID), by="Collar.ID")

collar_meta <- collar_meta %>% mutate(Animal.ID = case_when(grepl("15114_Unk_2015-01-05", UniqueID, ignore.case = TRUE) ~ "Elk003",
                                                            grepl("15114_Unk_2017-01-10", UniqueID, ignore.case = TRUE) ~ "Elk004",
                                                            grepl("15120_Unk_2014-12-03", UniqueID, ignore.case = TRUE) ~ "Elk009",
                                                            grepl("15120_Unk_2017-01-10", UniqueID, ignore.case = TRUE) ~ "Elk010",
                                                            grepl("22587_16-8261_2017-04-11", UniqueID, ignore.case = TRUE) ~ "Elk013",
                                                            grepl("22587_18-11025_2018-03-17", UniqueID, ignore.case = TRUE) ~ "Elk014",
                                                            TRUE ~ paste("Elk0",substr(collar_meta$AnimalID,4,5), sep="")))

# all animals without Animal.ID will be adding sequentially
# now that over 100 animals should pad with 0 to make 3 digit numeric
ElkNA <- collar_meta %>% filter(Animal.ID=="Elk0NA") %>% count(Animal.ID)
length_ElknonNA <- length(unique(origID$AnimalID))
suffix <- seq(from = length_ElknonNA+1, to=length_ElknonNA+ElkNA$n)

collar_meta$Animal.ID <- as.factor(ifelse(is.na(collar_meta$UniqueID), paste("Elk",str_pad(suffix, 3, pad = "0"), sep=""), collar_meta$Animal.ID))
as.data.frame(collar_meta %>% select(Collar.ID, Animal.ID))

#####################################################################################
#- downloaded telemetry data
glimpse(collar_pos)
collar_pos$Collar.ID <- as.factor(collar_pos$Collar.ID)
colnames(collar_pos)[3:4] <- c("Latitude", "Longitude")

collar_pos$Date.Time.PST <- with_tz(collar_pos$Date.Time.UTC, tz)
collar_pos$Time.PST <- times(strftime(collar_pos$Date.Time.PST,"%H:%M:%S", tz))

collar_pos <- collar_pos %>% mutate(Year = year(Date.Time.PST), Month = month(Date.Time.PST, label = T), jDay = yday(Date.Time.PST))
collar_pos %>% filter(Collar.ID %in% c("15114", "15120", "22587")) %>% group_by(Collar.ID, Year) %>% summarise(min(Date.Time.PST), max(Date.Time.PST))
dup.collars <- c("15114", "15120", "22587")

common_collars <- intersect(collar_meta$Collar.ID, collar_pos$Collar.ID)
length(common_collars) # 54 common Collar.IDs

collar_pos %>% filter(!Collar.ID %in% common_collars) %>% count(Collar.ID) # currently 11 collars out not yet included in meta data files

#- add in metadata to collar_pos file, while only selecting pertinent columns
collar_dat <- left_join(collar_pos %>% select(Collar.ID, Latitude, Longitude, Mortality.Status, Date.Time.PST, Time.PST, Year, Month, jDay),
                        collar_meta %>% select(Collar.ID, WLHID, Animal.ID, Age.Class, Sex, Pop.Unit.Release, Date.Released))

# need to update collar 22587 with Elk013 info for all datapoints prior to 2018
# first change Animal.ID to Elk013
collar_dat$Animal.ID <- as.factor(ifelse(collar_dat$Collar.ID=="22587" & collar_dat$Date.Time.PST < "2017-12-31 00:00:00", "Elk013",
                                         ifelse(collar_dat$Collar.ID=="22587" & collar_dat$Date.Time.PST > "2017-12-31 00:00:00", "Elk014",
                                                as.character(collar_dat$Animal.ID))))
collar_dat <- collar_dat %>%
  mutate(Pop.Unit.Release = case_when(Animal.ID %in% c('Elk013') ~ 'Stave',
                          TRUE ~ as.character(Pop.Unit.Release)),
        Date.Released = case_when(Animal.ID %in% c('Elk013') ~ as.Date('2017-04-11'),
                                   TRUE ~ as.Date(Date.Released)))

collar_dat %>% count(Animal.ID) # looks good, all collars with associated metadata are correctly loaded

glimpse(collar_dat)

#- remove all fixes prior to release dates
summary(as.Date(collar_dat$Date.Time.PST))
summary(as.Date(collar_dat$Date.Released))
collar_dat$use.fix <- if_else(as.Date(collar_dat$Date.Time.PST) - as.Date(collar_dat$Date.Released)>0, "yes", "no")
collar_dat %>% filter(use.fix!="yes") %>% count(Animal.ID)
# seems like Elk018 might have just had the collar on in the office / out flying, but will need to check
# all other collars just 1 day off or not in collar_meta (yet)

#####################################################################################
#- plot and visual cleaned telemetry data
telem_dat <- collar_dat %>% filter(use.fix=="yes")
nrow(collar_dat) - nrow(telem_dat) # removed 537 erroneous data points

telem_dat$Count <- 1 # add count for each fix


# check collar data loaded properly
collar.dates <- telem_dat %>% group_by(Collar.ID) %>% summarise(min.Date = min(Date.Time.PST), max.Date = max(Date.Time.PST))
min(collar.dates$min.Date); max(collar.dates$min.Date)
# [1] "2017-01-25 06:00:37 PST"
# [1] "2020-06-26 08:02:38 PDT"

min(collar.dates$max.Date); max(collar.dates$max.Date)
# [1] "2018-11-07 06:13:14 PST"
# [1] "2021-02-19 04:02:39 PST"

# will still need to clean collar data to make sure not included dates when collaring individuals
# speak to bios about # days post collaring to start including animals in analysis
collar_annual_fixes <- telem_dat %>% group_by(Collar.ID, Year) %>% summarise(Counts = sum(Count))
ggplot(collar_annual_fixes, aes(fill=Collar.ID, y=Counts, x=Collar.ID))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Year)


annual.fixes <- ggplot(data = collar_annual_fixes, aes(x = reorder(Collar.ID, -Counts), y = Counts, fill= Collar.ID)) +
  geom_bar(stat = "identity") +
  theme_classic() + ylab("Number of Annual Fixes per Collar") +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(size = 14)) + theme(axis.title.x = element_blank())+
  facet_wrap(~Year)

Cairo(file="out/Collar_annual_fixes.PNG",
      type="png",
      width=3000,
      height=2200,
      pointsize=15,
      bg="white",
      dpi=300)
annual.fixes
dev.off()

#- convert telem_dat to sf
telem_sf <- st_as_sf(telem_dat, coords = c("Longitude", "Latitude"), crs=4326)

#####################################################################################
EPU <- left_join(EPU_SBOT, EPU_inv, by=c("EPU.Unit.Name"="EPU"))
EPU_poly$EPU_Unit_N
EPU_poly$EPU_Unit_N <- str_remove(EPU_poly$EPU_Unit_N, "[*]") # for some reason need to do this 3 times, doesn't work with 3*
EPU$EPU.Unit.Name
all(!is.na(EPU_poly$EPU_Unit_N) %in% EPU$EPU.Unit.Name) # if TRUE then the same names (other than 1 NA)

glimpse(EPU)
names(EPU)
EPU <- EPU %>% select("EPU.Unit.Name", "EPU.Land.Area..Ha.", "EPU.Land.Area..km2.","Population.Estimate..2012.","Population.Density.Class..2012.",
                      "EWR.Requirement","EWR.available.quality","Forage.cover.availability","Forage.cover.interspersion","Population.Resiliency",
                      "Predation.Risk", "Unregulated.Hunting", "Min.2020", "Survey.Priority.2020", "Est.Popn.April.2020", "Target.Popn", "Est.Popn.Trend",
                      "GPS.Collars.2020","GPS.Collars.2021")
colnames(EPU) <- c("EPU.Unit.Name", "EPU.Area.Ha", "EPU.Area.km2","Pop.Est.2012","Pop.Dens.Class.2012", "EWR.Requirement","EWR.available.quality",
                   "Forage.cover.availability","Forage.cover.interspersion","Population.Resiliency","Predation.Risk", "Unregulated.Hunting",
                   "Pop.Min.2020", "Survey.Priority.2020", "Pop.Est.2020", "Target.Pop", "Est.Pop.Trend","GPS.Collars.2020","GPS.Collars.2021")

EPU$Pop.Change.2012.2020 <- EPU$Pop.Est.2020 - EPU$Pop.Est.2012
EPU$Est.Density.km2.2012 <- EPU$Pop.Est.2012 / EPU$EPU.Area.km2
EPU %>% group_by(Pop.Dens.Class.2012) %>% summarise(mean(Target.Pop, na.rm=T), min(Target.Pop, na.rm=T), max(Target.Pop, na.rm=T))

EPU %>% filter(!is.na(GPS.Collars.2021)) %>% filter(GPS.Collars.2021>0) %>% group_by(GPS.Collars.2021) %>% count(EPU.Unit.Name)
sum(EPU$GPS.Collars.2021,  na.rm=T) # why does the inventory spreadsheet say 34 collars while there are clearly twice as  many on the land?
# disregard the spreadsheet and go by vectronics download for collar info

all(EPU_poly$EPU_Unit_N %in% EPU$EPU.Unit.Name)
EPU_poly %>% filter(is.na(EPU_Unit_N))

#####################################################################################
###--- view OSM data and download appropriate section for study area
EPU_latlon <- st_transform(EPU_poly, crs=4326)
cities.SC_latlon <- st_transform(cities.SC, crs=4326)
st_bbox(EPU_latlon)
st_bbox(telem_sf)

# use EPU_latlon for entire study area

LAT1 = st_bbox(EPU_latlon)[2] ; LAT2 = st_bbox(EPU_latlon)[4]
LON1 = st_bbox(EPU_latlon)[3] ; LON2 = st_bbox(EPU_latlon)[1]

#our background map
map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
               type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[6],
               mergeTiles = TRUE)

## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# mytheme <- theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
#                  panel.background = element_rect(colour = NA),
#                  plot.background = element_rect(colour = NA),
#                  axis.title = element_text(face = "bold",size = rel(1)),
#                  axis.title.y = element_text(angle=90,vjust =2),
#                  axis.title.x = element_text(vjust = -0.2))

Collar_plot_all <- autoplot(map.latlon)  +
  labs(title = "Elk Locations", subtitle = "Vectronics Data - downloaded Feb 19, 2021",x = "Longitude", y="Latitude")+
  #geom_label(data=telem_dat, aes(x=Longitude ,y=Latitude, label=Animal.ID, fontface=7), hjust=1, vjust=0, size=4) +
  geom_point(data=telem_dat, aes(x=Longitude, y=Latitude, fill=Animal.ID), size=4, shape=21) +
  #scale_fill_gradientn(colours = rainbow(10)) +
  mytheme

Collar_plot_2021 <- autoplot(map.latlon)  +
  labs(title = "Elk Locations - 2021", subtitle = "Vectronics Data - downloaded Feb 19, 2021",x = "Longitude", y="Latitude")+
  geom_point(data=telem_dat[telem_dat$Year=="2021",], aes(x=Longitude, y=Latitude, fill=Animal.ID), size=4, shape=21) +
  mytheme

#####################################################################################
###--- simple map overlaying EPU and 2021 telem locations

ggplot() +
  geom_sf(data = EPU_latlon, fill = NA) +
  #geom_sf_label(data = EPU_poly, aes(label = EPU_Unit_N), cex=2) +
  geom_sf(data = telem_sf[telem_sf$Year=="2021",], aes(fill=Animal.ID, col=Animal.ID))+
  geom_sf(data = cities.SC[cities.SC$CITY_TYPE!="VL",]) + #geom_sf_label(data = cities.SC, aes(label = NAME), cex=2) +
  theme(legend.position = "none")+
  coord_sf()+
  theme_minimal()

# add EPU.Unit.Name to telem_sf point (intersect)
telem_sf <- st_join(telem_sf, EPU_latlon)
# then add that EPU location to telem_dat object
telem_dat$EPU.Fix <- telem_sf$EPU_Unit_N

cities.SC$NAME

# add distance to Sechelt for ease of logistics
# need to convert to utm for m distance
# espg 26910
telem_utm <- st_transform(telem_sf, crs=26910)
cities.SC_utm <- st_transform(cities.SC, crs=26910)

ls = st_nearest_points(cities.SC_utm[cities.SC_utm$NAME=="Sechelt",], telem_utm)
ls$dist.m <- st_length(ls)

# add distance from each point to sechelt
telem_dat$Sechelt.dist.m <- ls$dist.m
sechelt.dist <- telem_dat %>% group_by(EPU.Fix, Animal.ID) %>% summarise(mean.dist = mean(Sechelt.dist.m), sd.dist = sd(Sechelt.dist.m))
sechelt.dist.km <- sechelt.dist %>% group_by(EPU.Fix) %>% summarise(dist.km = mean(as.numeric(mean.dist))/1000)
as.data.frame(sechelt.dist.km %>% arrange(dist.km))

as.data.frame(telem_dat %>% group_by(Pop.Unit.Release) %>% count(EPU.Fix))
# some collar fixes in different EPU than animal released - did animal move or wrong release location in records?

elk.per.EPU <- as.data.frame(telem_dat %>% filter(Year=="2021") %>% group_by(EPU.Fix) %>% count(Animal.ID))
elk.per.EPU %>% count(EPU.Fix) # 22 EPUs with elk with active collars in 2021
elk.per.EPU %>% count(Animal.ID) # 47 elk with operational collars in 2021
