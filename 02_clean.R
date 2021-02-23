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
  select(Date, `Serial No.`, `Initial or Recapture`, `Population Unit`,
         `Easting (capture)`, `Northing (capture)`, `Release                       (on site or relocated)`,
         `Release Date`, `Population Unit (release)`, `Easting (release)`, `Northing (release)`, `Age Class`, Sex)

elk_cptr_telem <- droplevels(elk_cptr_telem)
colnames(elk_cptr_telem) <- c("Date.Capture", "Collar.ID", "Initial.Recapture", "Pop.Unit.Capture",
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

###--- join the two collar metadata files
collar_meta <- left_join(elk_cptr_telem, elk_inv)

# group status values into 3
collar_meta <- collar_meta %>% mutate(Status = case_when(grepl("On hand", Status, ignore.case = TRUE) ~ "Office",
                           grepl("last", Status, ignore.case = TRUE) ~ "Field.NotOperational",
                           TRUE ~ "Field.Operational"))

collar_meta$Status <- as.factor(collar_meta$Status)
as.data.frame(collar_meta %>% filter(Status=="Field.Operational") %>% count(Pop.Unit.Release))

#####################################################################################
#- downloaded telemetry data
collar_pos$Date.Time.PST <- with_tz(collar_pos$Date.Time.UTC, tz)
collar_pos$Time.PST <- times(strftime(collar_pos$Date.Time.PST,"%H:%M:%S", tz))

collar_pos <- collar_pos %>% mutate(Year = year(Date.Time.PST), Month = month(Date.Time.PST, label = T), jDay = yday(Date.Time.PST))


# check collar data loaded properly
collar.dates <- collar_pos %>% group_by(Collar.ID) %>% summarise(min.Date = min(Date.Time.PST), max.Date = max(Date.Time.PST))
min(collar.dates$min.Date); max(collar.dates$min.Date)
# [1] "2017-01-25 06:00:37 PST"
# [1] "2020-02-25 06:32:36 PST"

min(collar.dates$max.Date); max(collar.dates$max.Date)
# [1] "2018-11-07 06:13:14 PST"
# [1] "2021-02-19 04:02:39 PST"

# will still need to clean collar data to make sure not included dates when collaring individuals
# speak to bios about # days post collaring to start including animals in analysis
collar_annual_fixes <- collar_pos %>% group_by(Collar.ID, Year) %>% summarise(Counts = sum(Count))
ggplot(collar_annual_fixes, aes(fill=Collar.ID, y=Counts, x=Collar.ID))+
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~Year)


ggplot(data = collar_annual_fixes, aes(x = reorder(Collar.ID, -Counts), y = Counts, fill= Collar.ID)) +
  geom_bar(stat = "identity") +
  theme_classic() + ylab("Number of Annual Fixes per Collar") +
  theme(legend.position="none") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.y = element_text(size = 14)) + theme(axis.title.x = element_blank())+
  facet_wrap(~Year)

# check data loaded
glimpse(collar_pos)
collar_pos %>% count(Collar.ID) # 65 collars

names(collar_pos)

#####################################################################################
EPU <- left_join(EPU_SBOT, EPU_inv, by=c("EPU.Unit.Name"="EPU"))
EPU_poly$EPU_Unit_N
EPU_poly$EPU_Unit_N <- str_remove(EPU_poly$EPU_Unit_N, "[*]") # for some reason need to do this 3 times, doesn't work with 3*
EPU$EPU.Unit.Name
all(!is.na(EPU_poly$EPU_Unit_N) %in% EPU$EPU.Unit.Name) # if TRUE then the same names (other than 1 NA)


EPU$Popn.Change.2012.2020 <- EPU$Est.Popn.April.2020 - EPU$Population.Estimate..2012.
EPU$Est.Density.km2.2012 <- EPU$Population.Estimate..2012. / EPU$EPU.Land.Area..km2.

as.data.frame(EPU %>% filter(is.na(Est.Popn.April.2020)) %>% group_by(EPU.Unit.Name, Population.Density.Class..2012.) %>% select(Est.Density.km2.2012))
as.data.frame(EPU %>% filter(!is.na(Est.Popn.April.2020)) %>% group_by(EPU.Unit.Name, Population.Density.Class..2012.) %>% select(Est.Popn.April.2020, Population.Estimate..2012.))

EPU %>% group_by(Population.Density.Class..2012.) %>% summarise(mean(Target.Popn, na.rm=T), min(Target.Popn, na.rm=T), max(Target.Popn, na.rm=T))

