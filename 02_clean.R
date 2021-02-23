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
EPU$Popn.Change.2012.2020 <- EPU$Est.Popn.April.2020 - EPU$Population.Estimate..2012.
EPU$Est.Density.km2.2012 <- EPU$Population.Estimate..2012. / EPU$EPU.Land.Area..km2.

as.data.frame(EPU %>% filter(is.na(Est.Popn.April.2020)) %>% group_by(EPU.Unit.Name, Population.Density.Class..2012.) %>% select(Est.Density.km2.2012))
as.data.frame(EPU %>% filter(!is.na(Est.Popn.April.2020)) %>% group_by(EPU.Unit.Name, Population.Density.Class..2012.) %>% select(Est.Popn.April.2020, Population.Estimate..2012.))

EPU %>% group_by(Population.Density.Class..2012.) %>% summarise(mean(Target.Popn, na.rm=T), min(Target.Popn, na.rm=T), max(Target.Popn, na.rm=T))

