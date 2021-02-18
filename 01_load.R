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
# script to collate elk (meta)data for sightability methods, covariates
# written by Joanna Burgar (Joanna.Burgar@gov.bc.ca) - 17-Feb-2021
#####################################################################################

.libPaths("C:/Program Files/R/R-3.6.0/library") # to ensure reading/writing libraries from C drive
# overall process:
#- Upload Animal metadata and collar metadata (if applicable)
#-

# Load Packages
list.of.packages <- c("tidyverse", "lubridate", "bcdata", "bcmaps")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Import files
# Read deployment data csv for station covariates
EPU_SBOT <- read.csv("data/Elk_SBOT_data.csv", header=T, colClasses=c("character")) %>% type_convert()
EPU_inv <- read.csv("data/EPU_Priority.csv", header=T, colClasses=c("character")) %>% type_convert()

EPU <- left_join(EPU_SBOT, EPU_inv, by=c("EPU.Unit.Name"="EPU"))
glimpse(EPU)
EPU$Popn.Change.2012.2020 <- EPU$Est.Popn.April.2020 - EPU$Population.Estimate..2012.
EPU$Est.Density.km2.2012 <- EPU$Population.Estimate..2012. / EPU$EPU.Land.Area..km2.

as.data.frame(EPU %>% filter(is.na(Est.Popn.April.2020)) %>% group_by(EPU.Unit.Name, Population.Density.Class..2012.) %>% select(Est.Density.km2.2012)
as.data.frame(EPU %>% filter(!is.na(Est.Popn.April.2020)) %>% group_by(EPU.Unit.Name, Population.Density.Class..2012.) %>% select(Est.Popn.April.2020, Population.Estimate..2012.))

EPU %>% group_by(Population.Density.Class..2012.) %>% summarise(mean(Target.Popn, na.rm=T), min(Target.Popn, na.rm=T), max(Target.Popn, na.rm=T))
