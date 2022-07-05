# Elk Sightability Analysis
# Step 1: Load and clean

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")

# 1.1 LOAD PACKAGES ####
library("SightabilityModel")
library(tidyverse)
library(readxl)
library(rjags)
list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "scrbook", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 1.2 Modified Horowitz-Thompson Analysis ####
## 1.2.1 LOAD DATA ####
dat.2016 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2016 Survey Data", range = "A2:K78") 

dat.2017 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2017 Survey Data", range = "A1:K83") 

dat.2018 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2018 Survey Data", range = "A1:K76") 

dat.2019 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2019 Survey Data", range = "A1:K96")

dat.2020 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2020 Survey Data", range = "A1:K93")
dat.2021 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2021 Survey Data", range = "A1:O136", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "text", "text", "numeric", "text", 
                                     "text"))
dat.2022 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2022 Survey Data", range = "A1:P102", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "skip", "text", "text", "numeric", 
                                     "text", "text"))
EPU.areas <- read_csv("Effort/EPU_areas.csv", 
                      col_types = cols(OID = col_skip(), Shape_Leng = col_skip()))

EPU.list <- as.character(EPU.areas$Unit)
print(EPU.list)

## 1.2.2 SAMPLE INFO ####

area.2016 <- read_csv("Effort/areas_2016.csv")
eff.2016 <- area.2016 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2016)

area.2017 <- read_csv("Effort/areas_2017.csv") %>%
  # get rid of surveys that weren't counted in total
  filter(begin != "2017/03/14 21:06:36.055" | Unit!="Clowhom") %>%
  filter((begin != "2017/03/20 15:16:10.196" & begin != "2017/03/30 15:19:37.056") | Unit!="Stave") %>%
  filter(begin != "2017/03/27 15:12:24.072" | Unit!="Rainy-Gray") %>%
  filter(begin != "2017/03/30 15:19:37.056" | Unit!="Pitt")
eff.2017 <- area.2017 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2017)

area.2018 <- read_csv("Effort/areas_2018.csv") %>%
  filter(begin == "2017/03/30 15:19:37.056" | Unit!="Pitt")
eff.2018 <- area.2018 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2018)

area.2019 <- read_csv("Effort/areas_2019.csv")
eff.2019 <- area.2019 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2019)

area.2020 <- read_csv("Effort/areas_2020.csv") %>%
  filter(Unit != "Skwawka" | begin != "2020/03/24 19:15:10.996") %>%
  filter(Unit != "Sechelt")
eff.2020 <- area.2020 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2020)

area.2021 <- read_csv("Effort/areas_2021.csv")
eff.2021 <- area.2021 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2021)



## 1.2.3 EXPERIMENTAL DATASET ####

# clean survey types and filter out incidental observations
# 2 options:
## 1. 1 for spotting collared elk, 0 for searching for collar
## 2. 1 for spotting any elk (collared or not), 0 for searching for collar

exp.tmp <- bind_rows(dat.2021, dat.2022) %>%
  mutate(
    subunit = EPU,
    total = `Elk Obs.`,
    survey.type = `Survey type`,
    voc = `% cover`,
    .keep = "unused"
  ) %>%
  mutate(
    survey.type = case_when(
      grepl("incidental", survey.type, ignore.case = TRUE) ~ "Incidental",
      grepl("telemetry", survey.type, ignore.case = TRUE) ~ "Telemetry",
      grepl("transect", survey.type, ignore.case = TRUE) ~ "Inventory",
      grepl("inventory", survey.type, ignore.case = TRUE) ~ "Inventory",
      TRUE ~ "Other"
    )
  ) %>%
  filter(survey.type == "Telemetry" | survey.type == "Inventory")


# Option 1
exp.tmp <- exp.tmp %>%
  mutate(collar =
           case_when(
             grepl("no collar", `Notes:`, ignore.case = TRUE) ~ "No Collar",
             grepl("dart", `Notes:`, ignore.case = TRUE) ~ "No Collar",
             str_detect(`Notes:`, "15\\d.\\d\\d\\d") ~ as.character(
               str_extract_all(`Notes:`, "15\\d.\\d\\d\\d"), sep=", "),
             str_detect(`Notes:`, "\\d\\d\\d\\d\\d") ~ as.character(
               str_extract_all(`Notes:`, "\\d\\d\\d\\d\\d"), sep=", "),
             str_detect(`Notes:`, "2 yellow collar") ~ "Yellow, Yellow",      
             grepl("yellow collar", `Notes:`, ignore.case = TRUE) ~ "Yellow",
             grepl("Telemetry", `survey.type`) ~ "0",
             TRUE ~ "No Collar"
           )
  ) %>%
  filter(collar != "No Collar") %>%
  arrange(collar)

# duplicate observations with 2 collars, then clean
exp.tmp <- rbind(exp.tmp, exp.tmp[rep(c(44:48, 56:57), 1),])

exp <- exp.tmp %>%
  transmute(
    year = as.integer(Year),
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    voc = as.integer(voc*100),
    grpsize = as.integer(total),
    habitat = Habitat,
    activity = Activity
  )

# Option 2
# exp <- exp.tmp %>%
#   transmute(
#     year = as.integer(Year),
#     observed = as.integer(if_else(survey.type=="Transect", 1, 0)),
#     voc = as.integer(voc*100),
#     grpsize = as.integer(total),
#     habitat = Habitat,
#     activity = Activity
#   )

## 1.2.4 OBSERVATIONAL DATASET ####
#### 1.2.4.1 START OBS ####
# 2016
# deal w/ NAs and raghorn observations, then clean
dat.2016 <- dat.2016 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Raghorn = if_else(is.na(Raghorn), 0, Raghorn),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  )
obs.2016 <- dat.2016 %>%
  mutate(Bull = Bull+Raghorn) %>%
  select(!Raghorn) %>%
  transmute(
    year = 2016,
    stratum = 0,
    subunit = EPU,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    notes = `Notes:`
  )
# Get rid of incidentals/observations not counted in total
obs.2016 <- obs.2016 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# 2017
dat.2017 <- dat.2017 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Raghorn = if_else(is.na(Raghorn), 0, Raghorn),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  )
obs.2017 <- dat.2017 %>%
  mutate(Bull = Bull+Raghorn) %>%
  select(!Raghorn) %>%
  transmute(
    year = 2017,
    stratum = 0,
    subunit = EPU,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    notes = `Notes:`
  )
obs.2017 <- obs.2017 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# 2018
# unique() tells us there are no values for Raghorn or unclassified -> get rid of raghorn and make unclass = 0
dat.2018 <- dat.2018 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    `Unclass.` = 0
  )
obs.2018 <- dat.2018 %>%
  select(!Raghorn) %>%
  transmute(
    year = 2018,
    stratum = 0,
    subunit = EPU,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    notes = `Notes:`
  )
obs.2018 <- obs.2018 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# 2019
# unique() tells us no raghorn values exist -> just delete
dat.2019 <- dat.2019 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  )
obs.2019 <- dat.2019 %>%
  select(!Raghorn) %>%
  transmute(
    year = 2019,
    stratum = 0,
    subunit = EPU,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    notes = `Notes:`
  )
obs.2019 <- obs.2019 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# 2020
# same as 2018
dat.2020 <- dat.2020 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = 0
  )
obs.2020 <- dat.2020 %>%
  select(!Raghorn) %>%
  transmute(
    year = 2020,
    stratum = 0,
    subunit = EPU,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    notes = `Notes:`
  )
obs.2020 <- obs.2020 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# 2021
dat.2021 <- dat.2021 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  )
obs.2021 <- dat.2021 %>%
  transmute(
    year = 2021,
    stratum = 0,
    subunit = EPU,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    voc = `% cover`*100,
    habitat = Habitat,
    activity = Activity,
    notes = `Notes:`
  )
obs.2021 <- obs.2021 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# bind together
obs.all <- bind_rows(obs.2016, obs.2017, obs.2018, obs.2019, obs.2020, obs.2021)

# ensure EPU names match
setdiff(obs.all$subunit, EPU.list) # 29 names don't match EPU list names, fix below
obs.all <- obs.all %>%
  mutate(subunit = case_when(
    grepl("Rainy", subunit, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrows", subunit, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Deserted", subunit, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Chehalis", subunit, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sechelt", subunit, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homathco", subunit, ignore.case = TRUE) ~ "Homathko",
    grepl("Haslam", subunit, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", subunit, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quatum", subunit, ignore.case = TRUE) ~ "Quatam",
    grepl("Lillooet", subunit, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Vancouver", subunit, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squamish", subunit, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", subunit, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", subunit, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", subunit, ignore.case = TRUE) ~ "Theo",
    grepl("Mcnab", subunit, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", subunit, ignore.case = TRUE) ~ "Bear",
    TRUE ~ subunit
  ))

# make sure all EPU names match
setdiff(obs.all$subunit, EPU.list) #If returns "character(0)", all names match

# Get rid of incidental observations outside each year's surveyed EPUs
obs.all <- obs.all %>%
  filter(total > 0) %>%
  mutate(subunit = if_else(
    year == 2016 & subunit %in% eff.2016$Unit, subunit,
    if_else(
      year == 2017 & subunit %in% eff.2017$Unit, subunit,
      if_else(
        year == 2018 & subunit %in% eff.2018$Unit, subunit,
        if_else(
          year == 2019 & subunit %in% eff.2019$Unit, subunit,
          if_else(
            year == 2020 & subunit %in% eff.2020$Unit, subunit,
            if_else(
              year == 2021 & subunit %in% eff.2021$Unit, subunit,
              "X"))))))) %>%
  filter(subunit != "X")

### 1.2.4.2 STRATUM FIELD ####

# Create stratum ID field
## 1. Break obs.all into each year

tmp <- obs.all %>%
  filter(year==2016)
eff.2016 <- semi_join(eff.2016, tmp, by=c("Unit"="subunit", "year"))
eff.2016 <- eff.2016 %>%
  mutate(ID = as.integer(row.names(eff.2016)))
tmp.2016 <- left_join(tmp, eff.2016, by=c("subunit"="Unit", "year")) %>%
  mutate(stratum = ID) %>%
  select(year:grpsize, voc:activity)

tmp <- obs.all %>%
  filter(year==2017)
eff.2017 <- semi_join(eff.2017, tmp, by=c("Unit"="subunit", "year"))
eff.2017 <- eff.2017 %>%
  mutate(ID = as.integer(row.names(eff.2017)))
tmp.2017 <- left_join(tmp, eff.2017, by=c("subunit"="Unit", "year")) %>%
  mutate(stratum = ID) %>%
  select(year:grpsize, voc:activity)

tmp <- obs.all %>%
  filter(year==2018)
eff.2018 <- semi_join(eff.2018, tmp, by=c("Unit"="subunit", "year"))
eff.2018 <- eff.2018 %>%
  mutate(ID = as.integer(row.names(eff.2018)))
tmp.2018 <- left_join(tmp, eff.2018, by=c("subunit"="Unit", "year")) %>%
  mutate(stratum = ID) %>%
  select(year:grpsize, voc:activity)

tmp <- obs.all %>%
  filter(year==2019)
eff.2019 <- semi_join(eff.2019, tmp, by=c("Unit"="subunit", "year"))
eff.2019 <- eff.2019 %>%
  mutate(ID = as.integer(row.names(eff.2019)))
tmp.2019 <- left_join(tmp, eff.2019, by=c("subunit"="Unit", "year")) %>%
  mutate(stratum = ID) %>%
  select(year:grpsize, voc:activity)

tmp <- obs.all %>%
  filter(year==2020)
eff.2020 <- semi_join(eff.2020, tmp, by=c("Unit"="subunit", "year"))
eff.2020 <- eff.2020 %>%
  mutate(ID = as.integer(row.names(eff.2020)))
tmp.2020 <- left_join(tmp, eff.2020, by=c("subunit"="Unit", "year")) %>%
  mutate(stratum = ID) %>%
  select(year:grpsize, voc:activity)

tmp <- obs.all %>%
  filter(year==2021)
eff.2021 <- semi_join(eff.2021, tmp, by=c("Unit"="subunit", "year"))
eff.2021 <- eff.2021 %>%
  mutate(ID = as.integer(row.names(eff.2021)))
tmp.2021 <- left_join(tmp, eff.2021, by=c("subunit"="Unit", "year")) %>%
  mutate(stratum = ID) %>%
  select(year:grpsize, voc:activity)

## 2. Join back together
obs <- bind_rows(tmp.2016, tmp.2017, tmp.2018, tmp.2019, tmp.2020, tmp.2021)
paste0(2016)
eff.2016 %>%
  select(EPU = Unit, Stratum = ID) %>%
  print()
paste0(2017)
eff.2017 %>%
  select(EPU = Unit, Stratum = ID) %>%
  print()
paste0(2018)
eff.2018 %>%
  select(EPU = Unit, Stratum = ID) %>%
  print()
paste0(2019)
eff.2019 %>%
  select(EPU = Unit, Stratum = ID) %>%
  print()
paste0(2020)
eff.2020 %>%
  select(EPU = Unit, Stratum = ID) %>%
  print()
paste0(2021)
eff.2021 %>%
  select(EPU = Unit, Stratum = ID) %>%
  print()

# Get rid of eff records of surveyed areas that had no observations

eff <- bind_rows(eff.2016, eff.2017, eff.2018, eff.2019, eff.2020, eff.2021)
eff.max <- eff %>%
  group_by(Unit) %>%
  slice_max(area_surveyed_km) %>%
  mutate(Nh = area_surveyed_km) %>%
  select(Unit, Nh)

sampinfo <- left_join(eff, eff.max, by="Unit")

sampinfo <- sampinfo %>%
  mutate(stratum = as.integer(ID), Nh = as.integer(Nh), 
         nh = as.integer(area_surveyed_km), year = as.integer(year)) %>%
  select(year, stratum, Nh, nh)

### 1.2.4.3 FINSIH OBS ####
# Make all numeric fields integers
obs <- obs %>%
  mutate(year = as.integer(year),
         stratum = as.integer(stratum),
         total = as.integer(total),
         cows = as.integer(cows),
         calves = as.integer(calves),
         spikes = as.integer(spikes),
         bulls = as.integer(bulls),
         unclass = as.integer(unclass),
         voc = as.integer(voc),
         grpsize = as.integer(grpsize))

# make sure totals = sum of cows, calves, etc
obs %>%
  filter(obs$total != (obs$cows+obs$calves+obs$spikes+obs$bulls+obs$unclass)) %>%
  view()
# If records show up, use code below to add unclassified individuals & re-check
obs <- obs %>%
  mutate(
    unclass = if_else(
      total == (cows+calves+spikes+bulls+unclass), unclass,
      total-((cows+calves+spikes+bulls)))
  )
obs %>%
  filter(obs$total != (obs$cows+obs$calves+obs$spikes+obs$bulls+obs$unclass)) %>%
  view() # all good now


# Make sure all stratum listed in obs are listed in sampinfo for each year
setdiff(sampinfo$stratum[sampinfo$year==2016], unique(obs$stratum[obs$year==2016]))
setdiff(sampinfo$stratum[sampinfo$year==2017], unique(obs$stratum[obs$year==2017]))
setdiff(sampinfo$stratum[sampinfo$year==2018], unique(obs$stratum[obs$year==2018]))
setdiff(sampinfo$stratum[sampinfo$year==2019], unique(obs$stratum[obs$year==2019]))
setdiff(sampinfo$stratum[sampinfo$year==2020], unique(obs$stratum[obs$year==2020]))
setdiff(sampinfo$stratum[sampinfo$year==2021], unique(obs$stratum[obs$year==2021]))

## 1.2.5 SAVE DATA ####
save(list = c("eff", "exp", "obs", "sampinfo"), file = "elk_mHT.Rdata")
rm(list = ls())

# 1.3 Bayesian Analsysis ####

## 1.3.1 LOAD DATA ####

dat.2021 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2021 Survey Data", range = "A1:O136", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "text", "text", "numeric", "text", 
                                     "text"))
dat.2022 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2022 Survey Data", range = "A1:P102", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "skip", "text", "text", "numeric", 
                                     "text", "text"))
EPU.areas <- read_csv("Effort/EPU_areas.csv",
                      col_types = cols(OID = col_skip(), Shape_Leng = col_skip()))

# Create list of EPUs
EPU.list <- as.character(EPU.areas$Unit)
print(EPU.list)

## 1.3.2 CLEAN DATA ####
### 1.3.2.1 EFFORT DATA ####
# take effort data from summary pages

min.count <- read_excel("SurveyData_ SPRING_2022.xls", 
                        sheet = "2022 Summary", range = "A2:O29") %>%
  select(EPU = `...1`, Min_2021 = `Min 2021`, Min_2022 = `Min 2022`)

# get rid of letters in count columns and classify as integer fields

min.count$Min_2021 <- min.count$Min_2021 %>%
  str_remove_all("[:alpha:]") %>% 
  str_remove_all("[:punct:]") %>%
  str_remove_all("[:symbol:]") %>%
  as.integer(min.count$Min_2021)

min.count$Min_2022 <- min.count$Min_2022 %>%
  str_remove_all("[:alpha:]") %>% 
  str_remove_all("[:punct:]") %>%
  str_remove_all("[:symbol:]") %>%
  as.integer(min.count$Min_2022)

# make names match
setdiff(min.count$EPU, EPU.list)
min.count <- min.count %>%
  mutate(EPU = case_when(
    grepl("Chehalis", EPU, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sechelt", EPU, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Haslam", EPU, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", EPU, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Theo", EPU, ignore.case = TRUE) ~ "Theo",
    TRUE ~ EPU))
setdiff(min.count$EPU, EPU.list) # good now

eff <- min.count %>%
  mutate(surveyed_2021 = if_else(is.na(Min_2021)|Min_2021==0, F, T),
         surveyed_2022 = if_else(is.na(Min_2022)|Min_2022==0, F, T)) %>%
  filter(surveyed_2021 == T | surveyed_2022 == T) %>%
  select(EPU, surveyed_2021, surveyed_2022)
summary(eff)

# make sure numbers are as expected
# add ID# to each EPU
eff$ID <- as.double(row.names(eff))
eff %>% select(EPU, ID, surveyed_2021, surveyed_2022)

### 1.3.2.2 OBSERVATION DATA ####

# 2022
dat.2022 <- dat.2022 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  )
obs.2022 <- dat.2022 %>%
  transmute(
    year = 2022,
    date = as.Date(strptime(dat.2022$Date, "%b %d %Y")),
    stratum = EPU,
    subunit = 1,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    voc = `% cover`,
    habitat = Habitat,
    activity = Activity,
    survey_type = `Survey type`,
    notes = `Notes:`
  )
obs.2022 <- obs.2022 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) |
      grepl("telemetry", survey_type, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# 2021
dat.2021 <- dat.2021 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  )
obs.2021 <- dat.2021 %>%
  transmute(
    year = 2021,
    date = as.Date(strptime(dat.2021$Date, "%b %d %Y")),
    stratum = EPU,
    subunit = 1,
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    voc = `% cover`,
    habitat = Habitat,
    activity = Activity,
    survey_type = `Survey type`,
    notes = `Notes:`
  )
obs.2021 <- obs.2021 %>%
  mutate(counted = case_when(
    grepl("re-flown", notes, ignore.case = TRUE) |
      grepl("not in total", notes, ignore.case = TRUE) |
      grepl("not counted", notes, ignore.case = TRUE) |
      grepl("no effort", notes, ignore.case = TRUE) |
      grepl("incidental", notes, ignore.case = TRUE) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(counted=="Y")

# bind together
obs.all <- bind_rows(obs.2022, obs.2021)

# ensure EPU names match
setdiff(obs.all$stratum, EPU.list) # 11 names don't match EPU list names, fix below
obs.all <- obs.all %>%
  mutate(stratum = case_when(
    grepl("Rainy", stratum, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrows", stratum, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Deserted", stratum, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Chehalis", stratum, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sechelt", stratum, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", stratum, ignore.case = TRUE) ~ "Homathko",
    grepl("Haslam", stratum, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", stratum, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quatum", stratum, ignore.case = TRUE) ~ "Quatam",
    grepl("Lillooet", stratum, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Vancouver", stratum, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squamish", stratum, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", stratum, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", stratum, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", stratum, ignore.case = TRUE) ~ "Theo",
    grepl("Mcnab", stratum, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", stratum, ignore.case = TRUE) ~ "Bear",
    TRUE ~ stratum
  ))

# make sure all EPU names match
setdiff(obs.all$stratum, EPU.list) #If returns "character(0)", all names match

# standardize survey types, habitat types, and activity
obs.all <- obs.all %>%
  mutate(
    survey_type = case_when(
      grepl("incidental", survey_type, ignore.case = TRUE) ~ "Incidental",
      grepl("telemetry", survey_type, ignore.case = TRUE) ~ "Telemetry",
      grepl("transect", survey_type, ignore.case = TRUE) ~ "Inventory",
      grepl("inventory", survey_type, ignore.case = TRUE) ~ "Inventory",
      TRUE ~ "Other"
    ),
    # 1 - rock / other (gravel, landfill, road, WTP, other)
    # 2 - meadow / riparian (field, meadow, riparian, wetland, river, slide)
    # 3 - cutblock / powerline (block, powerline, NSR, FTG)
    # 4 - mature forest (mature, old)
    habitat = case_when(
      grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ 4,
      grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ 3,
      grepl("field|meadow|riparian|wetland|river|slide|out", habitat, ignore.case = TRUE) ~ 2,
      grepl("gravel|landfill|road|wtp|other", habitat, ignore.case = TRUE) ~ 1
    ),
    activity = case_when(
      grepl("standing|moving|run", activity, ignore.case = TRUE) ~ 1,
      grepl("bed", activity, ignore.case = TRUE) ~ 0)
  )

# standardize habitat


# Get rid of incidental observations outside each year's surveyed EPUs
obs.all.ins <- left_join(obs.all, eff, by=c("stratum"="EPU")) %>%
  filter(total > 0) %>%
  mutate(stratum = if_else(
    year == 2022 & surveyed_2022 == T, stratum,
    if_else(
      year == 2021 & surveyed_2021 == T, stratum,
      "X"))) %>%
  filter(subunit != "X")


# Set numeric fields 
obs <- obs.all.ins %>%
  mutate(year = as.integer(year),
         stratum = stratum,
         total = as.integer(total),
         cows = as.integer(cows),
         calves = as.integer(calves),
         spikes = as.integer(spikes),
         bulls = as.integer(bulls),
         unclass = as.integer(unclass),
         voc = as.double(voc),
         grpsize = as.integer(grpsize))

# make sure totals = sum of cows, calves, etc
obs %>%
  filter(obs$total != (obs$cows+obs$calves+obs$spikes+obs$bulls+obs$unclass)) %>%
  view()
# If records show up, use code below to add unclassified individuals & re-check
obs <- obs %>%
  mutate(
    unclass = if_else(
      total <= (cows+calves+spikes+bulls+unclass), unclass,
      total-((cows+calves+spikes+bulls))),
    total = (cows+calves+spikes+bulls+unclass)
  )
obs %>%
  filter(obs$total != (obs$cows+obs$calves+obs$spikes+obs$bulls+obs$unclass))
# resulting table should have 0 rows

# get rid of NAs in voc
# add mean voc by habitat
avg.voc <- obs %>%
  group_by(habitat) %>%
  summarize(mean_voc = mean(voc, na.rm=T))
avg.voc.overall <- mean(obs$voc, na.rm=T)

obs.voc <- left_join(obs, avg.voc, by="habitat") %>%
  mutate(mean_voc = if_else(is.na(mean_voc), avg.voc.overall, mean_voc),
         voc = if_else(is.na(voc), mean_voc, voc))

obs <- obs.voc

### 1.3.2.3 SIGHTABILITY DATA ####
exp.tmp <- obs %>%
  filter(survey_type == "Telemetry" | survey_type == "Inventory")
# only keep observations of collared elk
exp.tmp <- exp.tmp %>%
  mutate(collar =
           case_when(
             grepl("no collar", notes, ignore.case = TRUE) ~ "No Collar",
             grepl("collar in group", notes, ignore.case = TRUE) ~ "Yes",
             grepl("collars in group", notes, ignore.case = TRUE) ~ "Yes, Yes",
             grepl("dart", notes, ignore.case = TRUE) ~ "No Collar",
             str_detect(notes, "15\\d.\\d\\d\\d") ~ as.character(
               str_extract_all(notes, "15\\d.\\d\\d\\d"), sep=", "),
             str_detect(notes, "\\d\\d\\d\\d\\d") ~ as.character(
               str_extract_all(notes, "\\d\\d\\d\\d\\d"), sep=", "),
             str_detect(notes, "2 yellow collar") ~ "Yellow, Yellow",      
             grepl("yellow collar", notes, ignore.case = TRUE) ~ "Yellow",
             
             grepl("Telemetry", `survey_type`) ~ "Yes",
             
             TRUE ~ "No Collar"
           )
  ) %>%
  filter(collar != "No Collar") %>%
  arrange(collar)

# duplicate observations with 2 collars, then clean
exp.tmp <- rbind(exp.tmp, exp.tmp[rep(c(27:28, 45:47, 20:21), 1),])

## 1.3.3 SIGHT DAT ####

# Sightability survey data:  64 records
# w = habitat indicator ()
# x = visual obstrcution measurements associated with the test trial data used to develop the sightability model
# y = activity indicator (0 if bedded, 1 if standing)
# z = detection indicator (1 if the group was observed, 0 otherwise)

sight.dat <- exp.tmp %>%
  mutate(a = as.double(activity),
         s = as.double(habitat),
         x.tilde = as.double(voc),
         z.tilde = as.double(if_else(survey_type=="Telemetry", 0, 1), .keep="none"))

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

# test correlations between variables and z
sight.dat %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))

cor.test(sight.dat$z.tilde, sight.dat$x.tilde, method="pearson") # -0.5338735 with p-value 0.00002698 -> significant moderate negative correlation between voc and z
cor.test(sight.dat$z.tilde[!is.na(sight.dat$a)], sight.dat$a[!is.na(sight.dat$a)], method="pearson") # -0.08887529 with p-value 0.5268 -> no corelation between activity and z
cor.test(sight.dat$z.tilde, sight.dat$s, method="pearson") # -0.5193816 with p-value 0.0000484 -> significant moderate negative correlation between habitat and z

# voc is most significantly correlated with sightability -> select only voc
sight.dat <- sight.dat %>% select(x.tilde, z.tilde)

## 1.3.4 OPER DAT ####

# Operational survey data:  4380 records
# (includes observed and augmented data for the annual surveys in  2006 and 2007 combined).
# Augmented data records have NA (missing) for x, y, q.
# x = visual obstruction measurements
# ym1 = y-1, where y = observed group size
# h = stratum identifier (all same stratum)
# q = indicator variable that represents whether the group belongs to the study population (equal to 1 for all observed groups and NA for all augmented groups).
# z = detection indicator (equal to 1 if the group was observed during the operational survey and 0 otherwise)
# subunits = unique plot identifier (for all sampled plots).
# yr = year of observation (1 = 2006, 2= 2007)

# non-augmented data

 # exclude telemetry?
oper.dat <- obs %>%
  filter(survey_type == "Inventory") %>%
  transmute(a = as.double(activity),
            s = as.double(habitat),
            x = as.double(voc),
            ym1 = total-1,
            h = ID,
            q = 1,
#           z = if_else(survey_type == "Telemetry", 0, 1)
            z = 1,
            yr = if_else(year == 2021, 1, 2),
            subunits = ID) %>%
  glimpse()

# augmented data
# need to determine max m of each h
m.est <- oper.dat %>%
  group_by(yr, h) %>%
  summarize(m = n())
m.max <- m.est %>%
  group_by(h) %>%
  summarize(yr = yr,
            m = m,
            m.max = max(m))
b.h <- m.max %>%
  mutate(b.h = 10*m.max,
         B_minus_m = b.h-m)
oper.dat.aug <- b.h[rep(1:nrow(b.h), b.h$B_minus_m),] %>%
  mutate(a = NA, s = NA, x = NA, ym1 = NA, h = h, q = NA, z = 0, yr = yr, subunits = h, .keep="none") %>%
  ungroup()

oper.dat <- rbind(oper.dat, oper.dat.aug) %>%
  arrange(yr, h, q)

glimpse(oper.dat) # check

## 1.3.5 PLOT DAT ####

# Plot-level information data: 77 records (one for each of the plots sampled in either 2006 or 2007)
# h.plots = stratum to which the plot belonged (1, 2, 3 correspond to low, medium and high density strata)
# yr.plots = year the plot was sampled (1 = 2006, 2= 2007)
plot.dat <- oper.dat %>%
  select(yr, h) %>%
  distinct() %>%
  mutate(h.plots = h, 
         yr.plots = yr) %>%
  select(h.plots, yr.plots) %>%
  arrange(yr.plots, h.plots)
glimpse(plot.dat)

## 1.3.6 SCALAR DAT ####

#   Scalars:
#   R = number of sightability trials (64)
# 
#   Ngroups = number of observed and augmented groups for the 2006 and 2007 annual operational surveys (7290)
# 
#   Nsubunits.yr = total number of plots sampled in 2006 and 2007 combined = 81 
# 
#   ny1 = number of groups associated with the annual survey in 2006 (year 1) = 960

scalar.dat <- as.data.frame(matrix(NA, 1, 3))
colnames(scalar.dat) <- c("R", "Ngroups", "Nsubunits.yr")

unique(oper.dat$h[oper.dat$yr==1])
unique(oper.dat$h[oper.dat$yr==2])

scalar.dat <- scalar.dat %>%
  mutate(R = as.double(nrow(sight.dat)),
         Ngroups = as.double(nrow(oper.dat)),
         Nsubunits.yr = as.double(nrow(plot.dat)),
         ny1 = as.double(nrow(oper.dat %>% filter(yr == 1))),
         nh1y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 1))),
         nh2y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 2))),
         nh3y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 3))),
         nh4y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 4))),
         nh6y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 6))),
         nh7y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 7))),
         nh8y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 8))),
         nh9y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 9))),
         nh10y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 10))),
         nh11y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 11))),
         nh16y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 16))),
         nh17y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 17))),
         nh19y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 19))),
         nh21y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 21))),
         nh22y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 22))),
         nh23y1 = as.double(nrow(oper.dat %>% filter(yr == 1, h == 23))),
         nh1y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 1))),
         nh2y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 2))),
         nh4y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 4))),
         nh5y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 5))),
         nh6y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 6))),
         nh12y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 12))),
         nh13y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 13))),
         nh14y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 14))),
         nh15y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 15))),
         nh18y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 18))),
         nh20y2 = as.double(nrow(oper.dat %>% filter(yr == 2, h == 20)))
         )

## 1.3.7 SAVE DATA ####
save(list = c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff"), file = "elk_bayesian.Rdata")
rm(list = ls())
