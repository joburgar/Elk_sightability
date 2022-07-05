## LOAD PACKAGES ####
library("SightabilityModel")
library(tidyverse)
library(readxl)
library(rjags)
list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "scrbook", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

## LOAD DATA ####

dat.2021 <- read_excel("data/SurveyData_ SPRING_2022.xls", 
                       sheet = "2021 Survey Data", range = "A1:O136", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "text", "text", "numeric", "text", 
                                     "text"))
dat.2022 <- read_excel("data/SurveyData_ SPRING_2022.xls", 
                       sheet = "2022 Survey Data", range = "A1:P102", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "skip", "text", "text", "numeric", 
                                     "text", "text"))
EPU.areas <- read_csv("data/Effort/EPU_areas.csv",
                      col_types = cols(OID = col_skip(), Shape_Leng = col_skip()))
# Create list of EPUs surveyed
EPU.list <- as.character(EPU.areas$Unit)
print(EPU.list)

## CLEAN EFFORT DATA ####
# take effort data from summary pages

min.count <- read_excel("data/SurveyData_ SPRING_2022.xls", 
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
  mutate(surveyed_2021 = if_else(is.na(Min_2021), F, T),
         surveyed_2022 = if_else(is.na(Min_2022), F, T)) %>%
  select(EPU, surveyed_2021, surveyed_2022)
summary(eff)
# make sure numbers are as expected

## CLEAN OBSERVATION DATA ####

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

## GATHER SIGHTABILITY DATA ####
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

## SIGHT DAT ####

# 1.  sight_dat.csv = Sightability survey data:  64 records
# w = habitat indicator ()
# x = visual obstrcution measurements associated with the test trial data used to develop the sightability model
# y = activity indicator (0 if bedded, 1 if standing)
# z = detection indicator (1 if the group was observed, 0 otherwise)

sight.dat_Sechelt <- exp.tmp %>%
  mutate(a = as.double(activity),
         s = as.double(habitat),
         x.tilde = as.double(voc),
         z.tilde = as.double(if_else(survey_type=="Telemetry", 0, 1), .keep="none")) %>%
  select(w, x.tilde, y, z.tilde)
glimpse(sight.dat_Sechelt) # check - looks the same as Fieberg's sight_dat csv
summary(sight.dat_Sechelt)

## OPER DAT ####

#  2. oper_dat.csv:  Operational survey data:  4380 records
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

oper.dat <- obs %>%
  transmute(a = as.double(activity),
            s = as.double(habitat),
            x = voc,
            ym1 = total-1,
            h = stratum,
            q = 1,
            z = if_else(survey_type == "Telemetry", 0, 1),
            yr = if_else(year == 2021, 1, 2),
            subunits = 1
            ) %>%
  glimpse()
oper.dat_Sechelt <- oper.dat %>%
  filter(h == "Sechelt Peninsula") %>%
  mutate(h = 1,
         x = if_else(is.na(x), as.double(mean(x, na.rm = T)), as.double(x)))

# augmented data
# need to determine max m of each h
m.est <- oper.dat_Sechelt %>%
  group_by(yr) %>%
  summarize(m = n())
m.max <- max(m.est$m)
b.h <- 10*m.max

oper.dat.join_Sechelt <-  m.est %>%
  mutate (B_minus_m = b.h-m)
oper.dat.aug_Sechelt <- oper.dat.join_Sechelt[rep(1:nrow(oper.dat.join_Sechelt), oper.dat.join_Sechelt$B_minus_m),] %>%
  mutate(a = NA, s = NA, x = NA, ym1 = NA, h = 1, q = NA, z = 0, yr = yr, subunits = 1, .keep="none") %>%
  ungroup()

oper.dat_Sechelt <- rbind(oper.dat_Sechelt, oper.dat.aug_Sechelt) %>%
  arrange(yr, q)

glimpse(oper.dat_Sechelt) # check

## PLOT DAT ####

# 3.  plot_dat.csv:  Plot-level information data: 77 records (one for each of the plots sampled in either 2006 or 2007)
# h.plots = stratum to which the plot belonged (1, 2, 3 correspond to low, medium and high density strata)
# yr.plots = year the plot was sampled (1 = 2006, 2= 2007)
plot.dat_Sechelt <- oper.dat_Sechelt %>%
  select(yr, subunits) %>%
  distinct() %>%
  mutate(h.plots = 1, 
         yr.plots = yr) %>%
  select(h.plots, yr.plots)
glimpse(plot.dat_Sechelt)
# 4.  scalar_dat.csv:  
#   Scalars:
#   R = number of sightability trials (64)
# 
#   Ngroups = number of observed and augmented groups for the 2006 and 2007 annual operational surveys (7290)
# 
#   Nsubunits.yr = total number of plots sampled in 2006 and 2007 combined = 81 
# 
#   ny1 = number of groups associated with the annual survey in 2006 (year 1) = 960

scalar.dat_Sechelt <- as.data.frame(matrix(NA, 1, 4))
colnames(scalar.dat_Sechelt) <- c("R", "Ngroups", "Nsubunits.yr", "ny1")

scalar.dat_Sechelt <- scalar.dat_Sechelt %>%
  mutate(R = as.double(nrow(sight.dat_Sechelt)),
         Ngroups = as.double(nrow(oper.dat_Sechelt)),
         Nsubunits.yr = as.double(nrow(plot.dat_Sechelt)),
         ny1 = as.double(nrow(oper.dat_Sechelt[oper.dat_Sechelt$yr==1,]))
  )

## RUN MODEL ####

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("tau.samp1", "tau.samp2", "bo", "bvoc")

# MCMC settings
ni <- 40000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 20000 # build to 20000
nc <- 3

# Bundle data
bundle.dat_Sechelt <- list(x.tilde=sight.dat_Sechelt$x.tilde, z.tilde=sight.dat_Sechelt$z.tilde, #sight.dat
                           a=oper.dat_Sechelt$a, s=oper.dat_Sechelt$s, x=oper.dat_Sechelt$x+.000001, ym1=oper.dat_Sechelt$ym1, h=oper.dat_Sechelt$h, q=oper.dat_Sechelt$q, z=oper.dat_Sechelt$z, yr=oper.dat_Sechelt$yr, subunits=oper.dat_Sechelt$subunits, # oper_dat
                   h.plots=plot.dat_Sechelt$h.plots, yr.plots=plot.dat_Sechelt$yr.plots, # plot_dat
                   R=scalar.dat_Sechelt$R, Ngroups=scalar.dat_Sechelt$Ngroups, Nsubunits.yr=scalar.dat_Sechelt$Nsubunits.yr, ny1=scalar.dat_Sechelt$ny1) # scalar_dat

# Run model
jags_output_Sechelt <- jags(bundle.dat_Sechelt, inits, params, "beta_binom_model_elk2022.txt", nc, ni, nb, nt)
jags_output_Sechelt
summary(jags_output_Sechelt)