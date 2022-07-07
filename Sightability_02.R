# Elk Sightability Analysis
# Step 2: Modified Horowitz-Thompson Analysis

list.of.packages <-
  c(
    "tidyverse",
    "lubridate",
    "chron",
    "bcdata",
    "bcmaps",
    "sf",
    "rgdal",
    "readxl",
    "Cairo",
    "rjags",
    "coda",
    "OpenStreetMap",
    "ggmap",
    "SightabilityModel",
    "truncnorm",
    "doParallel",
    "nimble",
    "xtable",
    "statip",
    "R2jags"
  )
# Check you have them and load them

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")
load("mHT_input.Rdata")

# # 2.1 - 2016 ####
#
# # All data
# est.2016 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2016), sdat = exp,
#                       sampinfo = subset(sampinfo, year == 2016))
# results.all.2016 <-summary(est.2016)
#
# # By strata
# tau.hats <- matrix(NA, length(eff.2016$ID), 7)
# rownames(tau.hats) <- c(eff.2016$Unit)
# for(i in 1:length(eff.2016$ID)){
#   tempsamp <- sampinfo[sampinfo$year==2016 & sampinfo$stratum==i, ]
#   tempobs <- obs[obs$year == 2016 & obs$stratum == i, ]
#   temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
#   temp.summary <- summary(temp)
#   tau.hats[i,1:5] <- temp$est
#   tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
#   tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
# }
# colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
# tau.hats<-round(tau.hats, 0)
# results.strat.2016 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
# results.strat.2016 <- as.data.frame(tau.hats) %>%
#   select(tau.hat, LCL:UCL, everything())
#
# # 2.2 - 2017 ####
#
# # All data
# est.2017 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2017), sdat = exp,
#                       sampinfo = subset(sampinfo, year == 2017))
# results.all.2017 <-summary(est.2017)
#
# # By strata
# tau.hats <- matrix(NA, length(eff.2017$ID), 7)
# rownames(tau.hats) <- c(eff.2017$Unit)
# for(i in 1:length(eff.2017$ID)){
#   tempsamp <- sampinfo[sampinfo$year==2017 & sampinfo$stratum==i, ]
#   tempobs <- obs[obs$year == 2017 & obs$stratum == i, ]
#   temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
#   temp.summary <- summary(temp)
#   tau.hats[i,1:5] <- temp$est
#   tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
#   tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
# }
# colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
# tau.hats<-round(tau.hats, 0)
# results.strat.2017 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
# results.strat.2017 <- as.data.frame(tau.hats) %>%
#   select(tau.hat, LCL:UCL, everything())
#
#
# # 2.3 - 2018 ####
#
# # All data
# est.2018 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2018), sdat = exp,
#                       sampinfo = subset(sampinfo, year == 2018))
# results.all.2018 <-summary(est.2018)
#
# # By strata
# tau.hats <- matrix(NA, length(eff.2018$ID), 7)
# rownames(tau.hats) <- c(eff.2018$Unit)
# for(i in 1:length(eff.2018$ID)){
#   tempsamp <- sampinfo[sampinfo$year==2018 & sampinfo$stratum==i, ]
#   tempobs <- obs[obs$year == 2018 & obs$stratum == i, ]
#   temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
#   temp.summary <- summary(temp)
#   tau.hats[i,1:5] <- temp$est
#   tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
#   tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
# }
# colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
# tau.hats<-round(tau.hats, 0)
# results.strat.2018 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
# results.strat.2018 <- as.data.frame(tau.hats) %>%
#   select(tau.hat, LCL:UCL, everything())
#
#
# # 2.4 - 2019 ####
# # Keep in mind had to remove observations b/c lost effort data for Mar 29
# # All data
# est.2019 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2019), sdat = exp,
#                       sampinfo = subset(sampinfo, year == 2019))
# results.all.2019 <-summary(est.2019)
#
# # By strata
# tau.hats <- matrix(NA, length(eff.2019$ID), 7)
# rownames(tau.hats) <- c(eff.2019$Unit)
# for(i in 1:length(eff.2019$ID)){
#   tempsamp <- sampinfo[sampinfo$year==2019 & sampinfo$stratum==i, ]
#   tempobs <- obs[obs$year == 2019 & obs$stratum == i, ]
#   temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
#   temp.summary <- summary(temp)
#   tau.hats[i,1:5] <- temp$est
#   tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
#   tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
# }
# colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
# tau.hats<-round(tau.hats, 0)
# results.strat.2019 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
# results.strat.2019 <- as.data.frame(tau.hats) %>%
#   select(tau.hat, LCL:UCL, everything())
#
#
# # 2.5 - 2020 ####
# # All data
# est.2020 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2020), sdat = exp,
#                       sampinfo = subset(sampinfo, year == 2020))
# results.all.2020 <-summary(est.2020)
#
# # By strata
# tau.hats <- matrix(NA, length(eff.2020$ID), 7)
# rownames(tau.hats) <- c(eff.2020$Unit)
# for(i in 1:length(eff.2020$ID)){
#   tempsamp <- sampinfo[sampinfo$year==2020 & sampinfo$stratum==i, ]
#   tempobs <- obs[obs$year == 2020 & obs$stratum == i, ]
#   temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
#   temp.summary <- summary(temp)
#   tau.hats[i,1:5] <- temp$est
#   tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
#   tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
# }
# colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
# tau.hats<-round(tau.hats, 0)
# results.strat.2020 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
# results.strat.2020 <- as.data.frame(tau.hats) %>%
#   select(tau.hat, LCL:UCL, everything())

# 2.6 - 2021 ####
# All data
est.2021 <-
  Sight.Est(
    observed ~ 1,
    odat = subset(obs, year == 2021),
    sdat = exp,
    sampinfo = subset(sampinfo, year == 2021)
  )
mHT.all.2021 <- summary(est.2021)

# By strata
tau.hats <- matrix(NA, length(eff.2021$ID), 7)
rownames(tau.hats) <- c(eff.2021$Unit)
for (i in 1:length(eff.2021$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2021 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2021 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats <- round(tau.hats, 0)
mHT.strat.2021 <-
  print(format(tau.hats, big.mark = ","),
        justify = "left",
        quote = FALSE)
mHT.strat.2021 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())


## 2.6.1 With VOC ####


# remove any NAs in voc
obs.voc <- obs %>%
  filter(!is.na(voc))
# All data
est.2021.voc <-
  Sight.Est(
    observed ~ voc,
    odat = subset(obs.voc, year == 2021),
    sdat = exp,
    sampinfo = subset(sampinfo, year == 2021)
  )
mHT.all.2021.voc <- summary(est.2021.voc)

# By strata
tau.hats <- matrix(NA, length(eff.2021$ID), 7)
rownames(tau.hats) <- c(eff.2021$Unit)
for (i in 1:length(eff.2021$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2021 & sampinfo$stratum == i,]
  tempobs <- obs.voc[obs.voc$year == 2021 & obs.voc$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats <- round(tau.hats, 0)
mHT.strat.2021.voc <-
  print(format(tau.hats, big.mark = ","),
        justify = "left",
        quote = FALSE)
mHT.strat.2021.voc <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())

# 2.7 - 2022 ####
# All data
est.2022 <-
  Sight.Est(
    observed ~ 1,
    odat = subset(obs, year == 2022),
    sdat = exp,
    sampinfo = subset(sampinfo, year == 2022)
  )
mHT.all.2022 <- summary(est.2022)

# By strata
tau.hats <- matrix(NA, length(eff.2022$ID), 7)
rownames(tau.hats) <- c(eff.2022$Unit)
for (i in 1:length(eff.2022$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2022 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2022 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ 1, odat = tempobs, sdat = exp, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats <- round(tau.hats, 0)
mHT.strat.2022 <-
  print(format(tau.hats, big.mark = ","),
        justify = "left",
        quote = FALSE)
mHT.strat.2022 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())


## 2.7.1 With VOC ####


# remove any NAs in voc
obs.voc <- obs %>%
  filter(!is.na(voc))
# All data
est.2022.voc <-
  Sight.Est(
    observed ~ voc,
    odat = subset(obs.voc, year == 2022),
    sdat = exp,
    sampinfo = subset(sampinfo, year == 2022)
  )
mHT.all.2022.voc <- summary(est.2022.voc)

# By strata
tau.hats <- matrix(NA, length(eff.2022$ID), 7)
rownames(tau.hats) <- c(eff.2022$Unit)
for (i in 1:length(eff.2022$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2022 & sampinfo$stratum == i,]
  tempobs <- obs.voc[obs.voc$year == 2022 & obs.voc$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats <- round(tau.hats, 0)
mHT.strat.2022.voc <-
  print(format(tau.hats, big.mark = ","),
        justify = "left",
        quote = FALSE)
mHT.strat.2022.voc <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())

setwd("C:/Users/TBRUSH/R/Elk_sightability/out")

save(
  list = c(
    "mHT.all.2021",
    "mHT.strat.2021",
    "mHT.all.2021.voc",
    "mHT.strat.2021.voc",
    "mHT.all.2022",
    "mHT.strat.2022",
    "mHT.all.2022.voc",
    "mHT.strat.2022.voc"
  ),
  file = "mHT_output.RData"
)

rm(list = ls())

