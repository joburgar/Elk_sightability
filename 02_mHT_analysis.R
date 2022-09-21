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

# setwd("C:/Users/TBRUSH/R/Elk_sightability/input")
load("input/mHT_input.Rdata")


# 2021
tau.hats <- matrix(NA, length(eff$ID[eff$year == 2021]), 7)

for (i in 1:length(eff$ID[eff$year == 2021])) {
  tempsamp <- sampinfo[sampinfo$year == 2021 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2021 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.05,
              Vm.boot = TRUE,
              nboot = 10000)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "lcl_95", "ucl_95")
tau.hats <- round(tau.hats, 0)

mHT_y1 <- as.data.frame(tau.hats) %>%
  mutate(EPU = as.character(eff$Unit[eff$year == 2021])) %>%
  select(EPU, tau.hat, lcl_95:ucl_95, everything()) %>%
  mutate(sd = round(sqrt(VarTot)),
         cv = sd/tau.hat) %>%
  arrange(EPU)

# Get 50% intervals
tau.hats <- matrix(NA, length(eff$ID[eff$year == 2021]), 7)

for (i in 1:length(eff$ID[eff$year == 2021])) {
  tempsamp <- sampinfo[sampinfo$year == 2021 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2021 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.5,
              Vm.boot = TRUE,
              nboot = 10000)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "lcl_50", "ucl_50")
tau.hats <- round(tau.hats, 0) %>%
  as.data.frame()

mHT_y1 <- bind_cols(mHT_y1, tau.hats[,c(6:7)])


# 2022
tau.hats <- matrix(NA, length(eff$ID[eff$year == 2022]), 7)

for (i in (length(eff$ID[eff$year == 2021])+1):length(eff$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2022 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2022 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.05,
              Vm.boot = TRUE,
              nboot = 10000)
  temp.summary <- summary(temp)
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 1:5] <- temp$est
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "lcl_95", "ucl_95")
tau.hats <- round(tau.hats, 0)
mHT_y2 <- as.data.frame(tau.hats) %>%
  mutate(EPU = eff$Unit[eff$year == 2022]) %>%
  select(EPU, tau.hat, lcl_95:ucl_95, everything()) %>%
  mutate(sd = round(sqrt(VarTot)),
         cv = sd/tau.hat) %>%
  arrange(EPU)

# Get 50% intervals
tau.hats <- matrix(NA, length(eff$ID[eff$year == 2022]), 7)

for (i in (length(eff$ID[eff$year == 2021])+1):length(eff$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2022 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2022 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.5,
              Vm.boot = TRUE,
              nboot = 10000)
  temp.summary <- summary(temp)
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 1:5] <- temp$est
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "lcl_50", "ucl_50")
tau.hats <- round(tau.hats, 0) %>%
  as.data.frame()

mHT_y2 <- bind_cols(mHT_y2, tau.hats[,c(6:7)])

# SAVE AND CLEAR ENVIRONMENT ####
save(list = c("mHT_y1","mHT_y2"),file = "out/mHT_output.RData")

rm(list = ls())

