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


# 2021
tau.hats <- matrix(NA, length(eff$ID[eff$year == 2021]), 7)
rownames(tau.hats) <- c(eff$Unit[eff$year == 2021])
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
colnames(tau.hats) <- c(names(temp$est), "lcl", "ucl")
tau.hats <- round(tau.hats, 0)
mHT.2021 <-
  print(format(tau.hats, big.mark = ","),
        justify = "left",
        quote = FALSE)
mHT.2021 <- as.data.frame(tau.hats) %>%
  select(tau.hat, lcl:ucl, everything()) %>%
  mutate(sd = round(sqrt(VarTot)),
         cv = sd/tau.hat)

# 2022
tau.hats <- matrix(NA, length(eff$ID[eff$year == 2022]), 7)
rownames(tau.hats) <- c(eff$Unit[eff$year == 2022])
for (i in (length(eff$ID[eff$year == 2021])+1):length(eff$ID)) {
  tempsamp <- sampinfo[sampinfo$year == 2022 & sampinfo$stratum == i,]
  tempobs <- obs[obs$year == 2022 & obs$stratum == i,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.05,
              Vm.boot = TRUE,
              nboot = 1000)
  temp.summary <- summary(temp)
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 1:5] <- temp$est
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[(i-length(eff$ID[eff$year == 2021])), 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "lcl", "ucl")
tau.hats <- round(tau.hats, 0)
mHT.2022 <-
  print(format(tau.hats, big.mark = ","),
        justify = "left",
        quote = FALSE)
mHT.2022 <- as.data.frame(tau.hats) %>%
  select(tau.hat, lcl:ucl, everything()) %>%
  mutate(sd = round(sqrt(VarTot)),
         cv = sd/tau.hat)

# SAVE AND CLEAR ENVIRONMENT ####
setwd("C:/Users/TBRUSH/R/Elk_sightability/out")

save(
  list = c(
    "mHT.2021",
    "mHT.2022"
  ),
  file = "mHT_output.RData"
)

rm(list = ls())

