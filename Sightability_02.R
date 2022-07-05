# Elk Sightability Analysis
# Step 2: Modified Horowitz-Thompson Analysis

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")
load("elk_mHT.Rdata")

# 2.1 - 2016 ####

# All data
est.2016 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2016), sdat = exp1,
                      sampinfo = subset(sampinfo, year == 2016))  
results.all.2016 <-print(est.2016)

# By strata
tau.hats <- matrix(NA, length(eff.2016$ID), 7)
rownames(tau.hats) <- c(eff.2016$Unit)
for(i in 1:length(eff.2016$ID)){
  tempsamp <- sampinfo[sampinfo$year==2016 & sampinfo$stratum==i, ]
  tempobs <- obs[obs$year == 2016 & obs$stratum == i, ]
  temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2016 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2016 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())

# 2.2 - 2017 ####

# All data
est.2017 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2017), sdat = exp1,
                      sampinfo = subset(sampinfo, year == 2017))  
results.all.2017 <-print(est.2017)

# By strata
tau.hats <- matrix(NA, length(eff.2017$ID), 7)
rownames(tau.hats) <- c(eff.2017$Unit)
for(i in 1:length(eff.2017$ID)){
  tempsamp <- sampinfo[sampinfo$year==2017 & sampinfo$stratum==i, ]
  tempobs <- obs[obs$year == 2017 & obs$stratum == i, ]
  temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2017 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2017 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())


# 2.3 - 2018 ####

# All data
est.2018 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2018), sdat = exp1,
                      sampinfo = subset(sampinfo, year == 2018))  
results.all.2018 <-print(est.2018)

# By strata
tau.hats <- matrix(NA, length(eff.2018$ID), 7)
rownames(tau.hats) <- c(eff.2018$Unit)
for(i in 1:length(eff.2018$ID)){
  tempsamp <- sampinfo[sampinfo$year==2018 & sampinfo$stratum==i, ]
  tempobs <- obs[obs$year == 2018 & obs$stratum == i, ]
  temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2018 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2018 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())


# 2.4 - 2019 ####
# Keep in mind had to remove observations b/c lost effort data for Mar 29
# All data
est.2019 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2019), sdat = exp1,
                      sampinfo = subset(sampinfo, year == 2019))  
results.all.2019 <-print(est.2019)

# By strata
tau.hats <- matrix(NA, length(eff.2019$ID), 7)
rownames(tau.hats) <- c(eff.2019$Unit)
for(i in 1:length(eff.2019$ID)){
  tempsamp <- sampinfo[sampinfo$year==2019 & sampinfo$stratum==i, ]
  tempobs <- obs[obs$year == 2019 & obs$stratum == i, ]
  temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2019 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2019 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())


# 2.5 - 2020 ####
# All data
est.2020 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2020), sdat = exp1,
                      sampinfo = subset(sampinfo, year == 2020))  
results.all.2020 <-print(est.2020)

# By strata
tau.hats <- matrix(NA, length(eff.2020$ID), 7)
rownames(tau.hats) <- c(eff.2020$Unit)
for(i in 1:length(eff.2020$ID)){
  tempsamp <- sampinfo[sampinfo$year==2020 & sampinfo$stratum==i, ]
  tempobs <- obs[obs$year == 2020 & obs$stratum == i, ]
  temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2020 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2020 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())

# 2.6 - 2021 ####
# All data
est.2021 <- Sight.Est(observed ~ 1, odat = subset(obs,year == 2021), sdat = exp1,
                      sampinfo = subset(sampinfo, year == 2021))  
results.all.2021 <- print(est.2021)
summary(est.2021)

# By strata
tau.hats <- matrix(NA, length(eff.2021$ID), 7)
rownames(tau.hats) <- c(eff.2021$Unit)
for(i in 1:length(eff.2021$ID)){
  tempsamp <- sampinfo[sampinfo$year==2021 & sampinfo$stratum==i, ]
  tempobs <- obs[obs$year == 2021 & obs$stratum == i, ]
  temp <- Sight.Est(observed ~ 1, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2021 <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2021 <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())


## 2.6.1 With VOC ####


# remove any NAs in voc
obs.voc <- obs %>%
  filter(!is.na(voc))
# All data
est.2021.voc <- Sight.Est(observed ~ voc, odat = subset(obs.voc,year == 2021), sdat = exp1,
                          sampinfo = subset(sampinfo, year == 2021))  
results.all.2021.voc <-print(est.2021.voc)

# By strata
tau.hats <- matrix(NA, length(eff.2021$ID), 7)
rownames(tau.hats) <- c(eff.2021$Unit)
for(i in 1:length(eff.2021$ID)){
  tempsamp <- sampinfo[sampinfo$year==2021 & sampinfo$stratum==i, ]
  tempobs <- obs.voc[obs.voc$year == 2021 & obs.voc$stratum == i, ]
  temp <- Sight.Est(observed ~ voc, odat = tempobs, sdat = exp1, tempsamp)
  temp.summary <- summary(temp)
  tau.hats[i,1:5] <- temp$est
  tau.hats[i,6] <- as.numeric(gsub(",","",temp.summary$lcl))
  tau.hats[i,7] <- as.numeric(gsub(",","",temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "LCL", "UCL")
tau.hats<-round(tau.hats, 0)
results.strat.2021.voc <- print(format(tau.hats, big.mark = ","), justify = "left", quote = FALSE)
results.strat.2021.voc <- as.data.frame(tau.hats) %>%
  select(tau.hat, LCL:UCL, everything())

# 2.7 Summary plots ####

## 2.7.1 Sechelt Peninsula ####

tau.Sechelt <- matrix(NA,6,3)
i<- 1
for(i in 1:3 ){
  tau.Sechelt[1,i] <- results.strat.2016["Sechelt Peninsula", i]
  tau.Sechelt[2,i] <- results.strat.2017["Sechelt Peninsula", i]
  tau.Sechelt[3,i] <- results.strat.2018["Sechelt Peninsula", i]
  tau.Sechelt[4,i] <- results.strat.2019["Sechelt Peninsula", i]
  tau.Sechelt[5,i] <- results.strat.2020["Sechelt Peninsula", i]
  tau.Sechelt[6,i] <- results.strat.2021["Sechelt Peninsula", i]
}
rownames(tau.Sechelt) <- 2016:2021
colnames(tau.Sechelt) <- c("Estimate","LCL","UCL")
tau.Sechelt <- as.data.frame(tau.Sechelt)

Sechelt_plot = ggplot(tau.Sechelt, aes(x = row.names(tau.Sechelt), y=Estimate))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab(expression(paste("Year"))) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt), ymin = LCL, ymax = UCL)) +
  theme(axis.title.x= element_text(size=14)) +
  theme(axis.text.y = element_text(size=14))
Sechelt_plot


rm(list=ls()) 
