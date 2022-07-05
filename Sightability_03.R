# Elk Sightability Analysis
# Step 3: Bayesian Analysis

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")
load("elk_bayesian.Rdata")

library("SightabilityModel")
library(tidyverse)
library(readxl)
library(rjags)
list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "scrbook", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 3.1 RUN MODEL ####

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("bo", "bvoc", "tau.nh1y1", "tau.nh2y1", "tau.nh3y1", "tau.nh4y1", "tau.nh6y1", "tau.nh7y1", "tau.nh8y1", "tau.nh9y1", "tau.nh10y1", "tau.nh11y1", "tau.nh16y1", "tau.nh17y1", "tau.nh19y1", "tau.nh21y1", "tau.nh22y1", "tau.nh23y1", "tau.nh1y2", "tau.nh2y2", "tau.nh4y2", "tau.nh5y2", "tau.nh6y2", "tau.nh12y2", "tau.nh13y2", "tau.nh14y2", "tau.nh15y2", "tau.nh18y2")

# MCMC settings
ni <- 10000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 5000 # build to 20000
nc <- 3

# Bundle data
bundle.dat <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                   x=oper.dat$x+.000001, ym1=oper.dat$ym1, h=oper.dat$h, q=oper.dat$q, z=oper.dat$z, yr=oper.dat$yr, subunits=oper.dat$subunits, # oper_dat
                   h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot_dat
                   R=scalar.dat$R, Ngroups=scalar.dat$Ngroups, Nsubunits.yr=scalar.dat$Nsubunits.yr, ny1=scalar.dat$ny1, nh1y1=scalar.dat$nh1y1, nh2y1=scalar.dat$nh2y1, nh3y1=scalar.dat$nh3y1, nh4y1=scalar.dat$nh4y1, nh6y1=scalar.dat$nh6y1, nh7y1=scalar.dat$nh7y1, nh8y1=scalar.dat$nh8y1, nh9y1=scalar.dat$nh9y1, nh10y1=scalar.dat$nh10y1, nh11y1=scalar.dat$nh11y1, nh16y1=scalar.dat$nh16y1, nh17y1=scalar.dat$nh17y1, nh19y1=scalar.dat$nh19y1, nh21y1=scalar.dat$nh21y1, nh22y1=scalar.dat$nh22y1, nh23y1=scalar.dat$nh23y1, nh1y2=scalar.dat$nh1y2, nh2y2=scalar.dat$nh2y2, nh4y2=scalar.dat$nh4y2, nh5y2=scalar.dat$nh5y2, nh6y2=scalar.dat$nh6y2, nh12y2=scalar.dat$nh12y2, nh13y2=scalar.dat$nh13y2, nh14y2=scalar.dat$nh14y2, nh15y2=scalar.dat$nh15y2, nh18y2=scalar.dat$nh18y2) # scalar_dat

# Run model
jags_output <- jags(bundle.dat, inits, params, "beta_binom_model_elk2022.txt", nc, ni, nb, nt)

save("jags_output",file="jags_output.RData")

load("jags_output.Rdata")

jags_output

tau.matrix <- bind_cols(yr = bundle.dat$yr, h = bundle.dat$h, tau = jags_output$BUGSoutput$mean$taus) %>%
  group_by(h, yr) %>%
  summarize(tau.samp = sum(tau))

# 3.2 CLEAN OUTPUT ####

jags.summary <- as.data.frame(jags_output$BUGSoutput$summary)

tau.jags <- matrix(NA,6,5)
tau.jags[,1] <- c(2016, 2017, 2018, 2019, 2020, 2021)
count <- 1
tau.jags[count,2] <- round(jags.summary$mean[count+3])
tau.jags[count,3] <- round(jags.summary$`2.5%`[count+3])
tau.jags[count,4] <- round(jags.summary$`97.5%`[count+3])
tau.jags[count,5] <- round(jags.summary$Rhat[count+3], 2)
count <- count + 1
for(i in 1:1){
  tau.jags[count,2] <- round(jags.summary$mean[count+3])
  tau.jags[count,3] <- round(jags.summary$`2.5%`[count+3])
  tau.jags[count,4] <- round(jags.summary$`97.5%`[count+3])
  tau.jags[count,5] <- round(jags.summary$Rhat[count+3], 2)
}
count <- count + 1
for(i in 1:1){
  tau.jags[count,2] <- round(jags.summary$mean[count+3])
  tau.jags[count,3] <- round(jags.summary$`2.5%`[count+3])
  tau.jags[count,4] <- round(jags.summary$`97.5%`[count+3])
  tau.jags[count,5] <- round(jags.summary$Rhat[count+3], 2)
}
count <- count + 1
for(i in 1:1){
  tau.jags[count,2] <- round(jags.summary$mean[count+3])
  tau.jags[count,3] <- round(jags.summary$`2.5%`[count+3])
  tau.jags[count,4] <- round(jags.summary$`97.5%`[count+3])
  tau.jags[count,5] <- round(jags.summary$Rhat[count+3], 2)
}
count <- count + 1
for(i in 1:1){
  tau.jags[count,2] <- round(jags.summary$mean[count+3])
  tau.jags[count,3] <- round(jags.summary$`2.5%`[count+3])
  tau.jags[count,4] <- round(jags.summary$`97.5%`[count+3])
  tau.jags[count,5] <- round(jags.summary$Rhat[count+3], 2)
}
count <- count + 1
for(i in 1:1){
  tau.jags[count,2] <- round(jags.summary$mean[count+3])
  tau.jags[count,3] <- round(jags.summary$`2.5%`[count+3])
  tau.jags[count,4] <- round(jags.summary$`97.5%`[count+3])
  tau.jags[count,5] <- round(jags.summary$Rhat[count+3], 2)
}

tau.jags
colnames(tau.jags) <- c("year", "mean","LCL","UCL","Rhat")
tau.jags <- as.data.frame(tau.jags)


# 3.3 PLOT OUTPUT ####
tau.Sechelt50_jags_simsplot = ggplot(tau.Sechelt50_jags, aes(x = reorder(row.names(tau.Sechelt50_jags),mean), y=mean))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt50_jags), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
tau.Sechelt50_jags_simsplot
ggsave(tau.Sechelt50_jags_simsplot, file="out/tau.Sechelt50_jags_simsplot.PNG")


tau.Sechelt50_jags_sub <- tau.Sechelt50_jags %>% filter(mean <300) # only 15 / 46 with pop estimates < 300
tau.Sechelt50_jags_sub_simsplot = ggplot(tau.Sechelt50_jags_sub, aes(x = reorder(row.names(tau.Sechelt50_jags_sub),mean), y=mean))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(row.names(tau.Sechelt50_jags_sub), ymin = LCL, ymax = UCL)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
tau.Sechelt50_jags_sub_simsplot
ggsave(tau.Sechelt50_jags_simsplot, file="out/tau.Sechelt50_jags_simsplot.PNG")


tau.Sechelt50$model <- "mHT"
tau.Sechelt50$simID <- row.names(tau.Sechelt50)
names(tau.Sechelt50)
tau.Sechelt50_jags$model <- "Bayesian"
names(tau.Sechelt50_jags)
tau.Sechelt50_jags$simID <- row.names(tau.Sechelt50_jags)

Sechelt50 <- rbind(tau.Sechelt50, tau.Sechelt50_jags %>% select(-Rhat) %>% rename(tau.hat=mean))


col.cat <- as.character(c("#2028B2","#B2AA20"))
Sechelt50_sub <- Sechelt50 %>% filter(tau.hat<300)

Sechelt50_sub_simsplot = Sechelt50_sub %>%
  ggplot(aes(x = reorder(simID,tau.hat), y=tau.hat, fill=model))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(simID, ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))
Sechelt50_sub_simsplot
ggsave(Sechelt50_sub_simsplot, file="out/Sechelt50_both_sub_simsplot.PNG")

Sechelt50_simsplot = Sechelt50 %>%
  ggplot(aes(x = reorder(rownames(Sechelt50),tau.hat), y=tau.hat, fill=model))+
  geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
  scale_fill_manual(values=unique(col.cat)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(rownames(Sechelt50), ymin = LCL, ymax = UCL), position=position_dodge(width=1)) +
  geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(axis.text.y = element_text(size=14))+
  ylim(c(0,20000))
Sechelt50_simsplot
ggsave(Sechelt50_simsplot, file="out/Sechelt50_both_simsplot_simsplot.PNG")
