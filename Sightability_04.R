# Elk Sightability Analysis
# Step 4: Plotting and Reporting

# LOAD PACKAGES ####

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# LOAD DATA ####
setwd("C:/Users/TBRUSH/R/Elk_sightability/out")

load("jags_output.RData")
load("mHT_output.RData")

results_exp <- read_excel("C:/Users/TBRUSH/R/Elk_sightability/input/SurveyData_ SPRING_2022.xls", 
                          sheet = "2022 Summary", range = "A2:AH29")

# Expert estimates ####

results_exp <- results_exp %>%
  select(EPU=`...1`, y1=`Est., Population April 2021`, y2=`Est., Population April 2022`)

# mHT ####

## Year 1 plots ####

### All strata ####
mHT.all.2021$covariate <- "None"
mHT.all.2021$year <- "2021"
mHT.all.2021.voc$covariate <- "VOC"
mHT.all.2021.voc$year <- "2021"

mHT.all_y1 <- bind_rows(mHT.all.2021, mHT.all.2021.voc)

mHT.all_plot1 = ggplot(mHT.all_y1, aes(x=covariate, y=tau.hat))+
  geom_point(color="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab(expression(paste("2021"))) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(covariate, ymin =lcl, ymax = ucl)) +
  theme(axis.title.x= element_text(size=14)) +
  theme(axis.text.y = element_text(size=14))

mHT.all_plot1

### By strata ####
mHT.strat.2021$EPU <- row.names(mHT.strat.2021)
mHT.strat.2021$covariate <- "None"
mHT.strat.2021.voc$EPU <- row.names(mHT.strat.2021.voc)
mHT.strat.2021.voc$covariate <- "VOC"

mHT.strat_y1 <- bind_rows(mHT.strat.2021, mHT.strat.2021.voc) %>%
  arrange(EPU, covariate)

  mHT.strat_plot1 = ggplot(mHT.strat_y1, aes(x =EPU, y=tau.hat, color=covariate))+
    geom_point(shape=15, size=3, position=position_dodge(1))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(color="black")) +
    xlab(expression(paste("EPU"))) +
    ylab(expression(paste("Population Estimate ± 95 CI"))) +
    geom_linerange(aes(EPU, ymin =LCL, ymax = UCL), position=position_dodge(1)) +
    theme(axis.title.x= element_text(size=14), axis.text.x=element_text(size=12, angle = 90, color="black")) +
    theme(axis.text.y = element_text(size=14))
  mHT.strat_plot1

# voc may improve estimates -> go with
  
#   mHT.strat_plot1 = ggplot(mHT.strat.2021.voc, aes(x = ID, y=tau.hat))+
#     geom_point(shape=15, size=3)+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line()) +
#     xlab(expression(paste("EPU"))) +
#     ylab(expression(paste("Population Estimate ± 95 CI"))) +
#     geom_linerange(aes(ID, ymin =LCL, ymax = UCL)) +
#     theme(axis.title.x= element_text(size=14), axis.text.x = element_text(size=12, angle = 90, color="black")) +
#     theme(axis.text.y = element_text(size=12))
# mHT.strat_plot1

## Year 2 plots ####
mHT.all.2022$covariate <- "None"
mHT.all.2022$year <- "2022"
mHT.all.2022.voc$covariate <- "VOC"
mHT.all.2022.voc$year <- "2022"

mHT.all_y2 <- bind_rows(mHT.all.2022, mHT.all.2022.voc) 

mHT.all_plot2 = ggplot(mHT.all_y2, aes(x=covariate, y=tau.hat))+
  geom_point(shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab(expression(paste("2022"))) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(covariate, ymin =lcl, ymax = ucl)) +
  theme(axis.title.x= element_text(size=14)) +
  theme(axis.text.y = element_text(size=14))

mHT.all_plot2

### By strata ####
mHT.strat.2022$EPU <- row.names(mHT.strat.2022)
mHT.strat.2022$covariate <- "None"
mHT.strat.2022.voc$EPU <- row.names(mHT.strat.2022.voc)
mHT.strat.2022.voc$covariate <- "VOC"

mHT.strat_y2 <- bind_rows(mHT.strat.2022, mHT.strat.2022.voc) %>%
  arrange(EPU, covariate)

mHT.strat_plot2 = ggplot(mHT.strat_y2, aes(x =EPU, y=tau.hat, color=covariate))+
  geom_point(shape=15, size=3, position=position_dodge(.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line()) +
  xlab(expression(paste("EPU"))) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin =LCL, ymax = UCL), position=position_dodge(0.9)) +
  theme(axis.title.x= element_text(size=14), axis.text.x=element_text(size=12, angle = 90, color="black")) +
  theme(axis.text.y = element_text(size=14))
mHT.strat_plot2

# voc may improve estimates -> go with

# mHT.strat_plot1 = ggplot(mHT.strat.2021.voc, aes(x = ID, y=tau.hat))+
#   geom_point(shape=15, size=3)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line()) +
#   xlab(expression(paste("EPU"))) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(ID, ymin =LCL, ymax = UCL)) +
#   theme(axis.title.x= element_text(size=14), axis.text.x = element_text(size=12, angle = 90, color="black")) +
#   theme(axis.text.y = element_text(size=12))
# mHT.strat_plot1

## 2.7.2 Sechelt Peninsula ####
# 
# tau.Sechelt <- matrix(NA,6,3)
# i<- 1
# for(i in 1:3 ){
#   tau.Sechelt[1,i] <- mHT.strat.2016["Sechelt Peninsula", i]
#   tau.Sechelt[2,i] <- mHT.strat.2017["Sechelt Peninsula", i]
#   tau.Sechelt[3,i] <- mHT.strat.2018["Sechelt Peninsula", i]
#   tau.Sechelt[4,i] <- mHT.strat.2019["Sechelt Peninsula", i]
#   tau.Sechelt[5,i] <- mHT.strat.2020["Sechelt Peninsula", i]
#   tau.Sechelt[6,i] <- mHT.strat.2021["Sechelt Peninsula", i]
# }
# rownames(tau.Sechelt) <- 2016:2021
# colnames(tau.Sechelt) <- c("Estimate","LCL","UCL")
# tau.Sechelt <- as.data.frame(tau.Sechelt)
# 
# mHT.Sechelt_plot = ggplot(tau.Sechelt, aes(x = row.names(tau.Sechelt), y=Estimate))+
#   geom_point(colour="black", shape=15, size=3)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   xlab(expression(paste("Year"))) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(row.names(tau.Sechelt), ymin = LCL, ymax = UCL)) +
#   theme(axis.title.x= element_text(size=14)) +
#   theme(axis.text.y = element_text(size=14))
# mHT.Sechelt_plot
# 
# ggsave(mHT.Sechelt_plot, file="mHT.Sechelt_plot.PNG")

# JAGS ####

load("jags_effort.RData")

# 3.2 CLEAN OUTPUT ####

jags.summary <- as.data.frame(jags_output$BUGSoutput$summary)

tau.jags <- matrix(NA,26,6)
tau.jags <- as.data.frame(tau.jags)
tau.jags[,1] <- as.double(str_extract(jags_output$parameters.to.save[3:28] %>% sort(), "(?<=h)[:digit:]{1,2}"))
tau.jags[,2] <- str_extract(jags_output$parameters.to.save[3:28] %>% sort(), "(?<=y)[:digit:]")
tau.jags <- tau.jags %>% slice(c(1:11, 13:23, 26))
tau.jags[,3] <- round(jags.summary$`50%`[4:26])
tau.jags[,4] <- round(jags.summary$`2.5%`[4:26])
tau.jags[,5] <- round(jags.summary$`97.5%`[4:26])
tau.jags[,6] <- round(jags.summary$Rhat[4:26], 3)
colnames(tau.jags) <- c("ID", "year", "tau.hat","LCL","UCL","Rhat")
tau.jags <- left_join(tau.jags, eff[,c(1,4)], by="ID") %>%
  select(EPU, year:Rhat) %>%
  arrange(EPU, year)


library(writexl)
write_xlsx(tau.jags, "tau.jags.xlsx")

# 3.3 PLOT OUTPUT ####
## 2021 ####
tau.jags_y1 <- tau.jags %>% filter(year == 1) %>% select(-c(year, Rhat))
tau.jags_plot1 = ggplot(tau.jags_y1, aes(x = EPU, y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin = LCL, ymax = UCL)) +
  theme(axis.text.x=element_text(size=12, angle = 90)) +
  theme(axis.text.y = element_text(size=12))
tau.jags_plot1


## 2022 ####
tau.jags_y2 <- tau.jags %>% filter(year == 2) %>% select(-c(year, Rhat))
tau.jags_plot2 = ggplot(tau.jags_y2, aes(x = EPU, y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin = LCL, ymax = UCL)) +
  theme(axis.text.x=element_text(size=12, angle = 90)) +
  theme(axis.text.y = element_text(size=12))
tau.jags_plot2

# COMBINE PLOTS ####
## 2021 ####

mHT.2021 <- mHT.strat.2021 %>%
  select(EPU, tau.hat:UCL) %>%
  mutate(model = "mHT")
jags.2021 <- tau.jags_y1 %>%
  mutate(model = "Bayesian")
expert.2021<- results_exp %>%
  filter(EPU %in% jags.2021$EPU) %>%
  select(EPU, tau.hat=y1) %>%
  mutate(LCL = NA,
         UCL = NA,
         model = "None")
results_y1 <- bind_rows(mHT.2021, jags.2021, expert.2021)
row.names(results_y1) <- 1:42

results_plot1 = ggplot(results_y1, aes(x = EPU, y=tau.hat, color=model))+
  geom_point(shape=15, size=3, position = position_dodge(width = 0.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin = LCL, ymax = UCL), position = position_dodge(width = 0.9)) +
  theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_colour_grey(start = 0.2, end = 0.8)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_plot1

## 2022 ####

mHT.2022 <- mHT.strat.2022 %>%
  select(EPU, tau.hat:UCL) %>%
  mutate(model = "mHT")
jags.2022 <- tau.jags_y2 %>%
  mutate(model = "Bayesian")
expert.2022<- results_exp %>%
  filter(EPU %in% jags.2022$EPU) %>%
  select(EPU, tau.hat=y2) %>%
  mutate(LCL = NA,
         UCL = NA,
         model = "None")
results_y2 <- bind_rows(mHT.2022, jags.2022, expert.2022)
row.names(results_y2) <- 1:nrow(results_y2)

results_plot2 = ggplot(results_y2, aes(x = EPU, y=tau.hat, color=model))+
  geom_point(shape=15, size=3, position = position_dodge(width = 0.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin = LCL, ymax = UCL), position = position_dodge(width = 0.9)) +
  theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_colour_grey(start = 0.2, end = 0.8)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_plot2

# Agreeance measures ####
library(SimplyAgree)
a2021 = agree_test(x = mHT.2021$tau.hat,
                   y = jags.2021$tau.hat)
print(a2021)