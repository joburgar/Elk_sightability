# Elk Sightability Analysis
# Step 4: Plotting and Reporting

# LOAD PACKAGES ####

list.of.packages <- c("bayesplot", "tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
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
  select(EPU=`...1`, y1=`Est., Population April 2021`, y2=`Est., Population April 2022`) %>%
  arrange(EPU)

# mHT ####
# cv measure
mHT.cv <- matrix(NA, 2, 3)
mHT.cv[,1] <- c(2021, 2022)
mHT.cv[,2] <- c(median(mHT.2021.voc$cv), median(mHT.2022.voc$cv))
mHT.cv[,3] <- c(mean(mHT.2021.voc$cv), mean(mHT.2022.voc$cv))

colnames(mHT.cv) <- c("year", "median_cv", "mean_cv")
mHT.cv <- as.data.frame(mHT.cv)

## Year 1 plots ####

### All strata ####
# mHT.all.2021$covariate <- "None"
# mHT.all.2021$year <- "2021"
# mHT.all.2021.voc$covariate <- "VOC"
# mHT.all.2021.voc$year <- "2021"
# 
# mHT.all_y1 <- bind_rows(mHT.all.2021, mHT.all.2021.voc)
# 
# mHT.all_plot1 = ggplot(mHT.all_y1, aes(x=covariate, y=tau.hat))+
#   geom_point(color="black", shape=15, size=3)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   xlab(expression(paste("2021"))) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(covariate, ymin =lcl, ymax = ucl)) +
#   theme(axis.title.x= element_text(size=14)) +
#   theme(axis.text.y = element_text(size=14))
# 
# mHT.all_plot1

### By strata ####
mHT.2021$EPU <- row.names(mHT.2021)
mHT.2021$covariate <- "None"
mHT.2021.voc$EPU <- row.names(mHT.2021.voc)
mHT.2021.voc$covariate <- "VOC"

mHT.y1 <- bind_rows(mHT.2021, mHT.2021.voc) %>%
  arrange(EPU, covariate)

  mHT.plot1 = ggplot(mHT.y1, aes(x =EPU, y=tau.hat, color=covariate))+
    geom_point(shape=15, size=3, position=position_dodge(1))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(color="black")) +
    xlab(expression(paste("EPU"))) +
    ylab(expression(paste("Population Estimate ± 95 CI"))) +
    geom_linerange(aes(EPU, ymin =lcl, ymax = ucl), position=position_dodge(1)) +
    theme(axis.title.x= element_text(size=14), axis.text.x=element_text(size=12, angle = 90, color="black")) +
    theme(axis.text.y = element_text(size=14))
  mHT.plot1

## Year 2 plots ####
# mHT.all.2022$covariate <- "None"
# mHT.all.2022$year <- "2022"
# mHT.all.2022.voc$covariate <- "VOC"
# mHT.all.2022.voc$year <- "2022"
# 
# mHT.all_y2 <- bind_rows(mHT.all.2022, mHT.all.2022.voc) 
# 
# mHT.all_plot2 = ggplot(mHT.all_y2, aes(x=covariate, y=tau.hat))+
#   geom_point(shape=15, size=3)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   xlab(expression(paste("2022"))) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(covariate, ymin =lcl, ymax = ucl)) +
#   theme(axis.title.x= element_text(size=14)) +
#   theme(axis.text.y = element_text(size=14))
# 
# mHT.all_plot2

### By strata ####
mHT.2022$EPU <- row.names(mHT.2022)
mHT.2022$covariate <- "None"
mHT.2022.voc$EPU <- row.names(mHT.2022.voc)
mHT.2022.voc$covariate <- "VOC"

mHT.y2 <- bind_rows(mHT.2022, mHT.2022.voc) %>%
  arrange(EPU, covariate)

mHT.plot2 = ggplot(mHT.y2, aes(x =EPU, y=tau.hat, color=covariate))+
  geom_point(shape=15, size=3, position=position_dodge(.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line()) +
  xlab(expression(paste("EPU"))) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin =lcl, ymax = ucl), position=position_dodge(0.9)) +
  theme(axis.title.x= element_text(size=14), axis.text.x=element_text(size=12, angle = 90, color="black")) +
  theme(axis.text.y = element_text(size=14))
mHT.plot2

## 2.7.2 Sechelt Peninsula ####
# 
# tau.Sechelt <- matrix(NA,6,3)
# i<- 1
# for(i in 1:3 ){
#   tau.Sechelt[1,i] <- mHT.2016["Sechelt Peninsula", i]
#   tau.Sechelt[2,i] <- mHT.2017["Sechelt Peninsula", i]
#   tau.Sechelt[3,i] <- mHT.2018["Sechelt Peninsula", i]
#   tau.Sechelt[4,i] <- mHT.2019["Sechelt Peninsula", i]
#   tau.Sechelt[5,i] <- mHT.2020["Sechelt Peninsula", i]
#   tau.Sechelt[6,i] <- mHT.2021["Sechelt Peninsula", i]
# }
# rownames(tau.Sechelt) <- 2016:2021
# colnames(tau.Sechelt) <- c("Estimate","lcl","ucl")
# tau.Sechelt <- as.data.frame(tau.Sechelt)
# 


# JAGS ####

load("jags_effort.RData")

# 3.2 CLEAN OUTPUT ####

jags.summary <- as.data.frame(jags_output$BUGSoutput$summary)

tau.jags <- matrix(NA,(nrow(jags.summary)-3),7)
tau.jags <- as.data.frame(tau.jags)
tau.jags[,1] <- as.double(str_extract(jags_output$parameters.to.save[3:(length(jags_output$parameters.to.save)-1)] %>% sort(), "(?<=h)[:digit:]{1,2}"))
tau.jags[,2] <- as.numeric(str_extract(jags_output$parameters.to.save[3:(length(jags_output$parameters.to.save)-1)] %>% sort(), "(?<=y)[:digit:]"))
tau.jags[,3] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
tau.jags[,4] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
tau.jags[,5] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
tau.jags[,6] <- round(jags.summary$Rhat[4:nrow(jags.summary)], 3)
tau.jags[,7] <- round(jags.summary$sd[4:nrow(jags.summary)]/jags.summary$`50%`[4:nrow(jags.summary)], 3)

colnames(tau.jags) <- c("ID", "year", "tau.hat","lcl","ucl","Rhat", "cv")
tau.jags <- left_join(tau.jags, eff[,c(1,4)], by="ID") %>%
  select(EPU, year:cv) %>%
  arrange(EPU, year)


library(writexl)
write_xlsx(tau.jags, "tau.jags.xlsx")

# cv average
jags.cv <- matrix(NA, 2, 3)
jags.cv[,1] <- c(2021, 2022)
jags.cv[,2] <- c(median(tau.jags$cv[tau.jags$year == 1]), median(tau.jags$cv[tau.jags$year == 2]))
jags.cv[,3] <- c(mean(tau.jags$cv[tau.jags$year == 1]), mean(tau.jags$cv[tau.jags$year == 2]))
colnames(jags.cv) <- c("year", "median_cv", "mean_cv")
jags.cv <- as.data.frame(jags.cv)

# 3.3 PLOT OUTPUT ####
## 2021 ####
tau.jags_y1 <- tau.jags %>% filter(year == 1) %>% select(-c(year, Rhat))
tau.jags_plot1 = ggplot(tau.jags_y1, aes(x = EPU, y=tau.hat))+
  geom_point(colour="black", shape=15, size=3)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Population Estimate ± 95 CI"))) +
  geom_linerange(aes(EPU, ymin = lcl, ymax = ucl)) +
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
  geom_linerange(aes(EPU, ymin = lcl, ymax = ucl)) +
  theme(axis.text.x=element_text(size=12, angle = 90)) +
  theme(axis.text.y = element_text(size=12))
tau.jags_plot2

# AGREEANCE TESTS: mHT vs. Bayesian vs. Standard ####
library(SimplyAgree)
# mHT vs Bayesian
## 2021
# a2021mB = agree_test(x = mHT.y1$tau.hat[mHT.y1$covariate=="None"],
#                    y = tau.jags_y1$tau.hat)
# print(a2021mB) # 99.4%

a2021mB.voc = agree_test(x = mHT.y1$tau.hat[mHT.y1$covariate=="VOC"],
                       y = tau.jags_y1$tau.hat)
print(a2021mB.voc) # 87% -> lower than a2021B

# 2022
# a2022mB = agree_test(x = mHT.y2$tau.hat[mHT.y2$covariate=="None"],
#                     y = tau.jags_y2$tau.hat)
# print(a2022mB) #84%

a2022mB.voc = agree_test(x = mHT.y2$tau.hat[mHT.y2$covariate=="VOC"],
                        y = tau.jags_y2$tau.hat)
print(a2022mB.voc) # 73% -> lower than a2022B

# mHT vs Expert
# 2021
# a2021mE = agree_test(x = mHT.y1$tau.hat[mHT.y1$covariate=="None"],
#                     y = results_exp$y1[results_exp$EPU %in% mHT.y1$EPU])
# print(a2021mE) # 89%

a2021mE.voc = agree_test(x = mHT.y1$tau.hat[mHT.y1$covariate=="VOC"],
                        y = results_exp$y1[results_exp$EPU %in% mHT.y1$EPU])
print(a2021mE.voc) # 82% -> lower than a2021E

# 2022
# a2022mE = agree_test(x = mHT.y2$tau.hat[mHT.y2$covariate=="None"],
#                    y = results_exp$y2[results_exp$EPU %in% mHT.y2$EPU])
# print(a2022mE) #82%

a2022mE.voc = agree_test(x = mHT.y2$tau.hat[mHT.y2$covariate=="VOC"],
                       y = results_exp$y2[results_exp$EPU %in% mHT.y2$EPU])
print(a2022mE.voc) # 64% -> lower than a2022E

# DECISION POINT -> USE NON-VOC mHT ESTIMATES because they agree better with other 2 estimates

# JAGS vs Expert
# 2021
a2021BE = agree_test(x = tau.jags_y1$tau.hat,
                     y = results_exp$y1[results_exp$EPU %in% tau.jags_y1$EPU])
print(a2021BE) # 89%

# 2022
a2022BE = agree_test(x = tau.jags_y2$tau.hat,
                     y = results_exp$y2[results_exp$EPU %in% tau.jags_y2$EPU])
print(a2022BE) #82%

# Agreeance Table
Agreement <- as.data.frame(matrix(NA, 6, 3))
# Agreement[1,] <- c(2021, "mHT vs. Bayesian", "None", a2021mB$ccc.xy[1])
Agreement[1,] <- c(2021, "mHT vs. Bayesian", a2021mB.voc$ccc.xy[1])
# Agreement[3,] <- c(2022, "mHT vs. Bayesian", "None", a2022mB$ccc.xy[1])
Agreement[2,] <- c(2022, "mHT vs. Bayesian", a2022mB.voc$ccc.xy[1])
# Agreement[5,] <- c(2021, "mHT vs. Standard", "None", a2021mE$ccc.xy[1])
Agreement[3,] <- c(2021, "mHT vs. Standard", a2021mE.voc$ccc.xy[1])
# Agreement[7,] <- c(2022, "mHT vs. Standard", "None", a2022mE$ccc.xy[1])
Agreement[4,] <- c(2022, "mHT vs. Standard", a2022mE.voc$ccc.xy[1])
Agreement[5,] <- c(2021, "Bayesian vs. Standard", a2021BE$ccc.xy[1])
Agreement[6,] <- c(2022, "Bayesian vs. Standard", a2022BE$ccc.xy[1])

colnames(Agreement) <- c("Year", "Test", "Agreeance")

# write.csv(Agreement,"Agreement.csv", row.names = FALSE)

# COMBINE PLOTS ####

# CV all
cv.all <- bind_rows("mHT" = mHT.cv, "Bayesian" = jags.cv, .id = "Model") %>%
  arrange(year)
# write.csv(cv.all, "CV.csv", row.names = F)

## 2021 ####

mHT.2021 <- mHT.2021.voc %>%
  select(EPU, tau.hat:ucl, cv) %>%
  mutate(model = "mHT")
jags.2021 <- tau.jags_y1 %>%
  mutate(model = "Bayesian")
expert.2021<- results_exp %>%
  filter(EPU %in% jags.2021$EPU) %>%
  select(EPU, tau.hat=y1) %>%
  mutate(lcl = NA,
         ucl = NA,
         model = "Standard")
results_y1 <- bind_rows(mHT.2021, jags.2021, expert.2021) %>%
  mutate(year = 2021)
row.names(results_y1) <- 1:nrow(results_y1)

results_plot1 = ggplot(results_y1, aes(x = EPU, y=tau.hat, color=model))+
  geom_point(shape=15, size=3, position = position_dodge(width = 0.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous("Population Estimate", breaks=c(0, 200, 400, 600, 800), limits=c(0,950)) +
  geom_linerange(aes(EPU, ymin = lcl, ymax = ucl), position = position_dodge(width = 0.7)) +
  theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_colour_grey(start = 0.2, end = 0.8)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_plot1
ggsave("Results_2021.jpeg")

## 2022 ####

mHT.2022 <- mHT.2022.voc %>%
  select(EPU, tau.hat:ucl, cv) %>%
  mutate(model = "mHT")
jags.2022 <- tau.jags_y2 %>%
  mutate(model = "Bayesian")
expert.2022<- results_exp %>%
  filter(EPU %in% jags.2022$EPU) %>%
  select(EPU, tau.hat=y2) %>%
  mutate(lcl = NA,
         ucl = NA,
         model = "Standard")
results_y2 <- bind_rows(mHT.2022, jags.2022, expert.2022) %>%
  mutate(year = 2022)
row.names(results_y2) <- 1:nrow(results_y2)

results_plot2 = ggplot(results_y2, aes(x = EPU, y=tau.hat, color=model))+
  geom_point(shape=15, size=3, position = position_dodge(width = 0.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous("Population Estimate", breaks=c(0, 200, 400, 600, 800), limits=c(0,950)) +
  geom_linerange(aes(EPU, ymin = lcl, ymax = ucl), position = position_dodge(width = 0.7)) +
  theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=12)) +
  scale_colour_grey(start = 0.2, end = 0.8)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_plot2
ggsave("Results_2022.jpeg")

## Repeated EPUs ####
results_2x <- bind_rows(results_y1 %>% filter(EPU %in% results_y2$EPU), results_y2 %>% filter(EPU %in% results_y1$EPU)) %>%
  arrange(EPU, model, year)

results_2x %>%
  group_by(EPU, model) %>%
  summarize(pct_change = ((tau.hat[year == 2022]-tau.hat[year == 2021])/tau.hat[year == 2021])*100) %>%
  # write.csv("Results_2x.csv", row.names = F)

results_2x_plot = ggplot(results_2x, aes(x = as.factor(year), y=tau.hat, color=model))+
  geom_point(shape=15, size=3, position = position_dodge(width = 0.7)) +
  facet_wrap(vars(EPU), ncol = 3) +
  labs(y = "Population Estimate", size=12) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_linerange(aes(as.factor(year), ymin = lcl, ymax = ucl), position = position_dodge(width = 0.7)) +
  theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4), axis.title.x = element_blank()) +
  theme(axis.text.y = element_text(size=12)) +
  scale_colour_grey(start = 0.2, end = 0.8) +
  facet_wrap(vars(EPU), scales = "free_y",)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_2x_plot
ggsave("Results_2x_plot.jpeg")

# All results ####
bind_rows(results_y1, results_y2) %>%
  arrange(year, EPU) %>%
  select(year, EPU, model, everything()) %>%
  # write.csv("Results_All.csv", row.names = F)

# Check Distributions ####

mcmc_violin(jags_output$BUGSoutput$sims.array,
            pars = vars(tau.nh4y1, tau.nh4y2),
            probs = c(0.05, 0.5, 0.95)
) +
  scale_y_continuous("Population Estimate", breaks=c(0, 200, 400, 600, 800, 1000), limits=c(0,1000)) +
  theme(axis.text.y = element_text(size=12))
  
mcmc_areas(jags_output$BUGSoutput$sims.array,
           pars = vars(tau.nh1y1, tau.nh2y1, tau.nh3y1, tau.nh4y1, tau.nh6y1, tau.nh9y1, tau.nh10y1, tau.nh11y1, tau.nh16y1, tau.nh17y1, tau.nh19y1, tau.nh21y1, tau.nh22y1, tau.nh23y1),
           prob = 0.80,
           prob_outer = 0.95, area_method = "equal height")

mcmc_areas(jags_output$BUGSoutput$sims.array,
           pars = vars(tau.nh2y2, tau.nh4y2, tau.nh5y2, tau.nh6y2, tau.nh12y2, tau.nh13y2, tau.nh14y2, tau.nh15y2, tau.nh18y2, tau.nh20y2),
           prob = 0.80,
           prob_outer = 0.95, area_method = "equal height")
