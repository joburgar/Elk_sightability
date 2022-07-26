# Elk Sightability Analysis
# Step 4: Plotting and Reporting

# LOAD PACKAGES ####

list.of.packages <- c("bayesplot", "tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags", "SimplyAgree")
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

## Clean output ####
# Combine dataframes
mHT <- bind_rows(mHT_y1 %>% mutate(year = 1), mHT_y2 %>% mutate(year = 2))

## CV stats ####
mHT.cv <- matrix(NA, 2, 3)
mHT.cv[,1] <- c(2021, 2022)
mHT.cv[,2] <- c(median(mHT_y1$cv), median(mHT_y2$cv))
mHT.cv[,3] <- c(mean(mHT_y1$cv), mean(mHT_y2$cv))

colnames(mHT.cv) <- c("year", "median_cv", "mean_cv")
mHT.cv <- as.data.frame(mHT.cv)


# JAGS ####

load("jags_effort.RData")

## Clean output ####

jags.summary <- as.data.frame(jags_output$BUGSoutput$summary)

tau.jags <- matrix(NA,(nrow(jags.summary)-3),7)
tau.jags <- as.data.frame(tau.jags)
tau.jags[,1] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jags_output$BUGSoutput$median$tau.hat)], "(?<=h)[:digit:]{1,2}")) %>% sort()
tau.jags[,2] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jags_output$BUGSoutput$median$tau.hat)], "(?<=y)[:digit:]{1,2}")) %>% sort()
tau.jags[,3] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
tau.jags[,4] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
tau.jags[,5] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
tau.jags[,6] <- round(jags.summary$Rhat[4:nrow(jags.summary)], 3)
tau.jags[,7] <- round(jags.summary$sd[4:nrow(jags.summary)]/jags.summary$`50%`[4:nrow(jags.summary)], 3)

colnames(tau.jags) <- c("ID", "year", "tau.hat","lcl","ucl","Rhat", "cv")
tau.jags <- left_join(tau.jags, eff[,c(1,5)], by="ID") %>%
  select(EPU = Unit, year:cv) %>%
  arrange(EPU, year)


library(writexl)
write_xlsx(tau.jags, "tau.jags.xlsx")

tau.jags_y1 <- tau.jags %>% filter(year == 1) %>% select(-c(year, Rhat))
tau.jags_y2 <- tau.jags %>% filter(year == 2) %>% select(-c(year, Rhat))

## CV stats ####

jags.cv <- matrix(NA, 2, 3)
jags.cv[,1] <- c(2021, 2022)
jags.cv[,2] <- c(median(tau.jags$cv[tau.jags$year == 1]), median(tau.jags$cv[tau.jags$year == 2]))
jags.cv[,3] <- c(mean(tau.jags$cv[tau.jags$year == 1]), mean(tau.jags$cv[tau.jags$year == 2]))
colnames(jags.cv) <- c("year", "median_cv", "mean_cv")
jags.cv <- as.data.frame(jags.cv)


# COMBINE RESULTS ####


## 2021 ####

mHT_y1 <- mHT_y1 %>%
  select(EPU, tau.hat:ucl, cv) %>%
  mutate(model = "mHT")
jags_y1 <- tau.jags_y1 %>%
  mutate(model = "Bayesian")
expert_y1<- results_exp %>%
  filter(EPU %in% jags_y1$EPU) %>%
  select(EPU, tau.hat=y1) %>%
  mutate(lcl = NA,
         ucl = NA,
         model = "Standard")
results_y1 <- bind_rows(mHT_y1, jags_y1, expert_y1) %>%
  mutate(year = 2021)
row.names(results_y1) <- 1:nrow(results_y1)


## 2022 ####

mHT_y2 <- mHT_y2 %>%
  select(EPU, tau.hat:ucl, cv) %>%
  mutate(model = "mHT")
jags_y2 <- tau.jags_y2 %>%
  mutate(model = "Bayesian")
expert_y2<- results_exp %>%
  filter(EPU %in% jags_y2$EPU) %>%
  select(EPU, tau.hat=y2) %>%
  mutate(lcl = NA,
         ucl = NA,
         model = "Standard")
results_y2 <- bind_rows(mHT_y2, jags_y2, expert_y2) %>%
  mutate(year = 2022)
row.names(results_y2) <- 1:nrow(results_y2)


# 
# ## Repeated EPUs ####
# results_2x <- bind_rows(results_y1 %>% filter(EPU %in% results_y2$EPU), results_y2 %>% filter(EPU %in% results_y1$EPU)) %>%
#   arrange(EPU, model, year)
# 
# results_2x %>%
#   group_by(EPU, model) %>%
#   summarize(pct_change = ((tau.hat[year == 2022]-tau.hat[year == 2021])/tau.hat[year == 2021])*100) %>%
#   # write.csv("Results_2x.csv", row.names = F)
# 
# results_2x_plot = ggplot(results_2x, aes(x = as.factor(year), y=tau.hat, color=model))+
#   geom_point(shape=15, size=3, position = position_dodge(width = 0.7)) +
#   facet_wrap(vars(EPU), ncol = 3) +
#   labs(y = "Population Estimate", size=12) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_linerange(aes(as.factor(year), ymin = lcl, ymax = ucl), position = position_dodge(width = 0.7)) +
#   theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4), axis.title.x = element_blank()) +
#   theme(axis.text.y = element_text(size=12)) +
#   scale_colour_grey(start = 0.2, end = 0.8) +
#   facet_wrap(vars(EPU), scales = "free_y",)
# # For palette choices: 
# #   RColorBrewer::display.brewer.all()
# results_2x_plot
# ggsave("Results_2x_plot.jpeg")



## All results ####
tau_transposed <- bind_rows(results_y1, results_y2) %>%
  arrange(year, EPU) %>%
  select(EPU, year, model, tau.hat) %>%
  pivot_wider(names_from = model, values_from = tau.hat)

results_final <- bind_rows(results_y1, results_y2) %>%
  mutate(estimate = if_else(is.na(lcl), paste(tau.hat),
                            paste(tau.hat, " (", lcl, ",", ucl, ")", sep = ""))) %>%
  select(year, EPU, model, estimate, cv) %>%
  pivot_wider(names_from = model, values_from = c(estimate, cv)) %>%
  write.csv("Results_final.csv", row.names = F)

## Agreeance: mHT vs. Bayesian vs. Standard ####

# mHT vs Bayesian
## 2021

agree.mB = agree_test(x = tau_transposed$mHT,
                      y = tau_transposed$Bayesian, 
                      delta = 1)
print(agreemB) # 74%
plot(a2021mB)



# mHT vs Expert
# 2021

agree_mE = agree_test(x = mHT_y1$tau.hat,
                      y = expert_y1$tau.hat,
                      delta = 1)
print(a2021mE)# 56%
plot(a2021mE)

a2022mE = agree_test(x = mHT_y2$tau.hat,
                     y = results_y2$tau.hat, 
                     delta = 1)
print(a2022mE)
plot(a2022mE) # -22%

# JAGS vs Expert
# 2021
a2021BE = agree_test(x = jags_y1$tau.hat,
                     y = results_y1$tau.hat,
                     delta = 1)
print(a2021BE) # 82%
plot(a2021BE)

# 2022
a2022BE = agree_test(x = jags_y2$tau.hat,
                     y = results_y2$tau.hat, 
                     delta = 1)
print(a2022BE) #16%
plot(a2022BE)

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


## CV stats ####
cv.all <- bind_rows("mHT" = mHT.cv, "Bayesian" = jags.cv, .id = "Model") %>%
  arrange(year)
write.csv(cv.all, "CV.csv", row.names = F)

# PLOT RESULTS ####

## 2021 ####
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
