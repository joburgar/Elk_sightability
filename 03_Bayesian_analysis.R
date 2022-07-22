# Elk Sightability Analysis
# Step 3: Bayesian Analysis

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")
load("jags_input.Rdata")

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 3.1 RUN MODEL ####

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("bo", "bvoc", paste(colnames(scalar.dat)[1:(ncol(scalar.dat)-3)], sep = ","))

# MCMC settings
ni <- 1000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 500
nc <- 3

# i <- 1
# 
# bundle.hy <- as.character()
#   for(i in 1:(ncol(scalar.dat)-3)){
#   bundle.hy[i] <-
#     paste(colnames(scalar.dat)[i],
#           "=scalar.dat$",
#           colnames(scalar.dat)[i], 
#           ", ",
#           sep = "")
#   }
# # # run and copy the output to paste into data bundle (don't forget to remove quotes and final comma)
# # paste0(bundle.hy, collapse = "")

# Bundle data
bundle.dat <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                   x=oper.dat$x+.000001, ym1=oper.dat$ym1, h=oper.dat$h, q=oper.dat$q, z=oper.dat$z, yr=oper.dat$yr, subunits=oper.dat$subunits, # oper.dat
                   h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot_dat
                   R=scalar.dat$R, Ngroups=scalar.dat$Ngroups, Nsubunits.yr=scalar.dat$Nsubunits.yr, scalars=scalar.dat[,1:nrow(plot.dat)]) #scalar.dat
# Run model
jags_output <- jags(bundle.dat, inits, params, "beta_binom_model_elk2022.txt", nc, ni, nb, nt)

tau.hat <- matrix(NA, length(bundle.dat$scalars), 2)
for(i in 1:length(bundle.dat$scalars)){
  tau.hat[i,1] <- colnames(bundle.dat$scalars)[i]
  p <- if_else(i > 1, (sum(bundle.dat$scalars[1:(i-1)])+1), 1)
  q <- sum(bundle.dat$scalars[1:i])
  tau.hat[i, 2] <- as.double(sum(oper.dat$h[p:q]))
}
colnames(tau.hat) <- c("ID", "tau.hat")

setwd("C:/Users/TBRUSH/R/Elk_sightability/out")

save("jags_output",file="jags_output.RData")
save("eff",file="jags_effort.RData")

rm(list = ls())
# 
# tau.Sechelt50_jags_sub <- tau.Sechelt50_jags %>% filter(mean <300) # only 15 / 46 with pop estimates < 300
# tau.Sechelt50_jags_sub_simsplot = ggplot(tau.Sechelt50_jags_sub, aes(x = reorder(row.names(tau.Sechelt50_jags_sub),mean), y=mean))+
#   geom_point(colour="black", shape=15, size=3)+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(row.names(tau.Sechelt50_jags_sub), ymin = lcl, ymax = ucl)) +
#   geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
#   theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
#   theme(axis.text.y = element_text(size=14))
# tau.Sechelt50_jags_sub_simsplot
# ggsave(tau.Sechelt50_jags_simsplot, file="out/tau.Sechelt50_jags_simsplot.PNG")
# 
# 
# tau.Sechelt50$model <- "mHT"
# tau.Sechelt50$simID <- row.names(tau.Sechelt50)
# names(tau.Sechelt50)
# tau.Sechelt50_jags$model <- "Bayesian"
# names(tau.Sechelt50_jags)
# tau.Sechelt50_jags$simID <- row.names(tau.Sechelt50_jags)
# 
# Sechelt50 <- rbind(tau.Sechelt50, tau.Sechelt50_jags %>% select(-Rhat) %>% rename(tau.hat=mean))
# 
# 
# col.cat <- as.character(c("#2028B2","#B2AA20"))
# Sechelt50_sub <- Sechelt50 %>% filter(tau.hat<300)
# 
# Sechelt50_sub_simsplot = Sechelt50_sub %>%
#   ggplot(aes(x = reorder(simID,tau.hat), y=tau.hat, fill=model))+
#   geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
#   scale_fill_manual(values=unique(col.cat)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(simID, ymin = lcl, ymax = ucl), position=position_dodge(width=1)) +
#   geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
#   theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
#   theme(axis.text.y = element_text(size=14))
# Sechelt50_sub_simsplot
# ggsave(Sechelt50_sub_simsplot, file="out/Sechelt50_both_sub_simsplot.PNG")
# 
# Sechelt50_simsplot = Sechelt50 %>%
#   ggplot(aes(x = reorder(rownames(Sechelt50),tau.hat), y=tau.hat, fill=model))+
#   geom_point(colour="white", shape=21, size = 4, position=position_dodge(width=1))+
#   scale_fill_manual(values=unique(col.cat)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   ylab(expression(paste("Population Estimate ± 95 CI"))) +
#   geom_linerange(aes(rownames(Sechelt50), ymin = lcl, ymax = ucl), position=position_dodge(width=1)) +
#   geom_hline(yintercept=pop.size, linetype="dashed", color = "red") +
#   theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
#   theme(axis.text.y = element_text(size=14))+
#   ylim(c(0,20000))
# Sechelt50_simsplot
# ggsave(Sechelt50_simsplot, file="out/Sechelt50_both_simsplot_simsplot.PNG")
