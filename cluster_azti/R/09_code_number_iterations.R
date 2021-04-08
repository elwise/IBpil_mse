# Script information ------------------------------------------------------

# Title: Sensitivity of MSE risk performance indicators in the case 
# Authors: Leire Ibaibarriaga (libaibarriaga@azti.es) and Laura Wise (lwise@ipma.pt)

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(R.utils)

theme_set(theme_bw(base_size = 14))

# Working directory --------------------------------------------------------

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./output_long")
# directory with plots
plot.dir <- file.path(res.dir,"plots")

# List of scenarios with 10000 iterations ---------------------------------

scenario.list <- sub("results_(.*).RData", "\\1", list.files(file.path(".","output_long","scenarios")), perl = TRUE)

# Ad-hoc function to compute risk for a random subsample ------------------

# out.bio: summary object of bioSum
# sample_size: sample size
# proj.yrs: years to compute the sample size
# Blim: Blim

get_risks <- function(out.bio, sample_size, proj.yrs = 2061:2070, Blim ) {
  sampled_iters <- sample(unique(out.bio$iter), size = sample_size)
  xx <- subset(out.bio, out.bio$iter %in% sampled_iters & out.bio$year %in% proj.yrs)
  belowBlim <- xx[, 'ssb'] < Blim
  # Risk 1: P(SSB < Blim)
  r1 <- mean(belowBlim)
  # Risk 2: P(SSB < Blim) at least once
  r2 <- mean(tapply(belowBlim, xx$iter, any)  )
  # Risk 3: maximum anual P(SSB < Blim)
  r3 <- max(tapply(belowBlim, list(xx$year), mean)) 
  return(c(r1 = r1, r2 = r2, r3 = r3))
}

# Risk depending on the number of iterations ------------------------------

# maximum number of iterations

maxiter <- 10000

# bootstrap with different sampling size for each scenario

get_scenario_risks <- function(sc) {
  out.bio <- loadToEnv(file.path(".","output_long",paste0("scenarios/results_",sc, ".RData")))[["out.bio"]]
  rsc <- sub(pattern=".*REC *(.*?) *_INN.*", replacement="\\1", x=sc) # extract the recruitment scenario to choose Blim
  
  if(rsc=="lowmed") {  Blim <- 337448 } else { Blim <- 196334}
  message("Scenario:", sc, '\nRec:', rsc, ' Blim: ',Blim)
  lapply(seq(500, 10000, by = 500), 
         function(size) {
           message("  Sample size:", size)
           data.frame(scenario = sc, size = size, t(replicate(get_risks(out.bio, size, Blim = Blim), n = 100, simplify = TRUE)))
         }
  ) %>% plyr::ldply()
}

out.boot <- lapply(scenario.list, get_scenario_risks) %>% plyr::ldply()

names(out.boot) <- c("scenario","size", "risk1", "risk2", "risk3")

# from wide to long format for plotting
out <- out.boot %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE) %>% 
  pivot_longer(cols=starts_with("risk"),
               names_to = "risk",
               names_prefix = "risk", 
               values_to="value")

# save to file
save(out.boot, file=file.path(".","2020/cluster_azti/output_long","out.boot2.RData"))

# compute median risk for the maximum sampling size (we will assume this is our "best" guess)

out.med <- out %>% 
  filter(size==maxiter) %>% 
  group_by(risk, Ass,Rule, Rec) %>% 
  summarise(med=median(value))

# Changed this part of the code to avoid 'for'
# # Ad-hoc function to compute risk for a random subsample ------------------
# 
# # k: identifier for the sample, that sets the seed
# # out.bio: summary object of bioSum
# # size: sample size
# # proj.yrs: years to compute the sample size
# # Blim: Blim
# 
# ff <- function(k, out.bio, size=100, proj.yrs=2021:2070, Blim=337448){
#   set.seed(k)
#   samples.iter <- sample(x=unique(out.bio$iter), size=size, replace=T)
#   xx <- subset(out.bio, iter %in% samples.iter)
#   idx <-  xx$year %in% proj.yrs
#   xx <- subset(xx, idx)
#   it <- unique(xx$iter)
#   nit <- length(it)
#   yrnms <- proj.yrs
#   nyr <- length(yrnms)
#   #mp_yr <- 2023 #in this new evaluation there is no MP year where some objective should be met.
#   
#   out <- NULL
#   
#   # # Risk 1: P(SSB < Blim)
#   pBlim <- ifelse( xx[,'ssb'] < Blim, 1, 0)
#   tmp <- sum(pBlim) / (nit * nyr)
#   out <- c(out, tmp)
#   
#   # Risk 2: P(SSB < Blim) at least once
#   tmp <- mean( tapply(pBlim, list(xx$iter), max) )
#   out <- c(out, tmp)
#   
#   # Risk 3: maximum anual P(SSB < Blim)
#   tmp <- max( tapply(pBlim, list(xx$year), mean) ) 
#   out <- c(out, tmp)
#   
#   out
# }  
# 
# # List of scenarios with 10000 iterations ---------------------------------
# 
# scenario.list <- sub("results_(.*).RData", "\\1", list.files(file.path(".","/2020/cluster_azti/output_long","scenarios")), perl = TRUE)
# 
# # Risk depending on the number of iterations ------------------------------
# 
# #maximum number of iterations
# 
# maxiter <- 10000
# 
# # bootstrap with different sampling size for each scenario
# 
# out.boot <- NULL
# for (sc in scenario.list[1]){
#   out.bio <- loadToEnv(file.path(".","/2020/cluster_azti/output_long","scenarios",sc))[["out.bio"]]
#   rsc <- sub(pattern=".*REC *(.*?) *_INN.*", replacement="\\1", x=sc) # extract the recruitment scenario if needed
# 
#   # use always the same Blim? include two Blim values in the ff function?
#   
#   for (size in seq(500,10000,by=500)){
#       tmp <- sapply(1:1000, FUN=ff, out.bio=out.bio, size=size, proj.yrs=2021:2070, Blim=337448)
#     out.boot <- rbind(out.boot, data.frame(unique(out.bio$scenario), size, t(tmp)))
#   }
# }
# # out.boot <- as.data.frame(out.boot)
# names(out.boot) <- c("scenario","size", "risk1", "risk2", "risk3")
# 
# # from wide to long format for plotting
# 
# out.boot <- out.boot %>% 
#   pivot_longer(cols=starts_with("r"),
#                names_to = "risk",
#                names_prefix = "risk", 
#                values_to="value")
# 
# # save the file
# 
# save(out.boot, file=file.path("..","output_long","out.boot1.RData"))
# 
# # compute median risk for the maximum sampling size (we will assume this is our "best" guess)
# 
# out.med <- out.boot %>% 
#   filter(size==maxiter) %>% 
#   group_by(risk, scenario) %>% 
#   summarise(med=median(value))

# Plots  ------------------------------------------------------------------

#By REC

for (rr in c("REClow","REClowmed")){
  aux <- subset(out,Rec==rr & risk != '2')
  med <- subset(out.med, Rec == rr & risk != '2')
  
  ggplot(aux, aes(factor(size), value, fill=risk)) +
    geom_boxplot() +
    facet_grid(Ass~Rule, scales = "free_y") +
    geom_hline(aes(yintercept=0.05),linetype="dashed", size=0.9) +
    geom_hline(data=med, aes(yintercept=med, col=risk)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(plot.dir,'/',rr,"risks.png"))
  
}

for (rr in c("REClow","REClowmed")){
  aux <- subset(out,Rec==rr & risk != '2')
  
  aux %>%
    group_by(size, risk, Ass, Rule) %>%
    mutate(q95 = diff(quantile(value, probs = c(0.025, 0.975))),
           q50 = diff(quantile(value, probs = c(0.25, 0.75)))) %>%
    pivot_longer(c(q95, q50), names_to = "interval", values_to = "range") %>%
    ggplot(aes(x = size)) +
    geom_line(aes(y = range, linetype = interval, colour = risk)) +
    facet_grid(Ass~Rule, scales = "free_y")
  ggsave(paste0(plot.dir,'/',rr,"quantiles_risks.png"))
  
}

# Compute performance statistics to measure bias and accuracy -------------

out <- left_join(out, out.med, by=c("risk","Ass","Rule","Rec")) 

out.perf <- out %>% 
  group_by(Rec,Ass,Rule, size, risk) %>% 
  summarise(me=mean(value-med),
         rmse=sqrt( mean((value-med)^2) ),
         cv=sqrt(var(value))/mean(value))

#By REC

for (rr in c("REClow","REClowmed")){
  aux <- subset(out.perf,Rec==rr & risk != '2')
  
  ggplot(aux, aes(size, me, col=risk))+
    geom_point()+
    geom_line()+
    facet_grid(Ass~Rule)
  ggsave(paste0(plot.dir,'/',rr,"me.png"))
  
  ggplot(aux, aes(size, rmse, col=risk))+
    geom_point()+
    geom_line()+
    facet_grid(Ass~Rule)
  ggsave(paste0(plot.dir,'/',rr,"rmse.png"))
  
  ggplot(aux, aes(size, cv, col=risk))+
    geom_point()+
    geom_line()+
    facet_grid(Ass~Rule)
  ggsave(paste0(plot.dir,'/',rr,"cv.png"))
  
}
