# Script information ------------------------------------------------------

# Title: Sensitivity of MSE risk performance indicators in the case 
# Authors: Leire Ibaibarriaga (libaibarriaga@azti.es) 

# Load libraries ----------------------------------------------------------

library(tidyverse)

theme_set(theme_bw(base_size = 14))

# Ad-hoc function to compute risk for a random subsample ------------------

# k: identifier for the sample, that sets the seed
# out.bio: summary object of bioSum
# size: sample size
# proj.yrs: years to compute the sample size
# Blim: Blim

ff <- function(k, out.bio, size=100, proj.yrs=2021:2030, Blim=337448){
  set.seed(k)
  samples.iter <- sample(x=unique(out.bio$iter), size=size, replace=T)
  xx <- subset(out.bio, iter %in% samples.iter)
  idx <-  xx$year %in% proj.yrs
  xx <- subset(xx, idx)
  it <- unique(xx$iter)
  nit <- length(it)
  yrnms <- proj.yrs
  nyr <- length(yrnms)
  mp_yr <- 2023
  
  out <- NULL
  
  # # Risk 1: P(SSB < Blim)
  pBlim <- ifelse( xx[,'ssb'] < Blim, 1, 0)
  tmp <- sum(pBlim) / (nit * nyr)
  out <- c(out, tmp)
  
  # Risk 2: P(SSB < Blim) at least once
  tmp <- mean( tapply(pBlim, list(xx$iter), max) )
  out <- c(out, tmp)
  
  # Risk 3: maximum anual P(SSB < Blim)
  tmp <- max( tapply(pBlim, list(xx$year), mean) ) 
  out <- c(out, tmp)
  
  out
}  

# List of scenarios with 10000 iterations ---------------------------------

scenario.list <- list.files(file.path("..","output_long","scenarios")) 

# Risk depending on the number of iterations ------------------------------

#maximum number of iterations

maxiter <- 10000

# bootstrap with different sampling size for each scenario

out.boot <- NULL
for (sc in scenario.list){
  out.bio <- loadToEnv(file.path("..","output_long","scenarios",sc))[["out.bio"]]
  rsc <- sub(pattern=".*REC *(.*?) *_INN.*", replacement="\\1", x=sc) # extract the recruitment scenario if needed

  # use always the same Blim? include two Blim values in the ff function?
  
  for (size in seq(500,10000,by=500)){
      tmp <- sapply(1:1000, FUN=ff, out.bio=out.bio, size=size, proj.yrs=2021:2030, Blim=337448)
    out.boot <- rbind(out.boot, data.frame(unique(out.bio$scenario), size, t(tmp)))
  }
}
# out.boot <- as.data.frame(out.boot)
names(out.boot) <- c("scenario","size", "risk1", "risk2", "risk3")

# from wide to long format for plotting

out.boot <- out.boot %>% 
  pivot_longer(cols=starts_with("risk"),
               names_to = "risk",
               names_prefix = "risk", 
               values_to="value")

# save the file

save(out.boot, file=file.path("..","output_long","out.boot1.RData"))

# compute median risk for the maximum sampling size (we will assume this is our "best" guess)

out.med <- out.boot %>% 
  filter(size==maxiter) %>% 
  group_by(risk, scenario) %>% 
  summarise(med=median(value))

# Plots  ------------------------------------------------------------------

p <- ggplot(out.boot, aes(factor(size), value, fill=risk))+
  geom_boxplot()+
  facet_grid(scenario~factor(risk))+
  geom_hline(data=out.med, aes(yintercept=med, col=risk))+
  ylim(c(0,1))
p

p <- ggplot(out.boot, aes(factor(size), value, fill=risk))+
  geom_boxplot()+
  facet_grid(~scenario)+
  geom_hline(data=out.med, aes(yintercept=med, col=risk))+
  geom_hline(aes(yintercept=0.05), lty=2)+
  ylim(c(0,1))
p


# Compute performance statistics to measure bias and accuracy -------------

out.boot <- left_join(out.boot, out.med, by=c("risk","scenario")) 

out.perf <- out.boot %>% 
  group_by(scenario, size, risk) %>% 
  summarise(me=mean(value-med),
         rmse=sqrt( mean((value-med)^2) ),
         cv=sqrt(var(value))/mean(value))

p <- ggplot(out.perf, aes(size, me, col=risk))+
  geom_point()+
  geom_line()+
  facet_grid(~scenario)
p

p <- ggplot(out.perf, aes(size, rmse, col=risk))+
  geom_point()+
  geom_line()+
  facet_grid(~scenario)
p

p <- ggplot(out.perf, aes(size, cv, col=risk))+
  geom_point()+
  geom_line()+
  facet_grid(~scenario)
p
