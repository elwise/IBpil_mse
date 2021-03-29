
dat <- load(file.path("output_long","scenarios","results_ASSnone_HCR0_REClow_INNvar_OERnone.RData"))



ff <- function(k, out.bio, size=100, proj.yrs=2021:2030, Blim=337448, Blow=196334){
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

out.boot <- NULL
for (size in seq(100,1000,by=100)){
  tmp <- sapply(1:1000, FUN=ff, out.bio=out.bio, size=size, proj.yrs=2021:2030, Blim=337448, Blow=196334)
  out.boot <- rbind(out.boot, cbind(size, t(tmp)))
}
out.boot <- as.data.frame(out.boot)
names(out.boot) <- c("Size", "Risk1", "Risk2", "Risk3")

library(tidyr)
out.boot <- out.boot %>% 
  pivot_longer(cols=starts_with("Risk"),
               names_to = "Risk",
               names_prefix = "Risk", 
               values_to="Value")
  


med <- out.boot %>% 
  filter(Size==1000) %>% 
  group_by(Risk) %>% 
  summarise(Med=median(Value))
  
library(ggplot2)

p <- ggplot(out.boot, aes(Size, Value))+
  geom_point()+
  facet_wrap(~Risk)+
  geom_hline(data=med, aes(yintercept=Med))
p


p <- ggplot(out.boot, aes(factor(Size), Value, fill=Risk))+
  geom_boxplot()+
  geom_hline(data=med, aes(yintercept=Med))
p
