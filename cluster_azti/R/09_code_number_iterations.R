# Script information ------------------------------------------------------

# Title: Sensitivity of MSE risk performance indicators in the case 
# Authors: Leire Ibaibarriaga (libaibarriaga@azti.es) and Laura Wise (lwise@ipma.pt)

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(R.utils)

theme_set(theme_bw(base_size = 14))

# Working directory --------------------------------------------------------

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/" # main directory
#wd <- "C:/Use/GitHub/IBpil_mse/cluster_azti/"
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
  r2 <- mean(tapply(belowBlim, xx$iter, any))
  # Risk 3: maximum anual P(SSB < Blim)
  r3 <- max(tapply(belowBlim, list(xx$year), mean)) 
  
  # average catch
  r4 <- mean(tapply(xx$catch, xx$iter, mean)) 
  
  # average ssb
  r5 <- mean(tapply(xx$ssb, xx$iter, mean)) 
  
  # average rec
  r6 <- mean(tapply(xx$rec, xx$iter, mean)) 
  
  # average f
  r7 <- mean(tapply(xx$f, xx$iter, mean)) 
  
  return(c(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5, r6 = r6, r7 = r7))
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

set.seed(100)

# apply for each scenario

out.boot <- lapply(scenario.list, get_scenario_risks) %>% plyr::ldply()
names(out.boot) <- c("scenario","size", "risk1", "risk2", "risk3", "catch", "ssb", "rec", "f")
out.boot <- out.boot %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)  

# save to file
save(out.boot, file=file.path(".","output_long","out.boot4.RData"))

# change from wide to long format for plotting. only risks

out <- out.boot %>%
  select(scenario:risk3) %>% 
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE) %>% 
  pivot_longer(cols=starts_with("risk"),
             names_to = "risk",
             names_prefix = "risk", 
             values_to="value")

# compute median risk for the maximum sampling size (we will assume this is our "best" guess)

out.med <- out %>% 
  filter(size==maxiter) %>% 
  group_by(risk, Ass, Rule, Rec) %>% 
  summarise(med=median(value))

# Plots for risk ----------------------------------------------------------


# By REC

for (rr in c("REClow","REClowmed")){
  aux <- subset(out, Rec==rr & risk != '2')
  med <- subset(out.med, Rec == rr & risk != '2')
  
  ggplot(aux, aes(factor(size), value, fill=risk)) +
    geom_boxplot() +
    facet_grid(Rule~Ass, scales = "free_y") +
    #geom_hline(aes(yintercept=0.05),linetype="dashed", size=0.9) +
    geom_hline(data=med, aes(yintercept=med, col=risk)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(plot.dir,'/',rr,"_risks.png"))
  
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
    facet_grid(Rule~Ass, scales = "free_y")
  ggsave(paste0(plot.dir,'/',rr,"_quantiles_risks.png"))
  
}

# Compute performance statistics to measure bias and accuracy in Risks-------------

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
    facet_grid(Rule~Ass)
  ggsave(paste0(plot.dir,'/',rr,"_me.png"))
  
  ggplot(aux, aes(size, rmse, col=risk))+
    geom_point()+
    geom_line()+
    facet_grid(Rule~Ass)
  ggsave(paste0(plot.dir,'/',rr,"_rmse.png"))
  
  ggplot(aux, aes(size, cv, col=risk))+
    geom_point()+
    geom_line()+
    facet_grid(Rule~Ass)
  ggsave(paste0(plot.dir,'/',rr,"_cv.png"))
  
}


# Plots for catch, SSB, R and F -------------------------------------------

for (rr in c("REClow","REClowmed")){
  aux <- subset(out.boot, Rec==rr)
  aux0 <- aux %>%
    group_by(scenario, Rule, Ass) %>% 
    summarise(across(catch:f, median))
  
  ggplot(aux, aes(factor(size), ssb)) +
    geom_boxplot() +
    facet_wrap(Rule~Ass, scales = "free_y") +
    geom_hline(data=aux0, aes(yintercept=ssb), lwd=1)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(plot.dir,'/',rr,"_ssb.png"))
  
  ggplot(aux, aes(factor(size), catch)) +
    geom_boxplot() +
    facet_wrap(Rule~Ass, scales = "free_y") +
    geom_hline(data=aux0, aes(yintercept=catch), lwd=1)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(plot.dir,'/',rr,"_catch.png"))
  
  ggplot(aux, aes(factor(size), rec)) +
    geom_boxplot() +
    facet_wrap(Rule~Ass, scales = "free_y") +
    geom_hline(data=aux0, aes(yintercept=rec), lwd=1)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(plot.dir,'/',rr,"_rec.png"))
  
  ggplot(aux, aes(factor(size), f)) +
    geom_boxplot() +
    facet_wrap(Rule~Ass, scales = "free_y") +
    geom_hline(data=aux0, aes(yintercept=f), lwd=1)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  ggsave(paste0(plot.dir,'/',rr,"_f.png"))
  
}

#Plots  for ssb, f, catch, rec ------------------------------------------------------------------

# from wide to long format for plotting
dd <- out.boot[,c(1,2,6:9)] %>%
  pivot_longer(!c(scenario,size),names_to = "indicator",values_to="value") %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)


# compute median values for the maximum sampling size (we will assume this is our "best" guess)

dd.med <- dd %>% 
  filter(size==maxiter) %>% 
  group_by(indicator, Ass,Rule, Rec) %>% 
  summarise(med=median(value))

  #By REC
  for (ind in c("catch","ssb","rec","f")){
    
    ddff <- subset(dd, indicator == ind)
    d.med <- subset(dd.med, indicator == ind)
    
    for (rr in c("REClow","REClowmed")){
      aux <- subset(ddff,Rec==rr )
      med <- subset(d.med, Rec == rr )
    
    ggplot(aux, aes(factor(size), value)) +
      geom_boxplot() +
      facet_grid(Ass~Rule, scales = "free_y") +
      geom_hline(data=med, aes(yintercept=med)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ggsave(paste0(plot.dir,'/',rr,"_",ind, ".png"))
    
  }
}