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
# Blim: Blim

get_risks_year <- function(out.bio, sample_size, Blim ) {

  sampled_iters <- 1:sample_size # added sequentially, not bootstrapped
  out.bio$belowBlim <- as.numeric(out.bio$ssb < Blim)
  rr <- out.bio %>%
    filter(iter %in% sampled_iters) %>% 
    group_by(year) %>% 
    dplyr::summarise(med=median(ssb),
              q05=quantile(ssb, 0.05),
              q95=quantile(ssb, 0.95),
              risk=sum(belowBlim)/n()) %>% 
    ungroup()
  return(as.data.frame(rr))
}

# Risk depending on the number of iterations ------------------------------

# maximum number of iterations

maxiter <- 10000

# different sampling size for each scenario (they are added sequentially, not bootstrapped)

get_scenario_risks_year <- function(sc) {
  out.bio <- loadToEnv(file.path(".","output_long",paste0("scenarios/results_",sc, ".RData")))[["out.bio"]]
  rsc <- sub(pattern=".*REC *(.*?) *_INN.*", replacement="\\1", x=sc) # extract the recruitment scenario to choose Blim
  
  if(rsc=="lowmed") {  Blim <- 337448 } else { Blim <- 196334}
  message("Scenario:", sc, '\nRec:', rsc, ' Blim: ',Blim)
  lapply(seq(1000, 10000, by = 1000), 
         function(size) {
           message("  Sample size:", size)
           data.frame(scenario = sc, size = size, get_risks_year(out.bio, size, Blim = Blim))
         }
  ) %>% plyr::ldply()
}



out.stationarity <- lapply(scenario.list, get_scenario_risks_year) %>% plyr::ldply()

# separate scenario

out.stationarity <- out.stationarity %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE) %>% 
  mutate(size=as.factor(size))

out.stationarity$scenario <- fct_recode(out.stationarity$scenario,"ASSnone_HCR50_REClowmed_INNvar_OERnone"="ASSnone_HCR10_REClowmed_INNvar_OERnone",
           "ASSnone_ICES_low_REClowmed_INNvar_OERnone"="ASSnone_HCR11_REClowmed_INNvar_OERnone", 
           "ASSnone_HCR40_REClow_INNvar_OERnone"="ASSnone_HCR8_REClow_INNvar_OERnone",
           "ASSnone_HCR45_REClow_INNvar_OERnone"="ASSnone_HCR9_REClow_INNvar_OERnone",
           "ASSss3_HCR50_REClowmed_INNvar_OERnaq"= "ASSss3_HCR10_REClowmed_INNvar_OERnaq",
           "ASSss3_ICES_low_REClowmed_INNvar_OERnaq"="ASSss3_HCR11_REClowmed_INNvar_OERnaq",
           "ASSss3_HCR40_REClow_INNvar_OERnaq"="ASSss3_HCR8_REClow_INNvar_OERnaq",
           "ASSss3_HCR45_REClow_INNvar_OERnaq"='ASSss3_HCR9_REClow_INNvar_OERnaq')

# plots for ssb distribution

pdf(file.path(wd,"output_long","plots","stationarity_by_size_ssb.pdf"), width=9)
out.stationarity %>%
  filter(year>2019) %>% 
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=med, ymin=q05, ymax=q95, col=size, fill=size)+
             geom_point()+
             geom_line()+
             geom_ribbon(alpha=0.2)+
             facet_wrap(~size)+
             ylab("SSB") +
             ggtitle(unique(.$scenario))))
dev.off()

# plots for ssb distribution

pdf(file.path(wd,"output_long","plots","stationarity_by_size_ssb_last20yrs.pdf"), width=9)
out.stationarity %>%
  filter(year>2049) %>% 
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=med, ymin=q05, ymax=q95, col=size, fill=size)+
             geom_point()+
             geom_line()+
             geom_ribbon(alpha=0.2)+
             facet_wrap(~size)+
             ylab("SSB")+
             ggtitle(unique(.$scenario))))
dev.off()


# plots for ssb distribution

pdf(file.path(wd,"output_long","plots","stationarity_by_size_risk.pdf"), width=9)
out.stationarity %>%
  filter(year>2019) %>% 
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=risk, col=size, fill=size)+
             geom_point()+
             geom_line()+
             facet_wrap(~size)+
             ylab("P(SSB<Blim)")+
             ggtitle(unique(.$scenario))))
dev.off()

pdf(file.path(wd,"output_long","plots","stationarity_by_size_risk_last20yrs.pdf"), width=9)
out.stationarity %>%
  filter(year>2049) %>% 
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=risk, col=size, fill=size)+
             geom_point()+
             geom_line()+
             facet_wrap(~size)+
             ylab("P(SSB<Blim)")+
             ggtitle(unique(.$scenario))))
dev.off()

