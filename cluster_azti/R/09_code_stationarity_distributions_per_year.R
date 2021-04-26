# Script information ------------------------------------------------------

# Title: Checking stationarity along years 
# Authors: Leire Ibaibarriaga (libaibarriaga@azti.es) and Laura Wise (lwise@ipma.pt)

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(R.utils)

theme_set(theme_bw(base_size = 14))

# Working directory --------------------------------------------------------

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/" # main directory
# wd <- "C:/Use/GitHub/IBpil_mse/cluster_azti" # main directory
setwd(wd)

# Check stationarity for each scenario ------------------------------------
# Are the distributions the same along years for 1000 iterations?

# read the quantile summary file:

load(file.path(wd,"output","res_bio_all.RData"))

dat.bio.q$scenario <- gsub('HCR10_', 'HCR50_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR9_', 'HCR45_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR8_', 'HCR40_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR11_', 'ICES_low_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR0_', 'ICES_med_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR7_', 'HCR0_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR13_', 'HCR35_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR14_', 'HCR30_', dat.bio.q$scenario)

# ssb

pdf(file.path(wd,"output","plots","stationarity_ssb.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
    aes(x=year, y=ssb_q50, ymin=ssb_q05, ymax=ssb_q95)+
    geom_point()+
    geom_line()+
    geom_errorbar()+
    ylab("SSB(t)")+
    ggtitle(unique(.$scenario))))
dev.off()

# recruitment

pdf(file.path(wd,"output","plots","stationarity_rec.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=rec_q50, ymin=rec_q05, ymax=rec_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("Recruitment")+
             ggtitle(unique(.$scenario))))
dev.off()

# catch 

pdf(file.path(wd,"output","plots","stationarity_catch.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=catch_q50, ymin=catch_q05, ymax=catch_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("Catch (t)")+
             ggtitle(unique(.$scenario))))
dev.off()

# f

pdf(file.path(wd,"output","plots","stationarity_f.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=f_q50, ymin=f_q05, ymax=f_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("F")+
             ggtitle(unique(.$scenario))))
dev.off()

# Check stationarity for each scenario ------------------------------------
# Are the distributions the same along years for 10000 iterations?

# read the quantile summary file:

load(file.path(wd,"output_long","res_bio_all.RData"))

dat.bio.q$scenario <- gsub('HCR10_', 'HCR50_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR9_', 'HCR45_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR8_', 'HCR40_', dat.bio.q$scenario)
dat.bio.q$scenario <- gsub('HCR11_', 'ICES_low_', dat.bio.q$scenario)


# ssb

pdf(file.path(wd,"output_long","plots","stationarity_ssb.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=ssb_q50, ymin=ssb_q05, ymax=ssb_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("SSB(t)")+
             ggtitle(unique(.$scenario))))
dev.off()

# recruitment

pdf(file.path(wd,"output_long","plots","stationarity_rec.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=rec_q50, ymin=rec_q05, ymax=rec_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("Recruitment")+
             ggtitle(unique(.$scenario))))
dev.off()

# catch 

pdf(file.path(wd,"output_long","plots","stationarity_catch.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=catch_q50, ymin=catch_q05, ymax=catch_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("Catch (t)")+
             ggtitle(unique(.$scenario))))
dev.off()

# f

pdf(file.path(wd,"output_long","plots","stationarity_f.pdf"), width=9)
dat.bio.q %>%
  group_by(scenario) %>% 
  do(print(ggplot(data = .)+
             aes(x=year, y=f_q50, ymin=f_q05, ymax=f_q95)+
             geom_point()+
             geom_line()+
             geom_errorbar()+
             ylab("F")+
             ggtitle(unique(.$scenario))))
dev.off()

