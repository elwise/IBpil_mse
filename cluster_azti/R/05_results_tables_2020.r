################################################################################
#  IBpil results - summary tables                                             # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  11/10/2018                                                       #
#   modified:                                                                  #
################################################################################

# ibpil_results_tables_2018.r - summary plots for all scenarios
# msePIL8c9a/ibpil_results_tables_2018.r

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

#wd <- "C:/use/GitHub/FLBEIA_mseIBpil/" # main directory
#wd <- "~/Documents/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil"
wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil"
setwd(wd)

# directory with results
#res.dir  <- file.path("./output")
res.dir  <- file.path("./output2020")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
# library(ggplot2)
# library(FLBEIA)#, lib.loc="./Rlibs/")
library(R.utils)


#==============================================================================
# FUNCTIONS                                                               ----
#==============================================================================

source("./R/fun/ibpil_perfInd.R")

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios
load(file.path(res.dir, "scenario_list.RData"))

#length(scenario_list)


#==============================================================================
#  Summary tables for all scenarios:                                       ----
#==============================================================================


# - year by year
#----------------------------------------------------------------------

proj.yr <- 2020
qs <- c("q95","q50","q05")

load(file.path(res.dir,"res_bio_all2020.RData"))    # dat.bio.q
# dat.bio.q$stock <- NULL

load( file.path(res.dir,"res_eco_all2020.RData"))    # dat.eco.q
dat.eco.q <- dat.eco.q[,c("year","scenario",paste("effort",qs,sep="_"))]

load( file.path(res.dir,"res_adv_all2020.RData"))    # dat.adv.q
dat.adv.q <- dat.adv.q[,c("year","scenario",paste("tac",qs,sep="_"))]  #,paste("quotaUpt",qs,sep="_")

out.byr <- merge(dat.bio.q, dat.eco.q, by=c("scenario","year"))
out.byr <- merge(out.byr, dat.adv.q, by=c("scenario","year"))

# Separate scenario into different columns
out.byr$scenario <- plyr::mapvalues(out.byr$scenario,from= c("ASSnone_HCR8_REClowmed_INNvar_OERnonecatch40",
                                                             "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch45",
                                                             "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch50",
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch40"  ,
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch45",
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch50",
                                                             "ASSnone_HCR8_REClow_INNvar_OERnone",
                                                             "ASSss3_HCR8_REClow_INNvar_OERnaqcatch50",
                                                             "ASSss3_HCR8_REClow_INNvar_OERnaqcatch45"),
                                 to=c("ASSnone_HCR8_REClowmed_INNvar_OERnone_catch40",
                                      "ASSnone_HCR8_REClowmed_INNvar_OERnone_catch45",
                                      "ASSnone_HCR8_REClowmed_INNvar_OERnone_catch50",
                                      "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch40" ,
                                      "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch45",
                                      "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch50",
                                      "ASSnone_HCR8_REClow_INNvar_OERnone_catch50",
                                      "ASSss3_HCR8_REClow_INNvar_OERnaq_catch50",
                                      "ASSss3_HCR8_REClow_INNvar_OERnaq_catch45") )
out <- out.byr %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","catch"), sep = "_",  remove=FALSE)


out <- subset(out, year>=proj.yr)


write.table( out, file=file.path(res.dir, "stats_byyr2020.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm( qs, dat.bio.q, dat.eco.q, dat.adv.q)


# Projection years
# proj.yrs <- unique(out$year)

#Run the function perfInd.pil the number of times needed to estimate stats for all the different periods

# - global
#-----------

#! NOTE: different Blim for each scenario!!!!!

#initial years of projection 2021:2025

out.all5 <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio",
                      scenario=cs, file.dat=file.path(res.dir,"scenarios",paste("results2020_",cs,".RData",sep="")),
                      proj.yrs=2021:2025, Blim=337448, Blow=196334)
  
  out.all5 <- rbind(out.all5, obj)
  
}

out.all5 <- cbind(period=rep("initial",dim(out.all5)[1]),out.all5)

#first 10 years of projection 2021:2030

out.all10 <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", 
                      scenario=cs, file.dat=file.path(res.dir,"scenarios",paste("results2020_",cs,".RData",sep="")),
                      proj.yrs=2021:2030, Blim=337448, Blow=196334)
  
  out.all10 <- rbind(out.all10, obj)
  
}

out.all10 <- cbind(period=rep("short",dim(out.all10)[1]),out.all10)

#last 10 years of projection 2061:2070

out.all.last <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio",  
                      scenario=cs, file.dat=file.path(res.dir,"scenarios",paste("results2020_",cs,".RData",sep="")),
                      proj.yrs=2061:2070, Blim=337448, Blow=196334)
  
  out.all.last <- rbind(out.all.last, obj)
  
}

out.all.last <- cbind(period=rep("last",dim(out.all.last)[1]),out.all.last)


# all projection period 2019:2048

out.all.all <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio",  
                      scenario=cs, file.dat=file.path(res.dir,"scenarios",paste("results2020_",cs,".RData",sep="")),
                      proj.yrs=2021:2070, Blim=337448, Blow=196334)
  
  out.all.all <- rbind(out.all.all, obj)
  
}

out.all.all <- cbind(period=rep("all",dim(out.all.all)[1]), out.all.all)


# put all performance stats (for all periods) together

out.all <- rbind(out.all5, out.all10, out.all.last, out.all.all)

# Separate scenario into different columns
out.all$scenario <- plyr::mapvalues(out.all$scenario,from= c("ASSnone_HCR8_REClowmed_INNvar_OERnonecatch40",
                                                             "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch45",
                                                             "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch50",
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch40"  ,
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch45",
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch50"),
                                    to=c("ASSnone_HCR8_REClowmed_INNvar_OERnone_catch40",
                                         "ASSnone_HCR8_REClowmed_INNvar_OERnone_catch45",
                                         "ASSnone_HCR8_REClowmed_INNvar_OERnone_catch50",
                                         "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch40" ,
                                         "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch45",
                                         "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch50") )

out.final <-
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","catch"), sep = "_",  remove=FALSE)


# Save data
write.table( out.final, file=file.path(res.dir,"stats2020.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm( cs, obj, out.all5, out.all10, out.all.last, out.all.all, out.all, out.final)
rm( perfInd.pil, auxiliary.f, tacdif)







