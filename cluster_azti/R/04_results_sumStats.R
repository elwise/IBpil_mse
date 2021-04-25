################################################################################
#  IBpil results - summary statistics (joining scenarios)                      # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  03/10/2018                                                       #
#   modified: Laura Wise assess 2020                                           #
################################################################################

# Copyright: AZTI, 2018
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

t1 <- Sys.time()


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/"
setwd(wd)

# directory with results
res.dir <- file.path("./output")
#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

library(FLBEIA)
library(R.utils)
library(data.table)
library(tidyverse)


#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

#load(file.path(res.dir, "scenario_list.RData"))

#update<
files<-list.files(file.path(res.dir,"scenarios"))
scenario_list<-unique(sapply(gsub('.{6}$', '', files),function(x){paste(strsplit(x,"_")[[1]][2:6],collapse="_")}))
save(list=c("scenario_list"),file=file.path(res.dir, "scenario_list.RData"))

# names of the scenarios
scenario_list

#==============================================================================
#  Results:                                                                ----
#==============================================================================

# join results of all scenarios

# - bio.q
###########

dat.bio.q <- data.frame()

for (cs in scenario_list){
  
  # load file
  obj <- loadToEnv( file.path(res.dir,"scenarios",paste("results_",cs,".RData",sep="")))[["out.bio"]]
  
  obj$scenario <- as.character(cs)
  # combine all cases
  dat.bio.q <- rbind( dat.bio.q, as.data.frame(bioSumQ(obj)))
  rm(obj)
  
}

dat.bio.q <- dat.bio.q %>%separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

# Save data
save(dat.bio.q, file=file.path(res.dir,"res_bio_all.RData"))
rm(cs, dat.bio.q)


# - eco.q
###########

dat.eco.q <- data.frame()

for (cs in scenario_list){
  
  # load file
  obj <- loadToEnv( file.path(res.dir,"scenarios",paste("results_",cs,".RData",sep="")))[["out.flt"]]
  obj$scenario <- as.character(cs)
  # combine all cases
  dat.eco.q <- rbind( dat.eco.q, as.data.frame(fltSumQ(obj)))
  rm(obj)
  
}


dat.eco.q <- dat.eco.q %>%separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

# Save data
save(dat.eco.q, file=file.path(res.dir,"res_eco_all.RData"))
rm(cs, dat.eco.q)


# - adv.q
###########

dat.adv.q <- data.frame()

for (cs in scenario_list){
  
  # load file
  obj <- loadToEnv( file.path(res.dir,"scenarios",paste("results_",cs,".RData",sep="")))[["out.adv"]]
  obj$scenario <- as.character(cs)
  # combine all cases
  dat.adv.q <- rbind( dat.adv.q, as.data.frame(advSumQ(obj)))
  rm(obj)
  
}

dat.adv.q <- dat.adv.q %>%separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

# Save data
save( dat.adv.q, file=file.path(res.dir,"res_adv_all.RData"))
rm(cs, dat.adv.q)


