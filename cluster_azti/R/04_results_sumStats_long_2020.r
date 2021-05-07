################################################################################
#  IBpil results - summary statistics (joining scenarios)                     # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  03/10/2018                                                       #
#   modified: Laura Wise assess 2018                                           #
################################################################################

# ibpil_results_sumStats_2018.r - summary statistics for all scenarios
# msePIL8abd/ibpil_results_sumStats_2018.r

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
#wd <- "C:/Use/GitHub/IBpil_mse/cluster_azti/"
setwd(wd)

# directory with results
res.dir <- file.path("./output_long")
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

# Convert year in numeric
dat.bio.q$year    <- as.numeric(as.character(dat.bio.q$year))

# Separate scenario into different columns
# scenario   <- as.character(dat.bio.q$scenario)
# assessment <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[1], 4, nchar(x[1]))))
# rule       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[2], 4, nchar(x[2]))))
# rec        <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[3], 4, nchar(x[3]))))
# initNage   <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[4], 4, nchar(x[4]))))
# obsErr     <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[5], 4, nchar(x[4]))))
# ftgt       <- unlist(lapply( strsplit( scenario, "_"), function(x) substr(x[6], 6, nchar(x[5]))))
# 
# dat.bio.q <- cbind( scenario, assessment, rule, rec, initNage, obsErr, dat.bio.q[,-c(1,3)])
dat.bio.q <- dat.bio.q %>% separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

# Save data
save( dat.bio.q, file=file.path(res.dir,"res_bio_all.RData"))
rm( cs, dat.bio.q)


