################################################################################
#  IBpil results - summary tables  for reference points                        # 
#------------------------------------------------------------------------------#
#   Laura Wise (IPMA lwise@ipma.pt)                                            #
#   created:  7/4/2021                                                         #
################################################################################

# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/"
setwd(wd)

# directory with results
res.dir  <- file.path("./output_refpts")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

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


#==============================================================================
#  Summary tables for all scenarios:                                       ----
#==============================================================================


# - year by year
#----------------------------------------------------------------------

proj.yr <- 2020
qs <- c("q95","q50","q05")

load(file.path(res.dir,"res_bio_all.RData"))    

out <- subset(dat.bio.q, year>=proj.yr)

write.table(out, file=file.path(res.dir, "stats_byyr.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm( qs, dat.bio.q)


#Run the function perfInd.pil the number of times needed to estimate stats for all the different periods

# - global
#-----------

#! NOTE: different Blim for each scenario!!!!!

#initial years of projection 2021:2026

out.all5 <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio",
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results_",cs,".RData",sep="")),
                      proj.yrs=2021:2026, Blim=337448, Blow=196334)
  
  out.all5 <- rbind(out.all5, obj)
  
}

out.all5 <- cbind(period=rep("initial",dim(out.all5)[1]),out.all5)

#first 10 years of projection 2021:2030

out.all10 <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio", 
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results_",cs,".RData",sep="")),
                      proj.yrs=2021:2030, Blim=337448, Blow=196334)
  
  out.all10 <- rbind(out.all10, obj)
  
}

out.all10 <- cbind(period=rep("short",dim(out.all10)[1]),out.all10)


#last 10 years of a 30 yr projection 2041:2050

out.all.last <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio",  
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results_",cs,".RData",sep="")),
                      proj.yrs=2041:2050, Blim=337448, Blow=196334)
  
  out.all.last <- rbind(out.all.last, obj)
  
}

out.all.last <- cbind(period=rep("last",dim(out.all.last)[1]),out.all.last)


# all projection period 2021:2050

out.all.all <- NULL

for (cs in scenario_list){
  
  obj <- perfInd.pil( obj.bio="out.bio",  
                      scenario=cs, file.dat=file.path(res.dir,"output_scenarios",paste("results_",cs,".RData",sep="")),
                      proj.yrs=2021:2050, Blim=337448, Blow=196334)
  
  out.all.all <- rbind(out.all.all, obj)
  
}

out.all.all <- cbind(period=rep("all",dim(out.all.all)[1]), out.all.all)


# put all performance stats (for all periods) together

out.all <- rbind(out.all5, out.all10, out.all.last, out.all.all)

# Separate scenario into different columns
out.final <-  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER", "Fscan"), sep = "_",  remove=FALSE) %>%
  mutate_at("Fscan", ~ as.numeric(sub("Fscan", "", .x)))
  
# Save data
write.table( out.final, file=file.path(res.dir,"stats.csv"), dec = ".", sep = ";",
             row.names = FALSE)
rm(cs, obj, out.all5, out.all10, out.all.last, out.all.all, out.all, out.final)
rm(perfInd.pil, auxiliary.f, tacdif)






