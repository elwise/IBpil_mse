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
res.dir  <- file.path("./output")


# List of scenarios with 1000 iterations ---------------------------------

scenario.list <- sub("results_(.*).RData", "\\1", list.files(file.path(".","output","scenarios")), perl = TRUE)
 #remove ICES rules and no fishing rule
idx <- which(sub(pattern=".*HCR *(.*?) *_REC.*", replacement="\\1", x=scenario.list) %in% c("0","11","7"))
scenario.list <- scenario.list[-idx]

# Ad-hoc function to compute prob equal/above cap  ------------------


get_scenario_cap <- function(sc, proj.yrs = 2021:2026) {
  out.bio <- loadToEnv(file.path(".","output",paste0("scenarios/results_",sc, ".RData")))[["out.bio"]]
  hcr <- sub(pattern=".*HCR *(.*?) *_REC.*", replacement="\\1", x=sc) # extract the HCR scenario to choose cap
  
  cap <- ifelse(hcr =="10",50000,ifelse(hcr=="9",45000,ifelse(hcr=="8",40000,ifelse(hcr=="13",35000,30000))))
         
  message("Scenario:", sc, '\nHCR:', hcr, ' Cap: ',cap)
  
  xx <- subset(out.bio, out.bio$year %in% proj.yrs)
  
  reachCap <- xx[, 'catch'] >= cap
  
  # R1: P(Cacth >= cap)
  r1 <- mean(reachCap)
  # R3: maximum anual P(Catch >= cap)
  r3 <- max(tapply(reachCap, list(xx$year), mean)) 
  
  # median catch
  r4 <- median(tapply(xx$catch, xx$iter, mean)) 
  
  
  return(c(meanProb = r1, maxProb = r3, medianCatch = r4, cap=cap, scenario=sc))
  
}



# apply for each scenario
out.capInit <- lapply(scenario.list, get_scenario_cap) %>% plyr::ldply()
out.capInit$period <- rep('initial', dim(out.capInit)[1])
out.capShort <- lapply(scenario.list, get_scenario_cap) %>% plyr::ldply()
out.capShort$period <- rep('short', dim(out.capShort)[1])
out.capMed <- lapply(scenario.list, get_scenario_cap) %>% plyr::ldply()
out.capMed$period <- rep('med', dim(out.capMed)[1])
out.capLast <- lapply(scenario.list, get_scenario_cap) %>% plyr::ldply()
out.capLast$period <- rep('last', dim(out.capLast)[1])

out.cap <- rbind(out.capInit, out.capShort, out.capMed, out.capLast)

out.cap <- out.cap %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)  

# save to file
save(out.cap, file=file.path(".","output","out.cap.RData"))
