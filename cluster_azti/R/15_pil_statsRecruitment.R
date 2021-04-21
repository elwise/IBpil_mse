#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./output")
# directory with plots
plot.dir <- file.path(res.dir,"plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(tidyverse)
library(R.utils)

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

load(file.path(res.dir, "scenario_list.RData"))


#Data from no fishing scenarios with assessment
noFish <- scenario_list[grep("ASSss3_HCR7",scenario_list)]


out.all <- NULL

for (cs in noFish){
  
  file.dat <- file.path(res.dir,paste("scenarios/results_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux <- subset(aux, year>2019)
  
  out.all <- rbind(out.all, aux)
  
  rm(aux)
  
}

out.iters <- 
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE) %>%
  filter(Ass == "ASSss3")

rm(out.all)

get_stats <- function(out.bio, scenario, proj.yrs = proj.yrs, Blow = 196334, Blim = 337448, meanR_low = 6993117 , 
                      meanR_med = 12346264, q975R_low = 16030832, q975R_med = 34380573, rec2019 = 16760900) {
  
  xx <- data.frame(subset(out.bio, year %in% proj.yrs & scenario == cs))
  
  # quantiles rec
  out <- c(quantile(xx[,'rec'],c(0.025,0.5,0.975)))
  
  # Mean rec
  tmp <- mean(xx[,'rec'])
  out <- c(out,tmp)
  
  # Median last rec
  tmp <- median(xx[xx$year==max(proj.yrs),'rec'])
  out <- c(out, tmp)
  
  belowBlim <- xx[, 'ssb'] < Blim
  # Risk 1: P(SSB < Blim)
  r1 <- mean(belowBlim)
  out <- c(out, r1)
  # Risk 2: P(SSB < Blim) at least once
  r2 <- mean(tapply(belowBlim, xx$iter, any))
  out <- c(out, r2)
  # Risk 3: maximum anual P(SSB < Blim)
  r3 <- max(tapply(belowBlim, list(xx$year), mean)) 
  out <- c(out, r3)
  
  belowBlow <- xx[, 'ssb'] < Blow
  # Risk 1: P(SSB < Blow)
  r4 <- mean(belowBlow)
  out <- c(out, r4)
  # Risk 2: P(SSB < Blow) at least once
  r5 <- mean(tapply(belowBlow, xx$iter, any))
  out <- c(out, r5)
  # Risk 3: maximum anual P(SSB < Blow)
  r6 <- max(tapply(belowBlow, list(xx$year), mean)) 
  out <- c(out, r6)
  
  
  aboveR_low <- xx[, 'rec'] > meanR_low
  # Average number of years that rec > meanR_low
  tmp <- mean (aboveR_low)
  out <- c(out, tmp)
  
  aboveQR_low <- xx[, 'rec'] > q975R_low
  # Average number of years that rec > q975R_low
  tmp <- mean (aboveQR_low)
  out <- c(out, tmp)
  
  aboveR_med <- xx[, 'rec'] > meanR_med
  # Average number of years that rec > meanR_med
  tmp <- mean (aboveR_med)
  out <- c(out, tmp)
  
  aboveQR_med <- xx[, 'rec'] > q975R_med
  # Average number of years that rec > q975R_med
  tmp <- mean (aboveQR_med)
  out <- c(out, tmp)
  
  above2019 <- xx[, 'rec'] > rec2019
  # Average number of years that rec > rec2019
  tmp <- mean (above2019)
  out <- c(out, tmp)
  
  out <- data.frame(t(out), row.names=NULL)
  
  names(out) <- c("q025","q50","q975","Mean","Median_Last_Yr",
                  "Risk1_med","Risk2_med", "Risk3_med", 
                  "Risk1_low","Risk2_low", "Risk3_low", 
                  "Risk1_meanR_low", "Risk1_Q975_low", 
                  "Risk1_meanR_med", "Risk1_Q975_med",
                  "Risk1_rec2019")
  
  out <- cbind(scenario,out)
  

  return(out)
}

#initial years of projection 2021:2025

out.all5 <- NULL

for (cs in noFish){
  
  obj <- get_stats( out.bio = out.iters, scenario = cs, proj.yrs=2021:2025)
  
  out.all5 <- rbind(out.all5, obj)
  
  rm(obj)
  
}

out.all5 <- cbind(period=rep("initial",dim(out.all5)[1]),out.all5)

#first 10 years of projection 2021:2030

out.all10 <- NULL

for (cs in noFish){
  
  obj <- get_stats( out.bio = out.iters, scenario = cs, proj.yrs=2021:2030)
  
  out.all10 <- rbind(out.all10, obj)
  
}

out.all10 <- cbind(period=rep("short",dim(out.all10)[1]),out.all10)


#up to 30 years of projection 2021:2050

out.all.med <- NULL

for (cs in noFish){
  
  obj <- get_stats( out.bio = out.iters, scenario = cs, proj.yrs=2041:2050)
  
  out.all.med <- rbind(out.all.med, obj)
  
}

out.all.med <- cbind(period=rep("med",dim(out.all.med)[1]),out.all.med)



#last 10 years of projection 2061:2070

out.all.last <- NULL

for (cs in noFish){
  
  obj <- get_stats( out.bio = out.iters, scenario = cs, proj.yrs=2061:2070)
  
  out.all.last <- rbind(out.all.last, obj)
  
}

out.all.last <- cbind(period=rep("last",dim(out.all.last)[1]),out.all.last)


# all projection period 2021:2070

out.all.all <- NULL

for (cs in noFish){
  
  obj <- get_stats( out.bio = out.iters, scenario = cs, proj.yrs=2021:2070)
  
  out.all.all <- rbind(out.all.all, obj)
  
}

out.all.all <- cbind(period=rep("all",dim(out.all.all)[1]), out.all.all)


# put all performance stats (for all periods) together

out.all <- data.frame(rbind(out.all5, out.all10, out.all.med, out.all.last, out.all.all))

# Separate scenario into different columns
out.final <-
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

rm(out.all, out.all.all, out.all.last, out.all.med, out.all10, out.all5)

# Save data
write.table( out.final, file=file.path(res.dir,"statsForRec.csv"), dec = ".", sep = ";",
             row.names = FALSE)

#period as an ordered factor for the figures

out.final$period <- factor(out.final$period, levels=c("initial","short","med","last","all"))

#save(out.final, file=file.path(paste0("D:/ICES/2021/PTSP Special request/Presentations/","statsForRec.RData")))

# reshape to the long format for ggplot

df <- out.final %>% pivot_longer(!c(period,scenario,Ass,Rule,Rec,INN,OER), names_to = "indicator", values_to = "value")

#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- unique(df$indicator)
perflabels <- unique(df$indicator)

# by indicator
pdf(file.path(plot.dir,"plot_compare.pdf"), onefile=T)
for (i in 1:length(perfnms)){
  ind <- perfnms[i]
  aux <- subset(df, indicator %in% ind)
  p <- ggplot(aux, aes(x=period, y=value, fill=period))+
    geom_bar(stat="identity")+
    facet_grid(. ~ Rec)+
    ylab(perflabels[i])
    print(p)
}
dev.off()

# by group of indicators
ggplot(out.final, aes(x=period, y=q50))+
  geom_point() +
  geom_pointrange(aes(ymin = q025, ymax = q975),color="grey50") +
  facet_grid(. ~ Rec, scale="free_y") +
  geom_point(aes(y=Mean, col="Mean")) +
  geom_point(aes(y=Median_Last_Yr, col="LY Median")) +
  ylab("Recruitment")
ggsave(file.path(plot.dir,"compare_quantiles.png"), height=4)


aux <- subset(df, indicator %in% c("Risk1_med","Risk2_med", "Risk3_med","Risk1_low","Risk2_low", "Risk3_low" ))
aux <- separate(aux, indicator, into = c("Risk", "Blim"))
ggplot(aux, aes(x=period, y=value*100, col = Rec))+
  geom_point()+
  facet_grid(Risk ~ Blim, scale="free_y", labeller =  label_both) +
  ylab("Probability")
ggsave(file.path(plot.dir,"compare_RiskSSB.png"), height=4)


aux <- subset(df, indicator %in% c("Risk1_meanR_low", "Risk1_Q975_low", 
                                   "Risk1_meanR_med", "Risk1_Q975_med",
                                   "Risk1_rec2019"))
facet_names <- c("Risk1_meanR_low" = "mR_low", "Risk1_Q975_low" ="q975_low", 
                 "Risk1_meanR_med" = "R_med" , "Risk1_Q975_med" = "q975_med",
                 "Risk1_rec2019" = "R2019" )

ggplot(aux, aes(x=period, y=value*100, col = Rec))+
  geom_point()+
  facet_grid(indicator ~ ., scale="free_y", labeller = as_labeller(facet_names)) +
  ylab("Probability")
ggsave(file.path(plot.dir,"compare_RiskRec.png"), height=4)


# read data

dfyr <- read.table(file.path(res.dir,"stats_byyr.csv"), header=T, sep=";")

#select only variables of interest
dfyr <- dfyr[,c(1:7,32:37,23:25,11:13)]
#change units of variables of interest
dfyr[,c(8:10)] <- dfyr[,c(8:10)]/1000000
dfyr[,c(11:13)] <- dfyr[,c(11:13)]/1000
dfyr[,c(17:19)] <- dfyr[,c(17:19)]/1000

# reshape data 
dfyr%>%
  gather("var_q", "value", -c(1:7)) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  tidyr::spread("quantile", "value") %>% as.data.frame -> dfyr



###For HCR7 Plots of Recruitment
gg <- subset(dfyr, Rule %in% c('HCR7') & Ass == "ASSss3" & indicator == "rec")


for (rr in c("REClow","REClowmed","RECmix")){
  
  meanR_low <- 6993117/1000000
  q975R_low <- 16030832/1000000
  
  
  meanR_med <- 12346264/1000000
  q975R_med <- 34380573/1000000
  
  r2019 <- 16760900/1000000
  
  aux <- subset(gg,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6)) +
    geom_hline(aes(yintercept = meanR_med),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = meanR_low),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = q975R_med), linetype="dashed",color="red") +
    geom_hline(aes(yintercept = q975R_low), linetype="dashed",color="red") +
    geom_hline(aes(yintercept = r2019), linetype="dashed",color="black") +
    annotate("text", 2060, meanR_low, vjust = 0.2, hjust=0.2,label = "Mean_R_low_prod",parse=T,size=3,color="grey30") +
    annotate("text", 2060, q975R_low, vjust = 0.2, hjust=0.2,label = "Quantile97.5_low_prod",parse=T,size=3,color="grey30") +
    annotate("text", 2060, meanR_med, vjust = 0.2, hjust=0.2,label = "Mean_R_med_prod",parse=T,size=3,color="grey30") +
    annotate("text", 2060, q975R_med, vjust = 0.2, hjust=0.2,label = "Quantile97.5_med_prod",parse=T,size=3,color="grey30") +
    annotate("text", 2060, r2019, vjust = 0.2, hjust=0.2,label = "Recruitment_of_2019",parse=T,size=3,color="grey30")


  ggsave(paste0(plot.dir,'/',rr,"_HCR7_Recruitment.png"),height=4)
}


