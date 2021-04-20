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
library(ggplot2)
library(tidyr)
library(dplyr)
library(R.utils)

library(r4ss)
library(reshape2)

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

load(file.path(res.dir, "scenario_list.RData"))
length(scenario_list)

theme_set(
  theme_light()+
    theme(
      axis.text=element_text(size=10),
      axis.text.x = element_text(angle=90,vjust=1),
      axis.title=element_text(size=14,face="bold"),
      legend.text=element_text(size=14),
      text = element_text(size = 14),
      plot.background =	element_rect(colour = NA, fill = NA),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position="bottom"
    )
)

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


# data.frame for differences between runs with assessment and observation error with
# runs without assessment and no observation error

dfyr %>%
  dplyr::select(-scenario, -q05, -q95,-OER) %>%
  spread(Ass, q50) %>%
  mutate(ss3_relbias = (ASSss3 - ASSnone) / ASSnone) %>%
  as.data.frame -> relbias

# subset data.frame for variables of interest
relbias <- subset(relbias, indicator %in% c("ssb","f"))

##PLOTS for ASSss3 comparison with ASSnone
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+", 'HCR0'="HCR0", 'HCR10'="HCR10", 
                 'HCR11'="HCR11", 'HCR7'="HCR7" ,'HCR8'= "HCR8", 'HCR9'="HCR9")
#make sure order of variables is ssb and then f
relbias$indicator2 <- factor(relbias$indicator, levels=c("ssb","f"))

###Rule 0 and 11
aux.rule <- subset(relbias, Rule %in% c('HCR0','HCR11'))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

p <- ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = Rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
  facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("REClow", "REClowmed","RECmix"),
                        labels=c("Low", "LowMed","Mix"))
ggsave(paste0(plot.dir,'/',"relHCR0&HCR11.png"))

###Rule 8,9,10 
aux.rule <- subset(relbias, Rule %in% c('HCR8','HCR9','HCR10'))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

p <- ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = Rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
  facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("REClow", "REClowmed","RECmix"),
                        labels=c("Low", "LowMed","Mix"))
ggsave(paste0(plot.dir,'/',"relHCR8&HCR9&HCR10.png"))

###Rule 7 
aux.rule <- subset(relbias, Rule %in% c('HCR7') & indicator2=='ssb')
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

p <- ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = Rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
  facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("REClow", "REClowmed","RECmix"),
                        labels=c("Low", "LowMed","Mix"))
ggsave(paste0(plot.dir,'/',"relHCR7.png"))

# ############
# # Estimate 90% confidence interval for historical assessment
# ############
# 
# dir2020 <- c("D:/IPMA/SARDINE/WGHANSA2020/SS3/retrospective/2020/")
# 
# 
# #read ouputs and create database for ggplots
#   mydir <- dir2020
# 
#   setwd(mydir)
# 
#   out.ss3<-SS_output(mydir,forecast=FALSE,ncols=45)
#   dados<-SS_readdat(paste(mydir,"data.ss_new",sep=""), verbose = TRUE, echoall = FALSE, section = NULL,version = "3.30")
#   selectivity<-subset(out.ss3$ageselex[out.ss3$ageselex$Fleet==1 & out.ss3$ageselex$Factor=="Asel2" & out.ss3$ageselex$Yr %in% c(out.ss3$startyr:(out.ss3$endyr)),c("Yr","0","1","2","3","4","5","6") ])
# 
#   colnames(selectivity)[1]<-"year"
# 
#   sel<-melt(selectivity,id.vars='year')
# 
#   sel_bar<-rowMeans(selectivity[,4:7])
# 
#   startyr <- out.ss3$startyr
#   endyr <- out.ss3$endyr
#   aux <- out.ss3$derived_quants
#   idx <- match(paste("F_",startyr:(endyr-1),sep=""), aux[,1])
#   aux <- aux[idx, ]
# 
#   Fbar.dat <- data.frame(Year=startyr:(endyr-1),
#                          Value=aux$Value*sel_bar[-length(sel_bar)],
#                          CV=aux$StdDev/aux$Value*sel_bar[-length(sel_bar)],
#                          Lower=(aux$Value*sel_bar[-length(sel_bar)])-2*aux$StdDev,
#                          Upper=(aux$Value*sel_bar[-length(sel_bar)])+2*aux$StdDev,
#                          #sel_bar=sel_bar[-length(sel_bar)],
#                          #mean=mean(aux$Value),
#                          param="f")
# 
# 
#   aux <- out.ss3$derived_quants
#   idx <- match(paste("Recr_",startyr:(endyr-1),sep=""), aux[,1])
#   aux <- aux[idx, ]
# 
#   rec.dat <- data.frame(Year=startyr:(endyr-1),
#                         Value=aux$Value/1000000,
#                         CV=aux$StdDev/aux$Value,
#                         Lower=(aux$Value-2*aux$StdDev)/1000000,
#                         Upper=(aux$Value+2*aux$StdDev)/1000000,
#                         #sel_bar=sel_bar[-length(sel_bar)],
#                         #mean=mean(aux$Value),
#                         param="rec")
# 
#   #In our case we use Biomass1+ and not SSB.
#   #So we need to get values from the timeseries data.frame
#   #And use the StDev from the SSB since the model does not calculate the StDev from the Biomass1+
#   #aux <- out.ss3$derived_quants
#   #idx <- match(paste("SSB_",startyr:endyr,sep=""), aux[,1])
#   #aux <- aux[idx, ]
#   aux <- subset(out.ss3$timeseries, Era=="TIME",c("Yr","Bio_smry"))
#   idx <- grep("SSB_\\d",out.ss3$derived_quants$Label)
#   cv <- data.frame(cv=out.ss3$derived_quants[idx,"StdDev"]/out.ss3$derived_quants[idx,"Value"])
#   cv$Yr <- out.ss3$startyr:out.ss3$endyr
#   aux <- merge(aux,cv,by="Yr")
# 
#   bio.dat <- data.frame(Year=startyr:endyr,
#                         #Value=aux$Value,
#                         Value=aux$Bio_smry/1000,
#                         CV=aux$cv,
#                         Lower=(aux$Bio_smry-2*aux$cv*aux$Bio_smry)/1000,
#                         Upper=(aux$Bio_smry+2*aux$cv*aux$Bio_smry)/1000,
#                         #sel_bar=sel_bar,
#                         #mean=mean(aux$Bio_smry),
#                         param="ssb"
#   )
# 
#   landings <- dados$catch[dados$catch$year %in% c(startyr:(endyr-1)),4]/1000
# 
#   catch.dat <- data.frame(Year=startyr:(endyr-1),
#                           Value=landings,
#                           CV=rep(1,length(landings)),
#                           Lower=landings,
#                           Upper=landings,
#                           #sel_bar=sel_bar[-length(sel_bar)],
#                           #mean=mean(landings),
#                           param="catch"
# )
# 
#   all<-rbind(rec.dat,bio.dat,Fbar.dat,catch.dat)
#   all <- all[,-c(3)]
#  names(all) <- c("year","q50","q05","q95","indicator")
#  all <- subset(all,year %in% c(1978:2019))
# 
# save(all,file="ASSss3_2020Data.RData")
# rm(aux,bio.dat,cv,Fbar.dat,rec.dat,catch.dat,sel,selectivity,endyr,idx,mydir,out.ss3,sel_bar,startyr,dir2018,dados)

#get data from historical assessment
load("D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/data2020/ASSss3_2020Data.RData")

##make data.frame joining historical data with projection data 
dfyr %>%
  filter(year == 2020) %>%
  dplyr::select(-year, -q05, -q50, -q95) %>%
  left_join(all) %>% as.data.frame -> tt

dfyr %>%
  bind_rows(
    tt
  )%>% as.data.frame -> ww

ww <- subset(ww,Ass=="ASSss3")
# subset data.frame for variables of interest
ww <- subset(ww, indicator %in% c("rec","ssb","f","catch"))
ww$indicator2 <- factor(ww$indicator, levels=c("rec","ssb","f","catch"))

#Data from two iters
out.all <- NULL

for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,paste("scenarios/results_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux <- subset(aux, iter %in% c(45,235))
  aux <- subset(aux, year>2019)
  
  out.all <- rbind(out.all, aux)
  
}

out.iters <- 
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

#change units of variables of interest
out.iters[,10] <- out.iters[,10]/1000000
out.iters[,11] <- out.iters[,11]/1000
out.iters[,14] <- out.iters[,14]/1000

# reshape data 
out.iters <- melt(out.iters,id=1:9,variable.name = "indicator")
out.iters <- subset(out.iters,indicator %in% c("catch","f","rec","ssb"))
out.iters$indicator2 <- factor(out.iters$indicator,levels=c("rec","ssb","f","catch"))
out.iters <- subset(out.iters,Ass=="ASSss3")

##Now for the plots
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+", 'HCR0'="HCR0", 'HCR10'="HCR10", 
                 'HCR11'="HCR11", 'HCR7'="HCR7" ,'HCR8'= "HCR8", 'HCR9'="HCR9")
###For HCR7 
gg <- subset(ww, Rule %in% c('HCR7'))
aa <- subset(out.iters, Rule %in% c('HCR7'))

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_HCR7.png"),width = 3.92,height=6.65)
}


###For HCR8 
gg <- subset(ww, Rule %in% c('HCR8'))
aa <- subset(out.iters, Rule %in% c('HCR8'))

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_HCR8.png"),width = 3.92,height=6.65)
}

###For HCR9 
gg <- subset(ww, Rule %in% c('HCR9'))
aa <- subset(out.iters, Rule %in% c('HCR9'))

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_HCR9.png"),width = 3.92,height=6.65)
}

###For HCR10 
gg <- subset(ww, Rule %in% c('HCR10'))
aa <- subset(out.iters, Rule %in% c('HCR10'))

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_HCR10.png"),width = 3.92,height=6.65)
}

###For HCR0 
gg <- subset(ww, Rule %in% c('HCR0'))
aa <- subset(out.iters, Rule %in% c('HCR0'))

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_HCR0.png"),width = 3.92,height=6.65)
}

###For HCR11 
gg <- subset(ww, Rule %in% c('HCR11'))
aa <- subset(out.iters, Rule %in% c('HCR11'))

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_HCR11.png"),width = 3.92,height=6.65)
}


#######################
###Boxplots by year###
######################

#Data from scenario
out.all <- NULL
for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,paste("scenarios/results_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux <- subset(aux, year>2020)
  aux$scenario <- rep(cs,dim(aux)[1])
  out.all <- rbind(out.all, aux)
  
}

out.iters <- 
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_",  remove=FALSE)

#change units of variables of interest
out.iters[,10] <- out.iters[,10]/1000000
out.iters[,11] <- out.iters[,11]/1000
out.iters[,14] <- out.iters[,14]/1000

for (rule in c('HCR7','HCR8','HCR9','HCR10','HCR11','HCR0')){
  
aux <- subset(out.iters, Rule==rule)

p <- ggplot(data=aux,aes(x=year,y=ssb)) +
  geom_boxplot(aes(group = year)) +
  facet_grid(Rec~.,labeller = as_labeller(facet_names)) + 
  scale_x_continuous(name="Year",breaks = seq(2020,2070,5))+
  scale_y_continuous(breaks=scales::pretty_breaks(n = 6), name="Biomass (B1+)")

p + geom_hline(aes(yintercept = 337.448),linetype="dashed",color="#00A08A") +
  geom_hline(aes(yintercept = 196.334), linetype="dashed",color="#00A08A")

ggsave(paste0(plot.dir,'/boxplot_',rule,".png"),width = 3.92,height=6.65)

}

#==============================================================================
# Plots to see whether the conditions are met
#==============================================================================

# read data and compute pblim by year

successyr <- NULL
for (scenario in scenario_list){
  load(file.path(res.dir,paste("scenarios/results_",scenario,".RData",sep="")))
  aux <- out.bio %>% 
    separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_", remove=FALSE)%>%
    filter(year>2020)%>%
    group_by(year) %>% 
    summarize(pblim=sum(biomass>=337448)/length(biomass),
              pblow=sum(biomass>=196334)/length(biomass),
              pzero=sum(catch<= 1e-6)/length(catch))
  aux$scenario <- rep(scenario,dim(aux)[1])
  successyr <- rbind(successyr, as.data.frame(aux))
}

df <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_")

df$Rec2 <- factor(df$Rec, levels=c("REClow","REClowmed","RECmix"))

for (rule in c('HCR7','HCR8','HCR9','HCR10','HCR11','HCR0')){

ss <- subset(df, Rule == rule & Ass=='ASSss3')

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=pblim))+
  facet_grid(.~Rec2,labeller = as_labeller(facet_names))+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=pblow))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))
ggsave(paste0(plot.dir,'/pblim_',rule,".png"))

ggplot(data=ss)+
  geom_line(aes(x=year,y=pzero))+
  facet_grid(.~Rec2,labeller = as_labeller(facet_names))+
  ylim(c(0,1))+
  ylab("P(TAC = 0)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))
ggsave(paste0(plot.dir,'/pzero_',rule,".png"))

}


