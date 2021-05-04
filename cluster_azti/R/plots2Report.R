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
dfyr <- dfyr[,c(1:7,9,19,29,10,20,30,11,21,31,13,23,33)]
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
  tidyr::spread_('Ass', 'q50') %>%
  mutate(ss3_relbias = (ASSss3 - ASSnone) / ASSnone) %>%
  as.data.frame -> relbias

# subset data.frame for variables of interest
relbias <- subset(relbias, indicator %in% c("ssb","f"))

##PLOTS for ASSss3 comparison with ASSnone
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+", 'HCR0'="ICES_med", 'HCR10'="HCR50", 
                 'HCR11'="ICES_low", 'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30")
#make sure order of variables is ssb and then f
relbias$indicator2 <- factor(relbias$indicator, levels=c("ssb","f"))

relbias$Rule <- factor(relbias$Rule, levels=c("HCR14", "HCR13","HCR8", "HCR9", "HCR10","HCR7", "HCR0", "HCR11"))

###Rule 0 and 11
aux.rule <- subset(relbias, Rule %in% c('HCR0','HCR11'))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = Rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(2020,2070,10))+
  facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("REClow", "REClowmed","RECmix"),
                        labels=c("Low", "LowMed","Mix"))
ggsave(paste0(plot.dir,'/',"relHCR0&HCR11.png"))

###Rule 8 to 14
aux.rule <- subset(relbias, Rule %in% c('HCR14','HCR13','HCR8','HCR9','HCR10'))
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = Rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(2020,2070,10))+
  facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("REClow", "REClowmed","RECmix"),
                        labels=c("Low", "LowMed","Mix"))+
  theme(axis.text=element_text(size=8))
ggsave(paste0(plot.dir,'/',"relHCR8to14.png"))

###Rule 7 
aux.rule <- subset(relbias, Rule %in% c('HCR7') & indicator2=='ssb')
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

ggplot(data=aux.rule,aes(x = year, y = ss3_relbias, colour = Rec)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(2020,2070,10))+
  facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
  scale_colour_discrete(name="Recruitment",breaks=c("REClow", "REClowmed","RECmix"),
                        labels=c("Low", "LowMed","Mix"))
ggsave(paste0(plot.dir,'/',"relHCR7.png"))

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
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+", 'HCR0'="ICES_med", 'HCR10'="HCR50", 
                 'HCR11'="ICES_low", 'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30",
                 'REClow' = 'REClow', 'REClowmed'= 'REClowmed','RECmix'='RECmix' )

##worm plots 

for (hcr in c("HCR14", "HCR13","HCR8", "HCR9", "HCR10","HCR7")){
  
  gg <- subset(ww, Rule == hcr)
  aa <- subset(out.iters, Rule == hcr)

for (rr in c("REClow","REClowmed","RECmix")){
  
  if (rr == 'RECmix'){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,Rec==rr)
  }
  
  else {
    aux <- subset(gg,Rec==rr & year < 2051)
    aux.iters <- subset(aa,Rec==rr & year < 2051)
    }
  
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~Rule,scales="free",labeller = as_labeller(facet_names))+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,max(aux$year),10))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(gg, indicator=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(gg,indicator=="ssb"),linetype="dashed",color="#00A08A")
  ggsave(paste0(plot.dir,'/',rr,"_",hcr,".png"),width = 5,height=6.65)
}

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

for (rule in c('HCR13','HCR14','HCR7','HCR8','HCR9','HCR10','HCR11','HCR0')){
  
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
    filter(year>2019)%>%
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


for (rule in c('HCR7','HCR8','HCR9','HCR10','HCR13','HCR14')){
  
  ss <- df %>%
    filter(
      (year < 2051 & !Rec %in% c("RECmix")) | 
      (year < 2071 & Rec == "RECmix")) %>%
    filter(Rule == rule & Ass == 'ASSss3')
  
  ggplot(data=ss)+
    geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=pblim,group=Rec,colour=Rec),size=1.5)+
    geom_hline(yintercept = 0.95, linetype = "dashed")+
    geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=pblow,group=Rec,colour=Rec),size=1.5)+
    ylab("P(B1+>=Blim)")+xlab("Year")+
    scale_x_continuous(name="Year",breaks = seq(2020,2070,5))+
    scale_color_brewer(palette="Dark2") +
    theme(
      axis.text=element_text(size=14),
    )
  ggsave(paste0(plot.dir,'/pblim_',rule,".png"),width=10)
  
##THESE PLOTS ARE NOT NECESSARY SINCE PZERO IS ALWAYS ZERO.
##FOR THE NO FISHING SCENARIO IS DOES NOT MAKE SENSE EITHER
  # ggplot(data=ss)+
  #   geom_line(aes(x=year,y=pzero, group=Rec, colour=Rec),size=1.2)+
  #   #ylim(c(0,1))+
  #   ylab("P(TAC = 0)")+xlab("Year")+
  #   scale_x_continuous(name="Year",breaks = seq(1978,2070,5))
  # ggsave(paste0(plot.dir,'/pzero_',rule,".png"))
  
}


  
  
