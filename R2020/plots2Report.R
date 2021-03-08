#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/" # main directory
setwd(wd)

# directory with results
res.dir  <- file.path("./output2020")
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

dfyr <- read.table(file.path(res.dir,"stats_byyr2020.csv"), header=T, sep=";")

#select only variables of interest
dfyr <- dfyr[,c(1:8,33:38,24:26,12:14)]
#change units of variables of interest
dfyr[,c(9:11)] <- dfyr[,c(9:11)]/1000000
dfyr[,c(12:14)] <- dfyr[,c(12:14)]/1000
dfyr[,c(18:20)] <- dfyr[,c(18:20)]/1000

# reshape data 
dfyr%>%
  gather("var_q", "value", -c(1:8)) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  tidyr::spread("quantile", "value") %>% as.data.frame -> dfyr


# data.frame for differences between runs with assessment and observation error with
# runs without assessment and no observation error

subset(dfyr,Rec=="REClowmed") %>%
  dplyr::select(-scenario, -q05, -q95,-OER) %>%
  spread(Ass, q50) %>%
  mutate(ss3_relbias = (ASSss3 - ASSnone) / ASSnone) %>%
  as.data.frame -> relbias

# subset data.frame for variables of interest
relbias <- subset(relbias, indicator %in% c("ssb","f"))

##PLOTS for ASSss3 comparison with ASSnone
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+",'HCR8'="HCR")
#make sure order of variables is ssb and then f
relbias$indicator2 <- factor(relbias$indicator, levels=c("ssb","f"))

###rule 8
aux.rule <-relbias
aux.rule$ss3_relbias[!is.finite(aux.rule$ss3_relbias)] <- NA

ggplot(data=aux.rule,aes(x = year, y = ss3_relbias,colour=catch)) +
  geom_line() + ylab("ASSss3 relative to ASSnone")+
  scale_x_continuous(name="Year",breaks = seq(2020,2070,5))+
  facet_grid(indicator2~.,scales="free",labeller = as_labeller(facet_names))+
  
ggsave("HCRrel.png",path=plot.dir)


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
load("D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/data/ASSss3_2020Data.RData")

##make data.frame joining historical data with projection data 
dfyr %>%
  filter(year == 2020) %>%
  dplyr::select(-year, -q05, -q50, -q95) %>%
  left_join(all) %>% as.data.frame -> tt

dfyr %>%
  bind_rows(
    tt
  )%>% as.data.frame -> ww

#ww <- subset(ww,assessment=="ss3")
ww$indicator2 <- factor(ww$indicator, levels=c("rec","ssb","f","catch"))

#Data from two iters
out.all <- NULL
for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,paste("scenarios/results2020_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux <- subset(aux, iter %in% c(45,235))
  aux <- subset(aux, year>2019)
  aux$scenario <- rep(cs,dim(aux)[1])
  out.all <- rbind(out.all, aux)
  
}

out.all$scenario <- plyr::mapvalues(out.all$scenario,from= c("ASSnone_HCR8_REClowmed_INNvar_OERnonecatch40",
                                                             "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch45",
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch40"  ,
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch45",
                                                             "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch50"),
                                    to=c("ASSnone_HCR8_REClowmed_INNvar_OERnone_catch40",
                                         "ASSnone_HCR8_REClowmed_INNvar_OERnone_catch45",
                                         "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch40" ,
                                         "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch45",
                                         "ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch50") )



out.iters <- 
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","catch"), sep = "_",  remove=FALSE)

#change units of variables of interest
out.iters[,15] <- out.iters[,15]/1000000
out.iters[,16] <- out.iters[,16]/1000
out.iters[,14] <- out.iters[,14]/1000

# reshape data 
out.iters <- melt(out.iters,id=1:10,variable.name = "indicator")
names(out.iters)[11] <- "indicator"
out.iters <- subset(out.iters,indicator %in% c("catch","f","rec","ssb"))
out.iters$indicator2 <- factor(out.iters$indicator,levels=c("rec","ssb","f","catch"))
out.iters <- subset(out.iters,Ass=="ASSss3")

##Now for the plots
###For HCR8 without assessment
gg <- subset(ww, Ass!="ASSss3")
aa <- subset(out.iters, Ass!="ss3")

for (rr in c("REClow","REClowmed","RECmix")){
  aux <- subset(gg,Rec==rr)
  aux.iters <- subset(aa,rec_sc==substr(rr,4,12))
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~catch,scales="free")+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Year",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
    
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(ww, indicator2=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(ww,indicator2=="ssb"),linetype="dashed",color="#00A08A")
  
  ggsave(paste0(rr,"_HCR8.png"))
}

###For HCR8 with assessment
gg <- subset(ww, Ass=="ASSss3")
aa <- subset(out.iters, Ass=="ASSss3")

for (rr in c("catch40","catch45","catch50")){
  aux <- subset(gg,catch==rr)
  aux.iters <- subset(aa,catch==rr)
  p <- ggplot(data=aux,aes(x=year, y=q50))+
    geom_ribbon(data=aux,aes(ymin = q05, ymax = q95),alpha=0.2,show.legend = F,fill="#F9840044") +
    geom_line(data=aux,color="#F98400")+
    geom_line(data=subset(aux.iters,iter==45),color="green3",aes(x=year,y=value),alpha=0.6)+
    geom_line(data=subset(aux.iters,iter==235),color="blue",aes(x=year,y=value),alpha=0.6)+
    facet_grid(indicator2~.,scales="free")+
    geom_vline(xintercept = 2020, linetype = "dotted",color="#00A08A")+
    ylab("") +
    scale_x_continuous(name="Ano",breaks = seq(1978,2070,5))+
    scale_y_continuous(breaks=scales::pretty_breaks(n = 6))
  
  
  p + geom_hline(aes(yintercept = 337.448), data = subset(ww, indicator2=="ssb"),linetype="dashed",color="#00A08A") +
    geom_hline(aes(yintercept = 196.334), data = subset(ww,indicator2=="ssb"),linetype="dashed",color="#00A08A")
  
  ggsave(paste0(rr,"_HCR_ss3.png"),path=plot.dir)
}

#######################
###Boxplots by year###
######################

#Data from scenario
out.all <- NULL
for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,paste("scenarios/results2020_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  #aux <- subset(aux, iter %in% c(45,235))
  aux <- subset(aux, year>2020)
  aux$scenario <- rep(cs,dim(aux)[1])
  out.all <- rbind(out.all, aux)
  
}

facet_names <- c("ASSnone_HCR8_REClowmed_INNvar_OERnonecatch40" = 'none40', "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch45"= 'none45',
                 "ASSnone_HCR8_REClowmed_INNvar_OERnonecatch50"= 'none50', "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch40"  = 'ss40',
                 "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch45" = 'ss45',  "ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch50" = 'ss50' )

p <- ggplot(subset(out.all, scenario %in% c(scenario_list[4:6])), aes(x=year,y=biomass/1000)) +
  geom_boxplot(aes(group = year)) +
  facet_grid(scenario~.,labeller = as_labeller(facet_names)) + 
  scale_x_continuous(name="Year",breaks = seq(2020,2070,5))+
  scale_y_continuous(breaks=scales::pretty_breaks(n = 6), name="Biomass (B1+)")

p + geom_hline(aes(yintercept = 337.448), linetype="dashed",color="#00A08A") +
  geom_hline(aes(yintercept = 446.331), linetype="dashed",color="#00A08A")




#==============================================================================
# Plots to see whether the conditions are met
#==============================================================================

# read data and compute pblim and p80blim by year

successyr <- NULL
for (cs in scenario_list){
  
  file.dat <- file.path(res.dir,paste("scenarios/results2020_",cs,".RData",sep=""))
  out <- loadToEnv(file.dat)[["out.bio"]]
  out$scenario <- rep(cs,dim(out)[1])
  out$scenario <- plyr::mapvalues(out$scenario, from="ASSss3_HCR8_REClowmed_INNvar_OERnaqcatch40", to="ASSss3_HCR8_REClowmed_INNvar_OERnaq_catch40")
  aux <- out %>% 
    separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","catch"), sep = "_")%>%
    filter(year>2019)%>%
    group_by(year) %>% 
    summarize(pblim=sum(biomass>=337448)/length(biomass),
              p80blim=sum(biomass>= 0.8 * 337448)/length(biomass),
              pblow=sum(biomass>=196334)/length(biomass),
              p80blow=sum(biomass>= 0.8 * 196334)/length(biomass),
              pzero=sum(catch<= 1e-6)/length(catch))
  aux$scenario <- rep(unique(out$scenario),dim(aux)[1])

  successyr <- rbind(successyr, as.data.frame(aux))
  }

df <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","catch"), sep = "_")%>%
  filter(year>2020)

df$Rec2 <- factor(df$Rec, levels=c("RECmed","REClow","REClowmed","RECmix"))

ggplot(data=df)+
  geom_line(data=subset(df, Rec!="REClow"),aes(x=year,y=pblim,colour=catch))+
  #facet_grid(.~catch)+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))
ggsave("pblim_HCR_Assss3Colour.png",path=plot.dir)

ss <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_")%>%
  filter(Ass=="ASSnone")

ggplot(data=ss)+
  geom_line(data=subset(ss, Rec!="REClow"),aes(x=year,y=pblim))+
  facet_grid(.~Rec)+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  geom_line(data=subset(ss, Rec=="REClow"),aes(x=year,y=pblow))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))
ggsave("pblim_HCR8_AssNone.png")

dd <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER"), sep = "_")%>%
  filter(Rec=="REClowmed")
  
ggplot(data=dd)+
  geom_line(aes(x=year,y=pblim,colour=Ass))+
  facet_grid(.~Rec)+
  geom_hline(yintercept = 0.95, linetype = "longdash")+
  ylim(c(0,1))+
  ylab("P(B1+>=Blim)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2070,5))
ggsave("pblim_HCR8_lowmed.png")


####
ss <- subset(df, Rec=="REClow" & Rule %in% c("HCR1","HCR2"))
maxP <- max(ss$pzero)

ggplot(data=ss)+
  geom_line(aes(x=year,y=pzero))+
  facet_grid(Rule~.)+
  #geom_hline(yintercept = 0.90, linetype = "longdash")+
  #ylim(c(0,maxP))+
  ylim(c(0,1))+
  ylab("P(TAC = 0)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("pzero_HCR12.pdf")

ss <- subset(df, Rec=="REClow" & Rule %in% c("HCR3","HCR4"))
maxP <- max(ss$pzero)

ggplot(data=ss)+
  geom_line(aes(x=year,y=pzero))+
  facet_grid(Rule~.)+
  #geom_hline(yintercept = 0.90, linetype = "longdash")+
  #ylim(c(0,maxP))+
  ylim(c(0,1))+
  ylab("P(TAC = 0)")+xlab("Year")+
  scale_x_continuous(name="Year",breaks = seq(1978,2048,5))
ggsave("pzero_HCR34.pdf")

