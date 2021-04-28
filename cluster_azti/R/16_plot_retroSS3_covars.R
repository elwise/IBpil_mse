################################################################################
#  IBpil: look at retrospective estimates from ss32flbeia                     # 
#------------------------------------------------------------------------------#
#   Leire Citores (AZTI-Tecnalia)                                              #
#   created:  12/04/2019                                                       #
#                                                                              #
################################################################################

library(tidyverse)
library(FLCore)
library(r4ss)

wd <- "D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/" # main directory

# directory with results
res.dir  <- file.path("D:/IPMA/SARDINE/ADVICE_MP/FLBEIA_mseIBpil/2020/cluster_azti/output/output_iters")
res.plots <- paste0(wd,"output/plots")

setwd(res.dir)


ni<-1000
files<-list.files()

#CHECK SCENARIO YOU WANT TO RUN
#select scenario 
sc<-"ASSss3_HCR10_REClowmed_INNvar_OERnaq"
sc1 <- "ASSss3_HCR50_REClowmed_INNvar_OERnaq" #change number rule to max catch

##read individual runs
files_sc<-files[grep(sc, files)]
ni<-min(ni,length(files_sc))

load(files_sc[1])
covars<-get(sc)$covars

covars_all<-lapply(covars,FLCore::expand,iter=1:ni)
covars_all$real_ssb<-covars_all$ssb*NA
covars_all$real_rec<-covars_all$rec*NA
covars_all$real_fbar <- covars_all$fbar*NA
covars_all$indices<-covars_all$qs*NA

#summary of ni iters

for(i in 1:ni){
  print(i)
  load(files_sc[i])
  covars<-get(sc)$covars
  covars_all$ssb[,,,,,i]<-covars$ssb[,,,,,1]
  covars_all$rec[,,,,,i]<-covars$rec[,,,,,1]
  covars_all$fbar[,,,,,i]<-covars$fbar[,,,,,1]
  covars_all$sel[,,,,,i]<-covars$sel[,,,,,1]
  covars_all$qs[,,,,,i]<-covars$qs[,,,,,1]
  covars_all$conv[,,,,,i]<-covars$conv[,,,,,1]
  covars_all$real_ssb[,,,,,i]<-ssb(get(sc)$biols$PIL)
  covars_all$real_rec[,,,,,i]<-get(sc)$biols$PIL@n[1,]
  covars_all$real_fbar[,,,,,i]<-FLCore::expand(fbar(get(sc)$biols$PIL),year= 1978:2070)
  covars_all$ssb[ac(2070),,,,,i]<-ssb(get(sc)$biols$PIL)
  covars_all$rec[ac(2070),,,,,i]<-get(sc)$biols$PIL@n[1,]
  covars_all$fbar[ac(2070),,,,,i]<-FLCore::expand(fbar(get(sc)$biols$PIL),year= 1978:2070)
  covars_all$indices[1,,,,,i]<-quantSums(get(sc)$indices$PIL$AcousticNumberAtAge@index)
  covars_all$indices[2,,,,,i]<-get(sc)$indices$PIL$DEPM@index
}

save.image(file=paste0(wd,"output/biasData_",sc,".RData"))

#For SSB

ggplot(as.data.frame(covars_all$ssb[,,,,,]/covars_all$real_ssb[,,,,,]),aes(year,data,group=eval.year))+
  stat_summary(fun = median,
               geom = "line",
               aes(colour=eval.year))+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 2019,linetype="dashed")+
  ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim ssb/real ssb")
  ggsave(paste0(res.plots,"/","biasRelativeSSB_",sc1,"_",ni,"its.png"))
  
  as.data.frame(covars_all$ssb[ac(2020:2039),,,,,]/covars_all$real_ssb[ac(2020:2039),,,,,]) %>%
  ggplot(.,aes(year,data,group=eval.year))+
    facet_wrap(~eval.year)+
    stat_summary(fun.min = function(x) quantile(x,0.025),
                 fun.max = function(x) quantile(x,0.975),
                 geom = "ribbon",alpha=0.2,col=NA) +
    stat_summary(fun = median,
                 geom = "line",colour="red")+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim ssb/real ssb")
  ggsave(paste0(res.plots,"/","biasRelativeSSB_2020_2039_",sc1,"_",ni,"its.png"))
  
  as.data.frame(covars_all$ssb[ac(2040:2059),,,,,]/covars_all$real_ssb[ac(2040:2059),,,,,]) %>%
    ggplot(.,aes(year,data,group=eval.year))+
    facet_wrap(~eval.year)+
    stat_summary(fun.min = function(x) quantile(x,0.025),
                 fun.max = function(x) quantile(x,0.975),
                 geom = "ribbon",alpha=0.2,col=NA) +
    stat_summary(fun = median,
                 geom = "line",colour="red")+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim ssb/real ssb")
  ggsave(paste0(res.plots,"/","biasRelativeSSB_2040_2059_",sc1,"_",ni,"its.png"))
  
  as.data.frame(covars_all$ssb[ac(2060:2069),,,,,]/covars_all$real_ssb[ac(2060:2069),,,,,]) %>%
    ggplot(.,aes(year,data,group=eval.year))+
    facet_wrap(~eval.year)+
    stat_summary(fun.min = function(x) quantile(x,0.025),
                 fun.max = function(x) quantile(x,0.975),
                 geom = "ribbon",alpha=0.2,col=NA) +
    stat_summary(fun = median,
                 geom = "line",colour="red")+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim ssb/real ssb")
  ggsave(paste0(res.plots,"/","biasRelativeSSB_2060_2069_",sc1,"_",ni,"its.png"))
  
#For Fbar
  ggplot(as.data.frame(covars_all$fbar[,,,,,]/covars_all$real_fbar[,,,,,]),aes(year,data,group=eval.year))+
    stat_summary(fun = median,
                 geom = "line",
                 aes(colour=eval.year))+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim fbar/real fbar")
  ggsave(paste0(res.plots,"/","biasRelativeFbar_",sc1,"_",ni,"its.png"))
  
  as.data.frame(covars_all$fbar[ac(2020:2039),,,,,]/covars_all$real_fbar[ac(2020:2039),,,,,]) %>%
    ggplot(.,aes(year,data,group=eval.year))+
    facet_wrap(~eval.year)+
    stat_summary(fun.min = function(x) quantile(x,0.025),
                 fun.max = function(x) quantile(x,0.975),
                 geom = "ribbon",alpha=0.2,col=NA) +
    stat_summary(fun = median,
                 geom = "line",colour="red")+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim fbar/real fbar")
  ggsave(paste0(res.plots,"/","biasRelativeFbar_2020_2039_",sc1,"_",ni,"its.png"))
  
  as.data.frame(covars_all$fbar[ac(2040:2059),,,,,]/covars_all$real_fbar[ac(2040:2059),,,,,]) %>%
    ggplot(.,aes(year,data,group=eval.year))+
    facet_wrap(~eval.year)+
    stat_summary(fun.min = function(x) quantile(x,0.025),
                 fun.max = function(x) quantile(x,0.975),
                 geom = "ribbon",alpha=0.2,col=NA) +
    stat_summary(fun = median,
                 geom = "line",colour="red")+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim fbar/real fbar")
  ggsave(paste0(res.plots,"/","biasRelativeFbar_2040_2059_",sc1,"_",ni,"its.png"))
  
  as.data.frame(covars_all$fbar[ac(2060:2069),,,,,]/covars_all$real_fbar[ac(2060:2069),,,,,]) %>%
    ggplot(.,aes(year,data,group=eval.year))+
    facet_wrap(~eval.year)+
    stat_summary(fun.min = function(x) quantile(x,0.025),
                 fun.max = function(x) quantile(x,0.975),
                 geom = "ribbon",alpha=0.2,col=NA) +
    stat_summary(fun = median,
                 geom = "line",colour="red")+
    geom_hline(yintercept = 1)+
    geom_vline(xintercept = 2019,linetype="dashed")+
    ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim fbar/real fbar")
  ggsave(paste0(res.plots,"/","biasRelativeFbar_2060_2069_",sc1,"_",ni,"its.png"))

#For Recruitment
ggplot(as.data.frame(covars_all$rec[,,,,,]/covars_all$real_rec[,,,,,]),aes(year,data,group=eval.year))+
  stat_summary(fun = median,
               geom = "line",
               aes(colour=eval.year))+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 2019,linetype="dashed")+
  ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim rec/real rec")
ggsave(paste0(res.plots,"/","biasRelativeREC_",sc1,"_",ni,"its.png"))

as.data.frame(covars_all$rec[ac(2020:2039),,,,,]/covars_all$real_rec[ac(2020:2039),,,,,]) %>%
  ggplot(.,aes(year,data,group=eval.year))+
  facet_wrap(~eval.year)+
  stat_summary(fun.min = function(x) quantile(x,0.025),
               fun.max = function(x) quantile(x,0.975),
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun = median,
               geom = "line",colour="red")+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 2019,linetype="dashed")+
  ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim rec/real rec")
ggsave(paste0(res.plots,"/","biasRelativeRec_2020_2039_",sc1,"_",ni,"its.png"))

as.data.frame(covars_all$rec[ac(2040:2059),,,,,]/covars_all$real_rec[ac(2040:2059),,,,,]) %>%
  ggplot(.,aes(year,data,group=eval.year))+
  facet_wrap(~eval.year)+
  stat_summary(fun.min = function(x) quantile(x,0.025),
               fun.max = function(x) quantile(x,0.975),
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun = median,
               geom = "line",colour="red")+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 2019,linetype="dashed")+
  ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim rec/real rec")
ggsave(paste0(res.plots,"/","biasRelativeRec_2040_2059_",sc1,"_",ni,"its.png"))

as.data.frame(covars_all$rec[ac(2060:2069),,,,,]/covars_all$real_rec[ac(2060:2069),,,,,]) %>%
  ggplot(.,aes(year,data,group=eval.year))+
  facet_wrap(~eval.year)+
  stat_summary(fun.min = function(x) quantile(x,0.025),
               fun.max = function(x) quantile(x,0.975),
               geom = "ribbon",alpha=0.2,col=NA) +
  stat_summary(fun = median,
               geom = "line",colour="red")+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 2019,linetype="dashed")+
  ggtitle(sc1)+ylim(0.75,1.75)+ylab("estim rec/real rec")
ggsave(paste0(res.plots,"/","biasRelativeRec_2060_2069_",sc1,"_",ni,"its.png"))

#other variables

ggplot(as.data.frame(covars_all$qs[,,,,,]),aes(year,data,col=qs,fill=qs))+#facet_wrap(~eval.year)+
  stat_summary(fun.min = function(x) quantile(x,0.05),
               fun.max = function(x) quantile(x,0.95),
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun = median,
               geom = "line")+
  geom_hline(yintercept = 1)+geom_vline(xintercept = 2019)+
  xlab("Year") + ylab("Catchability")+ 
  ggtitle(sc1)
ggsave(paste0(res.plots,"/","biasCatchability_",sc1,"_",ni,"its.png"))

ggplot(as.data.frame((covars_all$indices[,,,,,])),aes(year,data,col=qs,fill=qs))+
  facet_grid(qs~.,scales="free")+
  stat_summary(fun.min = function(x) quantile(x,0.05), 
               fun.max = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun = median,
               geom = "line",size=1)+
  geom_vline(xintercept = 2019,linetype="dashed")+
  ggtitle(sc1)+
  ylab("Generated indices") +
  theme(legend.position = "none")
ggsave(paste0(res.plots,"/","biasIndices_",sc1,"_",ni,"its.png"))

ggplot(as.data.frame(covars_all$sel[,,,,,]),aes(age,data,col=factor(year),fill=factor(year)))+facet_wrap(~unit)+
  stat_summary(fun.ymin = function(x) quantile(x,0.05), 
               fun.ymax = function(x) quantile(x,0.95), 
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun = median,
               geom = "line")+
  ggtitle(sc1)+
  ylab("Selectivities by block") +
  theme(legend.position = "none")
ggsave(paste0(res.plots,"/","biasSelectivity_",sc1,"_",ni,"its.png"))

dir.spaly <- c("D:/IPMA/SARDINE/WGHANSA2020/SS3/2020_Update/")
spaly <- SS_output(dir = dir.spaly,forecast=FALSE,ncols=45)

ggplot(as.data.frame(covars_all$conv[,,,,,]),aes(year,data))+
  stat_summary(fun.min = function(x) quantile(x,0.05),
               fun.max = function(x) quantile(x,0.95),
               geom = "ribbon",alpha=0.1,col=NA) +
  stat_summary(fun = median,
               geom = "line")+
  geom_vline(xintercept = 2019)+
  geom_hline(yintercept = spaly$maximum_gradient_component,linetype="dashed")+
  annotate("text", x=2020, y=spaly$maximum_gradient_component,label=paste("Max gradient 2020"),vjust=-0.2,hjust=-0.2) +
  xlab("Year") + ylab("Mximum gradient")+ 
  ggtitle(sc1)
ggsave(paste0(res.plots,"/","biasConvergence_",sc1,"_",ni,"its.png"))

