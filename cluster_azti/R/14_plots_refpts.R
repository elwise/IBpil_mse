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
res.plots  <- file.path("./output_refpts/plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================
library(tidyverse)
library(icesAdvice)

df <- read.table(file.path(res.dir,"stats.csv"), header=T, sep=";")


###################################################
### Search for Flim = F that risk3/prob3 == 0.5 ###
###################################################
out.Flim <- data.frame()

for (rr in c("REClow","RECmed")){
  
  if(rr=="RECmed") {  Bref <- 337448 } else { Bref <- 196334}
  
  df1 <- subset(df, period=='med' &  Rule == 'HCR12' & Ass=='ASSnone' & Rec == rr)
  
if(rr=="RECmed") {  max_B1plus <- df1$max_P_B1plus_Blim } else { max_B1plus <- df1$max_P_B1plus_Blow}

  #extract associated F for which risk3 = 0.5
  i <- which.max(max_B1plus >= 0.5)# risk 3 = 0.05
  #make interpolation between that position and the previous one
  it <- approx(df1$Fscan[i-0:1],max_B1plus[i-0:1])
  #check Fscan that corresponds to risk3 closer to 0.05
  Flim <- as.numeric(icesRound(it$x[which.max(it$y >= 0.050)]))

#extract associated SSB, i.e., possible Bpa
i <- which.max(max_B1plus >= 0.05) # risk 3 = 0.05
#make interpolation between that position and the previous one
it <- approx(df1$Fscan[i-0:1],max_B1plus[i-0:1])
#check Fscan that corresponds to risk3 closer to 0.05
Fbpa <- round(it$x[which.max(it$y >= 0.050)],2)
#Now check what Median B1 plus corresponds to Fpa
Bpa <- round(df1$Median_B1plus[df1$Fscan==Fbpa],0)

###SSB plot

ggplot(df1, aes(x=Fscan)) +
  theme_bw() + 
  geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
  geom_line(aes(y = Median_B1plus)) + 
  geom_hline(yintercept = Bref, col = "red", linetype="dashed") +
  #annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0.8, vjust = 0) +
  #annotate("segment", x = 0, xend = Fbpa, y = Bpa, yend = Bpa, colour = "red", linetype = "dashed") +
  #annotate("segment", x = Fbpa, xend = Fbpa, y = 0, yend = Bpa, colour = "red", linetype = "dashed") +
  #annotate("text", x=Fbpa, y=Bpa,label=paste("possible Bpa = ", round(Bpa,0)),vjust=-0.2,hjust=-0.2,colour='red') +
  labs(y = "Biomass 1+",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSnone_SSBDistribution.png"),height = 4)

# Risk 3 plot
ggplot(df1, aes(x=Fscan, y=max_B1plus)) +
  theme_bw() +
  geom_point(alpha=1/10) +
  geom_line(aes(y=max_B1plus),col='black') +
  #annotate("segment", x = 0, xend = Fbpa, y = 0.05, yend = 0.05, colour = "grey40", linetype = "dashed") +
  #annotate("segment", x = Fbpa, xend = Fbpa, y = 0, yend = 0.05, colour = "grey40", linetype = "dashed") +
  #annotate("text", x=Fbpa, y=0.05,label=paste("Fbpa = ", round(Fbpa,2)),vjust=-0.2,hjust=-0.2) +
  annotate("segment", x = 0, xend = Flim, y = 0.5, yend = 0.5, colour = "grey40", linetype = "dashed") +
  annotate("segment", x = Flim, xend = Flim, y = 0, yend = 0.5, colour = "grey40", linetype = "dashed") +
  annotate("text", x=Flim, y=0.5,label=paste("Flim = ", Flim),vjust=-0.2,hjust=-0.2) +
  ylab("Risk 3") +
  xlab("Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSnone_Flim.png"),height = 4)


#if(rr=="RECmed") {  avg_B1plus <- df1$avg_P_B1plus_Blim } else { avg_B1plus <- df1$avg_P_B1plus_Blow}

###Probability plot (mean probability)
# ggplot(df1, aes(x=Fscan)) +
#   theme_bw() + 
#   geom_line(aes(y = avg_B1plus)) + 
#   geom_hline(yintercept = 0.05, col = "red", lwd = 1) +
#   labs(y = "Probability",x = "Ftarget")
# ggsave(paste0(res.plots,'/',rr,"_ASSnone_Probplot.png"),height = 4)


out <- data.frame(Rec = rr, Flim=icesRound(Flim), Fbpa = Fbpa, Bpa = Bpa)

out.Flim <- rbind(out.Flim,out)

}

write.table( out.Flim, file=file.path(res.dir,"Flim.csv"), dec = ".", sep = ";",
             row.names = FALSE)

rm(out, out.Flim, Bpa, it, i, Fbpa, Flim, df1, rr, max_B1plus, Bref)


#############################################
###             Search for FMSY           ###
#############################################

out.Fmsy <- data.frame()

for (rr in c("REClow","RECmed")){
  
  if(rr=="RECmed") {  Bref <- 337448 } else { Bref <- 196334}

  df2 <- subset(df, period=='med' &  Rule == 'HCR12' & Ass=='ASSss3' & Rec == rr)


###SSB plot

ggplot(df2, aes(x=Fscan)) +
  theme_bw() + 
  geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
  geom_line(aes(y = Median_B1plus)) + 
  geom_hline(yintercept = Bref, col = "red", linetype="dashed") +
  annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0.8, vjust = 0) +
  labs(y = "Biomass 1+",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSss3_constantF_SSBDistribution.png"))

###Probability plot (max probability)
if(rr=="RECmed") {  max_B1plus <- df2$max_P_B1plus_Blim } else { max_B1plus <- df2$max_P_B1plus_Blow}

ggplot(df2, aes(x=Fscan)) +
  theme_bw() + 
  geom_line(aes(y = max_B1plus)) + 
  geom_hline(yintercept = 0.05, col = "red", lwd = 1) +
  labs(y = "Risk 3",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSss3_constantF_Probplot.png"))

###FMSY plot(s)

p1 <- ggplot(df2, aes(x=Fscan, y=Median_Catch)) +
  geom_point(alpha=1/10) +
  ylab("Median Catch") +
  xlab("Ftarget") 

interval = 0.95

yield.p95 <- interval * max(df2$Median_Catch, na.rm = TRUE)
x.lm <- stats::loess(df2$Median_Catch ~ df2$Fscan, span = 0.4)
lm.pred <- data.frame(x = seq(min(df2$Fscan), max(df2$Fscan), length = 1000), 
                      y = rep(NA, 1000))
lm.pred$y <- stats::predict(x.lm, newdata = lm.pred$x)

p2 <- p1 + geom_line(data=lm.pred, aes(x=x,y=y)) + 
  geom_point(aes(x = lm.pred$x[lm.pred$y==max(lm.pred)],y=max(Median_Catch)),colour="red",size=1.5) +
  geom_hline(yintercept = max(df2$Median_Catch),colour="red", alpha=3/10) +
  annotate("text", 0.05, max(df2$Median_Catch)+200, vjust = 0.2, hjust=0.2,label = "MSY",parse=T,size=4,color="grey30")

#Extract FMSY
FMSY <- lm.pred$x[lm.pred$y==max(lm.pred)]

lm.pred.95 <- lm.pred[lm.pred$y >= yield.p95, ]
fmsy.lower <- min(lm.pred.95$x)
fmsy.upper <- max(lm.pred.95$x)
fmsy.lower.mean <- fmsy.lower
fmsy.upper.mean <- fmsy.upper
landings.lower.mean <- lm.pred.95[lm.pred.95$x == fmsy.lower.mean, 
                                  ]$y
landings.upper.mean <- lm.pred.95[lm.pred.95$x == fmsy.upper.mean, 
                                  ]$y

p3 <- p2 + geom_vline(xintercept=lm.pred$x[lm.pred$y==max(lm.pred)],alpha=5/10) +
  annotate("text", lm.pred$x[lm.pred$y==max(lm.pred)], 0, vjust = 0.8, hjust=0,label = "F[MSY]",parse=T,size=4,color="grey30")

p4 <- p3 + geom_hline(yintercept = yield.p95,colour="red", linetype="dashed", alpha=3/10) +
  annotate("text", 0.05, yield.p95, vjust = 0, hjust=0.5,label = "0.95~x~MSY",parse=T,size=4,color="grey30")+ 
  geom_point(aes(x = fmsy.lower.mean,y=landings.lower.mean),colour="red",size=1.5,alpha=1/10)+
  geom_point(aes(x = fmsy.upper.mean,y=landings.upper.mean),colour="red",size=1.5,alpha=1/10)

p5 <- p4 + geom_vline(xintercept=fmsy.lower.mean,linetype="dashed", alpha=5/10) +
  geom_vline(xintercept=fmsy.upper.mean,alpha=5/10,linetype="dashed")
  #annotate("text", fmsy.lower.mean, 0, vjust = 0.8, hjust=0,label = "F[MSY]~Lower",parse=T,size=4,color="grey30") +

  #annotate("text", fmsy.upper.mean, 0,  vjust = 0.8, hjust=0,label = "F[MSY]~Upper",parse=T,size=4,color="grey30")

p6 <- p5 + ylim(0,80000) + xlim(0,0.3)+ theme_bw () +
annotate(geom="text", x=0.27, y=30000, col="black",
                    label=paste("Fmsy Lower:", round(fmsy.lower.mean,2),
                                "\nFmsy:",round(lm.pred$x[lm.pred$y==max(lm.pred)],2), 
                                "\nFmsy Upper:",round(fmsy.upper.mean,2)))
ggsave(paste0(res.plots,'/',rr,"_ASSss3_Fmsy.png"))

#plot_grid(p1, p2, p3, p4, p5, p6, ncol=3, labels=LETTERS[1:6])

### SSB plot with associated Fmsy lower and upper
ggplot(df2, aes(x=Fscan)) +
  theme_bw() + 
  geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
  geom_line(aes(y = Median_B1plus)) +
  geom_vline(xintercept = c(fmsy.lower.mean,fmsy.upper.mean),col= "grey40", linetype="dashed") +
  geom_vline(xintercept = FMSY,col= "grey40") +
  geom_hline(yintercept = Bref, col = "red", linetype="dashed") +
  annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0, vjust = 0) +
  labs(y = "Biomass 1+",x = "Ftarget")
  ggsave(paste0(res.plots,'/',rr,"_ASSs3_constantF_SSBDistribution_FmsyIntervals.png"))
  
  out <- data.frame(Rec = rr, Fmsy=icesRound(FMSY), FmsyUpper = fmsy.upper, FmsyLower = fmsy.lower, Catch = df2$Median_Catch[df2$Fscan == icesRound(FMSY)])
  
  out.Fmsy <- rbind(out.Fmsy,out)

}

write.table( out.Fmsy, file=file.path(res.dir,"Fmsy.csv"), dec = ".", sep = ";",
             row.names = FALSE)

rm(out, out.Fmsy, fmsy.lower,fmsy.lower.mean, fmsy.upper,fmsy.upper.mean,max_B1plus,FMSY,interval,
   landings.lower.mean,landings.upper.mean,rr,yield.p95,Bref, df2,lm.pred,lm.pred.95,x.lm)

###################################################
###     Check if FMSY is precautionary          ###
###################################################

out.Fp05 <- data.frame()

for (rr in c("REClow","RECmed")){
  
  if(rr=="RECmed") { 
    
    Bref <- 337448 
    df3 <- subset(df, Rule == 'HCR16' & period=='med' & Ass == "ASSss3" & Rec == rr)
    max_B1plus <- df3$max_P_B1plus_Blim
    
  }
  
  else {
    
    Bref <- 196334
    df3 <- subset(df, Rule == 'HCR15' & period=='med' & Ass == "ASSss3" & Rec == rr)
    max_B1plus <- df3$max_P_B1plus_Blow
    
  }
  
  #extract associated F for which risk3 == 0.05
  i <- which.max(max_B1plus >= 0.05) # risk 3 = 0.05
  #make interpolation between that position and the previous one
  it <- approx(df3$Fscan[i-0:1],max_B1plus[i-0:1])
  #check Fscan that corresponds to risk3 closer to 0.05
  Fp05 <- as.numeric(icesRound(it$x[which.max(it$y >= 0.050)]))
  
  #make interpolation between that position and the previous one
  it <- approx(max_B1plus[i-0:1],df3$Median_Catch[i-0:1])
  #check Fscan that corresponds to risk3 closer to 0.05
  Catches05 <- round(it$y[which.max(it$x >= 0.050)],0)
  
  
  ###SSB plot

  ggplot(df3, aes(x=Fscan)) +
    theme_bw() + 
    geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
    geom_line(aes(y = Median_B1plus)) + 
    geom_hline(yintercept = Bref, col = "red", linetype = "dashed") +
    annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0.7, vjust = 0) +
    labs(y = "Biomass 1+",x = "Ftarget")
  ggsave(paste0(res.plots,'/',rr,"_",unique(df3$Rule),"_ASSss3_ICESRule_SSB.png"),height = 4)

  ###Maximum Probability plot (mean probability)

  ggplot(df3, aes(x=Fscan)) +
    theme_bw() + 
    geom_line(aes(y = max_B1plus))+ 
    annotate("segment", x = 0, xend = Fp05, y = 0.05, yend = 0.05, colour = "grey40", linetype = "dashed") +
    annotate("segment", x = Fp05, xend = Fp05, y = 0, yend = 0.05, colour = "grey40", linetype = "dashed") +
    annotate("text", x=Fp05, y=0.05,label=paste("Fp05 = ",Fp05),vjust=-0.2,hjust=-0.2) +
    labs(y = "Probability",x = "Ftarget")
  ggsave(paste0(res.plots,'/',rr,"_",unique(df3$Rule),"_ASSss3_ICESRule_probPlot.png"),height = 4)
  
  ### FMSY plot(s)
  
  p1 <- ggplot(df3, aes(x=Fscan, y=Median_Catch)) +
    geom_point(alpha=1/10) +
    ylab("Median Catch") +
    xlab("Ftarget") 
  
  interval = 0.95
  
  yield.p95 <- interval * max(df3$Median_Catch, na.rm = TRUE)
  x.lm <- stats::loess(df3$Median_Catch ~ df3$Fscan, span = 0.6)#add to increase span
  lm.pred <- data.frame(x = seq(min(df3$Fscan), max(df3$Fscan), length = 1000), 
                        y = rep(NA, 1000))
  lm.pred$y <- stats::predict(x.lm, newdata = lm.pred$x)
  
  p2 <- p1 + geom_line(data=lm.pred, aes(x=x,y=y)) + 
    geom_point(aes(x = lm.pred$x[lm.pred$y==max(lm.pred)],y=max(Median_Catch)),colour="red",size=1.5) +
    geom_hline(yintercept = max(df3$Median_Catch),colour="red", alpha=3/10) +
    annotate("text", 0.05, max(df3$Median_Catch)+200, vjust = 0.2, hjust=0.2,label = "MSY",parse=T,size=4,color="grey30")
  
  #Extract FMSY
  FMSY <- lm.pred$x[lm.pred$y==max(lm.pred)]
  
  lm.pred.95 <- lm.pred[lm.pred$y >= yield.p95, ]
  fmsy.lower <- min(lm.pred.95$x)
  fmsy.upper <- max(lm.pred.95$x)
  fmsy.lower.mean <- fmsy.lower
  fmsy.upper.mean <- fmsy.upper
  landings.lower.mean <- lm.pred.95[lm.pred.95$x == fmsy.lower.mean, 
                                    ]$y
  landings.upper.mean <- lm.pred.95[lm.pred.95$x == fmsy.upper.mean, 
                                    ]$y
  
  p3 <- p2 + geom_vline(xintercept=lm.pred$x[lm.pred$y==max(lm.pred)],alpha=5/10) +
    annotate("text", lm.pred$x[lm.pred$y==max(lm.pred)], 0, vjust = 0.8, hjust=1,label = "F[MSY]",parse=T,size=4,color="grey30")
  
  p4 <- p3 + geom_hline(yintercept = yield.p95,colour="red", linetype="dashed", alpha=3/10) +
    annotate("text", 0.05, yield.p95, vjust = 0, hjust=0.5,label = "0.95~x~MSY",parse=T,size=4,color="grey30")+ 
    geom_point(aes(x = fmsy.lower.mean,y=landings.lower.mean),colour="red",size=1.5,alpha=1/10)+
    geom_point(aes(x = fmsy.upper.mean,y=landings.upper.mean),colour="red",size=1.5,alpha=1/10)
  
  p5 <- p4 +geom_vline(xintercept=fmsy.lower.mean,linetype="dashed", alpha=5/10) +
    annotate("text", fmsy.lower.mean, 0, vjust = 0.8, hjust=0,label = "F[MSY]~Lower",parse=T,size=4,color="grey30") +
    geom_vline(xintercept=fmsy.upper.mean,alpha=5/10,linetype="dashed") +
    annotate("text", fmsy.upper.mean, 0,  vjust = 0.8, hjust=0,label = "F[MSY]~Upper",parse=T,size=4,color="grey30")
  
  p6 <- p5 + xlim(0,0.25) +annotate(geom="text", x=0.22, y=30000, col="black",
                      label=paste("Fmsy Lower:", round(fmsy.lower.mean,2),
                                  "\nFmsy:",round(lm.pred$x[lm.pred$y==max(lm.pred)],2), 
                                  "\nFmsy Upper:",round(fmsy.upper.mean,2)))
  ggsave(paste0(res.plots,'/',rr,"_",unique(df3$Rule),"_ASSss3_ICESRule_Fmsy.png"),height = 4)
  

###Extract Fp0.05 from runs with Btrigger (ICES Rule, Blim, Btrigger, FMSY)
yield.f05 <- stats::predict(x.lm, newdata = Fp05)
yield.f05.95 <- interval * yield.f05
lm.pred.f05.95 <- lm.pred[lm.pred$y >= yield.f05.95, ]
f05.lower <- min(lm.pred.f05.95$x)
f05.upper <- max(lm.pred.f05.95$x)

p3 + geom_point(aes(x=Fp05,y=yield.f05),colour="blue") +
  geom_vline(xintercept=Fp05,colour="blue") +
  annotate("text", Fp05, 0,  vjust = 0.8, hjust=0,label = "F[p.05]",parse=T,size=4,color="blue")+
  geom_point(aes(x=f05.lower,y=yield.f05.95),colour="orange") +
  geom_hline(yintercept=yield.f05,colour="blue") +
  geom_hline(yintercept=yield.f05.95,colour="blue",linetype="dashed") +
  geom_vline(xintercept=f05.lower,colour="blue",linetype="dashed") 
ggsave(paste0(res.plots,'/',rr,"_",unique(df3$Rule),"_ASSss3_ICESRule_Fp05lowerbound.png"),height = 4)

out <- data.frame(Rec = rr, Fmsy = FMSY, FmsyUpper = fmsy.upper, FmsyLower = fmsy.lower,
                  Fp05 = Fp05, Fp05Lower = f05.lower, Catch = Catches05)

out.Fp05 <- rbind(out.Fp05, out)

}

write.table( out.Fp05, file=file.path(res.dir,"Fp05.csv"), dec = ".", sep = ";",
             row.names = FALSE)

rm(list=ls())

####PLOTS FOR ICES MSY AR Low and Med
# read data

dfyr <- read.table(file.path(res.dir,"stats_byyr.csv"), header=T, sep=";")

#select only variables of interest
dfyr <- dfyr[,c(1:9,10,20,30,11,21,31,12,22,32,14,24,34)]
#change units of variables of interest
dfyr[,c(10:12)] <- dfyr[,c(10:12)]/1000000
dfyr[,c(13:15)] <- dfyr[,c(13:15)]/1000
dfyr[,c(19:21)] <- dfyr[,c(19:21)]/1000

# reshape data 
dfyr%>%
  gather("var_q", "value", -c(1:9)) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  tidyr::spread("quantile", "value") %>% as.data.frame -> dfyr

#choose Fscan closest to Fp05
df1 <- subset(dfyr, Ass == 'ASSss3' & Rule == 'HCR15' & Rec == 'REClow' & Fscan == 0.09)
df2 <- subset(dfyr, Ass == 'ASSss3' & Rule == 'HCR16' & Rec == 'RECmed' & Fscan == 0.11)

dfyr <- rbind(df1,df2)

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

# subset data.frame for variables of interest
ww <- subset(ww, indicator %in% c("rec","ssb","f","catch"))
ww$indicator2 <- factor(ww$indicator, levels=c("rec","ssb","f","catch"))

#Data from two iters
out.all <- NULL

for (cs in c('ASSss3_HCR15_REClow_INNvar_OERnaq_Fscan0.09','ASSss3_HCR16_RECmed_INNvar_OERnaq_Fscan0.11')){
  
  file.dat <- file.path(res.dir,paste("output_scenarios/results_",cs,".RData",sep=""))
  aux <- loadToEnv(file.dat)[["out.bio"]]
  aux <- subset(aux, iter %in% c(45,235))
  aux <- subset(aux, year>2019)
  
  out.all <- rbind(out.all, aux)
  
}

out.iters <- 
  out.all %>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER", "Fscan"), sep = "_",  remove=FALSE) %>%
  mutate_at("Fscan", ~ as.numeric(sub("Fscan", "", .x)))

#change units of variables of interest
out.iters[,11] <- out.iters[,11]/1000000
out.iters[,12] <- out.iters[,12]/1000
out.iters[,15] <- out.iters[,15]/1000

# reshape data 
out.iters <- reshape2::melt(out.iters,id=1:10,variable.name = "indicator")
out.iters <- subset(out.iters,indicator %in% c("catch","f","rec","ssb"))
out.iters$indicator2 <- factor(out.iters$indicator,levels=c("rec","ssb","f","catch"))
out.iters <- subset(out.iters,Ass=="ASSss3")

##Now for the plots
facet_names <- c('catch'= "Catch",'f'="Fbar",'rec'="Rec",'ssb'="B1+", 'HCR15'="ICES_low",'HCR16'="ICES_med",
                 'REClow' = 'REClow', 'REClowmed'= 'REClowmed','RECmix'='RECmix' )

##worm plots 

for (hcr in c("HCR15","HCR16")){
  
  aux <- subset(ww, Rule == hcr & year < 2051)
  aux.iters <- subset(out.iters, Rule == hcr & year < 2051)

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
    
    
    p + geom_hline(aes(yintercept = 337.448), data = subset(aux, indicator=="ssb"),linetype="dashed",color="#00A08A") +
      geom_hline(aes(yintercept = 196.334), data = subset(aux,indicator=="ssb"),linetype="dashed",color="#00A08A")
    ggsave(paste0(res.plots,'/',hcr,".png"),width = 5,height=6.65)

  
}

# read data and compute pblim by year
successyr <- NULL

for (cs in c('ASSss3_HCR15_REClow_INNvar_OERnaq_Fscan0.09','ASSss3_HCR16_RECmed_INNvar_OERnaq_Fscan0.11')){
  load(file.path(res.dir,paste("output_scenarios/results_",cs,".RData",sep="")))
  aux <- out.bio %>% 
    separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","Fscan"), sep = "_", remove=FALSE)%>%
    mutate_at("Fscan", ~ as.numeric(sub("Fscan", "", .x))) %>%
    filter(year>2019)%>%
    group_by(year) %>% 
    summarize(pblim=sum(biomass>=337448)/length(biomass),
              pblow=sum(biomass>=196334)/length(biomass),
              pzero=sum(catch<= 1e-6)/length(catch),
              paboveFmsy = sum(f>=0.092)/length(f))
  aux$scenario <- rep(cs,dim(aux)[1])
  successyr <- rbind(successyr, as.data.frame(aux))
}

aux <- successyr%>%
  separate(scenario, into = c("Ass", "Rule", "Rec", "INN", "OER","Fscan"), sep = "_", remove=FALSE)%>%
  mutate_at("Fscan", ~ as.numeric(sub("Fscan", "", .x)))

for (rule in c('HCR15','HCR16')){

  ggplot(data=subset(aux,year<2051 & Rule == rule))+
    geom_line(data=subset(aux, year<2051 & Rule == rule & Rec!="REClow"),aes(x=year,y=pblim),size=1)+
    geom_line(data=subset(aux, year<2051 & Rule == rule & Rec!="RECmed"),aes(x=year,y=pblow),size=1)+
    geom_hline(yintercept = 0.95, linetype = "dashed")+
    ylab("P(B1+>=Blim)")+xlab("Year")+
    scale_x_continuous(name="Year",breaks = seq(2020,2050,5))+
    scale_color_brewer(palette="Dark2") +
    theme(
      axis.text=element_text(size=14),
    )+
    ylim(0.6,1)
  ggsave(paste0(res.plots,"/pblim_",rule,".png"),width=10)
  
}


  df6yr <- aux %>% filter(year < 2027) %>%
    summarise (initial = mean(paboveFmsy))
  
  df10yr <- aux %>% filter(year < 2031) %>%
    summarise (short = mean(paboveFmsy))
  
  df20yr <- aux %>% filter(year < 2041) %>%
    summarise (twentyyrs = mean(paboveFmsy))
  
  df30yr <- aux %>% filter(year < 2051) %>%
    summarise (all = mean(paboveFmsy))
  
  dflast10yr <- aux %>% filter(year > 2040 & year < 2051) %>%
    summarise (last = mean(paboveFmsy))
  
  qq <- cbind(df6yr,df10yr,df20yr,dflast10yr,df30yr)
  

