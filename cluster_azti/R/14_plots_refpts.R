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
library(cowplot)

df <- read.table(file.path(res.dir,"stats.csv"), header=T, sep=";")


###################################################
### Search for Flim = F that risk3/prob3 == 0.5 ###
###################################################
out.Flim <- data.frame()

for (rr in c("REClow","RECmed")){
  
  if(rr=="RECmed") {  Bref <- 337448 } else { Bref <- 196334}
  
  df1 <- subset(df, period=='last' &  Rule == 'HCR12' & Ass=='ASSnone' & Rec == rr)
  
###SSB plot
  
ggplot(df1, aes(x=Fscan)) +
  theme_bw() + 
  geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
  geom_line(aes(y = Median_B1plus)) + 
  geom_hline(yintercept = Bref, col = "red", linetype="dashed") +
  annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0.8, vjust = 0) +
  labs(y = "Biomass 1+",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSnone_SSBDistribution.png"),height = 4)


if(rr=="RECmed") {  max_B1plus <- df1$max_P_B1plus_Blim } else { max_B1plus <- df1$max_P_B1plus_Blow}

###Flim Plot
g <- glm(max_B1plus ~ Fscan, data = df1, family = "quasibinomial", subset = Fscan < 1)
coefs <- coefficients(g)
Flim <- as.numeric(-coefs[1] / coefs[2]) #when risk is 0.5
df1$pred <- predict(g, newdata = df1["Fscan"], type = "response")

#extract associated SSB, i.e., possible Bpa
i <- which.max(max_B1plus >= 0.05) # risk 3 = 0.05
#make interpolation between that position and the previous one
it <- approx(df1$Fscan[i-0:1],max_B1plus[i-0:1])
#check Fscan that corresponds to risk3 closer to 0.05
Fbpa <- round(it$x[which.max(it$y >= 0.050)],2)
#Now check what Median B1 plus corresponds to Fpa
Bpa <- round(df1$Median_B1plus[df1$Fscan==Fpa],0)

ggplot(df1, aes(x=Fscan, y=max_B1plus)) +
  theme_bw() +
  geom_point(alpha=1/10) +
  geom_line(aes(y=pred),col='black') +
  annotate("segment", x = 0, xend = Fbpa, y = 0.05, yend = 0.05, colour = "grey40", linetype = "dashed") +
  annotate("segment", x = Fbpa, xend = Fbpa, y = 0, yend = 0.05, colour = "grey40", linetype = "dashed") +
  annotate("text", x=Fbpa, y=0.05,label=paste("Fbpa = ", round(Fbpa,2)),vjust=-0.2,hjust=-0.2) +
  annotate("segment", x = 0, xend = Flim, y = 0.5, yend = 0.5, colour = "grey40", linetype = "dashed") +
  annotate("segment", x = Flim, xend = Flim, y = 0, yend = 0.5, colour = "grey40", linetype = "dashed") +
  annotate("text", x=Flim, y=0.5,label=paste("Flim = ", round(Flim,2)),vjust=-0.2,hjust=-0.2) +
  ylab("Risk 3") +
  xlab("Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSnone_Flim.png"),height = 4)


if(rr=="RECmed") {  avg_B1plus <- df1$avg_P_B1plus_Blim } else { avg_B1plus <- df1$avg_P_B1plus_Blow}

###Probability plot (mean probability)
ggplot(df1, aes(x=Fscan)) +
  theme_bw() + 
  geom_line(aes(y = avg_B1plus)) + 
  geom_hline(yintercept = 0.05, col = "red", lwd = 1) +
  labs(y = "Probability",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSnone_Probplot.png"),height = 4)


out <- data.frame(Rec = rr, Flim=round(Flim,2), Fbpa = Fbpa, Bpa = Bpa)

out.Flim <- rbind(out.Flim,out)

}

write.table( out.Flim, file=file.path(res.dir,"Flim.csv"), dec = ".", sep = ";",
             row.names = FALSE)

rm(out, out.Flim, Bpa, it, i, Fbpa, Flim, g, coefs, df1, rr)


#############################################
###             Search for FMSY           ###
#############################################

out.Fmsy <- data.frame()

for (rr in c("REClow","RECmed")){
  
  if(rr=="RECmed") {  Bref <- 337448 } else { Bref <- 196334}

  df2 <- subset(df, period=='last' &  Rule != c('HCR0','HCR11') & Ass=='ASSss3' & Rec == rr)


###SSB plot

ggplot(df2, aes(x=Fscan)) +
  theme_bw() + 
  geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
  geom_line(aes(y = Median_B1plus)) + 
  geom_hline(yintercept = Bref, col = "red", linetype="dashed") +
  annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0.8, vjust = 0) +
  labs(y = "Biomass 1+",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSss3_SSBDistribution.png"))

###Probability plot (max probability)
if(rr=="RECmed") {  max_B1plus <- df2$max_P_B1plus_Blim } else { max_B1plus <- df2$max_P_B1plus_Blow}

ggplot(df2, aes(x=Fscan)) +
  theme_bw() + 
  geom_line(aes(y = max_B1plus)) + 
  geom_hline(yintercept = 0.05, col = "red", lwd = 1) +
  labs(y = "Risk 3",x = "Ftarget")
ggsave(paste0(res.plots,'/',rr,"_ASSss3_Probplot.png"))

###FMSY plot(s)

p1 <- ggplot(df2, aes(x=Fscan, y=Median_Catch)) +
  geom_point(alpha=1/10) +
  ylab("Median Catch") +
  xlab("Ftarget") 

interval = 0.95

yield.p95 <- interval * max(df2$Median_Catch, na.rm = TRUE)
x.lm <- stats::loess(df2$Median_Catch ~ df2$Fscan, span = 0.2)
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

p6 <- p5 + ylim(0,50000) + xlim(0,0.3)+ theme_bw () +
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
  ggsave(paste0(res.plots,'/',rr,"_ASSs3_SSBDistribution_FmsyIntervals.png"))
  
  out <- data.frame(Rec = rr, Fmsy=FMSY, FmsyUpper = fmsy.upper, FmsyLower = fmsy.lower)
  
  out.Fmsy <- rbind(out.Fmsy,out)

}

write.table( out.Fmsy, file=file.path(res.dir,"Fmsy.csv"), dec = ".", sep = ";",
             row.names = FALSE)

###################################################
###     Check if FMSY is precautionary          ###
###################################################

out.Fp05 <- data.frame()

for (rr in c("REClow","RECmed")){
  
  if(rr=="RECmed") { 
    
    Bref <- 337448 
    df3 <- subset(df, Rule == 'HCR0' & period=='last' & Ass == "ASSss3" & Rec == rr)
    
  }
  
  else {
    
    Bref <- 196334
    df3 <- subset(df, Rule == 'HCR11' & period=='last' & Ass == "ASSss3" & Rec == rr)
    
  }
  
  #extract Ftarget where P5th_B1plus == Bref
  i <- which.min(df3$P5th_B1plus > Bref) # first position where P5th_B1plus is above Bref
  #make interpolation between that position and the previous one
  it <- approx(df3$Fscan[i-0:1], df3$P5th_B1plus[i-0:1])
  #check Fscan that corresponds to P5th_B1plus closer to Bref
  Fp05 <- round(it$x[which.max(it$y <= Bref)],2)
  
  
  ###SSB plot

  ggplot(df3, aes(x=Fscan)) +
    theme_bw() + 
    geom_linerange(aes(ymin = P5th_B1plus, ymax = P95th_B1plus)) + 
    geom_line(aes(y = Median_B1plus)) + 
    geom_hline(yintercept = Bref, col = "red", linetype = "dashed") +
    geom_vline(xintercept = Fp05, col = "red", linetype = "dashed") +
    annotate("text", x = 0, y = Bref, label = "Blim", col = "red",hjust = 0.7, vjust = 0) +
    labs(y = "Biomass 1+",x = "Ftarget")
  ggsave(paste0(res.dir,'/',rr,"_",unique(df3$Rule),"_ASSss3_SSB.png"))

  ###Probability plot (mean probability)
  
  if(rr=="RECmed") {  avg_B1plus <- df3$avg_P_B1plus_Blim } else { avg_B1plus <- df3$avg_P_B1plus_Blow}
  
  ggplot(df3, aes(x=Fscan)) +
    theme_bw() + 
    geom_line(aes(y = avg_B1plus)) + 
    geom_hline(yintercept = 0.05, col = "red", lwd = 1) +
    labs(y = "Probability",x = "Ftarget")
  ggsave(paste0(res.dir,'/',rr,"_",unique(df3$Rule),"_ASSss3_probPlot.png"))
  
  ### FMSY plot(s)
  
  p1 <- ggplot(df3, aes(x=Fscan, y=Median_Catch)) +
    geom_point(alpha=1/10) +
    ylab("Median Catch") +
    xlab("Ftarget") 
  
  interval = 0.95
  
  yield.p95 <- interval * max(df3$Median_Catch, na.rm = TRUE)
  x.lm <- stats::loess(df3$Median_Catch ~ df3$Fscan, span = 0.2)
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
    annotate("text", lm.pred$x[lm.pred$y==max(lm.pred)], 0, vjust = 0.8, hjust=0,label = "F[MSY]",parse=T,size=4,color="grey30")
  
  p4 <- p3 + geom_hline(yintercept = yield.p95,colour="red", linetype="dashed", alpha=3/10) +
    annotate("text", 0.05, yield.p95, vjust = 0, hjust=0.5,label = "0.95~x~MSY",parse=T,size=4,color="grey30")+ 
    geom_point(aes(x = fmsy.lower.mean,y=landings.lower.mean),colour="red",size=1.5,alpha=1/10)+
    geom_point(aes(x = fmsy.upper.mean,y=landings.upper.mean),colour="red",size=1.5,alpha=1/10)
  
  p5 <- p4 +geom_vline(xintercept=fmsy.lower.mean,linetype="dashed", alpha=5/10) +
    annotate("text", fmsy.lower.mean, 0, vjust = 0.8, hjust=0,label = "F[MSY]~Lower",parse=T,size=4,color="grey30") +
    geom_vline(xintercept=fmsy.upper.mean,alpha=5/10,linetype="dashed") +
    annotate("text", fmsy.upper.mean, 0,  vjust = 0.8, hjust=0,label = "F[MSY]~Upper",parse=T,size=4,color="grey30")
  
  p6 <- p5 + annotate(geom="text", x=1.5, y=40000, col="black",
                      label=paste("Fmsy Lower:", round(fmsy.lower.mean,2),
                                  "\nFmsy:",round(lm.pred$x[lm.pred$y==max(lm.pred)],2), 
                                  "\nFmsy Upper:",round(fmsy.upper.mean,2)))
  ggsave(paste0(res.dir,'/',rr,"_",unique(df3$Rule),"_ASSss3_Fmsy.png"))
  

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
ggsave(paste0(res.dir,'/',rr,"_",unique(df3$Rule),"_ASSss3_Fp05.png"))

out <- data.frame(Rec = rr, Fmsy = FMSY, FmsyUpper = fmsy.upper, FmsyLower = fmsy.lower,
                  Fp05 = Fp05, Fp05Lower = f05.lower)

out.Fp05 <- rbind(out.Fp05, out)

}

write.table( out.Fp05, file=file.path(res.dir,"Fp05.csv"), dec = ".", sep = ";",
             row.names = FALSE)

rm(list=ls())