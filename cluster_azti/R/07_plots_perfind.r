################################################################################
#  IBpil results - plots of performance statistics                             # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                         #
#   created:  04/04/2019                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2019
# Author: AZTI (<libaibarriaga@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

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

theme_set(theme_bw())

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

load(file.path(res.dir, "scenario_list.RData"))
length(scenario_list)

#==============================================================================
# Load the reference points
#==============================================================================

# load the reference points

load(file.path(wd, "input", "PIL_refpts2020.RData"))
PIL_ref.pts

#==============================================================================
# Read performance statistics
#==============================================================================

# load performance statistics for all the scenarios

df <- read.table(file.path(res.dir,"stats.csv"), header=T, sep=";")

# reshape to the long format for ggplot

df <- reshape(df, idvar=c("period","scenario","Ass","Rule","Rec","INN","OER"), varying=list(8:ncol(df)), 
              times=names(df)[8:ncol(df)], timevar="indicator", v.names="value", direction="long")

#period as an ordered factor for the figures

df$period <- factor(df$period, levels=c("initial","short","med","last","all"))

#Rule as an ordered factor for the figures
df$Rule <- factor(df$Rule, levels=c("HCR7","HCR14", "HCR13","HCR8", "HCR9", "HCR10","HCR0", "HCR11"))

row.names(df) <- NULL


#==============================================================================
# Summary plots
#==============================================================================

# We don't want to include the outputs from the ICES rules with the old Reference Points
#summary plot for Median B1plus
ggplot(subset(df, indicator == 'Median_B1plus' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value/1000, breaks = c(-Inf, 196, 253,337,446,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value/1000,0)),show.legend =F,size=3) +
  scale_fill_manual(values=c("#D7191C","#FDAE61", "#FFFFBF" ,"#A6D96A", "#1A9641"),
                    name='B1plus (kt)', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_Biomass.png")),height = 4,width = 7)

#summary plot for catch
ggplot(subset(df, indicator == 'Median_Catch' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value/1000, breaks = c(-Inf,30, 35,40,45,50,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value/1000,0)),show.legend=F) +
  scale_fill_manual(values=c("#FFFFCC", "#D9F0A3" ,"#ADDD8E", "#78C679", "#31A354", "#006837"),
                    name='Catch (kt)', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_Catch.png")),height = 4,width = 7)

#summary plot for IAV
ggplot(subset(df, indicator == 'IAV1_Catch' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value/1000, breaks = c(-Inf,5,10,15,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value/1000,0)),show.legend=F) +
  scale_fill_manual(values=c("#FFFFB2" ,"#FECC5C" ,"#FD8D3C" ,"#E31A1C"),
                    name='Catch (kt)', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_IAV.png")),height = 4,width = 7)

#summary plot for Risk3

ggplot(subset(df, indicator == 'max_P_B1plus_Blow' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value*100, breaks = c(-Inf,3,4,5,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value*100,1)),show.legend=F, size=2) +
  scale_fill_manual(values = c( "#1A9641","#A6D96A" ,"#FDAE61","#D7191C" ),name='Risk3', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_Risk3Blow.png")),height = 4,width = 7)


ggplot(subset(df, indicator == 'max_P_B1plus_Blim' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value*100, breaks = c(-Inf,3,4,5,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value*100,1)),show.legend=F, size=2) +
  scale_fill_manual(values = c( "#1A9641","#A6D96A" ,"#FDAE61","#D7191C" ),name='Risk3', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_Risk3Blim.png")),height = 4,width = 7)

#summary plot for Risk1
ggplot(subset(df, indicator == 'avg_P_B1plus_Blow' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value*100, breaks = c(-Inf,3,4,5,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value*100,1)),show.legend=F, size=2) +
  scale_fill_manual(values = c( "#1A9641","#A6D96A" ,"#FDAE61","#D7191C" ),name='Risk1', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_Risk1Blow.png")),height = 4,width = 7)

ggplot(subset(df, indicator == 'avg_P_B1plus_Blim' & Ass=="ASSss3" & !Rule %in% c("HCR0","HCR11")),aes(x=Rule,y=period))+
  geom_tile(aes(fill=cut(value*100, breaks = c(-Inf,3,4,5,Inf))))+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(value*100,1)),show.legend=F, size=2) +
  scale_fill_manual(values = c( "#1A9641","#A6D96A" ,"#FDAE61","#D7191C" ),name='Risk1', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30"))
ggsave(file.path(plot.dir,paste0("SummaryScenarios_Risk1Blim.png")),height = 4,width = 7)


#Final plots with risk 3 just for the period needed
#If REC %in% REClow & REClowmed then period 2041:2050
#else period 2061:2070

r30_low <- subset(df, indicator == 'max_P_B1plus_Blow' & Ass=="ASSss3" &
                !Rule %in% c("HCR0","HCR11") & period == 'med' & Rec=="REClow")
r30_low$iters <- rep(1000,dim(r30_low)[1])

r30_low <- rbind(r30_low, c('med','ASSss3_HCR9_REClow_INNvar_OERnaq','ASSss3', 'HCR9','REClow','INNvar','OERnaq','max_P_B1plus_Blow',0.0415,10000))
r30_low <- rbind(r30_low, c('med','ASSss3_HCR10_REClow_INNvar_OERnaq','ASSss3', 'HCR10','REClow','INNvar','OERnaq','max_P_B1plus_Blow',0.0415,10000))

r30_lowmed <- subset(df, indicator == 'max_P_B1plus_Blim' & Ass=="ASSss3" &
                    !Rule %in% c("HCR0","HCR11") & period == 'med' & Rec=="REClowmed")
r30_lowmed$iters <- rep(1000,dim(r30_lowmed)[1])

r50_mix <- subset(df, indicator == 'max_P_B1plus_Blim' & Ass=="ASSss3" &
                !Rule %in% c("HCR0","HCR11") & period == 'last' & Rec == "RECmix")
r50_mix$iters <- rep(1000,dim(r50_mix)[1])

risk3 <- rbind(r30_low,r30_lowmed,r50_mix)
  
ggplot(data=risk3,aes(x=Rule,y=as.numeric(value)))+
  geom_point(aes(colour=cut(as.numeric(value)*100, breaks = c(-Inf,3,5,6,Inf)),shape=iters),size=2)+
  facet_grid(.~Rec)+
  geom_text(aes(label=round(as.numeric(value)*100,1)),show.legend=F, size=2,vjust=-1) +
  scale_colour_manual(values = c( "#1A9641","#A6D96A" ,"#FDAE61","#D7191C" ),name='Risk3', drop = F, guide = "coloursteps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(labels=c('HCR10'="HCR50",'HCR7'="HCR0" ,'HCR8'= "HCR40", 'HCR9'="HCR45", 'HCR13'="HCR35",'HCR14'="HCR30")) +
  expand_limits(y = 0.2)+
  scale_y_continuous(labels=scales::label_percent(),name="P(B1+ <= Blim)")
ggsave(file.path(plot.dir,paste0("Risk3.png")),height = 6,width = 7)

