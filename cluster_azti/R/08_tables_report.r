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
res.dir  <- file.path("./output/")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)
library(dplyr)

theme_set(theme_bw())

#==============================================================================
# Tables for the report
#==============================================================================

df <- read.table(file.path(res.dir,"stats.csv"), header=T, sep=";")

df$Rec <- ordered(df$Rec, c("REClow","REClowmed","RECmix"))


# tables for the report NO ASSESSMENT

aux <- df %>% subset(Rule %in% c("HCR14","HCR13","HCR8","HCR9","HCR10") & Ass=="ASSnone") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000,0),
            round(Median_B1plus[period=="short"]/1000,0),
            round(Median_B1plus[period=="med"]/1000,0),
            round(Median_B1plus[period=="last"]/1000,0),
            round(Median_F[period=="initial"],3),
            round(Median_F[period=="short"],3),
            round(Median_F[period=="med"],3),
            round(Median_F[period=="last"],3),
            round(Median_Catch[period=="initial"]/1000,0),
            round(Median_Catch[period=="short"]/1000,0),
            round(Median_Catch[period=="med"]/1000,0),
            round(Median_Catch[period=="last"]/1000,0),
            round(IAV1_Catch[period=="initial"]/1000,0),
            round(IAV1_Catch[period=="short"]/1000,0),
            round(IAV1_Catch[period=="med"]/1000,0),
            round(IAV1_Catch[period=="last"]/1000,0),
            round(closure[period=="initial"]*100,0),
            round(closure[period=="short"]*100,0),
            round(closure[period=="med"]*100,0),
            round(closure[period=="last"]*100,0),
            #round(P_B1plus_0.8Blim[period=="initial"]*100,0),
            #round(P_B1plus_0.8Blow[period=="initial"]*100,0),
            #round(firstyear_B1plus_0.8Blim[period=="all"],0),
            #round(firstyear_B1plus_0.8Blow[period=="all"],0),
            round(firstyear_B1plus_Blim[period=="all"],0),
            round(firstyear_B1plus_Blow[period=="all"],0),
            round(max_P_B1plus_Blim[period=="last"]*100,0),
            round(max_P_B1plus_Blow[period=="last"]*100,0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR8to14_ASSnone.csv"), sep=";", col.names=F)

aux <- df %>% subset(Rule %in% c("HCR7") & Ass=="ASSnone") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000,0),
            round(Median_B1plus[period=="short"]/1000,0),
            round(Median_B1plus[period=="last"]/1000,0),
            round(Median_F[period=="initial"],3),
            round(Median_F[period=="short"],3),
            round(Median_F[period=="last"],3),
            round(Median_Catch[period=="initial"]/1000,0),
            round(Median_Catch[period=="short"]/1000,0),
            round(Median_Catch[period=="last"]/1000,0),
            round(IAV1_Catch[period=="initial"]/1000,0),
            round(IAV1_Catch[period=="short"]/1000,0),
            round(IAV1_Catch[period=="last"]/1000,0),
            round(closure[period=="initial"]*100,0),
            round(closure[period=="short"]*100,0),
            round(closure[period=="last"]*100,0),
            #round(P_B1plus_0.8Blim[period=="initial"]*100,0),
            #round(P_B1plus_0.8Blow[period=="initial"]*100,0),
            #round(firstyear_B1plus_0.8Blim[period=="all"],0),
            #round(firstyear_B1plus_0.8Blow[period=="all"],0),
            round(firstyear_B1plus_Blim[period=="all"],0),
            round(firstyear_B1plus_Blow[period=="all"],0),
            round(max_P_B1plus_Blim[period=="last"]*100,0),
            round(max_P_B1plus_Blow[period=="last"]*100,0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR7_ASSnone.csv"), sep=";", col.names=F)

aux <- df %>% subset(Rule %in% c("HCR0","HCR11") & Ass=="ASSnone") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000,0),
            round(Median_B1plus[period=="short"]/1000,0),
            round(Median_B1plus[period=="last"]/1000,0),
            round(Median_F[period=="initial"],3),
            round(Median_F[period=="short"],3),
            round(Median_F[period=="last"],3),
            round(Median_Catch[period=="initial"]/1000,0),
            round(Median_Catch[period=="short"]/1000,0),
            round(Median_Catch[period=="last"]/1000,0),
            round(IAV1_Catch[period=="initial"]/1000,0),
            round(IAV1_Catch[period=="short"]/1000,0),
            round(IAV1_Catch[period=="last"]/1000,0),
            round(closure[period=="initial"]*100,0),
            round(closure[period=="short"]*100,0),
            round(closure[period=="last"]*100,0),
            #round(P_B1plus_0.8Blim[period=="initial"]*100,0),
            #round(P_B1plus_0.8Blow[period=="initial"]*100,0),
            #round(firstyear_B1plus_0.8Blim[period=="all"],0),
            #round(firstyear_B1plus_0.8Blow[period=="all"],0),
            round(firstyear_B1plus_Blim[period=="all"],0),
            round(firstyear_B1plus_Blow[period=="all"],0),
            round(max_P_B1plus_Blim[period=="last"]*100,0),
            round(max_P_B1plus_Blow[period=="last"]*100,0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR0&HCR11_ASSnone.csv"), sep=";", col.names=F)

# tables for the report WITH ASSESSMENT

aux <- df %>% subset(Rule %in% c("HCR14","HCR13","HCR8","HCR9","HCR10") & Ass=="ASSss3") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000,0),
            round(Median_B1plus[period=="short"]/1000,0),
            round(Median_B1plus[period=="med"]/1000,0),
            round(Median_B1plus[period=="last"]/1000,0),
            round(Median_F[period=="initial"],3),
            round(Median_F[period=="short"],3),
            round(Median_F[period=="med"],3),
            round(Median_F[period=="last"],3),
            round(Median_Catch[period=="initial"]/1000,0),
            round(Median_Catch[period=="short"]/1000,0),
            round(Median_Catch[period=="med"]/1000,0),
            round(Median_Catch[period=="last"]/1000,0),
            round(IAV1_Catch[period=="initial"]/1000,0),
            round(IAV1_Catch[period=="short"]/1000,0),
            round(IAV1_Catch[period=="med"]/1000,0),
            round(IAV1_Catch[period=="last"]/1000,0),
            round(closure[period=="initial"]*100,0),
            round(closure[period=="short"]*100,0),
            round(closure[period=="med"]*100,0),
            round(closure[period=="last"]*100,0),
            round(firstyear_B1plus_Blim[period=="all"],0),
            round(firstyear_B1plus_Blow[period=="all"],0),
            round(avg_P_B1plus_Blim[period=="initial"]*100,1),
            round(avg_P_B1plus_Blow[period=="initial"]*100,1),
            round(avg_P_B1plus_Blim[period=="short"]*100,1),
            round(avg_P_B1plus_Blow[period=="short"]*100,1),
            round(max_P_B1plus_Blim[period=="initial"]*100,1),
            round(max_P_B1plus_Blow[period=="short"]*100,1),
            round(max_P_B1plus_Blim[period=="med"]*100,1),
            round(max_P_B1plus_Blow[period=="last"]*100,1)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR8to14_ASSss3.csv"), sep=";", col.names=F)


aux <- df %>% subset(Rule %in% c("HCR7") & Ass=="ASSss3") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000,0),
            round(Median_B1plus[period=="short"]/1000,0),
            round(Median_B1plus[period=="last"]/1000,0),
            round(Median_F[period=="initial"],3),
            round(Median_F[period=="short"],3),
            round(Median_F[period=="last"],3),
            round(Median_Catch[period=="initial"]/1000,0),
            round(Median_Catch[period=="short"]/1000,0),
            round(Median_Catch[period=="last"]/1000,0),
            round(IAV1_Catch[period=="initial"]/1000,0),
            round(IAV1_Catch[period=="short"]/1000,0),
            round(IAV1_Catch[period=="last"]/1000,0),
            round(closure[period=="initial"]*100,0),
            round(closure[period=="short"]*100,0),
            round(closure[period=="last"]*100,0),
            #round(P_B1plus_0.8Blim[period=="initial"]*100,0),
            #round(P_B1plus_0.8Blow[period=="initial"]*100,0),
            #round(firstyear_B1plus_0.8Blim[period=="all"],0),
            #round(firstyear_B1plus_0.8Blow[period=="all"],0),
            round(firstyear_B1plus_Blim[period=="all"],0),
            round(firstyear_B1plus_Blow[period=="all"],0),
            round(max_P_B1plus_Blim[period=="last"]*100,0),
            round(max_P_B1plus_Blow[period=="last"]*100,0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR7_ASSss3.csv"), sep=";", col.names=F)

aux <- df %>% subset(Rule %in% c("HCR0","HCR11") & Ass=="ASSss3") %>%
  group_by(Rec, Rule) %>%
  summarise(round(Median_B1plus[period=="initial"]/1000,0),
            round(Median_B1plus[period=="short"]/1000,0),
            round(Median_B1plus[period=="last"]/1000,0),
            round(Median_F[period=="initial"],3),
            round(Median_F[period=="short"],3),
            round(Median_F[period=="last"],3),
            round(Median_Catch[period=="initial"]/1000,0),
            round(Median_Catch[period=="short"]/1000,0),
            round(Median_Catch[period=="last"]/1000,0),
            round(IAV1_Catch[period=="initial"]/1000,0),
            round(IAV1_Catch[period=="short"]/1000,0),
            round(IAV1_Catch[period=="last"]/1000,0),
            round(closure[period=="initial"]*100,0),
            round(closure[period=="short"]*100,0),
            round(closure[period=="last"]*100,0),
            #round(P_B1plus_0.8Blim[period=="initial"]*100,0),
            #round(P_B1plus_0.8Blow[period=="initial"]*100,0),
            #round(firstyear_B1plus_0.8Blim[period=="all"],0),
            #round(firstyear_B1plus_0.8Blow[period=="all"],0),
            round(firstyear_B1plus_Blim[period=="all"],0),
            round(firstyear_B1plus_Blow[period=="all"],0),
            round(max_P_B1plus_Blim[period=="last"]*100,0),
            round(max_P_B1plus_Blow[period=="last"]*100,0)
  )
write.table(t(aux), file=file.path(res.dir, "table_report_HCR0&HCR11_ASSss3.csv"), sep=";", col.names=F)

