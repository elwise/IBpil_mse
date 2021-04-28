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
library(dplyr)
library(tidyr)
library(gt)

#==============================================================================
# Tables for the report
#==============================================================================

df <- read.table(file.path(res.dir,"stats.csv"), header=T, sep=";")

df$Rec <- ordered(df$Rec, c("REClow","REClowmed","RECmix"))

df$period <- ordered(df$period, c("initial", "short", "med","last","all"))

aux <-  
  df %>% 
  select(Rule = Rule,Period = period,Ass,Rec, B1plus = Median_B1plus, F = Median_F,Catch = Median_Catch,IAV= IAV1_Catch,
         Closure = closure,"First year Blim"= firstyear_B1plus_Blim, "First year Blow" = firstyear_B1plus_Blow,
         "Risk 1 Blim" = avg_P_B1plus_Blim, "Risk 1 Blow" = avg_P_B1plus_Blow,
         "Risk 3 Blim" = max_P_B1plus_Blim,
         "Risk 3 Blow" = max_P_B1plus_Blow) %>%
  mutate_at(vars(starts_with("Risk")), ~ as.character(round(100 * .x, 1))) %>%
  mutate_at(vars(Closure), ~ as.character(round(100 * .x, 0))) %>%
  mutate_at(vars(starts_with("First")),  ~ as.character(round( .x, 0))) %>%
  mutate_at(vars(B1plus,Catch, IAV), ~ as.character(round(.x/1000, 0))) %>%
  mutate_at(vars(F), ~ as.character(round(.x, 3))) %>%
  pivot_longer(cols = -c(Rule, Period, Rec, Ass), names_to='indicator') %>%
  pivot_wider(names_from = c(Rec,Ass), id_cols=c(Rule, Period,indicator)) %>%
  arrange(Rule, indicator, Period) %>%
  select(1,2,Indicator = 3,4,7,5,8,6,9)%>%
  as.data.frame()
  
#to save for each rule one as to change the filter and the name of the image
  aux %>%
    filter(Rule == "HCR14") %>% select(-Rule) %>%
    filter(
      (Period != "all" & !Indicator %in% c("First year Blim", "First year Blow")) |
      (Period == "all" & Indicator %in% c("First year Blim", "First year Blow"))
    ) %>% 
    mutate_at(vars(starts_with("REC")), replace_na, "\u2014") %>%
    rename_at(vars(starts_with("REC")), ~ sub(pattern = "REC(.*)_ASS(.*)", replacement = "\\1 / \\2", x = .x, perl = TRUE)) %>%
    gt(groupname_col = "Indicator") %>%
    tab_spanner("Recruitment / Assessment", columns = 3:8, id = "rec_ass") %>%
    tab_style(
      locations = cells_column_spanners(spanners = "rec_ass"),
      style = list(cell_text(weight = "bold")))%>%
    gt::gtsave("psHCR14.png")
