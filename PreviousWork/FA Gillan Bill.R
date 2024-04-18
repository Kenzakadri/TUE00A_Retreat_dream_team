rm(list=ls()) #Clear the work space
dev.off() # clear all plots
# In this script:
# 1: Show correlation between dplyr::selected subscales
# 2: Perform factor analyses on all participants of the discovery sample, as well as on a half-split of the sample to check robustness of factor analysis
# 3: Some control analyses: including all subscales

#### Settings and loading the data ####
##setwd("~/Documents/R")
setwd("~/Documents/R")
plotFolder="./ExportedFigures"
statsFolder="./ExportedStats"
dataFolder="/Users/kenzakadri/Documents/R"
#savePath="/Users/kenzakadri/Documents/R/Results"
##dev.off() #Clear all plots
#Load packages
library(data.table)
library(ggplot2)
library(magrittr)
library(xml2)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(nloptr)
library(dplyr)
library(glmnet)
library(sjPlot)
library(ggpubr) 
library(GPArotation)
#load the data subscales (as a data table)
myDataT <-read.csv(file.path(dataFolder,'individual_items_study2.csv')) #fread("./Data_pilotStudy/Data/questionnaireSubscales.csv") 
# SCZ <- myDataT %>% select(SCZ_1:SCZ_43) %>% rowSums(na.rm = TRUE)
# OCI <- myDataT %>% select(OCI_1:OCI_18) %>% rowSums(na.rm = TRUE)
# EAT <- myDataT %>% select(EAT_1:EAT_26) %>% rowSums(na.rm = TRUE)
# AES <- myDataT %>% select(AES_1:AES_18) %>% rowSums(na.rm = TRUE)
# AUDIT <- myDataT %>% select(AUDIT_1:AUDIT_10) %>% rowSums(na.rm = TRUE)
# SDS <- myDataT %>% select(SDS_1:SDS_20) %>% rowSums(na.rm = TRUE)
# STAI <- myDataT %>% select(STAI_1:STAI_20) %>% rowSums(na.rm = TRUE)
# BIS <- myDataT %>% select(BIS_1:BIS_30) %>% rowSums(na.rm = TRUE)
# LSAS <- myDataT %>% select(LSAS_1:LSAS_24) %>% rowSums(na.rm = TRUE)
# load the total scores (to make illustration figures)

SCZ = myDataT[,c(2:44)]
OCI = myDataT[,c(45:62)]
EAT = myDataT[,c(63:88)]
AES = myDataT[,c(89:106)]
AUDIT = myDataT[,c(107:116)]
SDS = myDataT[,c(117:136)]
STAI = myDataT[,c(137:156)]
BIS = myDataT[,c(157:186)]
LSAS = myDataT[,c(188:211)]
SUBJ = myDataT[,c(187)]
summaryScores <- data.frame(SUBJ,SCZ,OCI,EAT,AES,AUDIT,SDS,STAI,BIS,LSAS)
summaryScores <- summaryScores %>% group_by(SUBJ)
write.csv(summaryScores, "./Gillian Work/summaryscores.csv", row.names = FALSE)
# Reversing Scores
reverse.likert <- c(
  "1" = 4,
  "2" = 3,
  "3" = 2,
  "4" = 1)
# Q1, 8 9 & 12 need to be reversed
BIS <- BIS %>% mutate(BIS_1 = recode(BIS_1, !!!reverse.likert),
           BIS_8 = recode(BIS_8, !!!reverse.likert),
           BIS_9 = recode(BIS_9, !!!reverse.likert),
           BIS_12 = recode(BIS_12, !!!reverse.likert)
          )
# Q1, 3, 6, 7, 10, 13, 14, 16, 19
STAI <- STAI %>% mutate(STAI_1 = recode(STAI_1, !!!reverse.likert),
                        STAI_3 = recode(STAI_3, !!!reverse.likert),
                        STAI_6 = recode(STAI_6, !!!reverse.likert),
                        STAI_7 = recode(STAI_7, !!!reverse.likert),
                        STAI_10 = recode(STAI_10, !!!reverse.likert),
                        STAI_13 = recode(STAI_13, !!!reverse.likert),
                        STAI_14 = recode(STAI_14, !!!reverse.likert),
                        STAI_16 = recode(STAI_16, !!!reverse.likert),
                        STAI_19 = recode(STAI_19, !!!reverse.likert)
                        )
  
# Making Summaries
BIS_ImpusivBehaviourV <- c(which(names(BIS)%in%c("BIS_2", "BIS_5", "BIS_14", "BIS_19")))
BIS_NoSelfControlV <- c(which(names(BIS)%in%c("BIS_1", "BIS_8", "BIS_9", "BIS_12")))
BIS <- cbind(BIS, BIS_ImpulsiveBehaviour = rowSums(BIS[BIS_ImpusivBehaviourV]), BIS_NoSelfControl = rowSums(BIS[BIS_NoSelfControlV]))
# As these are the only ones we care about, extracting them
BIS = BIS[,c(1,2,5,8,9,12,14,19,31,32)]

STAI_AnxietyAbsentV <- c(which(names(STAI)%in%c("STAI_1","STAI_3","STAI_6","STAI_7","STAI_10","STAI_13","STAI_14","STAI_16","STAI_19")))
STAI_AnxietyPresentV <- c(which(names(STAI)%in%c("STAI_2","STAI_4","STAI_5","STAI_8","STAI_9","STAI_11","STAI_12","STAI_15","STAI_17","STAI_18","STAI_20")))
STAI <- cbind(STAI, STAI_AnxietyPresent = rowSums(STAI[STAI_AnxietyPresentV]), STAI_AnxietyAbsent = rowSums(STAI[STAI_AnxietyAbsentV]))

STAI <- STAI %>% mutate(STAI_AnxietyPresent = STAI_2 + STAI_4 + STAI_5 + STAI_8 + STAI_9 + STAI_11 + STAI_12 + STAI_15 + STAI_17 + STAI_18 + STAI_20,
                        STAI_AnxietyAbsent = STAI_1 + STAI_3 + STAI_6 + STAI_7 + STAI_10 + STAI_13 + STAI_14 + STAI_16 + STAI_19)


SubScales <- summaryScores %>% mutate(STAI_AnxietyPresent = STAI_2 + STAI_4 + STAI_5 + STAI_8 + STAI_9 + STAI_11 + STAI_12 + STAI_15 + STAI_17 + STAI_18 + STAI_20,
                                      STAI_AnxietyAbsent = STAI_1 + STAI_3 + STAI_6 + STAI_7 + STAI_10 + STAI_13 + STAI_14 + STAI_16 + STAI_19,
                                      BIS_ImpusivBehaviour = BIS_2 + BIS_5 + BIS_14 + BIS_19,
                                      BIS_NoSelfControl = BIS_1 + BIS_8 + BIS_9 + BIS_12)
