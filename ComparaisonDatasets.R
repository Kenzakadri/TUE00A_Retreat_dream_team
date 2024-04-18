rm(list=ls()) #Clear the work space
dev.off() # clear all plots

#### Settings and loading the data ####

dataFolder="~/Documents/GitHub/TUE00A_Retreat_dream_team/datasets"
dev.off() #Clear all plots
#Load packages
library(data.table)
library(ggplot2)
library(magrittr)
library(xml2)
library(ggpubr)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(glmnet)
library(sjPlot)
library(ggpubr) 
library(nloptr)
library(dplyr)

Gillan <-read.csv(file.path(dataFolder,'Gillanetal2016.csv')) 
Scholl <-read.csv(file.path(dataFolder,'Scholletalraw.csv')) 


SCZ_G <- Gillan %>% select(SCZ_1:SCZ_43)  %>% rowSums(na.rm = TRUE)
OCI_G <- Gillan %>% select(OCI_1:OCI_18) %>% rowSums(na.rm = TRUE)
LSAS_G <- Gillan %>% select(LSAS_1:LSAS_24) %>% rowSums(na.rm = TRUE)



SCZ_S <- Scholl %>% select(Schizo.Q1:Schizo.Q33) %>% rowSums(na.rm = TRUE)
OCI_S <- Scholl %>% select(OCI.Q1:OCI.Q18) %>% rowSums(na.rm = TRUE)
LSAS_S <- Scholl %>% select(SocialAnx.Q1:SocialAnx.Q48) %>% rowSums(na.rm = TRUE)


# check oci

