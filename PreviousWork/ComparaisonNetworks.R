rm(list=ls()) #Clear the work space
dev.off() # clear all plots
# In this script:
# 1: Show correlation between dplyr::selected subscales
# 2: Perform factor analyses on all participants of the discovery sample, as well as on a half-split of the sample to check robustness of factor analysis
# 3: Some control analyses: including all subscales

#### Settings and loading the data ####
setwd("~/Documents/R")
plotFolder="./ExportedFigures"
statsFolder="./ExportedStats"
dataFolder="/Users/kenzakadri/Documents/R"
#savePath="/Users/kenzakadri/Documents/R/Results"
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
library(igraph)
library("MPsychoR")
library(qgraph)
library(smacof)
library(wordcloud)
library("eigenmodel")

###########################Loading#####################################
############################## Loading data Gillan

#load the data subscales (as a data table)
ind <-read.csv(file.path(dataFolder,'individual_items_study2.csv')) #fread("./Data_pilotStudy/Data/questionnaireSubscales.csv") 
SCZ <- ind %>% select(SCZ_1:SCZ_43) %>% rowSums(na.rm = TRUE)
OCI <- ind %>% select(OCI_1:OCI_18) %>% rowSums(na.rm = TRUE)
#EAT <- ind %>% select(EAT_1:EAT_26) %>% rowSums(na.rm = TRUE)
AES <- ind %>% select(AES_1:AES_18) %>% rowSums(na.rm = TRUE)
AUDIT <- ind %>% select(AUDIT_1:AUDIT_10) %>% rowSums(na.rm = TRUE)

#SDS <- ind %>% select(SDS_1:SDS_20) %>% rowSums(na.rm = TRUE)

STAI <- ind %>% select(STAI_1:STAI_20) %>% rowSums(na.rm = TRUE)
LSAS <- ind %>% select(LSAS_1:LSAS_24) %>% rowSums(na.rm = TRUE)
BIS = ind[,c(157:186)]
STAI = ind[,c(137:156)]

reverse.likert <- c(
  "1" = 1,
  "2" = 2,
  "3" = 3,
  "4" = 4)
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
BIS_ImpusivBehaviour <- c(which(names(BIS)%in%c("BIS_2", "BIS_5", "BIS_14", "BIS_19")))
BIS_NoSelfControl <- c(which(names(BIS)%in%c("BIS_1", "BIS_8", "BIS_9", "BIS_12")))
BIS <- cbind(BIS, BIS_ImpusivBehaviour = rowSums(BIS[BIS_ImpusivBehaviour]), BIS_NoSelfControl = rowSums(BIS[BIS_NoSelfControl]))
# As these are the only ones we care about, extracting them
BIS = BIS[,c(1,2,5,8,9,12,14,19,31,32)]

STAI_Absent <- c(which(names(STAI)%in%c("STAI_1","STAI_3","STAI_6","STAI_7","STAI_10","STAI_13","STAI_14","STAI_16","STAI_19")))
STAI_Present <- c(which(names(STAI)%in%c("STAI_2","STAI_4","STAI_5","STAI_8","STAI_9","STAI_11","STAI_12","STAI_15","STAI_17","STAI_18","STAI_20")))
STAI <- cbind(STAI, STAI_Present = rowSums(STAI[STAI_Present]), STAI_Absent = rowSums(STAI[STAI_Absent]))

STAI <- STAI %>% mutate(STAI_Present = STAI_2 + STAI_4 + STAI_5 + STAI_8 + STAI_9 + STAI_11 + STAI_12 + STAI_15 + STAI_17 + STAI_18 + STAI_20,
                        STAI_Absent = STAI_1 + STAI_3 + STAI_6 + STAI_7 + STAI_10 + STAI_13 + STAI_14 + STAI_16 + STAI_19)




#### doing subscales
###ApathyMotivationIndex
apathyMotivationIndex_Behavioural <- ind %>% select(AES_5,AES_9,AES_10,AES_11,AES_15)%>% rowSums(na.rm = TRUE)
apathyMotivationIndex_Emotional<- ind %>% select(AES_1,AES_6,AES_7,AES_13,AES_16,AES_18)%>% rowSums(na.rm = TRUE)
apathyMotivationIndex_Social<- ind %>% select(AES_2,AES_3,AES_4,AES_8,AES_14,AES_17)%>% rowSums(na.rm = TRUE)

###AUDIT
AUDIT_AdverseConsequences<- ind %>% select(AUDIT_7,AUDIT_8,AUDIT_9,AUDIT_10)%>% rowSums(na.rm = TRUE)
AUDIT_Consumption<- ind %>% select(AUDIT_1,AUDIT_2,AUDIT_3)%>% rowSums(na.rm = TRUE)
AUDIT_Dependance<- ind %>% select(AUDIT_4,AUDIT_5,AUDIT_6)%>% rowSums(na.rm = TRUE)

#####EAT
eatingAttitutesTest_Bulimia<- ind %>% select(EAT_3,EAT_4,EAT_9,EAT_18,EAT_21,EAT_25)%>% rowSums(na.rm = TRUE)
eatingAttitutesTest_Dieting <- ind %>% select(EAT_1,EAT_6,EAT_7,EAT_10,EAT_11,EAT_12,EAT_14,EAT_16,EAT_17,EAT_22,EAT_23,EAT_24,EAT_26)%>% rowSums(na.rm = TRUE)
eatingAttitutesTest_Oralcontrol<- ind %>% select(EAT_2,EAT_5,EAT_8,EAT_13,EAT_15,EAT_19,EAT_20)%>% rowSums(na.rm = TRUE)


###Impulsive

barrattImpulsivenessScale_ImpusivBehaviour <- BIS %>% select(BIS_2,BIS_5,BIS_14,BIS_19)%>% rowSums(na.rm = TRUE)

barrattImpulsivenessScale_NoSelfControl <- BIS %>% select(BIS_1,BIS_8,BIS_9,BIS_12)%>% rowSums(na.rm = TRUE)

### liebowitzSocialAnxietyScale

liebowitzSocialAnxietyScale_FearInteraction <- ind %>% select(LSAS_5,LSAS_7,LSAS_10,LSAS_11,LSAS_12,LSAS_15,LSAS_18,LSAS_19,LSAS_22,LSAS_23,LSAS_24)%>% rowSums(na.rm = TRUE)
liebowitzSocialAnxietyScale_FearPerformance<- ind %>% select(LSAS_1:LSAS_4,LSAS_6,LSAS_8,LSAS_9,LSAS_14,LSAS_16,LSAS_17,LSAS_20,LSAS_21)%>% rowSums(na.rm = TRUE)

###OCI

obsessiveCompulsiveInventory_Checking <- ind %>% select(OCI_2,OCI_8,OCI_14)%>% rowSums(na.rm = TRUE)
obsessiveCompulsiveInventory_Hoarding<- ind %>% select(OCI_1,OCI_7,OCI_13)%>% rowSums(na.rm = TRUE)
obsessiveCompulsiveInventory_Neutralizing<- ind %>% select(OCI_4,OCI_10,OCI_16)%>% rowSums(na.rm = TRUE)
obsessiveCompulsiveInventory_Obsessing<- ind %>% select(OCI_6,OCI_12,OCI_18)%>% rowSums(na.rm = TRUE)
obsessiveCompulsiveInventory_Ordering<- ind %>% select(OCI_3,OCI_9,OCI_15)%>% rowSums(na.rm = TRUE)
obsessiveCompulsiveInventory_Washing<- ind %>% select(OCI_5,OCI_11,OCI_17)%>% rowSums(na.rm = TRUE)

###SCZ
schizotypy_CognitiveDisorganization<- ind %>% select(SCZ_13:SCZ_23)%>% rowSums(na.rm = TRUE)
schizotypy_ImpulsiveNonconformity<- ind %>% select(SCZ_34:SCZ_43)%>% rowSums(na.rm = TRUE)
schizotypy_IntrovertiveAnhedonia<- ind %>% select(SCZ_24:SCZ_33)%>% rowSums(na.rm = TRUE)
schizotypy_UnusualExperiences<- ind %>% select(SCZ_1:SCZ_12)%>% rowSums(na.rm = TRUE)


###SDS

#Self_Rating_Negative<- ind %>% select(SDS_1,SDS_3,SDS_4,SDS_7,SDS_8:SDS_10,SDS_13,SDS_15,SDS_19)%>% rowSums(na.rm = TRUE)
#Self_Rating_Positive<- ind %>% select(SDS_2,SDS_5,SDS_6,SDS_11,SDS_12,SDS_14,SDS_16:SDS_18,SDS_20)%>% rowSums(na.rm = TRUE)

STAI_Absent <- STAI %>% select(STAI_1,STAI_3,STAI_6,STAI_7,STAI_10,STAI_13,STAI_14,STAI_16,STAI_19)%>% rowSums(na.rm = TRUE)
STAI_Present <- STAI %>% select(STAI_2,STAI_4,STAI_5,STAI_8,STAI_9,STAI_11,STAI_12,STAI_15,STAI_17,STAI_18,STAI_20)%>% rowSums(na.rm = TRUE)

# load the total scores (to make illustration figures)

summaryScores <- data.frame(SCZ,OCI,AES,AUDIT,STAI,BIS,LSAS)

#summaryScores <- data.frame(SCZ,OCI,EAT,AES,AUDIT,SDS,STAI,BIS,LSAS)
write.csv(summaryScores,"/Users/kenzakadri/Documents/R\\summaryKK.csv", row.names = TRUE)


G_myDataT <- data.frame(apathyMotivationIndex_Behavioural,apathyMotivationIndex_Emotional,apathyMotivationIndex_Social,AUDIT_AdverseConsequences,AUDIT_Consumption,AUDIT_Dependance,barrattImpulsivenessScale_ImpusivBehaviour,barrattImpulsivenessScale_NoSelfControl,liebowitzSocialAnxietyScale_FearInteraction,liebowitzSocialAnxietyScale_FearPerformance,obsessiveCompulsiveInventory_Checking,obsessiveCompulsiveInventory_Neutralizing,obsessiveCompulsiveInventory_Obsessing,obsessiveCompulsiveInventory_Hoarding,obsessiveCompulsiveInventory_Ordering,obsessiveCompulsiveInventory_Washing,schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,schizotypy_IntrovertiveAnhedonia,schizotypy_UnusualExperiences,STAI$STAI_Absent,STAI$STAI_Present)
#myDataT <- data.frame(apathyMotivationIndex_Behavioural,apathyMotivationIndex_Emotional,apathyMotivationIndex_Social,AUDIT_AdverseConsequences,AUDIT_Consumption,AUDIT_Dependance,eatingAttitutesTest_Bulimia,eatingAttitutesTest_Dieting,eatingAttitutesTest_Oralcontrol,BIS$BIS_ImpulsiveBehaviour,BIS$BIS_NoSelfControl,liebowitzSocialAnxietyScale_FearInteraction,liebowitzSocialAnxietyScale_FearPerformance,obsessiveCompulsiveInventory_Checking,obsessiveCompulsiveInventory_Neutralizing,obsessiveCompulsiveInventory_Obsessing,obsessiveCompulsiveInventory_Ordering,obsessiveCompulsiveInventory_Washing,schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,schizotypy_IntrovertiveAnhedonia,schizotypy_UnusualExperiences,Self_Rating_Negative,Self_Rating_Positive,STAI$STAI_AnxietyAbsent,STAI$STAI_AnxietyPresent)

G_myDataAllSubscales <- G_myDataT
G_myData <- G_myDataT # We take all questionnary but we can change after

#### Control analyses ####
# 1) Correlation plot for all subscales collected in the discovery sample
myData.names=colnames(G_myDataAllSubscales)
G_myData.shortNames=gsub("liebowitzSocialAnxietyScale","SA",myData.names)
G_myData.shortNames=gsub("obsessiveCompulsiveInventory","OCI",G_myData.shortNames)
G_myData.shortNames=gsub("schizotypy","SPQ",G_myData.shortNames)
G_myData.shortNames=gsub("barrattImpulsivenessScale","BIS",G_myData.shortNames)
G_myData.shortNames=gsub("apathyMotivationIndex","AMI",G_myData.shortNames)
G_myData.shortNames=gsub("traitAnxiety","ANX",G_myData.shortNames)
G_myData.shortNames=gsub("inventoryOfDepressiveSymptomatology","IDS",G_myData.shortNames)
G_myData.shortNames=gsub("UnusualExperiences","UnusExp",G_myData.shortNames)
G_myData.shortNames=gsub("IntrovertiveAnhedonia","Anhedonia",G_myData.shortNames)
G_myData.shortNames=gsub("CognitiveDisorganization","CogDisorg",G_myData.shortNames)
G_myData.shortNames=gsub("ImpulsiveNonconformity","ImpulsNonconf",G_myData.shortNames)
G_myData.shortNames=gsub("DifficultyIdentifingFeelings","DiffIdentifyFeel",G_myData.shortNames)
G_myData.shortNames=gsub("TA_DifficultyDescribingFeelings","TA_DiffDescrFeel",G_myData.shortNames)
G_myData.shortNames=gsub("ExternallyOrientedThinking","ExtThink",G_myData.shortNames)
G_myData.shortNames=gsub("Performance","Perf",G_myData.shortNames)
G_myData.shortNames=gsub("Avoidance","Avoid",G_myData.shortNames)
G_myData.shortNames=gsub("AdverseConsequences","AdvConseq",G_myData.shortNames)

G_myData.withShortNames.AllSubscales=G_myDataAllSubscales;
colnames(G_myData.withShortNames.AllSubscales)=G_myData.shortNames

# Correlation

G_myData.withShortNames.AllSubscales$Included <- NULL


G_myData.withShortNames.AllSubscales=G_myData.withShortNames.AllSubscales[,c(22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)] 
#G_myData.withShortNames.AllSubscales=G_myData.withShortNames.AllSubscales[,c(1,2,4,5,6,13,14,15,20,21,22,7,8,9,10,11,12,17,18,19,3,16)]
G_myData.withShortNames.AllSubscales=G_myData.withShortNames.AllSubscales[,c(1,2,4,5,6,13,14,15,7,8,9,10,11,12,17,18,19,3,16)]



###########@##Load Jacquie Data######################

J_myDataT <-read.csv(file.path(dataFolder,'questionnaireSubscales.csv')) #fread("./Data_pilotStudy/Data/questionnaireSubscales.csv") 
J_allitem <- read.csv(file.path(dataFolder,'questionnaireAllItems_allSubsInclusionIndex.csv'))
AUDIT_Dependance<- J_allitem %>% select(alcohol_Q4,alcohol_Q5,alcohol_Q6)%>% rowSums(na.rm = TRUE)
J_myDataT <- mutate(J_myDataT,AUDIT_Dependance)


J_myDataAllSubscales=dplyr::select(J_myDataT,-c(ID))
J_myData=dplyr::select(J_myDataT,-c(ID),-c(starts_with("fiveFacets")),
                     -c(starts_with("inventoryOfDepressive"),starts_with("PANAS"),
                        starts_with('fatigue'),starts_with('toronto'),starts_with('liebowitzSocialAnxietyScale_Avoidance'),starts_with('becks'),starts_with("apathy")))

J_myDataAllSubscales=dplyr::select(J_myDataAllSubscales,-c(starts_with("fatigue"),starts_with("inventoryOfDepressive"),starts_with("fiveFacets"),starts_with("toronto"),starts_with("PANAS"),starts_with('liebowitzSocialAnxietyScale_Avoidance'),starts_with('becks'),starts_with("apathy")))

J_myDataT=dplyr::select(J_myDataT,-c(starts_with("fatigue"),starts_with("inventoryOfDepressive"),starts_with("fiveFacets"),starts_with("toronto"),starts_with("PANAS"),starts_with('liebowitzSocialAnxietyScale_Avoidance'),starts_with('becks'),starts_with("apathy")))


# load the total scores (to make illustration figures)
J_summaryScores=read.csv(file.path(dataFolder,'questionnaireTotalScores.csv')) #fread("./Data_pilotStudy/Data/questionnaireTotalScores.csv")
J_summaryScores=dplyr::select(J_summaryScores,-c(ID),-c(starts_with("fatigue"),starts_with("inventoryOfDepressive"),starts_with("fiveFacets"),starts_with("toronto"),starts_with("PANAS"),starts_with("apathy")))



J_allitem=dplyr::select(J_allitem,-c(starts_with("fatigue"),starts_with("IDepr"),starts_with("Mindful"),starts_with("Alexi"),starts_with("PANAS"),starts_with("apathy")))





#### Control analyses ####
# 1) Correlation plot for all subscales collected in the discovery sample
J_myData.names=colnames(J_myDataAllSubscales)
J_myData.shortNames=gsub("liebowitzSocialAnxietyScale","SA",J_myData.names)
J_myData.shortNames=gsub("becksDepressionInventory","BDI",J_myData.shortNames)
J_myData.shortNames=gsub("obsessiveCompulsiveInventory","OCI",J_myData.shortNames)
J_myData.shortNames=gsub("schizotypy","SPQ",J_myData.shortNames)
J_myData.shortNames=gsub("barrattImpulsivenessScale","BIS",J_myData.shortNames)
J_myData.shortNames=gsub("apathyMotivationIndex","AMI",J_myData.shortNames)
J_myData.shortNames=gsub("traitAnxiety","STAI",J_myData.shortNames)
J_myData.shortNames=gsub("UnusualExperiences","UnusExp",J_myData.shortNames)
J_myData.shortNames=gsub("IntrovertiveAnhedonia","Anhedonia",J_myData.shortNames)
J_myData.shortNames=gsub("CognitiveDisorganization","CogDisorg",J_myData.shortNames)
J_myData.shortNames=gsub("ImpulsiveNonconformity","ImpulsNonconf",J_myData.shortNames)
J_myData.shortNames=gsub("Performance","Perf",J_myData.shortNames)
J_myData.shortNames=gsub("Avoidance","Avoid",J_myData.shortNames)
J_myData.shortNames=gsub("AdverseConsequences","AdvConseq",J_myData.shortNames)
J_myData.withShortNames.AllSubscales=J_myDataAllSubscales;
colnames(J_myData.withShortNames.AllSubscales)=J_myData.shortNames
# Make figure of correlations
# sort so that they are in a similar order to the shorter list in main text
#myData.withShortNames.AllSubscales <- myData.withShortNames.AllSubscales %>% filter(Included == 'Included')
J_myData.withShortNames.AllSubscales$Included <- NULL

J_myDataT <- J_myDataT[,c(3:21)]
J_myData <- J_myData[,c(2:20)]

J_myData.withShortNames.AllSubscales=J_myData.withShortNames.AllSubscales[,c(17,16,12,14,15,13,10,8,6,11,9,7,4,5,18,19,2,3,1)]

J_myData.withShortNames.AllSubscales=J_myData.withShortNames.AllSubscales[,c(4,6,10,3,7,8,1,15,19,11,12,14,18,9,13,16,5,2,17)]


#############################Gillan Network ######################@##############
### FD models

GillanN <- cor(G_myData.withShortNames.AllSubscales)
qgraph(GillanN, layout="spring")

##### MDS

G_dissimilarity_adult <- sim2diss(GillanN)
G_MDS <- mds(G_dissimilarity_adult)

head(round(G_MDS$conf, 2)) # top of # configuration matrix

#########SHepard plot
par(mfrow=c(2,2))
G_MDS_ordinal <- mds(G_dissimilarity_adult,type="ordinal")
plot(G_MDS_ordinal, plot.type = "Shepard",main="Ordinal")
text(1.1,0.3, paste("Stress =",round(G_MDS_ordinal$stress,2)))

G_MDS_ratio <- mds(G_dissimilarity_adult,type="ratio")
plot(G_MDS_ratio, plot.type = "Shepard",main="Ratio")
text(1.1,0.3, paste("Stress =",round(G_MDS_ratio$stress,2)))

G_MDS_interval <- mds(G_dissimilarity_adult,type="interval")
plot(G_MDS_interval, plot.type = "Shepard",main="Interval")
text(1.1,0.3, paste("Stress =",round(G_MDS_interval$stress,2)))

G_MDS_mspline <- mds(G_dissimilarity_adult,type="mspline")
plot(G_MDS_mspline, plot.type = "Shepard",main="Spline")
text(1.1,0.3, paste("Stress =", round(G_MDS_mspline$stress,2)))

dev.off() # clear all plots

### plot the lower stress


qgraph(GillanN, layout=G_MDS_ordinal$conf,  vsize=4)
text(-1,-1, paste("Stress=",
                  round(G_MDS_ordinal$stress,2)))

# Remove the 
qgraph(GillanN,
       layout=G_MDS_mspline$conf,
       vsize=0, rescale=FALSE, labels=FALSE)
points(G_MDS_mspline$conf, pch=16)
textplot(G_MDS_mspline$conf[,1]+.03,
         G_MDS_mspline$conf[,2]+.03,
         colnames(GillanN),
         new=F)

G_glasso <- EBICglasso(cor(G_myData.withShortNames.AllSubscales), n=408)
qgraph(G_glasso,
       layout=G_MDS_mspline$conf,
       vsize=4)


text(-1,-1, paste("Stress=",
                  round(G_MDS_mspline$stress,2)))

### PCA and eigenvalue

PCA_G <- principal(cor(G_myData.withShortNames.AllSubscales),nfactors = 2)

qgraph(G_glasso, layout=PCA_G$loadings,
       title="PCA Gillan data",
       layoutOffset=c(.3,.1), vsize=4)


text(1.5,-.8, paste("% var=",
                    round(sum(PCA_G$values[1:2]/
                                length(PCA_G$values)),2)))

title(xlab="Component 1",
      
      ylab= "Component 2")


### eigen network

diag(G_glasso) <- NA ## the function

# needs NA diagonals

p <- 2 ## 2-dimensional solution

fitEM <- eigenmodel_mcmc(Y = G_glasso,
                         
                         R = p, S = 1000, burn = 200, seed = 123)

EVD <- eigen(fitEM$ULU_postmean)

evecs <- EVD$vec[, 1:p] ## eigenvectors

# (coordinates)

qgraph(G_glasso, layout=evecs, vsize=4)
title(xlab="Dimension 1", ylab= "Dimension 2")



#############################Jacquie Network ######################@##############
### FD models

JackieN <- cor(J_myData.withShortNames.AllSubscales,use = "complete.obs")
qgraph(JackieN, layout="spring")

##### MDS

J_dissimilarity_adult <- sim2diss(JackieN)
J_MDS <- mds(J_dissimilarity_adult)

head(round(J_MDS$conf, 2)) # top of # configuration matrix

#########SHepard plot
par(mfrow=c(2,2))
J_MDS_ordinal <- mds(J_dissimilarity_adult,type="ordinal")
plot(J_MDS_ordinal, plot.type = "Shepard",main="Ordinal")
text(1.1,0.3, paste("Stress =",round(J_MDS_ordinal$stress,2)))

J_MDS_ratio <- mds(J_dissimilarity_adult,type="ratio")
plot(J_MDS_ratio, plot.type = "Shepard",main="Ratio")
text(1.1,0.3, paste("Stress =",round(J_MDS_ratio$stress,2)))

J_MDS_interval <- mds(J_dissimilarity_adult,type="interval")
plot(J_MDS_interval, plot.type = "Shepard",main="Interval")
text(1.1,0.3, paste("Stress =",round(J_MDS_interval$stress,2)))

J_MDS_mspline <- mds(J_dissimilarity_adult,type="mspline")
plot(J_MDS_mspline, plot.type = "Shepard",main="Spline")
text(1.1,0.3, paste("Stress =", round(J_MDS_mspline$stress,2)))

dev.off() # clear all plots

### plot the lower stress


qgraph(JackieN, layout=J_MDS_ordinal$conf,  vsize=4)
text(-1,-1, paste("Stress=",
                  round(J_MDS_ordinal$stress,2)))

# Remove the 
qgraph(JackieN,
       layout=J_MDS_mspline$conf,
       vsize=0, rescale=FALSE, labels=FALSE)
points(J_MDS_mspline$conf, pch=16)
textplot(J_MDS_mspline$conf[,1]+.03,
         J_MDS_mspline$conf[,2]+.03,
         colnames(J_myData.withShortNames.AllSubscales),
         new=F)

J_glasso <- EBICglasso(cor(J_myData.withShortNames.AllSubscales,use = "complete.obs"), n=408)
qgraph(J_glasso,
       layout=J_MDS_mspline$conf,
       vsize=4)


text(-1,-1, paste("Stress=",
                  round(J_MDS_mspline$stress,2)))

### PCA and eigenvalue

PCA_J <- principal(cor(J_myData.withShortNames.AllSubscales,use = "complete.obs"),nfactors = 2)

qgraph(J_glasso, layout=PCA_J$loadings,
       title="PCA Jacquie data",
       layoutOffset=c(.3,.1), vsize=4)


text(1.5,-.8, paste("% var=",
                    round(sum(PCA_J$values[1:2]/
                                length(PCA_J$values)),2)))

title(xlab="Component 1",
      
      ylab= "Component 2")


### eigen network

diag(J_glasso) <- NA ## the function

# needs NA diagonals

p <- 2 ## 2-dimensional solution

fitEM <- eigenmodel_mcmc(Y = J_glasso,
                         
                         R = p, S = 1000, burn = 200, seed = 123)

EVD <- eigen(fitEM$ULU_postmean)

evecs <- EVD$vec[, 1:p] ## eigenvectors

# (coordinates)

qgraph(J_glasso, layout=evecs, vsize=4)
title(xlab="Dimension 1", ylab= "Dimension 2")


###################### Comparison of networks #################@


fit_procrustes <- Procrustes(J_MDS_mspline$conf, G_MDS$conf)

par(mfrow=c(2,1)) 





qgraph(J_glasso, layout=fit_procrustes$X,
       groups = list("Anx/SPQ" = 1:8,
                     
                     "Compulsivity/Unsualexp" = 9:14,"AUDIT"=15:17,"Others"=18:19),
       
       color = c("lightblue", "lightsalmon","lightseagreen","gold"), title="Jacquie")

text(-1,-1, paste("Stress=",
                  round(J_MDS_mspline$stress,2)))


qgraph(G_glasso, layout=fit_procrustes$X,
       groups = list("Anx/SPQ" = 1:8,
                     
                     "Compulsivity" = 9:14,"AUDIT"=15:17,"Others"=18:19),
       
       color = c("lightblue", "lightsalmon","lightseagreen","gold"), title="Gillan")

text(-1,-1, paste("Stress=",
                  round(G_MDS_mspline$stress,2)))

############### Test plot 
fit_procrustes <- Procrustes(J_MDS_mspline$conf, G_MDS$conf)

par(mfrow=c(2,1)) 



qgraph(J_glasso, layout=fit_procrustes$X,
       groups = list("Anx/SPQ" = 1:9,
                     
                     "Compulsivity/Unsualexp" = 10:15,"AUDIT"=16:18,"Others"=19),
       
       color = c("lightblue", "lightsalmon","lightseagreen","gold"), title="Jacquie")

text(-1,-1, paste("Stress=",
                  round(J_MDS_mspline$stress,2)))


qgraph(G_glasso, layout=fit_procrustes$X,
       groups = list("Anx/SPQ" = 1:8,
                     
                     "Compulsivity" = 9:14,"AUDIT"=15:17,"Others"=18:19),
       
       color = c("lightblue", "lightsalmon","lightseagreen","gold"), title="Gillan")

text(-1,-1, paste("Stress=",
                  round(G_MDS_mspline$stress,2)))


