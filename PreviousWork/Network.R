library(igraph)
library("MPsychoR")
library(psych)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)#
library(corrplot)
library(glmnet)
library(sjPlot)
library(ggpubr) 
library(qgraph)
library(smacof)
library(wordcloud)
library("eigenmodel")


setwd("~/Documents/R")
dataFolder="/Users/kenzakadri/Documents/R"

data(Rogers)
data(Rogers_Adolescent)




colnames(Rogers) <- colnames(Rogers_Adolescent)<- 1:26

#load the data subscales (as a data table)
ind <-read.csv(file.path(dataFolder,'individual_items_study2.csv')) #fread("./Data_pilotStudy/Data/questionnaireSubscales.csv") 
SCZ <- ind %>% select(SCZ_1:SCZ_43) %>% rowSums(na.rm = TRUE)
OCI <- ind %>% select(OCI_1:OCI_18) %>% rowSums(na.rm = TRUE)
EAT <- ind %>% select(EAT_1:EAT_26) %>% rowSums(na.rm = TRUE)
AES <- ind %>% select(AES_1:AES_18) %>% rowSums(na.rm = TRUE)
AUDIT <- ind %>% select(AUDIT_1:AUDIT_10) %>% rowSums(na.rm = TRUE)

SDS <- ind %>% select(SDS_1:SDS_20) %>% rowSums(na.rm = TRUE)

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

cor(STAI$STAI_1,STAI$STAI_2, method="pearson")

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

BIS_ImpusivBehaviour <- BIS %>% select(BIS_2,BIS_5,BIS_14,BIS_19)%>% rowSums(na.rm = TRUE)

BIS_NoSelfControl <- BIS %>% select(BIS_1,BIS_8,BIS_9,BIS_12)%>% rowSums(na.rm = TRUE)

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

Self_Rating_Negative<- ind %>% select(SDS_1,SDS_3,SDS_4,SDS_7,SDS_8:SDS_10,SDS_13,SDS_15,SDS_19)%>% rowSums(na.rm = TRUE)
Self_Rating_Positive<- ind %>% select(SDS_2,SDS_5,SDS_6,SDS_11,SDS_12,SDS_14,SDS_16:SDS_18,SDS_20)%>% rowSums(na.rm = TRUE)

STAI_AnxietyAbsent <- STAI %>% select(STAI_1,STAI_3,STAI_6,STAI_7,STAI_10,STAI_13,STAI_14,STAI_16,STAI_19)%>% rowSums(na.rm = TRUE)
STAI_AnxietyPresent <- STAI %>% select(STAI_2,STAI_4,STAI_5,STAI_8,STAI_9,STAI_11,STAI_12,STAI_15,STAI_17,STAI_18,STAI_20)%>% rowSums(na.rm = TRUE)

# load the total scores (to make illustration figures)

#summaryScores <- data.frame(SCZ,OCI,AES,AUDIT,STAI,BIS,LSAS)

summaryScores <- data.frame(SCZ,OCI,EAT,AES,AUDIT,SDS,STAI,BIS,LSAS)
write.csv(summaryScores,"/Users/kenzakadri/Documents/R\\summaryKK.csv", row.names = TRUE)


#myDataT <- data.frame(apathyMotivationIndex_Behavioural,apathyMotivationIndex_Emotional,apathyMotivationIndex_Social,AUDIT_AdverseConsequences,AUDIT_Consumption,AUDIT_Dependance,BIS$BIS_ImpulsiveBehaviour,BIS$BIS_NoSelfControl,liebowitzSocialAnxietyScale_FearInteraction,liebowitzSocialAnxietyScale_FearPerformance,obsessiveCompulsiveInventory_Checking,obsessiveCompulsiveInventory_Neutralizing,obsessiveCompulsiveInventory_Obsessing,obsessiveCompulsiveInventory_Hoarding,obsessiveCompulsiveInventory_Ordering,obsessiveCompulsiveInventory_Washing,schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,schizotypy_IntrovertiveAnhedonia,schizotypy_UnusualExperiences,STAI$STAI_AnxietyAbsent,STAI$STAI_AnxietyPresent)
myDataT <- data.frame(apathyMotivationIndex_Behavioural,apathyMotivationIndex_Emotional,apathyMotivationIndex_Social,AUDIT_AdverseConsequences,AUDIT_Consumption,AUDIT_Dependance,eatingAttitutesTest_Bulimia,eatingAttitutesTest_Dieting,eatingAttitutesTest_Oralcontrol,BIS$BIS_ImpulsiveBehaviour,BIS$BIS_NoSelfControl,liebowitzSocialAnxietyScale_FearInteraction,liebowitzSocialAnxietyScale_FearPerformance,obsessiveCompulsiveInventory_Checking,obsessiveCompulsiveInventory_Neutralizing,obsessiveCompulsiveInventory_Obsessing,obsessiveCompulsiveInventory_Ordering,obsessiveCompulsiveInventory_Washing,schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,schizotypy_IntrovertiveAnhedonia,schizotypy_UnusualExperiences,Self_Rating_Negative,Self_Rating_Positive,STAI$STAI_AnxietyAbsent,STAI$STAI_AnxietyPresent)

myDataAllSubscales <- myDataT
myData <- myDataT # We take all questionnary but we can change after

#### Control analyses ####
# 1) Correlation plot for all subscales collected in the discovery sample
myData.names=colnames(myDataAllSubscales)
myData.shortNames=gsub("liebowitzSocialAnxietyScale","SA",myData.names)
myData.shortNames=gsub("obsessiveCompulsiveInventory","OCI",myData.shortNames)
myData.shortNames=gsub("schizotypy","SPQ",myData.shortNames)
myData.shortNames=gsub("barrattImpulsivenessScale","BIS",myData.shortNames)
myData.shortNames=gsub("apathyMotivationIndex","AMI",myData.shortNames)
myData.shortNames=gsub("traitAnxiety","ANX",myData.shortNames)
myData.shortNames=gsub("inventoryOfDepressiveSymptomatology","IDS",myData.shortNames)
myData.shortNames=gsub("UnusualExperiences","UnusExp",myData.shortNames)
myData.shortNames=gsub("IntrovertiveAnhedonia","Anhedonia",myData.shortNames)
myData.shortNames=gsub("CognitiveDisorganization","CogDisorg",myData.shortNames)
myData.shortNames=gsub("ImpulsiveNonconformity","ImpulsNonconf",myData.shortNames)
myData.shortNames=gsub("DifficultyIdentifingFeelings","DiffIdentifyFeel",myData.shortNames)
myData.shortNames=gsub("TA_DifficultyDescribingFeelings","TA_DiffDescrFeel",myData.shortNames)
myData.shortNames=gsub("ExternallyOrientedThinking","ExtThink",myData.shortNames)
myData.shortNames=gsub("Performance","Perf",myData.shortNames)
myData.shortNames=gsub("Avoidance","Avoid",myData.shortNames)
myData.shortNames=gsub("AdverseConsequences","AdvConseq",myData.shortNames)

### ???
myData.withShortNames.AllSubscales=myDataAllSubscales;
colnames(myData.withShortNames.AllSubscales)=myData.shortNames


#### Network

### Force-Directed Algorithms (e.g., Fruchterman-Reingold)

adult_zeroorder <- cor(Rogers)
qgraph(adult_zeroorder, layout="spring",
       groups = list(Depression = 1:16, "OCD" = 17:26),
       color = c("lightblue",
                 "lightsalmon"))


test <- cor(myDataT)
qgraph(test, layout="spring")

### here we can put color for each factor for example

dissimilarity_adult <- sim2diss(test)
adult_MDS <- mds(dissimilarity_adult)

head(round(adult_MDS$conf, 2)) # top of # configuration matrix


### Shepard plot

adult_MDS_ordinal <- mds(dissimilarity_adult,type="ordinal")
plot(adult_MDS_ordinal, plot.type = "Shepard",main="Ordinal")
text(1.1,0.3, paste("Stress =",round(adult_MDS_ordinal$stress,2)))

adult_MDS_ratio <- mds(dissimilarity_adult,type="ratio")
plot(adult_MDS_ratio, plot.type = "Shepard",main="Ratio")
text(1.1,0.3, paste("Stress =",round(adult_MDS_ratio$stress,2)))

adult_MDS_interval <- mds(dissimilarity_adult,type="interval")
plot(adult_MDS_interval, plot.type = "Shepard",main="Interval")
text(1.1,0.3, paste("Stress =",round(adult_MDS_interval$stress,2)))

adult_MDS_mspline <- mds(dissimilarity_adult,type="mspline")
plot(adult_MDS_mspline, plot.type = "Shepard",main="Spline")
text(1.1,0.3, paste("Stress =", round(adult_MDS_mspline$stress,2)))

#Here ratio have a higher stress and ordinal seems to be the lower

qgraph(test, layout=adult_MDS_ordinal$conf,  vsize=4)
text(-1,-1, paste("Stress=",
    round(adult_MDS_ordinal$stress,2)))

qgraph(test,
       layout=adult_MDS_mspline$conf,
       vsize=0, rescale=FALSE, labels=FALSE)
points(adult_MDS_mspline$conf, pch=16)
textplot(adult_MDS_mspline$conf[,1]+.03,
         adult_MDS_mspline$conf[,2]+.03,
         colnames(test),
         new=F)

adult_glasso <- EBICglasso(cor(myData), n=408)
qgraph(adult_glasso,
       layout=adult_MDS_mspline$conf,
       vsize=4)


text(-1,-1, paste("Stress=",
                  round(adult_MDS_mspline$stress,2)))

### PCA and eigenvalue

PCA_adult <- principal(cor(test),nfactors = 2)

qgraph(adult_glasso, layout=PCA_adult$loadings,
       title="PCA example",
       layoutOffset=c(.3,.1), vsize=4)


text(1.5,-.8, paste("% var=",
                    round(sum(PCA_adult$values[1:2]/
                                length(PCA_adult$values)),2)))

title(xlab="Component 1",
      
      ylab= "Component 2")




### eigen network

diag(adult_glasso) <- NA ## the function

# needs NA diagonals

p <- 2 ## 2-dimensional solution

fitEM <- eigenmodel_mcmc(Y = adult_glasso,
                         
                         R = p, S = 1000, burn = 200, seed = 123)

EVD <- eigen(fitEM$ULU_postmean)

evecs <- EVD$vec[, 1:p] ## eigenvectors

# (coordinates)

qgraph(adult_glasso, layout=evecs, vsize=4)
title(xlab="Dimension 1", ylab= "Dimension 2")


### Bonus function

adult_glasso <- EBICglasso(cor(test),
                           
                           n=408)

adult_qgraph <- qgraph(adult_glasso)

MDSnet(adult_qgraph, MDSadj = cor(test))

PCAnet(adult_qgraph, cormat = cor(test))

EIGENnet(adult_qgraph)


