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


myDataT <- data.frame(apathyMotivationIndex_Behavioural,apathyMotivationIndex_Emotional,apathyMotivationIndex_Social,AUDIT_AdverseConsequences,AUDIT_Consumption,AUDIT_Dependance,barrattImpulsivenessScale_ImpusivBehaviour,barrattImpulsivenessScale_NoSelfControl,liebowitzSocialAnxietyScale_FearInteraction,liebowitzSocialAnxietyScale_FearPerformance,obsessiveCompulsiveInventory_Checking,obsessiveCompulsiveInventory_Neutralizing,obsessiveCompulsiveInventory_Obsessing,obsessiveCompulsiveInventory_Hoarding,obsessiveCompulsiveInventory_Ordering,obsessiveCompulsiveInventory_Washing,schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,schizotypy_IntrovertiveAnhedonia,schizotypy_UnusualExperiences,STAI$STAI_Absent,STAI$STAI_Present)
#myDataT <- data.frame(apathyMotivationIndex_Behavioural,apathyMotivationIndex_Emotional,apathyMotivationIndex_Social,AUDIT_AdverseConsequences,AUDIT_Consumption,AUDIT_Dependance,eatingAttitutesTest_Bulimia,eatingAttitutesTest_Dieting,eatingAttitutesTest_Oralcontrol,BIS$BIS_ImpulsiveBehaviour,BIS$BIS_NoSelfControl,liebowitzSocialAnxietyScale_FearInteraction,liebowitzSocialAnxietyScale_FearPerformance,obsessiveCompulsiveInventory_Checking,obsessiveCompulsiveInventory_Neutralizing,obsessiveCompulsiveInventory_Obsessing,obsessiveCompulsiveInventory_Ordering,obsessiveCompulsiveInventory_Washing,schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,schizotypy_IntrovertiveAnhedonia,schizotypy_UnusualExperiences,Self_Rating_Negative,Self_Rating_Positive,STAI$STAI_AnxietyAbsent,STAI$STAI_AnxietyPresent)

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

# Correlation

myData.withShortNames.AllSubscales$Included <- NULL


myData.withShortNames.AllSubscales=myData.withShortNames.AllSubscales[,c(22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)] #STAI


#myData.withShortNames.AllSubscales=myData.withShortNames.AllSubscales[,
#                                                                      c(1,2,3, #apathy
#                                                                        4,5,6 # AUDIT
#                                                                        ,7,8,9 #EAT
#                                                                        ,10,11 #BIS
#                                                                        ,12,13 #LSAS
#                                                                        ,14,15,16,17,18 #OCI
#                                                                        ,19,20,21,22 #SCZ
#                                                                         )] #STAI

#myData.withShortNames.AllSubscales=myData.withShortNames.AllSubscales[, c(1,2,3, #apathy
# 4,5,6 # AUDIT
# ,7,8,9 #EAT
# ,10,11 #BIS
# ,12,13 #LSAS
# ,14,15,16,17,18 #OCI
# ,19,20,21,22, #SCZ
#23,24 #SDS
# ,25,26  )] #STAI



corr=cor(myData.withShortNames.AllSubscales,'use'='pairwise.complete.obs')
p.mat=cor_pmat(myData.withShortNames.AllSubscales,'use'='pairwise.complete.obs')
# Make image of correlation matrix (Corrplot also has functions for ordering the correlation matrix or it can be done manually)
plot.corr.allSubscales=ggcorrplot(corr,p.mat=p.mat,insig="blank")+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10))+ggtitle(" ")
plot.corr.allSubscales


# 2) Factor solution for all subscales
myCormatFull       =mixedCor(data=myData.withShortNames.AllSubscales,correct=0,d=NULL,p=NULL,smooth=TRUE,global=FALSE)
myFaT=fa(myCormatFull$rho,nfactors=3,n.obs=nrow(myData.withShortNames.AllSubscales),rotate="oblimin",fm="gls")
t1=data.frame(unclass(myFaT$loadings))
# dplyr::select items that have at least one loading > 0.4
ord=colnames(myData.withShortNames.AllSubscales)
t3=data.table(t1,keep.rownames=TRUE)
t3$rn=factor(t3$rn,levels=ord)
t2B=melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")
t2B$factors=recode_factor(t2B$factors,`GLS2`="Anx/Dep",`GLS3`="Compulsive/Intrusive",`GLS1`="Socialwithdrawal")

# replace the labels
plot.sixFactors=ggplot(t2B,aes(rn,(loadings),fill=loadings)) +
  facet_wrap(~ factors, nrow=1) +geom_bar(stat="identity",aes(fill=abs(loadings)>0.4))+
  scale_fill_manual("legend",values=c("TRUE"="black","FALSE"="grey"))+coord_flip()+ylab("Loading strength")+xlab("") +theme_bw(base_size=10)+
  theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+
  ggtitle(" ")
plot.allSubscales.merged=ggarrange(plot.corr.allSubscales,plot.sixFactors,nrow=2,labels=c("A Correlation matrix","B Factor loadings"),hjust=-1)

ggexport(plot.allSubscales.merged,filename=file.path(plotFolder,"AllSubscales.jpg"),width=(7.2*400),height=(9.724*400), res=400,pointsize=10)              
# journal: 89mm (3.5inch), 120-136mm or 183mm (7.205 inch), length:247mm (9.7244), let's go 400dpi
print(plot.sixFactors)



