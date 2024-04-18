rm(list=ls()) #Clear the work space
#dev.off() # clear all plots
# In this script:
# 1: Show correlation between dplyr::selected subscales
# 2: Perform factor analyses on all participants of the discovery sample, as well as on a half-split of the sample to check robustness of factor analysis
# 3: Some control analyses: including all subscales

#### Settings and loading the data ####
setwd("~/Documents/R")
plotFolder="./ExportedFigures"
statsFolder="./ExportedStats"
dataFolder="/Users/kenzakadri/Documents/R"
##savePath="/Users/JScholl/Documents/Experiments/OnlineProspective/Data_discoverySample/R_fits"
dev.off() #Clear all plots
#Load packages
library(psych)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)#
library(corrplot)
library(glmnet)
library(sjPlot)
library(ggpubr) 





#load the data subscales (as a data table)
myDataT <-read.csv(file.path(dataFolder,'questionnaireSubscales.csv')) #fread("./Data_pilotStudy/Data/questionnaireSubscales.csv") 
myDataAllSubscales=dplyr::select(myDataT,-c(ID))
myData=dplyr::select(myDataT,-c(ID,starts_with("fiveFacets"),schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,
                         starts_with("inventoryOfDepressive"),starts_with("PANAS"),starts_with("AUDIT"),
                         starts_with("barratt")))
# load the total scores (to make illustration figures)
summaryScores=read.csv(file.path(dataFolder,'questionnaireTotalScores.csv')) #fread("./Data_pilotStudy/Data/questionnaireTotalScores.csv")

#### Control analyses ####
# 1) Correlation plot for all subscales collected in the discovery sample
myData.names=colnames(myDataAllSubscales)
myData.shortNames=gsub("liebowitzSocialAnxietyScale","SA",myData.names)
myData.shortNames=gsub("torontoAlexithymiaScale","TA",myData.shortNames)
myData.shortNames=gsub("torontoAlexithymiaScale","TA",myData.shortNames)
myData.shortNames=gsub("fiveFacetsOfMindfulness","FFM",myData.shortNames)
myData.shortNames=gsub("becksDepressionInventory","BDI",myData.shortNames)
myData.shortNames=gsub("obsessiveCompulsiveInventory","OCI",myData.shortNames)
myData.shortNames=gsub("schizotypy","SPQ",myData.shortNames)
myData.shortNames=gsub("fatigueAssessmentScale","FAT",myData.shortNames)
myData.shortNames=gsub("barrattImpulsivenessScale","BIS",myData.shortNames)
myData.shortNames=gsub("apathyMotivationIndex","AMI",myData.shortNames)
myData.shortNames=gsub("traitAnxiety","ANX",myData.shortNames)
myData.shortNames=gsub("inventoryOfDepressiveSymptomatology","IDS",myData.shortNames)
myData.shortNames=gsub("PANAS","PN",myData.shortNames)
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
myData.withShortNames.AllSubscales=myDataAllSubscales;
colnames(myData.withShortNames.AllSubscales)=myData.shortNames
myData.withShortNames.AllSubscales$FFM_Observe  = -myData.withShortNames.AllSubscales$FFM_Observe 
myData.withShortNames.AllSubscales$FFM_Describe = -myData.withShortNames.AllSubscales$FFM_Describe 
myData.withShortNames.AllSubscales$FFM_Actaware = -myData.withShortNames.AllSubscales$FFM_Actaware 
myData.withShortNames.AllSubscales$FFM_Nonjudge = -myData.withShortNames.AllSubscales$FFM_Nonjudge 
myData.withShortNames.AllSubscales$FFM_Nonreact = -myData.withShortNames.AllSubscales$FFM_Nonreact 
myData.withShortNames.AllSubscales$PN_Positiveaffect = -myData.withShortNames.AllSubscales$PN_Positiveaffect 

# Make figure of correlations
# sort so that they are in a similar order to the shorter list in main text
#myData.withShortNames.AllSubscales <- myData.withShortNames.AllSubscales %>% filter(Included == 'Included')
myData.withShortNames.AllSubscales$Included <- NULL
myData.withShortNames.AllSubscales=myData.withShortNames.AllSubscales[,
           c(26,25, #alc
             27,28,29,31,6,7, #alexi
             40,39,38,37,  #social anx
             17,15,35,36,20,21,22,30,33,32,13,19,18,5,4,1,24,23, #depranx
             34,16,3,2, #apathy
             14,12,11,10,9,8)] #compulsivity

corr=cor(myData.withShortNames.AllSubscales,'use'='pairwise.complete.obs')
p.mat=cor_pmat(myData.withShortNames.AllSubscales,'use'='pairwise.complete.obs')
# Make image of correlation matrix (Corrplot also has functions for ordering the correlation matrix or it can be done manually)
plot.corr.allSubscales=ggcorrplot(corr,p.mat=p.mat,insig="blank")+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10))+ggtitle(" ")
plot.corr.allSubscales

# 2) Factor solution for all subscales
myCormatFull       =mixedCor(data=myData.withShortNames.AllSubscales,correct=0,d=NULL,p=NULL,smooth=TRUE,global=FALSE)
myFaT=fa(myCormatFull$rho,nfactors=6,n.obs=nrow(myData.withShortNames.AllSubscales),rotate="oblimin",fm="gls")
t1=data.frame(unclass(myFaT$loadings)) # plot
# dplyr::select items that have at least one loading > 0.4
ord=colnames(myData.withShortNames.AllSubscales)
#summary(myFa) or print(myFa) to see the eig values and choose the number of factor
t3=data.table(t1,keep.rownames=TRUE)
t3$rn=factor(t3$rn,levels=ord)
t2B=melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")
t2B$factors=recode_factor(t2B$factors,`GLS2`="Compulsivity",`GLS5`="Alcohol",`GLS1`="DeprAnx",`GLS3`="SocialAnx",`GLS4`="AlexiImpulse",`GLS6`="Apathy")

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


