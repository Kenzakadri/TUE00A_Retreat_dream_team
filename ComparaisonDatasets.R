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
library(psych)
library(GPArotation)

Gillan <-read.csv(file.path(dataFolder,'Gillanetal2016.csv')) 
Scholl <-read.csv(file.path(dataFolder,'Scholletalraw.csv')) 


SCZ_G <- Gillan %>% select(SCZ_1:SCZ_43)  %>% rowSums(na.rm = TRUE)
SCZ_IntrovertiveAnhedoniaG<- Gillan %>% select(SCZ_24:SCZ_33)%>% rowSums(na.rm = TRUE)
SCZ__UnusualExperiencesG<- Gillan %>% select(SCZ_1:SCZ_12)%>% rowSums(na.rm = TRUE)
SCZ_G = SCZ_IntrovertiveAnhedoniaG + SCZ__UnusualExperiencesG


Scholl_subscale <-read.csv(file.path(dataFolder,'Scholletal2022.csv')) 


SCZ_IntrovertiveAnhedoniaS = Scholl_subscale$schizotypy_IntrovertiveAnhedonia
SCZ__UnusualExperiencesS = Scholl_subscale$schizotypy_UnusualExperiences

# 
ttestBF(SCZ_IntrovertiveAnhedoniaG,SCZ_IntrovertiveAnhedoniaS)
ttestBF(SCZ__UnusualExperiencesS,SCZ__UnusualExperiencesG)

OCI_G <- Gillan %>% select(OCI_1:OCI_18) #%>% rowSums(na.rm = TRUE)
LSAS_G <- Gillan %>% select(LSAS_1:LSAS_24) %>% rowSums(na.rm = TRUE)





OCI_S <- Scholl %>% select(OCI.Q1:OCI.Q18) # %>% rowSums(na.rm = TRUE)
LSAS_S <- Scholl %>% select(SocialAnx.Q1:SocialAnx.Q24) %>% rowSums(na.rm = TRUE)


# check oci
# Gillan
OCI_CheckingG <- Gillan %>% select(OCI_2,OCI_8,OCI_14)%>% rowSums(na.rm = TRUE)
OCI_HoardingG<- Gillan %>% select(OCI_1,OCI_7,OCI_13)%>% rowSums(na.rm = TRUE)
OCI_NeutralizingG<- Gillan %>% select(OCI_4,OCI_10,OCI_16)%>% rowSums(na.rm = TRUE)
OCI_ObsessingG<- Gillan %>% select(OCI_6,OCI_12,OCI_18)%>% rowSums(na.rm = TRUE)
OCI_OrderingG<- Gillan %>% select(OCI_3,OCI_9,OCI_15)%>% rowSums(na.rm = TRUE)
OCI_WashingG<- Gillan %>% select(OCI_5,OCI_11,OCI_17)%>% rowSums(na.rm = TRUE)

# Scholl



OCI_CheckingS <- Scholl_subscale %>% select(obsessiveCompulsiveInventory_Checking)%>% rowSums(na.rm = TRUE)
OCI_HoardingS<- Scholl_subscale %>% select(obsessiveCompulsiveInventory_Hoarding)%>% rowSums(na.rm = TRUE)
OCI_NeutralizingS<- Scholl_subscale %>% select(obsessiveCompulsiveInventory_Neutralizing)%>% rowSums(na.rm = TRUE)
OCI_ObsessingS<- Scholl_subscale %>% select(obsessiveCompulsiveInventory_Obsessing)%>% rowSums(na.rm = TRUE)
OCI_OrderingS<- Scholl_subscale %>% select(obsessiveCompulsiveInventory_Ordering)%>% rowSums(na.rm = TRUE)
OCI_WashingS<- Scholl_subscale %>% select(obsessiveCompulsiveInventory_Washing)%>% rowSums(na.rm = TRUE)

# Checking

oci_checking = t.test(OCI_CheckingG,OCI_CheckingS)

ggplot() +
  geom_density(aes(x = OCI_CheckingG)) +
  geom_density(aes(x = OCI_CheckingS), fill = 'blue', alpha = 0.5, position='dodge') +
 theme_bw()  

# Hoarding

OCI_Hoarding = t.test(OCI_HoardingS,OCI_HoardingG)

ggplot() +
  geom_density(aes(x = OCI_HoardingS)) +
  geom_density(aes(x = OCI_HoardingG), fill = 'blue', alpha = 0.5, position='dodge') +
  theme_bw()  

#Neutralising

OCI_Neutralising = t.test(OCI_NeutralizingG,OCI_NeutralizingS)

ggplot() +
  geom_density(aes(x = OCI_NeutralizingG)) +
  geom_density(aes(x = OCI_NeutralizingS), fill = 'blue', alpha = 0.5, position='dodge') +
  theme_bw()  

boxplot(OCI_NeutralizingG,OCI_NeutralizingS)

# Obssessing
t.test(OCI_ObsessingS,OCI_ObsessingG)

boxplot(OCI_ObsessingS,OCI_ObsessingG)

# ordering
t.test(OCI_OrderingS,OCI_OrderingG)
boxplot(OCI_OrderingS,OCI_OrderingG)

# washing
t.test(OCI_WashingS,OCI_WashingG)

boxplot(OCI_WashingS,OCI_WashingG)

# Social anxiety

LSAS_G_FearInteractionG <- Gillan %>% select(LSAS_5,LSAS_7,LSAS_10,LSAS_11,LSAS_12,LSAS_15,LSAS_18,LSAS_19,LSAS_22,LSAS_23,LSAS_24)%>% rowSums(na.rm = TRUE)
LSAS_G_FearPerformanceG<- Gillan %>% select(LSAS_1:LSAS_4,LSAS_6,LSAS_8,LSAS_9,LSAS_14,LSAS_16,LSAS_17,LSAS_20,LSAS_21)%>% rowSums(na.rm = TRUE)

LSAS_G_FearInteractionS <- Scholl_subscale$liebowitzSocialAnxietyScale_FearInteraction
LSAS_G_FearPerformanceS <- Scholl_subscale$liebowitzSocialAnxietyScale_FearPerformance


ttestBF(LSAS_G_FearInteractionG,LSAS_G_FearInteractionS)
ttestBF(LSAS_G_FearPerformanceG,LSAS_G_FearPerformanceS)

myMatrixSubscaleG <- data.frame(SCZ_IntrovertiveAnhedoniaG,SCZ__UnusualExperiencesG,
                                OCI_CheckingG,OCI_HoardingG, OCI_NeutralizingG, OCI_ObsessingG,
                                OCI_OrderingG,OCI_WashingG,LSAS_G_FearInteractionG,LSAS_G_FearPerformanceG)

myMatrixSubscaleS <- data.frame(SCZ_IntrovertiveAnhedoniaS,SCZ__UnusualExperiencesS,
                                OCI_CheckingS,OCI_HoardingS, OCI_NeutralizingS, OCI_ObsessingS,
                                OCI_OrderingS,OCI_WashingS,LSAS_G_FearInteractionS,LSAS_G_FearPerformanceS)



corrG=cor(myMatrixSubscaleG,'use'='pairwise.complete.obs')
p.mat=cor_pmat(myMatrixSubscaleG,'use'='pairwise.complete.obs')
# Make image of correlation matrix (Corrplot also has functions for ordering the correlation matrix or it can be done manually)
plot.corr.allSubscalesG=ggcorrplot(corrG,p.mat=p.mat,insig="blank")+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10))+ggtitle(" ")
plot.corr.allSubscalesG


corrS=cor(myMatrixSubscaleS,'use'='pairwise.complete.obs')
p.mat=cor_pmat(myMatrixSubscaleS,'use'='pairwise.complete.obs')
# Make image of correlation matrix (Corrplot also has functions for ordering the correlation matrix or it can be done manually)
plot.corr.allSubscalesS=ggcorrplot(corrS,p.mat=p.mat,insig="blank")+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10))+ggtitle(" ")
plot.corr.allSubscalesS


sim.by.hclust <- hclust(dist(corrS))
plot(sim.by.hclust)

sim.by.hclust <- hclust(dist(corrG))
plot(sim.by.hclust)

fa.parallel(myMatrixSubscaleG)
fa.parallel(myMatrixSubscaleS)

myFaT=fa(myMatrixSubscaleG,nfactors=3,n.obs=nrow(myMatrixSubscaleG),rotate="oblimin",fm="gls")
t1=data.frame(unclass(myFaT$loadings))

ord=colnames(myMatrixSubscaleG)
t3=data.table(t1,keep.rownames=TRUE)
t3$rn=factor(t3$rn,levels=ord)
t2B=melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")
t2B$factors=recode_factor(t2B$factors,`GLS2`="Compulsivity",`GLS5`="Alcohol",`GLS1`="DeprAnx",`GLS3`="SocialAnx",`GLS4`="AlexiImpulse",`GLS6`="Apathy")

# replace the labels
plotGillan=ggplot(t2B,aes(rn,(loadings),fill=loadings)) +
  facet_wrap(~ factors, nrow=1) +geom_bar(stat="identity",aes(fill=abs(loadings)>0.4))+
  scale_fill_manual("legend",values=c("TRUE"="black","FALSE"="grey"))+coord_flip()+ylab("Loading strength")+xlab("") +theme_bw(base_size=10)+
  theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+
  ggtitle(" ")

print(plot.allSubscales.merged)
## Scholl

myFaT=fa(myMatrixSubscaleS,nfactors=3,n.obs=nrow(myMatrixSubscaleS),rotate="oblimin",fm="gls")
t1=data.frame(unclass(myFaT$loadings))

ord=colnames(myMatrixSubscaleS)
t3=data.table(t1,keep.rownames=TRUE)
t3$rn=factor(t3$rn,levels=ord)
t2B=melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")
t2B$factors=recode_factor(t2B$factors,`GLS2`="Compulsivity",`GLS5`="Alcohol",`GLS1`="DeprAnx",`GLS3`="SocialAnx",`GLS4`="AlexiImpulse",`GLS6`="Apathy")

# replace the labels
plotScholl=ggplot(t2B,aes(rn,(loadings),fill=loadings)) +
  facet_wrap(~ factors, nrow=1) +geom_bar(stat="identity",aes(fill=abs(loadings)>0.4))+
  scale_fill_manual("legend",values=c("TRUE"="black","FALSE"="grey"))+coord_flip()+ylab("Loading strength")+xlab("") +theme_bw(base_size=10)+
  theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+
  ggtitle(" ")



plot.allSubscales.merged=ggarrange(plot.corr.allSubscalesS,plotScholl,nrow=2,labels=c("A Correlation matrix","B Factor loadings"),hjust=-1)
print(plot.allSubscales.merged)

plot.allSubscales.merged=ggarrange(plot.corr.allSubscalesG,plotGillan,nrow=2,labels=c("A Correlation matrix","B Factor loadings"),hjust=-1)
print(plot.allSubscales.merged)

# 





