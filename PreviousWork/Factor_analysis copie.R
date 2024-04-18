rm(list=ls()) #Clear the work space
dev.off() # clear all plots
# In this script:
# 1: Show correlation between dplyr::selected subscales
# 2: Perform factor analyses on all participants of the discovery sample, as well as on a half-split of the sample to check robustness of factor analysis
# 3: Some control analyses: including all subscales

#### Settings and loading the data ####
setwd("/Users/JScholl/Documents/Experiments/OnlineProspective")
plotFolder="./Data_discoverySample/ExportedFigures"
statsFolder="./Data_discoverySample/ExportedStats"
dataFolder="./Data_discoverySample/Data"
savePath="/Users/JScholl/Documents/Experiments/OnlineProspective/Data_discoverySample/R_fits"
dev.off() #Clear all plots
#Load packages
library(psych)
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)#library(corrplot)
library(glmnet)
library(sjPlot)
library(ggpubr) 

#load the data subscales (as a data table)
myDataT <-fread(file.path(dataFolder,'questionnaireSubscales.csv')) #fread("./Data_pilotStudy/Data/questionnaireSubscales.csv") 
myDataAllSubscales=dplyr::select(myDataT,-c(ID))
myData=dplyr::select(myDataT,-c(ID,starts_with("fiveFacets"),schizotypy_CognitiveDisorganization,schizotypy_ImpulsiveNonconformity,
                         starts_with("inventoryOfDepressive"),starts_with("PANAS"),starts_with("AUDIT"),
                         starts_with("barratt")))
# load the total scores (to make illustration figures)
summaryScores=fread(file.path(dataFolder,'questionnaireTotalScores.csv')) #fread("./Data_pilotStudy/Data/questionnaireTotalScores.csv")

#### Correlation figures ####
# get shorter labels (so that they fit on the correlation plots)
myData.names=colnames(myData)
myData.shortNames=gsub("liebowitzSocialAnxietyScale","SA",myData.names)
myData.shortNames=gsub("torontoAlexithymiaScale","TA",myData.shortNames)
myData.shortNames=gsub("torontoAlexithymiaScale","TA",myData.shortNames)
myData.shortNames=gsub("becksDepressionInventory","BDI",myData.shortNames)
myData.shortNames=gsub("obsessiveCompulsiveInventory","OCI",myData.shortNames)
myData.shortNames=gsub("schizotypy","SPQ",myData.shortNames)
myData.shortNames=gsub("fatigueAssessmentScale","FAT",myData.shortNames)
myData.shortNames=gsub("apathyMotivationIndex","AMI",myData.shortNames)
myData.shortNames=gsub("traitAnxiety","ANX",myData.shortNames)
myData.shortNames=gsub("AnxietyPresent","AnxPresent",myData.shortNames)
myData.shortNames=gsub("AnxietyAbsent","AnxAbsent",myData.shortNames)
myData.shortNames=gsub("UnusualExperiences","UnusExp",myData.shortNames)
myData.shortNames=gsub("IntrovertiveAnhedonia","Anhedonia",myData.shortNames)
myData.shortNames=gsub("DifficultyIdentifingFeelings","DiffIdentFeel",myData.shortNames)
myData.shortNames=gsub("TA_DifficultyDescribingFeelings","TA_DiffDescrFeel",myData.shortNames)
myData.shortNames=gsub("ExternallyOrientedThinking","ExtThink",myData.shortNames)
myData.shortNames=gsub("Performance","Perf",myData.shortNames)
myData.shortNames=gsub("Avoidance","Avoid",myData.shortNames)
myData.shortNames=gsub("AvoidIntearction","AvoidInteract",myData.shortNames)
myData.shortNames=gsub("FearInteraction","FearInteract",myData.shortNames)
myData.withShortNames=myData;
colnames(myData.withShortNames)=myData.shortNames

# Correlations
myData.withShortNames=myData.withShortNames[,c(24,23,22,21,
                                               19,18,11,15,14,5,4,1,17,16,
                                               20,13,3,2,
                                               12,10,9,8,7,6)]

corr=cor(myData.withShortNames,'use'='pairwise.complete.obs')
p.mat=cor_pmat(myData.withShortNames,'use'='pairwise.complete.obs')
# Make image of correlation matrix (Corrplot also has functions for ordering the correlation matrix or it can be done manually)
plot.corr=ggcorrplot(corr,p.mat=p.mat,insig="blank")+theme(axis.text.x=element_text(size=10),axis.text.y=element_text(size=10))+ggtitle(" ")

#### Histograms of total scores with clinical cutoffs highlighted ####
# Let's show some example questionnaires that relate well to our factors: 
# BDI, social anxiety, apathy and compulsivity
p=list()
p$BDI=  gghistogram(summaryScores,x="becksDepressionInventory",bins=20,xlab="",title=" ",
                    fill="black",ylab="# Participants",palette = "jco",ylim=c(0,90)) +
  geom_vline(xintercept = 12.5)+geom_vline(xintercept = 19.5) +geom_vline(xintercept = 28.5)+
  annotate("text", x = 9.5, y = 50, label = "None",hjust=0,angle=90)+#,size=4
  annotate("text", x = 16, y = 50, label = "Mild",hjust=0,angle=90)+
  annotate("text",x=24,y=50,label = "Moderate",hjust=0,angle=90)+
  annotate("text",x=32,y=50,label = "Severe",hjust=0,angle=90)
# see Beck, A. T., Steer, R. A., Ball, R., & Ranieri, W. F. (1996)  and Whisman & Richardson 2015
# <13: minimal, <20 mild, <29 moderat >=29:severe
p$Apathy=gghistogram(summaryScores,x="apathyMotivationIndex",bins=20,fill="black",xlab="",title=" ",ylab="# Participants",palette = "jco",ylim=c(0, 90)) +
  geom_vline(xintercept = (1.91*18))+geom_vline(xintercept=(2.38*18))+
  annotate("text", x = 38, y = 50, label = "Moderate",hjust=0,angle=90)+
  annotate("text", x = 46, y = 50, label = "Severe",hjust=0,angle=90)
# see Angh et al. (2017) [no cut-offs provided for mild]
p$Compulsivity=gghistogram(summaryScores,x="obsessiveCompulsiveInventory",bins=20,fill="black",xlab="",title=" ",ylab="# Participants",palette = "jco",ylim=c(0, 90)) +
  geom_vline(xintercept = 20.5)+
  annotate("text",x=21,y=c(60,52,44),label = c("Above", "clinical","threshold"),hjust=0) #),parse=TRUE)
# see Foa et al. (2002)
p$SocialAnx=gghistogram(summaryScores,x="liebowitzSocialAnxietyScale",bins=20,fill="black",xlab="",title=" ",ylab="# Participants",palette = "jco",ylim=c(0,75)) +
  geom_vline(xintercept = 30)+geom_vline(xintercept = 60)+
  annotate("text",x=15,y=40,label = "None",hjust=0,angle=90)+
  annotate("text",x=c(38,52),y=42,label = c("Social", "anxiety"),hjust=0,angle=90)+ 
  annotate("text",x=c(70,85,100,115),y=42,label = c("General.","social","anxiety","disorder"),hjust=0,angle=90)
# see e.g. Mennin et al. (2002)
plot.psych= ggarrange(p$Compulsivity,p$Apathy,p$BDI,p$SocialAnx,nrow=4,ncol=1)

#### Factor analyses ####
# Do separate factor analyses for the first and the second half of the sample (done with 'psych' package)
# Get correlation matrix (as we use subscales, can do Pearson correlations)
#myData=myData.withShortNames
myData.firstHalf.withShortNames=myData.withShortNames[1:224,]
myData.secondHalf.withShortNames=myData.withShortNames[225:449,]
myCormat           =mixedCor(data=myData.withShortNames,correct=0,d=NULL,p=NULL,smooth=TRUE,global=FALSE)
myCormat.firstHalf =mixedCor(data=myData.firstHalf.withShortNames,correct=0,d=NULL,p=NULL,smooth=TRUE,global=FALSE)
myCormat.secondHalf=mixedCor(data=myData.secondHalf.withShortNames,correct=0,d=NULL,p=NULL,smooth=TRUE,global=FALSE)

# Determine the number of factors using 'Parallel analysis' (Horn, 1965, see also Hayton, Allen & Scrapello, 2004)
fa.parallel(myCormat$rho,n.obs=nrow(myData.withShortNames),fm="gls",nfactors=15) #-> full sample: 4 factors
fa.parallel(myCormat.firstHalf$rho,n.obs=nrow(myData.firstHalf.withShortNames),fm="gls",nfactors=15) #-> first half: 4 factors
fa.parallel(myCormat.secondHalf$rho,n.obs=nrow(myData.secondHalf.withShortNames),fm="gls",nfactors=15) # -> second half: 4 factors
# -> we consistently find that we should extract 4 factors.

# Run factor analyses with 4 factors for full sample, first and second half
for (itype in 1:3){
  print(itype)
  if (itype==1) {
    myFaT=fa(myCormat$rho,nfactors=4,n.obs=nrow(myData.withShortNames),rotate="oblimin",fm="gls")
  } else if (itype==2){
    myFaT=fa(myCormat.firstHalf$rho,nfactors=4,n.obs=nrow(myData.firstHalf.withShortNames),rotate="oblimin",fm="gls")
  } else{
    myFaT=fa(myCormat.secondHalf$rho,nfactors=4,n.obs=nrow(myData.secondHalf.withShortNames),rotate="oblimin",fm="gls")
  }

  t1=data.frame(unclass(myFaT$loadings))
  ord=colnames(myData.withShortNames)#rev(colnames(myData))
  t3=data.table(t1,keep.rownames=TRUE)
  t3$rn=factor(t3$rn,levels=ord)
  t2B=melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings") # could have used 'gather' or 'melt' (from reshape2)
  if (itype==1){
    t2B$factors=recode_factor(t2B$factors,`GLS2`="Compulsivity",`GLS4`="Apathy",`GLS1`="DeprAnx",`GLS3`="SocialAnx")
  } else{
    t2B$factors=recode_factor(t2B$factors,`GLS2`="Compuls.",`GLS4`="Apathy",`GLS1`="DeprAnx",`GLS3`="SocialAnx")
  }
  # plot the results
  plot=ggplot(t2B,aes(rn,(loadings),fill=loadings)) +
    facet_wrap(~ factors, nrow=1) +
    geom_bar(stat="identity",aes(fill=abs(loadings)>0.4))+
    scale_fill_manual("legend",values=c("TRUE"="black","FALSE"="grey"))+
    coord_flip()+
    ylab("Loading strength") +
    theme_minimal(base_size=10)+xlab("")
  
  if (itype==1){
    print('type=1')
    plot.factors.fullDiscovSample=plot+
      theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+
      ggtitle(" ")
    print(plot+theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+ggtitle("Factor analysis for full discovery sample"))
  } else if(itype==2){
    print('type=2')
    plot.factors.firstHalfDiscovSample=plot+
      theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+ #,plot.title=element_text(hjust=0))+
      ggtitle("")
  }else{
    plot.factors.secondHalfDiscovSample=plot+
      theme(legend.position="none",axis.text.x=element_text(size=10),axis.title.y=element_blank(),axis.text.y=element_blank(),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+#,plot.title=element_text(hjust=0))+
      ggtitle("")
  }
}
# Save the factor scores to relate to the behaviour later 
myFaT=fa(myCormat$rho,nfactors=4,n.obs=nrow(myData.withShortNames),rotate="oblimin",fm="gls")
t2=factor.scores(myData.withShortNames,myFaT)
myScores=data.frame(unclass(t2$scores))
myFactors=myScores
myFactors=dplyr::rename(myFactors,
                 DeprAnx=GLS1,
                 Compulsivity=GLS2, 
                 SocialAnx=GLS3, 
                 Apathy=GLS4) 
myFactors=cbind(myFactors,myDataT$ID)
myFactors=dplyr::rename(myFactors,ID=`myDataT$ID`)
save(myFactors,file=file.path(savePath,"R_FactorAnalysis.R"))

# Plot correlation between the factors
myFactors=myFactors[,c(3,4,1,2)]
sjt.corr(myFactors,fade.ns=FALSE,digits=2,file=file.path(statsFolder,'FactorCorrelations.html'))



#### Put all plots into a single figure ####

p.empty=ggplot()+theme_void()
finalPlot=ggarrange(
    ggarrange(p$Compulsivity,p$Apathy,p$BDI,p$SocialAnx,nrow=4,ncol=1,labels=c("Ai Obsessive-compulsive","Aii Apathy","Aiii Depression","Aiv Social anxiety"),hjust=0),
    ggarrange(
      plot.corr,plot.factors.fullDiscovSample,nrow=2,ncol=1,labels=c("","C Factor loadings"),hjust=-0.4),
    ncol=2,nrow=1,widths=c(1,3),labels=c("","B Correlation matrix"),hjust=-0.4)
    
ggexport(finalPlot,filename=file.path(plotFolder,"Questionnaires.jpeg"),pointsize=10,
         width=(7.2*400),height=(8*400), res=400)              
# journal: 89mm (3.5inch), 120-136mm or 183mm (7.205 inch), length:247mm (9.7244), let's go 400dpi
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


# 2) Factor solution for all subscales
myCormatFull       =mixedCor(data=myData.withShortNames.AllSubscales,correct=0,d=NULL,p=NULL,smooth=TRUE,global=FALSE)
myFaT=fa(myCormatFull$rho,nfactors=6,n.obs=nrow(myData.withShortNames.AllSubscales),rotate="oblimin",fm="gls")
t1=data.frame(unclass(myFaT$loadings))
# dplyr::select items that have at least one loading > 0.4
ord=colnames(myData.withShortNames.AllSubscales)
t3=data.table(t1,keep.rownames=TRUE)
t3$rn=factor(t3$rn,levels=ord)
t2B=melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")
t2B$factors=recode_factor(t2B$factors,`GLS2`="Compulsivity",`GLS5`="Apathy",`GLS1`="DeprAnx",`GLS3`="SocialAnx",`GLS4`="AlexiImpulse",`GLS6`="Alcohol")

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




# 3) Comparing factor solution first and second half of discovery sample
factors.halfSplit=ggarrange(plot.factors.firstHalfDiscovSample,plot.factors.secondHalfDiscovSample,
              ncol=2,nrow=1,widths=c(1.45,1),font.label=list(size=12),
              labels=c("A First half of discovery sample","B Second half of discovery sample"),hjust=0)



ggexport(factors.halfSplit,filename=file.path(plotFolder,"Factors_HalfSplit.jpeg"),pointsize=10,
         width=(7.2*400),height=(4*400), res=400)              


