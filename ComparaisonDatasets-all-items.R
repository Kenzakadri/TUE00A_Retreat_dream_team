rm(list=ls()) #Clear the work space
dev.off() # clear all plots

#### Settings and loading the data ####

# dataFolder="~/Documents/GitHub/TUE00A_Retreat_dream_team/datasets"
#Load packages
library(ggplot2)

library(data.table)
library(tidyverse)
library(corrplot)

library(BayesFactor)
library(vegan)
library(psych)

Gillan <-read.csv('datasets/Gillanetal2016.csv')
Scholl <-read.csv('datasets/Scholletalraw.csv')

SchollProcessed <- read.csv('datasets/Scholletal2022.csv')
SchollProcessed[c("schizotypy_UnusualExperiences"                            , "schizotypy_UnusualExperiences")]

SCZ_G <- Gillan %>% select(SCZ_24:SCZ_33) %>% rowSums(na.rm = TRUE)
SCZ_G <- SCZ_G + Gillan %>% select(SCZ_1:SCZ_12) %>% rowSums(na.rm = TRUE)
OCI_G <- Gillan %>% select(OCI_1:OCI_18) %>% rowSums(na.rm = TRUE)
LSAS_G <- Gillan %>% select(LSAS_1:LSAS_24) %>% rowSums(na.rm = TRUE)

SCZ_S <- SchollProcessed %>%
    select(schizotypy_UnusualExperiences, schizotypy_UnusualExperiences) %>% rowSums(na.rm = TRUE)

OCI_S <- Scholl %>% select(OCI.Q1:OCI.Q18) %>% rowSums(na.rm = TRUE)
LSAS_S <- Scholl %>% select(SocialAnx.Q1:SocialAnx.Q24) %>% rowSums(na.rm = TRUE)

# check oci

ttestBF(x = OCI_G, y = OCI_S)
ttestBF(x = LSAS_G, y = LSAS_S)
ttestBF(x = SCZ_G, y = SCZ_S)


oci_plot <- rbind(data.table(source = "gillan", value = OCI_G),
      data.table(source = "scholl", value = OCI_S))

ggplot(oci_plot, aes(x = value, fill = source)) + geom_density(alpha = 0.5)

ggsave('datasets/oci_plot.png')

lsas_plot <- rbind(data.table(source = "gillan", value = LSAS_G),
      data.table(source = "scholl", value = LSAS_S))

ggplot(lsas_plot, aes(x = value, fill = source)) + geom_density(alpha = 0.5)

ggsave('datasets/lsas_plot.png')

schizo <- rbind(data.table(source = "gillan", value = SCZ_G),
      data.table(source = "scholl", value = SCZ_S))

ggplot(schizo, aes(x = value, fill = source)) + geom_density(alpha = 0.5)

ggsave('datasets/schizo_plot.png')

SCZ_G <- Gillan %>% select(SCZ_24:SCZ_33)
SCZ_G <- cbind(SCZ_G, Gillan %>% select(SCZ_1:SCZ_12))
OCI_G <- Gillan %>% select(OCI_1:OCI_18) 
LSAS_G <- Gillan %>% select(LSAS_1:LSAS_24)



SCZ_S <- Scholl %>% select(Schizo.Q1:Schizo.Q33)
OCI_S <- Scholl %>% select(OCI.Q1:OCI.Q18)
LSAS_S <- Scholl %>% select(SocialAnx.Q1:SocialAnx.Q24)



gillan_fa <- fa.parallel(mixedCor(cbind(SCZ_G, OCI_G, LSAS_G), method='spearman', d=1:22, c=23:64)$rho, n.iter = 20, n.obs = nrow(Gillan))

scholl_fa <- fa.parallel(mixedCor(na.omit(cbind(SCZ_S, OCI_S, LSAS_S)), method='spearman', d=1:22, c=23:64)$rho)


########################################################################

plot(gillan_fa)
plot(scholl_fa)

AES_G <- Gillan %>% select(AES_1:AES_18)
AES_S <- Scholl %>% select(AMI.Q1:AMI.Q18)

SDS_G <- Gillan %>% select(SDS_1:SDS_20)
SDS_S <- Scholl %>% select(BDI.Q1:BDI.Q21)

STA_G <- Gillan %>% select(STAI_1:STAI_20)
STA_S <- Scholl %>% select(traitAnx.Q1:traitAnx.Q20)



gillan_fa_full <- fa.parallel(
    mixedCor(cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G), method = 'spearman',
             d = 1:22, c = 23:122)$rho,
    n.iter = 20, n.obs = nrow(Gillan)
    )

scholl_fa_full <- fa.parallel(
    mixedCor(na.omit(cbind(SCZ_S, OCI_S, LSAS_S, AES_S, SDS_S, STA_S)),
             method = 'spearman', d = 1:22, c = 23:123)$rho,
    n.iter = 20, n.obs = nrow(Scholl)
    )

gillan_plotting_fa <- cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G)

myCormatFull <- mixedCor(gillan_plotting_fa,
                        method = 'spearman',
                        d = 1:22, c = 23:122)
myFaT <- fa(myCormatFull$rho,nfactors=16,n.obs=nrow(Gillan),rotate="oblimin",fm="gls")
t1 <- data.frame(unclass(myFaT$loadings))
# dplyr::select items that have at least one loading > 0.4
ord <- colnames(gillan_plotting_fa)
t3 <- data.table(t1, keep.rownames=TRUE)
t3$rn <- factor(t3$rn,levels=ord)
t2B <- melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")


# replace the labels
plot.sixFactors <- ggplot(t2B,aes(rn,(loadings),fill=loadings)) +
  facet_wrap(~ factors, nrow=1) +geom_bar(stat="identity",aes(fill=abs(loadings)>0.4))+
  scale_fill_manual("legend",values=c("TRUE"="black","FALSE"="grey"))+coord_flip()+ylab("Loading strength")+xlab("") +theme_bw(base_size=10)+
  theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+
  ggtitle(" ")

ggsave(plot.sixFactors, filename = "datasets/gillan_factors.png")


myCormatFull <- mixedCor(scholl_plotting_fa,
                        method = 'spearman',
                        d = 1:22, c = 23:122)
myFaT <- fa(myCormatFull$rho,nfactors=16,n.obs=nrow(Gillan),rotate="oblimin",fm="gls")
t1 <- data.frame(unclass(myFaT$loadings))
# dplyr::select items that have at least one loading > 0.4
ord <- colnames(gillan_plotting_fa)
t3 <- data.table(t1, keep.rownames=TRUE)
t3$rn <- factor(t3$rn,levels=ord)
t2B <- melt.data.table(t3,id.vars="rn",variable.name="factors",value.name="loadings")


# replace the labels
plot.sixFactors <- ggplot(t2B,aes(rn,(loadings),fill=loadings)) +
  facet_wrap(~ factors, nrow=1) +geom_bar(stat="identity",aes(fill=abs(loadings)>0.4))+
  scale_fill_manual("legend",values=c("TRUE"="black","FALSE"="grey"))+coord_flip()+ylab("Loading strength")+xlab("") +theme_bw(base_size=10)+
  theme(legend.position="none",axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),text = element_text(size=10),strip.text = element_text(size = 10,face="bold"))+
  ggtitle(" ")

ggsave(plot.sixFactors, filename = "datasets/gillan_factors.png")

#############################################################

complete_gillan <- cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G)

data_points <- seq(400, 1413, by = 20)


bigout <- data.frame()
for (i in data_points) {
    print(paste(which(data_points == i), '/', length(data_points), sep = ""))
    current <- complete_gillan[sample(1:nrow(complete_gillan), i), ]
    rownames(current) <- 1:nrow(current)
    output <- fa.parallel(
        mixedCor(na.omit(current), method = 'spearman', d = 1:22, c = 23:122, global = FALSE, use = 'complete.obs')$rho,
        n.iter = 20, n.obs = i
        )
    print(output$nfact)
    bigout <- rbind(bigout, cbind(i, output$nfact, output$ncomp))
}

write.csv(bigout, 'datasets/FA_nfact.csv')

bigout <- read.csv('datasets/FA_nfact.csv')

ggplot(bigout, aes(x = i, y = V2)) + geom_point(size=3) + theme_bw()

ggsave('FA_nfact.png')

colnames(Gillan)

