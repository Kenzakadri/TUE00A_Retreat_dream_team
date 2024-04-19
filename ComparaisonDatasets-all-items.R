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

lsas_plot <- rbind(data.table(source = "gillan", value = LSAS_G),
      data.table(source = "scholl", value = LSAS_S))

ggplot(lsas_plot, aes(x = value, fill = source)) + geom_density(alpha = 0.5)

schizo <- rbind(data.table(source = "gillan", value = SCZ_G),
      data.table(source = "scholl", value = SCZ_S))

ggplot(schizo, aes(x = value, fill = source)) + geom_density(alpha = 0.5)

SCZ_G <- Gillan %>% select(SCZ_24:SCZ_33)
SCZ_G <- cbind(SCZ_G, Gillan %>% select(SCZ_1:SCZ_12))
OCI_G <- Gillan %>% select(OCI_1:OCI_18) 
LSAS_G <- Gillan %>% select(LSAS_1:LSAS_24)



SCZ_S <- Scholl %>% select(Schizo.Q1:Schizo.Q33)
OCI_S <- Scholl %>% select(OCI.Q1:OCI.Q18)
LSAS_S <- Scholl %>% select(SocialAnx.Q1:SocialAnx.Q24)



gillan_fa <- fa.parallel(cbind(OCI_G, SCZ_G, LSAS_G))
scholl_fa <- fa.parallel(na.omit(cbind(OCI_S, SCZ_S, LSAS_S)))

plot(gillan_fa)
plot(scholl_fa)

AES_G <- Gillan %>% select(AES_1:AES_18)
AES_S <- Scholl %>% select(AMI.Q1:AMI.Q18)

SDS_G <- Gillan %>% select(SDS_1:SDS_20)
SDS_S <- Scholl %>% select(BDI.Q1:BDI.Q21)

STA_G <- Gillan %>% select(STAI_1:STAI_20)
STA_S <- Scholl %>% select(traitAnx.Q1:traitAnx.Q20)


mixedCor(cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G), method = 'spearman', d = 1:22)

gillan_fa_full <- fa.parallel(
    mixedCor(cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G), method = 'spearman',
             d = 1:22, p = 23:122)$rho,
    n.iter = 20, n.obs = nrow(Gillan)
    )

scholl_fa_full <- fa.parallel(
    mixedCor(na.omit(cbind(SCZ_S, OCI_S, LSAS_S, AES_S, SDS_S, STA_S)),
             method = 'spearman', d = 1:22, p = 23:123)$rho,
    n.iter = 20, n.obs = nrow(Scholl)
    )

fa.diagram(
    mixedCor(cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G),
             method = 'spearman', d = 1:22, d = 1:22, p = 23:123)$rho
             )

fa.diagram(mixedCor(na.omit(cbind(SCZ_S, OCI_S, LSAS_S, AES_S, SDS_S, STA_S)),
           method = 'spearman', d = 1:22, p = 23:122)$rho
           )


mixedCor(complete_gillan[1:400,], method = 'spearman',
             d = 1:22, p = 23:122)$rho

complete_gillan <- cbind(SCZ_G, OCI_G, LSAS_G, AES_G, SDS_G, STA_G)

data_points <- seq(400, 1413, by = 20)


bigout <- data.frame()
for (i in data_points) {
    print(paste(which(data_points == i), '/', length(data_points), sep = ""))
    rownames(current) <- 1:nrow(current)
    output <- fa.parallel(
        mixedCor(na.omit(current), method = 'spearman', d = 1:22, p = 23:122, global = FALSE, use = 'na.or.complete.obs')$rho,
        n.iter = 20, n.obs = i
        )
    print(output$nfact)
    bigout <- rbind(bigout, cbind(i, output$nfact, output$ncomp))
}
