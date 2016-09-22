 ## Sequence analysis for Anonymous work

install.packages("TraMineR")
install.packages("cluster")
install.packages("reshape2")
install.packages("TraMineRextras")
install.packages("reshape")

library(reshape2)
library(cluster)
library(TraMineR)
library(TraMineRextras)
library(reshape)

# Subset session with anonymous work proportion greater than 0 and less than 100
# Opened .csv file and added number from session 
# Session_Classification=IF(AND(A3=A2,C3=C2),F2+1,IF(AND(A3=A2,C3>C2),1,1))
# Unique_Session =IF(AND(A3=A2,C3=C2),G2,IF(AND(A3=A2,C3>C2),G2+1,IF(A3<>A2,G2+1,"NA")))

# Setwd and import file generated in Anon_Work.R 
setwd("~/Desktop/Sequence/SmallSessions")
Annotation_History <- read.csv("MixAsteroidAnnotations.csv")

# New Variables for Anonymous and Registered
attach(Annotation_History)
Annotation_History$AnnotationType[is.na(Annotation_History$user_name)] <- "AU"
Annotation_History$AnnotationType[!is.na(Annotation_History$user_name)] <- "RU"
detach(Annotation_History)
# Subset to only examine 100 classifications in a session
Annotation_History <- rename(Annotation_History, c("AnnotationType" = "value"))


######## FULL POPULATION
########################
Annotation_History_FULL <- Annotation_History[c(6:8)]
Annotation_History_FULL$Unique_Session <- as.numeric(Annotation_History_FULL$Unique_Session)
Annotation_History_FULL$Session_Classification <- as.numeric(Annotation_History_FULL$Session_Classification)
# Cast annotations
Annotation_History_FULL_Cast <- dcast(Annotation_History_FULL, Unique_Session~Session_Classification)
Annotation_History_FULL <- as.data.frame(Annotation_History_FULL_Cast)
# Change NA to END using find and replace
Annotation_History_FULL[is.na(Annotation_History_FULL)] <- "END"
remove(Annotation_History_FULL_Cast)


######## Subset 100 Anno
########################
Annotation_History_100Sample <- Annotation_History[which(Annotation_History$Session_Classification < 100),]
Annotation_History_100Sample <- Annotation_History_100Sample[c(6:8)]
Annotation_History_100Sample$Unique_Session <- as.numeric(Annotation_History_100Sample$Unique_Session)
Annotation_History_100Sample$Session_Classification <- as.numeric(Annotation_History_100Sample$Session_Classification)
# Cast annotations
Annotation_History_100Sample_Cast <- dcast(Annotation_History_100Sample, Unique_Session~Session_Classification)
Annotation_History_100Sample_Cast_Plot <- Annotation_History_100Sample_Cast[c(2:100)]

# Export data set and change NA to END using find and replace
Annotation_History_100Sample[is.na(Annotation_History_100Sample)] <- "END"
remove(Annotation_History_100Sample_Cast)


####### Build Sequence Plots for 100 annotation SAMPLE #############
#(http://traminer.unige.ch/preview-main.shtml)
# http://rpackages.ianhowson.com/rforge/TraMineR/
annon.alphabet <- c("AU","RU","END")
annon.labels <- c("Anonymous User","Registered User","Session End")
annon.scodes <- c("AU","RU","END")
anno.seq <- seqdef(Annotation_History_FULL_Cast, 1:99, alphabet = annon.alphabet, states = annon.scodes, 
    labels = annon.labels, xtstep = 3)

# Plotting sequence data
# par(mfrow = c(2, 2))

#Export 10 most populat sequences
setwd("~/Dropbox/ZooSOCS dropbox/Papers/CSCW 2017 (AnonWork)/Anon-LaTex/figs")
pdf("asteroid_sample_sequences1.pdf", height=5, width=10) # Need to change plot name
seqiplot(anno.seq, withlegend = FALSE, border = NA)
dev.off()

pdf("asteroid_sample_sequences2.pdf", height=5, width=10) # Need to change plot name
seqIplot(anno.seq, sortv = "from.start", withlegend = FALSE)
dev.off()

anno.om <- seqdist(anno.seq, method = "OM", indel = 1, sm = "TRATE")

# Cluster for Sample
library(cluster)
clusterward <- agnes(anno.om, diss = TRUE, method = "ward")
plot(clusterward, which.plot = 2)
cl.4 <- cutree(clusterward, k = 4)
cl.4fac <- factor(cl.4, labels = paste("Type", 1:4))

# Graph for clusters
pdf("asteroid_sample_sequences3.pdf", height=5, width=10) # Need to change plot name
seqIplot(anno.seq, group = cl.4fac, sortv = "from.start")
dev.off()

#################################################################
############# Build Sequence Plots for FULL DATASET #############
#################################################################
annon.alphabet <- c("AU","RU","END")
annon.labels <- c("Anonymous User","Registered User","Session End")
annon.scodes <- c("AU","RU","END")
anno.seq.full <- seqdef(Annotation_History_FULL, 2:552, alphabet = annon.alphabet, states = annon.scodes, 
    labels = annon.labels, xtstep = 3)

#Graphs
pdf("asteroid_all_sequences1.pdf", height=5, width=10) # Need to change plot name
seqiplot(anno.seq.full, withlegend = FALSE, border = NA)
dev.off()

pdf("asteroid_all_sequences2.pdf", height=5, width=10) # Need to change plot name
seqIplot(anno.seq.full, sortv = "from.start", withlegend = FALSE)
dev.off()

# Cluster for FULL
annofull.om <- seqdist(anno.seq.full, method = "OM", indel = 1, sm = "TRATE")
library(cluster)
clusterward1 <- agnes(annofull.om, diss = TRUE, method = "ward")
plot(clusterward1, which.plot = 4)
cl1.6 <- cutree(clusterward1, k = 4)
cl1.6fac <- factor(cl1.6, labels = paste("Type", 1:4))

# Graph for clusters
pdf("asteroid_all_sequences3.pdf", height=5, width=10) # Need to change plot name
seqIplot(anno.seq.full, group = cl1.6fac, sortv = "from.start")
dev.off()


# print(anno.seq.full[1:5, ], format = "SPS")
seq.full <- print(anno.seq.full, format = "SPS")

setwd("/Users/coreyjackson/Desktop/Sequence/Asteroid Sequence/Data")
write.csv(seq.full,"Asteroid_RawSequences.csv")

# In file above get sequence with number next to sequence. First replace - with [space]. Import file with space as delimiter

#Anon_Plot_Higgs <- read.csv("HiggsAnon_Plot.csv")

seq.full.freq <- seqtab(anno.seq.full, tlim = 1:4)


How many accounts