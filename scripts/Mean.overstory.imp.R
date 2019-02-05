#caluculates MEAN importance values for each species in overstory pre and post fire by plot

library(tidyverse)
library(vegan)
library(RColorBrewer)

############################################################
#import overstory data
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0

#combine snag/stump and dbh 
comp <- comp %>%
  rename(diam = dbh, stump = snag_stump..diam) %>%
  mutate(dbh = (diam + stump))

#import sumarized focal oak data 
qukes <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv")

#severity data
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")
#break into severity categories for each fire
rdnbr <- rdnbr %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) %>%
  select(-Plot)

#reclassify severity factors as names vs. numbers
rdnbr$Storrie <- factor(rdnbr$Storrie, labels = c("Unburned", "Low", "Moderate", "High"), ordered = is.ordered(rdnbr$Storrie))

rdnbr$Chips <- factor(rdnbr$Chips, labels = c("Unburned", "Low", "Moderate", "High"), ordered = is.ordered(rdnbr$Chips))

rdnbr <- rdnbr %>%
  mutate(combined = storrie_rdnbr+chips_rdnbr)
########################
#Pre-fire values

###### Density
#calculates number of trees living pre-Storrie 
pre.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#realtivize density metrics
pre.dens <- pre.density[,2:7] #remove plot column
pre.dens <- decostand(pre.dens, method="total")
#alphabetize columns
pre.dens <- pre.dens[,order(colnames(pre.dens))]

### frequency
pre.freq <- pre.density %>%
  gather(2:7, key = Species, value = freq)

#sort by plot #
pre.freq <- pre.freq[order(pre.freq$plot),]

#replace # with 1 or 0
pre.freq$freq <- if_else(pre.freq$freq > 0, 1, 0)

pre.freq <- pre.freq %>%
  spread(key = Species, value = freq)

pre.freq <- pre.freq[2:7]

###calculates total basal area for each species

pre.basal <- pre.basal %>%
  group_by(plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#transposes rows to columns
pre.basal <- spread(pre.basal, key = "Spp", value = "ba", fill = 0.0)

#removes plot and ? columns
pre.basal <- pre.basal %>%
  select(-ACMA, -PIMO, -QUCH)
pre.basal <- pre.basal[2:7]

#relativizes 
pre.basal <- decostand(pre.basal, method = "total")

###combine data frames and calculate importance value by species & plot
pre.import <- (pre.dens + pre.basal + pre.freq)*100

#re-attach plot #s to importance data frame
pre.import <- data.frame(rdnbr$plot, pre.import)

#re-orgainze data into long format
pre.import2 <- gather(pre.import, key = "Species", value = "Importance Value", ABCO:QUKE)
pre.import2 <-  pre.import2 %>%
  rename ("plot" = rdnbr.plot)
pre.import2$time <- as.factor("Pre-fire")

############## #POST-Storrie fire values
####caluculate relative importance values based on basal area, density and frequency

#calculates relative density of each species by plot
postS.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "SC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#removes severity column
postS.dens <- postS.density[,2:7]

#relativizes
postS.dens <- decostand(postS.dens, method = "total")

#### frequency 
postS.freq <- postS.density %>%
  gather(2:7, key = Species, value = freq)

#sort by plot #
postS.freq <- postS.freq[order(postS.freq$plot),]

#replace # with 1 or 0
postS.freq$freq <- if_else(postS.freq$freq > 0, 1, 0)

postS.freq <- postS.freq %>%
  spread(key = Species, value = freq)

postS.freq <- postS.freq[2:7]

###
#calculates total basal area for each species by storrie and chips severity
postS.basal <- comp %>%
  group_by(plot,Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "KSQ", "SC")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#transposes rows to columns
postS.basal <- spread(postS.basal, key = "Spp", value = "ba", fill = 0.0)

#Adds in basal diameters of Storrie sprouts from focal oak data - multiplied by num of clumps in each plot
qukes <- qukes %>%
  mutate(Sto.ba = ((diam.storrie.live+diam.storrie.dead)^2*.0017*clumps))

QUKE2 <- qukes$Sto.ba
  
#ADDS basal area of sprouts using extrapolated focal oak data
postS.basal$QUKE= (QUKE2+postS.basal$QUKE)

#replaces basal area in overstory with values from focal oak data if overstory basal area =0 
#postS.basal$QUKE <- ifelse(postS.basal$QUKE==0, qukes$Sto.ba, postS.basal$QUKE)

#filters out uncommon % unk spp's
postS.basal <- postS.basal %>%
  select(-ACMA, -PIMO, -QUCH)
postS.basal <- postS.basal[,2:7]

#relativizes 
postS.basal <- decostand(postS.basal, method = "total")

#adds post-Storrie density and basal area values to get importance vals
postS.import <- (postS.dens+postS.basal+postS.freq)*100

#re-orgainze data into long format
postS.import2 <- data.frame(rdnbr$plot, postS.import)
postS.import2 <- postS.import2 %>%
  gather(2:7, key = "Species", value = "Importance Value")
postS.import2 <-  postS.import2 %>%
  rename ("plot" = rdnbr.plot)
postS.import2$time <- as.factor("Post-Storrie Fire")

############Post CHIPS fire values

#calculates relative density of each species by plot
postC.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "SSKC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#removes severity column
postC.dens <- postC.density[,2:7]

#relativizes
postC.dens <- decostand(postC.dens, method = "total")

#### frequency
postC.freq <- postC.density %>%
  gather(2:7, key = Species, value = freq)

#sort by plot #
postC.freq <- postC.freq[order(postC.freq$plot),]

#replace # with 1 or 0
postC.freq$freq <- if_else(postC.freq$freq > 0, 1, 0)

postC.freq <- postC.freq %>%
  spread(key = Species, value = freq)

postC.freq <- postC.freq[2:7]

####

#calculates total basal area for each species by storrie and chips severity
postC.basal <- comp %>%
  group_by(plot, Spp) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "KSQ", "KCQ", "SSKCQ", "SSKC")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#transposes rows to columns
postC.basal <- spread(postC.basal, key = "Spp", value = "ba", fill = 0.0)

#Adds in basal diameters of Storrie sprouts from focal oak data - multiplied by num of clumps in each plot
qukes2 <- qukes %>%
  mutate(Chips.ba = ((diam.storrie.live+diam.chips)^2*.0017*clumps))

QUKE2c <- qukes2$Chips.ba

#ADDS basal area of sprouts using extrapolated focal oak data
postC.basal$QUKE= (QUKE2c+postC.basal$QUKE)

#replaces basal area in overstory with values from focal oak data if overstory basal area =0 
#postC.basal$QUKE <- ifelse(postC.basal$QUKE==0, qukes$Chips.ba, postC.basal$QUKE)

#filters out uncommon % unk spp's
postC.basal <- postC.basal %>%
  select(-ACMA, -CONU, -PIMO, -QUCH)
postC.basal <- postC.basal[,2:7]

#relativizes 
postC.basal <- decostand(postC.basal, method = "total")

#adds post-Storrie density and basal area values to get importance vals
postC.import <- (postC.dens+postC.basal+postC.freq)*100

#re-orgainze data into long format
postC.import2 <- data.frame(rdnbr$plot, postC.import)
postC.import2 <- postC.import2 %>%
  gather(2:7, key = "Species", value = "Importance Value")
postC.import2 <-  postC.import2 %>%
  rename ("plot" = rdnbr.plot)
postC.import2$time <- as.factor("Post-Chips Fire")
############################################
#save wide file of mean vals for stats
#prep
all.import <- data.frame(pre.import, postS.import, postC.import)

#reorder columns
#all.import <- all.import[c(7,8,9,10,11,1:6,12:23)]

write.csv(all.import, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv", row.names = F)
###
###save long file of mean values for ggplot, etc.  ############
#prep 
all.import2 <- rbind(pre.import2, postS.import2, postC.import2)

write.csv(all.import2, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.long.csv", row.names = F)
##################################
#plots with continuous (rdnbr) predictor variables
par(mfrow=c(2,2))
par(mar=c(4,4.1,2.5,1.6))
#sets the bottom, left, top and right margins

plot(pre.import$ABCO~pre.import$storrie_rdnbr, main="ABCO", xlab="", ylim=c(0,200), ylab="Importance Values", pch=4, col = "blue")
points(postS.import$ABCO~pre.import$storrie_rdnbr, pch=19, col = "red")
points(postC.import$ABCO~pre.import$chips_rdnbr, pch=22, bg="grey")

plot(pre.import$PSME~pre.import$storrie_rdnbr, main="PSME", ylab="", pch=4, xlab="", ylim=c(0,200))
points(postS.import$PSME~pre.import$storrie_rdnbr, pch=19)
points(postC.import$PSME~pre.import$chips_rdnbr, pch=22, bg="grey")
legend(1250, 130, legend = c("prefire", "post-Storrie", "post-Chips"), y.intersp=.5, xjust = 1, yjust=0, pch = c(4, 19, 22), pt.bg = "grey", cex=1.2, text.width = 220)

plot(pre.import$PIPO~pre.import$storrie_rdnbr, main="PIPO", ylab="Importance Values", pch=4, xlab="RdNBR", ylim=c(0,200))
points(postS.import$PIPO~pre.import$storrie_rdnbr, pch=19)
points(postC.import$PIPO~pre.import$chips_rdnbr, pch=22, bg="grey")

plot(pre.import$QUKE~pre.import$storrie_rdnbr, main="QUKE", ylab="", pch=4, xlab="RdNBR")
points(postS.import$QUKE~pre.import$storrie_rdnbr, pch=19)
points(postC.import$QUKE~pre.import$chips_rdnbr, pch=22, bg="grey")

#################
#plots with continuous predictors (COMBINED rdnbr)
#sort data frame by combined RdNBR
comb <- rdnbr$combined
comb <- sort(comb)

plot(pre.import$ABCO~comb, main="ABCO", xlab="", ylim=c(0,200), ylab="Importance Values", pch=24, bg="green", xlim=c(0,2000))
lines(predict(lm(pre.import$ABCO~comb)))
points(postS.import$ABCO~rdnbr$comb, pch=19)
points(postC.import$ABCO~comb, pch=22, bg="blue")

plot(pre.import$PSME~rdnbr$combined, main="PSME", ylab="", pch=4, xlab="", ylim=c(0,200), xlim=c(0,2000))
points(postS.import$PSME~rdnbr$combined, pch=19)
points(postC.import$PSME~rdnbr$combined, pch=22, bg="grey")
legend(1560, 130, legend = c("prefire", "post-Storrie", "post-Chips"), y.intersp=.5, xjust = 0, yjust=0, pch = c(4, 19, 22), pt.bg = "grey", cex=1.2, text.width = 220)

plot(pre.import$PIPO~rdnbr$combined, main="PIPO", ylab="Importance Values", pch=4, xlab="Combined RdNBR", ylim=c(0,200), xlim=c(0,2000))
points(postS.import$PIPO~rdnbr$combined, pch=19)
points(postC.import$PIPO~pre.import$storrie_rdnbr, pch=22, bg="grey")

plot(pre.import$QUKE~rdnbr$combined, main="QUKE", ylab="", pch=4, xlab="Combined RdNBR", xlim=c(0,2000))
points(postS.import$QUKE~rdnbr$combined, pch=19)
points(postC.import$QUKE~rdnbr$combined, pch=22, bg="grey")

###Set up for anovas?
###############################################################
###compare IV's pre to post-Storrie Fire

#create dataframe with ABCO values###########
abco <- data.frame(pre.import$Storrie, pre.import$ABCO, postS.import$ABCO)
abco <-  abco %>% 
  rename(Storrie = pre.import.Storrie, prefire = pre.import.ABCO, postS = postS.import.ABCO)

require(reshape2)
abco <- melt(abco)

abco <- abco %>%
  rename(time = variable, IV = value)

#box plot comparing Iv's pre and post storrie by severity class
par(mfrow=c(1,1))
boxplot(abco$IV~abco$time+abco$Storrie, main = "ABCO IV's: Pre & post Storrie Fire", ylab ="Mean Importance Value", col = c("blue", "orange"))

#df of post-storrie values only
abco.postS <- filter(abco, time == "postS")

#anova comparing change in mean IV by Storrie severity class
aps <- aov(abco.postS$IV~abco.postS$Storrie)
summary(aps)
TukeyHSD(aps)

#plot of same
par(mar=c(4,4.1,2.5,1.6))
boxplot(abco.postS$IV~abco.postS$Storrie, main = "ABCO Post Storrie IV's", ylab ="Mean Importance Value", col = c("blue", "green", "yellow1", "red"))

###############
#create dataframe with QUKE values###################
quke <- data.frame(pre.import$Storrie, pre.import$QUKE, postS.import$QUKE)
quke <-  quke %>% 
  rename(Storrie = pre.import.Storrie, prefire = pre.import.QUKE, postS = postS.import.QUKE)

require(reshape2)
quke <- melt(quke)

quke <- quke %>%
  rename(time = variable, IV = value)

#box plot comparing Iv's pre and post storrie by severity class
par(mfrow=c(1,1))
boxplot(quke$IV~quke$time+quke$Storrie, main = "QUKE IV's: Pre & post Storrie Fire", ylab ="Mean Importance Value", col = c("blue", "orange"))

quke.posts <- filter(quke, quke$time=="postS")

qps <- aov(quke.posts$IV~quke.posts$Storrie)
summary(qps)
TukeyHSD(qps)

#boxplot of same
boxplot(quke.posts$IV~quke.posts$Storrie, main = "QUKE Post-Storrie IV's", col = c("blue", "green", "yellow", "red"), ylab ="Mean Importance Value", pch=19)

###compare IV's pre to post-CHIPS Fire#######################################

#create dataframe with ABCO values, severity combo's###########
abco2 <- data.frame(postS.import$Storrie, postS.import$Chips, postS.import$ABCO, postC.import$ABCO)
#rename severity classes
abco2$Storrie <- factor(rdnbr$Storrie, labels = c("Un", "Low", "Mod", "High"), ordered = is.ordered(abco2$Storrie))
abco2$Chips <- factor(rdnbr$Chips, labels = c("Un", "Low", "Mod", "High"), ordered = is.ordered(abco2$Chips))

#remove old severity classes
abco2 <- abco2[,3:6]

abco2 <- abco2 %>%
  rename(preC = postS.import.ABCO, postC = postC.import.ABCO)%>%
  unite(combined, Storrie, Chips, remove = T)

require(reshape2)
abco2 <- melt(abco2)

abco2 <- abco2 %>%
  rename(time = variable, IV = value)

abco2$combined <- factor(abco2$combined, levels = c("Un_Un", "Un_Low", "Un_Mod", "Un_High", "Low_Un", "Low_Low", "Low_Mod", "Low_High", "Mod_Un", "Mod_Low", "Mod_Mod", "Mod_High", "High_Un", "High_Low", "High_Mod", "High_High"), ordered = is.ordered(abco2$combined))

#box plot comparing Iv's pre and post storrie by severity class
par(mfrow=c(1,1))
par(mar=c(8,4.1,2.5,1.6))
boxplot(abco2$IV~abco2$time+abco2$combined, main = "ABCO IV's: Pre & post Chips Fire", ylab ="Mean Importance Value", col = c("orange", "red"), ylim = c(0,200), las=2)

#anova comparing change in mean IV by Chips severity class
apc <- aov(abco2$IV~abco2$combined)
summary(apc)
TukeyHSD(apc)

####Post-Chips QUKE
#create dataframe with QUKE values##########
quke2 <- data.frame(postS.import$Storrie, postS.import$Chips, postS.import$QUKE, postC.import$QUKE)
#rename severity classes
quke2$Storrie <- factor(rdnbr$Storrie, labels = c("Un", "Low", "Mod", "High"), ordered = is.ordered(quke2$Storrie))
quke2$Chips <- factor(rdnbr$Chips, labels = c("Un", "Low", "Mod", "High"), ordered = is.ordered(quke2$Chips))

#remove old severity classes
quke2 <- quke2[,3:6]

quke2 <- quke2 %>%
  rename(preC = postS.import.QUKE, postC = postC.import.QUKE)%>%
  unite(combined, Storrie, Chips, remove = T)

require(reshape2)
quke2 <- melt(quke2)

quke2 <- quke2 %>%
  rename(time = variable, IV = value)

quke2$combined <- factor(quke2$combined, levels = c("Un_Un", "Un_Low", "Un_Mod", "Un_High", "Low_Un", "Low_Low", "Low_Mod", "Low_High", "Mod_Un", "Mod_Low", "Mod_Mod", "Mod_High", "High_Un", "High_Low", "High_Mod", "High_High"), ordered = is.ordered(quke2$combined))

#box plot comparing Iv's pre and post storrie by severity class
par(mfrow=c(1,1))
boxplot(quke2$IV~quke2$time+quke2$combined, main = "QUKE IV's: Pre & post Chips Fire", ylab ="Mean Importance Value", col = c("orange", "red"), las=2)

qpc <- aov(quke2$IV~quke2$combined)
summary(qpc)
TukeyHSD(qpc)

#boxplot of same
boxplot(quke.postc$IV~quke.postc$Chips, main = "QUKE Post-Chips IV's", col = c("blue", "green", "yellow", "red"), ylab ="Mean Importance Value", pch=19)

##############################
#Plotting
##ABCO####
#par(mfrow=c(1,2))
m <- rbind(c(1,1,2))
layout(m)
par(mar=c(4,4.1,2.5,1.6))
#box plot comparing ABCO Iv's pre and post storrie by severity class
boxplot(abco$IV~abco$time+abco$Storrie, main = "ABCO IV's: Pre & post Storrie Fire", ylab ="Mean Importance Value", col = c("blue", "orange"), ylim=c(0,200))

#plot ABCO post-Storrie response by severity
boxplot(abco.postS$IV~abco.postS$Storrie, main = "ABCO Post Storrie IV's", ylab ="Mean Importance Value", col = c("blue", "green", "yellow1", "red"), ylim=c(0,200))

#comparing ABCO Iv's pre and post Chips by severity class
boxplot(abco2$IV~abco2$time+abco2$Chips, main = "ABCO IV's: Pre & post Chips Fire", ylab ="Mean Importance Value", col = c("orange", "red"), ylim = c(0,200))
#comparing change in mean IV post-Chips by severity class
boxplot(abco.postC$IV~abco.postC$Chips, main = "ABCO Post Chips IV's", ylab ="Mean Importance Value", col = c("blue", "green", "yellow1", "red"), ylim = c(0,200))

###QUKE####
m <- rbind(c(1,1,2))
layout(m)
par(mar=c(4,4.1,2.5,1.6))
boxplot(quke$IV~quke$time+quke$Storrie, main = "QUKE IV's: Pre & post Storrie Fire", ylab ="Mean Importance Value", col = c("blue", "orange"))
boxplot(quke.posts$IV~quke.posts$Storrie, main = "QUKE Post-Storrie IV's", col = c("blue", "green", "yellow", "red"), ylab ="Mean Importance Value", pch=19)

layout(1,1)
