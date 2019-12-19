#caluculates importance values for each species in overstory pre and post fire

library(tidyverse)
library(vegan)
library(RColorBrewer)

############################################################
#import raw overstory data and rdnbr values
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0
#comp <- read.csv("D:/for R/overstory.raw.csv")
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")
#imports summarized QUKE clump (FO) data 
qukes <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv")


#combine snag/stump diams with dbh for pre-fire dbh's
comp <- comp %>%
  rename(diam = dbh, stump = snag_stump..diam) %>%
  mutate(dbh = (diam + stump))


############## #Pre-fire values
####re-do by grouping plots into severity categories first, and then caluculating relative importance values based on summed basal area and density

########################

#calculates relative density of each species for each storrie severity category
pre.density2 <- comp %>%
  group_by(Storrie.Severity) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#removes severity column
pre.density2 <- pre.density2[,2:7]

#relativizes
pre.density2 <- decostand(pre.density2, method = "total")

####
#calculates total basal area for each species by storrie severity
pre.basal2 <- comp %>%
  group_by(Storrie.Severity, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#transposes rows to columns
pre.basal2 <- spread(pre.basal2, key = "Spp", value = "ba", fill = 0.0)

#filters out uncommon % unk spp's
pre.basal2 <- pre.basal2 %>%
  select(-ACMA, -PIMO, -QUCH)
pre.basal2 <- pre.basal2[,2:7]

#relativizes 
pre.basal2 <- decostand(pre.basal2, method = "total")

#caluculate frequency for each species in each severity category
pre.freq <- comp %>%
  group_by(Storrie.Severity, plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(sum.dist = sum(dist))

pre.freq <- spread(pre.freq, key = "Spp", value = "sum.dist", fill = 0.0)

pre.freq <- pre.freq %>%
  group_by(Storrie.Severity) %>% 
  summarize(ABCO=length(which(ABCO!=0))/n(), CADE=length(which(CADE!=0))/n(), PILA=length(which(PILA!=0))/n(), PIPO=length(which(PIPO!=0))/n(), PSME=length(which(PSME!=0))/n(), QUKE=length(which(QUKE!=0))/n())

pre.freq <- pre.freq[,2:7]

pre.import <- (pre.density2+pre.basal2+pre.freq)*100

pre.import <- t(pre.import)

colnames(pre.import) <- c("unburned", "low", "moderate","high")

#mean IV's for all plots
pre.import.mean <- as.data.frame(rowMeans(pre.import))

#plot of mean importance values for all plots pre-fire
barplot(pre.import.mean$`rowMeans(pre.import)`, names.arg = rownames(pre.import.mean), beside = T, col = colors, ylab = "Importance Value", ylim = c(0,200), main = "Mean pre-fire values - overstory only")
legend(5, 200, legend = rownames(pre.import.mean), fill = colors, horiz = F)

############## #POST-Storrie fire values
####group plots into severity categories first, and then caluculate relative importance values based on summed basal area and density

#calculates relative density of each species for each storrie category
postS.dens <- comp %>%
  group_by(Storrie.Severity) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "SC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")) )

#removes severity column
postS.dens <- postS.dens[,2:7]

#relativizes
postS.dens <- decostand(postS.dens, method = "total")

#calculates total basal area for each species by storrie and chips severity
postS.basal <- comp %>%
  group_by(Storrie.Severity, plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "KSQ", "SC")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#Adds in basal diameters of Storrie sprouts from focal oak data - multiplied by num of clumps in each plot
qukes <- qukes %>%
  mutate(Sto.ba = ((diam.storrie.live+diam.storrie.dead)^2*.0017*clumps))


#filter out QUKE from other spps 
q <- filter(postS.basal, Spp=="QUKE")

#ADDS basal area of sprouts using extrapolated focal oak data
q$ba= (qukes$Sto.ba+q$ba)

#replaces basal area in quke data with values from focal oak data if overstory basal area =0 
#q$ba <- ifelse(q$ba==0, qukes$Sto.ba, q$ba)

#remove QUKE data from rest of basal area data
postS.basal <- postS.basal %>% filter(!Spp %in% "QUKE")

#re-merges new quke data with rest of basal area data
postS.basal <- rbind(q, postS.basal)

#transposes rows to columns
postS.basal <- spread(postS.basal, key = "Spp", value = "ba", fill = 0.0) 

#sums ba by spp
postS.basal <- postS.basal %>%
  group_by(Storrie.Severity) %>%
  summarize(ABCO=sum(ABCO), CADE=sum(CADE), PILA=sum(PILA), PIPO=sum(PIPO), PSME=sum(PSME), QUKE=sum(QUKE))

#remove storrie sev col
postS.basal <- postS.basal[2:7]

#relativizes 
postS.basal <- decostand(postS.basal, method = "total")

#caluculate frequency for each species in each severity category
postS.freq <- comp %>%
  group_by(Storrie.Severity, plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U")) %>%
  summarize(sum.dist = sum(dist))

postS.freq <- spread(postS.freq, key = "Spp", value = "sum.dist", fill = 0.0)

postS.freq <- postS.freq %>%
  group_by(Storrie.Severity) %>% 
  summarize(ABCO=length(which(ABCO!=0))/n(), CADE=length(which(CADE!=0))/n(), PILA=length(which(PILA!=0))/n(), PIPO=length(which(PIPO!=0))/n(), PSME=length(which(PSME!=0))/n(), QUKE=length(which(QUKE!=0))/n())

postS.freq <- postS.freq[,2:7]

postS.import <- (postS.dens+postS.basal+postS.freq)*100

postS.import <- t(postS.import)

colnames(postS.import) <- c("unburned", "low", "moderate","high")


#####plot some stuff!
par(mfrow=c(2,1))
par(mar=c(3,4.1,2.5,1.6))
#sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.

colors <- brewer.pal(n = 6, name = "RdBu")

#plots of pre-fire values broken into storrie severity categories
barplot(height = pre.import, beside = T, col = colors, ylab = "Importance Value", ylim = c(0,300), main = "Pre-Fire - overstory only", xaxt="n")

barplot(height = postS.import, beside = T, col = colors, ylab = "Importance Value", ylim = c(0,300), main = "Post-Storrie - overstory only")
legend(22, 300, legend = rownames(postS.import), fill = colors, horiz = F, y.intersp = .8)


par(las = 0)

#legend.text = rownames(pre.import), args.legend = list(x=25, y=-10, horiz=T)
#legend(7, -10, legend = rownames(pre.import), fill = colors, horiz = T)

############## #POST-Chips fire values
####group plots into severity categories first, and then caluculate relative importance values based on summed basal area and density

#calculates relative density of each species for each storrie and chips severity category
postC.dens <- comp %>%
  group_by(Storrie.Severity, Chips.Severity) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "SSKC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#removes severity column
postC.dens <- postC.dens[,3:8]

#relativizes
postC.dens <- decostand(postC.dens, method = "total")

#calculates total basal area for each species by storrie and chips severity
postC.basal <- comp %>%
  group_by(Storrie.Severity, Chips.Severity, plot, Spp) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "KSQ", "KCQ", "SSKCQ")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#imports QUKE clump (FO) data 
qukes <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv")

#Adds in basal diameters of Storrie/Chips sprouts from focal oak data - multiplied by num of clumps in each plot
qukesC <- qukes %>%
  mutate(Chips.ba = ((diam.storrie.live+diam.chips)^2*.0017*clumps))

#filter out QUKE from other spps 
qC <- filter(postC.basal, Spp=="QUKE")

#replaces basal area in quke data with values from focal oak data if overstory basal area =0 
qC$ba <- ifelse(qC$ba==0, qukesC$Chips.ba, qC$ba)

#remove QUKE data from rest of basal area data
postC.basal <- postC.basal %>% filter(!Spp %in% "QUKE")

#re-merges new quke data with rest of basal area data
postC.basal <- rbind(qC, postC.basal)

#transposes rows to columns
postC.basal <- spread(postC.basal, key = "Spp", value = "ba", fill = 0.0) 

#sums ba by spp
postC.basal <- postC.basal %>%
  group_by(Storrie.Severity, Chips.Severity) %>%
  summarize(ABCO=sum(ABCO), CADE=sum(CADE), PILA=sum(PILA), PIPO=sum(PIPO), PSME=sum(PSME), QUKE=sum(QUKE))

#remove storrie sev col
postC.basal <- postC.basal[3:8]

#relativizes 
postC.basal <- decostand(postC.basal, method = "total")

#caluculate frequency for each species in each severity category
postC.freq <- comp %>%
  group_by(Storrie.Severity, Chips.Severity,  plot, Spp) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "SSKC")) %>%
  summarize(sum.dist = sum(dist))

postC.freq <- spread(postC.freq, key = "Spp", value = "sum.dist", fill = 0.0)

postC.freq <- postC.freq %>%
  group_by(Storrie.Severity, Chips.Severity) %>% 
  summarize(ABCO=length(which(ABCO!=0))/n(), CADE=length(which(CADE!=0))/n(), PILA=length(which(PILA!=0))/n(), PIPO=length(which(PIPO!=0))/n(), PSME=length(which(PSME!=0))/n(), QUKE=length(which(QUKE!=0))/n())

postC.freq <- postC.freq[,3:8]

postC.import <- (postC.dens+postC.basal+postC.freq)*100

postC.import <- t(postC.import)

colnames(postC.import) <- c("un/un", "un/low", "un/mod","un/high", "low/un", "low/low", "low/mod", "low/high", "mod/un", "mod/low", "mod/mod", "mod/high", "high/un", "high/low", "high/mod", "high/high")

####################
#####plot some stuff!
par(mfrow=c(3,1))
par(mar=c(3,4.1,2.5,1.6))
#sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.

colors <- brewer.pal(n = 6, name = "RdBu")

#plots of pre-fire values broken into storrie severity categories
barplot(height = pre.import, beside = T, col = colors, ylab = "Importance Value", ylim = c(0,300), main = "Pre-Fire - overstory only", xaxt="n")

barplot(height = postS.import, beside = T, col = colors, ylab = "Importance Value", ylim = c(0,300), main = "Post-Storrie - overstory only")
legend(22, 300, legend = rownames(postS.import), fill = colors, horiz = F, y.intersp = .8)

barplot(height = postC.import, beside = T, col = colors, ylab = "Importance Value", ylim = c(0,300), main = "Post-Chips - overstory only", space = c(.5,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 3,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 3,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 3,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0, 1,0,0,0,0,0))

par(las = 0)

#legend.text = rownames(pre.import), args.legend = list(x=25, y=-10, horiz=T)
#legend(7, -10, legend = rownames(pre.import), fill = colors, horiz = T)


##############################################

#calculate importance values across all plots
pre.density3 <- comp %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#relativizes
pre.density3 <- decostand(pre.density3, method = "total")

#calculates total basal area for each species by storrie severity
pre.basal3 <- comp %>%
  group_by(Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(.0017*sum.dbh^2)) %>%
  select(-sum.dbh)

#transposes rows to columns
pre.basal3 <- spread(pre.basal3, key = "Spp", value = "ba", fill = 0.0)

#filters out uncommon % unk spp's
pre.basal3 <- pre.basal3 %>%
  select(-ACMA, -PIMO, -QUCH)

#relativizes 
pre.basal3 <- decostand(pre.basal3, method = "total")

#caluculate frequency for each species in each severity category
pre.freq3 <- comp %>%
  group_by(plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U")) %>%
  summarize(sum.dist = sum(dist))

pre.freq3 <- spread(pre.freq3, key = "Spp", value = "sum.dist", fill = 0.0)

pre.freq3 <- pre.freq3 %>%
  summarize(ABCO=length(which(ABCO!=0))/n(), CADE=length(which(CADE!=0))/n(), PILA=length(which(PILA!=0))/n(), PIPO=length(which(PIPO!=0))/n(), PSME=length(which(PSME!=0))/n(), QUKE=length(which(QUKE!=0))/n())

pre.freq3 <- pre.freq3[,2:7]

pre.freq3 <- colSums(pre.freq3)

pre.freq3 <- t(pre.freq3)

#relativizes
pre.freq3 <- decostand(pre.freq3, method = "max", MARGIN = 1)

pre.import3 <- ((pre.density3+pre.basal3+pre.freq3)*100)

#pre.import <- data.frame(pre.import, "Storrie"=c(1:4))

pre.import3 <- t(pre.import3)

#plot of mean importance values for all plots pre-fire
barplot(pre.import3, names.arg = rownames(pre.import3), beside = T, col = colors, ylab = "Importance Value", ylim = c(0,200), main = "Relative pre-fire values for all plots - overstory only", space = .1)
legend(4.6, 200, legend = rownames(pre.import3), fill = colors, horiz = F)




