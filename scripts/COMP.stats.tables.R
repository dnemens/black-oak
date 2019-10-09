#calculates stats for tables (QMD, SDI, TPH)

library(tidyverse)
library(vegan)

#######################
#import overstory data
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0

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

################
#PRE-FIRE ###
#TPHa
pre.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#into long form
pre.dens <- pre.density %>%
  gather(2:7, key = "Spp", value = "tpp")

#convert trees/plot to trees/ha
pre.dens <- pre.dens %>%
  mutate(tph = tpp*22.22) %>%
  mutate(tpp=NULL)

######## basal area
###WRONG!!!!pre.basal <- comp %>%
  #group_by(plot, Spp) %>%
  #filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  #summarize(sum.dbh = sum(dbh)) %>%
  #mutate(ba=(0.00007854*sum.dbh^2)) %>%
  #select(-sum.dbh)

pre.basal <- comp %>%
  group_by(plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  mutate(trba=(0.00007854*dbh^2)) %>%
  summarize(ba=sum(trba)*22.22) 

pre.basal <- pre.basal %>%
  group_by(plot) %>%
  spread(key = "Spp", value = "ba") %>%
  select(ABCO, CADE, PILA, PIPO, PSME, QUKE)

pre.basal [is.na(pre.basal)] <- 0

pre.bas <- pre.basal %>%
  gather(2:7, key = "Spp", value = "ba")

#####
#data frame of pre-fire Vals
p.stats <- data.frame(pre.bas, pre.dens$tph)
p.stats <- rename(p.stats, tph = pre.dens.tph)
p.stats$Condition <- "Pre-fire"

####POst-Storrie
postS.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "SC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#into long form
postS.dens <- postS.density %>%
  gather(2:7, key = "Spp", value = "tpp")

#convert trees/plot to trees/ha
postS.dens <- postS.dens %>%
  mutate(tph = tpp*22.22) %>%
  mutate(tpp=NULL)

####
#calculates total basal area for each species 
postS.basal <- comp %>%
  group_by(plot,Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "KSQ", "SC")) %>%
  mutate(trba=(0.00007854*dbh^2)) %>%
  summarize(ba=sum(trba)*22.22) 

#transposes rows to columns
postS.basal <- spread(postS.basal, key = "Spp", value = "ba", fill = 0.0)

#Adds in basal diameters of Storrie sprouts from focal oak data - multiplied by num of clumps in each plot
qukes <- qukes %>%
  mutate(Sto.ba = ((baSD+baSL)*clumps*22.22))

QUKE2 <- qukes$Sto.ba

#ADDS basal area of sprouts using extrapolated focal oak data
postS.basal$QUKE= (QUKE2+postS.basal$QUKE)

#filters out uncommon % unk spp's
postS.basal <- postS.basal %>%
  select(-ACMA, -PIMO, -QUCH)

postS.bas <- postS.basal %>%
  gather(2:7, key = "Spp", value = "ba")

#data frame of post-Storrie Vals
pS.stats <- data.frame(postS.bas, postS.dens$tph)
pS.stats <- rename(pS.stats, tph = postS.dens.tph)
pS.stats$Condition <- "Post-Storrie Fire"

########
postC.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "SSKC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#into long form
postC.dens <- postC.density %>%
  gather(2:7, key = "Spp", value = "tpp")

#convert trees/plot to trees/ha
postC.dens <- postC.dens %>%
  mutate(tph = tpp*22.22) %>%
  mutate(tpp=NULL)

######
postC.basal <- comp %>%
  group_by(plot, Spp) %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "KSQ", "KCQ", "SSKCQ", "SSKC")) %>%
  mutate(trba=(0.00007854*dbh^2)) %>%
  summarize(ba=sum(trba)*22.22) 

#transposes rows to columns
postC.basal <- spread(postC.basal, key = "Spp", value = "ba", fill = 0.0)

#Adds in basal diameters of Chips sprouts from focal oak data - multiplied by num of clumps in each plot
qukes2 <- qukes %>%
  mutate(Chips.ba = ((baSL+baC)*22.22*clumps))

QUKE2c <- qukes2$Chips.ba

#ADDS basal area of sprouts using extrapolated focal oak data
postC.basal$QUKE= (QUKE2c+postC.basal$QUKE)

#filters out uncommon % unk spp's
postC.basal <- postC.basal %>%
  select(-ACMA, -CONU, -PIMO, -QUCH)

postC.bas <- postC.basal %>%
  gather(2:7, key = "Spp", value = "ba")

#data frame of post-Storrie Vals
pC.stats <- data.frame(postC.bas, postC.dens$tph)
pC.stats <- rename(pC.stats, tph=postC.dens.tph)
pC.stats$Condition <- "Post-Chips Fire"
#######
stats <- rbind(p.stats, pS.stats, pC.stats)

tph <- stats$tph
ba <- stats$ba
#######
#QMD
qmd = function( ba, tph)
{
    qmd = sqrt((ba / tph) / 0.00007854)
  }
  qmd

  
QMD <- qmd (ba, tph)

#Stand density index
sdi = function( tph, qmd )
{
      sdi = (tph * (qmd / 25.4)^1.605)
    }
  sdi

  SDI <- sdi(ba, QMD)
  
#create csv with stats
  
stats2 <- data.frame(stats, QMD, SDI)
stats2[is.na(stats2)] <- 0
stats2 <- stats2[c("plot", "Spp", "Condition", "ba", "tph", "QMD", "SDI")]

write.csv(stats2, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/over.stats.raw.csv", row.names = F)

stats.sum <- stats2 %>%
  #separate(plot, c("Storrie", "Chips", "Plot"), remove = F) %>%
  select(-plot) %>%
  #group_by(Storrie, Chips, Spp) %>%
  group_by(Condition, Spp) %>%
  rename("Species" = Spp) %>%
  summarise_all(funs(mean=mean, sd=sd)) 


stats.sum.t <- stats.sum %>%
  group_by(Condition, Species) %>%
 transmute(BA = paste0(round(ba_mean,2), " (", round(ba_sd, 2), ")"), TPH = paste0(round(tph_mean,2), " (", round(tph_sd, 2), ")"), SDI = paste0(round(SDI_mean,2), " (", round(SDI_sd, 2), ")"), QMD = paste0(round(QMD_mean,2), " (", round(QMD_sd, 2), ")")) %>%
  arrange(desc(Condition))


write.csv(stats.sum.t, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/over.stats.summary.csv", row.names = F)
  