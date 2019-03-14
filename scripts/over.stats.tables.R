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

#importance values
#import data sheet -- long form mean importance values
import <-  read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.long.csv", header = T)

#Order time factor
import$time <- ordered(import$time, levels = c("Pre-fire", "Post-Storrie Fire", "Post-Chips Fire"))

################
#TPHa
pre.density <- comp %>%
  group_by(plot) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

pre.density <- pre.density[,2:7] #remove plot column
pre.density <- decostand(pre.density, method="total") #relativize ???should we do this???  
pre.density <- data.frame(rdnbr$plot, pre.density) #adds plot column back
pre.density <- rename(pre.density, plot = rdnbr.plot)

#into long form
pre.dens <- pre.density %>%
  gather(2:7, key = "Spp", value = "tpp")

#convert trees/plot to trees/ha
pre.dens <- pre.dens %>%
  mutate(tph = tpp*22.22)

tph <- pre.dens$tph

######## basal area
pre.basal <- comp %>%
  group_by(plot, Spp) %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  summarize(sum.dbh = sum(dbh)) %>%
  mutate(ba=(0.00007854*sum.dbh^2)) %>%
  select(-sum.dbh)

pre.basal <- pre.basal %>%
  group_by(plot) %>%
  spread(key = "Spp", value = "ba") %>%
  select(ABCO, CADE, PILA, PIPO, PSME, QUKE)

pre.basal [is.na(pre.basal)] <- 0

pre.bas <- pre.basal %>%
  gather(2:7, key = "Spp", value = "ba")

##########
#create vectors
ba <- pre.bas$ba
tph <- pre.dens$tph

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
stats <- data.frame(pre.bas, tph, QMD, SDI)

write.csv(stats, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/over.stats.raw", row.names = F)

stats.sum <- stats %>%
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) %>%
  select(-Plot) %>%
  group_by(Storrie, Chips, Spp) %>%
  summarise(mean.BA = mean(ba), sd.BA = sd(ba), mean.tph = mean(tph), sd.tph = sd(tph), mean.SDI = mean(SDI), sd.SDI = sd(SDI), mean.QMD = mean(QMD), sd.QMD = sd(QMD))

write.csv(stats.sum, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/over.stats.summary.csv", row.names = F)
  