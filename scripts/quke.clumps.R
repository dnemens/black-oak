#summary stats of basal area and sprouts per clump, plot and ha

comp <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.csv")
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

library(dplyr)


#filters out live QUKE clumps, and counts clumps per plot
qukes <- comp %>%
  filter(Spp == "QUKE") %>%
  group_by(plot) %>%
  filter(snag_stump..diam==0) %>%
  summarize(clumps = n()) 
  

#calculates extrapolated clumps/ha for each plot
quke.clumps <- mutate (qukes, clumps.ha=clumps*22.2222)

mean(quke.clumps$clumps.ha)
sd(quke.clumps$clumps.ha)


######calculates basal area and num of sprouts per clump for each plot#######
#loads csv of sprout response data 
sprout <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/focaloak.csv")

#function for calculating SE
se <- function(x) sqrt(var(x)/length(x))

#sprout <- mutate(sprout, ba.m2ha=chips.live^2*.00007854)
sprout2 <- mutate(sprout, ba.cm2.C=(((chips.live/2)^2)*pi), ba.cm2.S=(((storrie.live/2)^2)*pi))

#replaces NAs with 0's
sprout2[is.na(sprout2)] <- 0

#calculates total basal area of each sprout clump
sprout3 <- sprout2 %>%
  group_by(plot) %>%
  summarize(BA.C = sum(ba.cm2.C), BA.S = sum(ba.cm2.S), stems.C = length(chips.live[which(chips.live>0)]), stems.S = length(storrie.live[which(storrie.live>0)]))

sprout4 <-  merge(sprout3, quke.clumps, by="plot")

#calculates the total sprout basal area per plot
sprout4 <- mutate(sprout4, bap.S=(BA.S*clumps), bap.C=(BA.C*clumps))

#calculates total basal area per ha
sprout4 <- mutate(sprout4, bah.S=(BA.S*clumps.ha), bah.C=(BA.C*clumps.ha))

#calculates sprouts per ha
sprout4 <- mutate(sprout4, sph.S=(stems.S*clumps.ha), sph.C=(stems.C*clumps.ha))

#produces summary stats 
#for Storrie sprouts
quke.S <- sprout4 %>%
  filter(stems.S>0) %>%
  summarise(mean.S.b = mean(bap.S), se.S.b = se(bap.S), sd.S.b = sd(bap.S), mean.S = mean(sph.S), sd.S = sd(sph.S), se.S = se(sph.S)) 
#should I filter out the 0's?  probably...

#for Chips sprouts
quke.C <- sprout4 %>%
  filter(stems.C>0) %>%
  summarise(mean.C = mean(sph.C), sd.C = sd(sph.C), se.C=se(sph.C))

########################################################################################
#caluculates a stems/ha for each Storrie severity category (1-4)
library(tidyr)
#re-creates Storrie and Chips severity category columns
sprout4 <- sprout4 %>% separate(plot, c("Storrie", "Chips", "Plot"))

qukes.S <- quke.clumps %>%
  group_by(Storrie) %>%
  summarize(clumps = mean(clumps.ha)) 

#caluculates a mean ba/ha for each Chips severity category (1-4)
qukes.C <- quke.clumps %>%
  group_by(Chips) %>%
  summarize(clumps = mean(clumps.ha)) 



