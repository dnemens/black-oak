#summary stats of basal area and sprouts per clump, plot and ha

comp <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.csv")
comp[is.na(comp)] <- 0
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")
#loads csv of sprout response data 
sprout <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/focaloak.csv")
sprout[is.na(sprout)] <- 0

library(dplyr)


#filters out live QUKE clumps, and counts clumps per plot
qukes2 <- comp %>%
  filter(Spp == "QUKE"& snag.class!= "STUMP") %>%
  group_by(plot) %>%
  filter(dbh==0) %>%
  summarize(clumps = n()) 

sprouts <- sprout %>%
  group_by(plot) %>%
  summarise(diam.storrie.live = sum(storrie.live), diam.storrie.dead = (sum(storrie.dead)), diam.chips = sum(chips.live))

#merges both df's keeping plots with no clumps
qukes2 <- merge(qukes2, sprouts, by="plot", all.y = T)

qukes2[is.na(qukes2)] <- 0 #replaces na's with 0
  
#creates csv of sprout clump data
write.csv(qukes2, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv", row.names = F)

###################
#calculates extrapolated clumps/ha for each plot
quke.clumps <- mutate (qukes, clumps.ha=clumps*22.2222)

mean(quke.clumps$clumps.ha)
sd(quke.clumps$clumps.ha)


######calculates basal area and num of sprouts per clump for each plot#######
#loads csv of sprout response data 
sprout <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/focaloak.csv")

#replaces NAs with 0's
sprout[is.na(sprout)] <- 0

#function for calculating SE
se <- function(x) sqrt(var(x)/length(x))

#sprout <- mutate(sprout, ba.m2ha=chips.live^2*.00007854)
sprout2 <- mutate(sprout, ba.cm2.C=(((chips.live/2)^2)*pi), ba.cm2.S=(((storrie.live/2)^2)*pi))

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

write.csv(sprout4, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/sprout_calc.csv")

#produces summary stats 
#for Storrie sprouts
quke.S <- sprout4 %>%
  filter(stems.S>0) %>%
  summarise(mean.S.b = mean(BA.S), se.S.b = se(BA.S), mean.S = mean(stems.S), se.S=se(stems.S), mean.Sh = mean(sph.S), se.Sh = se(sph.S)) 

#for Chips sprouts
quke.C <- sprout4 %>%
  filter(stems.C>0) %>%
  summarise(mean.C.b = mean(BA.C), se.C.b = se(BA.C), mean.C = mean(stems.C), se.Cc=se(stems.C), mean.Ch = mean(sph.C), se.Ch=se(sph.C))

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

########################################################
#determines whether fire history is mostly uniform for quke clumps in each plot
q <- comp %>%
  filter(!snag.class %in% c("", " ")) %>%
  filter(!fire.hist %in% c("", " ", "U")) %>%
  filter(Spp == "QUKE") %>%
  group_by(plot) %>%
  summarise(n = n_distinct(fire.hist))

hist(q$n)
