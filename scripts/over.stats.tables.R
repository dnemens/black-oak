#calculates stats for tables

library(nlme)
library(tidyverse)
library(vegan)
library(RColorBrewer)

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

############
#repeated measures ANoVA for importance values
imp <- import$Importance.Value
time <- import$time
spp <- import$Species

corAR1()

model = lme(imp ~ time + spp + spp*time, 
            correlation = corAR1(form = ~ Month | Student,
                                 value = 0.4287),
            data=Data,
            method="REML")

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

########
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

#create vectors
ba <- pre.bas$ba
tpa <- pre.dens$tph


#QMD
qmd = function( ba, tph, unittype="imperial" )
{
    qmd = sqrt((ba / tph) / 0.00007854)
  }
  qmd


#basal area
basalarea = function( dia, weight, unittype="imperial" )
{
  # Function to calculate the basal area per unit area
  # weight is the expansion factor to convert the sample area number to 
  # the unit area.
  # By David R. Larsen, Copyright, October 9, 2012
  # Creative Commons http://creativecommons.org/licenses/by-nc/3.0/us/
  
  if ( unittype == "imperial" ){
    bat = 0.005454154 * dia ^ 2 * weight
  }else if ( unittype == "metic" ){
    bat = 0.00007854 * dia ^ 2 * weight
  }else{
    bat = rep( 0, length=length(dia) )
  }
  ba = sum(bat)
  ba
}
#Stand density index
sdi = function( tpa, qmd, unittype="imperial" )
{
  # Function to calculate the stand density index
  # by David R. Larsen, Copyright October 9, 2012
  # Creative Commons http://creativecommons.org/licenses/by-nc/3.0/us/
  
  if (unittype == "imperial" ){
    sdi = tpa * ( qmd / 10 )^ 1.605
  }else if ( unittype == "metric" ){
    sdi = tpa * ( qmd / 25.4)^1.605
  }else{
    sdi = 0
  }
  sdi
}  