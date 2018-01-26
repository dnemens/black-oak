#extracts summary stats for conifer seedlings and saplings

center <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/center sub plot.csv")
library(tidyr) #for pipe operator?
library(dplyr)
library(MASS)

#calculates density for both abco and psme using center sub-plot data
abco <- center %>%
  filter(Spp == "ABCO" | Spp == "PSME") %>%
  group_by(ht) %>%
  group_by(plot) %>%
  summarize(abco.density = length(which(Spp == "ABCO")*63.69), psme.density = length(which(Spp == "PSME")*63.69))

ab.s <- sum (abco$abco.density)
ps.s <-  sum (abco$psme.density)

midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/midstory.csv")

#caluculates 
abco.m <- midstory %>%
  filter(spp == "ABCO" | spp == "PSME") %>%
  summarize(abco.density = length(which(spp == "ABCO")*63.69), psme.density = length(which(spp == "PSME")*63.69))
  
  group_by(Plot) %>%
  summarize(abco.density = sum((tree.num)[which(spp == "ABCO")]*63.69), psme.density = sum((tree.num)[which(spp == "PSME")]*63.69))
