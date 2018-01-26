#extracts summary stats for conifer seedlings and saplings
library(tidyr) #for pipe operator?
library(dplyr)

center <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/center sub plot.csv")

#calculates sapling (>50cm) density using center sub-plot data
density.c <- center %>%
  filter(Spp == "ABCO" | Spp == "PSME") %>%
  filter(ht>=3) %>%
  group_by(plot) %>%
  summarize(abco.density = length(which(Spp == "ABCO")), psme.density = length(which(Spp == "PSME")))

  density.c <- mutate(density.c, den.ha.abco = density.c$abco.density*177, den.ha.psme = psme.density*177)
  
mean(density.c$den.ha.abco)
mean(density.c$den.ha.psme)
sd(density.c$den.ha.abco)
sd(density.c$den.ha.psme)

#calculates sapling frequency using center sub-plot data
abcos <- density.c$abco.density
length(abcos[which(abcos>0)])
psmes <- density.c$psme.density
length(psmes[which(psmes>0)])

#calculates seedling (<50cm) density using center sub-plot data
density.c.s <- center %>%
  filter(Spp == "ABCO" | Spp == "PSME") %>%
  filter(ht<3) %>%
  group_by(plot) %>%
  summarize(abco.density = length(which(Spp == "ABCO")), psme.density = length(which(Spp == "PSME")))

density.c.s <- mutate(density.c.s, den.ha.abco = abco.density*177, den.ha.psme = psme.density*177)

mean(density.c.s$den.ha.abco)
sd(density.c.s$den.ha.abco)
mean(density.c.s$den.ha.psme)
sd(density.c.s$den.ha.psme)

#calculates seedling frequency using center sub-plot data
abcos <- density.c.s$abco.density
length(abcos[which(abcos>0)])
psmes <- density.c.s$psme.density
length(psmes[which(psmes>0)])

###################same as above with focal oak midstory data#######################
midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/midstory.csv")

#caluculates sapling density using focal oak midstory data
#number of stems per plot 
density.m <- midstory %>%
  filter(spp == "ABCO" | spp == "PSME") %>%
  group_by(Plot) %>% 
  filter(tree.ht.class>=3) %>% 
  summarize(abco.density = sum((tree.num)[which(spp == "ABCO")]), psme.density = sum((tree.num)[which(spp == "PSME")]))
#number of stems per ha
density.m <- mutate(density.m, den.ha.abco = density.m$abco.density*63.69, den.ha.psme = density.m$psme.density*63.69)

mean(density.m$den.ha.abco)
sd(density.m$den.ha.abco)
mean(density.m$den.ha.psme)
sd(density.m$den.ha.psme)

#calculates sapling frequency using focal oak midstory data
abcos <- density.m$abco.density
length(abcos[which(abcos>0)])
psmes <- density.m$psme.density
length(psmes[which(psmes>0)])

###############################################
#caluculates seedling density using focal oak midstory data
#number of stems per plot 
density.m.s <- midstory %>%
  filter(spp == "ABCO" | spp == "PSME") %>%
  group_by(Plot) %>% 
  filter(tree.ht.class<3) %>% 
  summarize(abco.density = sum((tree.num)[which(spp == "ABCO")]), psme.density = sum((tree.num)[which(spp == "PSME")]))
#number of stems per ha
density.m.s <- mutate(density.m.s, den.ha.abco = density.m.s$abco.density*63.69, den.ha.psme = density.m.s$psme.density*63.69)

mean(density.m.s$den.ha.abco)
sd(density.m.s$den.ha.abco)
mean(density.m.s$den.ha.psme)
sd(density.m.s$den.ha.psme)

#calculates seedling frequency using focal oak midstory data
abcos <- density.m.s$abco.density
length(abcos[which(abcos>0)])
psmes <- density.m.s$psme.density
length(psmes[which(psmes>0)])



