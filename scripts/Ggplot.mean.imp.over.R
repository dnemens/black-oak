#ggplots of continuous mean importance values - overstory

library(tidyverse)

all <- read.csv(file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv")

all = all[4:23]
all.g <- all %>%
  gather(ABCO:QUKE.2, key = "Species", value = "Importance.Value")

  

ggplot(data=all.g, aes(x=storrie_rdnbr))+
  geom_point(aes(y=Importance.Value, colour=Species))+
  geom_smooth(aes(y= Importance.Value, colour = Species), span = 2)+
  facet_wrap(~Species, ncol = 3)+ coord_cartesian(ylim = c(0,200), xlim = c(10,1180))+
  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = "none")

OR
ggplot(data=all, aes(x=storrie_rdnbr))+
  geom_point(aes(y=ABCO, colour = "blue"))+
  geom_smooth(aes(storrie_rdnbr, ABCO, colour ="blue"))+
  geom_point(aes(storrie_rdnbr, ABCO.1, colour = "red"), shape = 17)+
  geom_smooth(aes(storrie_rdnbr, ABCO.1, colour = "red"))+
  geom_point(aes(storrie_rdnbr, ABCO.2, colour = "green"), shape = 15)+
  geom_smooth(aes(storrie_rdnbr, ABCO.2, colour = "green"))+
  scale_colour_manual("", breaks = c("Pre-Fire", "Post-Storrie", "Post-Chips"),
         values = c("blue", "red", "green")) +
  coord_cartesian(ylim = c(0,200), xlim = c(10,1180))+
  theme_bw() +
  theme(panel.grid = element_blank())
  

  
