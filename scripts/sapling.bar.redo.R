#sapling bar graph redo with focal oak midstory data instead of center sub-plot data

#loads focal oak sub-plot midstory data
midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/midstory.csv")
midstory[is.na(midstory)] <-  0 #replaces missing data with 0's
#loads rdnbr values for each plot
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")


library(tidyr) #for pipe operator?
library(dplyr)

#caluculates sapling density using focal oak midstory data
#number of stems per plot 
density.m <- midstory %>%
  filter(spp == "ABCO" | spp == "PSME") %>%
  group_by(Plot) %>% 
  filter(tree.ht.class>=3) %>% 
  
  summarize(abco.density = sum((tree.num)[which(spp == "ABCO")]), psme.density = sum((tree.num)[which(spp == "PSME")]))
#number of stems per ha
#density.m <- mutate(density.m, den.ha.abco = density.m$abco.density*63.69, den.ha.psme = density.m$psme.density*63.69)

density.m  <-  rename(density.m, plot=Plot, ABCO=abco.density, PSME=psme.density)
midstory2 <-  merge(density.m, rdnbr, by="plot")


library(ggplot2)

ggplot(midstory2, aes(chips_rdnbr))+
  geom_bar(aes(y=ABCO), stat = "identity", position = "dodge", fill="grey50", color="black", width = 15)+
  geom_bar(aes(y=PSME), stat = "identity", fill="red", position="dodge", width = 15)+
  scale_x_continuous(breaks=seq(-500,999,125))+
  coord_cartesian(xlim=c(-470, 900))+
  scale_y_continuous(limits = c(0,61), expand = c(0, 0))+
  theme_classic()+
  theme (panel.border = element_rect(fill = NA))+
  geom_vline(xintercept = 69)+
  geom_vline(xintercept = 315)+
  geom_vline(xintercept = 641)+
  geom_text(x=40, y=30, angle=90, label = "Unburned")+
  geom_text(x=290, y=30, angle=90, label = "Low") +
  geom_text(x=615, y=30, angle=90, label = "Moderate")+
  geom_text(x=930, y=30, angle=90, label = "High")+
  labs(y="Sapling density", x="Chips fire severity (RdNBR)")+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=12, color="black"), axis.title.x = element_text(margin=margin(t=18)), axis.title.y = element_text(margin=margin(r=22)))+
  theme(legend.position = c(.09,.85), legend.title = element_blank(), legend.text = element_text(size=12), legend.background = element_blank())+
  annotate("text", x=875, y=55, label="b)", size=5)  


ggplot(midstory2, aes(chips_rdnbr)) +
  scale_x_continuous(breaks=seq(-500,999,125))+
  coord_cartesian(xlim=c(-470, 900))+
  scale_y_continuous(limits = c(0,65), expand = c(0, 0)) +
  theme_classic()+
  theme (panel.border = element_rect(fill = NA))+
  geom_vline(xintercept = 69)+
  geom_vline(xintercept = 315)+
  geom_vline(xintercept = 641)+
  geom_text(x=40, y=20, angle=90, label = "Unburned")+
  geom_text(x=290, y=20, angle=90, label = "Low") +
  geom_text(x=615, y=20, angle=90, label = "Moderate")+
  geom_text(x=930, y=20, angle=90, label = "High") +
  geom_histogram(aes(fill=Spp), colour="black", position = "dodge") +
  scale_fill_manual(values=c("grey90", "black")) +
  labs(y="Sapling density", x="Chips fire severity (RdNBR)")+
  theme(axis.title = element_text(size=20), axis.text = element_text(size=12, color="black"), axis.title.x = element_text(margin=margin(t=18)), axis.title.y = element_text(margin=margin(r=22)))+
  theme(legend.position = c(.09,.85), legend.title = element_blank(), legend.text = element_text(size=12), legend.background = element_blank())+
  annotate("text", x=875, y=60, label="b)", size=5)  
