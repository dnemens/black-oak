## preps dataframe for plotting on continuous variables - RdNBR

library(tidyverse)
library(RColorBrewer)

#import data sheet -- wide form mean importance values
import <-  read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv", header = T)
import <-  read.csv("C:/Users/debne/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv", header = T)

#imports rdnbr values
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv", header = T)
rdnbr <- read.csv("C:/Users/debne/Dropbox/CBO/black-oak/data sheets/rdnbr.csv", header = T)

#replaces all pre-fire 0's with NA's (prevents false 0's when calculating delta IV's)
wide <- import[,2:7]
wide[wide==0] <- NA
wide.2 <- import[,14:19]

#subtracts pre-fire from post-chips IVs  
import.delta <- (wide.2-wide)
names(import.delta) <- names(wide)

#adds rdnbr values to dataframe, fixes column names, removes post-storrie values
import.rd <- data.frame(rdnbr$storrie_rdnbr, rdnbr$chips_rdnbr, import.delta)
import.rd <-  import.rd %>%
  rename ("Storrie" = rdnbr.storrie_rdnbr, "Chips" = rdnbr.chips_rdnbr) 

#combines rdnbr values                                                                             
import.rd <- import.rd %>%
  mutate(comb = Storrie + Chips)

#saves a new csv
write.csv(import.rd, "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/import.over.delta.csv", row.names = F)

#makes long form data frame
import.rd.long <- gather(import.rd, key = "Species", value = "Importance_Value", ABCO:QUKE)
#saves a new csv
write.csv(import.rd.long, "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/import.over.delta.long.csv", row.names = F)

##########
colors <- brewer.pal(n = 6, name = "RdBu")
#colors <- c('#fafac4', '#f9f17f', '#fd8d3c','#d94801','#86131f','#430a0f') #sequential, greyscaleable

ggplot(import.rd, aes(x=comb))+
  geom_point(aes(y=ABCO), size=3, colour='#fafac4')+
  geom_smooth(aes(y=ABCO), method="gam",  formula = y ~ s(x, bs = "cs"), linetype="dashed",
              color='#fafac4', fill='#fafac4')+
  geom_point(aes(y=PILA), size=3, colour = '#fd8d3c')+
  geom_smooth(aes(y=PILA), method=glm,  linetype="dashed",
              color='#fd8d3c', fill='#fd8d3c')+
  geom_point(aes(y=PIPO), size=3, colour = '#d94801')+
  geom_point(aes(y=QUKE), size=3, colour = '#430a0f')+
  geom_smooth(aes(y=QUKE), method=glm,  linetype="dashed",
              color='#430a0f', fill='#430a0f')+
  coord_cartesian(ylim = c(-300, 300))+
  theme(axis.title = element_text(size=16), axis.text = element_text(size=12), panel.background = element_blank())+
  labs(x="Combined Fire Severity (RdNBR)", y="Importance Value")+
  geom_hline(aes(yintercept=0), colour="black")

###############
#plot using long form dataframe
allSpp <- ggplot(import.rd.long, aes(x=comb, y=Importance_Value, colour=Species)) +
  geom_point(size=3)+
  geom_smooth(span=2)+
  coord_cartesian(ylim = c(-250, 250))+
  scale_colour_manual (values= rev(colors), labels = c("White fir - ABCO", "Incense-cedar - CADE", "Sugar pine - PILA", "Ponderosa - PIPO", "Douglas-fir - PSME", "Black oak - QUKE"))+
theme(axis.title = element_text(size=30), axis.text = element_text(size=20), panel.background = element_blank(), legend.text = element_text(size = 18), legend.key.height = unit(1.1, "cm"), legend.title = element_blank())+
  labs(x="Combined Fire Severity (RdNBR)", y="Delta Importance Value")+
  geom_segment(aes(x=0, xend=1928, y=0, yend=0), size=1, colour="black")
  

setwd("/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(allSpp, filename = "deltaIVs.RdNBR.codes.tiff", dpi = 300, width = 10, height = 6)


##############
#plot only abco & quke values for contrast
import.selected <- filter(import.rd.long, Species == "ABCO" | Species == "QUKE")
sel <- ggplot(import.selected, aes(x=comb, y=Importance_Value, colour=Species)) +
  geom_point(size=3)+
  geom_smooth(span=100)+
  coord_cartesian(ylim = c(-250, 250))+
  theme(axis.title = element_text(size=30), axis.text = element_text(size=20), panel.background = element_blank(), legend.text = element_text(size = 20))+
  labs(x="Combined Fire Severity (RdNBR)", y="Delta Importance Value")+
  geom_segment(aes(x=0, xend=1928, y=0, yend=0), size=1, colour="black")+
  scale_color_manual(values = c('#b2182b', '#2166ac'))

setwd("/Users/debne/Dropbox/CBO/black-oak/plots")
ggsave(sel, filename = "deltaIVs.RdNBR.2spp.tiff", dpi = 300, width = 10, height = 7)

