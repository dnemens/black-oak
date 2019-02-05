#Midstory importance values by categorical fire severity

library(tidyverse)
library(vegan)
library(RColorBrewer)

#import understory data (center sub-plot)
under <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/center sub plot.csv")
#replaces na's with 0
under[is.na(under)] <- 0

under <-  under %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) %>%
  select(-Plot) 

###########
#Caluclulates importance values by Chips severity only

##########calculates relative density per plot
density <- under %>%
  group_by(Chips) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#remove Severity column 
density <- density[,2:7] 
#realtivize density metrics
density <- decostand(density, method="total")

##############
#caluculate frequency for each species in each severity category
freq <- under %>%
  group_by(Chips, plot, Spp) %>%
  summarize(sum.dist = sum(dist))

freq <- spread(freq, key = "Spp", value = "sum.dist", fill = 0.0)

freq <- freq %>%
  group_by(Chips) %>% 
  summarize(ABCO=length(which(ABCO!=0))/n(), CADE=length(which(CADE!=0))/n(), PILA=length(which(PILA!=0))/n(), PIPO=length(which(PIPO!=0))/n(), PSME=length(which(PSME!=0))/n(), QUKE=length(which(QUKE!=0))/n())

freq <- freq[,2:7]

##########calculates relative dominance using crown area values
crown <- under %>%
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) %>%
  select(-Plot) %>%
  group_by(Chips, Spp) %>%
  summarize(area=sum(crown.area))

#transposes rows to columns
crown <- spread(crown, key = "Spp", value = "area", fill = 0.0)

#removes plot and ? columns
crown <- crown %>%
  select(ABCO, CADE, PILA, PIPO, PSME, QUKE)
crown <- crown [2:7]

#relativizes 
crown <- decostand(crown, method = "total")

##########combines matrices to calculate relative importance values

under.import2 <- (freq+crown+density)*100

under.import2 <- t(under.import2)

colnames(under.import2) <- c("unburned", "low", "moderate", "high")

#########Plot
par(mfrow=c(1,1))
par(mar=c(3,4.1,2.5,1.6))
#sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.

colors <- brewer.pal(n = 6, name = "RdBu")

#plots of pre-fire values broken into storrie severity categories
barplot(height = under.import2, beside = T, col = colors, ylab = "Importance Value", ylim = c(0,300), main = "Midstory: Post-Chips Fire")
legend(23, 300, legend = row.names(under.import2), fill = colors)

###  Plot Components
########### prep for ggplot

dens2 <- t(density*100)
colnames(dens2) <- c("unburned", "low", "moderate", "high")
dens2 <- data.frame(dens2)

dens2 <- dens2 %>%
  gather(1:4, key = "history", value = "Density")

dens2$Species <- c("ABCO", "CADE", "PILA", "PIPO", "PSME", "QUKE")

##
freq2 <- freq %>%
  gather(1:6, key = "Species", value = "Frequency")

freq2$Frequency <- freq2$Frequency*100

freq2$history <- c("unburned", "low", "moderate", "high")

##
crown2 <- crown %>%
  gather(1:6, key = "Species", value = "Crown.Area")

crown2$Crown.Area <- crown2$Crown.Area*100

crown2$history <- c("unburned", "low", "moderate", "high")
##
imp2 <- merge(dens2, crown2, by = c("history", "Species"))
imp2 <- merge(imp2, freq2, by = c ("history", "Species"))

imp2g <- imp2 %>%
  gather(3:5, key="var", value = "val")

imp2g$history <- factor(imp2g$history, levels = c("unburned", "low", "moderate", "high"))

######### GGplot

ggplot(imp2g, aes(y=val, x=history, fill=Species)) + 
  geom_bar(stat="identity", position = "dodge") +    
  facet_wrap(~var)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("")+
  xlab("Fire Severity")+
  scale_y_continuous(limits = c(0,105))

  