#caluculates and plots MEAN importance values for each species in midstory pre and post fire by plot

library(DescTools)
library(tidyverse)
library(vegan)
library(RColorBrewer)

#import understory data (center sub-plot)
under <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/center sub plot.csv")
#replaces na's with 0
under[is.na(under)] <- 0

#import severity data
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")
rdnbr <- rdnbr %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) %>%
  select(-Plot) %>%
  unite(combined, Storrie, Chips, remove = T)

###################################################
##########calculates relative density per plot
density <- under %>%
  group_by(plot) %>%
  summarize(ABCO = length(which(Spp == "ABCO")), CADE = length(which(Spp == "CADE")), PILA = length(which(Spp == "PILA")), PIPO = length(which(Spp == "PIPO")), PSME = length(which(Spp == "PSME")), QUKE = length(which(Spp == "QUKE")))

#remove plot column 
density <- density[,2:7] 
#realtivize density metrics
density2 <- decostand(density, method="total")

density2 <- data.frame(rdnbr$plot, density)

### frequency
freq <- density2 %>%
  rename(plot = rdnbr.plot) %>%
  gather(2:7, key = Species, value = freq)

#sort by plot #
freq <- freq[order(freq$plot),]

#replace # with 1 or 0
freq$freq <- if_else(freq$freq > 0, 1, 0)

freq <- freq %>%
  spread(key = Species, value = freq)

freq <- freq[2:7]

#########calculates relative dominance by plot (using crown area vs. basal area)
crown <- under %>%
  group_by(plot, Spp) %>%
  summarize(area = sum(crown.area)) 

#transposes rows to columns
crown <- spread(crown, key = "Spp", value = "area", fill = 0.0)

#removes plot and ? columns
crown <- crown %>%
  select(ABCO, CADE, PILA, PIPO, PSME, QUKE)
crown <- crown [2:7]

#relativizes 
crown <- decostand(crown, method = "total")

##########
#prep COMPONENTS for plotting
###########
freq2 <- data.frame(rdnbr$plot, freq*100)
freq2 <- freq2 %>%
  mutate(comp = "Frequency") %>%
  separate(rdnbr.plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot, -Storrie)

density3 <- data.frame(rdnbr$plot, density*100)
density3 <- density3 %>%
  mutate(comp = "Density")%>%
  separate(rdnbr.plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot, -Storrie)

crown2 <- data.frame(rdnbr$plot, crown*100)
crown2 <- crown2 %>%
  mutate(comp = "Crown area")%>%
  separate(rdnbr.plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot, -Storrie)

under2 <- rbind(crown2, density3, freq2)
under2 <- under2[c(1,8,2:7)]

#turn into long form data frame
#might need to reload dpylr before executing?  
under2 <- under2 %>%
  gather(3:8, key = Species, value = val)

########### Prep for ggplot
under2.sum <- under2 %>%
  group_by(Chips, Species, comp) %>%
  summarise(N = length(val),
            mean = mean(val),
            sd   = sd(val),
            se   = sd / sqrt(N))

########## Plot! 
colors <- c('#fafac4', '#f9f17f', '#fd8d3c','#d94801','#86131f','#430a0f') #sequential, greyscaleable

colorsG <- ColToGray(colors)

ggplot(under2.sum, aes(y=mean, x=Chips, fill=Species)) + 
  facet_wrap(~comp)+
  geom_bar(stat="identity", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("")+
  xlab("Chips Fire Severity")+
  labs(title = "Mean Understory Components")+
  theme(axis.text.x = element_text(angle = 45), panel.grid = element_blank(), plot.title = element_text(hjust = 0.5))

###########
#prep IMPORTANCE VALUES for plotting
######

#creates matrix of importance values
under.import <- (crown+density+freq)*100

#convert to long form
under.import2 <- data.frame(rdnbr$plot, under.import)

under.import2 <- gather(under.import2, key = "Species", value = "Importance.Value", ABCO:QUKE)

under.import2 <-  under.import2 %>%
  rename ("plot" = rdnbr.plot)


#severity CATEGORIES
under.import2 <- under.import2 %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) 

#calculate stats for error bars etc.  
under.sum <- under.import2 %>%
  group_by(Chips, Species) %>%
  summarise(N = length(Importance.Value),
            mean = mean(Importance.Value),
            sd   = sd(Importance.Value),
            se   = sd / sqrt(N))

##PLot!
colors <- c('#fafac4', '#f9f17f', '#fd8d3c','#d94801','#86131f','#430a0f') #sequential, greyscaleable


colorsG <- ColToGray(colors)

ggplot(under.sum, aes(y=mean, x=Chips, fill=Species)) + 
  #facet_wrap(~Storrie)+
  geom_bar(stat="identity", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= colorsG, name = "Species")+
  ylab("Importance Value")+
  xlab("Chips Fire Severity")+
  #labs(title = "Mean Understory Importance Values")+
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())+
  scale_x_discrete(labels = c("Unburned", "Low", "Moderate", "High"))

#######
#prep imp values for plotting Combined severity
under.import.comb <- data.frame(rdnbr$combined, under.import)

under.import.comb2 <- under.import.comb %>%
  rename (comb = rdnbr.combined) %>%
  gather(2:7, key = Species, value = val) %>%
  group_by(Species, comb) %>%
  summarise(N = length(val),
            mean = mean(val),
            sd   = sd(val),
            se   = sd / sqrt(N))

##PLot!
colors <- c('#fafac4', '#f9f17f', '#fd8d3c','#d94801','#86131f','#430a0f') #sequential, greyscaleable

library(DescTools)
colorsG <- ColToGray(colors)

ggplot(under.import.comb2, aes(y=mean, x=comb, fill=Species)) + 
  #facet_wrap(~Storrie)+
  geom_bar(stat="identity", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("Importance Value")+
  xlab("Combined Fire Severity")+
  #labs(title = "Mean Understory Importance Values")+
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())

#  scale_x_discrete(labels = c("Unburned", "Low", "Moderate", "High"))
  
###########
#Prep for plot of Density only
density4 <- data.frame(rdnbr$combined, density)
density4.comb <- density4 %>%
  rename (comb = rdnbr.combined) %>%
  gather(2:7, key = Species, value = val) %>%
  group_by(Species, comb) %>%
  summarise(N = length(val),
            mean = mean(val),
            sd   = sd(val),
            se   = sd / sqrt(N))
ggplot(density4.comb, aes(y=mean, x=comb, fill=Species)) + 
  #facet_wrap(~Storrie)+
  geom_bar(stat="identity", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("Density (per plot)")+
  xlab("Combined Fire Severity")+
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())
  


#IMPORTANCE VALUE stats

#ABCO
ab <- subset(under.import2, under.import2$Species == "ABCO")
abImp <- ab$Importance.Value
abCsev <- as.factor(ab$Chips)

moda <- kruskal.test(abImp, abCsev)
moda

library (FSA)
modAdunn <- dunnTest(abImp, abCsev)
modAdunn

#QUKE
q <- subset(under.import2, under.import2$Species == "QUKE")
qImp <- q$Importance.Value
qCsev <- as.factor(q$Chips)

modq <- kruskal.test(qImp, qCsev)
modq

#PSME
ps <- subset(under.import2, under.import2$Species == "PSME")
psImp <- ps$Importance.Value
psCsev <- as.factor(ps$Chips)

modps <- kruskal.test(psImp, psCsev)
modps

#######
#COMBINED SEVERITY
#########

# convert combined sev to long form
under.import.comb2 <- gather(under.import.comb, key = "Species", value = "Importance.Value", ABCO:QUKE)

under.import.comb2 <- rename (under.import.comb2, comb = rdnbr.combined)

#Prep for ggplot - combined severity
#calculate stats for error bars etc.  
under.comb.sum <- under.import.comb2 %>%
  group_by(comb, Species) %>%
  summarise(N = length(Importance.Value),
            mean = mean(Importance.Value),
            sd   = sd(Importance.Value),
            se   = sd / sqrt(N))
##PLot!
colors <- brewer.pal(n = 6, name = "RdYlBu")

ggplot(under.comb.sum, aes(y=mean, x=comb, fill=Species)) + 
  geom_bar(stat="identity", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("Mean Importance Value")+
  xlab("Combined Fire Severity")+
  labs(title = "Mean Understory Importance Values")+
  theme(axis.text.x = element_text(angle = 45), panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())

#####################################

#####base R Plotting
par(mfrow = c(2,1))
par(mar=c(4,4.1,2.5,1.6))
plot(under.import$ABCO~under.import$rdnbr.combined, ylab = "Mean Importance Value", xlab = "", applcol=c("blue", "green", "yellow", "red"), main = "ABCO Midstory IV's")

plot(under.import$QUKE~under.import$rdnbr.combined, ylab = "Mean Importance Value", xlab = "Combined fire severity", xlab = "", col=c("blue", "green", "yellow", "red"), main = "QUKE Midstory Iv's")

plot(under.import$PIPO~under.import$rdnbr.combined, ylab = "Mean Importance Value", xlab = "", col=c("blue", "green", "yellow", "red"), main = "PIPO Midstory Iv's")

plot(under.import$PSME~under.import$rdnbr.combined, ylab = "Mean Importance Value", xlab = "Combined fire severity", xlab = "", col=c("blue", "green", "yellow", "red"), main = "PSME Midstory Iv's", ylim=c(0,100))


