#Plots of mean overstory IV's 

library(tidyverse)
library(RColorBrewer)

#import data sheet -- long form mean importance values
import <-  read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.long.csv", header = T)

#Order time factor
import$time <- ordered(import$time, levels = c("Pre-fire", "Post-Storrie Fire", "Post-Chips Fire"))

#severity categories
import <- import %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = F) 

#reclassify severity factors as names vs. numbers
import$Storrie <- factor(import$Storrie, labels = c("Unburned", "Low", "Moderate", "High"), ordered = is.ordered(import$Storrie))
import$Chips <- factor(import$Chips, labels = c("Unburned", "Low", "Moderate", "High"), ordered = is.ordered(import$Chips))

#summarize basic stats by Storrie Severity
import.sum <- import %>%
        group_by(Species, time, Storrie) %>%
        summarise(N = length(Importance.Value),
               mean = mean(Importance.Value),
               sd   = sd(Importance.Value),
               se   = sd / sqrt(N))

sum1 <- data.frame(t(import.sum))
#save as file
write.csv(sum1, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/sum.over.Storrie.csv", row.names = F)


#summarize basic stats by combined Severity   
import.comb <- import %>%
  unite("comb", c(Storrie, Chips), sep = "/", remove = F)

import.sum2 <- import.comb %>%
  group_by(Species, time, comb, Storrie) %>%
  summarise(N = length(Importance.Value),
            mean = mean(Importance.Value),
            sd   = sd(Importance.Value),
            se   = sd / sqrt(N))

sum2 <- data.frame(t(import.sum))
#save as file
write.csv(sum2, file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/sum.over.combined.csv", row.names = F)

#### Stratify by stage
import.pre <- subset(import.sum, time == "Pre-fire")
import.postS <- subset(import.sum, time == "Post-Storrie Fire")
import.postC <- subset(import.sum2, time == "Post-Chips Fire")
import.postC$comb <- factor(import.postC$comb, levels = c("Unburned/Unburned", "Unburned/Low","Unburned/Moderate", "Unburned/High","Low/Unburned", "Low/Low", "Low/Moderate","Low/High","Moderate/Unburned", "Moderate/Low", "Moderate/Moderate", "Moderate/High",  "High/Unburned", "High/Low", "High/Moderate", "High/High"), ordered = is.ordered(import.postC$comb))

##PLot!
colors <- brewer.pal(n = 6, name = "RdBu")

pre <- ggplot(import.pre, aes(y=mean, x=Storrie, fill=Species)) + 
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("Importance Value")+
  xlab("Storrie Severity")+
  labs(title = "Mean Pre-fire Importance Values")+
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0,315))

postS <- ggplot(import.postS, aes(y=mean, x=Storrie, fill=Species)) + 
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("Importance Value")+
  xlab("Storrie Severity")+
  labs(title = "Mean Post-Storrie Importance Values")+
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0,315))

postC <- ggplot(import.postC, aes(y=mean, x=comb, fill=Species)) + 
  #facet_wrap(~Storrie)+
  geom_bar(stat="identity", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  ylab("Importance Value")+
  xlab("Combined Severity")+
  labs(title = "Mean Post-Chips Importance Values", hjust = .5)+
  theme(axis.text.x = element_text(angle = 45), legend.position = 0, panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank())

library(gridExtra)
grid.arrange(pre, postS, postC, nrow = 3)


#### histograms of diam distr
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0

comp <- comp %>%
  filter(Spp %in% c("ABCO","PILA","PIPO"))

colors <- brewer.pal(n = 6, name = "RdBu")

ggplot(comp, aes(x=dbh, fill=Spp)) + 
  geom_histogram(binwidth = 2)+
# facet_wrap(~Spp)+
  #geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = .4)+
  scale_fill_manual(values= colors, name = "Species")+
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(0,75))
