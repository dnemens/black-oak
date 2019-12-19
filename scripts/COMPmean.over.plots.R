#Plots of mean overstory IV's 

library(tidyverse)
library(RColorBrewer)
library(gridExtra) 

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
#colors <- c('#fafac4', '#f9f17f', '#fd8d3c','#d94801','#86131f','#430a0f') #sequential, greyscaleable
colorsG <- ColToGray(colors) #converts to greyscale

pre <- ggplot(import.pre, aes(y=mean, x=Storrie, fill=Species)) + 
  geom_bar (stat= "summary", fun.y = "mean", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = .4)+
  scale_fill_manual(values= rev(colors), name = "Species")+
  ylab("Importance Value")+
  xlab("Storrie Severity")+
  #labs(title = "Mean Pre-fire Importance Values")+
  theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
  theme(legend.position = c(.5, .9), legend.title = element_blank(), legend.text = element_text(size = 15), legend.direction  = "horizontal")+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, direction = "horizontal"))+
  scale_y_continuous(limits = c(0,315))+
  theme(plot.margin = margin(0, 1, 6, 1, "pt"))+
  annotate(geom = "text", 4.2, 300, label = "a)", size = 7, fontface="bold")

postS <- ggplot(import.postS, aes(y=mean, x=Storrie, fill=Species)) + 
  geom_bar (stat= "summary", fun.y = "mean", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = .4)+
  scale_fill_manual(values= rev(colors), name = "Species")+
  ylab("Importance Value")+
  xlab("Storrie Severity")+
  #labs(title = "Mean Post-Storrie Importance Values")+
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5), legend.position = 0, axis.ticks.x = element_blank(), axis.title = element_text(size = 25), axis.text = element_text(size = 20))+
    scale_y_continuous(limits = c(0,315))+
  theme(plot.margin = margin(0, 1, 5, 1, "pt"))+
  annotate(geom = "text", 4.2, 300, label = "b)", size = 7, fontface="bold")

two <- grid.arrange(pre, postS, nrow = 2)

setwd("C:/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(two, filename = "meanIVstop2.leg.tiff", dpi = 300, width = 10, height = 10)

postC <- ggplot(import.postC, aes(y=mean, x=comb, fill=Species)) + 
  geom_bar (stat= "summary", fun.y = "mean", width = .5, position = position_dodge(.7))+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.7), width = .4)+
  scale_fill_manual(values= rev(colors), name = "Species")+
  ylab("Importance Value")+
  xlab("Fire Severity")+
  #labs(title = "Mean Post-Chips Importance Values", hjust = .5)+
  theme(axis.text.x = element_text(angle = 45), legend.position = 0, panel.grid = element_blank(), plot.title = element_text(hjust = 0.5), panel.background = element_blank(), axis.ticks.x = element_blank(), axis.title = element_text(face = 2, size = 25), axis.text = element_text(size = 20))+
  theme(plot.margin = margin(0, 0, 0, 1, "pt"))+
  theme(axis.title.x = element_text(margin = margin(t = -20, r = 0, b = 0, l = 0)))
  #annotate(geom = "text", .8, 300, label = "c)", size = 5)

all <- grid.arrange(pre, postS, postC, nrow = 3)

setwd("/Users/debne/Dropbox/CBO/black-oak/plots")
ggsave(all, filename = "meanIVs.tiff", dpi = 300, width = 12, height = 15)


########################################################################
#Pre-fire plot with all sev's combined
#Prep for error bars
import.sum3 <- import.comb %>%
  group_by(Species, time) %>%
  summarise(N = length(Importance.Value),
            mean = mean(Importance.Value),
            sd   = sd(Importance.Value),
            se   = sd / sqrt(N))

import.pre3 <- subset(import.sum3, time == "Pre-fire")

pre2 <- ggplot(import.pre3, aes(y=mean, x=Species, fill=Species)) + 
  geom_bar (stat= "summary", fun.y = "mean", position = "dodge")+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(.9), width = .4)+
  scale_fill_manual(values= rev(colors), name = "Species", labels = c("White fir", "Incense-cedar", "Sugar pine", "Ponderosa", "Douglas-fir", "Black oak"))+
  ylab("Importance Value")+
  #labs(title = "Mean Pre-fire Importance Values")+
  theme(panel.background = element_blank(), plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.title.y = element_text(size=25), axis.text.y = element_text(size=18), axis.text.x = element_blank(), legend.position = "null")+
  scale_y_continuous(limits = c(0,315))+ 
  theme(axis.text.x = element_blank(), legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=20), legend.key.height = unit(1.1, "cm"))+
  theme(plot.margin = margin(0, 1, 0, 3, "pt"))

setwd("/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(pre2, filename = "meanIVs.pre.tiff", dpi = 300, width = 7, height = 5)

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
