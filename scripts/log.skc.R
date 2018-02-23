#makes plots comparing mortality of Storrie sprouts and conifer saplings in Chips Fire
sprout <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/sprout.response.csv")

#subset out plots with no storrie sprouts
sBAkc <- sprout$per.Storrie.sprout.BA.killed.Chips #these plots did not have oaks that sprouted in Storrie
sprout.log <- subset(sprout, sBAkc!="NA") #subsets out plots without NA's in storrie sprout survival
skc <- (sprout.log$per.Storrie.num.killed.Chips)

#Turn Storrie sprout variables into logistic terms in order to predict topkill of Storrie sprouts in Chips with logistic regression
logis.skc <- (ifelse(skc>.7, 1,0)) #all clumps with more than 70% mortality are considered dead
chipr.skc <- sprout.log$chips_rdnbr

#logistic regression of Storrie sprout mortality
logreg <- glm(logis.skc~chipr.skc, data=sprout.log, family=binomial) #with 1, 0's created from response data
summary (logreg)

#some diagnostics
#Chisq test
anova (logreg, test="Chisq")

#Pseudo R2
library(rcompanion)
nagelkerke(logreg)

#ROC curve
library(pROC)
roc(chipr.skc, logis.skc)
#################################################################
#Sapling suvival
#loads center sub-plot data
center <- read.csv("C:/Users/dnemens/Downloads/center sub plot.csv", header=TRUE)
center <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/center sub plot.csv")
center[is.na(center)] <-  0 #replaces missing data with 0's

#loads severity file
rdnbr <- read.csv("C:/Users/dnemens/Downloads/rdnbr.csv",na.strings=c(""))
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")
rdnbr[is.na(rdnbr)] <-  0 #replaces missing data with 0's

library(dplyr)
#merges the two files using plot #, creates new dataframe "sap"
sap  <- merge(center, rdnbr, by="plot")
#selctes only saplings, only psme and abco
sap.sub <- filter(sap, ht>=3, Spp=="ABCO"|Spp=="PSME")
write.csv (sap.sub, "C:/Users/dnemens/Downloads/sapling3.csv")
#################################################################
#prepped for ggplot
dat <- read.csv("C:/Users/dnemens/Downloads/sapling3.csv", header = T)
dat <- read.csv("~/Grad School/LASSEN PROJECT/for R/sapling3.csv", header=T)
Spp <- dat$Spp

library(ggplot2)

#logistic regression plot of Storrie sprout survival by Chips RdNBR using ggplot
a  <-  ggplot(sprout.log, aes(chipr.skc, logis.skc)) + 
  theme_classic()+
  geom_vline(xintercept = 69)+
  geom_vline(xintercept = 315)+
  geom_vline(xintercept = 641)+
  stat_smooth (method="glm", se=F, method.args = list(family="binomial"), colour = "black") + 
  scale_x_continuous(breaks=seq(-500,999,125))+
  scale_y_continuous(breaks=seq(0,1,1), labels = c("Live", "Dead"))+
  theme(axis.title.x = element_blank())+
  theme (panel.border = element_rect(fill = NA))+
  geom_point (aes(y=logis.skc), size=4) +
  labs(y="Sprout clump status", x=element_blank()) + 
  geom_text(x=-400, y=1, label="P<0.0001", cex=4.5, colour="black") +
  theme(axis.title = element_text(size=20), axis.text = element_text(size=12, colour = "black"))+
  geom_text(x=40, y=.4, angle=90, label = "Unburned")+
  geom_text(x=285, y=.39, angle=90, label = "Low") +
  geom_text(x=615, y=.4, angle=90, label = "Moderate")+
  geom_text(x=920, y=.39, angle=90, label = "High") +
  labs(x="Chips fire severity (RdNBR)")+
  annotate("text", x=875, y=.9, label="a)", size=5)
  
#plot of chips rdnbr vs. sapling density  
b <-  ggplot(dat, aes(chips_rdnbr)) +
    scale_x_continuous(breaks=seq(-500,999,125))+
    coord_cartesian(xlim=c(-470, 900))+
    scale_y_continuous(limits = c(0,15.2), expand = c(0, 0)) +
    theme_classic()+
    theme (panel.border = element_rect(fill = NA))+
    geom_vline(xintercept = 69)+
    geom_vline(xintercept = 315)+
    geom_vline(xintercept = 641)+
    geom_text(x=40, y=8, angle=90, label = "Unburned")+
    geom_text(x=290, y=8, angle=90, label = "Low") +
    geom_text(x=615, y=8, angle=90, label = "Moderate")+
    geom_text(x=930, y=8, angle=90, label = "High") +
    geom_histogram(aes(fill=Spp), colour="black", position = "dodge") +
    scale_fill_manual(values=c("grey90", "black")) +
    labs(y="Sapling density", x="Chips fire severity (RdNBR)")+
    theme(axis.title = element_text(size=20), axis.text = element_text(size=12, color="black"), axis.title.x = element_text(margin=margin(t=18)), axis.title.y = element_text(margin=margin(r=22)))+
    theme(legend.position = c(.09,.85), legend.title = element_blank(), legend.text = element_text(size=12), legend.background = element_blank())+
    annotate("text", x=875, y=13, label="b)", size=5)  
  

library(grid)
library(gridExtra)
  
#creates uniform widths for plots
a <- ggplot_gtable(ggplot_build(a))
maxWidth = unit.pmax(b$widths[2:3], a$widths[2:3])
b$widths[2:3] <- maxWidth
a$widths[2:3] <- maxWidth

#stack both ggplots
grid.arrange(a, b, nrow=2, ncol=1)

##extra code#############
#gA <- ggplotGrob(a)
#gB <- ggplotGrob(b)
#grid::grid.newpage()
#stacks both plots
#grid::grid.draw(rbind(gB, gA))

