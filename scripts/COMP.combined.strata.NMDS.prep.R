#preps combined overstory and midstory data for nmds

library(labdsv)
library(tidyverse)
library(vegan)
library(RColorBrewer)

############################################################
#import raw overstory data 
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0

comp <- comp %>%
  rename(diam = dbh, stump = snag_stump..diam) %>%
  mutate(dbh = (diam + stump))

comp <- comp %>%
  select(plot, Spp, dbh, fire.hist)

#imports summarized QUKE clump (FO) data 
qukes <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv")
#################### Pre-Fire
#creates a data frame of pre-fire values
pre.trees <- comp %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  group_by(plot, Spp) %>%
  summarize(dbh = sum(dbh)) %>%
  filter(Spp %in% c("ABCO", "CADE", "PILA", "PIPO", "PSME", "QUKE")) 

#transposes rows to columns
pre.trees <- spread(pre.trees, key = "Spp", value = "dbh", fill = 0.0)

#remove plot column
pre.trees <- pre.trees[2:7]

pre.trees <- pre.trees %>%
  rename(ABCO.t = ABCO, CADE.t=CADE, PILA.t = PILA, PIPO.t = PIPO, PSME.t = PSME, QUKE.t=QUKE)

#Creates distance matrix, standardized by Species max, then plot total (propotional contribution of each species)   
pre.treesR <- vegdist(wisconsin(pre.trees), method = "bray")

#################### Post-storrie
#creates a data frame of post-Storrie Fire values
pS.trees <- comp %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "KSQ", "SC")) %>%
  group_by(plot, Spp) %>%
  summarize(dbh = sum(dbh)) %>%
  filter(Spp %in% c("ABCO", "CADE", "PILA", "PIPO", "PSME", "QUKE")) 

#Adds in basal diameters of Storrie sprouts from focal oak data - multiplied by num of clumps in each plot
qukes <- qukes %>%
  mutate(Sto.dbh = ((Sldiam+SDdiam)*clumps))

#filter out QUKE from other spps 
q <- filter(pS.trees, Spp=="QUKE")

#ADDS dbh of sprouts using extrapolated focal oak data
q$dbh= (qukes$Sto.dbh+q$dbh)

#remove QUKE data from rest of data
pS.trees <- pS.trees %>% filter(!Spp %in% "QUKE")

#re-merges new quke data with rest of data
pS.trees <- rbind(q, pS.trees)

#transposes rows to columns
pS.trees <- spread(pS.trees, key = "Spp", value = "dbh", fill = 0.0)

pS.trees <- pS.trees %>%
  rename(ABCO.t = ABCO, CADE.t=CADE, PILA.t = PILA, PIPO.t = PIPO, PSME.t = PSME, QUKE.t=QUKE)

#remove plot column
pS.trees <- pS.trees[2:7]

#add dummy variable for distance measure
pS.trees$dummy = (2.5)

#standardize by Species max, then plot total (propotional contribution of each species) 
pS.treesR <- vegdist(wisconsin(pS.trees), method = "bray")

#################### Post-chips
#creates a data frame of post-chips Fire values
pC.trees <- comp %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "KSQ", "KCQ", "SSKCQ", "SSKC")) %>%
  group_by(plot, Spp) %>%
  summarize(dbh = sum(dbh)) %>%
  filter(Spp %in% c("ABCO", "CADE", "PILA", "PIPO", "PSME", "QUKE")) 

#Adds in dbh of Storrie/Chips sprouts from focal oak data - multiplied by num of clumps in each plot
qukeC <- qukes %>%
  mutate(Chips.dbh = ((Sldiam+Cldiam)*clumps))

#filter out QUKE from other spps 
qC <- filter(pC.trees, Spp=="QUKE")

#ADDS dbh of sprouts using extrapolated focal oak data
qC$dbh= (qukeC$Chips.dbh+qC$dbh)

#remove QUKE data from rest of data
pC.trees <- pC.trees %>% filter(!Spp %in% "QUKE")

#re-merges new quke data with rest of data
pC.trees <- rbind(qC, pC.trees)

#transposes rows to columns
pC.trees <- spread(pC.trees, key = "Spp", value = "dbh", fill = 0.0)

pC.trees <- pC.trees %>%
  rename(ABCO.t = ABCO, CADE.t=CADE, PILA.t = PILA, PIPO.t = PIPO, PSME.t = PSME, QUKE.t=QUKE)

pC.trees <- pC.trees[2:7]

#relativize by Species max, then plot total (propotional contribution of each species)
#pC.treesR <- vegdist(wisconsin(pC.trees), method = "bray")

#relativize by plot total only, then create dist matrix
PC.trees.R <- decostand(pC.trees, "total")
#################

#import shrub data (center sub-plot)
center <- read.csv("C:/Users/dnemens/Dropbox/CBO/chaparral/center data/data sheets/center sub plot.csv", header = T)

#summarizes data sheet, giving total crown area per species for each plot
cover <- center %>%
  group_by(plot, Spp) %>%
  summarize(cover = sum(crown.area))

#transposes rows to columns
cover <- spread(cover, key = "Spp", value = "cover", fill = 0.0)

cover <- cover[,3:41]

#removes rare species
cover.C <- vegtab(taxa = cover, minval = (.05*nrow(cover)))

####
#combines Post-Chips trees with shrub layer
pC.ts <- data.frame(pC.trees, cover.C)

#relativize each species by maxima
pc.tsR <- decostand(pC.ts, "max")

############## NMDS
z <- metaMDS(pc.tsR, autotransform = F, k = 3, try=30, trymax = 75)
stressplot(z) 
z$stress
plot(z, display = "sites", type = "n")

#check goodness of fit
gof <- goodness(object = z)
plot(z, display = "sites", type = "none", main="GOF")
points(z, display = "sites", cex = 2*gof/mean(gof))

#display spp scores
sp <- wascores(x = z$points, w = pc.tsR, expand = TRUE)
points(z, display = "sites", pch = 19, col = "grey50")
text(sp, rownames(sp), col = "red", font = 2, pos = 2, adj = 5)

#assess correlation between axes and variables
cor(pc.tsR, z$points)

#overlay continuous fire severity (rdnbr) for both fires
#load csv with predictor variables
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

S.ord <- ordisurf(z, rdnbr$storrie_rdnbr, bubble = 4, main = "storrie RdNBR")
summary(S.ord)

C.ord <- ordisurf(z, rdnbr$chips_rdnbr, bubble = 4, main = "Chips RdNBR")
summary(C.ord)

######### GGPLOT version
##overlays severity values onto ordination####

#storrie severity
ordiSto <- ordisurf(z, rdnbr$storrie_rdnbr, main="Burn severity")
#extracts variables from ordisurf for plotting in ggplot
Sordi.grid <- ordiSto$grid #extracts the ordisurf object
Sordi.mite <- expand.grid(x = Sordi.grid$x, y = Sordi.grid$y) #get x and ys
Sordi.mite$z <- as.vector(Sordi.grid$z) #unravel the matrix for the z scores
Sordi.mite.na <- data.frame(na.omit(Sordi.mite)) #gets rid of the nas

#chips severity 
ordiChip <- ordisurf(z, rdnbr$chips_rdnbr, main="Burn severity")
#extracts variables from ordisurf for plotting in ggplot
Cordi.grid <- ordiChip$grid #extracts the ordisurf object
Cordi.mite <- expand.grid(x = Cordi.grid$x, y = Cordi.grid$y) #get x and ys
Cordi.mite$z <- as.vector(Cordi.grid$z) #unravel the matrix for the z scores
Cordi.mite.na <- data.frame(na.omit(Cordi.mite)) #gets rid of the nas

##########################
#ggplot
library(directlabels)
library(ggrepel)
library(grid)
library(gridExtra)

gz <- data.frame(scores(z), rdnbr$storrie_rdnbr, rdnbr$chips_rdnbr)
sp <- data.frame(sp)
spp <- rownames(sp)

#compare gam vs. ordisurf
mod <- gam(rdnbr$storrie_rdnbr~s(gz$NMDS1, gz$NMDS2))
summary(mod)
mod2 <- gam(rdnbr$chips_rdnbr~s(gz$NMDS1, gz$NMDS2), method = "REML")
summary(mod2)

sp$fire = as.factor(c(1,1,2,2,1,2,2,2,2,2,2,1,2,2,2,1,1,2))

# Create a text grob <- grobTree(textGrob("Scatter plot", x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=13, fontface="italic")))
# Plot sp2 + annotation_custom(grob)
 # 
#######################
#OVERLAYS RdNBR FOR EACH FIRE ON ORDINATION OF COMBINED STRATA  
#STORRIE
a <- ggplot(gz, aes(x=NMDS1, y=NMDS2))  +
  geom_point(aes(), alpha = .3, size=2)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill="white", colour = "black"),  axis.ticks = element_blank(), axis.text = element_blank(),legend.position = c(1,0), legend.justification = c(1,0))+
  coord_cartesian(xlim = c(-1.6, 1.2), ylim = c(-1.6, 1.5))+
  stat_contour(data = Sordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  #labs(colour="Fire severity\n(RdNBR)", title= expression("Storrie Fire (r"^"2"*"=0.32)"))+
  theme(plot.title = element_text(hjust=.5, size=15))+
  annotate(geom = "text", -1.7, .2, label ="Mixed -\nconifer \nForest", colour = "green4", size = 7, fontface = "bold", hjust = 0)+
  annotate(geom = "text", 1.1, 1.5, label ="a)", size = 6, fontface = "bold")+
  annotate(geom = "text", 1.15, .2, label ="Oak -\nshrub", colour = "green4", size = 7, fontface = "bold")
Sto <- direct.label(a, "top.points")

#CHIPS
b <- ggplot(gz, aes(x=NMDS1, y=NMDS2))  +
  geom_point(alpha = .3, size=2)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill="white", colour = "black"), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(1,0), legend.justification = c(1,0))+
  coord_cartesian(xlim = c(-1.6, 1.2), ylim = c(-1.6,1.5))+
  stat_contour(data = Cordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  #labs(colour="Fire severity\n(RdNBR)", title=expression("Chips Fire (r"^"2"*"=0.35)"))+
  theme(plot.title = element_text(hjust=.5, size=15), legend.position = "none")+
  annotate(geom = "text", -1.7, .2, label ="Mixed -\nconifer \nForest", colour = "green4", size = 7, fontface = "bold", hjust = 0)+
  annotate(geom = "text", 1.15, .2, label ="Oak -\nshrub", colour = "green4", size = 7, fontface = "bold")+
  annotate(geom = "text", 1.1, 1.5, label ="b)", size = 6, fontface = "bold")

Chip <- direct.label(b, "top.points")

#SPECIES CENTOIDS AFTER CHIPS                     
c <- 
  ggplot()  +
  geom_point(data=gz, aes(x=NMDS1, y=NMDS2), alpha = .3, size = 2)+
  #geom_text(data= sp, aes(x=MDS1, y=MDS2, label=spp), colour = "red", position=position_jitter(width=.08, height = .08))+
  geom_text_repel(data= sp, aes(x=MDS1, y=MDS2, label=rownames(sp), colour=fire), fontface = "bold", force = .01, size=8)+
  theme(panel.grid = element_blank(), legend.title = element_blank(), legend.text = element_text(size=12), panel.background = element_rect(fill="white", colour = "black"), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(.01,.99), legend.justification = c(0,1))+
  #Position legend in graph, where x,y is 0,0 (bottom left) to 1,1 (top right)
  coord_cartesian(xlim = c(-1.6, 1.2), ylim = c(-1.6,1.5))+
  scale_color_manual(values = c("blue", "red"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  annotate(geom = "text", -1.7, .2, label ="Mixed -\nconifer \nForest", colour = "green4", size = 7, fontface = "bold", hjust = 0)+
  annotate(geom = "text", 1.15, .2, label ="Oak -\nshrub", colour = "green4", size = 7, fontface = "bold")+
  annotate(geom = "text", 1.1, 1.5, label ="c)", size = 6, fontface = "bold")

#creates uniform widths for plots
#Sto <- ggplot_gtable(ggplot_build(Sto))
#maxWidth = unit.pmax(Chip$widths[5:3], Sto$widths[5:3])
#Chip$widths[5:3] <- maxWidth
#Sto$widths[5:3] <- maxWidth

#stack both ggplots
all <- grid.arrange(Sto, Chip, c, nrow=1, ncol=3)

setwd("/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(all, filename = "combined.strata.nmds.tiff", dpi = 300, width = 20, height = 7.5)

