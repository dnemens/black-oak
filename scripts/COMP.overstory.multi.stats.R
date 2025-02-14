#multivariate stats and some plots for analysis of changes in overstory species abundance between fires

library(vegan)
library(ggrepel)
library(tidyverse) 
library(directlabels)
library(gridExtra)

### PREP
#create dataframe of abundances (basal area) for each stage (pre-postS-postC)

comp <- read.csv ("C:/Users/debne/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0

comp <- comp %>%
  rename(diam = dbh, stump = snag_stump..diam) %>%
  mutate(dbh = (diam + stump))

#load csv with predictor variables
rdnbr <- read.csv ("C:/Users/debne/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

#imports summarized QUKE clump (FO) data 
qukes <- read.csv("C:/Users/debne/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv")
qukes <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/quke.clumps.csv")

### Pre-fire
pre.trees <- comp %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
  group_by(plot, Spp) %>%
  summarize(dbh = sum(dbh)) %>%
  filter(Spp %in% c("ABCO", "CADE", "PILA", "PIPO", "PSME", "QUKE")) 

#transposes rows to columns
pre.trees <- spread(pre.trees, key = "Spp", value = "dbh", fill = 0.0)

#remove plot column
pre.trees <- pre.trees[2:7]

#Relativize abundance values
pre.treesR <- decostand(pre.trees, method = "total")
pre.trees.dist <- vegdist(pre.treesR)

#################### Post-storrie
#creates a data frame of post-Storrie Fire values
pS.trees <- comp %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "KSQ", "SC")) %>%
  group_by(plot, Spp) %>%
  summarize(dbh = sum(dbh)) %>%
  filter(Spp %in% c("ABCO", "CADE", "PILA", "PIPO", "PSME", "QUKE")) 

#Adds in basal diameters of Storrie sprouts from focal oak data - multiplied by num of clumps in each plot
#calculate quke sprout diams
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
  mutate(QUKE = QUKE + .01)

pS.trees <- pS.trees[2:7]

#relativize by plot totals
pS.treesR <- decostand(pS.trees, method = "total")
pS.trees.dist <- vegdist(pS.treesR)
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

pC.trees <- pC.trees[2:7]

#relativize by plot totals
pC.treesR <- decostand(pC.trees, method = "total")

pC.trees.dist <- vegdist(pC.treesR)
##############

#conduct NMDS of each stage
z1 <- metaMDS(pre.treesR, autotransform = F, k = 3, try=30, trymax = 75)
z1$stress
stressplot(z1)
z2 <- metaMDS(pS.treesR, autotransform = F, k = 3, try=30, trymax = 75)
z2$stress
stressplot(z2)
z3 <- metaMDS(pC.treesR, autotransform = F, k = 3, try=30, trymax = 75)
z3$stress
stressplot(z3)

################
#Procrustes Analyses comparing ordinations of each 
#pro1 <- protest(pre.treesR, pS.treesR) #compares distance matrices
pro1 <- protest(z1, z2)  #compares ordinations
pro1
plot(pro1)
#pro2 <- protest(pS.trees.dist, pC.trees.dist)
pro2 <- protest(z2, z3)
pro2
plot(pro2)
#pro3 <- protest(pre.trees.dist, pC.trees.dist)
pro3 <- protest(z1, z3)
pro3
plot(pro3)

#PLOTS PROCRUSTES COMPARING ORDINATIONS OF EACH CONDITION  
par(mfrow=c(1,3))
#main = "Change pre-fire to post-Storrie Fire", 
plot(pro1, main = "", xlab = "", ylab ="", xaxt = "n", yaxt = "n", ar.col = "green4")
text(.175, .21, label = "f)", cex = 2, font = 2)
text(.175, .185, label = "Sig = .001", cex = 2)
text(.175, .165, label = bquote(m^2 == 0.78), cex = 2)

#main = "Change post-Storrie Fire to post-Chips Fire", 
plot(pro2, main = "", xlab = "", ylab ="", xaxt = "n", yaxt = "n", ar.col = "darkmagenta")
text(.09,.18, label = "g)", cex = 2, font = 2)
text(.09,.16, label = "Sig = .001", cex = 2)
text(.09,.14, label = bquote(m^2 == 0.73), cex = 2)

#main = "Total change pre-fire to post-Chips Fire", 
plot(pro3, main = "", xlab = "", ylab ="", xaxt = "n", yaxt = "n", ar.col = "grey20")
text(.17,.20, label = "h)", cex = 2, font = 2)
text(.17,.175, label = "Sig = .001", cex = 2)
text(.17,.155, label = expression("m"^"2"*" = 0.89"), cex = 2)

###########################
#visualize Ordinations
#create data frames of scores for each NMDS
gp1 <- data.frame(scores(z1))
gp2 <- data.frame(scores(z2))
gp3 <- data.frame(scores(z3))

#extract spp scores
sp1 <- as.data.frame(wascores(x = z1$points, w = pre.treesR, expand = TRUE))
sp1$fire = as.factor(c(1,1,2,2,2,3))
sp2 <- as.data.frame(wascores(x = z2$points, w = pS.treesR, expand = TRUE))
sp2$fire = as.factor(c(1,1,2,2,2,3))
sp3 <- as.data.frame(wascores(x = z3$points, w = pC.treesR, expand = TRUE))
sp3$fire = as.factor(c(1,1,2,2,2,3))

fire <- sp1$fire
fire2 <- sp3$fire
#############################
#PLOT ORDINATIONS OF OVERSTORY FOR EACH CONDITION 
#PRE-FIRE
a <- ggplot ()+
  geom_point(data=gp1, aes(x=NMDS1, y=NMDS2), alpha = .4, size = 2)+
  geom_text(data= sp1, aes(x=MDS1, y=MDS2, label=rownames(sp1), colour = fire), fontface = "bold", size = 8)+
  #labs(title="Pre-Fire")+
  #theme(plot.title = element_text(hjust = 0.5), legend.position = "")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(1,.25), legend.justification = c(1,.9), legend.title = element_blank(), legend.text = element_text(size=15))+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())+
  #scale_color_manual(values = c("black"))+
  scale_color_manual(values = c("black","darkorange", "red"), labels = c("Fire Sensitive", "Fire Tolerant", "Fire Tolerant (HS)"))+
  annotate(geom = "text", 1.5, 1, label = "a)", size = 6, hjust = 1, fontface=2) +
  #annotate(geom = "text", 1.5, 1, label = "Stress = 0.12", size = 5, hjust = 1)+
  coord_cartesian(ylim = c(-1,1.05))

#POST-STORRIE
b <- ggplot()+
  geom_point(data=gp2, aes(NMDS1, NMDS2), colour = "grey20", size = 2, alpha = .4)+
  geom_text(data= sp2, aes(x=MDS1, y=MDS2, label=rownames(sp2), colour = fire), fontface = "bold", size = 8)+
  #labs(title="Post-Storrie Fire")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "")+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())+
  scale_color_manual(values = c("black","darkorange", "red"), labels = c("Fire Sensitive", "Fire Tolerant", "Fire Tolerant (HS)"))+
  #scale_color_manual(values = c("black", "red"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  #annotate(geom = "text", 1.5, 1, label = "b)", size = 6, hjust = 1) +
  #annotate(geom = "text", 1.5, 1, label = "Stress = 0.08", size = 5, hjust = 1)+
  coord_cartesian(ylim = c(-1,1.05))

#POST-CHIPS
c <- ggplot()+
  geom_point(data=gp3, aes(NMDS1, NMDS2), colour = "grey20", size = 2, alpha = .4)+
  geom_text_repel(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3), colour = fire), fontface = "bold", force = .01, size = 8)+
  #geom_text(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3)), fontface = "bold")+
  #labs(title = "Post-Chips Fire")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "")+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())+
  scale_color_manual(values = c("black","darkorange", "red"), labels = c("Fire Sensitive", "Fire Tolerant", "Fire Tolerant (HS)"))+
 #scale_color_manual(values = c("black", "red"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  #annotate(geom = "text", 1.5, 1, label = "c)", size = 6, hjust = 1) +
  #annotate(geom = "text", 1.5, 1, label = "Stress = 0.05", size = 5, hjust = 1)+
  coord_cartesian(ylim = c(-1,1.05))

all3 <- grid.arrange(a,b,c, ncol=3, nrow=1)

setwd("/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(all3, filename = "overstory.nmds.3cols.tiff", dpi = 300, width = 15, height = 5.5)
#########################################################
##overlays severity values onto ordinations####
#prep

#storrie severity
ordiSto <- ordisurf(z2, rdnbr$storrie_rdnbr, main="Burn severity", bubble = 4)
summary(ordiSto)
#extracts variables from ordisurf for plotting in ggplot
Sordi.grid <- ordiSto$grid #extracts the ordisurf object
Sordi.mite <- expand.grid(x = Sordi.grid$x, y = Sordi.grid$y) #get x and ys
Sordi.mite$z <- as.vector(Sordi.grid$z) #unravel the matrix for the z scores
Sordi.mite.na <- data.frame(na.omit(Sordi.mite)) #gets rid of the nas

#chips severity 
ordiChip <- ordisurf(z3, rdnbr$chips_rdnbr, main="Burn severity", bubble = 4)
summary(ordiChip)
#extracts variables from ordisurf for plotting in ggplot
Cordi.grid <- ordiChip$grid #extracts the ordisurf object
Cordi.mite <- expand.grid(x = Cordi.grid$x, y = Cordi.grid$y) #get x and ys
Cordi.mite$z <- as.vector(Cordi.grid$z) #unravel the matrix for the z scores
Cordi.mite.na <- data.frame(na.omit(Cordi.mite)) #gets rid of the nas

#elevation on post-Chips ### no sig effect of elevation on species!
ordielevC <- ordisurf(z3, rdnbr$elev, main="Elevation", bubble = 4)
summary(ordielevC)
#extracts variables from ordisurf for plotting in ggplot
Ecordi.grid <- ordielevC$grid #extracts the ordisurf object
Ecordi.mite <- expand.grid(x = Ecordi.grid$x, y = Ecordi.grid$y) #get x and ys
Ecordi.mite$z <- as.vector(Ecordi.grid$z) #unravel the matrix for the z scores
Ecordi.mite.na <- data.frame(na.omit(Ecordi.mite)) #gets rid of the nas

#Plots elevations onto post-Chip ordination
ElevC <- ggplot(gp3, aes(x=NMDS1, y=NMDS2))  +
  geom_point(alpha = .4, size=2)+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(1,0), legend.justification = c(1,0))+
  stat_contour(data = Ecordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "yellow", high ="red")+
  geom_text(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3)), colour=c("black", "black", "darkorange", "darkorange", "darkorange", "red"), fontface = "bold", size = 8)+
   theme(plot.title = element_text(hjust=.5, size=15), legend.position = "none")

ElevC_lab <- direct.label(ElevC, method = "top.points")

##############
#PlotS OVERSTORY ORDINATIONS WITH OVERLAID SEVERITY OF EACH FIRE (RdNBR)

PS <- ggplot(gp2, aes(x=NMDS1, y=NMDS2))  +
  geom_point(aes(), alpha = .4, size=2)+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(1,0), legend.justification = c(1,0))+
   #coord_cartesian(xlim = c(-1.5,1.1), ylim = c(-1.5,1.5))+
  stat_contour(data = Sordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  #geom_text(data= sp2, aes(x=MDS1, y=MDS2, label=rownames(sp2)), colour=c("black"), fontface = "bold", size = 8)+
  geom_text(data= sp2, aes(x=MDS1, y=MDS2, label=rownames(sp2)), colour=c("black", "black", "darkorange", "darkorange", "darkorange", "red"), fontface = "bold", size = 8)+
   annotate(geom = "text", 1.5, 1.1, label = "b)", size = 6, hjust = 1, fontface=2) +
  #text(1.5, 1, label = bquote(r^2==0.49))+
  #labs(colour="Fire severity\n(RdNBR)", title= expression("Post-Storrie Fire (r"^"2"*"=0.49)"))+
  theme(plot.title = element_text(hjust=.5, size=15), legend.position = "none")

PostSto <- direct.label(PS, method = "top.points")
#direct.label(PS, list(cex = 1.5, "far.from.others.borders", "calc.boxes", "enlarge.box", 
#                    box.color = NA, fill = "transparent", "draw.rects"))

PC <- ggplot(gp3, aes(x=NMDS1, y=NMDS2))  +
  geom_point(alpha = .4, size=2)+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(1,0), legend.justification = c(1,0))+
  #coord_cartesian(xlim = c(-1.5,1.1), ylim = c(-1.5,1.5))+
  stat_contour(data = Cordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  #geom_text(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3)), colour=c("black"), fontface = "bold", size = 8)+
  geom_text(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3)), colour=c("black", "black", "darkorange", "darkorange", "darkorange", "red"), fontface = "bold", size = 8)+
  #scale_color_manual(values = c("blue", "red"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  annotate(geom = "text", 1.5, 1.1, label = "c)", size = 6, hjust = 1, fontface=2) +
  #labs(colour="Fire severity\n(RdNBR)", title=expression("Post-Chips Fire (r"^"2"*"=0.34)"))+
  theme(plot.title = element_text(hjust=.5, size=15), legend.position = "none")

PostChip <- direct.label(PC, method = "top.points")

both <- grid.arrange(PostSto, PostChip, ncol = 2)

#allplots <- grid.arrange(a, PS, PC, ncol = 3)
allplots.sev <- grid.arrange(a, PostSto, PostChip, ncol = 3)

setwd("/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(allplots, filename = "overstory.nmds.tiff", dpi = 300, width = 15, height = 5.5)
ggsave(allplots.sev, filename = "overstory.with.severity.nmds.tiff", dpi = 300, width = 15, height = 5.5)
ggsave(both, filename = "overstory.with.severity2.nmds.tiff", dpi = 300, width = 15, height = 5.5)
