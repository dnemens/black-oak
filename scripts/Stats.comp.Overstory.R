#STATS
library(tidyverse)

#Univariate

#load file of overstory importance values
imps <- read.csv(file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv")

#are data normal? 
qqnorm(all.import$ABCO)
qqline(all.import$ABCO, col = "red")


#performs Mann-whitney test
#paired test
abco.test <- wilcox.test(all.import$ABCO, all.import$ABCO.1)
abco.test

abco.test2 <- wilcox.test(all.import$ABCO.1, all.import$ABCO.2)
abco.test2


########################################################################
####multivariate
library(vegan)
library(ggrepel)

### PREP
#create dataframe of abundances (basal area) for each stage (pre-postS-postC)

comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0

comp <- comp %>%
  rename(diam = dbh, stump = snag_stump..diam) %>%
  mutate(dbh = (diam + stump))

#load csv with predictor variables
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

#imports summarized QUKE clump (FO) data 
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
qukes <- qukes %>%
  mutate(Sto.dbh = ((diam.storrie.live+diam.storrie.dead)*clumps))

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
  mutate(Chips.dbh = ((diam.storrie.live+diam.chips)*clumps))

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

par(mfrow=c(1,3))
plot(pro1, main = "Change pre-fire to post-Storrie Fire", xlab = "", ylab ="", xaxt = "n", yaxt = "n", ar.col = "green4")
text(.175, .21, label = bquote(m^2 == 0.78), cex = 1.3)
text(.175, .185, label = "Sig = .001", cex = 1.3)
plot(pro2, main = "Change post-Storrie Fire to post-Chips Fire", xlab = "", ylab ="", xaxt = "n", yaxt = "n", ar.col = "darkmagenta")
text(.09,.18, label = bquote(m^2 == 0.73), cex = 1.3)
text(.09,.16, label = "Sig = .001", cex = 1.3)
plot(pro3, main = "Total change pre-fire to post-Chips Fire", xlab = "", ylab ="", xaxt = "n", yaxt = "n", ar.col = "red")
text(.17,.20, label = expression("m"^"2"*" = 0.89"), cex = 1.3)
text(.17,.175, label = "Sig = .001", cex = 1.3)

###########################
#visualize Ordinations
#create data frames of scores for each NMDS
gp1 <- data.frame(scores(z1))
gp2 <- data.frame(scores(z2))
gp3 <- data.frame(scores(z3))

#extract spp scores
sp1 <- as.data.frame(wascores(x = z1$points, w = pre.treesR, expand = TRUE))
sp1$fire = as.factor(c(1,1,2,2,1,2))
sp2 <- as.data.frame(wascores(x = z2$points, w = pS.treesR, expand = TRUE))
sp2$fire = as.factor(c(1,1,2,2,1,2))
sp3 <- as.data.frame(wascores(x = z3$points, w = pC.treesR, expand = TRUE))
sp3$fire = as.factor(c(1,1,2,2,1,2))
#############################

#plot 
a <- ggplot ()+
  geom_point(data=gp1, aes(x=NMDS1, y=NMDS2), colour = "green4", alpha = .5, size = 2)+
   geom_text(data= sp1, aes(x=MDS1, y=MDS2, label=rownames(sp1), colour = fire), fontface = "bold")+
  labs(title="Pre-Fire")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(1,.15), legend.justification = c(1.1,.9), legend.title = element_blank())+
  scale_color_manual(values = c("red", "blue"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  annotate(geom = "text", 1.5, 1, label = "Stress = 0.12", size = 5, hjust = 1)+
  coord_cartesian(ylim = c(-1,1.05))
b <- ggplot()+
  geom_point(data=gp2, aes(NMDS1, NMDS2), colour = "darkmagenta", size = 2, alpha = .5)+
  geom_text(data= sp2, aes(x=MDS1, y=MDS2, label=rownames(sp2), colour = fire), fontface = "bold")+
  labs(title="Post-Storrie Fire")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "")+
  scale_color_manual(values = c("red", "blue"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  annotate(geom = "text", 1.5, 1, label = "Stress = 0.08", size = 5, hjust = 1)+
  coord_cartesian(ylim = c(-1,1.05))
c <- ggplot()+
  geom_point(data=gp3, aes(NMDS1, NMDS2), colour = "red", size = 2, alpha = .5)+
  geom_text_repel(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3), colour = fire), fontface = "bold", force = .01)+
  #geom_text(data= sp3, aes(x=MDS1, y=MDS2, label=rownames(sp3)), fontface = "bold")+
  labs(title = "Post-Chips Fire")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "")+
  scale_color_manual(values = c("red", "blue"), labels = c("Fire Sensitive", "Fire Tolerant"))+
  annotate(geom = "text", 1.5, 1, label = "Stress = 0.05", size = 5, hjust = 1)+
  coord_cartesian(ylim = c(-1,1.05))

library(gridExtra)
grid.arrange(a,b,c, ncol=3, nrow=1)
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

##############
#Plot
library(directlabels)

PS <- ggplot(gp2, aes(x=NMDS1, y=NMDS2))  +
  geom_point(aes(), alpha = .3, size=2)+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(1,0), legend.justification = c(1,0))+
  #coord_cartesian(xlim = c(-1.5,1.1), ylim = c(-1.5,1.5))+
  stat_contour(data = Sordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  labs(colour="Fire severity\n(RdNBR)", title= expression("Post-Storrie Fire (r"^"2"*"=0.49)"))+
  theme(plot.title = element_text(hjust=.5, size=15))
 
PostSto <- direct.label(PS, method = "top.points")
#direct.label(PS, list(cex = 1.5, "far.from.others.borders", "calc.boxes", "enlarge.box", 
 #                    box.color = NA, fill = "transparent", "draw.rects"))

PC <- ggplot(gp3, aes(x=NMDS1, y=NMDS2))  +
  geom_point(alpha = .3, size=2)+
  theme(panel.grid = element_blank(), legend.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), legend.position = c(1,0), legend.justification = c(1,0))+
  #coord_cartesian(xlim = c(-1.5,1.1), ylim = c(-1.5,1.5))+
  stat_contour(data = Cordi.mite.na, aes(x = x, y = y, z = z, colour = ..level..), cex=1.1)+
  scale_color_gradient(low = "green", high ="red")+
  labs(colour="Fire severity\n(RdNBR)", title=expression("Post-Chips Fire (r"^"2"*"=0.34)"))+
  theme(plot.title = element_text(hjust=.5, size=15), legend.position = "none")

PostChip <- direct.label(PC, method = "top.points")
 
grid.arrange(PostSto, PostChip, ncol = 2)

##############
###Mantel Tests
man1 <- mantel(pre.trees.dist, pS.trees.dist)
man1
man2 <- mantel(pC.trees.dist, pS.trees.dist)
man2
man3 <- mantel(pre.trees.dist, pC.trees.dist)
man3

