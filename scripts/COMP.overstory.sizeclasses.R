#creates histograms of size classes for dominant tree species

library(tidyverse)
library(vegan)
library(RColorBrewer)

############################################################
#import overstory data
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.KSQ.KCQ.csv")
comp[is.na(comp)] <- 0  #do I want to do this?  

#combine snag/stump diams with dbh for pre-fire dbh's
comp <- comp %>%
  rename(diam = dbh, stump = snag_stump..diam) %>%
  mutate(dbh = (diam + stump))

##############  Pre-fire structure
#filter out all post-fire trees
pre <- comp %>%
    filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "U", "SC", "SPSC")) %>%
    filter(!Spp %in% c("PIMO", "ACMA", "QUCH")) %>%
    select(1:5, "dbh") %>%
    filter(dbh > 0)

pre.sum <- pre %>%
    group_by(plot, Spp) %>%
    summarize(n=n())  %>%
  mutate(tph = n*22.22) %>%
  mutate(n=NULL) %>%
  group_by(Spp) %>%
  summarize(mean.tph = mean(tph), sd.tph = sd(tph))

#################  Post-Storrie structure
postSto <- comp %>%
  filter(!fire.hist %in% c("CP", "PLSKC", "PLSSC", "KS", "U", "KSQ", "SC")) %>%
  filter(!Spp %in% c("PIMO", "ACMA", "QUCH")) %>%
  select(1:5, "dbh") %>%
  filter(dbh > 0)

#Add post-Storrie sprouts!!!


############# Post-Chips structure

postChi <- comp %>%
  filter(!fire.hist %in% c("PLSKC", "PLSSC", "KS", "U", "KC", "SSKC")) %>%
  filter(snag.class %in% c("", " ")) %>%
  filter(!Spp %in% c("PIMO", "ACMA", "QUCH", "CONU"))
 
length(which(postChi$Spp == "ABCO" & postChi$dbh>35))
################# Plots
library(grid)
library(gridExtra)

#creates uniform widths for plots
a <- ggplot_gtable(ggplot_build(a))
maxWidth = unit.pmax(b$widths[2:3], a$widths[2:3], c$widths[2:3])
b$widths[2:3] <- maxWidth
a$widths[2:3] <- maxWidth

colors <- brewer.pal(n = 6, name = "RdBu")

a <- ggplot(pre, aes(dbh, fill = Spp)) +
  geom_histogram(binwidth = 5, position = "dodge")+
  scale_fill_manual(values= colors, name = "Species")+
  xlab("Dbh (cm)")+
  #ylab("")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_x_continuous(breaks=seq(0, 150, 15))+
  coord_cartesian(xlim=c(0, 150), ylim = c(0,500))


b <- ggplot(postSto, aes(dbh, fill = Spp)) +
  geom_histogram(binwidth = 5, position = "dodge")+
  scale_fill_manual(values= colors, name = "Species")+
  xlab("Dbh (cm)")+
  ylab("")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  scale_x_continuous(breaks=seq(0, 150, 15))+
  coord_cartesian(xlim=c(0, 150), ylim = c(0,500))


c <- ggplot(postChi, aes(dbh, fill = Spp)) +
  geom_histogram(binwidth = 5, position = "dodge")+
  scale_fill_manual(values= colors, name = "Species")+
  xlab("Dbh (cm)")+
  ylab("")+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  scale_x_continuous(breaks=seq(0, 150, 15))+
  coord_cartesian(xlim=c(0, 150), ylim = c(0,500))

#stack both ggplots
grid.arrange(a, b, c, nrow=3, ncol=1)
