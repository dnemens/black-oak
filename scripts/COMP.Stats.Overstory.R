#STATS
library(tidyverse)

#Univariate

#load file of overstory importance values
wide <- read.csv(file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv")

#load file of rdnbr vals
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

#are data normal? 
qqnorm(imps$ABCO)
qqline(imps$ABCO, col = "red")


#performs Mann-whitney test
#paired test
abco.test <- wilcox.test(all.import$ABCO, all.import$ABCO.1)
abco.test

abco.test2 <- wilcox.test(all.import$ABCO.1, all.import$ABCO.2)
abco.test2
###########
#test Difference between pre- and post-Storrie importance
imps <- wide %>%
  select(-contains(".2") )%>%
  separate(rdnbr.plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot, Chips)

timps <- imps %>%
  transmute(ABCO = ABCO.1 - ABCO, CADE= CADE.1-CADE, PILA=PILA.1-PILA, PIPO=PIPO.1-PIPO, PSME = PSME.1 - PSME,  QUKE = QUKE.1 - QUKE)

mimps <- data.frame(imps$Storrie, rdnbr$storrie_rdnbr, timps)
mimps <- rename(mimps, Storrie = imps.Storrie,  storR =rdnbr.storrie_rdnbr)

limps <- gather(mimps, key = "Species", value = "mean.val", 3:8)

ab <- filter(limps, Species == "ABCO")
amod <- aov(ab$mean.val~ab$Storrie)
summary(amod)
TukeyHSD(amod)

#########Plot this
library(RColorBrewer)
colors <- brewer.pal(n = 6, name = "RdBu")

ggplot(data=limps, aes(x=storR, y=mean.val, colour=Species))+
  geom_jitter(size = 4, width = 20, height = 10, alpha = .7)+
  ylab("delta Importance Value")+
  xlab("Fire severity (RdNBR)")+
  scale_colour_manual(values= colors, name = "Species")+
  coord_cartesian(ylim = c(-310,310))+
  geom_hline(yintercept=0)+
  theme_classic()


############
#ANoVA for importance values

#load file of overstory importance values
long <- read.csv(file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.long.csv")

###PREP
#break severity into categories
imps.long <- long %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot) %>%
  filter(time!="Post-Chips Fire")
  
#combine time with severity
imps.long <- imps.long %>%
  unite("time", c(Storrie, time), remove = T) %>%
  select(-Chips)

#imps.long <- gather(imps.long, key = "Fire", value = "severity", Storrie:Chips)

############
#one-way anova without acf 
#isolating each spp
abco <- imps.long %>%
  filter(Species=="ABCO") 

a.imp <- abco$Importance.Value
a.mod <- aov(a.imp ~ abco$time)
summary(a.mod)
histogram(residuals(a.mod))
plot(fitted(a.mod), residuals(a.mod))

abco.T <- TukeyHSD(a.mod) 
abco.T

require(lsmeans)
library(multcomp)

#QUKE
quke <- imps.long %>%
  filter(Species=="QUKE") 

q.imp <- quke$Importance.Value
q.mod <- aov(q.imp ~ quke$time)
summary(q.mod)
TukeyHSD(q.mod)

######### repeated measures ANOVA
library(nlme)

#determine autocorrelation structure
ab2 <- filter(long, Species == "ABCO" & time !="Post-Chips Fire")
mod.a = gls(Importance.Value ~ time,
              data=ab2)
ACF(mod.a,
    form = ~ 1 | plot)
