#extracts summary stats for shrub cover (mean, sd and SE)

midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/midstory.csv")

midstory <- midstory[, 1:10]
midstory <- na.omit(midstory)

sto <- as.factor(midstory$Storrie.Severity)
chip <- as.factor(midstory$Chips.severity)
cover <- (midstory$shrub.cover.midpoint)


###############################################################################
library(tidyverse)
mid <- midstory %>%
  group_by(Plot, spp)%>%
  summarize(mean=mean(shrub.cover.midpoint))

mid2 <- mid %>%
  group_by(Plot)%>%
  summarize(cov = sum(mean))

mid2 <- mid2 %>% 
  separate(Plot, c("Storrie", "Chips", "plot"), remove = F)

sto <- as.factor(mid2$Storrie)
chip <- as.factor(mid2$Chips)
cover <- (mid2$cov)

# shrub cover by storrie sev category
test <- aov(cover~sto)
anova(test)
TukeyHSD(test)

#shrub cover by chips sev category
test2 <- aov(cover~chip)
anova(test2)
TukeyHSD(test2)

#plots
boxplot(cover~sto)
title("Storrie")
boxplot(cover~chip)
title("Chips")

#check assumptions
shapiro.test(x = test2$residuals) #significant!  assumptions not met

#use non-parametric krusall-wallis test
kw1 <- kruskal.test(cover~sto)
print(kw1)
pairwise.wilcox.test(cover, sto, p.adjust.method = "BH")

kw2 <- kruskal.test(cover~chip)
print(kw2)
pairwise.wilcox.test(cover, chip, p.adjust.method = "BH")


#function for calculating SE
se <- function(x) sqrt(var(x)/length(x))

mean(cover[which(sto==1)])
mean(cover[which(sto==2)])
mean(cover[which(sto==3)])
mean(cover[which(sto==4)])
mean(cover[which(chip==4)])
mean(cover[which(chip==3)])
mean(cover[which(chip==2)])
mean(cover[which(chip==1)])