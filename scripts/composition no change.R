setwd("~/DEFENSE/analyses/")

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library (rcompanion)
library(MASS)
############################################################
#re-do using R to come up with #'s
comp <- read.csv("~/CBO manuscript/Data/data sheets/overstory.csv")
rdnbr <- read.csv("~/CBO manuscript/Data/data sheets/rdnbr.csv")

#calculates number of living pre-chips firs
pre.firs <- comp %>%
  filter(Spp == "ABCO" | Spp == "PSME") %>%
  group_by(plot, Spp) %>%
  filter(fire.hist %in% c("KC", "SC", "SSSC", "PLSKC", "PLSSC", "SSKC")) %>%
  summarize(count = n()) %>%
  group_by(plot) %>%
  summarize(pre.fir = sum(count)) %>%
  complete(plot)
  
#calculated number of living post-chips firs
post.firs <- comp %>%
  filter(Spp == "ABCO" | Spp == "PSME") %>%
  group_by(plot, Spp) %>%
  filter(fire.hist %in% c("SC", "SSSC", "PLSSC")) %>%
  summarize(count = n()) %>%
  group_by(plot) %>%
  summarize(post.fir = sum(count)) %>%
  complete(plot)

#sets post-chips non-existant firs to 0 (NA's ok for pre-chips firs in plots which had 100% mortality in Storrie Fire)
post.firs[is.na(post.firs)] <- 0

#creates single file for firs 
firs <- merge(pre.firs, post.firs, by = "plot", all.x = T)

#calculates number of living pre-chips oaks
pre.oaks <- comp %>%
  filter(Spp == "QUKE") %>%
  group_by(plot, Spp) %>%
  filter(fire.hist %in% c("KC", "SC", "SSSC", "SPSC", "SSKC", "KSSC", "KSKC")) %>%
  summarize(count = n()) %>%
  group_by(plot) %>%
  summarize(pre.oak = sum(count)) %>%
  complete(plot)

#calculates number of living post-chips oaks
post.oaks <- comp %>%
  filter(Spp == "QUKE") %>%
  group_by(plot, Spp) %>%
  filter(fire.hist %in% c("CP", "KSKC", "KSSC", "SC", "SSSC", "SPSC", "SSKC")) %>%
  summarize(count = n()) %>%
  group_by(plot) %>%
  summarize(post.oak = sum(count)) %>%
  complete(plot)

oaks <- merge(pre.oaks, post.oaks, by = "plot")

all <- merge(firs, oaks, by="plot")

dom <- all %>%
  mutate(pre.trees=pre.oak+ pre.fir, 
         post.trees=post.fir+ post.oak,
         prop.pre.fir = pre.fir/pre.trees,
         prop.pre.oak = pre.oak/pre.trees,
         prop.post.fir = post.fir/post.trees,
         prop.post.oak = post.oak/post.trees, 
         fir.change = prop.post.fir-prop.pre.fir,
         oak.change = prop.post.oak -prop.pre.oak)

dom <- merge(dom, rdnbr, by="plot")
dom <- na.omit(dom)


#creates variables from spreadsheet
#independent variables
chipr <- dom$chips_rdnbr
#response variables
conc <- dom$fir.change*100
qukec <- dom$oak.change*100

### a different way to look at data
#or use these response variables -- without calculating change in dominance
#independent variables
chipr <- dom$chips_rdnbr
#response variables
fir.post <- dom$prop.post.fir
oak.post <- dom$prop.post.oak
oak.pre <- dom$pre.oak

### rplot with both responses
plot(oak.post~chipr, pch=20)
points(oak.pre, pch=3)
points(dom$fir.change, pch=2)


#vectors for analysis 
chiprdnbr <- dom.df$chipr
conchange <- dom.df$conc
qukechange <- dom.df$qukec

#graphs the resulting points on the same graph
ggplot(dom.df, aes(chiprdnbr)) + 
  geom_point(aes(y=conchange), size=4, shape=17, color="red") + 
  geom_point(aes(y=qukechange), size=4, color="blue") + 
  geom_smooth (aes(y=conchange), span=2, color="red") + 
  geom_smooth (aes(y=qukechange), span=2, color="blue") + 
  #geom_smooth (aes(y=qukechange+12), method="glm", method.args = list(family="Gamma"), colour = "blue", fullrange = TRUE) +
  #geom_smooth (aes(y=conchange), method="glm", formula = y~I(x^2), colour = "red", fullrange = TRUE) +
    theme(legend.title=element_blank()) +  
  theme_classic()+
  theme(axis.title=element_text(size=20))+
  theme(axis.text = element_text(size=12))+
  geom_hline(aes(yintercept=0), colour="black")+
  labs(x="Chips Fire Severity (RdNBR)", y="Change in relative dominance (%)") + 
  ylim(-100, 100) +
  geom_text(x=-400, y=100  , label="Black oak", cex=6, color="blue", fontface="italic") +
  geom_text(x=-290, y=-90, label="White fir + Douglas fir", cex=6, color="red", fontface="italic") +
  geom_text(x=-400, y=90, label="R^2==0.43", parse=T, cex=5) + 
  geom_text(x=-400, y=-100, label="R^2==0.43", parse=T, cex=5) 

#regular r plot 
y1 <- predict(mod2, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")
plot(qukechange~chiprdnbr, pch=20, col="blue", cex=1.4, ylim=c(-20,100))
xx <- order(chiprdnbr)
points(chiprdnbr[xx], y1$fit[xx], type="l", lwd=3, col="red")
points(chiprdnbr[xx], y1$fit[xx]-1.96*y1$se.fit[xx], type="l", lwd=1, col=2)
points(chiprdnbr[xx], y1$fit[xx]+1.96*y1$se.fit[xx], type="l", lwd=1, col=2)
polygon(c(chiprdnbr[xx], rev(chiprdnbr[xx])), c(y1$fit[xx]-1.96*y1$se.fit[xx], rev(y1$fit[xx]+1.96*y1$se.fit[xx])), border="black", col=rgb(0,0,0,0.15))

#stats for each line
rmse <- function(x) {sqrt(mean(x^2))}

#trying linear model
mod1 <- quke.lm <- lm(qukechange+12~chiprdnbr) #r2=.5635, pval<.0001
summary (mod1)
plot(mod1)
AIC (mod1) #636
rmse(mod1$residuals) #18.2

library(MASS)
boxcox(lm((qukechange+12)~chiprdnbr))

#trying adding square term to model given results of boxcox
mod2 <- lm(qukechange-12~I(chiprdnbr^2)+chiprdnbr)
summary(mod2)
AIC(mod2) #632.37
rmse(mod2$residuals) #17.41
plot(mod2)
nagelkerke(mod2) #loglik =  -32.321

#model using Gamma distribution 
quke.glm <- glm((qukechange+12)~chiprdnbr, family = Gamma(link="log"))
summary (quke.glm) #AIC 606
rmse(quke.glm$residuals) #.02
plot (quke.glm)
nagelkerke(quke.glm) #loglik = -27.327

#how about a log transformation?
mod3 <- lm((sqrt(qukechange+12))~chiprdnbr)
summary(mod2)
AIC(mod2) #632
rmse(mod2$residuals) #17.4
plot(mod2)




con.lm <- lm((conchange)~chiprdnbr) #r2=.5028, pval<.0001
summary(con.lm)
plot (conchange~chiprdnbr)

con.glm <- glm((conchange+85)~chiprdnbr, family=Gamma)
summary(con.glm)
nagelkerke(quke.glm.p)
