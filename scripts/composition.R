#conducts analysis and creates graph of compositional change in conifer and oak along Chips severity gradient

library(tidyverse)
library (rcompanion)
library(MASS)
############################################################
#re-do using R to come up with #'s
comp <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/overstory.csv")
#comp <- read.csv("D:/for R/overstory.raw.csv")
rdnbr <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

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


#creates variables from spreadsheet
#independent variables
chipr <- dom$chips_rdnbr
#response variables
conc <- dom$fir.change*100
qukec <- dom$oak.change*100

#new data frame removing NA's 
dom.df <- data.frame(chipr, conc, qukec)
dom.df <- na.omit(dom.df)

#vectors for analysis 
chiprdnbr <- dom.df$chipr
conchange <- dom.df$conc
qukechange <- dom.df$qukec

#graphs the resulting points on the same graph
ggplot(dom.df, aes(chiprdnbr)) + 
  geom_point(aes(y=conchange), shape=17, size=3) + 
  geom_point(aes(y=qukechange+12), size=3, shape=1) + 
  geom_smooth (aes(y=conchange), span=2, color="black") + 
  #geom_smooth (aes(y=(conchange+85)), method="glm", method.args = list(family=Gamma(link="log")), 
  #             colour = "black", fullrange = TRUE) +
  geom_smooth (aes(y=qukechange), span=2, color="black") + 
  #geom_smooth (aes(y=qukechange+12), method="glm", method.args = list(family=Gamma(link="log")), 
  #       colour = "red", fullrange = TRUE) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title=element_text(size=20, family="serif"))+
  theme(axis.text = element_text(size=12))+
  theme(axis.title.x = element_text(margin=margin(t=20)))+
  geom_hline(aes(yintercept=0), colour="black")+
  labs(x="Chips Fire Severity (RdNBR)", y="Change in relative dominance (%)") + 
  geom_text(x=-465, y=100, label="Black oak", cex=6, color="black", fontface="italic", family="serif", hjust=0) +
  geom_text(x=-465, y=-100, label="White fir + Douglas-fir", cex=6, color="black", fontface="italic", family="serif", hjust=0) 
  #geom_text(x=-480, y=85, label="R^2==0.59", parse=T, cex=5, family="serif", hjust=0) + 
  #geom_text(x=-480, y=-100, label="R^2==0.59", parse=T, cex=5, family="serif", hjust=0) 

setwd("C:/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave("composition.tiff", width=22, height=15, units="cm", device="tiff")

####################################################################################
#regular r plot with regression lines and conidence intervals
plot(qukechange~chiprdnbr, pch=20, col="black", cex=1.4, ylim=c(-103, 103))
points(conchange~chiprdnbr, pch=5)
abline(h=0)
xx <- order(chiprdnbr)
#adds regression lines
y1 <- predict(mod2, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")
y2 <- predict(mod3, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")
points(chiprdnbr[xx], y1$fit[xx]+12, type="l", lwd=3, col="black")
points(chiprdnbr[xx], y2$fit[xx]+12, type="l", lwd=3, col="black")
#points(chiprdnbr[xx], y1$fit[xx]-1.96*y1$se.fit[xx], type="l", lwd=1, col=2)
#points(chiprdnbr[xx], y1$fit[xx]+1.96*y1$se.fit[xx], type="l", lwd=1, col=2)
#adds confidence intervals
polygon(c(chiprdnbr[xx], rev(chiprdnbr[xx])), c((y1$fit[xx]-1.96*y1$se.fit[xx])-12, (rev(y1$fit[xx]+1.96*y1$se.fit[xx])))+12, border="black", col=rgb(0,0,0,0.15))
polygon(c(chiprdnbr[xx], rev(chiprdnbr[xx])), c(y2$fit[xx]-1.96*y2$se.fit[xx], rev(y2$fit[xx]+1.96*y2$se.fit[xx])), border="black", col=rgb(0,0,0,0.15))

#######################################################################################
#stats for each line
#funtion for root mean square error
rmse <- function(pred, obs) {sqrt(mean((pred-obs)^2))}

#trying linear model
mod1 <- quke.lm <- lm(qukechange+12~chiprdnbr) #r2=.5635, pval<.0001
summary (mod1)
plot(mod1)
AIC (mod1) #636
rmse(mod1$residuals) #18.2

library(MASS)
boxcox(lm((qukechange+12)~chiprdnbr))

#linear model with square term (given results of boxcox)
mod2 <- lm(qukechange+12~I(chiprdnbr^2)+chiprdnbr)
summary(mod2)

#mod2 diagnostics
AIC(mod2) #632.37
pred2 <- predict(mod2, type="response")
obs2 <- qukechange+12
rmse(pred=pred2, obs=obs2) #17.4
plot(mod2) 
nagelkerke(mod2) #loglik =  -34.321 cox&snell 0.6 -- crap?

###@#this turned out best of three using AIC
#model using Gamma distribution 
quke.glm <- glm((qukechange+12)~chiprdnbr, family = Gamma(link="log"))
summary (quke.glm) 

#diagnostics
AIC(quke.glm) #AIC 597
predg <- predict(quke.glm, type="response")
obsg <- qukechange+12
rmse(pred=predg, obs=obsg) #17.8
plot (quke.glm)
nagelkerke(quke.glm) #loglik = -32.358, cox&snell 0.59 -- this is prob crap
library(modEvA)
Dsquared(quke.glm, adjust = T)

#gam?
library(mgcv)
mod.gam <- gam((qukechange+12) ~ s(chiprdnbr, bs="tp", k=3), family=Gamma(link="log"))
summary (mod.gam)

gam.check(mod.gam)
AIC(mod2, quke.glm, mod.gam)
predgam <- predict(mod.gam, type="response")
obsgam <- qukechange+12
rmse(pred=predgam, obs=obsgam) #17.9
