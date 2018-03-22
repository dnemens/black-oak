#evaluates chips sprout metrics (basal area and height) by chips Fire RdNBR
#includes plots with no sprouts in model!

#loads csv of sprout response data 
sprout <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/sprout.response.csv")

#loads nesessary packages
library(tidyverse)
library(MASS)
library(rcompanion)

#independent variables
plot <- (sprout$plot)
chiprdnbr <- (sprout$chips_rdnbr)

#######response variables######

#chips sprout basal area per clump
BAclump <- sprout$Chips.BA.clump
 
#chips sprout basal area per hectare
chipBAha <- (sprout$Chips.BA.ha)*22.22

#height of chips sprouts
height <- sprout$Max.of.oak.live.emer.ht

#########################################################
#analyses

#funtion for root mean square error
rmse <- function(pred, obs) {sqrt(mean((pred-obs)^2))}

### looking at model that includes plots with no chips sprouts
#gamma dist glm for chips sprout BA
mod <- glm ((BAclump+1)~chiprdnbr, family=Gamma(link="log"), control = list(maxit = 100))
summary(mod)

#glm diagnostics
pred1 <- predict(mod, type="response")
obs1 <- BAclump+1
rmse(pred=pred1, obs=obs1)
#how much deviance explained? 
1-(modg$deviance/modg$null)

#gam with Gamma dist
library(mgcv)
modg <- gam((BAclump+1) ~ s(chiprdnbr, bs="tp", k=7), family=Gamma(link="log"))
summary (modg) 

#model diagnostics
gam.check(modg)
predg <- predict(modg, type="response")
obsg <- BAclump+1
rmse(pred=predg, obs=obsg)
#how much deviance explained? 
1-(modg$deviance/modg$null)

#compare glm to gam with Chi-squared test
anova(mod, modg, test = "F")
AIC(mod, modg)

#prep for r plot
xx <- order(chiprdnbr)
yg <- predict(modg, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")

######################################################################
#chips sprout height model eval
par(mfrow = c(1, 2))
hist (chip.height)
qqnorm (chip.height)
qqline (chip.height)
dev.off()

#model of max sprout height including 0's 
#glm
mod2 <- glm((height+1)~chiprdnbr, family = Gamma(link = "log"), control = list(maxit=100))
summary (mod2)
#diagnostics
pred2 <- predict(mod2, type="response")
obs2 <- height+1
rmse(pred=pred2, obs=obs2)

#gam
mod2g <- gam((height+1) ~ s(chiprdnbr, bs="tp", k=5), family=Gamma(link="log"))
summary(mod2g)
gam.check(mod2g)
pred2g <- predict(mod2g, type="response")
obs2g <- height+1
rmse(pred=pred2g, obs=obs2g)

#compare glm to gam
anova(mod2, mod2g, test = "F")
AIC(mod2, mod2g)

##############################################
#plot with ggplot####
library(ggplot2)

#data frame for ggplot

dat <- data.frame(chiprdnbr, BAclump, height)

#plot of basal area 
a <- ggplot(dat, aes(chiprdnbr)) + 
  geom_point(aes(y=BAclump)) + 
  stat_smooth ((aes(y=BAclump+1)), method="gam", formula = y~s(x, bs="tp", k=7), se=T, 
          method.args = list(family=Gamma(link="log")), colour = "black", fullrange=T) + 
  #             colour = "black", fullrange=T) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title=element_text(size=20, family="serif"))+
  theme(axis.text = element_text(size=12, family="serif"))+
  theme(axis.title.y = element_text(margin = margin(l=-4, r=12)))+
  labs(x="", y=expression(paste(text="Basal area (cm"^{2}*")"))) + 
  coord_cartesian(ylim = c(0, 300))+
  annotate("text", x=-465, y=290, label="(a)", size=5, family="serif", fontface="bold")
  #annotate("text", x=-394, y=267, label="P < 0.0001", size=5, family="serif")
  #annotate("text", x=-415, y=245, label="r^2==0.44", size=5, parse=T, family="serif")

#plot of height
b <- ggplot(dat, aes(chiprdnbr)) + 
  geom_point(aes(y=height+1)) + 
  stat_smooth ((aes(y=height+1)), method="gam", formula = y~s(x, bs="tp", k=5), 
        method.args = list(family=Gamma(link="log")), se=T, colour = "black") + 
  theme_bw()+
  scale_y_continuous(labels = function(y)y-1)+ 
  coord_cartesian(ylim = c(1.1, 6))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(margin = margin(l=25)))+
  theme(axis.title.x = element_text(margin=margin(t=20)))+
  theme(axis.title=element_text(size=20, family="serif"))+
  theme(axis.text = element_text(size=12, family="serif"))+
  labs(x="Chips Fire Severity (RdNBR)", y="Height (m)") + 
  annotate("text", x=-465, y=5.9, label="(b)", size=5, family="serif", fontface="bold")
  #annotate("text", x=-394, y=5.5, label="P < 0.0001", size=5, family="serif")
  #annotate("text", x=-415, y=5.15, label="r^2==0.62", size=5, parse=T, family="serif")

#creates uniform widths for plots
library(gridExtra)
library(grid)
a <- ggplot_gtable(ggplot_build(a))
maxWidth = unit.pmax(b$widths[2:3], a$widths[2:3])
b$widths[2:3] <- maxWidth
a$widths[2:3] <- maxWidth

#stack both ggplots
c <- grid.arrange(a, b, nrow=2, ncol=1)

setwd("C:/Users/dnemens/Dropbox/CBO/black-oak/plots")
ggsave(plot=c, "sprout.metrics.gam.tiff", width=20, height=25, units="cm", device = "tiff")


#######################################################################
#regular r plotting####
#produces stacked sprout metric plots (basal area and height)

setwd("C:/Users/dnemens/Dropbox/CBO/black-oak/plots")

tiff(file="sprout.response3.tiff", width=720, height=900, bg="transparent")

par(mfrow=c(2,1), 
    oma = c(1.5,.5,.5,1),
    mar=c(3.9,5,.6,1))

#setup for confidence intervals BA plot
xx <- order(chiprdnbr)
y1 <- predict(modg, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")

#plot of Basal area vs. chips severity with regression line & confid intervals
plot(BAclump~chiprdnbr, ylab=("Basal area "~ (cm^2/clump)), xlab=" ", pch=19, xlim=c(-500, 1010), cex.lab=1.5, cex.axis=1.2)
#adds regression line based on prediction from glm model
points(chiprdnbr[xx], y1$fit[xx], type="l", lwd=2)
#confidence intervals
polygon(c(chiprdnbr[xx], rev(chiprdnbr[xx])), c(y1$fit[xx]-1.96*y1$se.fit[xx], rev(y1$fit[xx]+1.96*y1$se.fit[xx])), border="black", col=rgb(0,0,0,.2))
#adds text
rsq1 <- expression(R^2==0.59)
text(-420, 250, "(a)", cex=1.5)
text(-420, 230, "P<0.0001", cex=1.2)
text(-420, 210, rsq1, cex=1.2)

#set up for conifedence intervals for height plot
x1 <- order(chiprdnbr)
y3=predict(mod2g, se.fit=TRUE, type="response")

#plot of height vs chips rdnbr
plot(height~chiprdnbr, ylab=expression(paste("Maximum sprout height (m)")), xlab="Chips Fire Severity (RdNBR)", pch=19, xlim=c(-500, 1010), cex.lab=1.5, cex.axis=1.2)
#adds regression line and confidence intervals
points(chiprdnbr[x1], (y3$fit[x1]-1), type="l", lwd=2)
polygon(c(chiprdnbr[x1], rev(chiprdnbr[x1])), c((y3$fit[x1]-1)-1.96*(y3$se.fit[x1]), rev((y3$fit[x1]-1)+1.96*(y3$se.fit[x1]))), border="black", col=rgb(0,0,0,.2))
rsq <- expression(R^2==0.24)
text(-420, 3.67, ("P<0.0001"), cex=1.2)
text(-420, 3.41, rsq, cex=1.2)
text(-420, 4, "(b)", cex=1.5)

dev.off()
