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
rmse <- function(x) {sqrt(mean(x^2))}

### looking at model that includes plots with no chips sprouts
#gamma dist glm for chips sprout BA
mod <- glm ((BAclump+.01)~chiprdnbr, family=Gamma(link="log"), control = list(maxit = 100))
summary(mod)
#diagnostics
rmse(mod$residuals)
nagelkerke(mod)

#gam??
library(mgcv)
mod.gam <- gam((BAclump+.01) ~ s(chiprdnbr, bs="cs", k=3), family=Gamma(link="log"))
rmse(mod.gam$residuals)
summary (mod.gam) 
gam.check(mod.gam)

y2=predict(mod.gam, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")


######################################################################
#chips sprout height model eval
par(mfrow = c(1, 2))
hist (chip.height)
qqnorm (chip.height)
qqline (chip.height)
dev.off()

#model of max sprout height without 0's removed
mod2 <- glm((height+.01)~chiprdnbr, family = Gamma(link = "log"), control = list(maxit=100))
summary (mod2)
#diagnostics
rmse(mod2$residuals)
nagelkerke(mod2)

#######################################################################
#produces stacked sprout metric plots (basal area and height)

setwd("C:/Users/dnemens/Dropbox/CBO/black-oak/plots")

tiff(file="sprout.response2.tiff", width=720, height=900, bg="transparent")

par(mfrow=c(2,1), 
    oma = c(1.5,.5,.5,1),
    mar=c(3.9,5,.6,1))

#setup for confidence intervals BA plot
xx <- order(chiprdnbr)
y1 <- predict(mod, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")

#plot of Basal area vs. chips severity with regression line & confid intervals
plot(BAclump~chiprdnbr, ylab=("Basal area "~ (cm^2/clump)), xlab=" ", pch=19, xlim=c(-500, 1010), cex.lab=1.5, cex.axis=1.2)
#adds regression line based on prediction from glm model
points(chiprdnbr[xx], y1$fit[xx], type="l", lwd=2)
#confidence intervals
polygon(c(chiprdnbr[xx], rev(chiprdnbr[xx])), c(y1$fit[xx]-1.96*y1$se.fit[xx], rev(y1$fit[xx]+1.96*y1$se.fit[xx])), border="black", col=rgb(0,0,0,.2))
#adds text
rsq1 <- expression(R^2==0.39)
text(-420, 250, "(a)", cex=1.5)
text(-420, 230, "P<0.0001", cex=1.2)
text(-420, 210, rsq1, cex=1.2)

#set up for conifedence intervals for height plot
x1 <- order(chiprdnbr)
y3=predict(mod2, se.fit=TRUE, type="response")

#plot of height vs chips rdnbr
plot(height~chiprdnbr, ylab=expression(paste("Maximum sprout height (m)")), xlab="Chips Fire Severity (RdNBR)", pch=19, xlim=c(-500, 1010), cex.lab=1.5, cex.axis=1.2)
#adds regression line and confidence intervals
points(chiprdnbr[x1], y3$fit[x1], type="l", lwd=2)
#points(chiprdnbr[x1], y3$fit[x1]-1.96*y3$se.fit[x1], type="l", lwd=1, col=2)
#points(chiprdnbr[x1], y3$fit[x1]+1.96*y3$se.fit[x1], type="l", lwd=1, col=2)
polygon(c(chiprdnbr[x1], rev(chiprdnbr[x1])), c(y3$fit[x1]-1.96*y3$se.fit[x1], rev(y3$fit[x1]+1.96*y3$se.fit[x1])), border="black", col=rgb(0,0,0,.2))
rsq <- expression(R^2==0.24)
text(-420, 3.67, ("P<0.0001"), cex=1.2)
text(-420, 3.41, rsq, cex=1.2)
text(-420, 4, "(b)", cex=1.5)

dev.off()

