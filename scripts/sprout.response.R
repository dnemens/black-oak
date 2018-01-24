#evaluates chips sprout metrics (basal area and height) by chips Fire RdNBR

#loads csv of sprout response data 
sprout <- read.csv ("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/sprout.response.csv")

#loads nesessary packages
library(fitdistrplus)
library(pscl)
library(ggplot2)
library (QTLRel)
library (popbio)
library(dplyr)
library (rcompanion)
library(ResourceSelection)
library(MASS)

#independent variables
plot <- (sprout$plot)
storrdnbr <- (sprout$storrie_rdnbr)
chiprdnbr <- (sprout$chips_rdnbr)

chipr.h <- chiprdnbr[which(sprout$num.Chips.sprouts>0)]

#######response variables######

#chips sprout basal area per clump
chipBAclump <- sprout$Chips.BA.clump
BAclump <- chipBAclump[which(sprout$num.Chips.sprouts>0)]
#chips sprout basal area per hectare
chipBAha <- (sprout$Chips.BA.ha)
BAha <- chipBAha[which(sprout$num.Chips.sprouts>0)]
#chips sprout density per clump
chipct <- (sprout$num.Chips.sprouts)
#chips sprout crown area
crown <- sprout$Mean.crown.area
crown <- crown[which(sprout$num.Chips.sprouts>0)]

#height of chips sprouts
height <- sprout$Max.of.oak.live.emer.ht
chip.height <- height[which(sprout$num.Chips.sprouts>0)]

#########################################################
#analyses

#funtion for root mean square error
rmse <- function(x) {sqrt(mean(x^2))}

#exponential fit for BA per ha
mod.exp <- lm(BAha)

#gamma dist glm for chips sprout BA/ha 
ba.plus <- BAha+.01#add .1 b/c gamma needs positive response 
mod1 <- glm (ba.plus~chipr.h, family=Gamma(link="log"), control = list(maxit = 100))
summary(mod1)

#model diagnostics
rmse(mod1$residuals) #.26
library(rcompanion)
nagelkerke(mod1)

#gam
library(mgcv)
mod2 <- gam(ba.plus ~ s(chiprdnbr, bs="cs", k=2), family=Gamma(link="log"))
y2=predict(mod2, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")
summary (mod2) 
gam.check(mod2)

xx <- order(chiprdnbr)
y1 <- predict(mod1, data.frame(x=chiprdnbr), se.fit = TRUE, type="response")

######################################################################
#chips sprout height model eval
par(mfrow = c(1, 2))
hist (chip.height)
qqnorm (chip.height)
qqline (chip.height)
dev.off()

#linear model of max sprout height with 0's removed
ht.lm <- lm(chip.height~chipr.h)
summary (ht.lm)

#######################################################################
#produces stacked sprout metric plots (basal area and height)

tiff(filename="C:/Users/dnemens/Documents/CBO manuscript/plots/sprout.response.tiff", width=720, height=900, bg="transparent")

par(mfrow=c(2,1), 
    oma = c(1.5,.5,.5,1),
    mar=c(3.9,5,.6,1))

#setup for confidence intervals BA plot
xx <- order(chipr.h)
y1 <- predict(mod1, data.frame(x=chipr.h), se.fit = TRUE, type="response")

#plot of Basal area vs. chips severity with regression line & confid intervals
plot(ba.plus~chipr.h, ylab=("Basal area "~ (cm^2/clump)), xlab=" ", pch=19, xlim=c(0, 1010), cex.lab=1.5, cex.axis=1.2)
#adds regression line based on prediction from glm model
#curve(predict(mod1,data.frame(chipr.h=x), type="response"), add=TRUE, lwd=2, col="red")  does same as below
points(chipr.h[xx], y1$fit[xx], type="l", lwd=2)
#confidence intervals
polygon(c(chipr.h[xx], rev(chipr.h[xx])), c(y1$fit[xx]-1.96*y1$se.fit[xx], rev(y1$fit[xx]+1.96*y1$se.fit[xx])), border="black", col=rgb(0,0,0,.2))
#adds text
rsq1 <- expression(R^2==0.45)
text(40, .033, "P<0.0001", cex=1.2)
text(40, .031, rsq1, cex=1.2)
text(30, .036, "(a)", cex=1.5)


#set up for conifedence intervals for height plot
x1 <- order(chipr.h)
y3=predict(ht.lm, se.fit=TRUE, type="response")

#plot of height vs chips rdnbr
plot(chip.height~chipr.h, ylab=expression(paste("Maximum sprout height (m)")), xlab="Chips Fire Severity (RdNBR)", pch=19, xlim=c(0, 1010), cex.lab=1.5, cex.axis=1.2)
#adds regression line and confidence intervals
points(chipr.h[x1], y3$fit[x1], type="l", lwd=2)
#points(chipr.h[x1], y3$fit[x1]-1.96*y3$se.fit[x1], type="l", lwd=1, col=2)
#points(chipr.h[x1], y3$fit[x1]+1.96*y3$se.fit[x1], type="l", lwd=1, col=2)
polygon(c(chipr.h[x1], rev(chipr.h[x1])), c(y3$fit[x1]-1.96*y3$se.fit[x1], rev(y3$fit[x1]+1.96*y3$se.fit[x1])), border="black", col=rgb(0,0,0,0.15))
rsq <- expression(R^2==0.29)
text(30, 3.67, ("P<0.0001"), cex=1.2)
text(30, 3.41, rsq, cex=1.2)
text(30, 4, "(b)", cex=1.5)

dev.off()

###########################################################
#code below uses r to calculate basal area, but for some reason a few plots dissapear!

sprout2 <- read.csv ("~/CBO manuscript/Data/data sheets/focaloak.csv")
rdnbr <- read.csv("~/CBO manuscript/Data/data sheets/rdnbr.csv")

#calculates basal area of chips sprouts 
sprout2 <- mutate(sprout2, ba.m2ha=chips.live^2*.00007854)
sprout2 <- mutate(sprout2, ba.cm2clump=(((chips.live/2)^2)*pi))

#calculates total basal area of sprout clump
sprout <- sprout2 %>%
  group_by(plot) %>%
  summarize(BA = sum(ba.cm2clump))

#merges chips ba info with rdnbr for each plot
sprout <-  merge(sprout, rdnbr, by="plot")
sprout <- na.omit(sprout)

#creates vectors for variables 
chiprdnbr <- (sprout$chips_rdnbr)
ba <- sprout$BA

plot(ba~chiprdnbr)

