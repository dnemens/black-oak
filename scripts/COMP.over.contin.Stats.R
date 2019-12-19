#models continuous severity vs overstory importance values
library(tidyverse)

import.rd <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/import.over.delta.csv", header = T)

#############
#Modelling
###

#funtion for root mean square error
rmse <- function(pred, obs) {sqrt(mean((pred-obs)^2))}

######
#QUKE
#linear model
quke.iv <- import.rd$QUKE

q.mod1 <- lm(quke.iv~import.rd$comb)
summary (q.mod1)
q.mod2 <- lm(quke.iv~import.rd$Storrie+import.rd$Chips)
summary(q.mod2)
q.mod3 <- lm(quke.iv~import.rd$Storrie*import.rd$Chips)
summary(q.mod3)

AIC (q.mod1, q.mod2, q.mod3)
BIC (q.mod1, q.mod2, q.mod3)
qs1 <- summary(q.mod1)
qs2 <- summary(q.mod2)
qs3 <- summary(q.mod3)
data.frame(qs1$adj.r.squared, qs2$adj.r.squared, qs3$adj.r.squared)

pred <- predict(q.mod1, type="response")
obs <- import.rd$QUKE
rmse(pred=pred, obs=obs)

pred <- predict(q.mod2, type="response")
obs <- import.rd$QUKE
rmse(pred=pred, obs=obs)

pred <- predict(q.mod3, type="response")
obs <- import.rd$QUKE
rmse(pred=pred, obs=obs)

#ABCO
#linear model
import.rd.a <- select(import.rd, "Storrie", "Chips", "ABCO", "comb")
import.rd.a <-  na.omit(import.rd.a)
ABCO.iv <- import.rd.a$ABCO

a.mod1 <- lm(ABCO.iv~import.rd.a$comb)  
summary (a.mod1)
a.mod2 <- lm(ABCO.iv~import.rd.a$Storrie+import.rd.a$Chips)
summary(a.mod2)
a.mod3 <- lm(ABCO.iv~import.rd.a$Storrie*import.rd.a$Chips)
summary(a.mod3)

AIC (a.mod1, a.mod2, a.mod3)
BIC (a.mod1, a.mod2, a.mod3)

sa1 <- summary(a.mod1)
sa2 <- summary(a.mod2)
sa3 <- summary(a.mod3)
data.frame(sa1$adj.r.squared, sa2$adj.r.squared, sa3$adj.r.squared)


pred <- predict(a.mod1, type="response")
obs <- import.rd.a$ABCO
rmse(pred=pred, obs=obs)

pred <- predict(a.mod2, type="response")
obs <- import.rd.a$ABCO
rmse(pred=pred, obs=obs)

pred <- predict(a.mod3, type="response")
obs <- import.rd.a$ABCO
rmse(pred=pred, obs=obs)


#PSME
#linear model
import.rd.ps <- select(import.rd, "Storrie", "Chips", "PSME", "comb")
import.rd.ps <-  na.omit(import.rd.ps)
PSME.iv <- import.rd.ps$PSME 

ps.mod1 <- lm(PSME.iv~import.rd.ps$comb)
summary (ps.mod1)
ps.mod2 <- lm(PSME.iv~import.rd.ps$Storrie+import.rd.ps$Chips)
summary(ps.mod2)
ps.mod3 <- lm(PSME.iv~import.rd.ps$Storrie*import.rd.ps$Chips)
summary(ps.mod3)
ps.mod4 <- lm(PSME.iv~import.rd.ps$Chips)
summary(ps.mod4)

AIC (ps.mod1, ps.mod2, ps.mod3, ps.mod4)
BIC (ps.mod1, ps.mod2, ps.mod3)

sp1 <- summary(ps.mod1)
sp2 <- summary(ps.mod2)
sp3 <- summary(ps.mod3)
data.frame(sp1$adj.r.squared, sp2$adj.r.squared, sp3$adj.r.squared)


pred <- predict(ps.mod1, type="response")
obs <- import.rd.ps$PSME
rmse(pred=pred, obs=obs)

pred <- predict(ps.mod2, type="response")
obs <- import.rd.ps$PSME
rmse(pred=pred, obs=obs)

pred <- predict(ps.mod3, type="response")
obs <- import.rd.ps$PSME
rmse(pred=pred, obs=obs)

#PIPO
#linear model
import.rd.pi <- select(import.rd, "Storrie", "Chips", "PIPO", "comb")
import.rd.pi <-  na.omit(import.rd.pi)
PIPO.iv <- import.rd.pi$PIPO

pi.mod1 <- lm(PIPO.iv~import.rd.pi$comb)
summary (pi.mod1)
pi.mod2 <- lm(PIPO.iv~import.rd.pi$Storrie+import.rd.pi$Chips)
summary(pi.mod2)
pi.mod3 <- lm(PIPO.iv~import.rd.pi$Storrie*import.rd.pi$Chips)
summary(pi.mod3)
AIC (pi.mod1, pi.mod2, pi.mod3)
BIC (pi.mod1, pi.mod2, pi.mod3)

spi1 <- summary(pi.mod1)
spi2 <- summary(pi.mod2)
spi3 <- summary(pi.mod3)
data.frame(spi1$adj.r.squared, spi2$adj.r.squared, spi3$adj.r.squared)


pred <- predict(pi.mod1, type="response")
obs <- import.rd.pi$PIPO
rmse(pred=pred, obs=obs)

pred <- predict(pi.mod2, type="response")
obs <- import.rd.pi$PIPO
rmse(pred=pred, obs=obs)

pred <- predict(pi.mod3, type="response")
obs <- import.rd.pi$PIPO
rmse(pred=pred, obs=obs)

#PILA
#linear model
import.rd.pl <- select(import.rd, "Storrie", "Chips", "PILA", "comb")
import.rd.pl <-  na.omit(import.rd.pl)
PILA.iv <- import.rd.pl$PILA

pl.mod1 <- lm(PILA.iv~import.rd.pl$comb)
summary (pl.mod1)
pl.mod2 <- lm(PILA.iv~import.rd.pl$Storrie+import.rd.pl$Chips)
summary(pl.mod2)
pl.mod3 <- lm(PILA.iv~import.rd.pl$Storrie*import.rd.pl$Chips)
summary(pl.mod3)
AIC (pl.mod1, pl.mod2, pl.mod3)
BIC (pl.mod1, pl.mod2, pl.mod3)

spl1 <- summary(pl.mod1)
spl2 <- summary(pl.mod2)
spl3 <- summary(pl.mod3)
data.frame(spl1$adj.r.squared, spl2$adj.r.squared, spl3$adj.r.squared)


pred <- predict(pl.mod1, type="response")
obs <- import.rd.pl$PILA
rmse(pred=pred, obs=obs)

pred <- predict(pl.mod2, type="response")
obs <- import.rd.pl$PILA
rmse(pred=pred, obs=obs)

pred <- predict(pl.mod3, type="response")
obs <- import.rd.pl$PILA
rmse(pred=pred, obs=obs)

#CADE
#linear model
import.rd.c <- select(import.rd, "Storrie", "Chips", "CADE", "comb")
import.rd.c <-  na.omit(import.rd.c)
CADE.iv <- import.rd.c$CADE

c.mod1 <- lm(CADE.iv~import.rd.c$comb)
summary (c.mod1)
c.mod2 <- lm(CADE.iv~import.rd.c$Storrie+import.rd.c$Chips)
summary(c.mod2)
c.mod3 <- lm(CADE.iv~import.rd.c$Storrie*import.rd.c$Chips)
summary(c.mod3)
AIC (c.mod1, c.mod2, c.mod3)
BIC (c.mod1, c.mod2, c.mod3)

sc1 <- summary(c.mod1)
sc2 <- summary(c.mod2)
sc3 <- summary(c.mod3)
data.frame(sc1$adj.r.squared, sc2$adj.r.squared, sc3$adj.r.squared)


pred <- predict(c.mod1, type="response")
obs <- import.rd.c$CADE
rmse(pred=pred, obs=obs)

pred <- predict(c.mod2, type="response")
obs <- import.rd.c$CADE
rmse(pred=pred, obs=obs)

pred <- predict(c.mod3, type="response")
obs <- import.rd.c$CADE
rmse(pred=pred, obs=obs)


#model using Gamma distribution 
abco <- glm((import.rd$ABCO+300)~import.rd$comb, family = Gamma(link="log"))
summary (abco) 
Dsquared(abco, adjust = T)
abco <- glm((import.rd$ABCO+38)~import.rd$comb, family = Gamma(link="log"))
summary (abco) 



#diagnostics
AIC(quke) #AIC 
predg <- predict(quke, type="response")
obsg <- import.rd$QUKE+38
rmse(pred=predg, obs=obsg)
plot (quke)
nagelkerke(quke.glm) 

library(modEvA)
Dsquared(quke, adjust = T)
