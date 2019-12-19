#STATS
library(car)
library(multcomp)
library(tidyverse)
library(RColorBrewer)

#categorical stats on change in overstory importance values pre-fire to post-Storrie

#load long form importance data
long <- read.csv(file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.long.csv")

###PREP for ANOVA comparing Imp vals for each species Post-Storrie by severity category
#break severity into categories
imps.long <- long %>% 
  separate(plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot) %>%
  filter(time!="Post-Chips Fire")

#combine time with severity
imps.long <- imps.long %>%
  unite("time_Sev", c(Storrie, time), remove = F) %>%
  select(-Chips)

time <- as.factor(imps.long$time)
Species <- as.factor(imps.long$Species)
imps <- imps.long$Importance.Value
ts <- as.factor(imps.long$time_Sev)
sev <- as.factor(imps.long$Storrie)

#Pre vs. post-storrie by species only, severity lumped
ggplot(imps.long, aes(x=Species, y=Importance.Value, fill = Species)) +
  geom_boxplot()+
  facet_wrap(~time)+
  theme(legend.position = "null")


####Post-Storrie condition by species and severity
ggplot(imps.long, aes(x=sev, y=Importance.Value, fill = Species)) +
  geom_boxplot()+
  facet_wrap(~Species)+
  theme(legend.position = "null")

#Pre-Storrie composition by species
pre.fire <- subset(imps.long, time == "Pre-fire")
                   
ggplot(pre.fire, aes(x=Species, y=Importance.Value, fill = Species)) +
  geom_boxplot()+
  theme(legend.position = "null", element_text(size=5))
########################
#ANOVAS
#ABCO ###
abimps <- imps.long %>%
  filter(Species == "ABCO")

abimps.post <- abimps %>%
  filter(time == "Post-Storrie Fire")

ab.po <- abimps.post$Importance.Value
aSto <- as.factor(abimps.post$Storrie)

boxplot(ab.po~aSto)

hist((ab.po))
a <- aov(ab.po~aSto)

#Tukey comparison
atuk <- glht(a, linfct = mcp(aSto = "Tukey"))
summary(atuk)
cld(atuk)
plot(cld(atuk), main = "ABCO")

#model eval
par(mfrow=c(2,2))
plot(a)
par(mfrow=c(1,1))
hist(a$residuals)
plot(ab.po~a$fitted.values)
leveneTest(a)

#look at diff in variance
by(ab.po, aSto, function(x){ var(x, na.rm=T) })


#PSME ###
psimps <- imps.long %>%
  filter(Species == "PSME") 

psimps.post <- psimps %>%
  filter(time == "Post-Storrie Fire")

ps <- (psimps.post$Importance.Value)
psSto <- as.factor(psimps.post$Storrie)

ps.mod <- aov((ps)~psSto)
summary(ps.mod)

#model eval -- probs with heteroscedsaticity!!
hist(ps)
par(mfrow=c(2,2))
plot(ps.mod)
hist(ps.mod$residuals)
plot(ps~ps.mod$fitted.values)
leveneTest(ps.mod) #see here

#look at diff in variance - largest variance is more than 9x > smallest! (<4X ok)
by(ps, psSto, function(x){ var(x, na.rm=T) })

#How to fix heteroscedasity?
boxcox(ps~psSto)
#y ^-.5 might help!  
#NOPE!  nor does log!  

#How about welches adjustment??
oneway.test(ps~psSto)
#could help!  

pstuk <- glht(ps.mod, linfct = mcp(psSto = "Tukey"))
summary(pstuk)
cld(pstuk)
plot(cld(pstuk), main = "PSME")

#QUKE
qimps <- imps.long %>%
  filter(Species == "QUKE")

qimps.post <- qimps %>%
  filter(time == "Post-Storrie Fire")

q <- qimps.post$Importance.Value
qSto <- as.factor(qimps.post$Storrie)

#diagnostics
q.mod <- aov(q~qSto)
summary(q.mod)
par(mfrow=c(2,2))
plot(q.mod)
hist(q.mod$residuals)

#Tukey comparison
qtuk <- glht(q.mod, linfct = mcp(qSto = "Tukey"))
summary(qtuk)
cld(qtuk)
plot(cld(qtuk), main = "QUKE")

################################
#T-TESTS (or similar) for pre-to-post fire comparison for each species
#ABCO
abco1 <- subset(abimps, abimps$Storrie == 1)

ab.pre1 <- abco1$Importance.Value[which(abco1$time == "Pre-fire")]
ab.post1 <- abco1$Importance.Value[which(abco1$time == "Post-Storrie Fire")]

var.test(ab.pre1, ab.post1)
qqplot(ab.pre1, ab.post1)

t.test(ab.pre1, ab.post1, var.equal = T)


abco2 <- subset(abimps, abimps$Storrie == 2)
aimp2 <- abco2$Importance.Value
atime2 <- as.factor(abco2$time_Sev)  

a.low <- t.test(aimp2~atime2)
a.low

abco3 <- subset(abimps, abimps$Storrie == 3)
aimp3 <- abco3$Importance.Value
atime3 <- as.factor(abco3$time_Sev)  

a.mod <- t.test(aimp3~atime3)
a.mod

abco4 <- subset(abimps, abimps$Storrie == 4)
apre4 <- abco4$Importance.Value[which(abco4$time == "Pre-fire")]
apost4 <- abco4$Importance.Value[which(abco4$time == "Post-Storrie Fire")]

var.test(apre4, apost4) #variance equal
t.test(apre4, apost4, var.equal = T)


#PSME
psme1 <- subset(psimps, psimps$Storrie == 1)

ps.pre1 <- psme1$Importance.Value[which(psme1$time == "Pre-fire")]
ps.post1 <- psme1$Importance.Value[which(psme1$time == "Post-Storrie Fire")]

var.test(ps.pre1, ps.post1)
qqplot(ps.pre1, ps.post1)

t.test(ps.pre1, ps.post1, var.equal = T)

psme2 <- subset(psimps, psimps$Storrie == 2)
ps.pre2 <- psme2$Importance.Value[which(psme2$time == "Pre-fire")]
pstime2 <- psme2$Importance.Value[which(psme2$time == "Post-Storrie Fire")]

t.test(ps.pre2, pstime2, var.equal = T)

psme3 <- subset(psimps, psimps$Storrie == 3)
ps.pre3 <- psme3$Importance.Value[which(psme3$time == "Pre-fire")]
pstime3 <- psme3$Importance.Value[which(psme3$time == "Post-Storrie Fire")]

t.test(ps.pre3, pstime3, var.equal = T)


psme4 <- subset(psimps, psimps$Storrie == 4)
pspre4 <- psme4$Importance.Value[which(psme4$time == "Pre-fire")]
pspost4 <- psme4$Importance.Value[which(psme4$time == "Post-Storrie Fire")]

var.test(pspre4, pspost4) #unequal variance, Welches correction applied
t.test(pspre4, pspost4, var.equal = F)

#QUKE
quke1 <- subset(qimps, qimps$Storrie == 1)

q.pre1 <- quke1$Importance.Value[which(quke1$time == "Pre-fire")]
q.post1 <- quke1$Importance.Value[which(quke1$time == "Post-Storrie Fire")]

var.test(q.pre1, q.post1)
qqplot(q.pre1, q.post1)

t.test(q.pre1, q.post1, var.equal = T)

quke2 <- subset(qimps, qimps$Storrie == 2)
q.pre2 <- quke2$Importance.Value[which(quke2$time == "Pre-fire")]
qtime2 <- quke2$Importance.Value[which(quke2$time == "Post-Storrie Fire")]

t.test(q.pre2, qtime2, var.equal = T)

quke3 <- subset(qimps, qimps$Storrie == 3)
q.pre3 <- quke3$Importance.Value[which(quke3$time == "Pre-fire")]
qtime3 <- quke3$Importance.Value[which(quke3$time == "Post-Storrie Fire")]

t.test(q.pre3, qtime3, var.equal = T)

quke4 <- subset(qimps, qimps$Storrie == 4)
qpre4 <- quke4$Importance.Value[which(quke4$time == "Pre-fire")]
qpost4 <- quke4$Importance.Value[which(quke4$time == "Post-Storrie Fire")]

var.test(qpre4, qpost4)
t.test(qpre4, qpost4, var.equal = T)

#CADE
cade4 <- imps.long %>%
  filter(Species == "CADE" & Storrie == 4)
cade.pre4 <- cade4$Importance.Value[which(cade4$time == "Pre-fire")]
cade.post4 <- cade4$Importance.Value[which(cade4$time == "Post-Storrie Fire")]

var.test(cade.pre4, cade.post4)
t.test(cade.pre4, cade.post4)

#PILA
pila4 <- imps.long %>%
  filter(Species == "PILA" & Storrie == 4)
pila.pre4 <- pila4$Importance.Value[which(pila4$time == "Pre-fire")]
pila.post4 <- pila4$Importance.Value[which(pila4$time == "Post-Storrie Fire")]

var.test(pila.pre4, pila.post4)
t.test(pila.pre4, pila.post4)
