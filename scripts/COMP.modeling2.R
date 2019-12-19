library(tidyverse)

wide <- read.csv(file = "C:/Users/debne/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv")

rdnbr <- read.csv(file = "C:/Users/debne/Dropbox/CBO/black-oak/data sheets/rdnbr.csv")

#combines dataframes for rdnbr values
import.wide <- data.frame(wide, rdnbr)

#selects only post-chips importance values
import.wide <- data.frame(import.wide[14:22])

#cleans up data frame
import.wide <- import.wide %>%
  separate(plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot) %>%
  mutate(ABCO= "ABCO.2", CADE = "CADE.2", PILA = "PILA.2", PIPO="PIPO.2", PSME="PSME.2", QUKE="QUKE.2")
  
abco <- import.wide$ABCO
cade <- import.wide$CADE
pila <- import.wide$PILA
pipo <- import.wide$PIPO
psme <- import.wide$PSME
quke <- import.wide$QUKE

StoR <- import.wide$storrie_rdnbr
ChiR <- import.wide$chips_rdnbr

Stocat <- import.wide$Storrie
Chipcat <- import.wide$Chips

mod.A <- glm(abco~Stocat*Chipcat)

mod.A.R <- glm(abco~StoR*ChiR)
