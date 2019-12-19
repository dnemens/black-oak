#evaluate relationship between delta biomass via LiDAR and severity via RdNBR
library(tidyverse)

#import data sheets
brian <-  read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/deltaBiomass.csv", header = T)
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv", header = T)

#change syntax of Plot column in brian to match rdnbr
plot <- brian$Plot
plot1 <- gsub("plot.", "", plot)
plot2 <- gsub("_", ".", plot1)
plot3 <- gsub("0", "", plot2)

#calculate delta biomass
bio <- data.frame(plot3, brian[,158:159])
bio <- mutate(bio, delta=(Biomass_Mg_15-Biomass_Mg_09))

#merge dataframes
bio <- rename(bio, plot="plot3")
bios <- merge(bio, rdnbr, by="plot")

#create vectors
delta <- bios$delta
chipR <- bios$chips_rdnbr

plot(delta~chipR)
mod <- lm(delta~chipR)
summary(mod)


