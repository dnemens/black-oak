library(tidyverse)

#load wide form importance data
wide <- read.csv(file = "C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/mean.import.over.csv")

#imports rdnbr values
rdnbr <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/rdnbr.csv", header = T)

#adds rdnbr values to dataframe, fixes column names, removes all but post-chips values
wide.rd <- data.frame(rdnbr$storrie_rdnbr, rdnbr$chips_rdnbr, wide)

wide.rd <-  wide.rd %>%
  separate(rdnbr.plot, c("Storrie", "Chips", "Plot"), remove = T) %>%
  select(-Plot) %>%
  rename ("StorrieR" = rdnbr.storrie_rdnbr, "ChipsR" = rdnbr.chips_rdnbr) %>%
  select (-10, -11, -12, -13, -14, -15)