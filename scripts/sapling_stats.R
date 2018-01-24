#extracts summary stats for conifer seedlings and saplings

center <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/center sub plot.csv")
abco <- center %>%
  filter(Spp == "ABCO" | Spp == "PSME") %>%
  group_by(ht) %>%
  group_by(plot) %>%
  summarize(abco.density = length(which(Spp == "ABCO")*63.69), psme.density = length(which(Spp == "PSME")*63.69)



midstory <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/midstory.csv")

library(dplyr)

abco <- midstory %>%
  filter(spp == "ABCO" | spp == "PSME") %>%
  group_by(`tree ht class`) %>%
  group_by(Plot) %>%
summarize(abco.density = sum((`tree #`)[which(spp == "ABCO")]*63.69), psme.density = sum((`tree #`)[which(spp == "PSME")]*63.69))
