#extracts summary stats for conifer seedlings and saplings

center <- read_csv("C:/Users/dnemens/Dropbox/CBO/Data/data sheets/center sub plot.csv")
midstory <- read_csv("C:/Users/dnemens/Dropbox/CBO/Data/data sheets/midstory.csv")

library(dplyr)

abco <- midstory %>%
  filter(spp == "ABCO" | spp == "PSME") %>%
  group_by(`tree ht class`) %>%
  group_by(Plot) %>%
summarize(abco.density = sum((`tree #`)[which(spp == "ABCO")]*63.69), psme.density = sum((`tree #`)[which(spp == "PSME")]*63.69))
