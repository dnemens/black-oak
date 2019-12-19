#heat map

library(tidyverse)

plotz <- read.csv("C:/Users/dnemens/Dropbox/CBO/black-oak/data sheets/import.over.delta.long.csv")

plotz <- plotz %>% filter(!is.na(Importance_Value))

#plotz <-plotz  %>%
 # mutate(storrie=storrie/10, chips=chips/10)


heat <- ggplot(data = plotz, aes(x=Storrie, y=Chips)) + 
  #geom_tile(width = 100, height=100)+
  geom_point(aes(color=Importance_Value), size=5)+
  scale_color_gradient2(limits=c(-280,280), low="darkblue", mid= "white", high="red", name="Delta IV")+
  #geom_raster(interpolate = T)+
  #scale_fill_gradient2( limits=c(-280,280), low="darkblue", mid= "white", high="red", name="Delta Importance Value")+
  coord_cartesian(xlim = c(-500, 1300), ylim = c(-500, 1300))+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill="grey80"), axis.title = element_text(size = 18), strip.text = element_text(size = 12, face = "bold"), legend.title = element_text(size=12))+
  xlab("Storrie Severity (RdNBR)")+
  ylab("Chips Severity (RdNBR)")+
  facet_wrap(~Species)
 
heat

ggsave(heat, filename = "C:/Users/dnemens/Dropbox/CBO/black-oak/plots/heatmap.tiff", dpi = 300, width = 10.5, height = 7)



