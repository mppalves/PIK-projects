library(readr)

data_maps = load("grass_results4Marcos.RData")
wt <- read_table2("Koeppen-Geiger-ASCII.txt")

#viasualizing the number of cells per climate zone
x = table(wt$Cls)

#removing antartica from the grid
wt_art = wt[which(wt$Lat > 0 & wt$Cls == "EF"), ] #removing climate EF bellow lat o (equator)
wt_wor = wt[which(wt$Cls != "EF"), ]
wt_ant = rbind(wt_art, wt_wor)

#Merging wether climate with the LPJml grid
wt_f = merge(
  wt_ant,
  grid,
  by.x = c("Lon", "Lat") ,
  by.y = c("lon", "lat"),
  all = TRUE
)

save(wt_f, file= "wt_f.Rdata")

#viasualizing the number of cells per climate zone after merging
y = table(wt_f$Cls)
View(as.data.frame(y))


#Ploting
library(ggmap)

world <- c(
  left = -180,
  bottom = -60,
  right = 179,
  top = 80
)

map <- get_stamenmap(world, zoom = 3, maptype = "toner-lite")

MapPoints_harv <- ggmap(map) +
  geom_point(aes(x = Lon, y = Lat, colour = as.factor(Cls)), data = wt_f) +
  scale_colour_manual(values = c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF", "Black")) 
MapPoints_harv

ggsave("Climate Clustering_Koeppen-Geiger.jpg",
       width = 80,
       height = 64,
       units = "cm",
       limitsize = TRUE)


MapPoints_harv <- ggmap(map) +
  geom_point(aes(x = Lon, y = Lat), data = wt_f[is.na(wt_f$Cls),])
  #scale_colour_manual(values = c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF", "#64FFFF", "#F5FFFF", "Black")) 
MapPoints_harv

ggsave("Climate Clustering_Koeppen-Geiger_NAs.jpg",
       width = 80,
       height = 64,
       units = "cm",
       limitsize = TRUE)
