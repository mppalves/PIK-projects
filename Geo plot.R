library(ggplot2)
library(ggmap)


world <- c(
  left = -170,
  bottom = -60,
  right = 170,
  top = 80
)
germany <- c(
  left = 5,
  bottom = 45,
  right = 20,
  top = 55
)
map <- get_stamenmap(world, zoom = 5, maptype = "toner-lite")

concentration = dimnames(maps)$lsu
mapvariable = dimnames(maps)$var

for (j in 1:length(mapvariable)) {
  vartitle = mapvariable[j]
  for (i in 1:length(concentration)) {
    numtitle = concentration[i]
    gCm2 = as.numeric(maps[, i, j])
    
    MapPoints <- ggmap(map) +
      geom_point(aes(x = lon,
                     y = lat,
                     colour = gCm2), data = grid) +
      scale_color_gradientn(
        colours = c("#ffff00", "#ff3300"),
        limits = c(0, max(maps[, , j]))) +
      ggtitle(paste0(vartitle, numtitle, "LSU"))
    
    ggsave(paste0(vartitle, numtitle, "LSU.jpg"),
      width = 80,
      height = 64,
      units = "cm",
      limitsize = TRUE)
    
  }
}

