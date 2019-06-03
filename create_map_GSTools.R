#' @title Ploting on map
#' @description automatizing the process to create the maps and save them the jpg.
library(ggplot2)
library(ggmap)
library(tidyverse)
library(viridis)


create_map = function(data,color_range = c("#FFFFFF","#ff2323"),color_option = "D",map_name = NULL, save_jpg = list(T, width = 80, height = 64), cathegorical = F, limits = NULL){
color_range[2]
  world <- c(
    left = -170,
    bottom = -60,
    right = 170,
    top = 80
  )
  
  map <- get_stamenmap(world, zoom = 2, maptype = "terrain-background")
  
  if(cathegorical){
      data[,1] = as.factor(data[,1])
  }
  
  MapPoints <- ggmap(map) +

    geom_point(aes(x = data[,2], y = data[,3], colour = data[,1]), data = data) +
    if(cathegorical){
      #scale_colour_manual(name = colnames(data)[1], values = terrain.colors(length(unique(data[,1]))));
      scale_color_viridis(discrete = TRUE, option = color_option)
    }else{
      scale_color_gradientn(colours = color_range, name = colnames(data)[1], limits=limits);
    }
  
  
  MapPoints
  
  if(save_jpg[[1]]==TRUE){
    
    ggsave(paste0(map_name,"_", str_remove(str_remove(Sys.time(),":"),":"), ".jpg"),
           width = save_jpg[[2]],
           height = save_jpg[[3]],
           units = "cm",
           limitsize = TRUE)
    
    return(MapPoints)
    stringrs::str_remove()
  }else{
    return(MapPoints)
  }

}

