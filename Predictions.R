library(keras)
set.seed(310)

#updating df.harvest to add spatial information

harvest = NULL
for (i in 1:length(dimnames(maps)$lsu)) {
  cols = cbind("gCm2" = as.numeric(maps[,i,1]),"LSU" = as.numeric(dimnames(maps)$lsu[i]), grid)
  harvest = rbind(harvest, cols)
}
df.harvest <- as.data.frame(harvest)

train_data <- df.harvest[1:667000,2:4]
prediction_data <- df.harvest[,2:4]

train_data <- scale(train_data)
col_means_train <- attr(train_data, "scaled:center")
col_stddevs_train <- attr(train_data, "scaled:scale")
prediction_data_std <- scale(prediction_data, center = col_means_train, scale = col_stddevs_train)

model <- load_model_hdf5("my_model.h5")
model %>% summary()
gCm2_predc <- model %>% predict(prediction_data_std)

df.harvest_predicted = cbind(gCm2_predc,prediction_data)

#map predictions

library(ggplot2)
library(ggmap)


world <- c(
  left = -170,
  bottom = -60,
  right = 170,
  top = 80
)

map <- get_stamenmap(world, zoom = 5, maptype = "toner-lite")
con = c(0,2,4,6,8,10,12,14,16,18,20)
for (i in con) {
MapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, colour = gCm2_predc), data = df.harvest_predicted[df.harvest_predicted$LSU == i,]) +
  scale_color_gradientn(colours = c("#ffff00", "#ff3300"),limits = c(0, max(maps[, , 1]))) +
  ggtitle(paste0("Harvest Predic ", i, " LSU"))

ggsave(paste0("Harvest Predic ", i, " LSU.jpg"),
       width = 80,
       height = 64,
       units = "cm",
       limitsize = TRUE)
}
