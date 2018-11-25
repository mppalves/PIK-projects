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
germany <- c(
  left = 5,
  bottom = 45,
  right = 20,
  top = 55
)
map <- get_stamenmap(world, zoom = 5, maptype = "toner-lite")