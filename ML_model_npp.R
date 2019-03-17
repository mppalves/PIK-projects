library(keras)
library(ggplot2)
library(tidyr)
library(readr)

weather <- read_csv("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/global_weather_2000.csv")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/grass_results4Marcos.RData")
set.seed(310)

#updating df.npp to add spatial information
npp = NULL
for (i in 1:length(dimnames(maps)$lsu)) {
  cols = cbind("gCm2" = as.numeric(maps[,i,1]),"LSU" = as.numeric(dimnames(maps)$lsu[i]), grid, weather[,c(1,3,5)])
  npp = rbind(npp, cols)
}

df.npp <- as.data.frame(npp)
df.combined = cbind(maps[,,"npp"],grid,weather[,c(1,3,5)])
df.npp = df.combined %>% gather(key = "LSU","lon":"radiation_mean","00":"20")
colnames(df.npp)[ncol(df.npp)] = "gCm2"
df.npp$LSU = as.numeric(df.npp$LSU)/10

#randomize data in df.npp
nr<-dim(df.npp)[1]
df.npp = df.npp[sample.int(nr),]

#divide data in training and testing

smp_size <- floor(0.90 * nrow(df.npp))

train_ind <- sample(seq_len(nrow(df.npp)), size = smp_size)

train_data <- df.npp[train_ind,1:6]
train_labels <- df.npp[train_ind,7]

test_data <- df.npp[-train_ind,1:6]
test_labels <- df.npp[-train_ind,7]

train_data <- as.matrix(train_data)
train_labels <- as.matrix(train_labels)
test_data <- as.matrix(test_data)
test_labels <- as.matrix(test_labels)

#hypermparameters
epochs <- 10
batch_size <- 100
optimizer <- optimizer_nadam()
loss <- "mse"
normalized = T

#scaling
if (normalized) {
  train_data <- scale(train_data)
  col_means_train <- attr(train_data, "scaled:center")
  col_stddevs_train <- attr(train_data, "scaled:scale")
  test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)
  save(col_means_train, file ="col_means_train.Rdata")
  save(col_stddevs_train, file = "col_stddevs_train.Rdata")
  print("Normalized")
}

### Modelo reduced--------

build_model <- function() {

  model <- keras_model_sequential() %>%
    layer_dense(units = 200, activation = "relu", input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dense(units = 100, activation = "relu") %>%
    layer_dense(units = 1)

  model %>% compile(
    loss = loss,
    optimizer = optimizer,
    metrics = list("mean_absolute_error")
  )

  model
}


model <- build_model()
model %>% summary()


# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 100)

history <- model %>% fit(
  train_data,
  train_labels,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop)
)

model %>% save_model_hdf5("npp.h5")
model %>% save_model_weights_hdf5("npp.h5")

#evaluating results
c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 1))
plot(history, metrics = "mean_absolute_error", smooth = T)
paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))

test_predictions <- model %>% predict(test_data)
test_results = cbind(test_predictions[1:100, 1],test_labels[1:100,1])
test_corr = cor(test_predictions, test_labels)

hist(test_predictions, breaks = 100, xlim = c(0,200))
hist(test_labels,breaks = 100,xlim = c(0,200))

#exporting input_data for analysis
output_data <- model %>% predict(train_data)
input_data <- train_labels
save(output_data, file = "output_data_ML.Rdata")
save(input_data, file = "input_data_ML.Rdata")

#writing results
x = list("epochs"= epochs, "batch_size: "= batch_size, "optimizer"= as.character(optimizer), "loss"= loss, "Mean absolute error on test set"= mae, "normalized" = normalized, test_results, "correlation" = test_corr)
write.csv2(x,file = paste0("ML_coordinates_tempM_precM_radM_reduced_200_epochs",".csv"))




##################################################
#Old achitectures

#building the model
# #Modelo completo----
# build_model <- function() {
# 
#   model <- keras_model_sequential() %>%
#     layer_dense(units = 500, activation = "relu", input_shape = dim(train_data)[2]) %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 450, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 400, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 350, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 300, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 250, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 200, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 150, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 100, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 50, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 10, activation = "relu") %>%
#     layer_dropout(rate = 0.1) %>%
#     layer_dense(units = 1)
# 
# 
#   model %>% compile(
#     loss = loss,
#     optimizer = optimizer,
#     metrics = list("mean_absolute_error")
#   )
# 
#   model
# }

#gold
# build_model <- function() {
#   
#   model <- keras_model_sequential() %>%
#     layer_dense(units = 100, activation = "relu", input_shape = dim(train_data)[2]) %>%
#     layer_dense(units = 50, activation = "relu") %>%
#     layer_dense(units = 30, activation = "relu") %>%
#     layer_dense(units = 10, activation = "relu") %>%
#     layer_dense(units = 1)
#   
#   model %>% compile(
#     loss = loss,
#     optimizer = optimizer,
#     metrics = list("mean_absolute_error")
#   )
#   
#   model
# }
