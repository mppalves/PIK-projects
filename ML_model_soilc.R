library(keras)
library(tidyr)
library(readr)
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
setwd(mainDir)

weather <- read_csv("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/global_weather_2000.csv")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/grass_results4Marcos.RData")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/soil_type.Rdata")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/wt_f.Rdata")
df_soilc = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/df_soilc25.Rds")
source("create_model_input_gams.R")
set.seed(310)

#harvest = maps[,,"harvest"]
soilc = df_soilc

#updating df.soilc to add spatial information
df.combined = cbind(soilc,weather,soil_type)
df.soilc = df.combined %>% gather(key = "LSU","lon":"radiation_mean","0":"25")
colnames(df.soilc)[ncol(df.soilc)] = "gCm2"
df.soilc$LSU = as.numeric(df.soilc$LSU)/10

#randomize data in df.soilc
nr<-dim(df.soilc)[1]
df.soilc = df.soilc[sample.int(nr),]

#divide data in training and testing

smp_size <- floor(0.90 * nrow(df.soilc))

train_ind <- sample(seq_len(nrow(df.soilc)), size = smp_size)

train_data <- df.soilc[train_ind,1:8]
train_labels <- df.soilc[train_ind,9]

test_data <- df.soilc[-train_ind,1:8]
test_labels <- df.soilc[-train_ind,9]

train_data <- as.matrix(train_data)
train_labels <- as.matrix(train_labels)
test_data <- as.matrix(test_data)
test_labels <- as.matrix(test_labels)

#hypermparameters
epochs <- 200
batch_size <- 100
optimizer <- optimizer_nadam()
loss <- "mse"
normalized = T

#Creating directory to save files of the run
subDir <- "ML_soilc25_envi_soil_200_mse_300_200_100_35_1_sigmoid_test"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

#scaling
if (normalized) {
  train_data <- scale(train_data)
  col_means_train_soilc <- attr(train_data, "scaled:center")
  col_stddevs_train_soilc <- attr(train_data, "scaled:scale")
  test_data <- scale(test_data, center = col_means_train_soilc, scale = col_stddevs_train_soilc)
  saveRDS(col_means_train_soilc, file ="col_means_train_soilc.Rds")
  saveRDS(col_stddevs_train_soilc, file = "col_stddevs_train_soilc.Rds")
  print("Normalized")
}

### Modelo reduced--------

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = 300, activation = "softplus", input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 200, activation = "softplus") %>%
    layer_dense(units = 100, activation = "softplus") %>%
    layer_dense(units = 35, activation = "softplus") %>%
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
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 10)
dir.create(file.path(mainDir,"tensorboard"), showWarnings = T)
# tensor_board =  callback_tensorboard(log_dir = file.path(mainDir,"tensorboard"), histogram_freq = 1,
#                                      batch_size = 500, write_graph = TRUE, write_grads = T,
#                                      write_images = T, embeddings_freq = 0,
#                                      embeddings_layer_names = NULL, embeddings_metadata = NULL,
#                                      embeddings_data = NULL, update_freq = "epoch")

history <- model %>% fit(
  train_data,
  train_labels,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.05,
  verbose = 1,
  callbacks = list(early_stop),
  view_metrics = "F"
)

model %>% save_model_hdf5("model.h5")
model %>% save_model_weights_hdf5("model_weights.h5")

#evaluating results
c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 1))

postscript(file="Learning curve.eps",horiz=TRUE,onefile=FALSE,width=9,height=5,paper="letter")
plot(history, metrics = "mean_absolute_error", smooth = T)
paste0("Mean squared error on test set: ", sprintf("%.2f", mae))
dev.off()

test_predictions <- model %>% predict(test_data)
test_results = cbind(test_predictions[1:200, 1],test_labels[1:200,1])
test_corr = cor(test_predictions, test_labels)

hist(test_predictions, breaks = 100, xlim = c(0,100000))
hist(test_labels,breaks = 100,xlim = c(0,100000))

#exporting input_data for analysis
output_data <- model %>% predict(train_data)
input_data <- train_labels
saveRDS(output_data, file = "output_data_ML.Rds")
saveRDS(input_data, file = "input_data_ML.Rds")

#writing results
x = list("epochs"= epochs, "batch_size: "= batch_size, "optimizer"= as.character(optimizer), "loss"= loss, "Mean absolute error on test set"= mae, "normalized" = normalized, test_results, "correlation" = test_corr)
write.csv2(x,file = paste0("excell_comparisson",".csv"))


#saving weights and bias for GAMs application
subDir_gams <- "weights_GAMS"
dir.create(file.path(mainDir, subDir,subDir_gams), showWarnings = FALSE)
setwd(file.path(mainDir,subDir, subDir_gams))
inputs = c(colnames(df.soilc)[-ncol(df.soilc)])
export_weights_gams(model, inputs, "harvest")

#https://sefiks.com/2017/08/11/softplus-as-a-neural-networks-activation-function/ softplus reference