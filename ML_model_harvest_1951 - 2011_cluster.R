library(keras)
library(tidyr)
library(readr)

mainDir <- "/home/pedrosa/LPJmLClimateScenario1951-2099/Machine_learning_Model"
setwd(mainDir)

df.harvest = read_rds(paste0(mainDir,"/harvest_lsu_temp_prec_shor_long_soil1951-2011.rds"))
source("create_model_input_gams.R")
set.seed(310)



#randomize data in df.harvest
nr<-dim(df.harvest)[1]
df.harvest = df.harvest[sample.int(nr),]

#divide data in training and testing
smp_size <- floor(0.90 * nrow(df.harvest))

train_ind <- sample(seq_len(nrow(df.harvest)), size = smp_size)

train_data <- df.harvest[train_ind,2:7]
train_labels <- df.harvest[train_ind,1]

test_data <- df.harvest[-train_ind,2:7]
test_labels <- df.harvest[-train_ind,1]

train_data <- as.matrix(train_data)
train_labels <- as.matrix(train_labels)
test_data <- as.matrix(test_data)
test_labels <- as.matrix(test_labels)

#hypermparameters
epochs <- 500
batch_size <- 1000
optimizer <- optimizer_nadam()
loss <- "mse"
normalized = T


#Creating directory to save files of the run
subDir <- "ML_harvest25_lsu_temp_prec_shor_long_soil_softplus_1000.500.200.10.1"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))


#scaling
if (normalized) {
  train_data <- scale(train_data)
  col_means_train_harvest <- attr(train_data, "scaled:center")
  col_stddevs_train_harvest <- attr(train_data, "scaled:scale")
  test_data <- scale(test_data, center = col_means_train_harvest, scale = col_stddevs_train_harvest)
  saveRDS(col_means_train_harvest, file ="col_means_train_harvest.Rds")
  saveRDS(col_stddevs_train_harvest, file = "col_stddevs_train_harvest.Rds")
  print("Normalized")
}


### Modelo reduced--------

build_model <- function() {

  model <- keras_model_sequential() %>%
    layer_dense(units = 1000, activation = "softplus", input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 500, activation = "softplus") %>%
    layer_dense(units = 200, activation = "softplus") %>%
    layer_dense(units = 100, activation = "softplus") %>%
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

#writting model information
library(stringr)
x = list("Model Summary" = build_model, "normalized" = normalized, "epochs" = epochs, "batch_size" = batch_size, "loss" = loss)
y = capture.output(x)
writeLines(y, con = file(paste0("Trained model info",stringr::str_remove(str_remove(Sys.time(),":"),":"),".txt")))

# The patience parameter is the amount of epochs to check for improvement.
early_stop = callback_early_stopping(monitor = "val_loss", patience = 100)
# dir.create(file.path(mainDir,"tensorboard"), showWarnings = T)
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
  verbose = 2,
  callbacks = list(early_stop),
  view_metrics = "F"
)

model %>% save_model_hdf5(paste0("model",".h5"))
model %>% save_model_weights_hdf5(paste0("model","_weights.h5"))

#evaluating results
c(loss, mae) %<-% (model %>% evaluate(test_data, test_labels, verbose = 1))

postscript(file="Learning curve.eps",horiz=TRUE,onefile=FALSE,width=9,height=5,paper="letter")
plot(history, metrics = "mean_absolute_error", smooth = T)
paste0("Mean squared error on test set: ", sprintf("%.2f", mae))
dev.off()

test_predictions <- model %>% predict(test_data)
test_results = cbind(test_predictions[1:100, 1],test_labels[1:100,1])
test_corr = cor(test_predictions, test_labels)

hist(test_predictions, breaks = 100, xlim = c(0,200))
hist(test_labels,breaks = 100,xlim = c(0,200))

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
inputs = c(colnames(df.harvest)[-ncol(df.harvest)])
export_weights_gams(model, inputs, "harvest")
