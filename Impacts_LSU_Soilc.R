# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
# subDir_harvest <- "ML_harvest_envi_soil_300_mae_300_200_100_35_1"
subDir <- "ML_soilc_envi_soil_300_mse_300_200_100_35_1"
setwd(file.path(mainDir, subDir))

#loading basic data
load(file.path(mainDir,"grass_results4Marcos.RData"))
weather_dataset = read_csv(file.path(mainDir,"global_weather_2000.csv"))
load(file.path(mainDir,"LPJML_inputs/soil_type.Rdata"))
load(file.path(mainDir,"wt_f.Rdata"))

#loading helper functions
source(file.path(mainDir,"create_map.R"))
source(file.path(mainDir,"generate_test_data.R"))
source(file.path(mainDir,"reconstruct_dataset.R"))
source(file.path(mainDir,"analysis_functions.R"))

#################################################################################################################################

# #loading model harvest soil
# col_means_train_harvest = readRDS(file.path(mainDir,subDir_harvest,"col_means_train_harvest.Rds"))
# col_stddevs_train_harvest = readRDS(file.path(mainDir,subDir_harvest,"col_stddevs_train_harvest.Rds"))
# ML_model_harvest= load_model_hdf5(file.path(mainDir,subDir_harvest,"model.h5"))

#Loading model soilc
col_means_train_soilc = readRDS(file.path(mainDir,subDir,"col_means_train_soilc.Rds"))
col_stddevs_train_soilc = readRDS(file.path(mainDir,subDir,"col_stddevs_train_soilc.Rds"))
ML_model_soilc = load_model_hdf5(file.path(mainDir,subDir,"model.h5"))

##################################################################################################################################

# #generate harvest data
# map_harvest = maps[,,"harvest"]
# test_data = generate_test_data(map_harvest, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, soil_type = soil_type)
# test_data_unscaled = generate_test_data(map_harvest, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, scaled = F, soil_type = soil_type)
# 
# #Reconstructing harvest dataset
# harvest_predicted = predict(ML_model_harvest, test_data)
# map_harvest_predicted = recostruct_dataset(test_data_unscaled, harvest_predicted)
# 
# #finding the Optimum LSU values 
# opt_lsu_real = as.numeric(colnames(map_harvest)[apply(map_harvest,1,which.max)])/10
# opt_lsu_predicted = as.numeric(colnames(map_harvest_predicted)[apply(map_harvest_predicted,1,which.max)])
opt_lsu_real = read_rds(file.path(mainDir,"opt_lsu/opt_lsu_real.rds"))
opt_lsu_predicted = read_rds(file.path(mainDir,"opt_lsu/opt_lsu_predicted.rds"))


#Predicting soilc for real LSU data
map_lsu = cbind(grid, weather_dataset[,c(1,3,5)],soil_type, opt_lsu_real)
map_lsu_scaled = scale(map_lsu,center = col_means_train_soilc, scale=col_stddevs_train_soilc)
soilc_predicted_harvest_real = predict(ML_model_soilc, map_lsu_scaled)


#Predicting soilc for predicted LSU data removing EF, ET climate
if(length(opt_lsu_predicted)<60000) {
  dummy.x = cbind(grid,weather_dataset[,c(1,3,5)],soil_type,wt_f)
  df.combined = dummy.x[which(dummy.x$Cls != "EF" & dummy.x$Cls != "ET"),c(1:6)]
  map_lsu =  cbind(df.combined, opt_lsu_predicted)
}
map_lsu = cbind(grid, weather_dataset[,c(1,3,5)],soil_type, opt_lsu_predicted)    
map_lsu_scaled = scale(map_lsu,center = col_means_train_soilc, scale=col_stddevs_train_soilc)
soilc_predicted_harvest_predicted = predict(ML_model_soilc, map_lsu_scaled)

#comparing soilc output for both models
Run_analysis(soilc_predicted_harvest_real,soilc_predicted_harvest_predicted,"Soil C predicted for opt LSU", cor = "red")

#ploting opt_soilc on Map
plot_data_soilc_predicted_harvest_real = cbind(soilc_predicted_harvest_real, grid)
create_map(plot_data_soilc_predicted_harvest_real, map_name = "soilc_predicted_harvest_real", save_jpg = T)

#ploting opt_soilc on Map
plot_data_soilc_predicted_harvest_predicted = cbind(soilc_predicted_harvest_predicted, grid)
create_map(plot_data_soilc_predicted_harvest_predicted, map_name = "soilc_predicted_harvest_predicted", save_jpg = T)

