# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
subDir <- "ML_soilc25_envi_soil_200_mse_300_200_100_35_1_sigmoid"
setwd(file.path(mainDir, subDir))

#loading basic data
load(file.path(mainDir,"grass_results4Marcos.RData"))
weather_dataset = read_csv(file.path(mainDir,"global_weather_2000.csv"))
load(file.path(mainDir,"LPJML_inputs/soil_type.Rdata"))
load(file.path(mainDir,"wt_f.Rdata"))
soilc = readRDS(file.path(mainDir,"GrassData2/df_soilc25.rds"))

#loading helper functions
source(file.path(mainDir,"create_map.R"))
source(file.path(mainDir,"generate_test_data.R"))
source(file.path(mainDir,"reconstruct_dataset.R"))
source(file.path(mainDir,"analysis_functions.R"))

#################################################################################################################################

#Loading model soilc
col_means_train_soilc = readRDS(file.path(mainDir,subDir,"col_means_train_soilc.Rds"))
col_stddevs_train_soilc = readRDS(file.path(mainDir,subDir,"col_stddevs_train_soilc.Rds"))
ML_model_soilc = load_model_hdf5(file.path(mainDir,subDir,"model.h5"))

##################################################################################################################################

#generate soilc harvest data
test_data = generate_test_data(soilc, col_means_train_soilc, col_stddevs_train_soilc, grid, weather_dataset, soil_type = soil_type)
test_data_unscaled = generate_test_data(soilc, col_means_train_soilc, col_stddevs_train_soilc, grid, weather_dataset, scaled = F, soil_type = soil_type)

#Reconstructing harvest dataset
soilc_predicted = predict(ML_model_soilc, test_data)
map_soilc_predicted = reconstruct_dataset(test_data_unscaled, soilc_predicted)

#finding max soilc 
max_soil_real = apply(soilc,1,max)
max_soil_predicted = apply(map_soilc_predicted,1,max)

#########################################################################
####LSU####

#finding max LSU soilc
opt_lsu_original_soilc = as.numeric(colnames(soilc)[apply(soilc,1,which.max)])/10
opt_lsu_predicted_soilc = as.numeric(colnames(map_soilc_predicted)[apply(map_soilc_predicted,1,which.max)])

#loading max LSU harvest
opt_lsu_real = read_rds(file.path(mainDir,"opt_lsu/opt_lsu_real.rds"))
opt_lsu_predicted = read_rds(file.path(mainDir,"opt_lsu/opt_lsu_predicted.rds"))

#comparing he two LSU outputs
Run_analysis(opt_lsu_predicted,opt_lsu_predicted_soilc,"Max LSU opt comparison (soilc vs Carbon)", cor = "red")

#########################################################################
####impacts evaluation####

#Predicting soilc for predicted LSU data removing EF, ET climate
if(length(opt_lsu_predicted)<60000) {
  dummy.x = cbind(grid,weather_dataset[,c(1,3,5)],soil_type,wt_f)
  df.combined = dummy.x[which(dummy.x$Cls != "EF" & dummy.x$Cls != "ET"),c(1:6)]
  map_lsu =  cbind(df.combined, opt_lsu_predicted)
}
map_lsu = cbind(grid, weather_dataset[,c(1,3,5)],soil_type, opt_lsu_predicted)    
map_lsu_scaled = scale(map_lsu,center = col_means_train_soilc, scale=col_stddevs_train_soilc)
soilc_predicted_harvest_predicted = predict(ML_model_soilc, map_lsu_scaled)

#change in soil carbon
change_soil_carbon =   max_soil_predicted - soilc_predicted_harvest_predicted
change_percent = change_soil_carbon*100/max_soil_predicted
hist(change_percent, breaks =10000,  xlim = c(0,100))
median(change_percent)

plot(change_percent)

#comparing soilc output for two scenarios no LSU and opt LSU
Run_analysis(soilc_predicted_harvest_predicted,max_soil_predicted,"Soil C predicted vs max carbon", cor = "red")

20728.79  - 13889.39 
#########################################################################
####maps####

#ploting max_soilc on Map
plot_data_max = cbind(max_soil_predicted, grid)
create_map(plot_data_max, map_name = "max_soil_predicted", save_jpg =  list(T, width = 48, height = 38.4), color_range = c("#FFFFFF", "#910B0B"))

#ploting soilc_predicted_harvest_predicted on Map
plot_data_max_pred = cbind(soilc_predicted_harvest_predicted, grid)
create_map(plot_data_max_pred, map_name = "soilc_predicted_harvest_predicted", save_jpg =  list(T, width = 32, height = 25.6),color_range = c("#FFFFFF", "#910B0B"))

#ploting change_soil_carbon on Map
plot_data_change = cbind(change_soil_carbon, grid)
create_map(plot_data_change, map_name = "change_soil_carbon", save_jpg =  list(T, width = 48, height = 38.4),color_range = c("#FFFFFF", "#EB6A00"))


#ploting change_percent on Map
plot_data_change = cbind(change_percent, grid)
create_map(plot_data_change, map_name = "change_percent", save_jpg =  list(T, width = 48, height = 38.4),color_range = c("#FFFFFF", "#9700BD"), limits = c(0,5))

