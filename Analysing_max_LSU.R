# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
subDir <- "ML_harvest25_envi_soil_500_mse_300_200_100_35_1"
setwd(file.path(mainDir, subDir))

#loading basic data
load(file.path(mainDir,"grass_results4Marcos.RData"))
weather_dataset = read_csv(file.path(mainDir,"global_weather_2000.csv"))
load(file.path(mainDir,"LPJML_inputs/soil_type.Rdata"))
harvest = readRDS(file.path(mainDir,"GrassData2/df_harvest25.rds"))

#loading helper functions
source(file.path(mainDir,"create_map.R"))
source(file.path(mainDir,"generate_test_data.R"))
source(file.path(mainDir,"reconstruct_dataset.R"))
source(file.path(mainDir,"analysis_functions.R"))

#loading model with soil
col_means_train_harvest = readRDS(file.path(mainDir,subDir,"col_means_train_harvest.Rds"))
col_stddevs_train_harvest = readRDS(file.path(mainDir,subDir,"col_stddevs_train_harvest.Rds"))
ML_model= load_model_hdf5(file.path(mainDir,subDir,"model.h5"))
soil = soil_type

##################################################################################

#generate data
#map_harvest = maps[,,"harvest"]
map_harvest = harvest
test_data = generate_test_data(map_harvest, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, soil_type = soil 
                               )
test_data_unscaled = generate_test_data(map_harvest, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, scaled = F, soil_type = soil
                              )

#Reconstructing harvest dataset
harvest_predicted = predict(ML_model, test_data)
map_harvest_predicted = reconstruct_dataset(test_data_unscaled, harvest_predicted)

#finding the maximum LSU values
opt_lsu_real = as.numeric(colnames(map_harvest)[apply(map_harvest,1,which.max)])/10
opt_lsu_predicted = as.numeric(colnames(map_harvest_predicted)[apply(map_harvest_predicted,1,which.max)])

setwd(file.path(mainDir, "opt_lsu"))
saveRDS(opt_lsu_real, file = file.path(mainDir,"opt_lsu/opt_lsu_real_points1.rds"))
saveRDS(opt_lsu_predicted, file = file.path(mainDir,"opt_lsu/opt_lsu_predicted_points1.rds"))

#Run analysis comparing the maximum LSUs
Run_analysis(opt_lsu_predicted,opt_lsu_real, title = "Optimum LSU points 25", cor = "red")

#ploting on map
#ploting original
plot_data_harvest_real = cbind(opt_lsu_real, grid)
create_map(plot_data_harvest_real, save_jpg = T, map_name = "LSU_Optimal_real_points", cathegorical = F, color_range = c("#FFFFFF", "#FF0000"), limits = c(0,2.5))

#ploting predicted
plot_data_harvest_predicted = cbind(opt_lsu_predicted, grid)
create_map(plot_data_harvest_predicted, save_jpg = T, map_name = "LSU_Optimal_predicted_points",  cathegorical = F, color_range = c("#FFFFFF", "#FF0000"), limits = c(0,2.5))
