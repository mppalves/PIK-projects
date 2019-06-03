# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
subDir <- "ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu"
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
#climate change
# weather_dataset[,3] = weather_dataset[,3] - 72
# weather_dataset[,1] = weather_dataset[,1] + 1

#generate data
#map_harvest_real = maps[,,"harvest"]
#points = as.list(seq(0,20,0.1)) Adding dummy points for model evaluation
map_harvest_real = harvest
test_data = generate_test_data(map_harvest_real, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, soil_type = soil)
test_data_unscaled = generate_test_data(map_harvest_real, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, scaled = F, soil_type = soil)
#, points = as.list(seq(0,25,0.5))

#Reconstructing harvest dataset
harvest_predicted = predict(ML_model, test_data)
map_harvest_predicted = reconstruct_dataset(test_data_unscaled, harvest_predicted)
#saveRDS(map_harvest_predicted,"map_harvest_predicted.rds")

#finding the maximum Harvest values 
opt_harvest_real = apply(map_harvest_real,1,max)
opt_harvest_predicted = apply(map_harvest_predicted,1,max)

# saveRDS(opt_harvest_real, file = file.path(mainDir,"opt_lsu/opt_harvest_real.rds"))
# saveRDS(opt_harvest_predicted, file = file.path(mainDir,"opt_lsu/opt_harvest_predicted.rds"))

# climate change scenarios
# library(readr)
# opt_harvest_predicted = read_rds(file.path(mainDir,"opt_lsu/opt_harvest_predicted.rds"))


#Run analysis comparing the maximum values 
Run_analysis(opt_harvest_predicted,opt_harvest_real, "Harvest optimal production points 0.1", cor = "green")

#ploting on map
#ploting original
plot_data_harvest = cbind(opt_harvest_real, grid)


# create_map(plot_data_harvest, save_jpg = T, map_name = "Harvest_optimal_production_real_points", cathegorical = F, color_range = c("#FF0000", "#FFFF00", "#2200FF"),limits = c(-53,53))
create_map(plot_data_harvest, save_jpg = T, map_name = "Harvest_optimal_production_real_points", cathegorical = F, color_range = c("#FFFFFF", "#006311"), limits = c(0,283))

#ploting predicted
plot_data_harvest_predicted = cbind(opt_harvest_predicted, grid)
# create_map(plot_data_harvest_predicted, save_jpg = T, map_name = "Harvest_optimal_production_predicted_points",cathegorical = F, color_range = c("#FF0000", "#FFFF00", "#2200FF"), limits = c(-53,53))
create_map(plot_data_harvest_predicted, save_jpg =  list(T, width = 80, height = 64), map_name = "Harvest_optimal_production_predicted_points",cathegorical = F, color_range = c("#FFFFFF", "#006311"), limits = c(0,283))
saveRDS(plot_data_harvest_predicted, file = "max_harvest_cell.rds")

##########################################################################
#building climate change scenario
opt_harvest_prec_red = readRDS(file.path(mainDir,"opt_lsu/opt_harvest_predicted_prec_red.rds"))
opt_harvest_no_prec_red= readRDS(file.path(mainDir,"opt_lsu/opt_harvest_predicted.rds"))
change = opt_harvest_prec_red - opt_harvest_no_prec_red
hist(change)
mean(change)
map_change = cbind(change,grid)
create_map(map_change, save_jpg = T, map_name = "Climate change scenario 10 percent preciptation reduction + 1 degree temp increase",cathegorical = F, color_range = c("#FF0F47", "#FFFFFF", "#59BF34DB"), limits = c(-75,75))

###########################################################################

