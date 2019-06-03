# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)

#setting the directories for loading
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

######################################################################

mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
subDir <- "ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu"
climate = "climate_change Scenarios"
dir.create(file.path(mainDir, subDir, climate), showWarnings = F)
setwd(file.path(mainDir, subDir,climate))

#generate data
#points = as.list(seq(0,20,0.1)) Adding dummy points for model evaluation
map_harvest_real = harvest

test_data = generate_test_data(map_harvest_real, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, soil_type = soil)
test_data_unscaled = generate_test_data(map_harvest_real, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_dataset, scaled = F, soil_type = soil)

#Reconstructing harvest dataset
harvest_predicted = predict(ML_model, test_data)
map_harvest_predicted = reconstruct_dataset(test_data_unscaled, harvest_predicted)
#saveRDS(map_harvest_predicted,"map_harvest_predicted.rds")

#finding the maximum Harvest values 
opt_harvest_predicted = apply(map_harvest_predicted,1,max)
create_map(cbind(opt_harvest_predicted,grid))

######################################################
#loading climate change scenarios and checking
precipitation2100 = read_rds(file.path(mainDir,"LPJML_inputs/prec_2100.rds"))
temperature2100 = read_rds(file.path(mainDir,"LPJML_inputs/temp_2100.rds"))
#ordeing as the grid
precipitation2100 = precipitation2100[order(precipitation2100$y),]
precipitation2100 = precipitation2100[order(precipitation2100$x),]
temperature2100 = temperature2100[order(temperature2100$y),]
temperature2100 = temperature2100[order(temperature2100$x),]

#climate change
weather_dataset = as.data.frame(weather_dataset)

# weather_dataset[,"precipitation_mean"]  = weather_dataset[,"precipitation_mean"] + 300
change_temp = temperature2100[,1]- weather_dataset[,"temperature_mean"]
change_prec = precipitation2100[,1]- weather_dataset[,"precipitation_mean"]

#checagem de mapa
# create_map(cbind(temperature2100[,1],grid),map_name = "2100_temp")
# create_map(cbind(weather_dataset[,"temperature_mean"],grid),map_name = "today_temp")
#create_map(cbind(change_prec,grid),map_name = "change_temp", color_range = c("#3860FF", "#FFFFFF", "#E61717"), limits = c(-5000,5000))

#creating wheater com climate change
weather_climate_change = weather_dataset
weather_climate_change[,"temperature_mean"] = temperature2100[,1]
weather_climate_change[,"precipitation_mean"] = precipitation2100[,1]

########################################################################

#col_means_train_harvest_climate_change = col_means_train_harvest["temperature_mean"]
test_data_climate_change = generate_test_data(map_harvest_real, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_climate_change, soil_type = soil)
test_data_unscaled_climate_change = generate_test_data(map_harvest_real, col_means_train_harvest, col_stddevs_train_harvest, grid, weather_climate_change, scaled = F, soil_type = soil)

#Reconstructing harvest dataset
harvest_predicted_climate_change = predict(ML_model, test_data_climate_change)
map_harvest_predicted_climate_change = reconstruct_dataset(test_data_unscaled_climate_change, harvest_predicted_climate_change)
#saveRDS(map_harvest_predicted,"map_harvest_predicted.rds")

#finding the maximum Harvest values 
opt_harvest_predicted_climate_change = apply(map_harvest_predicted_climate_change,1,max)
change =  opt_harvest_predicted_climate_change - opt_harvest_predicted

Run_analysis(opt_harvest_predicted_climate_change,opt_harvest_predicted, "Climate change increase RCP8.5 (prec)", cor = "red")
map_change = cbind(change,grid)
create_map(map_change, save_jpg = list(T,32,25.6), map_name = "Climate change scenario RCP8.5 (prec)", cathegorical = F, color_range = c("#FF0F47", "#FFFFFF", "#59BF34DB"), limits = c(-200,200))
mean(change)