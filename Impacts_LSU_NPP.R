# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
subDir_npp <- "ML_npp_envi_soil_1000_mse_300_200_100_35_1"
setwd(file.path(mainDir, subDir_npp))

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

#Loading model NPP
col_means_train_npp = readRDS(file.path(mainDir,subDir_npp,"col_means_train_npp.Rds"))
col_stddevs_train_npp = readRDS(file.path(mainDir,subDir_npp,"col_stddevs_train_npp.Rds"))
ML_model_npp = load_model_hdf5(file.path(mainDir,subDir_npp,"model.h5"))

##################################################################################################################################

opt_lsu_real = read_rds(file.path(mainDir,"opt_lsu/opt_lsu_real_points.rds"))
opt_lsu_predicted = read_rds(file.path(mainDir,"opt_lsu/opt_lsu_predicted_points.rds"))

    #Predicting NPP for real LSU data
    map_lsu = cbind(grid, weather_dataset[,c(1,3,5)],soil_type, opt_lsu_real)
    map_lsu_scaled = scale(map_lsu,center = col_means_train_npp, scale=col_stddevs_train_npp)
    npp_predicted_harvest_real = predict(ML_model_npp, map_lsu_scaled)
    
    #Predicting NPP for predicted LSU data (removing EF, ET climate)
    if(length(opt_lsu_predicted)<60000) {
      dummy.x = cbind(grid,weather_dataset[,c(1,3,5)],soil_type,wt_f)
      df.combined = dummy.x[which(dummy.x$Cls != "EF" & dummy.x$Cls != "ET"),c(1:6)]
      map_lsu =  cbind(df.combined, opt_lsu_predicted)
    }else{
      map_lsu = cbind(grid, weather_dataset[,c(1,3,5)],soil_type, opt_lsu_predicted)    
    }
    map_lsu_scaled = scale(map_lsu,center = col_means_train_npp, scale=col_stddevs_train_npp)
    
    
    npp_predicted_harvest_predicted = predict(ML_model_npp, map_lsu_scaled)

#comparing NPP output for both models
Run_analysis(npp_predicted_harvest_real,npp_predicted_harvest_predicted, "Impact LSU on NPP points", cor = "blue")

    #ploting opt_npp on Map
    plot_data_npp_predicted_harvest_real = cbind(npp_predicted_harvest_real, grid)
    create_map(plot_data_npp_predicted_harvest_real, map_name = "NPP_predicted_harvest_real_points", save_jpg = T)
    
    #ploting opt_npp on Map
    plot_data_npp_predicted_harvest_predicted = cbind(npp_predicted_harvest_predicted, grid)
    create_map(plot_data_npp_predicted_harvest_predicted, map_name = "NPP_predicted_harvest_predicted_points", save_jpg = T)
