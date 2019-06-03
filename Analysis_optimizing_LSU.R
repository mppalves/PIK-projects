#' @title File to analyze the impacts of LSU optmization in the Soilc content
library(keras)
library(ggplot2)
library(tidyr)
library(readr)
set.seed(310)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
subDir <- "ML_harvest25_envi_soil_30_mse_300_200_100_35_1_softplus"
setwd(mainDir)

#loading axiliary functions
source(file.path(mainDir,"optimize_lsu.R"))
source(file.path(mainDir,"generate_test_data.R"))
source(file.path(mainDir,'ploting_individual_cells.R'))
source(file.path(mainDir,'create_map.R'))

#loading basic data
weather <- read_csv(file.path(mainDir,"global_weather_2000.csv"))
load(file.path(mainDir,"grass_results4Marcos.RData"))
load(file.path(mainDir,"LPJML_inputs/soil_type.Rdata"))
harvest = readRDS(file.path(mainDir,"GrassData2/df_harvest25.rds"))

#loading model harvest with soil
col_means_train_harvest = readRDS(file.path(mainDir,subDir,"col_means_train_harvest.Rds"))
col_stddevs_train_harvest = readRDS(file.path(mainDir,subDir,"col_stddevs_train_harvest.Rds"))
load(file.path(mainDir,"wt_f.Rdata"))
ML_model_harvest = load_model_hdf5(file.path(mainDir,subDir,"model.h5"))
soil = soil_type

#generating test data
test_data = generate_test_data(harvest, col_means_train_harvest, col_stddevs_train_harvest, grid, weather, soil_type = soil, scaled = T)

#Optmizing all cells
start_time <- Sys.time()
optimum_lsu_cell = Optimize_LSU(test_data, ML_model_harvest,min_Lsu = -2, max_Lsu = 2,col_means_train_harvest,col_stddevs_train_harvest, range = 28415:28416)
end_time <- Sys.time()

#Predicting output on optmization
test = test_data[1,]
test["LSU"] = optimum_lsu_cell[1,1]

predict(ML_model_harvest, t(test))

#Total run time
end_time - start_time

#ploting optmized cells
create_map(optimum_lsu_cell)

#visualizing optimization on individual cells
ploting_individual_cells(test_data=test_data, cell = 28415,Lsu_range =c(-1.5,1.5) ,climate_variable = "precipitation_mean",climate_variable_range = c(-1.5,1.5), ML_model_harvest)
#9040 central USA
#18652 Iguaçu
#20050 central brazil
#25000 North Spain
#27000 North Africa  
#28000 South Germany
#28011 South Germany
#28415 Norway
#63000 east russia
#59000 west australia
#58760 south west china
map_cell_cor = cbind(seq(1,nrow(grid),1),grid)
create_map(map_cell_cor, color_range = c("#FFFFFF", "#FF0000", "#44FF00", "#F700FF", "#FFD900"))



