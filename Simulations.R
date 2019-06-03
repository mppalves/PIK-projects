#'@author Marcos Alves
#'@Description Script to run different kinds of simulations and analysis on 
#'possible candidate formulas.
#'@date 08/03/2019

library(tidyr)
library(plotly)
library(readr)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"


source(file.path(mainDir,"analysis_functions.R"))
load(file.path(mainDir,"grass_results4Marcos.RData"))
global_weather_2000 <- read_csv(file.path(mainDir,"global_weather_2000.csv"))

#################################
###Harvest experiments###########

# input data pre-processing
input_data = as.data.frame(cbind(maps[,,"harvest"],rowMeans(maps[,,"harvest"])))
input_data <- input_data %>% gather("LSU", "output", "00":"20")
input_data[,2] = as.numeric(input_data[,2])/10
names(input_data) = c("x","w","y")

###----Mechanistic models---###

#'@Experiment-1 @Yield_mean: simulation run for equation describing harvest output in relation to
#'harvest mean values and LSU. x = mean value per cell, w = LSU, y = output.
#'@function_calculation: Regession analyis run over the coefficients a and b of the 
#'exponential equation y ~ a.x.exp^(b.x). Coefficient a was regressed using the same 
#'exponential equation while coeff b was regressed using a linear function.
dir.create(file.path(mainDir,  "Formulas","Model_Evaluation-1"), showWarnings = T)
setwd(file.path(mainDir, "Formulas","Model_Evaluation-1"))
Run_simulation(func = y ~ 8.76366*exp(-0.0163042*x + (-1.56578 + 0.0103395*x)*w)*x*w, 
               w=input_data$w, 
               x=input_data$x,
               input_data = input_data, 
               title = "Formula Evaluation Harvest, Yield mean",
               cor = "#217A00",
               comment = "y ~ 8.76366*exp(-0.0163042*x + (-1.56578 + 0.0103395*x)*w)*x*w")


#'@Experiment-2 @Yield_mean: simulation run for equation describing harvest output in relation to
#'harvest mean values and LSU. x = mean value per cell, w = LSU, y = output.
#'@function_calculation: Regession analyis run over the coefficients a and b of the 
#'exponential equation y ~ a.x.exp^(b.x). Coefficient a was regressed using the same 
#'exponential equation while coeff b was regressed using a non-linear function: 
#'b = sigma + (x/rho)^theta.

#Original from regression
#y ~ 8.76366 exp(-0.0163042*x + (-1.34087 + 0.0000796483*x^2.06438)*w)*x*w

#Manually fine-tuned function (optimum)
#y ~ 9.76366*exp(-0.0163042*x + (-1.34087 + 0.0000796483*x^2.01438)*w)*x*w
dir.create(file.path(mainDir,  "Formulas","Model_Evaluation-2"), showWarnings = T)
setwd(file.path(mainDir, "Formulas","Model_Evaluation-2"))
Run_simulation(y ~ 9.76366*exp(-0.0163042*x + (-1.34087 + 0.0000796483*x^2.01438)*w)*x*w, 
               w=input_data$w, 
               x=input_data$x,
               input_data = input_data,title = "Formula Evaluation Harvest, Yield mean",
               cor = "red",
               comment = "y ~ 9.76366*exp(-0.0163042*x + (-1.34087 + 0.0000796483*x^2.01438)*w)*x*w")


#'@input data pre-processing for wheather
input_data = as.data.frame(cbind(maps[,,"harvest"], global_weather_2000[,"precipitation_mean"]))
input_data <- input_data %>% gather("LSU", "output", "00":"20")
input_data[,2] = as.numeric(input_data[,2])/10
names(input_data) = c("x","w","y")

#'@Experiment-3 @Preciptation: simulation run for equation describing harvest output in relation to
#'preciptation and LSU. x = preciptation per cell, w = LSU, y = output.
#'@function_calculation: Regession analyis run over the coefficients a and b of the 
#'exponential equation y ~ a.x.exp^(b.x). Coefficient a was regressed using the same 
#'exponential equation while coeff b was regressed using a non-linear function: 
#'b = sigma + (x/rho)^theta.
dir.create(file.path(mainDir,  "Formulas","Model_Evaluation-3"), showWarnings = T)
setwd(file.path(mainDir, "Formulas","Model_Evaluation-3"))
Run_simulation(y  ~ (174.973*exp((-1.08723 + 0.507974/(1 + exp(-0.0109738*(-531.41 + x))))*w)*w)/(1 + exp(-0.0171056*(-187.73 + x))), 
               w=input_data$w, 
               x=input_data$x,
               input_data = input_data, "Formula Evaluation Harvest, Preciptation mean",
               cor = "red",
               comment = "y  ~ (174.973*exp((-1.08723 + 0.507974/(1 + exp(-0.0109738*(-531.41 + x))))*w)*w)/(1 + exp(-0.0171056*(-187.73 + x)))")


###----Machine Learning---###

#'@Experiment-4 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental variables.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_200_mse_100_50_30_1/output_data_ML.Rdata")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_200_mse_100_50_30_1/input_data_ML.Rdata")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_200_mse_100_50_30_1/Model_Evaluation", showWarnings = T)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_200_mse_100_50_30_1/Model_Evaluation")
Run_analysis(output_data,input_data, title = "Model Evaluation harvest", cor = "green")

#'@Experiment-5 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental and SOIL_type variable for harvest.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_soil_300_mae_300_200_100_35_1/output_data_ML.Rdata")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_soil_300_mae_300_200_100_35_1/input_data_ML.Rdata")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_soil_300_mae_300_200_100_35_1/Model_Evaluation", showWarnings = T)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest_envi_soil_300_mae_300_200_100_35_1/Model_Evaluation")
Run_analysis(output_data,input_data,title = "Model Evaluation harvest",cor = "green")

ML_harvest40_envi_soil_10_mse_300_200_100_35_1

#'@Experiment-8 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental and SOIL_type variable for harvest with max 4 animals.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
output_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest40_envi_soil_50_mse_300_200_100_35_1/output_data_ML.Rds")
input_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest40_envi_soil_50_mse_300_200_100_35_1/input_data_ML.Rds")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest40_envi_soil_50_mse_300_200_100_35_1/Model_Evaluation", showWarnings = T)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest40_envi_soil_50_mse_300_200_100_35_1/Model_Evaluation")
Run_analysis(output_data,input_data,title = "Model Evaluation harvest 40",cor = "green")


#'@Experiment-9 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental and SOIL_type variable for harvest  with max 2.5 animals.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
output_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu/output_data_ML.Rds")
input_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu/input_data_ML.Rds")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu/Model_Evaluation", showWarnings = T)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu/Model_Evaluation")
Run_analysis(output_data,input_data,title = "Model Evaluation harvest Elu",cor = "green")


#'@Experiment-10 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental and SOIL_type variable for harvest  with max 2.5 animals sigmoid functiont.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
output_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_10_mse_300_200_100_35_1_sigmoid/output_data_ML.Rds")
input_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_10_mse_300_200_100_35_1_sigmoid/input_data_ML.Rds")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_10_mse_300_200_100_35_1_sigmoid/Model_Evaluation", showWarnings = F)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_10_mse_300_200_100_35_1_sigmoid/Model_Evaluation")
Run_analysis(output_data,input_data,title = "Model Evaluation harvest 25 sigmoid",cor = "green")

#'@Experiment-11 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental and SOIL_type variable for harvest  with max 2.5 animals softplus functiont.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
output_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_softplus/output_data_ML.Rds")
input_data = read_rds("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_softplus/input_data_ML.Rds")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_softplus/Model_Evaluation", showWarnings = F)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_softplus/Model_Evaluation")
Run_analysis(output_data,input_data,title = "Model Evaluation harvest 25 sigmoid",cor = "red")



#############################
### NPP experiments #########

#'@Experiment-6 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental variables for NPP.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
output_data_npp = readRDS("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_npp_envi_soil_1000_mse_300_200_100_35_1/output_data_ML.Rds")
input_data_npp = readRDS("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_npp_envi_soil_1000_mse_300_200_100_35_1/input_data_ML.Rds")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_npp_envi_soil_1000_mse_300_200_100_35_1/Model_Evaluation", showWarnings = T)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_npp_envi_soil_1000_mse_300_200_100_35_1/Model_Evaluation")
Run_analysis(output_data_npp,input_data_npp, title = "Model Evaluation NPP", cor = "blue")

################################
### Soilc experiments ##########

#'@Experiment-7 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental variables for soilc.
#'Files originated with scrip ML_model.R line 172 pik-projects folder
output_data_soilc = readRDS("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_soilc25_envi_soil_200_mse_300_200_100_35_1_sigmoid/output_data_ML.Rds")
input_data_soilc = readRDS("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_soilc25_envi_soil_200_mse_300_200_100_35_1_sigmoid/input_data_ML.Rds")
dir.create("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_soilc25_envi_soil_200_mse_300_200_100_35_1_sigmoid/Model_Evaluation", showWarnings = T)
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_soilc25_envi_soil_200_mse_300_200_100_35_1_sigmoid/Model_Evaluation")
Run_analysis(output_data_soilc,input_data_soilc, title = "Model Evaluation Soilc", cor = "red")

hist(input_data_soilc)
