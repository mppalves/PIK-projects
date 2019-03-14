#'@author Marcos Alves
#'@Description Script to run different kinds of simulations and analysis on 
#'possible candidate formulas.
#'@date 08/03/2019

library(tidyr)
library(plotly)
library(readr)
source('simulation_module-v2.R')
load("grass_results4Marcos.RData")
global_weather_2000 <- read_csv("global_weather_2000.csv")


#'@input data pre-processing
input_data = as.data.frame(cbind(maps[,,"harvest"],rowMeans(maps[,,"harvest"])))
input_data <- input_data %>% gather("LSU", "output", "00":"20")
input_data[,2] = as.numeric(input_data[,2])/10
names(input_data) = c("x","w","y")

#####-----
#'@Experiment-1 @Yield_mean: simulation run for equation describing harvest output in relation to
#'harvest mean values and LSU. x = mean value per cell, w = LSU, y = output.
#'@function_calculation: Regession analyis run over the coefficients a and b of the 
#'exponential equation y ~ a.x.exp^(b.x). Coefficient a was regressed using the same 
#'exponential equation while coeff b was regressed using a linear function.

Run_simulation(y ~ 8.76366*exp(-0.0163042*x + (-1.56578 + 0.0103395*x)*w)*x*w, 
               w=input_data$w, 
               x=input_data$x,
               input_data = input_data)

#####-----
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

Run_simulation(y ~ 9.76366*exp(-0.0163042*x + (-1.34087 + 0.0000796483*x^2.01438)*w)*x*w, 
               w=input_data$w, 
               x=input_data$x,
               input_data = input_data)

#####-----

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

Run_simulation(y  ~ (174.973*exp((-1.08723 + 0.507974/(1 + exp(-0.0109738*(-531.41 + x))))*w)*w)/(1 + exp(-0.0171056*(-187.73 + x))), 
               w=input_data$w, 
               x=input_data$x,
               input_data = input_data)


#'@Experiment-4 @Machine_learning: simulation run for the results of machine learning model 
#'with all environmental variables.
#'Files originated with scrip ML_model.R
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_coordinates_tempM_precM_radM1_normT_reduced_updated/output_data_ML.Rdata")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_coordinates_tempM_precM_radM1_normT_reduced_updated/input_data_ML.Rdata")
Run_analysis(output_data,input_data)


