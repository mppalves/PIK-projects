# @title: function to check the maximum evaluating the table
library(keras)
library(readr)
library(ggfortify)
library(GSTools)

#setting the directories
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient"
setwd(file.path(mainDir))

#loading basic data
grid_ambient = readRDS(file.path(mainDir,"grid_ambient.Rds"))

#loading LPJmL simulations
map_harvest_2000 =  read_rds("harvest_ambient_2000.rds")
map_harvest_2099 =  read_rds("harvest_ambient_2099.rds")
##################################################################################

#finding the maximum Harvest values 
opt_harvest_2000 = apply(map_harvest_2000,1,max)
opt_harvest_2099 = apply(map_harvest_2099,1,max)

# saveRDS(opt_harvest_2000, file = file.path(mainDir,"opt_lsu/opt_harvest_2000.rds"))
# saveRDS(opt_harvest_2099, file = file.path(mainDir,"opt_lsu/opt_harvest_2099.rds"))

# climate change scenarios
# library(readr)
# opt_harvest_2099 = read_rds(file.path(mainDir,"opt_lsu/opt_harvest_2099.rds"))


#Run analysis comparing the maximum values 
Run_analysis(opt_harvest_2099,opt_harvest_2000, "LPJmL climate simulation (2000 - 2099)", cor = "green")

#ploting on map
#ploting original
plot_data_harvest = cbind(opt_harvest_2000, grid_ambient)
# create_map(plot_data_harvest, save_jpg = T, map_name = "Harvest_optimal_production_real_points", cathegorical = F, color_range = c("#FF0000", "#FFFF00", "#2200FF"),limits = c(-53,53))
create_map(plot_data_harvest, map_name = "LPJmL climate simulation (2000)", cathegorical = F, color_range = c("#FFFFFF", "#006311"), limits = c(0,283))

#ploting predicted
plot_data_harvest_predicted = cbind(opt_harvest_2099, grid_ambient)
# create_map(plot_data_harvest_predicted, save_jpg = T, map_name = "Harvest_optimal_production_predicted_points",cathegorical = F, color_range = c("#FF0000", "#FFFF00", "#2200FF"), limits = c(-53,53))
create_map(plot_data_harvest_predicted, save_jpg =  list(T, width = 80, height = 64), map_name = "LPJmL climate simulation (2099)",cathegorical = F, color_range = c("#FFFFFF", "#006311"), limits = c(0,283))
saveRDS(plot_data_harvest_predicted, file = "max_harvest_cell.rds")

##########################################################################
#Evaluating climate change scenario
change = opt_harvest_2099 - opt_harvest_2000
hist(change)
mean(change)
map_change = cbind(change,grid_ambient)
create_map(map_change, map_name = "LPJmL climate simulation (change)",cathegorical = F, color_range = c("#FF0F47", "#FFFFFF", "#59BF34DB"), limits = c(-75,75))

###########################################################################

