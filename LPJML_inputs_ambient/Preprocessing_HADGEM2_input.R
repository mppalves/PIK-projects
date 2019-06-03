#Script to manipulate LPJmL output of various simulations and organize them in a 3d matrix for several years

library(PIKTools)
library(LandMark)
library(readr)

#################################################################################
#BASIC PARAMETERS
setwd("/home/pedrosa/LPJmLClimateScenario1951-2099/ambient")
dirs = list.dirs(path = "/home/pedrosa/LPJmLClimateScenario1951-2099/ambient")[-1]
file_name = "/pft_harvest.pft.bin"
wyears = seq(1951,2011,10)
soilcells = F
if (soilcells) {
  cells = 59199
}else{
  cells = 67420
}

#################################################################################
#HARVEST EXTRACTION
harvest = array(NaN,dim = c(cells,length(wyears),length(dirs)), 
                dimnames = list(1:cells,wyears,basename(dirs)))

for (i in 1:length(dirs)) {
  for (j in 1:length(wyears)) {
    tmp <- (readLPJBin(file_name = file_name, file_folder =  dirs[i], 
                       wyears=wyears[j],syear=1951, years=149,bands=32, soilcells = soilcells,
                       bytes = 4, ncells = 67420, z_dim = "category")[[14]])
    tmp = rasterToPoints(tmp,spatial=T)
    tmp = as.matrix(as.data.frame(tmp)[1])
    
    harvest[,j,i] = tmp
  }
}

dimnames(harvest)[[3]] = c("00","02","04","06","08","10","12","14","16","18","20","25")

#saving the file for all years and the same LSU allows to study the changes over the years in individual cells.
saveRDS(harvest, file = paste0("harvest_",wyears[1],"-",wyears[length(wyears)],".rds"))

#----- from cluster to local pc
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient")
harvest = read_rds("harvest_1951-2011.rds")

#quick evaluation of grass growth averages per LSU in different years
  plot(apply(harvest[,,1], 2, mean),ylim=c(0,200) , type="l")
  for (i in 2:12) {
  lines(apply(harvest[,,i], 2, mean))
  }

###----
#Converting the dataset to be organized by year and be ready to be merged with the climate variables.
harvest_lsu = array(NaN,dim = c(cells,12,length(wyears)),dimnames = list(1:cells,c("00","02","04","06","08","10","12","14","16","18","20","25"),wyears))
#harvest_lsu = array(NaN,dim = c(cells,length(dirs),length(wyears)),dimnames = list(1:cells,basename(dirs),wyears)) #cluster version

for (year in dimnames(harvest)[[2]]) {
  harvest_lsu[,,year] = harvest[,year,]
}
saveRDS(harvest_lsu, file = paste0("harvest_lsu_",wyears[1],"-",wyears[length(wyears)],".rds"))
harvest_lsu = read_rds("harvest_lsu_1951-2011.rds")

#quick evaluation of grass growth averages per year
  plot(apply(harvest_lsu[,,1], 2, mean),ylim=c(0,100) , type="l")
  for (i in 2:7) {
    lines(apply(harvest_lsu[,,i], 2, mean))
  }
  
#################################################################################
#CREATING GRID
grid_harvest <- (readLPJBin(file_name = "pft_harvest.pft.bin", file_folder = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/", wyears=2000,syear=1951, years=149,bands=32, soilcells = F,
                              bytes = 4, ncells = 67420, z_dim = "category")[[14]])
grid_harvest = rasterToPoints(grid_harvest,spatial=T)
grid_harvest = as.matrix(as.data.frame(grid_harvest)[c(2,3)])
  
########################################################################################################################
#TEMPERATURE CLIMATE SCENARIO
file = "tas_mon_HadGEM2-AO_r1i1p1_rcp85_190001-209912.qmbc_CRU_TS3.21.clm"
dir = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/"
temperature = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"temperature",wyears))
for (i in 1:length(wyears)) {
  temp<-readLPJBin(file_name = file, file_folder = dir, wyears = wyears[i], syear = 1901, bands =12,
                   ncells = 67420, headlines = 43, bytes = 2, datatype = "integer", file_type="input", time_unit = "months")
  r_temp = rasterToPoints(temp,spatial=TRUE)
  data_temp = rowMeans(as.data.frame(r_temp)[,-c(13,14)])/10
  temperature[,,i] = data_temp
}
# grid_temperature = as.data.frame(r_temp)[,c(13,14)] #not necessary once grid_harvest is the same
# temperature = cbind(temperature,grid_temperature)
library(GSTools)
create_map(as.data.frame(cbind(temperature[,,7], grid_harvest)))
saveRDS(temperature, file = paste0("Temperature_HadGEM2-AO_",wyears[1],"-",wyears[length(wyears)],".rds"))
temperature = readRDS(paste0("Temperature_HadGEM2-AO_",wyears[1],"-",wyears[length(wyears)],".rds"))
#mean change over the years
plot(apply(temperature[,1,], 2, sd), type="l")

###########################################################################################################
#PRECIPITATION CLIMATE SCENARIO
file = "pr_mon_HadGEM2-AO_r1i1p1_rcp85_190001-209912.qmbc_GPCCv6.clm"
dir = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/"
precipitation = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"precipitation",wyears))
for (i in 1:length(wyears)) {
  temp<-readLPJBin(file_name = file, file_folder = dir, wyears = wyears[i], syear = 1901, bands =12,
                   ncells = 67420, headlines = 43, bytes = 2, datatype = "integer", file_type="input", time_unit = "months")
  r_temp = rasterToPoints(temp,spatial=TRUE)
  data_temp = rowMeans(as.data.frame(r_temp)[,-c(13,14)])
  precipitation[,,i] = data_temp
}
# grid_precipitation = as.data.frame(r_temp)[,c(13,14)]
# precipitation = cbind(precipitation,grid_precipitation)
library(GSTools)
create_map(as.data.frame(cbind(precipitation[,,7], grid_harvest)))
saveRDS(precipitation, file = paste0("precipitation_HadGEM2-AO_",wyears[1],"-",wyears[length(wyears)],".rds"))
precipitation = readRDS(paste0("precipitation_HadGEM2-AO_",wyears[1],"-",wyears[length(wyears)],".rds"))
#mean change over the years
plot(apply(precipitation[,1,], 2, mean), type="l")


###########################################################################################################
#SHORTWAVE RADIATION
file = "swdown_erainterim_1901-2011.clm"
dir = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/"
shortwave_radiation = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"temperature",wyears))
for (i in 1:length(wyears)) {
  temp<-readLPJBin(file_name = file, file_folder = dir, wyears = wyears[i], syear = 1901, bands =12,
                   ncells = 67420, headlines = 38, bytes = 2, datatype = "integer", file_type="input", time_unit = "months")
  r_temp = rasterToPoints(temp,spatial=TRUE)
  data_temp = rowMeans(as.data.frame(r_temp)[,-c(13,14)])/10
  shortwave_radiation[,,i] = data_temp
}
create_map(as.data.frame(cbind(shortwave_radiation[,,2], grid_harvest)))
saveRDS(shortwave_radiation, file = paste0("swdown_erainterim_",wyears[1],"-",wyears[length(wyears)],".rds"))
shortwave_radiation = readRDS(paste0("swdown_erainterim_",wyears[1],"-",wyears[length(wyears)],".rds"))
shortwave_radiation[1:100,,]
###########################################################################################################
#LONGWAVE RADIATION
file = "lwnet_erainterim_1901-2011.clm"
dir = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/"
longwave_radiation = array(NaN,dim = c(cells,1,length(wyears)), dimnames = list(1:cells,"temperature",wyears))
for (i in 1:length(wyears)) {
  temp<-readLPJBin(file_name = file, file_folder = dir, wyears = wyears[i], syear = 1901, bands =12,
                   ncells = 67420, headlines = 38, bytes = 2, datatype = "integer", file_type="input", time_unit = "months")
  r_temp_l = rasterToPoints(temp,spatial=TRUE)
  data_temp = rowMeans(as.data.frame(r_temp_l)[,-c(13,14)])/10
  longwave_radiation[,,i] = data_temp
}

# create_map(as.data.frame(cbind(longwave_radiation[,,1], grid_harvest)))
saveRDS(longwave_radiation, file = paste0("lwnet_erainterim",wyears[1],"-",wyears[length(wyears)],".rds"))
longwave_radiation = readRDS(paste0("lwnet_erainterim",wyears[1],"-",wyears[length(wyears)],".rds"))
#Obs: check why longwave radiation has negative signs

###########################################################################################################
#LOADING SOILDATA
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/soil_type.Rdata") #soil_type

###########################################################################################################
# CONNECTING DATASETS
lsus = rep(c(00,0.2,0.4,0.6,0.8,1,1.2,1.4,1.6,1.8,2.0,2.5), times = 7, each =67420)
harvest_lsu_temp_prec_shor_long_soil = cbind(harvest_lsu,lsus,temperature, precipitation,shortwave_radiation, longwave_radiation, soil_type)
colnames(harvest_lsu_temp_prec_shor_long_soil)[1] = "gC/m2/y"
saveRDS(harvest_lsu_temp_prec_shor_long_soil, file = paste0("harvest_lsu_temp_prec_shor_long_soil",wyears[1],"-",wyears[length(wyears)],".rds") )
# Checkings
summary(harvest_lsu_temp_prec_shor_long_soil)
dim(harvest_lsu_temp_prec_shor_long_soil)
length(lsus)

###########################################################################################################

# prof of concenpt for dataset aggregation
# harvest_test = array(seq(45,89,1),dim = c(5,3,3), dimnames = list(1:5,10:12,c(2000,2001,2002)))
# temperature_test = array(seq(1,15,1),dim = c(5,1,3), dimnames = list(1:5,"temperature",c(2000,2001,2002)))
# preciptation_test = array(seq(15,30,1),dim = c(5,1,3), dimnames = list(1:5,"preciptation",c(2000,2001,2002)))
# lsu_test = array(rep(10:12,times = 3, each =5),dim = c(5,1,3), dimnames = list(1:5,"lsu",c(2000,2001,2002)))
# radiation_test = array(seq(1,5,1),dim = c(5,1,3), dimnames = list(1:5,"radiation",c(2000,2001,2002)))

# (file.size("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/tas_mon_HadGEM2-AO_r1i1p1_rcp85_190001-209912.qmbc_CRU_TS3.21.clm")-43)/67420/2/12/200
