#####################################################
#####################################################
#Scip to manipulate LPJmL output of various simulations and organize them in a 3d matrix for several years

library(PIKTools)
library(LandMark)

#basic parameters
setwd("/home/pedrosa/LPJmLClimateScenario1951-2099/ambient")
dirs = list.dirs(path = "/home/pedrosa/LPJmLClimateScenario1951-2099/ambient")[-1]
file_name = "/pft_harvest.pft.bin"
wyears = 2000:2010
soilcells = F
if (soilcells) {
  cells = 59199
}else{
  cells = 67420
}

rm(harvest)
basename(dirs)
harvest = array(NaN,dim = c(cells,length(wyears),length(dirs)), dimnames = list(1:cells,wyears,basename(dirs)))

for (i in 1:length(dirs)) {
  for (j in 1:length(wyears)) {
    tmp <- (readLPJBin(file_name = file_name, file_folder =  dirs[i], wyears=wyears[j],syear=1951, years=149,bands=32, soilcells = soilcells,
                       bytes = 4, ncells = 67420, z_dim = "category")[[14]])
    tmp = rasterToPoints(tmp,spatial=T)
    tmp = as.matrix(as.data.frame(tmp)[1])
    harvest[,j,i] = tmp
  }
}
dimnames(harvest)[[3]] = c("00","02","04","06","08","10","12","14","16","18","20","25","30","35","40")
#saving the file for all years and the same LSU allows to study the changes over the years in individual cells.
saveRDS(harvest, file = paste0("harvest_",wyears[1],"-",wyears[length(wyears)],".rds"))
setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient")
harvest = read_rds("harvest_2000-2010.rds")

  plot(apply(harvest[,,1], 2, mean),ylim=c(0,200) , type="l")
  for (i in 2:15) {
  lines(apply(harvest[,,i], 2, mean))
  }

###----
#Converting the dataset to be organized by year and be reado to be merged with the climate variables.
harvest_lsu = array(NaN,dim = c(cells,15,length(wyears)),dimnames = list(1:cells,c("00","02","04","06","08","10","12","14","16","18","20","25","30","35","40"),wyears))
#harvest_lsu = array(NaN,dim = c(cells,length(dirs),length(wyears)),dimnames = list(1:cells,basename(dirs),wyears)) #cluster version

for (year in dimnames(harvest)[[2]]) {
  tmp = harvest[,year,]
  harvest_lsu[,,year] = tmp
}
saveRDS(harvest_lsu, file = paste0("harvest_lsu",wyears[1],"-",wyears[length(wyears)],".rds"))
