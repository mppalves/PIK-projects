dirs = data.frame(dir = c("/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_00_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_02_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_04_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_06_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_08_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_10_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_12_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_14_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_16_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_18_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_20_LSUs_ambient/",
                          "/p/projects/landuse/users/wirth/LPJmL/Output/GrazingIntensityGradientGlobalRCP85_HadGEM2-ES_20190426/ambient/LPJmL4GlobalExp_25_LSUs_ambient/"))

readharvest <- function(wyear, file_name.= "pft_harvest.pft.bin", file_folder.= file_folder, syear.=syear,
                        years.=100, bands=32, soilcells=T, bytes=4, ncells=67420, z_dim="category", ban=14)
{
  tmp <- (readLPJBin(file_name = file_name., file_folder =  file_folder., wyears=wyear,syear=syear., years=years.,bands=bands, soilcells = soilcells,
                     bytes = bytes, ncells = ncells, z_dim = z_dim)[[ban]])
  tmp = rasterToPoints(tmp,spatial=T)
  tmp = as.data.frame(tmp)[1]
  return(tmp)
}

##################################################################################################

#harvest for the year 2099
harvest1 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[1])
harvest2 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[2,])
harvest3 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[3,])
harvest4 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[4,])
harvest5 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[5,])
harvest6 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[6,])
harvest7 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[7,])
harvest8 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[8,])
harvest9 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[9,])
harvest10 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[10,])
harvest11 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[11,])
harvest12 = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = dirs[12,])

harvest_ambient = cbind(harvest1,harvest2,harvest3, harvest4,harvest5,harvest6,harvest7,harvest8,harvest9,harvest10,harvest11,harvest12)
colnames(harvest_ambient) = c("00","02","04","06","08","10","12","14","16","18","20","25")
saveRDS(harvest_ambient, file = "harvest_ambient_2099.rds")

##################################################################################################

#harvest for the year 2000
harvest1 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[1,])
harvest2 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[2,])
harvest3 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[3,])
harvest4 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[4,])
harvest5 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[5,])
harvest6 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[6,])
harvest7 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[7,])
harvest8 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[8,])
harvest9 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[9,])
harvest10 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[10,])
harvest11 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[11,])
harvest12 = readharvest(wyear = 2000, syear. = 1951,years. = 149, file_folder. = dirs[12,])

harvest_ambient = cbind(harvest1,harvest2,harvest3, harvest4,harvest5,harvest6,harvest7,harvest8,harvest9,harvest10,harvest11,harvest12)
colnames(harvest_ambient) = c("00","02","04","06","08","10","12","14","16","18","20","25")
saveRDS(harvest_ambient, file = "harvest_ambient_2000.rds")




#second try, autimation

dirs = list.dirs("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/teste");dirs
dirs = dirs[-1]

rm(test)
test = matrix(ncol=length(dirs), nrow=1)
for (i in 1:length(dirs)) {
  test[,i] = as.numeric(read.table(paste0(dirs[i],"/1.txt")))
}
dim(test)
test

harvest_ambient_2099 = readRDS("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/harvest_ambient_2099.rds")
plot_harvest = cbind(harvest_ambient_2099[,"16"],grid_ambient)
create_map(plot_harvest,map_name = "test harvest",cathegorical = F)

summary(plot_harvest)

saveRDS()

grid_ambient = readharvest(wyear = 2099, syear. = 1951,years. = 149, file_folder. = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/")
grid_ambient = grid_ambient[,c(2,3)]
saveRDS(grid_ambient, file = "grid_ambient.rds")

summary(grid_ambient)

plot(harvest)

harvestpoints = rasterToPoints(harvest,spatial=T)
df.harvest = as.data.frame(harvestpoints)[,1]
df.harvest[,c(1,2,3)] = df.harvest[,c(3,1,2)]
dim(df.harvest)
library(GSTools)
summary(df.harvest)

create_map(plot_harvest,map_name = "test harvest",cathegorical = F)

"C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs_ambient/"
