library(PIKTools)
library(car)
mainDir <- "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects"
load(file.path(mainDir,"grass_results4Marcos.RData"))
options(digits = 22)
#Calculating the value to offset in the begin of the file
#round((file.size("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/pr_mon_HadGEM2-AO_r1i1p1_rcp85_190001-209912.qmbc_GPCCv6.clm")-43)/67420/200/12/2,100)

source(file.path(mainDir,"create_map.R"))

#soil_type
sk <- file("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/soil_new_67420.bin","rb")
soil_type <- readBin(sk,integer(),n=67420,size=1)
close(sk)
save(soil_type, file = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/soil_type2.Rdata")

#recoding soytype, renaming the soiltyper from the most productive to the least productive
x = cbind(maps[,,"harvest"],soil_type, rowMeans(maps[,,"harvest"]))
y = aggregate(x, by = list(soil_type),mean)
Soil_conversion_table = data.frame(y[order(y[,14]),],"new_soil_label" = seq(12,1,-1))
soil_type_recode = recode(soil_type,"13=12;12=11;8=10;11=9;9=8;7=7;2=6;6=5;4=4;5=3;1=2;3=1")
soil_type = soil_type_recode
#save(soil_type, file = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/soil_type.Rdata")
load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/soil_type.Rdata")

#longwave_radiation
sk <- file("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/lwnet_erainterim_1901-2011.clm","rb")
seek(sk, where=38, origen = "start")
longwave_radiation <- readBin(sk,"int",n=67420,size=2)
close(sk)
save(longwave_radiation, file = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/longwave_radiation2.Rdata")


#shortwave_radiation
sk <- file("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/swdown_erainterim_1901-2011.clm","rb")
seek(sk, where=43, origen = "start")
shortwave_radiation <- readBin(sk,"int", n=67420, size=2)
close(sk)
save(shortwave_radiation, file = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/shortwave_radiation2.Rdata")

library(GSTools)
create_map(as.data.frame(cbind(longwave_radiation, grid)))
getwd()
#importing and manipulating raster files

library(PIKTools)
library(LandMark)
library(raster)

#temperature climate scenario
file = "tas_mon_HadGEM2-AO_r1i1p1_rcp85_190001-209912.qmbc_CRU_TS3.21.clm"
dir = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/"
temp<-readLPJBin(file_name = file, file_folder = dir, wyears = 2090, syear = 1901,years = 1, bands = 12,
                 ncells = 67420, headlines = 43, bytes = 2, datatype = "integer", file_type="input")
r_temp = rasterToPoints(temp,spatial=TRUE)
data_temp = as.data.frame(r_temp)[,c(11,13,14)]
data_temp[,1] = data_temp[,1]/10
saveRDS(data_temp, file.path(mainDir,"/LPJML_inputs/temp_2100.rds"))


#preciptation climate scenario
file = "pr_mon_HadGEM2-AO_r1i1p1_rcp85_190001-209912.qmbc_GPCCv6.clm"
dir = "C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/LPJML_inputs/"
prec<-readLPJBin(file_name = file, file_folder = dir, wyears = 2090, syear = 1901,years = 1, bands = 12,
                 ncells = 67420, headlines = 43, bytes = 2, datatype = "integer", file_type="input")
r_prec = rasterToPoints(prec,spatial=TRUE)
data_prec = as.data.frame(r_prec)[,c(11,13,14)]
data_prec[,1] = data_prec[,1]*10
saveRDS(data_prec, file.path(mainDir,"/LPJML_inputs/prec_2100.rds"))

