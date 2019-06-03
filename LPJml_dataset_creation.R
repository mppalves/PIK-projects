#reshape lpjml runs
library(readr)
library(tidyr)
library(dplyr)

setwd("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2")

load("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/grass_results4Marcos.RData")
global_weather_2000 <- read_csv("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/global_weather_2000.csv")

X00_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_00_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X02_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_02_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X04_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_04_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X06_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_06_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X08_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_08_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X10_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_10_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X12_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_12_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X14_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_14_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X16_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_16_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X18_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_18_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X20_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_20_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X25_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_25_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X30_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_30_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X35_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_35_LSUst.csv", 
                      col_names = FALSE, skip = 1)
X40_LSU <- read_table2("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/GrassData2/nyeardata_LPJmL4GlobalExp_40_LSUst.csv", 
                      col_names = FALSE, skip = 1)

X00_LSU$LSU = 0
X02_LSU$LSU = 0.2
X04_LSU$LSU = 0.4
X06_LSU$LSU = 0.6
X08_LSU$LSU = 0.8
X10_LSU$LSU = 1.0
X12_LSU$LSU = 1.2
X14_LSU$LSU = 1.4
X16_LSU$LSU = 1.6
X18_LSU$LSU = 1.8
X20_LSU$LSU = 2.0
X25_LSU$LSU = 2.5
X30_LSU$LSU = 3.0
X35_LSU$LSU = 3.5
X40_LSU$LSU = 4.0

X00_LSU$LSU = 0
X02_LSU$LSU = 2
X04_LSU$LSU = 4
X06_LSU$LSU = 6
X08_LSU$LSU = 8
X10_LSU$LSU = 10
X12_LSU$LSU = 12
X14_LSU$LSU = 14
X16_LSU$LSU = 16
X18_LSU$LSU = 18
X20_LSU$LSU = 20
X25_LSU$LSU = 25
X30_LSU$LSU = 30
X35_LSU$LSU = 35
X40_LSU$LSU = 40

df.long = rbind(X00_LSU,X02_LSU, X04_LSU,X06_LSU,X08_LSU,X10_LSU,X12_LSU,X14_LSU,X16_LSU,X18_LSU,X20_LSU,X25_LSU,X30_LSU,X35_LSU,X40_LSU)
df.long = rbind(X00_LSU,X02_LSU, X04_LSU,X06_LSU,X08_LSU,X10_LSU,X12_LSU,X14_LSU,X16_LSU,X18_LSU,X20_LSU,X25_LSU)


create_wide_data = function(df.long,col){

    LSUs = unique(pull(df.long[,5]))
    df.wide = matrix(nrow = 67420)
    for (i in LSUs) {
      x = df.long[df.long$LSU == i,col]
      df.wide = cbind(df.wide,x)
    }
    df.wide = df.wide[,-1]
    colnames(df.wide) = LSUs
    return(df.wide)
}

#'col 2 = Harverst
#'col 3 = soilc
#'col 4 = npp

df_harvest = create_wide_data(df.long,2)
df_npp = create_wide_data(df.long,4)
df_soilc = create_wide_data(df.long,3)

saveRDS(df_harvest,file = "df_harvest25.Rds")
saveRDS(df_npp,file = "df_npp25.Rds")
saveRDS(df_soilc,file = "df_soilc25.Rds")



X00_LSU = cbind(X00_LSU,grid, global_weather_2000)
X02_LSU = cbind(X02_LSU,grid, global_weather_2000)
X04_LSU = cbind(X04_LSU,grid, global_weather_2000)
X06_LSU = cbind(X06_LSU,grid, global_weather_2000)
X08_LSU = cbind(X08_LSU,grid, global_weather_2000)
X10_LSU = cbind(X10_LSU,grid, global_weather_2000)
X12_LSU = cbind(X12_LSU,grid, global_weather_2000)
X14_LSU = cbind(X14_LSU,grid, global_weather_2000)
X16_LSU = cbind(X16_LSU,grid, global_weather_2000)
X18_LSU = cbind(X18_LSU,grid, global_weather_2000)
X20_LSU = cbind(X20_LSU,grid, global_weather_2000)
X25_LSU = cbind(X25_LSU,grid, global_weather_2000)
X30_LSU = cbind(X30_LSU,grid, global_weather_2000)
X35_LSU = cbind(X35_LSU,grid, global_weather_2000)
X40_LSU = cbind(X40_LSU,grid, global_weather_2000)


LPJ_dataset = rbind(X00_LSU,X02_LSU,X04_LSU,X06_LSU,X08_LSU,X10_LSU,X12_LSU,X14_LSU,X16_LSU,X18_LSU,X20_LSU,X25_LSU,X30_LSU,X35_LSU,X40_LSU)
colnames(LPJ_dataset)[1:4] = c("cell","harvest", "soilc", "npp")

save(LPJ_dataset,file = "LPJ_dataset.Rdata")

load("LPJ_dataset.Rdata")


library(dplyr)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
