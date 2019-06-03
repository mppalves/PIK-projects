#' @title LSU optmization
#' @Description Optimize LSU for each cell using the machine learning model
#' @inputs Machine learning model, wheather data, dataset base, means and stddev used to standardize the inputs, 
#' grid information, upper boundary of the LSUs allowed
#' @outputs Vector of optmized LSUs per grid cell
#' @author Marcos Alves
library(tidyr)
# source("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/generate_test_data.R")

Optimize_LSU = function(test_data, model,min_Lsu, max_Lsu,col_means,col_stddevs, range){
  
  optimization = function(test_data){
     
    network_lsu = function(lsu){
      
      test_data["LSU"] = lsu
      y = model %>% predict(t(test_data))
      return(y)
  
    }
  
  opt = optimize(network_lsu, lower = min_Lsu, upper = max_Lsu, maximum = T)
  print(opt$maximum)
  LSU_optmimum = opt$maximum * col_stddevs["LSU"] + col_means["LSU"]
  return(LSU_optmimum)
  }
  # optmizing all cells using apply function
  optimum_lsu = apply(test_data[range,], 1, optimization)
  
  # combining the output with the grid indormation 
  output_data = cbind(optimum_lsu,grid[range,])

  return(output_data)
}
