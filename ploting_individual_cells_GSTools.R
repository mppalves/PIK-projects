#'@title plotting individual cells
#'@description Function to plot selected cells response to changes in LSU and climate variables
#'@observation: Possible climate variables: 
#'"temperature_mean", 
#'"precipitation_mean", 
#'"radiation_mean"
#'
library(plotly)

ploting_individual_cells = function(test_data,cell,Lsu_range, climate_variable, climate_variable_range, ML_model){
  #Ploting the model results for one cell
  

  x = seq(Lsu_range[1],Lsu_range[2],0.1)
  r = seq(climate_variable_range[1],climate_variable_range[2],0.1)
  test_input = test_data[cell,]

  res=data.frame()
  for (i in 1:length(x)) {
    test_input["LSU"] = x[i]
    for (j in 1:length(r)) {
      test_input[climate_variable] = r[j]
      y = predict(ML_model, t(matrix(test_input)))
      res[j,i] = y
    }
  }
  
  axx = list(title = "lsu")
  axy = list(title = climate_variable)
  axz = list(title = "output")
  
  p <- plot_ly(y=r, x=x, z = as.matrix(res)) %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))  %>% add_surface()
  return(p)
}

