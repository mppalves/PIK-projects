#' @title function to reconstruct in spread format the table with the values predicted by the model

reconstruct_dataset = function(test_data_unscaled, harvest_predicted){
  x = data.frame(test_data_unscaled, "gmC2" = harvest_predicted)
  y = spread(x,"LSU", "gmC2")
  y = y[,dim(test_data_unscaled)[2]:dim(y)[2]]
  return(y)
}