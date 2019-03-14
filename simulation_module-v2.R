#'@title Evaluate expression
#'@author Marcos Alves
#'@description Function to take as input testing functions, LSU and predictors and output a table with a dataset 
#'@param x LSU
#'@param w secondary parameter (preciptation, radiation, mean for example)
#'#Linear b
#'@example y = evaluate_express(y ~ a*exp(b*x + (c + d*x)*w)*x*w, w=input_data$w, x=input_data$x, 
#'                      func_coefficients = list(a=8.76366,
#'                                               b=-0.0163042,
#'                                               c=-1.56578,
#'                                               d=0.0103395))
#'exponential b sigma(rho/x)theta
#'@example                                                
#' y = evaluate_express(y ~ a*exp(b*x + (c + d*x^e)*w)*x*w, w=input_data$w, x=input_data$x,
#'                      func_coefficients = list(a=8.291,
#'                                               b=-0.015,
#'                                               c=-1.254,
#'                                               d=0.0000117804,
#'                                               e = 2.47))
#'                                               
#'Possible 2d ploting
#'ploting the results with plotly (temporaraly unavailable)
#'plot_matrix = spread(unique(output_data), x, y)
#'p <- plot_ly(y=output_data$w, x=output_data$x, z = as.matrix(plot_matrix)) %>% add_surface();p
#'
#'@example How to preprocess the input and output datafor Run_simulation and Run_analsis functions
#'input_data = as.data.frame(cbind(maps[,,"harvest"],rowMeans(maps[,,"harvest"])))
#'input_data <- input_data %>% gather("LSU", "output", "00":"20")
#'input_data[,2] = as.numeric(input_data[,2])/10
#'names(input_data) = c("x","w","y")

#tranforming formulas in functions
as.function <- function(formula, w, x) {
  cmd <- tail(as.character(formula),1)
  exp <- parse(text=cmd)
  function(w,x) eval(exp, list(w=w,x=x))
}

evaluate_expression <- function(func,w, x){
  
  func = as.function(func,w,x)
  y = do.call(func,list(w=w,x=x))
  return(y)
}


Run_simulation = function(func, w, x, input_data) {
  
  #pre-processing data
  colnames(input_data) = c("x","w","y")
  
  #simulating y
  y = evaluate_expression(func, input_data$w, input_data$x)

  #combining the results in a dataframe and constructing a matrix to be ploted
  combineded_output = data.frame(x = input_data$x, w=input_data$w, y)
  
  output_data = combineded_output$y
  input_data = input_data$y
  
  #running statistical analysis on the results
  Run_analysis(output_data,input_data)
}


Run_analysis = function(output_data, input_data, comment = NULL){

  #comparing results
  ttest = t.test(input_data, output_data, paired =F, var.equal = T)
  correlation = cor(input_data, output_data)
  residuals = output_data-input_data
  variances = var.test(output_data,input_data)
  par(mfrow =c(3,1))
  hist(input_data, xlim = c(0,200), main = paste0("Data input "), breaks = 50) 
  hist(output_data, xlim = c(0,200), main = paste0("Simulation output"), breaks = 50)
  hist(residuals, main = "Residual distribution")
  par(mfrow =c(1,1))
  
  plot(output_data,input_data, pch='.', main = "Correlation between input and output data", xlab = "Predicted output", ylab = "real output", col=rgb(0,0,0,alpha=0.01) )
  
  if(!is.null(comment)){
    return(list("t test" = ttest, "correlation" = correlation, "function" = comment))
  }else{
    return(list("t test" = ttest, "correlation" = correlation, "Variances" = variances))
  }
  
}
