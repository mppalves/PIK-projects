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
library(ggplot2)
library(stringr)

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


Run_simulation = function(func, w, x, input_data, title, cor ="red", comment = NULL) {
  
  #pre-processing data
  colnames(input_data) = c("x","w","y")
  
  #simulating y
  y = evaluate_expression(func, input_data$w, input_data$x)

  #combining the results in a dataframe and constructing a matrix to be ploted
  combineded_output = data.frame(x = input_data$x, w=input_data$w, y)
  
  output_data = combineded_output$y
  input_data = input_data$y
  
  #running statistical analysis on the results
  Run_analysis(output_data,input_data, title,cor, comment)
}


Run_analysis = function(output_data, input_data, title, cor, comment = NULL){

  #comparing results
  ttest = t.test(input_data, output_data, paired =F, var.equal = T)
  correlation = cor(input_data, output_data)
  Residuals = output_data-input_data
  variances = var.test(output_data,input_data)
  
  # par(mfrow =c(3,1))
  # hist(input_data, xlim = c(0,max(input_data)), main = paste0("Data input "), breaks = 50) 
  # hist(output_data, xlim = c(0,max(input_data)), main = paste0("Simulation output"), breaks = 50)
  # hist(residuals, main = "Residual distribution")
  # par(mfrow =c(1,1))
  # 
  # print(plot(output_data,input_data, pch='.', main = paste0("Correlation between real and predicted output for", title) , xlab = "Predicted output", ylab = "real output", col=rgb(0,0,0,alpha=0.1)))
  # dev.copy(jpeg,paste0(title,".jpeg"))
  # dev.off()
  
  # Overlaid histograms
  a = data.frame(input_data) 
  b = data.frame(output_data)
  a$Distribution = "Original"
  b$Distribution = "Predicted"
  colnames(a)[1] = "Output"
  colnames(b)[1] = "Output"
  dat = rbind(a,b)
  
  size=max(input_data)*0.02
  
  histo1 = ggplot(dat, aes(x=Output, fill= Distribution)) +
    geom_histogram(binwidth=size, alpha=.5, colour="#595959", position = 'identity') + 
    #facet_grid(Distribution ~ .) +
    theme(text = element_text(size = 18)) +
    #coord_cartesian(xlim = c(0,125000)) +
    xlab("Output") +
    ylab("Density") 
  
  print(histo1)
  
  ggsave(paste0(title,"_histogram_", str_remove(str_remove(Sys.time(),":"),":"), ".pdf"),
         width = 22,
         height = 15,
         units = "cm",
         limitsize = TRUE)
  
  histo2 = ggplot(as.data.frame(Residuals), aes(x=Residuals)) +
    geom_histogram(binwidth=size, alpha=.5, colour="#595959") +
    theme(text = element_text(size = 18)) +
    xlab("Residuals") +
    ylab("Density")
  
  print(histo2)
  
  ggsave(paste0(title,"_residuals_", str_remove(str_remove(Sys.time(),":"),":"), ".pdf"),
         width = 15,
         height = 15,
         units = "cm",
         limitsize = TRUE)
  

  
  #plotting with ggplot and adding a density function
  ggplot_dataset = data.frame(input_data, output_data)
  scatter = ggplot(data = ggplot_dataset,aes(x=input_data ,y=output_data)) +
    geom_point( size = 0.1, stroke = 0, shape = 16, alpha = 0.3) +
    ylab("Predicted output (gC/m²)") +
    xlab("Original output (gC/m²)") +
    ggtitle(paste0(title), subtitle = "Output correlation between Orginal and Predicted") +
    geom_density_2d(alpha = 0.7, colour = cor) +
    geom_abline(intercept = 0, slope = 1, linetype="dashed") +
    #coord_cartesian(xlim = c(0,125000), ylim = c(0,125000)) +
    theme(text = element_text(size = 18))


  print(scatter)

  ggsave(paste0(title,"_dispersion_", str_remove(str_remove(Sys.time(),":"),":"), ".pdf"),
         width = 15,
         height = 15,
         units = "cm",
         limitsize = F)
   
  
  
  if(!is.null(comment)){
    x = list("t test" = ttest, "correlation" = correlation, "Variances" = variances, "function" = comment)
    y = capture.output(x)
    writeLines(y, con = file(paste0(title,"_statistical_analysis",str_remove(str_remove(Sys.time(),":"),":"),".txt")))
    return(x)
  }else{
    x = list("t test" = ttest, "correlation" = correlation, "Variances" = variances)
    y = capture.output(x)
    writeLines(y, con = file(paste0(title,"_statistical_analysis_",str_remove(str_remove(Sys.time(),":"),":"),".txt")))

    return(x)
  }
  
}
