#' Save and export model weights and biases for use in GAMS
#' @input input = c("lsu","lon","lat","tem","pre","rad", "soil")
#' @parameters model = keras h5 saved model.
require(keras)
# model = load_model_hdf5("C:/Users/MarcosPaulo/OneDrive/PIK - Intership/PIK-projects/ML_harvest25_envi_soil_500_mse_300_200_100_35_1_elu/model.h5")
# weights = get_weights(model)

list_col_names = function(x) {
  k = list()
  w = NULL
  for (i in 1:length(x)) {
    l = floor((i + 1) / 2)
    if ((i %% 2) == 0) {
      k[[i]] = as.character(paste0("n", l))
    }else{
      #else, then weights.
      for (j in 1:ncol(x[[i]])) {
        w = append(w,as.character(paste0("n", l,"_", j)))
      }
      k[[i]] = w
      w = NULL
    }
  }
  return(k)
}

update_weights = function(inputs_vec, weights, names_list) {
  weights[[1]] = cbind(inputs_vec, weights[[1]])
  weights[[1]] = rbind(append("dummy", names_list[[1]]), weights[[1]])
  for (i in 2:length(weights)) {
    if ((i %% 2) != 0) {
      weights[[i]] = cbind(names_list[[(i - 2)]], weights[[i]])
      weights[[i]] = rbind(append("dummy", names_list[[i]]), weights[[i]])
    } else{
      weights[[i]] = cbind(names_list[[(i - 1)]], weights[[i]])
      weights[[i]] = rbind(append("dummy", names_list[[i]]), weights[[i]])
    }
  }
  return(weights)
}

save_weights = function(weights,data_set) {
  for (i in 1:length(weights)) {
    l = floor((i + 1) / 2)
    if ((i %% 2) != 0) {
      write.table(data.frame(weights[[i]]), paste0(data_set,"_weights_", l, ".csv"), col.names = F, row.names = F, sep = ',')
    }else{
      write.table(data.frame(weights[[i]]), paste0(data_set,"_bias_", l, ".csv"), col.names = F, row.names = F, sep = ',')
    }
  }
}

export_weights_gams = function(model, inputs_vec, data_set){
  weights = get_weights(model)
  names_list = list_col_names(weights)
  weights_up = update_weights(inputs_vec,weights,names_list)
  save_weights(weights_up, data_set) 
}

###############
#Additional functions
bias_names = function(x) {
  bias_names = list()
  for (i in 1:length(x)) {
    c = (i+1)/2
    if ((i %% 2) == 0) {
      bias_names[[c-0.5]] = as.character(paste0("b", c-0.5))
    }
  }
  return(bias_names)
}

weight_names = function(x) {
  weights_names = list()
  w = NULL
  for (i in 1:length(x)) {
    c = (i+1)/2
    if ((i %% 2) != 0) {
      for (j in 1:ncol(x[[i]])) {
        y = as.character(paste0("w", c,"_", j))
        w = append(w,y)
      }
      weights_names[[c]] = w
      w = NULL
    }
  }
  return(weights_names)
}

give_names = function(x) {
  
  weights_names = list()
  bias_names = list()
  w = NULL
  b = NULL
  for (i in 1:length(x)) {
    c = (i+1)/2
    if ((i %% 2) == 0) {
      #if even, then bias.
      #print(paste0("b", c-0.5))
      bias_names[[c-0.5]] = as.character(paste0("b", c-0.5))
    }else{
      #else, then weights.
      for (j in 1:ncol(x[[i]])) {
        #print(paste0("w", c, j))
        y = as.character(paste0("w", c,"_", j))
        w = append(w,y)
      }
      weights_names[[c]] = w
      w = NULL
    }
  }
  return(c(list(bias_names), list(weights_names)))
}

# list_col_names = function(x) {
#   k = list()
#   w = NULL
#   for (i in 1:length(x)) {
#     l = floor((i + 1) / 2)
#     if ((i %% 2) == 0) {
#       k[[i]] = as.character(paste0("b", l))
#     }else{
#       #else, then weights.
#       for (j in 1:ncol(x[[i]])) {
#         w = append(w,as.character(paste0("w", l,"_", j)))
#       }
#       k[[i]] = w
#       w = NULL
#     }
#   }
#   return(k)
# }

