#' random_linear_forest
#' model class using a random forest of linear forest models
#'  
#'
#' @slot formula formula used to create trees and linear models 
#' @slot models list of each tree model in model  
#' @slot data data.frame of the input data
#' @slot resid numeric vector of residuals
#' @slot inbag_data list of data.frame each model trained on
#' @slot oob_data list of data.frame containing info model not trained on 
#' @slot oob_resid numeric vector of oob residuals
#' @slot inc_mse \% increased mean squared error
#'
#' @return random_linear_forest object
#' @export
#'
#' @examples
#' data("Example_data", package = "DSIWastewater")
#' random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
setClass(
  "random_linear_forest",
  slots = list(formula = "formula",
               models = "list",
               data = "data.frame",
               resid = "numeric",
               inbag_data = "list",
               oob_data = "list",
               oob_resid = "numeric",
               inc_mse = "data.frame")
)


#' Fitting linear random forest
#' 
#' This uses the linear tree model from party kit and bootstrapping
#' to create linear random forests that work like a random forest but with
#' the linear dynamics permuted with a linear method
#'
#' @param data a dataframe containing the variables in the model
#' @param num_tree numeric, the number of trees in the random forest.
#' @param model_formula an object of class "formula": a symbolic description of the model to be fitted. 
#' @param num_features number of tree features in each tree. if left NULL rounded up square of the number of columns
#' @param max_depth the max depth of each tree in the forest
#' @param na.action passed to lmtree to handle missing data
#' @param importance controls if the importance should be calculated and stored
#' @param verbose If true it prints training progress
#'
#' @return random_linear_forest object trained using given data
#' @export
#'
#' @examples
#' data(Example_data, package = "DSIWastewater")
#' random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
random_linear_forest <- function(data,
                                 num_tree,
                                 model_formula,
                                 num_features = NULL,
                                 na.action = na.roughfix,
                                 max_depth = 5,
                                 importance = FALSE,
                                 verbose = FALSE){
  
  exp_formula <- expand_formula(model_formula, data)
  #move formula columns to front
  dep_term <- as.character(exp_formula[[2]])
  lm_pred_term <- all.vars(exp_formula[[3]][[2]])
  tree_pred_term <- all.vars(exp_formula[[3]][[3]])
  data <- select(data, all_of(c(dep_term, lm_pred_term, tree_pred_term)))
  ####
  
  #create random_linear_forest class object the function returns
  linear_forest <- new("random_linear_forest",
                       formula = exp_formula,
                       data = data)
  
  
  #create index for linking bagged data back together
  data$index <- 1:nrow(data)
  data <- select(data, .data$index, everything())
  
  #do the Bootstrap aggregating do improve error
  Boot_list_DF <- bagging(data, 
                          num_bags = num_tree,
                          num_features = num_features,
                          include_first = 2 + length(lm_pred_term))
  
  #add data to return object
  linear_forest@inbag_data <- Boot_list_DF[[1]]
  linear_forest@oob_data <- Boot_list_DF[[2]]
  
  i = 1
  #function used for each tree
  lmtree_func <- function(bag) {#glmtree
    mod_tree <- lmtree(formula = model_formula,
                       data = select(data, -.data$index),# family = gaussian,
                       na.action = na.action,
                       maxdepth = max_depth)
    if(verbose){
      print(paste(i, "tree fitted"))
      i <<- i + 1
    }
    return(mod_tree)
  }
  
  #train the models
  linear_forest@models <- lapply(linear_forest@inbag_data,
                                 lmtree_func)
  
  
  
  #save residuals
  linear_forest@resid <- data[[dep_term]] - predict(linear_forest, data)
  
  #save out of bag residuals
  #linear_forest@oob_resid <- gen_OOB_pred(linear_forest,
  #                                        resid = TRUE)
  
  return(linear_forest)
}

#downsample and down column
#fit data
#calc OOBMSE
#calc incMSE
#get MSE vs number of tree
# gen_tree <- function(){
#   mod_tree <- lmtree(formula = model_formula,
#                      data = select(data, -index),# family = gaussian,
#                      na.action = na.action,
#                      maxdepth = max_depth)
# }
