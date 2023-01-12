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
#'
#' @return random_linear_forest object
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' random_linear_forest(example_data, 2, PMMoV ~ N1 + N2 | date + site)
setClass(
  "random_linear_forest",
  slots = list(formula = "formula",
               models = "list",
               data = "data.frame",
               resid = "numeric",
               inbag_data = "list",
               oob_data = "list",
               oob_resid = "numeric")
)

#' display form for random_linear_forest class
#'
#' @param random_linear_forest object with class random_linear_forest
#'
#' @return NULL
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' random_linear_forest(PMMoV ~ N1 + N2 | date + site)
setMethod(f = "show",
          signature = "random_linear_forest",
          function(object){
            print(object@formula)
            print(paste("size of data:", nrow(object@data)))
            print(paste("MSE:", mean(object@resid**2, na.rm = TRUE)))
          })

#' predict new data from random_linear_forest models
#'
#' @param random_linear_forest 
#'
#' @return vector of predictions for each row
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' model <- random_linear_forest(PMMoV ~ N1 + N2 | date + site)
#' predict(model, example_data)
setMethod(f = "predict",
          signature = "random_linear_forest",
          function(object, new_data){
            lapply(object@models, predict, newdata = new_data)%>% 
              transpose()%>%
              lapply(unlist)%>%
              lapply(mean)%>%
              unlist()
          })


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
#'
#' @return random_linear_forest object trained using given data
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' random_linear_forest(PMMoV ~ N1 + N2 | date + site)
random_linear_forest <- function(data,
                                 num_tree,
                                 model_formula,
                                 num_features = NULL){
  #move formula columns to front
  dep_term <- as.character(model_formula[[2]])
  lm_pred_term <- all.vars(model_formula[[3]][[2]])
  tree_pred_term <- all.vars(model_formula[[3]][[3]])
  data <- data%>%
    select(all_of(c(dep_term, 
           lm_pred_term)),
           everything())
  ####
  
  #create random_linear_forest class object we will return
  linear_forest <- new("random_linear_forest",
                       formula = model_formula,
                       data = data)
  
  
  #create index for linking bagged data back together
  data$index <- 1:nrow(data)
  data <- data%>%
    select(index, everything())
  
  Boot_list_DF <- bagging(data, 
                          num_bags = num_tree,
                          num_features = num_features,
                          include_first = 2 + length(lm_pred_term))
  
  linear_forest@inbag_data <- Boot_list_DF[[1]]
  linear_forest@oob_data <- Boot_list_DF[[2]]
  
  
  glmtree_func <- function(data) {
    data%>%
      select(-index)%>%
      glmtree(
        formula = model_formula,
        data = ., family = gaussian,
        na.action = na.pass,
        maxdepth = 5)
  }
  linear_forest@models <- linear_forest@inbag_data%>%
    lapply(glmtree_func)
  
  linear_forest@resid <- data$conf_case - predict(linear_forest, data)
  
  linear_forest@oob_resid <- gen_OOB_pred(linear_forest,
                                          resid = TRUE)
  
  return(linear_forest)
}
