#' display form for random_linear_forest class
#'
#' @param object object with class random_linear_forest
#'
#' @return Prints to output a summary of the model
#' 
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
setMethod(f = "show",
          signature = "random_linear_forest",
          function(object){
            print(summary(object))
          })

#' summary method for linear forest class
#'
#' @param object random_linear_forest being used
#' @param ... extra parameters ignored
#'
#' @return summary.random_linear_forest object
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' model <-  random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
#' summary(model)
setMethod(f = "summary",
          signature = "random_linear_forest",
          #currently just a list version of the show method
          #can be expanded to have more info in the future
          function(object, ...){
            dep_var <- object@data[[object@formula[[2]]]]
            MSE <- mean((object@resid)**2, na.rm = TRUE)
            ans <- list()
            ans[[1]] <- object@formula
            ans[[2]] <- paste("size of data:", nrow(object@data))
            ans[[3]] <- paste("Number of trees:", length(object@models))
            ans[[4]] <- paste("Mean of squared residuals:", MSE)
            ans[[5]] <- paste("% Var explained:", 
                            100 * (1 - (MSE/var(dep_var, na.rm = TRUE))))
            class(ans) <- "summary.random_linear_forest"
            return(ans)
          })

#' predict new data from random_linear_forest models
#'
#' @param object random_linear_forest being used
#' @param new_data data.frame. 
#' @param ... extra parameters ignored
#'
#' @return vector of predictions for each row
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' model <- random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
#' predict(model, Example_data)
setMethod(f = "predict",
          signature = "random_linear_forest",
          definition = function(object, new_data, ...){
            #get each tree predictions
            tree_pred <- lapply(object@models, predict, newdata = new_data)
            #get it in the right shape
            tree_pred <- lapply(transpose(tree_pred), unlist)
            #get a vector of predictions for each row
            tree_pred <- unlist(lapply(tree_pred, mean))
            return(tree_pred)
          })