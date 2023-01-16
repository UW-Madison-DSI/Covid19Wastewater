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

#' summary method for linear forest class
#'
#' @param random_linear_forest 
#'
#' @return summary.random_linear_forest object
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' model <- random_linear_forest(PMMoV ~ N1 + N2 | date + site)
#' summary(model)
setMethod(f = "summary",
          signature = "random_linear_forest",
          #currently just a list version of the show method
          #can be expanded to have more info in the future
          function(object){
            ans <- list()
            ans[1] <- object@formula
            ans[2] <- paste("size of data:", nrow(object@data))
            ans[3] <- paste("MSE:", mean(object@resid**2, na.rm = TRUE))
            class(ans) <- "summary.random_linear_forest"
            return(ans)
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
            #get each tree predictions
            tree_pred <- lapply(object@models, predict, newdata = new_data)
            #get it in the right shape
            tree_pred <- lapply(transpose(tree_pred), unlist)
            #get a vector of predictions for each row
            tree_pred <- unlist(lapply(tree_pred, mean))
            return(tree_pred)
          })