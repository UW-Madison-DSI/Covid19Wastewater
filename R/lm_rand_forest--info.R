#' Bootstrap aggregating of dataset
#' gen a list of dataframes using row resampling and column downsizing
#'
#' @param data data.frame info
#' @param num_features number of columns in each output data.frame
#' @param num_bags number of resamplings done
#' @param include_first auto include the first n rows
#'
#' @return return list of 2 list of data.frame given different bagged data
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' bagging(example_data, 10, 5, 2)
bagging <- function(data, num_bags, num_features = NULL, include_first = 0){
  
  #if num_features not specified use default of sqrt(num cols)
  if(is.null(num_features)){
    num_features = ceiling(sqrt(length(data) - include_first))
  }
  #####
  data_list <- bootstraps(data, times = num_bags)$splits
  base_names <- names(data)
  ret_IB <- list()
  ret_OOB <- list()
  ign_cols <- base_names[1:include_first]
  col_options <- base_names[(include_first + 1):length(base_names)]
  for(i in 1:length(data_list)){
    selected_columns <- c(ign_cols, 
                          sample(x = col_options, size = num_features))

    ret_IB[[i]] <- select(analysis(data_list[[i]]), any_of(selected_columns))
    
    ret_OOB[[i]] <- select(assessment(data_list[[i]]), any_of(selected_columns))
  }
  return(list(ret_IB, ret_OOB))
}



#' get OOB predictions of the training dataset
#' returns the predictions of each row of the input data
#' using only trees not trained on the row
#'
#' @param tree_model random_linear_forest object you want the OOB predictions of 
#' @param incMSE if its a numeric scramble the given column
#' @param resid if True return the residuals of the model instead
#'
#' @return data.frame or vector of oob predictions
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' model <- random_linear_forest(example_data, 2, PMMoV ~ N1 + N2 | date + site)
#' gen_OOB_pred(model, resid = TRUE)
gen_OOB_pred <- function(tree_model, 
                         incMSE = NA, resid = FALSE){
  model_list <- tree_model@models
  oob_data_list <- tree_model@oob_data
  num_trees <- length(model_list)
  temp_pred <- list()
  for(i in 1:num_trees){
    pred_name <- as.name(paste0("pred_", i))
    assessment_DF <- oob_data_list[[i]]
    if(!is.na(incMSE) & incMSE %in% names(assessment_DF)){
      assessment_DF[incMSE] <- sample(pull(assessment_DF,incMSE))
    }
    
    pred_output <- mutate(assessment_DF, !!pred_name := predict(model_list[[i]], newdata = assessment_DF))
    
    temp_pred[[i]] <- select(pred_output, index, conf_case, !!pred_name)
  }
  ret <- plyr::join_all(temp_pred, by = "index", type='full')
  if(resid){
    ret_pred <- rowMeans(select(ret, -index, -conf_case), na.rm = TRUE)
    return(ret$conf_case - ret_pred)
  }else{
    return(ret)
  }
}



#' get increased mean square error for each column
#'
#' @param tree_model random_linear_forest object you want the MSE of
#'
#' @return data.frame containing each column and its MSE increase
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' model <- random_linear_forest(example_data, 2, PMMoV ~ N1 + N2 | date + site)
#' gen_OOB_pred(model)
gen_INCMSE <- function(tree_model){
  model_list <- tree_model@models
  oob_data_list <- tree_model@oob_data
  col_names <- names(tree_model@data)
  ret_DF_list <- list()
  base_MSE <- mean(gen_OOB_pred(tree_model,
                            resid = TRUE)**2, na.rm = TRUE)
  
  for(i in 4:(length(col_names))){
    I_MSE <- mean(gen_OOB_pred(tree_model,
                           incMSE = col_names[i],
                           resid = TRUE)**2,
                  na.rm = TRUE)
    
    ret_DF_list[[i - 3]] <- data.frame(
      incMSE = 100*(I_MSE - base_MSE)/base_MSE,
      var = col_names[i])
  }
  ret <- bind_rows(ret_DF_list)
  return(ret)
}


#' get OOB MSE vs number of forest in trees
#'
#' @param tree_model 
#'
#' @return get dataframe of number of trees and OOB MSE
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' model <- random_linear_forest(example_data, 2, PMMoV ~ N1 + N2 | date + site)
#' OOB_MSE_num_trees(model)
OOB_MSE_num_trees <- function(tree_model){
  pred_DF = gen_OOB_pred(tree_model)
  OOB_MSE_TREE <- list()
  for(i in 1:length(tree_model@models)){
    OOB_MSE_TREE[[i]] <-
      mutate(
      summarise(
      mutate(pred_DF, 
             mean_pred = rowMeans(select(pred_DF,
                                         pred_1:paste0("pred_",i)),
                                  na.rm = TRUE)),
      model_MSE = MSE(conf_case, mean_pred)),
      num_tree = i)
  }
  return(bind_rows(OOB_MSE_TREE))
}
