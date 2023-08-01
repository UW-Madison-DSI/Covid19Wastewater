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
#' data(Example_data, package = "DSIWastewater")
#' head(bagging(Example_data, 10, 5, 2))
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
#' data(Example_data, package = "DSIWastewater")
#' model <- random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
#' gen_OOB_pred(model, resid = TRUE)
gen_OOB_pred <- function(tree_model, 
                         incMSE = NA, resid = FALSE){
  model_list <- tree_model@models
  oob_data_list <- tree_model@oob_data
  num_trees <- length(model_list)
  temp_pred <- list()
  dep_var <- tree_model@formula
  for(i in 1:num_trees){
    pred_name <- as.name(paste0("pred_", i))
    assessment_DF <- oob_data_list[[i]]
    if(!is.na(incMSE) & incMSE %in% names(assessment_DF)){
      assessment_DF[incMSE] <- sample(pull(assessment_DF,incMSE))
    }
    
    pred_output <- mutate(assessment_DF, !!pred_name := predict(model_list[[i]],
                                                              newdata = assessment_DF))
    
    temp_pred[[i]] <- select(pred_output, .data$index, where(dep_var), !!pred_name)
  }
  ret <- join_all(temp_pred, by = "index", type='full')
  if(resid){
    ret_pred <- rowMeans(select(.data$ret, -.data$index, where(dep_var)), na.rm = TRUE)
    return(ret[[dep_var]] - ret_pred)
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
#' data(Example_data, package = "DSIWastewater")
#' model <- random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
#' gen_INCMSE(model)
gen_INCMSE <- function(tree_model){
  num_ignore = length(all.vars(tree_model@formula[[3]][[2]])) + 1
  model_list <- tree_model@models
  oob_data_list <- tree_model@oob_data
  col_names <- names(tree_model@data)
  ret_DF_list <- list()[[2]]
  dep_var <- tree_model@formula
  
  base_MSE <- gen_OOB_pred(tree_model)%>%
                    mutate(across(starts_with("pred_"), ~ (dep_var - .x)**2))%>%
                    summarise(across(starts_with("pred_"), ~ mean(.x, na.rm = TRUE)))
  
  for(i in (num_ignore+1):(length(col_names))){
    I_MSE <- gen_OOB_pred(tree_model,
                           incMSE = col_names[i])%>%
      mutate(across(starts_with("pred_"), ~ (dep_var - .x)**2))%>%
      summarise(across(starts_with("pred_"), ~ mean(.x, na.rm = TRUE)))
    
    diff_vector = as.numeric(I_MSE[1,]) - as.numeric(base_MSE[1,])
    
    ret_DF_list[[i - num_ignore]] <- data.frame(
      incMSE = 100*mean(diff_vector, na.rm = TRUE)/sd(diff_vector, na.rm = TRUE),
      old_incMSE = 100*mean(diff_vector, na.rm = TRUE)/mean(as.numeric(base_MSE[1,])),
      men = mean(diff_vector, na.rm = TRUE),
      sd = sd(diff_vector, na.rm = TRUE),
      var = col_names[i])
  }
  ret <- bind_rows(ret_DF_list)
  return(ret)
}


#' get OOB MSE vs number of forest in trees
#'
#' @param tree_model random_linear_forest model to calculate the OOB mean squared error
#'
#' @return get dataframe of number of trees and OOB MSE
#' @export
#'
#' @examples
#' data(Example_data, package = "DSIWastewater")
#' model <- random_linear_forest(Example_data, 2, PMMoV ~ N1 + N2 | pcr_type)
#' OOB_MSE_num_trees(model)
OOB_MSE_num_trees <- function(tree_model){
  pred_DF = gen_OOB_pred(tree_model)
  OOB_MSE_TREE <- list()
  dep_var <- tree_model@formula
  for(i in 1:length(tree_model@models)){
    OOB_MSE_TREE[[i]] <- pred_DF%>%
      mutate(mean_pred = rowMeans(select(pred_DF,
                                  .data$pred_1:paste0("pred_",i)), na.rm = TRUE))%>%
      summarise(model_MSE = mean((!!sym(dep_var) - .data$mean_pred)**2, na.rm = TRUE))%>%
      mutate(num_tree = i)
  }
  return(bind_rows(OOB_MSE_TREE))
}
