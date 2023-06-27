#' Expand formula for increased info
#' takes a formula with shape A ~ B | C 
#' and convert . to its real representation
#'
#' @param X formula object like A ~ B | C 
#' @param data data object to extract . info from
#'
#' @return formula object
expand_formula <- function(X, data){
  dep <- X[[2]]
  expand_dep <- as.character(X[[3]])
  form_list <- list()
  for(i in 2:3){
    
    temp_formula <-as.formula(paste0("A ~", expand_dep[i]))
    formula_fixed <- attr(terms.formula(temp_formula, data = data, simplify = TRUE), "term.labels")
    formula_fixed <- formula_fixed[formula_fixed != dep]
    formula_fixed <- paste(formula_fixed, collapse = " + ")
    form_list[i-1] = formula_fixed
  }
  
  ret_string <- as.formula(paste(dep, "~", form_list[[1]] , "|" , form_list[[2]]))
  return(ret_string)
}
