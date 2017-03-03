#' Return summary of columns of numerics in a table
#' 
#' @param X list created by 'examine'
#' @param group group from which to retrieve summary
#' 
#' @export
report_num <- function(X, group = "") {
  if (group == "") {
    return(tibble::as_tibble(X[[1]]))
  }
  
  return(tibble::as_tibble(X[[group]][[1]]))
}

#' Return summary of columns of characters in a table
#' 
#' @param X list created by 'examine'
#' @param group group from which to retrieve summary
#' 
#' @export
report_chr <- function(X, group = "") {
  if (group == "") {
    return(tibble::as_tibble(X[[2]]))
  }
  
  return(tibble::as_tibble(X[[group]][[2]]))
}

#' Return summary of columns of categoricals in a table
#' 
#' @param X list created by 'examine'
#' @param group group from which to retrieve summary
#' 
#' @export
report_fct <- function(X, group = "") {
  if (group == "") {
    X <- tibble::as_tibble(X[[3]]) %>%
      dplyr::select(-unq)
    
    X <- suppressWarnings(tidyr::unnest(X))
    return(X)
  }
  
  X <- tibble::as_tibble(X[[group]][[3]]) %>%
    dplyr::select(-unq)
  
  
  X <- suppressWarnings(tidyr::unnest(X))
  return(X)
}
