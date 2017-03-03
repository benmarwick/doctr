#' Return table with summary statistics of X's numeric
#'   variables (based on results from \code{examine(X)})
#'   
#' This function uses the result of \code{examine()}'s
#'   execution to generate a report on a table's numeric
#'   variables; it is also able to retrieve the results
#'   from only one group given that \code{examine()}
#'   was used with its \code{group} argument
#' 
#' @param res A list returned from \code{examine()}
#' @param group Group from which to retrieve summary
#'   (see \code{vignette("doctr_examine")} for more
#'   information)
#' 
#' @export
report_num <- function(res, group = "") {
  if (group == "") {
    return(tibble::as_tibble(res[[1]]))
  }
  
  return(tibble::as_tibble(res[[group]][[1]]))
}

#' Return table with summary statistics of X's text
#'   variables (based on results from \code{examine(X)})
#'   
#' This function uses the result of \code{examine()}'s
#'   execution to generate a report on a table's text
#'   variables; it is also able to retrieve the results
#'   from only one group given that \code{examine()}
#'   was used with its \code{group} argument
#' 
#' @param res A list returned from \code{examine()}
#' @param group Group from which to retrieve summary
#'   (see \code{vignette("doctr_examine")} for more
#'   information)
#' 
#' @export
report_chr <- function(res, group = "") {
  if (group == "") {
    return(tibble::as_tibble(res[[2]]))
  }
  
  return(tibble::as_tibble(res[[group]][[2]]))
}

#' Return table with summary statistics of X's factor
#'   variables (based on results from \code{examine(X)})
#'   
#' This function uses the result of \code{examine()}'s
#'   execution to generate a report on a table's factor
#'   variables; it is also able to retrieve the results
#'   from only one group given that \code{examine()}
#'   was used with its \code{group} argument
#' 
#' @param res A list returned from \code{examine()}
#' @param group Group from which to retrieve summary
#'   (see \code{vignette("doctr_examine")} for more
#'   information)
#' 
#' @export
report_fct <- function(res, group = "") {
  if (group == "") {
    res <- tibble::as_tibble(res[[3]]) %>%
      dplyr::select(-unq)
    
    res <- suppressWarnings(tidyr::unnest(res))
    return(res)
  }
  
  res <- tibble::as_tibble(res[[group]][[3]]) %>%
    dplyr::select(-unq)
  
  
  res <- suppressWarnings(tidyr::unnest(res))
  return(res)
}
