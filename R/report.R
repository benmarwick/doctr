#' @title Fetch EDA of a specific type of column
#' 
#' @description Returns table with summary statistics of \code{X}'s
#'   numeric|text|factor variables (based on results from \code{examine(X)})
#'   
#' @details This function uses the result of \code{examine()}'s
#'   execution to generate a report on a table's numeric|text|factor
#'   variables; it is also able to retrieve the results
#'   from only one group given that \code{examine()}
#'   was used with its \code{group} argument
#' 
#' @param res A list returned from \code{examine()}
#' @param group Group from which to retrieve summary
#'   (see \code{vignette("doctr_examine")} for more
#'   information)
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#'   
#' # Getting EDA for numeric and text variables
#' txhousing %>% examine() %>% report_num()
#' txhousing %>% examine() %>% report_txt()
#'   
#' # Getting EDA with grouping (of a factor variable)
#' txhousing %>%
#'   mutate(city = as.factor(city)) %>%
#'   examine(1) %>%
#'   report_fct("Austin")
#' }
#' 
#' @name report


#' @rdname report
#' 
#' @export
report_num <- function(res, group = "") {
  if (group == "") {
    return(tibble::as_tibble(res[[1]]))
  }
  
  return(tibble::as_tibble(res[[group]][[1]]))
}

#' @rdname report
#' 
#' @export
report_txt <- function(res, group = "") {
  if (group == "") {
    return(tibble::as_tibble(res[[2]]))
  }
  
  return(tibble::as_tibble(res[[group]][[2]]))
}

#' @rdname report
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
