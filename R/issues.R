#' Create message with issues found in x
#' 
#' @param x List with data, result, and any errors already found
#' @param name Name of column referring to x
#' @param verbose Specify the issues found in each column
issues_ <- function(x, name, verbose) {
  if (x$result) {
    return(paste0("No issues found in '", name, "'\n"))
  }
  
  msg <- paste0("Issues found in '", name, "'\n")
  if (verbose) {
    for (i in 3:length(x)) {
      msg <- paste0(msg, "    ", x[[i]], "\n")
    }
  }
  
  return(msg)
}

#' @title Prints issues found in other functions
#'
#' @description Prints messages with issues found in \code{X} (or one of its columns)
#'   after running either \code{diagnose(X)} or \code{compare(X)}
#'   
#' @details This function interprets the results from \code{diagnose()} and
#'   \code{compare()}, and generates a comprehensive report printed as
#'   messages to the console; for more information about what gets printed
#'   consult \code{vignette("doctr_diagnose")} or \code{vignette("doctr_examine")}
#' 
#' @param res A list returned from \code{diagnose()} or \code{compare()}
#' @param i Index or name of column from which to print issues
#' @param verbose Specify what issues were found in each column
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#'   
#' # Getting issues from diagnostic
#' txhousing %>% diagnose() %>% issues()
#'   
#' # Getting issues from comparison
#' txhousing %>% compare(txhousing) %>% issues()
#'   
#' # Getting issues from specific variable
#' txhousing %>% diagnose() %>% issues("inventory", verbose = TRUE)
#' txhousing %>% compare(sample_n(txhousing, 20)) %>% issues(1, verbose = TRUE)
#' }
#' 
#' @export
issues <- function(res, i = 0, verbose = FALSE) {
  if (!is.numeric(i)) {
    i <- grep(i, names(res))
  }
  
  if (i != 0) {
    msg <- issues_(res[[i]], names(res)[i], verbose)
    message(stringr::str_sub(msg, 1, -2))
  }
  else {
    msg <- ""
    
    for (i in 1:length(res)) {
      msg <- paste0(msg, issues_(res[[i]], names(res)[i], verbose))
    }
    message(stringr::str_sub(msg, 1, -2))
  }
}
