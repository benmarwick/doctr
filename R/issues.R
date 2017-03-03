#' Create message with issues found in x
#' 
#' @param x list with data, result, and any errors already found
#' @param name name of column referring to 'x'
#' @param verbose specify the issues found in each column
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

#' Print messages with issues found in 'X' (or in one of its columns)
#' 
#' @param X list of lists with data, result, and any errors already found for each column
#' @param i index of column from which to print issues
#' @param verbose specify the issues found in each column
#' 
#' @export
issues <- function(X, i = 0, verbose = TRUE) {
  if (!is.numeric(i)) {
    i <- grep(i, names(X))
  }
  
  if (i != 0) {
    msg <- issues_(X[[i]], names(X)[i], verbose)
    message(stringr::str_sub(msg, 1, -2))
  }
  else {
    msg <- ""
    
    for (i in 1:length(X)) {
      msg <- paste0(msg, issues_(X[[i]], names(X)[i], verbose))
    }
    message(stringr::str_sub(msg, 1, -2))
  }
}
