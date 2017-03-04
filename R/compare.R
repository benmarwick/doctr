#' @title Compares two tables
#' 
#' @description Compares the profiles of two tables, checking if
#'   they can be considered similar enough
#' 
#' @details This funcion takes 100 random samples with replacement
#'   of table \code{X}, creating confidence intervals (of size
#'   \code{ci}) for its summary statistics; it then verifies
#'   what summary statistics of \code{Y} don't fall inside these
#'   confidence intervals and creates comprehensive reports about
#'   them (you can access these with \code{issues()})
#' 
#' @param X Table used as standard for comparison
#' @param Y Table to be evaluated
#' @param ci The size of the confidence interval
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#'   
#' # Comparing a table to itself
#' txhousing %>% compare(txhousing) %>% issues()
#'   
#' # Comparing two different tables
#' txhousing %>% compare(sample_n(txhousing, 20)) %>% issues(verbose = TRUE)
#' }
#' 
#' @export
compare <- function(X, Y, ci = 0.05) {
  prof_X <- profile(X)
  prof_Y <- profile(Y)
  
  prof_X <- prof_X %>%
    purrr::map(function(.x) {
      .x$list <- NULL
      .x
    })
  prof_Y <- prof_Y %>%
    purrr::map(function(.x) {
      .x$list <- NULL
      .x
    })
  
  results <- prof_X %>%
    purrr::map(~list(.x))
  results <- mapply(append, results, TRUE, SIMPLIFY = FALSE)
  results <- results %>%
    purrr::map(function(.x) {
      names(.x) <- c("data", "result")
      .x
    })
  
  for (i in 1:3) {
    if (prof_X$meta[[i]] != prof_Y$meta[[i]]) {
      results$meta$result <- FALSE
      results$meta$meta <- "Metadata for both tables is different"
      return(list(meta = results$meta))
    }
  }
  
  results$meta <- NULL
  
  sample_X <- purrr::map(1:100, function(x, data){
    dplyr::sample_n(data, nrow(data), TRUE)
  }, data = X) %>%
    purrr::map(~profile(.x)) %>%
    purrr::map(function(.x) {
      purrr::map(.x, function(.x) {
        .x$list <- NULL
        .x
      })
    }) %>%
    purrr::transpose()
  
  sample_X$meta <- NULL
  
  sample_X <- sample_X %>%
    purrr::map(~purrr::transpose(.x)) %>%
    purrr::map(function(.x) {
      .x <- purrr::map(.x, purrr::flatten_dbl)
      
      for (i in 1:length(.x)) {
        .x[[i]] <- as.numeric(
          stats::quantile(.x[[i]], c(ci/2, 1 - ci/2), na.rm = TRUE)
        )
      }
      .x
    })
  
  for (i in 1:(length(prof_Y) - 1)) {
    for (j in 1:length(prof_Y[[i]])) {
      
      if (names(prof_X[[i]])[j] == "unq") {
        if (prof_Y[[i]][[j]] > prof_X[[i]][[j]] * 1.5) {
          results[[i]]$result <- FALSE
          results[[i]][[names(prof_X[[i]])[j]]] <- paste0(
            "New value for '",
            names(prof_X[[i]])[j],
            "' is too high"
          )
        }
        else if (prof_Y[[i]][[j]] < prof_X[[i]][[j]] * 0.5) {
          results[[i]]$result <- FALSE
          results[[i]][[names(prof_X[[i]])[j]]] <- paste0(
            "New value for '",
            names(prof_X[[i]])[j],
            "' is too low"
          )
        }
      }
      else if (names(prof_X[[i]])[j] != "len" && names(prof_X[[i]])[j] != "list") {
        if (prof_Y[[i]][[j]] > sample_X[[i]][[j]][2]) {
          results[[i]]$result <- FALSE
          results[[i]][[names(prof_X[[i]])[j]]] <- paste0(
            "New value for '",
            names(prof_X[[i]])[j],
            "' is too high"
          )
        }
        else if (prof_Y[[i]][[j]] < sample_X[[i]][[j]][1]) {
          results[[i]]$result <- FALSE
          results[[i]][[names(prof_X[[i]])[j]]] <- paste0(
            "New value for '",
            names(prof_X[[i]])[j],
            "' is too low"
          )
        }
      }
    }
  }
  
  return(results)
}
