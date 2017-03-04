#' @title Checks if a table is in its expected form
#'
#' @description Runs tests on a table to check if its variables pass
#'   certain standards and fit certain assumptions
#'   specified via \code{exams}
#'   
#' @details This function receives a table and a battery of exams
#'   that its variables should pass; if a variable doesn't
#'   pass any of these tests, comprehensive reports are
#'   created (you can access them with \code{issues()})
#' 
#' @param X Table to run tests on
#' @param exams Tests to be run on \code{X} (see \code{vignette("doctr_diagnose")}
#'   for more information)
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#'   
#' # Running default diagnostics on a table
#' txhousing %>% diagnose() %>% issues()
#'   
#' # Running custom diagnostics on a table
#' exams <- guess_exams(txhousing)
#' exams$max_val[5] <- 2000000000
#' txhousing %>% diagnose(exams) %>% issues(verbose = TRUE)
#' }
#' 
#' @export
diagnose <- function(X, exams = guess_exams(X)) {
  exams[is.na(exams)] <- ""
  funs <- translate(exams$funs)
  
  X <- exams %>%
    dplyr::mutate(
      data = purrr::map(exams[[1]], ~X[[.x]])
    ) %>%
    purrr::transpose() %>%
    purrr::map(function(arg) {
      arg$x <- list(data = arg$data, result = TRUE)
      arg$data <- NULL
      
      arg
    }) %>%
    purrr::map(~purrr::keep(.x, function(x) any(x != ""))) %>%
    purrr::map(function(x) {
      x$cols <- NULL
      x$funs <- NULL
      
      x
    })
  
  for (i in 1:length(funs)) {
    suppressWarnings(X[[i]] <- purrr::invoke(funs[[i]], X[[i]]))
  }
  names(X) <- exams[[1]]
  
  return(X)
}
