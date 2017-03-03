#' Run tests on a table to check its variables pass
#'   certain standards and fit certain assumptions
#'   specified via \code{exams}
#'   
#' This function receives a table and a battery of exams
#'   that its variables should pass; if a variable doesn't
#'   pass any of these tests, comprehensive reports are
#'   created (you can access them with \code{issues()})
#' 
#' @param X Table to run tests on
#' @param exams Tests to be run on X (see \code{vignette
#'   ("doctr_diagnose")} for more information)
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
