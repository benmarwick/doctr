#' Create summary statistics for every column in X (no grouping)
#' 
#' @param X table to be examined
examine_ <- function(X) {
  cols <- names(X)
  
  X <- X %>%
    as.list() %>%
    purrr::map(~list(.x)) %>%
    purrr::map(function(.x) {
      names(.x) <- "data"
      .x
    })
  
  numeric <- dplyr::tibble()
  character <- dplyr::tibble()
  categorical <- dplyr::tibble()
  for (i in 1:length(X)) {
    X[[i]] <- switch(
      class(X[[i]]$data),
      numeric = suppressWarnings(profile_num(X[[i]])),
      integer = suppressWarnings(profile_num(X[[i]])),
      character = suppressWarnings(profile_chr(X[[i]])),
      factor = suppressWarnings(profile_fct(X[[i]]))
    )
    
    if (class(X[[i]]$data) == "numeric" || class(X[[i]]$data) == "integer") {
      X[[i]]$data <- NULL
      X[[i]] <- unlist(list(list(name = cols[i]), X[[i]]), recursive = FALSE)
      numeric <- dplyr::bind_rows(numeric, X[[i]])
    } else if (class(X[[i]]$data) == "character") {
      X[[i]]$data <- NULL
      X[[i]] <- unlist(list(list(name = cols[i]), X[[i]]), recursive = FALSE)
      character <- dplyr::bind_rows(character, X[[i]])
    } else {
      X[[i]]$data <- NULL
      X[[i]] <- unlist(list(list(name = cols[i]), X[[i]]), recursive = FALSE)
      categorical <- dplyr::bind_rows(categorical, X[[i]])
    }
  }
  
  return(list(numeric, character, categorical))
}

#' Create summary statistics for every column of X, varying
#'   summarization strategy depending on the type of variable
#'   
#' This function determines the types of the variables in
#'   \code{X} (numeric, text or factor) and creates a report
#'   for each type of variable; these reports can be accessed
#'   with \code{report_[num|chr|fct]()} and more information
#'   about these are available at \code{vignette("doctr_examine")}
#' 
#' @param X Table to be examined
#' @param group A variable (name or index) to group X by before examining
#' 
#' @export
examine <- function(X, group = 0) {
  if (group == 0) {
    return(examine_(X))
  }
  
  if (!is.numeric(group)) {
    group <- grep(group, names(X))
  }
  
  X <- X %>%
    split(.[[group]]) %>%
    purrr::map(examine_)
  
  return(X)
}
