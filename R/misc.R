# Declaring global variables
utils::globalVariables(c(".", "data", "n", "cnt", "unq"))

#' Convert vector of types to corresponding functions
#' 
#' @param types vector with data types (money, count, etc.)
translate <- function(types) {
  
  new_funs <- c()
  for (i in 1:length(types)) {
    new_funs <- append(
      new_funs,
      switch(
        types[i],
        money = is_money,
        count = is_count,
        quantity = is_quantity,
        continuous = is_continuous,
        character = is_character,
        categorical = is_categorical
      )
    )
  }
  
  return(new_funs)
}

#' @title Guesses column types
#'
#' @description Tries to guess what exams a table's variables should go
#'   through in \code{disgnose()}
#'   
#' @details This function samples 20\% of \code{X} and tries to roughly identify
#'   what are its variables' types (money, count, etc.) and, once
#'   this process is done, it creates a table with the chosen
#'   exams given the identified types; you can learn more about
#'   the output of this function and how to customize it at
#'   \code{vignette("doctr_diagnose")}
#' 
#' @param X Table to be examined
#' @param verbose Whether to specify the parsed column specifications
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#'   
#' # Running custom diagnostics on a table
#' exams <- guess_exams(txhousing)
#' exams$max_val[5] <- 2000000000
#' txhousing %>% diagnose(exams) %>% issues(verbose = TRUE)
#' }
#' 
#' @export
guess_exams <- function(X, verbose = FALSE) {
  cols <- names(X)
  s_size <- round(0.2*nrow(X), 0)
  
  X <- X %>%
    as.list() %>%
    purrr::map(~list(.x)) %>%
    purrr::map(function(.x) {
      names(.x) <- "data"
      .x$data <- sample(.x$data, s_size)
      .x$result <- TRUE
      .x
    })
  
  funs <- c()  
  for (i in 1:length(X)) {
    if (class(X[[i]]$data) == "numeric" || class(X[[i]]$data) == "integer") {
      if (is_percentage(X[[i]])$result) {
        funs[i] <- "percentage"
      }
      else if (!is_money(X[[i]], max_dec_places = 1)$result &&
               is_money(X[[i]])$result) {
        funs[i] <- "money"
      }
      else if (is_count(X[[i]])$result) {
        funs[i] <- "count"
      }
      else if (is_quantity(X[[i]])$result) {
        funs[i] <- "quantity"
      }
      else {
        funs[i] <- "continuous"
      }
    }
    else if (class(X[[i]]$data) == "character") {
      funs[i] <- "character"
    }
    else {
      funs[i] <- "categorical"
    }
  }
  
  exams <- cbind(
    cols, funs, max_na = "",
    min_val = "", max_val = "", max_dec_places = "",
    min_unq = "", max_unq = "", least_frec_cls = ""
  )
  
  if (verbose) {
    msg <- "Parsed with column specification:\ncols(\n"
    for (i in 1:length(cols)) {
      msg <- stringr::str_c(msg, "    ", cols[i], " = ", funs[i], "\n")
    }
    msg <- stringr::str_c(msg, ")")
    message(msg)
  }
  
  return(tibble::as_tibble(exams))
}
