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

#' Try to guess which exam a dataset should go through
#' 
#' @param X table to be examined
#' 
#' @export
guess_exams <- function(X) {
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
  
  msg <- "Parsed with column specification:\ncols(\n"
  for (i in 1:length(cols)) {
    msg <- stringr::str_c(msg, "    ", cols[i], " = ", funs[i], "\n")
  }
  msg <- stringr::str_c(msg, ")")
  message(msg)
  
  return(tibble::as_tibble(exams))
}
