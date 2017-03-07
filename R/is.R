#' Check if x$data is a character variable
#' 
#' @param x List with data, result, and any errors already found
#' @param min_unq Minimum number of unique classes x$data can have
#' @param max_unq Maximum number of unique classes x$data can have
#' @param max_na Fraction of x$data that can be NA
#' @param least_frec_cls Minimum fraction of total represented by least frequent class
#' 
#' @rdname is_character
is_character <- function(x, min_unq = 0, max_unq = Inf, max_na = 0.9, least_frec_cls = 0) {
  min_unq <- as.numeric(min_unq)
  max_unq <- as.numeric(max_unq)
  max_na <- as.numeric(max_na)
  least_frec_cls <- as.numeric(least_frec_cls)
  
  x <- x %>%
    check_len(0) %>%
    check_type("character") %>%
    check_max_na(max_na, TRUE) %>%
    check_min_unq(min_unq) %>%
    check_max_unq(max_unq) %>%
    check_lfc(least_frec_cls)
  
  return(x)
}

#' Check if x$data is a continuous variable
#' 
#' @param x List with data, result, and any errors already found
#' @param min_val Minimum value x$data can have
#' @param max_val Maximum value x$data can have
#' @param max_na Fraction of x$data that can be NA
#' @param max_dec_places Maximum number of decimal places in values of x$data
#' 
#' @rdname is_continuous
is_continuous <- function(x, min_val = -Inf, max_val = Inf, max_na = 0.9, max_dec_places = Inf) {
  min_val <- as.numeric(min_val)
  max_val <- as.numeric(max_val)
  max_na <- as.numeric(max_na)
  max_dec_places <- as.numeric(max_dec_places)
  
  x <- x %>%
    check_len(0) %>%
    check_type("numeric or integer") %>%
    check_max_na(max_na, TRUE) %>%
    check_mdp(max_dec_places) %>%
    check_max_val(max_val) %>%
    check_min_val(min_val)
  
  return(x)
}

#' Check if x$data is a quantity variable
#'
#' @rdname is_continuous
is_quantity <- function(x, min_val = 0, max_val = Inf, max_na = 0.9, max_dec_places = Inf) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if x$data is a count variable
#'
#' @rdname is_continuous
is_count <- function(x, min_val = 0, max_val = Inf, max_na = 0.9, max_dec_places = 0) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if x$data is a money variable
#'
#' @rdname is_continuous
is_money <- function(x, min_val = 0, max_val = Inf, max_na = 0.9, max_dec_places = 2) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if x$data is a percentage variable
#'
#' @rdname is_continuous
is_percentage <- function(x, min_val = 0, max_val = 1, max_na = 0.9, max_dec_places = Inf) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if x$data is a categorical variable
#' 
#' @param x List with data, result, and any errors already found
#' @param min_unq Minimum number of unique classes x$data can have
#' @param max_unq Maximum number of unique classes x$data can have
#' @param max_na Fraction of x$data that can be NA
#' @param least_frec_cls Minimum fraction of total represented by least frequent class
#' 
#' @rdname is_categorical
is_categorical <- function(x, min_unq = 0, max_unq = Inf, max_na = 0.9, least_frec_cls = 0) {
  min_unq <- as.numeric(min_unq)
  max_unq <- as.numeric(max_unq)
  max_na <- as.numeric(max_na)
  least_frec_cls <- as.numeric(least_frec_cls)
  
  x <- x %>%
    check_len(0) %>%
    check_type("factor") %>%
    check_max_na(max_na, TRUE) %>%
    check_min_unq(min_unq) %>%
    check_max_unq(max_unq) %>%
    check_lfc(least_frec_cls)
  
  return(x)
}
