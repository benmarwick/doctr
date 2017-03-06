#' Create profile for table
#' 
#' @param X Table in list form
profile_tbl <- function(X) {
  meta <- list()
  meta$ncol <- length(X)
  
  meta$names <- paste(names(X), collapse = " ")
  meta$types <- paste(sapply(purrr::map(X, ~.x[[1]]), class), collapse = " ")
  
  X$meta <- meta
  
  return(X)
}

#' Create profile for column of numerics
#' 
#' @param x List with data of a column
profile_num <- function(x) {
  x$len <- length(x$data)
  
  x$min <- min(x$data, na.rm = TRUE)
  x$max <- max(x$data, na.rm = TRUE)
  
  qntl <- stats::quantile(x$data, c(0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95,
                                    0.99), na.rm = TRUE)
  x <- append(x, as.list(qntl))
  
  x$mean <- mean(x$data, na.rm = TRUE)
  x$sd <- stats::sd(x$data, na.rm = TRUE)
  
  x$na <- sum(is.na(x$data))/length(x$data)
  x$val <- sum(!is.na(x$data))/length(x$data)
  
  x$neg <- sum(x$data < 0, na.rm = TRUE)/length(x$data)
  x$zero <- sum(x$data == 0, na.rm = TRUE)/length(x$data)
  x$pos <- sum(x$data > 0, na.rm = TRUE)/length(x$data)
  
  x$unq <- length(unique(x$data))
  
  dp <- stringr::str_length(stringr::str_extract(as.character(x$data), "\\.[0-9]*")) - 1
  dp[is.na(dp)] <- 0
  x$mdp <- max(dp)
  
  return(x)
}

#' Create profile for column of characters
#' 
#' @param x List with data of a column
profile_txt <- function(x) {
  x$len <- length(x$data)
  
  str_len <- suppressWarnings(stringr::str_length(x$data))
  str_len[is.na(str_len)] <- 0
  
  x$min <- min(str_len)
  x$max <- max(str_len)
  
  qntl <- stats::quantile(str_len, c(0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95,
                                     0.99), na.rm = TRUE)
  x <- append(x, as.list(qntl))
  
  x$mean <- mean(str_len, na.rm = TRUE)
  x$sd <- stats::sd(str_len, na.rm = TRUE)
  
  x$na <- sum(is.na(x$data))/length(x$data)
  x$val <- sum(!is.na(x$data))/length(x$data)
  
  x$unq <- length(unique(x$data))
  
  str <- paste(x$data[!is.na(x$data)], collapse = "")
  x$asc <- ifelse(as.character(readr::guess_encoding(charToRaw(str))[1, 1])
                  == "ASCII", 1, 0)
  x$ltr <- stringr::str_count(str, "[a-zA-Z ]")/stringr::str_length(str)
  x$num <- stringr::str_count(str, "[0-9]")/stringr::str_length(str)
  
  return(x)
}

#' Create profile for column of characters
#' 
#' @param x List with data of a column
profile_fct <- function(x) {
  x$unq <- length(unique(x$data))
  
  l <- tibble::as_tibble(x = list(data = x$data)) %>%
    dplyr::group_by(data) %>%
    dplyr::summarise(cnt = n())
  
  tot <- sum(l$cnt)
  
  l <- l %>%
    dplyr::mutate(frq = cnt/tot) %>%
    list()
  
  x$list <- l
  
  return(x)
}

#' Create profile of every column in X
#' 
#' @param X Table to be profiled
profile <- function(X) {
  
  X <- X %>%
    as.list() %>%
    purrr::map(~list(.x)) %>%
    purrr::map(function(.x) {
      names(.x) <- "data"
      .x
    })
  
  X <- profile_tbl(X)
  for (i in 1:(length(X) - 1)) {
    X[[i]] <- switch(
      class(X[[i]]$data),
      numeric = profile_num(X[[i]]),
      integer = profile_num(X[[i]]),
      character = profile_txt(X[[i]]),
      factor = profile_fct(X[[i]])
    )
  }
  
  X <- X %>%
    purrr::map(function(.x){
      .x$data <- NULL
      .x
    })
  
  return(X)
}
