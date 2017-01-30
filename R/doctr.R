


## PROBLEMS ---------------------------------------------------------

#' Create message with problems found in x
#' 
#' @param x list with data, result, and any errors already found
#' @param name name of column referring to 'x'
problems_ <- function(x, name) {
  if (x$result) {
    return(paste0("No problems found in '", name, "'\n"))
  }
  
  msg <- paste0("Problems found in '", name, "'\n")
  for (i in 3:length(x)) {
    msg <- paste0(msg, "    ", x[[i]], "\n")
  }
  
  return(msg)
}

#' Print messages with problems found in 'X' (or in one of its columns)
#' 
#' @param X list of lists with data, result, and any errors already found for each column
#' @param i index of column from which to print problems
problems <- function(X, i = 0) {
  
  if (i != 0) {
    msg <- problems_(X[[i]], names(X)[i])
    message(stringr::str_sub(msg, 1, -2))
  }
  else {
    msg <- ""
    
    for (i in 1:length(X)) {
      msg <- paste0(msg, problems_(X[[i]], names(X)[i]))
    }
    message(stringr::str_sub(msg, 1, -2))
  }
}



## CHECKS -----------------------------------------------------------

#' Check if 'x$data' has length > 'len'
#' 
#' @param x list with data, result, and any errors already found
#' @param len minimum length 'x$data' can have
check_len <- function(x, len) {
  if (length(x$data) < len) {
    x$len <- "Data has length 0"
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if 'x$data' is of type 'type'
#' 
#' @param x list with data, result, and any errors already found
#' @param type 'x$data' should have
check_type <- function(x, type) {
  if (typeof(x$data) != type) {
    x$type <- paste0("Data isn't of type ", type)
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if fraction of 'x$data' that is NA is => 'min_na'
#' 
#' @param x list with data, result, and any errors already found
#' @param min_na minimum fraction of 'x$data' that can be NA
#' @param rm_na whether NAs should be removed once test is over
check_min_na <- function(x, min_na, rm_na = FALSE) {
  if (sum(is.na(x$data))/length(x$data) < min_na) {
    x$min_na <- paste0("Less than ", min_na*100, "% of entries are NAs")
    x$result <- FALSE
  }
  
  if (rm_na) {
    x$data <- x$data[!is.na(x$data)]
  }
  
  return(x)
}

#' Check if fraction of 'x$data' that is NA is <= 'max_na'
#' 
#' @param x list with data, result, and any errors already found
#' @param max_na maximum fraction of 'x$data' that can be NA
#' @param rm_na whether NAs should be removed once test is over
check_max_na <- function(x, max_na, rm_na = FALSE) {
  if (sum(is.na(x$data))/length(x$data) > max_na) {
    x$max_na <- paste0("More than ", max_na*100, "% of entries are NAs")
    x$result <- FALSE
  }
  
  if (rm_na) {
    x$data <- x$data[!is.na(x$data)]
  }
  
  return(x)
}

#' Check if no entry of 'x$data' has more decimal places than 'mdp'
#' 
#' @param x list with data, result, and any errors already found
#' @param mdp maximum number of decimal places an entry in 'x$data' can have
check_mdp <- function(x, mdp) {
  dp <- stringr::str_length(stringr::str_extract(as.character(x$data), "\\.[0-9]*")) - 1
  dp[is.na(dp)] <- 0
  
  if (sum(dp > mdp) > 0) {
    x$mdp <- paste0(sum(dp > mdp), " entries have more than ", mdp, " decimal places")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if no entry of 'x$data' is larger than 'max_val'
#' 
#' @param x list with data, result, and any errors already found
#' @param max_val maximum value an entry in 'x$data' can have
check_max_val <- function(x, max_val) {
  if (sum(x$data > max_val) > 0) {
    x$max_val <- paste0(sum(x$data > max_val), " entries are larger than ", max_val)
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if no entry of 'x$data' is smaller than 'min_val'
#' 
#' @param x list with data, result, and any errors already found
#' @param min_val maximum value an entry in 'x$data' can have
check_min_val <- function(x, min_val) {
  if (sum(x$data < min_val) > 0) {
    x$min_val <- paste0(sum(x$data < min_val), " entries are smaller than ", min_val)
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if there aren't less than 'min_unq' classes in 'x$data'
#' 
#' @param x list with data, result, and any errors already found
#' @param min_unq minimum number of distinct classes 'x$data' can have
check_min_unq <- function(x, min_unq) {
  unq <- unique(x$data)
  
  if (length(unq) < min_unq) {
    x$min_unq <- paste0("There are less than ", min_unq, " unique classes")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if there aren't more than 'max_unq' classes in 'x$data'
#' 
#' @param x list with data, result, and any errors already found
#' @param max_unq maximum number of distinct classes 'x$data' can have
check_max_unq <- function(x, max_unq) {
  unq <- unique(x$data)
  
  if (length(unq) > max_unq) {
    x$max_unq <- paste0("There are more than ", max_unq, " unique classes")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if all classes represent at least 'lfc' of 'x$data'
#' 
#' @param x list with data, result, and any errors already found
#' @param lfc minimum fraction of total for least frequent class
check_lfc <- function(x, lfc) {
  lf <- lfc*length(x$data)
  c <- table(x$data)[table(x$data) < lf]
  
  if (length(c) > 0) {
    x$lfc <- paste0("There are ", length(c), " classes that represent less than ",
                    lfc*100, "% of the total")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if no classe represents more than 'mfc' of 'x$data'
#' 
#' @param x list with data, result, and any errors already found
#' @param mfc maximum fraction of total for most frequent class
check_mfc <- function(x, mfc) {
  mf <- mfc*length(x$data)
  c <- table(x$data)[table(x$data) > mf]
  
  if (length(c) > 0) {
    x$mfc <- paste0("There are ", length(c), " classes that represent more than ",
                    mfc*100, "% of the total")
    x$result <- FALSE
  }
  
  return(x)
}



## PRE-SET EXAMS ----------------------------------------------------

#' Check if 'x$data' is a continuous variable
#' 
#' @param x list with data, result, and any errors already found
#' @param min_val minimum value 'x$data' can have
#' @param max_val maximum value 'x$data' can have
#' @param max_na fraction of 'x$data' that can be NA
#' @param max_dec_places maximum number of decimal places in values of 'x$data'
#' 
#' @rdname is_continuous
is_continuous <- function(x, min_val = -Inf, max_val = Inf, max_na = 1.0, max_dec_places = Inf) {
  min_val <- as.numeric(min_val)
  max_val <- as.numeric(max_val)
  max_na <- as.numeric(max_na)
  max_dec_places <- as.numeric(max_dec_places)
  
  x <- x %>%
    check_len(0) %>%
    check_type("double") %>%
    check_max_na(max_na, TRUE) %>%
    check_mdp(max_dec_places) %>%
    check_max_val(max_val) %>%
    check_min_val(min_val)
  
  return(x)
}

#' Check if 'x$data' is a count variable
#'
#' @rdname is_continuous
is_count <- function(x, min_val = 0, max_val = Inf, max_na = 1.0, max_dec_places = 0) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a quantity variable
#'
#' @rdname is_continuous
is_quantity <- function(x, min_val = 0, max_val = Inf, max_na = 1.0, max_dec_places = Inf) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a percentage variable
#'
#' @rdname is_continuous
is_percentage <- function(x, min_val = 0, max_val = 1, max_na = 1.0, max_dec_places = Inf) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a money variable
#'
#' @rdname is_continuous
is_money <- function(x, min_val = 0, max_val = 10000, max_na = 1.0, max_dec_places = 2) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a categorical variable
#' 
#' @param x list with data, result, and any errors already found
#' @param min_unq minimum number of unique classes 'x$data' can have
#' @param max_unq maximum number of unique classes 'x$data' can have
#' @param max_na fraction of 'x$data' that can be NA
#' @param least_frec_cls minimum fraction of total represented by least frequent class
#' 
#' @rdname is_categorical
is_categorical <- function(x, min_unq = 0, max_unq = Inf, max_na = 1.0, least_frec_cls = 0) {
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



## PROFILING --------------------------------------------------------

#' Create profile for table
#' 
#' @param X table in list form
profile_tbl <- function(X) {
  meta <- list()
  meta$ncol <- length(X)
  
  meta$names <- paste(names(X), collapse = " ")
  meta$types <- paste(sapply(map(X, ~.x[[1]]), typeof), collapse = " ")
  
  X$meta <- meta
  
  return(X)
}

#' Create profile for column of doubles
#' 
#' @param x list with data, and all of the summary statistics of that data
profile_dbl <- function(x) {
  x$min <- min(x$data, na.rm = TRUE)
  x$max <- max(x$data, na.rm = TRUE)
  
  qntl <- quantile(x$data, c(0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95,
                             0.99), na.rm = TRUE)
  x <- append(x, as.list(qntl))
  
  x$mean <- mean(x$data, na.rm = TRUE)
  x$sd <- sd(x$data, na.rm = TRUE)
  
  x$na <- sum(is.na(x$data))
  x$val <- length(x$data) - x$na
  
  x$neg <- sum(x$data < 0, na.rm = TRUE)
  x$zero <- sum(x$data == 0, na.rm = TRUE)
  x$pos <- sum(x$data > 0, na.rm = TRUE)
  
  x$unq <- length(unique(x$data))
  
  dp <- str_length(str_extract(as.character(x$data), "\\.[0-9]*")) - 1
  dp[is.na(dp)] <- 0
  x$mdp <- max(dp)
  
  return(x)
}

#' Create profile for column of characters
#' 
#' @param x list with data, and all of the summary statistics of that data
profile_chr <- function(x) {
  str_len <- suppressWarnings(str_length(x$data))
  str_len[is.na(str_len)] <- 0
  
  x$min <- min(str_len)
  x$max <- max(str_len)
  
  qntl <- quantile(str_len, c(0.01, 0.05, seq(0.1, 0.9, 0.1), 0.95,
                              0.99), na.rm = TRUE)
  x <- append(x, as.list(qntl))
  
  x$mean <- mean(str_len, na.rm = TRUE)
  x$sd <- sd(str_len, na.rm = TRUE)
  
  x$na <- sum(is.na(x$data))
  x$val <- length(x$data) - x$na
  
  x$unq <- length(unique(x$data))
  
  sample <- paste(sample(x$data, 100), collapse = "")
  x$asc <- ifelse(as.character(guess_encoding(charToRaw(sample))[1, 1])
                  == "ASCII", 1, 0)
  x$ltr <- str_count(sample, "[a-zA-Z ]")/str_length(sample)
  x$num <- str_count(sample, "[0-9]")/str_length(sample)
  
  return(x)
}

#' Create profile of every column in X
#' 
#' @param X table in list form
profile <- function(X) {
  
  X <- X %>%
    as.list() %>%
    map(~list(.x)) %>%
    map(function(.x) {
      names(.x) <- "data"
      .x
    })
  
  X <- profile_tbl(X)
  for (i in 1:(length(X) - 1)) {
    X[[i]] <- switch(typeof(X[[i]]$data),
                     double = profile_dbl(X[[i]]),
                     integer = profile_dbl(X[[i]]),
                     character = profile_chr(X[[i]]))
  }
  
  X <- X %>%
    map(function(.x){
      .x$data <- NULL
      .x
    })
  
  return(X)
}



## EXPORTED ---------------------------------------------------------

#' Run tests on a table to check if it fits it's expected form
#' 
#' @param X table to run tests on
#' @param exams tests to be run on 'X'
#' 
#' @export
diagnose <- function(X, exams) {
  exams[is.na(exams)] <- ""
  funs <- purrr::map(exams$funs, get)
  
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

#' Compare the profiles of two tables
#' 
#' @param X table used as standard for comparison
#' @param Y table to be evaluated
#' 
#' @export
compare <- function(X, Y) {
  X <- profile(X)
  Y <- profile(Y)
  
  # if (X$meta != Y$meta) {
  #   return(FALSE)
  # }
  
  dist <- c()
  for (i in 1:(length(X) - 1)) {
    dist <- append(dist, dist(bind_rows(flatten(X[[i]]), flatten(Y[[i]])))[1])
  }
  
  return(dist)
}


