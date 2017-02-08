


## MISC -------------------------------------------------------------

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
  
  return(tibble::as_tibble(exams))
}



## SUMMARIES --------------------------------------------------------

#' Return summary of columns of numerics in a table
#' 
#' @param X list created by 'examine'
#' @param group group from which to retrieve summary
#' 
#' @export
report_num <- function(X, group = "") {
  if (group == "") {
    return(tibble::as_tibble(X[[1]]))
  }
  
  return(tibble::as_tibble(X[[group]][[1]]))
}

#' Return summary of columns of characters in a table
#' 
#' @param X list created by 'examine'
#' @param group group from which to retrieve summary
#' 
#' @export
report_chr <- function(X, group = "") {
  if (group == "") {
    return(tibble::as_tibble(X[[2]]))
  }
  
  return(tibble::as_tibble(X[[group]][[2]]))
}

#' Return summary of columns of categoricals in a table
#' 
#' @param X list created by 'examine'
#' @param group group from which to retrieve summary
#' 
#' @export
report_fct <- function(X, group = "") {
  if (group == "") {
    X <- tibble::as_tibble(X[[3]]) %>%
      dplyr::select(-unq) %>%
      tidyr::unnest()
    return(X)
  }
  
  X <- tibble::as_tibble(X[[group]][[3]]) %>%
    dplyr::select(-unq) %>%
    tidyr::unnest()
  return(X)
}



## ISSUES -----------------------------------------------------------

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
  if (stringr::str_detect(class(x$data), type)) {
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

#' Check if 'x$data' is a character variable
#' 
#' @param x list with data, result, and any errors already found
#' @param min_unq minimum number of unique classes 'x$data' can have
#' @param max_unq maximum number of unique classes 'x$data' can have
#' @param max_na fraction of 'x$data' that can be NA
#' @param least_frec_cls minimum fraction of total represented by least frequent class
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

#' Check if 'x$data' is a continuous variable
#' 
#' @param x list with data, result, and any errors already found
#' @param min_val minimum value 'x$data' can have
#' @param max_val maximum value 'x$data' can have
#' @param max_na fraction of 'x$data' that can be NA
#' @param max_dec_places maximum number of decimal places in values of 'x$data'
#' 
#' @rdname is_continuous
is_continuous <- function(x, min_val = -Inf, max_val = Inf, max_na = 0.9, max_dec_places = Inf) {
  min_val <- as.numeric(min_val)
  max_val <- as.numeric(max_val)
  max_na <- as.numeric(max_na)
  max_dec_places <- as.numeric(max_dec_places)
  
  x <- x %>%
    check_len(0) %>%
    check_type("numeric ou integer") %>%
    check_max_na(max_na, TRUE) %>%
    check_mdp(max_dec_places) %>%
    check_max_val(max_val) %>%
    check_min_val(min_val)
  
  return(x)
}

#' Check if 'x$data' is a quantity variable
#'
#' @rdname is_continuous
is_quantity <- function(x, min_val = 0, max_val = Inf, max_na = 0.9, max_dec_places = Inf) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a count variable
#'
#' @rdname is_continuous
is_count <- function(x, min_val = 0, max_val = Inf, max_na = 0.9, max_dec_places = 0) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a money variable
#'
#' @rdname is_continuous
is_money <- function(x, min_val = 0, max_val = 10000, max_na = 0.9, max_dec_places = 2) {
  is_continuous(x, min_val, max_val, max_na, max_dec_places)
}

#' Check if 'x$data' is a percentage variable
#'
#' @rdname is_continuous
is_percentage <- function(x, min_val = 0, max_val = 1, max_na = 0.9, max_dec_places = Inf) {
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
is_categorical <- function(x, min_unq = 0, max_unq = Inf, max_na = 0.9, least_frec_cls = 0) {
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



## PROFILES ---------------------------------------------------------

#' Create profile for table
#' 
#' @param X table in list form
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
#' @param x list with data of a column
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
  x$val <- (length(x$data) - x$na)/length(x$data)
  
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
#' @param x list with data of a column
profile_chr <- function(x) {
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
  x$val <- (length(x$data) - x$na)/length(x$data)
  
  x$unq <- length(unique(x$data))
  
  str <- paste(x$data, collapse = "")
  x$asc <- ifelse(as.character(readr::guess_encoding(charToRaw(str))[1, 1])
                  == "ASCII", 1, 0)
  x$ltr <- stringr::str_count(str, "[a-zA-Z ]")/stringr::str_length(str)
  x$num <- stringr::str_count(str, "[0-9]")/stringr::str_length(str)
  
  return(x)
}

#' Create profile for column of characters
#' 
#' @param x list with data of a column
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
#' @param X table to be profiled
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
      character = profile_chr(X[[i]]),
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



## MAIN -------------------------------------------------------------

#' Create summary statistics for every column in 'X' (no grouping)
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

#' Create summary statistics for every column in 'X'
#' 
#' @param X table to be examined
#' @param group variable to group X by before examining
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

#' Run tests on a table to check if it fits it's expected form
#' 
#' @param X table to run tests on
#' @param exams tests to be run on 'X'
#' 
#' @export
diagnose <- function(X, exams) {
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

#' Compare the profiles of two tables
#' 
#' @param X table used as standard for comparison
#' @param Y table to be evaluated
#' @param ci percentage to be used for the CI
#' 
#' @export
compare <- function(X, Y, ci = 0.05) {
  prof_X <- profile(X)
  prof_Y <- profile(Y)
  
  prof_X <- prof_X %>%
    map(function(.x) {
      .x$list <- NULL
      .x
    })
  prof_Y <- prof_Y %>%
    map(function(.x) {
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
    map(function(.x) {
      map(.x, function(.x) {
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


