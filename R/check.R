#' Check if x$data has length > len
#' 
#' @param x List with data, result, and any errors already found
#' @param len Minimum length x$data can have
check_len <- function(x, len) {
  if (length(x$data) < len) {
    x$len <- "Data has length 0"
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if x$data is of type type
#' 
#' @param x List with data, result, and any errors already found
#' @param type x$data should have
check_type <- function(x, type) {
  if (!stringr::str_detect(type, class(x$data))) {
    x$type <- paste0("Data isn't of type ", type)
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if fraction of x$data that is NA is => min_na
#' 
#' @param x List with data, result, and any errors already found
#' @param min_na Minimum fraction of x$data that can be NA
#' @param rm_na Whether NAs should be removed once test is over
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

#' Check if fraction of x$data that is NA is <= max_na
#' 
#' @param x List with data, result, and any errors already found
#' @param max_na Maximum fraction of x$data that can be NA
#' @param rm_na Whether NAs should be removed once test is over
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

#' Check if no entry of x$data has more decimal places than mdp
#' 
#' @param x List with data, result, and any errors already found
#' @param mdp Maximum number of decimal places an entry in x$data can have
check_mdp <- function(x, mdp) {
  dp <- stringr::str_length(stringr::str_extract(as.character(x$data), "\\.[0-9]*")) - 1
  dp[is.na(dp)] <- 0
  
  if (sum(dp > mdp) > 0) {
    x$mdp <- paste0(sum(dp > mdp), " entries have more than ", mdp, " decimal places")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if no entry of x$data is larger than max_val
#' 
#' @param x List with data, result, and any errors already found
#' @param max_val Maximum value an entry in x$data can have
check_max_val <- function(x, max_val) {
  if (sum(x$data > max_val) > 0) {
    x$max_val <- paste0(sum(x$data > max_val), " entries are larger than ", max_val)
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if no entry of x$data is smaller than min_val
#' 
#' @param x List with data, result, and any errors already found
#' @param min_val Maximum value an entry in x$data can have
check_min_val <- function(x, min_val) {
  if (sum(x$data < min_val) > 0) {
    x$min_val <- paste0(sum(x$data < min_val), " entries are smaller than ", min_val)
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if there arent less than min_unq classes in x$data
#' 
#' @param x List with data, result, and any errors already found
#' @param min_unq Minimum number of distinct classes x$data can have
check_min_unq <- function(x, min_unq) {
  unq <- unique(x$data)
  
  if (length(unq) < min_unq) {
    x$min_unq <- paste0("There are less than ", min_unq, " unique classes")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if there arent more than max_unq classes in x$data
#' 
#' @param x List with data, result, and any errors already found
#' @param max_unq Maximum number of distinct classes x$data can have
check_max_unq <- function(x, max_unq) {
  unq <- unique(x$data)
  
  if (length(unq) > max_unq) {
    x$max_unq <- paste0("There are more than ", max_unq, " unique classes")
    x$result <- FALSE
  }
  
  return(x)
}

#' Check if all classes represent at least lfc of x$data
#' 
#' @param x List with data, result, and any errors already found
#' @param lfc Minimum fraction of total for least frequent class
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

#' Check if no classe represents more than mfc of x$data
#' 
#' @param x List with data, result, and any errors already found
#' @param mfc Maximum fraction of total for most frequent class
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
