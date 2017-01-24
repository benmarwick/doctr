library(tidyverse)
library(stringr)
library(lubridate)
library(purrr)
library(assertthat)
library(readxl)

# Imprimir um objeto logi_prob
print.logi_prob <- function(x) {
  print(x == TRUE)
}

# Imprimir os problemas associados a uma coluna de uma tabela
problems <- function(x, i = 0) {
  if (!is.numeric(i)) {
    i <- grep(i, x$columns)
  }
  
  if (i == 0) {
    if (is.null(dim(x))) {
      return(attr(x, "problems"))
    }
    
    y <- attr(x, "problems")
    msg <- ""
    for (i in 1:length(y$columns)) {
      msg <- str_c(msg, "For ", y$columns[i], ":\n")
      msg <- str_c(msg, y$problems[i], "\n")
    }
    
    msg <- str_sub(msg, 1, -2)
    message(msg)
  }
  else {
    y <- attr(x, "problems")
    msg <- ""
    
    msg <- str_c(msg, "For ", y$columns[i], ":\n")
    msg <- str_c(msg, y$problems[i], "\n")
    
    msg <- str_sub(msg, 1, -2)
    message(msg)
  }
}

# Verificar se é um vetor de variável continua atendendo aos parâmetros especificados
is_continuous <- function(x, min_val = -Inf, max_val = Inf, allow_na = TRUE, rational = TRUE, max_dec_places = Inf) {
  min_val = as.numeric(min_val)
  max_val = as.numeric(max_val)
  allow_na = as.logical(allow_na)
  rational = as.logical(rational)
  max_dec_places = as.numeric(max_dec_places)
  
  msg <- ""
  flag <- TRUE
  
  # Verificar o comprimento do vetor
  if (length(x) == 0) {
    msg <- paste0(msg, "  Vector has length 0\n")
    
    msg <- str_sub(msg, 1, -2)
    ret <- FALSE
    attr(ret, "problems") <- msg
    class(ret) <- c("logi_prob", class(ret))
    return(ret)
  }
  
  # Verificar a presença de NAs
  if (!allow_na && sum(is.na(x)) > 0) {
    count <- sum(is.na(x))
    msg <- paste0(msg, "  Found ", count, " NAs\n")
    flag <- FALSE
  }
  
  x <- x[!is.na(x)]
  
  # Verificar a presença de não-inteiros/não-racionais
  if (!rational) {
    if (length(x) > 0 && sum(map_lgl(x, function(i) !are_equal(i, as.integer(i)))) > 0) {
      count <- sum(map_lgl(x, function(i) !are_equal(i, as.integer(i))))
      msg <- paste0(msg, "  Found ", count, " non-integers\n")
      x <- x[!map_lgl(x, function(i) !are_equal(i, as.integer(i)))]
      flag <- FALSE
    }
  }
  else {
    if (length(x) > 0 && sum(map_lgl(x, function(i) !are_equal(i, as.numeric(i)))) > 0) {
      count <- sum(map_lgl(x, function(i) !are_equal(i, as.numeric(i))))
      msg <- paste0(msg, "  Found ", count, " non-numerics\n")
      x <- x[!map_lgl(x, function(i) !are_equal(i, as.numeric(i)))]
      flag <- FALSE
    }
    
    if (max_dec_places > 0) {
      len <- str_length(str_extract(as.character(x), "\\.[0-9]*")) - 1
      len[is.na(len)] <- 0
      
      if (sum(len > max_dec_places) > 0) {
        count <- sum(len > max_dec_places)
        msg <- paste0(msg, "  Found ", count, " values with more than ", max_dec_places, " decimal places\n")
        x <- x[len <= max_dec_places]
        flag <- FALSE
      }
    }
  }
  
  # Verificar o valor máximo
  if (length(x) > 0 && sum(x > max_val)) {
    count <- sum(x > max_val)
    msg <- paste0(msg, "  Found ", count, " values larger than max_val\n")
    x <- x[x <= max_val]
    flag <- FALSE
  }
  
  # Verificar o valor mínimo
  if (length(x) > 0 && sum(x < min_val)) {
    count <- sum(x < min_val)
    msg <- paste0(msg, "  Found ", count, " values smaller than min_val\n")
    x <- x[x >= min_val]
    flag <- FALSE
  }
  
  # Caso não tenham sido encontrados problemas, retornar TRUE
  if (flag) {
    ret <- TRUE
    attr(ret, "problems") <-"  No problems found"
    class(ret) <- c("logi_prob", class(ret))
    return(ret)
  }
  
  # Caso tenham sido encontrados problemas, retornar uma mensagem e FALSE
  msg <- str_sub(msg, 1, -2)
  ret <- FALSE
  attr(ret, "problems") <- msg
  class(ret) <- c("logi_prob", class(ret))
  return(ret)
}

# Verificar se é um vetor de contagens (números, inteiros, [0, Inf])
is_count <- function(x, min_val = 0, max_val = Inf, allow_na = TRUE) {
  is_continuous(x, min_val, max_val, allow_na, rational = FALSE)
}

# Verificar se é um vetor de quantidades (números, racionais, [-Inf, Inf])
is_quantity <- function(x, min_val = 0, max_val = Inf, allow_na = TRUE) {
  is_continuous(x, min_val, max_val, allow_na)
}

# Verificar se é um vetor de porcentagens (números, racionais, [0, 100])
is_percentage <- function(x, min_val = 0, max_val = 100, allow_na = TRUE) {
  is_continuous(x, min_val, max_val, allow_na)
}

# Verificar se é um vetor de valores monetários (números, racionais, [0, Inf])
is_money <- function(x, min_val = 0, max_val = 10000, allow_na = TRUE, max_dec_places = 2) {
  is_continuous(x, min_val, max_val, allow_na, max_dec_places = max_dec_places)
}

# Rodar bateria de testes
run_tests <- function(cols, funs, args) {
  results <- c()
  problems <- c()

  for (i in 1:length(funs)) {
    suppressWarnings(logi_prob <- invoke(funs[[i]], args[[i]]))
    results <- append(results, logi_prob)
    problems <- append(problems, problems(logi_prob))
  }
  
  ret <- tibble(columns = cols, results)
  probs <- tibble(columns = cols, problems)
  
  attr(ret, "problems") <- probs
  class(ret) <- c("results_prob", class(ret))
  
  return(ret)
}

# Obter inputs para uma bateria de testes
diagnose <- function(data, exams) {

  # Remover NAs
  exams[is.na(exams)] <- ""
  
  # Obter os nomes das colunas
  cols <- exams$cols
  
  # Transformar os nomes de funções em uma lista de chamadas
  funs <- map(exams$funs, get)
  
  # Transformar as colunas em listas dentro de 'exams'
  args <- exams %>%
    mutate(
      x = map(cols, ~data[[.x]])
    ) %>%
    transpose()
  
  # Remover argumentos vazios
  args <- args %>% map(~keep(.x, function(x) any(x != "")))
  
  # Remover funções e nomes de coluna da lista de argumentos
  args <- args %>% map(function(x) {
    x[["cols"]] <- NULL
    x[["funs"]] <- NULL
    x
  })
  
  # Rodar os testes estabelecidos
  run_tests(cols, funs, args)
}

eda <- function(x) {
  na <- length(x[is.na(x)])
  non_na <- length(x) - na
  
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  
  qntl <- quantile(x, na.rm = TRUE, c(00.1, 0.05, seq(0.1, 0.9, 0.1), 0.95, 0.99))
  names(qntl) <- NULL
  
  as.character(c(na, non_na, min, qntl, max))
}

examine <- function(data, cols) {
  results <- c()
  
  for(i in 1:length(cols)) {
    results <- rbind(results, c(cols[i], eda(data[[cols[i]]])))
  }
  
  results <- as_tibble(results)
  
  names(results) <- c("column", "missing", "value", "min", "p01", "p05", "p10",
                      "p20", "p30", "p40", "p50", "p60", "p70", "p80", "p90", "p95", "p99", "max")
  
  results
}













