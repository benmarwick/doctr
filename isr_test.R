source("K:/Projetos em andamentos/Samsung/ISR/isr.R")

## is_continuous(x)
## is_continuous(x, min_val = -Inf, max_val = Inf, allow_na = TRUE, rational = TRUE, max_dec_places = Inf)

# Não permite vetores vazios
x <- c()
is_continuous(x)

# Aceita inteiros
x <- c(1, 2, 3, 4)
is_continuous(x)

# Aceita racionais
x <- c(1.1, 2.2, 3.3, 4)
is_continuous(x)

# Não tem limites superior nem inferior
x <- c(10001, 2.2, 3.3, -40001)
is_continuous(x)

# Permite NAs
x <- c(10001, NA, 3.3, -40001)
is_continuous(x)

# Não tem limite de casas decimais
x <- c(10001, NA, 3.14159, -40001)
is_continuous(x)

# Não permite strings
x <- c(10001, NA, 3.14159, -40001, "1")
is_continuous(x)


## is_count(x)
## is_continuous(x, min_val = 0, max_val = Inf, allow_na = TRUE, rational = FALSE, max_dec_places = Inf)

# Aceita inteiros
x <- c(1, 2, 3, 4)
is_count(x)

# Aceita racionais
x <- c(1.1, 2.2, 3.3, 4)
is_count(x)

# Não aceita negativos
x <- c(10001, 2.2, 3.3, -40001)
is_count(x)

# Permite NAs
x <- c(10001, NA, 3.3, -40001)
is_count(x)


## is_quantity(x)
## is_continuous(x, min_val = 0, max_val = Inf, allow_na = TRUE, rational = TRUE, max_dec_places = Inf)

# Aceita inteiros
x <- c(1, 2, 3, 4)
is_quantity(x)

# Aceita racionais
x <- c(1.1, 2.2, 3.3, 4)
is_quantity(x)

# Não aceita negativos
x <- c(10001, 2.2, 3.3, -40001)
is_quantity(x)

# Permite NAs
x <- c(10001, NA, 3.3, -40001)
is_quantity(x)

# Não tem limite de casas decimais
x <- c(10001, NA, 3.14159, -40001)
is_quantity(x)


## is_percentage(x)
## is_continuous(x, min_val = 0, max_val = 100, allow_na = TRUE, rational = TRUE, max_dec_places = Inf)

# Aceita inteiros
x <- c(1, 2, 3, 4)
is_percentage(x)

# Aceita racionais
x <- c(1.1, 2.2, 3.3, 4)
is_percentage(x)

# Não aceita negativos nem positivos > 100
x <- c(10001, 2.2, 3.3, -40001)
is_percentage(x)

# Pode permitir apenas porcentagens [0,1] com facilidade
x <- c(0, 0.3, 0.5, 1, 1.2)
is_percentage(x, max_val = 1)

# Permite NAs
x <- c(10001, NA, 3.3, -40001)
is_percentage(x)

# Não tem limite de casas decimais
x <- c(10001, NA, 3.14159, -40001)
is_percentage(x)


## is_money(x)
## is_continuous(x, min_val = 0, max_val = 10000, allow_na = TRUE, rational = TRUE, max_dec_places = 2)

# Aceita inteiros
x <- c(1, 2, 3, 4)
is_money(x)

# Aceita racionais
x <- c(1.1, 2.2, 3.3, 4)
is_money(x)

# Não aceita negativos nem positivos > 10000
x <- c(10001, 2.2, 3.3, -40001)
is_money(x)

# Permite NAs
x <- c(10001, NA, 3.3, -40001)
is_money(x)

# Não tem limite de casas decimais
x <- c(10001, NA, 3.14159, -40001)
is_money(x)


## Rodando análises em uma base

# Obter dados para os exames
data <- read_excel("C:/Users/ctruzzi/Desktop/teste_isr.xlsx", 2)
exams <- read_excel("C:/Users/ctruzzi/Desktop/teste_isr.xlsx", 1)

# Rodar análise automatizada numa base
resultados <- diagnose(data, exams)

# Obter problemas encontrados nos testes
problems(resultados)

# Obter problemas encontrados nos testes para uma dada coluna
problems(resultados, 3)
problems(resultados, "tipo_pagamento")

