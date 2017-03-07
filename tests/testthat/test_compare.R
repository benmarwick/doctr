library(doctr)
library(tidyverse)
context("Compare")

# Setup
data(mpg)
mpg2 <- mpg %>%
  mutate(class = as.factor(class))
c <- mpg2 %>%
  compare(sample_n(mpg2, 100))
c2 <- mpg2 %>%
  compare(sample_n(mpg2, 100), 0.5)

test_that("comapre() returns a valid list", {
  expect_is(c, "list")
})

test_that("comapre() result has correct length", {
  expect_length(c, 11)
})

test_that("diagnose() results have more than two FALSE", {
  suppressWarnings(len <- sum(flatten_lgl(transpose(c)$result)))
  
  expect_gt(len, 2)
})

test_that("diagnose() results differ based on ci", {
  suppressWarnings(len <- sum(flatten_lgl(transpose(c)$result)))
  suppressWarnings(len2 <- sum(flatten_lgl(transpose(c2)$result)))
  
  expect_gt(len, len2)
})
