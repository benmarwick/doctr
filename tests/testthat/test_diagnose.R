library(doctr)
library(tidyverse)
context("Diagnose")

# Setup
data(mpg)
d <- mpg %>%
  mutate(class = as.factor(class)) %>%
  diagnose()
exams <- mpg %>%
  mutate(class = as.factor(class)) %>%
  guess_exams()

test_that("diagnose() returns valid lists", {
  expect_is(d, "list")
  expect_is(transpose(d)$result, "list")
  expect_is(transpose(d)$data, "list")
})

test_that("diagnose() results has correct length", {
  expect_length(d, 11)
  expect_length(transpose(d)$result, 11)
  expect_length(transpose(d)$data, 11)
})

test_that("diagnose() results are all TRUE", {
  expect_true(all(flatten_lgl(transpose(d)$result) == TRUE))
})

test_that("guess_exams() returns a valid table", {
  expect_is(exams, "tbl")
})

test_that("guess_exams() result has correct width and length", {
  expect_length(exams, 9)
  expect_equal(nrow(exams), 11)
})

test_that("diagnose() finds issues correctly", {
  exams$max_val[9] <- 30
  exams$max_unq[10] <- 3
  exams$least_frec_cls[11] <- 0.2
  
  d2 <- mpg %>%
    mutate(class = as.factor(class)) %>%
    diagnose(exams)
  
  suppressWarnings(len <- sum(flatten_lgl(transpose(d2)$result)))
             
  expect_equal(len, 8)
})
