library(doctr)
library(tidyverse)
context("Examine")

# Setup
data(mpg)
e <- mpg %>%
  mutate(class = as.factor(class)) %>%
  examine()
e2 <- mpg %>%
  mutate(class = as.factor(class)) %>%
  examine("class")

test_that("examine() returns a valid list", {
  expect_is(e, "list")
})

test_that("examine() results have correct length", {
  expect_length(e, 3)
  expect_length(e[[1]], 26)
  expect_length(e[[2]], 25)
  expect_length(e[[3]], 3)
})

test_that("examine() results have correct number of rows", {
  expect_equal(nrow(e[[1]]), 5)
  expect_equal(nrow(e[[2]]), 5)
  expect_equal(nrow(e[[3]]), 1)
})

test_that("examine() is able to group correcly", {
  expect_is(e2, "list")
  expect_length(e2, 7)
})
