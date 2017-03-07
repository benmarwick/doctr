library(doctr)
library(tidyverse)
context("Report")

# Setup
data(mpg)
e <- mpg %>%
  mutate(class = as.factor(class)) %>%
  examine()
e2 <- mpg %>%
  mutate(class = as.factor(class)) %>%
  examine("class")
n <- report_num(e)
t <- report_txt(e)
f <- report_fct(e)

test_that("report_*() returns valid tables", {
  expect_is(n, "tbl")
  expect_is(t, "tbl")
  expect_is(f, "tbl")
})

test_that("report_*() results have correct length", {
  expect_length(n, 26)
  expect_length(t, 25)
  expect_length(f, 4)
})

test_that("report_*() results have correct number of rows", {
  expect_equal(nrow(n), 5)
  expect_equal(nrow(t), 5)
  expect_equal(nrow(f), 7)
})

test_that("report_*() can fetch grouped results", {
  expect_is(report_num(e2, "suv"), "tbl")
  expect_is(report_txt(e2, "midsize"), "tbl")
  expect_is(report_fct(e2, "compact"), "tbl")
})
