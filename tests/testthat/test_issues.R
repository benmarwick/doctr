library(doctr)
library(tidyverse)
context("Issues")

# Setup
data(mpg)
exams <- mpg %>%
  mutate(class = as.factor(class)) %>%
  guess_exams()
exams$max_val[9] <- 30
exams$max_unq[10] <- 3
exams$least_frec_cls[11] <- 0.2
d <- mpg %>%
  mutate(class = as.factor(class)) %>%
  diagnose(exams)
mpg2 <- mpg %>%
  mutate(class = as.factor(class))
c <- mpg2 %>%
  compare(sample_n(mpg2, 100))
suppressMessages(id <- issues(d))
suppressMessages(ic <- issues(c))
d2 <- mpg %>%
  mutate(class = as.factor(class)) %>%
  diagnose()

test_that("issues() doesn't return anything", {
  expect_null(id)
  expect_null(ic)
})

test_that("issues() outputs a message with issues found", {
  expect_message(issues(d), "Issues")
  expect_message(issues(d2), "No")
  expect_message(issues(c), "Issues")
})

test_that("issues() can print verbose messages", {
  expect_message(issues(d, verbose = TRUE), "There")
  expect_message(issues(c, verbose = TRUE), "New")
})

test_that("issues() can print verbose messages for specific variables", {
  expect_message(issues(d, i = 2, verbose = TRUE))
  expect_message(issues(c, i = 2, verbose = TRUE))
  expect_message(issues(c, i = "hwy", verbose = TRUE))
})
