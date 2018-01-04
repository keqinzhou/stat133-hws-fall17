# Script containing unit tests
#library(testthat)
#source('functions.R')

library('testthat')

#remove_missing
context("remove_missing function")
test_that("remove_missing works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  d <- c(NA, 2, 3)
  expect_equal(remove_missing(a), c(1,2,3))
  expect_equal(remove_missing(b), c(1,2,3))
  expect_equal(remove_missing(c), c(1,2))
  expect_equal(remove_missing(d), c(2,3))
})


#get_minimum
context("get_minimum function")
test_that("get_minimum works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_minimum(a,na.rm = TRUE), c(1))
  expect_equal(get_minimum(b,na.rm = TRUE), c(1))
  expect_equal(get_minimum(c,na.rm = TRUE), c(1))
})
test_that("get_minimum throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_minimum(strings))
})


#get_maximum
context("get_maximum function")
test_that("get_maximum works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_maximum(a,na.rm = TRUE), c(3))
  expect_equal(get_maximum(b,na.rm = TRUE), c(3))
  expect_equal(get_maximum(c,na.rm = TRUE), c(2))
})
test_that("get_maximum throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_maximum(strings))
})


#get_range
context("get_range function")
test_that("get_range works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, 7, NA)
  expect_equal(get_range(a,na.rm = TRUE), c(2))
  expect_equal(get_range(b,na.rm = TRUE), c(2))
  expect_equal(get_range(c,na.rm = TRUE), c(6))
})
test_that("get_range throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_range(strings))
})

#get_percentile10
context("get_percentile10 function")
test_that("get_percentile10 works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_percentile10(a,na.rm = TRUE), c(1.2))
  expect_equal(get_percentile10(b,na.rm = TRUE), c(1.2))
  expect_equal(get_percentile10(c, na.rm = TRUE), c(1.1))
})
test_that("get_percentile10 throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_percentile10(strings))
})

#get_percentile90
context("get_percentile90 function")
test_that("get_percentile90 works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_percentile90(a,na.rm = TRUE), c(2.8))
  expect_equal(get_percentile90(b,na.rm = TRUE), c(2.8))
  expect_equal(get_percentile90(c, na.rm = TRUE), c(1.9))
})
test_that("get_percentile90 throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_percentile90(strings))
})

#get_median
context("get_median function")
test_that("get_median works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_median(a,na.rm = TRUE), c(2))
  expect_equal(get_median(b,na.rm = TRUE), c(2))
  expect_equal(get_median(c,na.rm = TRUE), c(1.5))

})
test_that("get_median throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_median(strings))
})

#get_average
context("get_average function")
test_that("get_average works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_average(a,na.rm = TRUE), c(2))
  expect_equal(get_average(b,na.rm = TRUE), c(2))
  expect_equal(get_average(c,na.rm = TRUE), c(1.5))
  
})
test_that("get_average throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_average(strings))
})

#get_stdev
context("get_stdev function")
test_that("get_stdev works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_stdev(a,na.rm = TRUE), c(1))
  expect_equal(get_stdev(b,na.rm = TRUE), c(1))
  expect_equal(get_stdev(c,na.rm = TRUE), c(sd(c(1,2))))
  
})
test_that("get_stdev throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_stdev(strings))
})

#get_quartile1
context("get_quartile1 function")
test_that("get_quartile1 works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_quartile1(a,na.rm = TRUE), c(1.5))
  expect_equal(get_quartile1(b,na.rm = TRUE), c(1.5))
  expect_equal(get_quartile1(c,na.rm = TRUE), c(1.25))
})
test_that("get_quartile1 throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_quantile1(strings))
})

#get_quartile3
context("get_quartile3 function")
test_that("get_quartile3 works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  expect_equal(get_quartile3(a,na.rm = TRUE), c(2.5))
  expect_equal(get_quartile3(b,na.rm = TRUE), c(2.5))
  expect_equal(get_quartile3(c,na.rm = TRUE), c(1.75))
})
test_that("get_quartile3 throws error", {
  strings <- c('1', '2', '3')
  expect_error(get_quantile3(strings))
})

#count_missing
context("count_missing function")
test_that("count_missing works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 3, NA)
  c <- c(1, 2, NA, NA)
  d <- c(1, NA, 2, NA, NA)
  expect_equal(count_missing(a), c(0))
  expect_equal(count_missing(b), c(1))
  expect_equal(count_missing(c), c(2))
  expect_equal(count_missing(d), c(3))
})

#rescale100
context("rescale100 function")
test_that("rescale100 works", {
  a <- c(1, 2, 3)
  b <- c(1, 2)
  expect_equal(rescale100(a,0,20), c(5,10,15))
  expect_equal(rescale100(a,0,40), c(2.5,5,7.5))
  expect_equal(rescale100(b,0,20), c(5,10))
  expect_equal(rescale100(b,0,40), c(2.5,5))
})

#drop_lowest
context("drop_lowest function")
test_that("drop_lowest works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 8, 7, 9)
  c <- c(4, 8, 2)
  d <- c(3, 5)
  expect_equal(drop_lowest(a), c(2,3))
  expect_equal(drop_lowest(b), c(2,8,7,9))
  expect_equal(drop_lowest(c), c(4,8))
  expect_equal(drop_lowest(d), c(5))
})

#score_hw
context("score_homework function")
test_that("score_homework works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 8, 7, 9)
  expect_equal(score_homework(a,drop=TRUE), c(2.5))
  expect_equal(score_homework(b,drop=TRUE), c(6.5))
  expect_equal(score_homework(a,drop=FALSE), c(2))
  expect_equal(score_homework(b,drop=FALSE), c(5.4))
})

#score_quiz
context("score_quiz function")
test_that("score_quiz works", {
  a <- c(1, 2, 3)
  b <- c(1, 2, 8, 7, 9)
  expect_equal(score_quiz(a,drop=TRUE), c(2.5))
  expect_equal(score_quiz(b,drop=TRUE), c(6.5))
  expect_equal(score_quiz(a,drop=FALSE), c(2))
  expect_equal(score_quiz(b,drop=FALSE), c(5.4))
})

#score_lab
context("score_lab function")
test_that("score_lab works", {
  expect_equal(score_lab(11), c(100))
  expect_equal(score_lab(9), c(60))
  expect_equal(score_lab(7), c(20))
  expect_equal(score_lab(4), c(0))
})

