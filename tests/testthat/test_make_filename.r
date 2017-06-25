library(ffrmv)

test_that("make_filename works", expect_that(make_filename('2014'), equals("accident_2014.csv.bz2")))