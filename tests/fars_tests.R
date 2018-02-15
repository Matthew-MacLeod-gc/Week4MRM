library(testthat)
expect_that(Week4MRM::make_filename(2014), matches("accident_2014.csv.bz2"))
