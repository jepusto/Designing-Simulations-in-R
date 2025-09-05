

# demo testing file

my_DGP <- function( N, mu, beta ) {
  stopifnot( N > 0, beta <= 1 )
  dat = tibble( X = rnorm( N, mean = 0, sd = 1 ),
                Y = mu + beta * X + rnorm( N, sd = 1-beta^2 ) )
}


library(testthat)
set.seed(44343)
test_that("my_DGP works as expected", {
  dta <- my_DGP(10, 0, 0.5)
  # Check that the output is a tibble
  expect_s3_class(dta, "tbl_df")
  
  # Check that the output has the right number of rows
  expect_equal(nrow(dta), 10)
  
  # Check that the output has the right columns
  expect_true(all(c("X", "Y") %in% colnames(dta)))
  
  # Check that the mean of Y is close to mu
  dta2 = my_DGP(1000, 2, 0.5)
  expect_equal(mean(dta2$Y), 2, tolerance = 0.1)
  
  # Check we get an error when we should
  expect_error(my_DGP(-10, 0, 0.5, 0.5) )
})


test_that("my_DGP works as expected (test 2)", {
  dta <- my_DGP(10000, 2, -2)
  expect_equal( sd( dta$X ), 1, tolerance = 0.02 )
  
  dta <- my_DGP(10000, 2, 0.5)
  expect_equal( var(dta$Y), 1, tolerance = 0.02 )
  
  M = lm( Y ~ X, data=dta )
  expect_equal( coef(M)[[2]], 0.5, tolerance = 0.02 )
} )