context("test-morey")

# error messages ttest
test_that("error messages ttest", {

  
  expect_error(morey_plot.ttest(n=NULL,
                                type = "two.sample",
                                alternative = "two.sided",
                                alpha_level = .045))
  
  expect_error(morey_plot.ttest(n=2,
                                type = "two.sample",
                                alternative = "two.sided",
                                alpha_level = .045))
  
  expect_error(morey_plot.ttest(n=20,
                                type = "twosd",
                                alternative = "two.sided",
                                alpha_level = .045))
  
  expect_error(morey_plot.ttest(n=20,
                          type = "two.sample",
                          alternative = "two.sided",
                          alpha_level = 1.045))
  
  expect_error(morey_plot.ttest(es = seq(-1,1,.01),
                                n=30,
                                type = "two.sample",
                                alternative = "two.sided",
                                alpha_level = .045))

  
})

test_that("no error messages ttest", {
  test = morey_plot.ttest(n=20,
                          type = "two.sample",
                          alternative = "two.sided",
                          alpha_level = .045)
  
  test = morey_plot.ttest(n=c(20,40,50),
                          type = "two.sample",
                          alternative = "two.sided",
                          alpha_level = c(.01,.05,.1))
  
  test = morey_plot.ttest(n=c(20,40,50),
                          type = "one.sample",
                          alternative = "two.sided",
                          alpha_level = c(.01,.05,.1))
  test = morey_plot.ttest(n=c(20,40,50),
                          type = "paired",
                          alternative = "two.sided",
                          alpha_level = c(.01,.05,.1))
  test = morey_plot.ttest(n=c(20,40,50),
                          type = "one.sample",
                          alternative = "one.sided",
                          alpha_level = c(.01,.05,.1))
  test = morey_plot.ttest(n=c(20,40,50),
                          type = "two.sample",
                          alternative = "one.sided",
                          alpha_level = c(.01,.05,.1))
  expect_equal(class(test)[1], "gg")
})

# Error messages ftest

test_that("error messages ftest",{
  
  expect_error(morey_plot.ftest(
    den_df = NULL,
    alpha_level = .055,
    liberal_lambda = FALSE
  ))
  
  expect_error(morey_plot.ftest(
    num_df = NULL,
    den_df = 20,
    alpha_level = .055,
    liberal_lambda = TRUE
  ))
  
  expect_error(morey_plot.ftest(
    den_df = 20,
    alpha_level = 1.055,
    liberal_lambda = FALSE
  ))
  
  expect_error(
    morey_plot.ftest(es = seq(-1,1,.01),
                     num_df = c(1,2),
                     den_df = c(20,33),
                     alpha_level = .055,
                     liberal_lambda = FALSE)
  )
})

test_that("No error messages ftest",{
  test = morey_plot.ftest(num_df = c(1,2),
                          den_df = c(20,33),
                          alpha_level = .055,
                          liberal_lambda = FALSE)
  
  test = morey_plot.ftest(num_df = c(1,2),
                          den_df = c(20,33),
                          alpha_level = .055,
                          liberal_lambda = TRUE)
  
  expect_equal(class(test)[1], "gg")
  
})
