context("test-justifieR")
#library(pwr2ppl)


# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)

  expect_error(optimal_alpha())
  expect_error(power_standardized_alpha())


})


test_that("optimal_alpha",{
  res <- optimal_alpha(power_function = "power.t.test(delta = .5, sd = 1, n = 100,
sig.level = x, type = 'two.sample', alternative = 'two.sided')$power",
                       plot = FALSE)
  expect_equal(res$alpha, .05101, tolerance = .001)
  expect_equal(res$beta, .05853, tolerance = .001)
  
  res <- optimal_alpha(power_function = "power.t.test(delta = .656, sd = 1, n = 50,
sig.level = x, type = 'two.sample', alternative = 'two.sided')$power",
                       plot = FALSE,
                       error = "balance")
  expect_equal(res$alpha, .0725, tolerance = .001)
  expect_equal(res$beta, .0725, tolerance = .001)
  
})

test_that("power_standardized_alpha",{
  res <- power_standardized_alpha(power_function = "power.t.test(delta = .3,
  sd = 1, n = x,  sig.level = a_stan, type = 'two.sample', 
                                  alternative = 'two.sided')$power", 
                                  power = 0.9, alpha = 0.05)
  expect_equal(res$N, 265)
  
})


