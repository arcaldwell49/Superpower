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
  expect_error(p_standardized())
  expect_error(alpha_standardized())


})


test_that("optimal_alpha",{
  res <- optimal_alpha(power_function = "power.t.test(delta = .5, sd = 1, n = 100,
sig.level = x, type = 'two.sample', alternative = 'two.sided')$power",
                       plot = FALSE)
  expect_equal(res$alpha, .05101, tolerance = .001)
  expect_equal(res$beta, .05853, tolerance = .001)
  
  skip_on_cran()
  res <- optimal_alpha(power_function = "power.t.test(delta = .656, sd = 1, n = 50,
sig.level = x, type = 'two.sample', alternative = 'two.sided')$power",
                       plot = FALSE,
                       error = "balance")
  expect_equal(res$alpha, .0725, tolerance = .001)
  expect_equal(res$beta, .0725, tolerance = .001)
  
  res <- optimal_alpha(power_function = "power.ftest(num_df = 1,
                       den_df = 15,
                       cohen_f = .28,
                       alpha_level = x)$power/100",
                       plot = FALSE)
  
})

test_that("power_standardized_alpha",{
  res <- power_standardized_alpha(power_function = "power.t.test(delta = .3,
  sd = 1, n = x,  sig.level = a_stan, type = 'two.sample', 
                                  alternative = 'two.sided')$power", 
                                  power = 0.9, alpha = 0.05,
  verbose = FALSE)
  expect_equal(res$N, 265)
  
})

test_that("power_standardized_alpha",{
  res <- power_standardized_alpha(power_function = "power.t.test(delta = .3,
  sd = 1, n = x,  sig.level = a_stan, type = 'two.sample', 
                                  alternative = 'two.sided')$power", 
                                  power = 0.9, alpha = 0.05,
  verbose = FALSE)
  expect_equal(res$N, 265)
  
})

test_that("p_standardized",{
  ## Check it yields .05 for N = 100:
  res1 = p_standardized(p = 0.05, N = 100)
  expect_equal(res1, .05)
  ## Check it yields .05 for N = 200, p = 0.03535534:
  res2 = p_standardized(p = 0.03535534, N = 200)
  expect_equal(res2, 0.05)
  ## What is a standardized p-value for p = .05 and N = 200?
  res3 = p_standardized(p = 0.05, N = 200)
  expect_equal(res3, 0.07071068)
  ## You can change the standardization N, repeating the example above:
  res4 = p_standardized(p = 0.05,
                        N = 100,
                        standardize_N = 200)
  expect_equal(res4, 0.03535534)
  
})

test_that("alpha_standardized",{
  ## Check it yields .05 for N = 100:
  expect_equal(alpha_standardized(alpha = 0.05, N = 100), .05)
  ## Check it yields .05 for N = 200:
  expect_equal(alpha_standardized(alpha = 0.07071068, N = 200), .05)
  ## Which alpha should we use with N = 200?
  expect_equal(alpha_standardized(alpha = 0.05, N = 200),0.03535534)
  ## You can change the standardization N, repeating the example above:
  expect_equal(alpha_standardized(alpha = 0.05, N = 100, standardize_N = 200),0.07071068)
})

