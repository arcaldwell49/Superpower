context("test-oneway_ancova")

# error messages
test_that("error messages", {
  
  ex = power_oneway_ancova(mu = c(400, 450, 500), 
                           n = c(21,21,21),
                           r2 = .1^2, sd = 100)
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100,
    alpha_level = 0.05,
    beta_level = .15
  ))
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21),
    r2 = .1^2, sd = 100,
    alpha_level = 0.05
  ))
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100,
    alpha_level = -0.05
  ))
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100
  ))
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100,
    alpha_level = NULL,
    beta_level = -1
  ))
  
  
  
})