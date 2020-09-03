context("test-emmeans_power")

# error messages
test_that("error messages", {

  
  expect_error(emmeans_power(), "argument \"x\" is missing, with no default")

  
})




#2b null
test_that("2b null", {
  design <- ANOVA_design(design = "2b", n = 100, mu = c(0, 0), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, emm = TRUE, verbose = FALSE)
  
  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, 5)
  expect_equal(p$emm_results$power, 5)
  
})




#2b moderate effect
test_that("2b", {
  design <- ANOVA_design(design = "2b", n = 22, mu = c(0, 0.65), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, emm = TRUE, verbose = FALSE)

  expect_equal(p$main_results$power, 55.8, tolerance = 0.1)
  expect_equal(p$pc_results$power, 55.8, tolerance = 0.1)
  expect_equal(p$emm_results$power, 55.8, tolerance = 0.1)
  
})



#4b low power
test_that("4b", {
  skip_on_cran()
  design <- ANOVA_design(design = "4b", n = 15,
                         mu = c(0, 0.25, 0.33, 0.44),
                         sd = 1, plot = FALSE)
  p_monte <- ANOVA_power(design, emm = TRUE, verbose = FALSE, 
                         nsims = 1000, seed = 22042020,
                         emm_p_adjust = "bonferroni")
  p <- ANOVA_exact(design, emm = TRUE, verbose = FALSE)
  p_bonf <- ANOVA_exact(design, emm = TRUE, verbose = FALSE,
                        alpha_level = .05/6)
  
  expect_equal(p$main_results$power, 15, tolerance = 0.1)
  expect_equal(p$pc_results$power,
               c(10.14,14.08,21.39,5.51,7.94,5.98),
               tolerance = 0.1)
  expect_equal(p$pc_results$power,
               p_monte$pc_results$power,
               tolerance = 0.1)
  expect_equal(p_monte$emm_results$power,
               p_bonf$emm_results$power,
               tolerance = 0.1)

  
})


