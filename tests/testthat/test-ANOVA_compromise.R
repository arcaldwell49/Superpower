context("test-ANOVA_compromise")
hush=function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}
# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)

  expect_error(ANOVA_compromise())

})


test_that("example #1 2w",{
  design_result <- ANOVA_design(design = "2b",
  n = 100,
  mu = c(1:2),
  sd = 2,
  plot = FALSE)
  res = ANOVA_compromise(design_result)
  expect_equal(res$aov_comp$alpha,.05101,tolerance = .001)
  expect_equal(res$aov_comp$beta,.05853,tolerance = .001)
  
  design_result <- ANOVA_design(design = "2b",
                                n = 100,
                                mu = c(1:2),
                                sd = 2,
                                plot = FALSE)
  res = ANOVA_compromise(design_result,
                         liberal_lambda = TRUE)
  expect_equal(res$aov_comp$alpha,.05101,tolerance = .001)
  expect_equal(res$aov_comp$beta,.05853,tolerance = .005) # larger error margin with liberal lambda
  p = plot(res)
  pr = hush(print(res))
})

test_that("example #2 2w",{
  skip_on_cran()
  design_result <- ANOVA_design(design = "2w",
                                n = 50,
                                mu = c(1:2),
                                sd = 2,
                                plot = FALSE,
                                r = .5)
  res = ANOVA_compromise(design_result, 
                         liberal_lambda = FALSE)
  res2 <- optimal_alpha(power_function = "power.t.test(delta = 1, sd = 2, n = 50,
sig.level = x, type = 'paired', alternative = 'two.sided')$power",
                       plot = FALSE,
                       error = "balance")
  expect_equal(res$aov_comp$alpha,res2$alpha,tolerance = .01)
  expect_equal(res$aov_comp$beta,res2$beta,tolerance = .01)

  res = ANOVA_compromise(design_result,
                         liberal_lambda = TRUE)
  expect_equal(res$aov_comp$alpha,.05101,tolerance = .01)
  expect_equal(res$aov_comp$beta,.05853,tolerance = .01)
})

test_that("example #3 emmeans",{
  design_result <- ANOVA_design(design = "2w",
                                n = 50,
                                mu = c(1:2),
                                sd = 2,
                                plot = FALSE,
                                r = .5)
  res = ANOVA_compromise(design_result, 
                         liberal_lambda = FALSE,
                         emm = TRUE)
  res2 <- optimal_alpha(power_function = "power.t.test(delta = 1, sd = 2, n = 50,
sig.level = x, type = 'paired', alternative = 'two.sided')$power",
                        plot = FALSE,
                        error = "balance")
  expect_equal(res$emmeans_comp$alpha,res2$alpha,tolerance = .01)
  expect_equal(res$emmeans_comp$beta,res2$beta,tolerance = .01)
  
  res = ANOVA_compromise(design_result,
                         liberal_lambda = TRUE,
                         emm = TRUE)
  expect_equal(res$emmeans_comp$alpha,.05101,tolerance = .01)
  expect_equal(res$emmeans_comp$beta,.05853,tolerance = .01)
})