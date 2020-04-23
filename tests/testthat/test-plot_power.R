context("test-plot_power")



# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)

  expect_error(plot_power(), "argument \"design_result\" is missing, with no default")
  expect_error(plot_power(design),
               "plot_power must have an ANOVA_design object with n > the product of the factors; please increase the n in ANOVA_design function.",
               fixed = TRUE)

})


test_that("test 2b", {
  design <- ANOVA_design(design = "2b",
                         n = 7,
                         mu = c(0,.5),
                         sd = 1,
                         plot = FALSE)
  
  p = plot_power(design, min_n = 7, max_n = 100)
  expect_equal(p$power_df[1,2], 15,tolerance = .1)
  expect_equal(p$power_df[94,2],96,tolerance = .01)
  
})
