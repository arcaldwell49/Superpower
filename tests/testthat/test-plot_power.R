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
               "plot_power cannot handle small sample sizes (n < the product of the factors) at this time; please increase the in ANOVA_design function.",
               fixed = TRUE)

})
