context("test-plot_power_2x2_within")



# error messages
test_that("error messages", {

  design_result1 <- ANOVA_design(design = "2w*2w",
                                 n = 100,
                                 mu = c(24,26,25,27),
                                 sd = 6.4,
                                 plot = FALSE)


  expect_error(plot_power_2x2_within(), "argument \"design_result\" is missing, with no default" )
  expect_error(plot_power_2x2_within(design_result1), "argument \"max_n\" is missing, with no default" )

})
