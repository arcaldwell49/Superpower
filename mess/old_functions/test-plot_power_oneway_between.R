context("test-plot_power_oneway_between")



# error messages
test_that("error messages", {

  design_result1 <- ANOVA_design(design = "2b",
                                 n = 100,
                                 mu = c(24,26.2),
                                 sd = 6.4,
                                 plot = FALSE)


  expect_error(plot_power_oneway_between(), "argument \"design_result\" is missing, with no default" )
  expect_error(plot_power_oneway_between(design_result1), "argument \"max_n\" is missing, with no default" )

})
