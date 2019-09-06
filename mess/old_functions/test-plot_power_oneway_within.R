context("test-plot_power_oneway_within")



# error messages
test_that("error messages", {

  design_result1 <- ANOVA_design(design = "2w",
                                 n = 100,
                                 mu = c(24,26.2),
                                 sd = 6.4,
                                 r= 0.5,
                                 plot = FALSE)

  expect_error(plot_power_oneway_within(), "argument \"design_result\" is missing, with no default" )
  expect_error(plot_power_oneway_within(design_result1), "argument \"max_n\" is missing, with no default" )

})
