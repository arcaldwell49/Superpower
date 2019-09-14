context("test-power_threeway_between")

# error messages
test_that("error messages", {


  expect_error(power_twoway_between(), "argument \"design_result\" is missing, with no default" )

  design_result1 <- ANOVA_design(design = "2b*2b*2w",
                                n = 100,
                                mu = c(24, 26.2, 27, 28,
                                       24, 26.2, 27, 28),
                                sd = 6.4,
                                plot = FALSE)



  expect_error(power_threeway_between(design_result1), "Only three-way between designs allowed for this function")

  design_result2 <- ANOVA_design(design = "2b*2b",
                                n = 100,
                                mu = c(24, 26.2, 27, 28),
                                sd = 6.4,
                                plot = FALSE)

  expect_error(power_threeway_between(design_result2), "Only three-way between designs allowed for this function")

})

#Function check
test_that("3-way design", {

  #From ANOVA_power_simulation validation file https://github.com/Lakens/ANOVA_power_simulation/blob/master/validation_files/4.6_threeway_interactions.md

  design_result <- ANOVA_design(design = "2b*2b*2b",
                                n = 50,
                                mu = c(2, 2, 6, 1, 6, 6, 1, 8),
                                sd = 10,
                                plot = FALSE)


  power <- power_threeway_between(design_result)

  expect_equal(power$power_ABC, 84.9, tolerance = .01)

})
