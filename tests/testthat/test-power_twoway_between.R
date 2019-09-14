context("test-power_twoway_between")

# error messages
test_that("error messages", {

  design <- "2b*2w"
  n <- 100
  mu <- c(24, 26.2, 27, 28)
  sd <- 6.4

  design_result <- ANOVA_design(design = design,
                                n = n,
                                mu = mu,
                                sd = sd,
                                plot = FALSE)


  expect_error(power_twoway_between(), "argument \"design_result\" is missing, with no default" )
  expect_error(power_twoway_between(design_result), "Only two-way between designs allowed for this function")

})

#Function check
test_that("2x2 design", {

  #From Data Colada validation file https://github.com/Lakens/ANOVA_power_simulation/blob/master/validation_files/4.2_power_for_interactions.pdf
  design <- "2b*2b"
  n <- 150
  mu <- c(20, 20, 20, 25) #All means are equal - so there is no real difference.
  sd <- 20
  labelnames <- c("fruit", "apple", "banana", "hunger", "no hunger", "very hungry") #
  # the label names should be in the order of the means specified above.
  design_result <- ANOVA_design(design = design,
                                n = n,
                                mu = mu,
                                sd = sd,
                                labelnames = labelnames,
                                plot = FALSE)



  power <- power_twoway_between(design_result)
  expect_equal(c(power$power_A, power$power_B, power$power_AB), c(33,33,33), tolerance = .5)

})
