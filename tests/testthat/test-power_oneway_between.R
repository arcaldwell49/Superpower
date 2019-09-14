context("test-power_oneway_between")

# error messages
test_that("error messages", {

  design <- "2b"
  n <- 100
  mu <- c(24, 26.2)
  sd <- 6.4

  design_result <- ANOVA_design(design = design,
                                n = n,
                                mu = mu,
                                sd = sd,
                                plot = FALSE)


  expect_error(power_oneway_between(), "argument \"design_result\" is missing, with no default" )

})

#Function check
test_that("2b and 3b", {

  design_result1 <- ANOVA_design(design = "2b",
                                n = 100,
                                mu = c(24,26.2),
                                sd = 6.4, plot = FALSE)

  expect_equal(power_oneway_between(design_result1, alpha_level = 0.05)$power,
               pwr::pwr.t.test(d = 2.2/6.4,
                          n = 100,
                          sig.level = 0.05,
                          type="two.sample",
                          alternative="two.sided")$power*100,
               tolerance = .001) #example from validation files

  design_result2 <- ANOVA_design(design = "3b",
                                 n = 50,
                                 mu = c(24, 26.2, 26.6),
                                 sd = 6.4, plot = FALSE)
  expect_equal(power_oneway_between(design_result2, alpha_level = 0.05)$power,
               pwr::pwr.anova.test(n = 50,
                              k = 3,
                              f = 0.1786086, #From Gpower
                              sig.level = .05)$power*100,
               tolerance = .001) #example from validation files
})
