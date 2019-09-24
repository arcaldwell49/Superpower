context("test-anova_exact")
#library(pwr2ppl)


# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)

  expect_error(ANOVA_exact(), "argument \"design_result\" is missing, with no default")
  expect_error(ANOVA_exact(design, verbose = FALSE),
               "ANOVA_exact cannot handle small sample sizes (n < the product of the factors) at this time; please pass this design_result to the ANOVA_power function to simulate power",
               fixed = TRUE)

})


#2w null
test_that("2w null", {
  design <- ANOVA_design(design = "2w", n = 100, mu = c(0, 0), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, 5)

})

#2b null
test_that("2b null", {
  design <- ANOVA_design(design = "2b", n = 100, mu = c(0, 0), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, 5)

})


#2w moderate effect
test_that("2w", {
  design <- ANOVA_design(design = "2w", n = 21, mu = c(0, 0.65), sd = 1, r = 0.55, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 84.7, tolerance = 0.1)
  expect_equal(p$pc_results$power, 84.7, tolerance = 0.1)

})

#2b moderate effect
test_that("2b", {
  design <- ANOVA_design(design = "2b", n = 22, mu = c(0, 0.65), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 55.8, tolerance = 0.1)
  expect_equal(p$pc_results$power, 55.8, tolerance = 0.1)

})

#3w null
test_that("3w null", {
  design <- ANOVA_design(design = "3w", n = 100,
                         mu = c(0, 0, 0), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, c(5,5,5))

})

#4b low power
test_that("4b", {
  design <- ANOVA_design(design = "4b", n = 15,
                         mu = c(0, 0.25, 0.33, 0.44),
                         sd = 1, plot = FALSE)

  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 15, tolerance = 0.1)
  expect_equal(p$pc_results$power,
               c(10.14,14.08,21.39,5.51,7.94,5.98),
               tolerance = 0.1)

})

#2x4 repeated measures
#test_that("2b*4w", {
#  design <- ANOVA_design(design = "2b*4w", n = 9,
#                         mu = c(0.0, 0.0, 0.0, 0.0,
#                                0.5, 0.5, 0.5, 0.5),
#                         r = 0.71,
#                         sd = 2, plot = FALSE)
#
#
#  set.seed(7224)
#  p <- ANOVA_exact(design, verbose = FALSE)
#  set.seed(354186)
#  p2 <- ANOVA_power(design, nsims = 10000)
#
#  expect_equal(p$main_results$power, c(8.7, 5, 5), tolerance = 0.1)
#  
#  expect_equal(p$main_results$power, p2$main_results$power, tolerance = .5)
#
#
#})

#2x4 repeated measures
test_that("2b*4w", {
  design <- ANOVA_design(design = "2b*4w", n = 9,
                         mu = c(0.0, 0.0, 0.0, 0.0,
                                0, 0.5, 0.5, 0.5),
                         r = 0.71,
                         sd = 2, plot = FALSE)
  
  
  set.seed(7224)
  p <- ANOVA_exact(design, verbose = FALSE)
  set.seed(354186)
  p2 <- ANOVA_power(design, nsims = 1000, verbose = FALSE)
  
  expect_equal(p$main_results$power, c(7.1, 9.2, 9.2), tolerance = 0.1)
  
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 1.5)
  
  
})

#3w
test_that("3w", {

  design <- ANOVA_design(design = "3w", n = 20,
                         mu = c(-0.3061862, 0.0000000, 0.3061862),
                         r = 0.8,
                         sd = 1, plot = FALSE)


  p <- ANOVA_exact(design, verbose = FALSE)

  expect_equal(p$main_results$power, 96.9, tolerance = 0.1)


})

####Match Test results with Aberson pwr2ppl

#2x2
#test_that("Aberson 2x2", {
  
#  design <- ANOVA_design(design = "2b*2b", n = 35,
#                         mu = c(5.5,7.2,
#                                6.1,3.4),
#                         sd = 3.7, plot = FALSE)
#  
#  
#  p <- ANOVA_exact(design, verbose = FALSE)
#  
#  suppressMessages(
#    aberson <- anova2x2(
#      m1.1 = 5.5,
#      m1.2 = 7.2,
#      m2.1 = 6.1,
#      m2.2 = 3.4,
#      s1.1 = 3.7,
#      s1.2 = 3.7,
#      s2.1 = 3.7,
#      s2.2 = 3.7,
#      n1.1 = 35,
#      n1.2 = 35,
#      n2.1 = 35,
#      n2.2 = 35,
#      alpha = .05
#    )
#  )
#  
#  expect_equal(p$main_results$power[1], aberson$`Power A`*100, tolerance = 0.1)
#  expect_equal(p$main_results$power[2], aberson$`Power B`*100, tolerance = 0.1)
#  expect_equal(p$main_results$power[3], (aberson$`Power AxB`) * 100, tolerance = 0.1)
#  
#})


#test_that("Aberson 2x2 #2", {
  
#  design <- ANOVA_design(design = "2b*2b", n = 55,
#                         mu = c(1.1,1.1,
#                                2.5,2.3),
#                         sd = .95, plot = FALSE)
#  
#  
#  p <- ANOVA_exact(design, verbose = FALSE)
#  
#  suppressMessages(
#    aberson <- anova2x2(
#      m1.1 = 1.1,
#      m1.2 = 1.1,
#      m2.1 = 2.5,
#      m2.2 = 2.3,
#      s1.1 = .95,
#      s1.2 = .95,
#      s2.1 = .95,
#      s2.2 = .95,
#      n1.1 = 55,
#      n1.2 = 55,
#      n2.1 = 55,
#      n2.2 = 55,
#      alpha = .05
#    )
#  )
#  
#  expect_equal(p$main_results$power[1], aberson$`Power A`*100, tolerance = 0.1)
#  expect_equal(p$main_results$power[2], aberson$`Power B`*100, tolerance = 0.1)
#  expect_equal(p$main_results$power[3], (aberson$`Power AxB`) * 100, tolerance = 0.1)
#  
#  
#})
