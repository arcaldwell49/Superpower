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

####Match Test results with appendices from SuperpowerBook


test_that("Aberson #1",{
  design_result <- ANOVA_design(design = "4b",
                                n = 60,
                                sd = 10,
                                mu = c(80, 82, 82, 86),
                                labelnames = c("DORM",
                                               "Control",
                                               "T1",
                                               "T2",
                                               "T3"),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, 81.2, tolerance = 0.1)
}
          )

test_that("Aberson #2",{
  design_result <- ANOVA_design(design = "2b*2b",
                                n = 100,
                                sd = 1.7,
                                mu = c(.85, .85, 
                                       0, .6),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(89.8,42.1,42.1), tolerance = 0.1)
}
)

test_that("Aberson #3",{
  design_result <- ANOVA_design(design = "2b*2b",
                                n = 250,
                                sd = 1.7,
                                mu = c(.85, .85, 
                                       0, .6),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(99.9,79.6,79.6), tolerance = 0.1)
}
)

test_that("Aberson #4",{
  design_result <- ANOVA_design(design = "4w",
                                n = 25,
                                sd = c(.4,.5,.6,.7),
                                mu = c(-.25, .00, .10, .15),
                                r = c(.50, 
                                      .30,
                                      .15, 
                                      .5,
                                      .30, 
                                      .50),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(80.9), tolerance = 0.1)
}
)

test_that("Aberson #5",{
  design_result <- ANOVA_design(design = "4w",
                                n = 100,
                                sd = c(.4,.5,2.5,2),
                                mu = c(-.25, .00, .10, .15),
                                r = c(.50, 
                                      .30,
                                      .1, 
                                      .5,
                                      .30, 
                                      .40),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(39.7), tolerance = 0.1)
}
)

test_that("Aberson #6",{
  design_result <- ANOVA_design(design = "2w*4w",
                                n = 80,
                                sd = c(.4,0.5, 
                                       2.5,2.0, 
                                       0.4,0.5, 
                                       2.5,2.0),
                                mu = c(-0.25,0.0, 
                                       0.10,0.15, 
                                       -0.25,0.10, 
                                       0.30,0.35),
                                r = c(.5),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(27.2,74.8,10.2), tolerance = 0.1)
}
)

test_that("Aberson #7",{
  design_result <- ANOVA_design("2b*4w",
                                n = 50,
                                sd = c(.4, .5, 0.6, .7,
                                       .4, .5, .6, .7),
                                r = c(1.0,0.5,0.3,0.15,0.0,0.0,0.0,0.0,
                                      0.5,1.0,0.5,0.3,0.0,0.0,0.0,0.0,
                                      0.3,0.5,1.0,0.5,0.0,0.0,0.0,0.0,
                                      0.15,0.3,0.5,1.0,0.0,0.0,0.0,0.0,
                                      0.0,0.0,0.0,0.0,1.0,0.5,0.3,0.15,
                                      0.0,0.0,0.0,0.0,0.5,1.0,0.5,0.3,
                                      0.0,0.0,0.0,0.0,0.3,0.5,1.0,0.5,
                                      0.0,0.0,0.0,0.0,0.15,0.3,0.5,1.0),
                                mu = c(-.25, 0.0, 0.10, 0.15,
                                       -.25,-.25,-.25,-.25),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(86.4,82.7,82.7), tolerance = 0.1)
}
)

