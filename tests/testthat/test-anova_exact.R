context("test-anova_exact")


# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)

  expect_error(ANOVA_exact(), "argument \"design_result\" is missing, with no default")
  expect_error(ANOVA_exact(design, verbose = FALSE),
               fixed = TRUE)
  
  expect_error(ANOVA_exact2())
  
  design <- ANOVA_design(design = "2b*4w",
                         n = 75,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  
  expect_error(ANOVA_exact2(design,
                            emm = TRUE,
                            emm_model = "NOT"))
  
  expect_error(ANOVA_exact2(design,
                            emm = TRUE,
                            contrast_type = "NOT"))
  
  expect_error(ANOVA_exact2(design,
                            emm = TRUE,
                            correction = "none1"))
  
  expect_error(ANOVA_exact2(design,
                            emm = TRUE,
                            alpha_level = 1.05))
  
  expect_error(ANOVA_exact(design,
                            emm = TRUE,
                            emm_model = "NOT"))
  
  expect_error(ANOVA_exact(design,
                            emm = TRUE,
                            contrast_type = "NOT"))
  
  expect_error(ANOVA_exact(design,
                            emm = TRUE,
                            correction = "none1"))
  
  expect_error(ANOVA_exact(design,
                            emm = TRUE,
                            alpha_level = 1.05))


})


#2w null
test_that("2w null", {
  design <- ANOVA_design(design = "2w", n = 100, 
                         mu = c(0, 0), 
                         sd = 1, 
                         r = 0.5, 
                         plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)
  p2 = ANOVA_exact2(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$pc_results$power, 5)
  expect_equal(p2$main_results, p$main_results)
  
  # confint error
  expect_error(confint(p))


})

#2b null
test_that("2b null", {
  design <- ANOVA_design(design = "2b", n = 100, mu = c(0, 0), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)
  p2 = ANOVA_exact2(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p$main_results$power, p2$main_results$power)
  expect_equal(p$pc_results$power, 5)

})


#2w moderate effect
test_that("2w", {
  design <- ANOVA_design(design = "2w", n = 21, mu = c(0, 0.65), sd = 1, r = 0.55, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)
  p2 <- ANOVA_exact2(design, verbose = FALSE)

  expect_equal(p$main_results$power, 84.7, tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.1)
  expect_equal(p$pc_results$power, 84.7, tolerance = 0.1)

})

#2b moderate effect
test_that("2b", {
  design <- ANOVA_design(design = "2b", n = 22, mu = c(0, 0.65), sd = 1, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)
  p2 <- ANOVA_exact2(design, verbose = FALSE)

  expect_equal(p$main_results$power, 55.8, tolerance = 0.05)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.05)
  expect_equal(p$pc_results$power, 55.8, tolerance = 0.1)

})

#3w null
test_that("3w null", {
  design <- ANOVA_design(design = "3w", n = 100,
                         mu = c(0, 0, 0), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_exact(design, verbose = FALSE)
  p2 <- ANOVA_exact2(design, verbose = FALSE)

  expect_equal(p$main_results$power, 5)
  expect_equal(p2$main_results$power, p$main_results$power)
  expect_equal(p$pc_results$power, c(5,5,5))

})

#4b low power
test_that("4b", {
  design <- ANOVA_design(design = "4b", n = 15,
                         mu = c(0, 0.25, 0.33, 0.44),
                         sd = 1, plot = FALSE)

  p <- ANOVA_exact(design, verbose = FALSE)
  p2 <- ANOVA_exact2(design, verbose = FALSE)
  
  expect_equal(p$main_results$power, 15, tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.05)
  expect_equal(p$main_results$partial_eta_squared, 
               p2$main_results$partial_eta_squared, tolerance = 0.01)
  expect_equal(p$pc_results$power,
               c(10.14,14.08,21.39,5.51,7.94,5.98),
               tolerance = 0.1)

})

#2x4 repeated measures
test_that("2b*4w", {
  skip_on_cran()
  design <- ANOVA_design(design = "2b*4w", n = 9,
                         mu = c(0.0, 0.0, 0.0, 0.0,
                                0, 0.5, 0.5, 0.5),
                         r = 0.71,
                         sd = 2, plot = FALSE)
  
  
  set.seed(7224)
  p <- ANOVA_exact(design, verbose = FALSE)
  p22 <- ANOVA_exact2(design, verbose = FALSE)
  set.seed(354186)
  p2 <- ANOVA_power(design, nsims = 1000, verbose = FALSE)
  
  expect_equal(p$main_results$power, c(7.1, 9.2, 9.2), tolerance = 0.1)
  
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 1.5)
  expect_equal(p$main_results$power, p22$main_results$power, tolerance = .08)
  expect_equal(p$main_results$partial_eta_squared, 
               p22$main_results$partial_eta_squared, tolerance = .01)
  expect_equal(p$manova_results$power, p22$manova_results$power, tolerance = 0.05)
  expect_equal(p$manova_results$cohen_f, p22$manova_results$cohen_f, tolerance = 0.05)
  
  
})

#3w
test_that("3w", {

  design <- ANOVA_design(design = "3w", n = 20,
                         mu = c(-0.3061862, 0.0000000, 0.3061862),
                         r = 0.8,
                         sd = 1, plot = FALSE)


  p <- ANOVA_exact(design, verbose = FALSE)
  p2 <- ANOVA_exact2(design, verbose = FALSE)

  expect_equal(p$main_results$power, 96.9, tolerance = 0.05)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.05)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
  expect_equal(p$manova_results$power, p2$manova_results$power, tolerance = 0.02)
  expect_equal(p$manova_results$cohen_f, p2$manova_results$cohen_f, tolerance = 0.02)


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
  p2 <- ANOVA_exact2(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, 81.2, tolerance = 0.01)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.01)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
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
  p2 <- ANOVA_exact2(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(89.8,42.1,42.1), tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.01)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
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
  p2 <- ANOVA_exact2(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(99.9,79.6,79.6), tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.01)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
}
)

test_that("Aberson #4",{
  design_result <- ANOVA_design(design = "4w",
                                n = 25, #25
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
  p2 <- ANOVA_exact2(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(80.9), tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.025)
  expect_equal(p$main_results$partial_eta_squared, 
               p2$main_results$partial_eta_squared, tolerance = 0.025)
  expect_equal(p$manova_results$power, p2$manova_results$power, tolerance = 0.02)
  expect_equal(p$manova_results$cohen_f, p2$manova_results$cohen_f, tolerance = 0.01)
  
  design_result <- ANOVA_design(design = "4w",
                                n = 25,
                                sd = c(.7),
                                mu = c(-.25, .00, .10, .15),
                                r = c(.50),
                                plot = FALSE)
  
  p <- ANOVA_exact(design_result,
                   verbose=FALSE)
  p2 <- ANOVA_exact2(design_result,
                     verbose=FALSE)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.05)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
  expect_equal(p$manova_results$power, p2$manova_results$power, tolerance = 0.02)
  expect_equal(p$manova_results$cohen_f, p2$manova_results$cohen_f, tolerance = 0.01)
  
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
  p2 <- ANOVA_exact2(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(39.7), tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.1)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.1)
  expect_equal(p$manova_results$power, p2$manova_results$power, tolerance = 0.01)
  expect_equal(p$manova_results$cohen_f, p2$manova_results$cohen_f, tolerance = 0.01)
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
  p2 <- ANOVA_exact2(design_result,
                   verbose=FALSE)
  
  expect_equal(p$main_results$power, c(27.2,74.8,10.2), tolerance = 0.1)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.01)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
  expect_equal(p$manova_results$power, p2$manova_results$power, tolerance = 0.01)
  expect_equal(p$manova_results$cohen_f, p2$manova_results$cohen_f, tolerance = 0.01)
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
                   verbose = FALSE)
  p2 <- ANOVA_exact2(design_result,
                   verbose = FALSE)
  
  expect_equal(p$main_results$power, c(86.4,82.7,82.7), tolerance = 0.01)
  expect_equal(p$main_results$power, p2$main_results$power, tolerance = 0.01)
  expect_equal(p$main_results$partial_eta_squared, p2$main_results$partial_eta_squared, tolerance = 0.01)
  expect_equal(p$manova_results$power, p2$manova_results$power, tolerance = 0.01)
  expect_equal(p$manova_results$cohen_f, p2$manova_results$cohen_f, tolerance = 0.01)
}
)

test_that("Aberson Table 5.5",{
  design_result <- Superpower::ANOVA_design("4b", sd = 10, n = 60, plot = FALSE,
                                            mu = c(80,82,82,86))
  
  power_result <- Superpower::ANOVA_exact(design_result, verbose = FALSE,
                                          emm = TRUE, contrast_type = "poly")
  power_result2 <- Superpower::ANOVA_exact2(design_result, verbose = FALSE,
                                          emm = TRUE, contrast_type = "poly")
  
  expect_equal(round(power_result$emm_results$power, 1),c(87.4,12,17.9))
  expect_equal(round(power_result$emm_results$power, 1),
               round(power_result2$emm_results$power, 1))
  expect_equal(round(power_result$emm_results$partial_eta_squared, 1),
               round(power_result2$emm_results$partial_eta_squared, 1))
  
  design_result <- Superpower::ANOVA_design("4b", sd = 10, n = 30, plot = FALSE,
                                            mu = c(80,82,82,86))
  
  power_result <- Superpower::ANOVA_exact(design_result, verbose = FALSE,
                                          emm = TRUE, contrast_type = "poly")
  power_result2 <- Superpower::ANOVA_exact2(design_result, verbose = FALSE,
                                            emm = TRUE, contrast_type = "poly")
  
  expect_equal(round(power_result$main_results$power, 1),
               round(power_result2$main_results$power, 1), tolerance = 0.05)
  expect_equal(round(power_result$main_results$partial_eta_squared, 1),
               round(power_result2$main_results$partial_eta_squared, 1), tolerance = 0.01)
  expect_equal(round(power_result$emm_results$power, 1),
               round(power_result2$emm_results$power, 1), tolerance = 0.05)
  expect_equal(round(power_result$emm_results$partial_eta_squared, 1),
               round(power_result2$emm_results$partial_eta_squared, 1), tolerance = 0.01)
})

test_that("Aberson Table 5.6",{
  design_result <- Superpower::ANOVA_design("4b", sd = 10, n = 60, plot = FALSE,
                                            mu = c(80,82,82,86))
  
  power_result1 <- Superpower::ANOVA_exact(design_result, verbose = FALSE,
                                          emm = TRUE, alpha_level = .0085)
  power_result2 <- Superpower::ANOVA_exact2(design_result, verbose = FALSE,
                                          emm = TRUE, alpha_level = .0085)
  
  expect_equal(round(power_result1$emm_results$power, 1),
               c(6.1,6.1,73.6,.8,32.4,32.4))
  
  expect_equal(power_result1$main_results$power, 
               power_result2$main_results$power,
               tolerance = .01)
  expect_equal(power_result1$main_results$partial_eta_squared, 
               power_result2$main_results$partial_eta_squared,
               tolerance = .01)
  
  expect_equal(power_result1$emm_results$power, 
               power_result2$emm_results$power,
               tolerance = .01)
  
  expect_equal(power_result1$emm_results$partial_eta_squared, 
               power_result2$emm_results$partial_eta_squared,
               tolerance = .01)
})

test_that("Aberson Table 5.9",{
design_result <- Superpower::ANOVA_design("2b*2b", sd = 1.7, n = 250, plot = FALSE,
                                          mu = c(.85,.85,0,.6))

power_result1 <- Superpower::ANOVA_exact(design_result, verbose = FALSE,
                                         emm = TRUE, emm_comp = "b|a")
power_result1a <- Superpower::ANOVA_exact(design_result, verbose = FALSE,
                                         emm = TRUE, emm_comp = "a|b")

power_result2 <- Superpower::ANOVA_exact2(design_result, verbose = FALSE,
                                         emm = TRUE, emm_comp = "b|a")

power_simple <- round(c(power_result1$emm_results$power,
                  power_result1a$emm_results$power),1)

expect_equal(power_simple, c(5,97.6,100,37.6))

expect_equal(power_result1$main_results$power, 
             power_result2$main_results$power)
expect_equal(power_result1$main_results$partial_eta_squared, 
             power_result2$main_results$partial_eta_squared)

expect_equal(power_result1$emm_results$power, 
             power_result2$emm_results$power)

expect_equal(power_result1$emm_results$partial_eta_squared, 
             power_result2$emm_results$partial_eta_squared)

})

test_that("check lambda and verbose",{
  hush=function(code){
    sink("NUL") # use /dev/null in UNIX
    tmp = code
    sink()
    return(tmp)
  }
  des <- ANOVA_design(
    "2b*4w",
    n = 50,
    sd = c(2),
    r = c(.45),
    mu = c(-.25, 0.0, 0.10, 0.15,-.25, -.25, -.25, -.25),
    plot = FALSE
  )
  
  res = hush(ANOVA_exact(design_result = des,
                    emm = TRUE,
                    verbose = TRUE,
                    liberal_lambda = TRUE))
  
  res = hush(ANOVA_exact2(design_result = des,
                    emm = TRUE,
                    verbose = TRUE,
                    liberal_lambda = TRUE))
  
  des <- ANOVA_design(
    "2b*4w*3b",
    n = 50,
    sd = c(2),
    r = c(.45),
    mu = c(1:24),
    plot = FALSE
  )
  
  res = hush(ANOVA_exact(design_result = des,
                    emm = TRUE,
                    verbose = TRUE,
                    liberal_lambda = TRUE))
  test = hush(print(res))
  res = hush(ANOVA_exact2(design_result = des,
                     emm = TRUE,
                     verbose = TRUE,
                     liberal_lambda = TRUE))
  test = hush(print(res))
  
})