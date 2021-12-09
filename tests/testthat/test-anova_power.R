context("test-anova_power")

hush=function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}

# error messages
test_that("error messages", {
  Superpower_options(verbose = FALSE)
  design <- ANOVA_design(design = "2w", n = 10, mu = c(0, 0), sd = 1, plot = FALSE)

  expect_error(ANOVA_power(), "argument \"design_result\" is missing, with no default")
  expect_error(ANOVA_power(design, nsims = -1), "The number of repetitions in simulation must be at least 10; suggested at least 1000 for accurate results")
  expect_error(ANOVA_power(design, nsims = 10, p_adjust = "BEEFERONNI"), "p_adjust must be of an acceptable adjustment method: see ?p.adjust",
               fixed = TRUE)
  # adding coverage 
  design <- ANOVA_design(design = "2w", n = 10, mu = c(0, 0), sd = 1, plot = FALSE)
  res = ANOVA_power(design, nsims = 10,
                    emm = NULL)
  expect_error(ANOVA_power(design, nsims = 10,
                    emm = TRUE, contrast_type = "knope"))
  expect_error(ANOVA_power(design, nsims = 10,
                           emm = TRUE, emm_p_adjust = "knope"))
  expect_error(ANOVA_power(design, nsims = 10,
                           emm = TRUE, emm_model = "mvt"))
  expect_error(ANOVA_power(design, nsims = 10,
                           emm = TRUE, correction = "another"))
  expect_error(ANOVA_power(design, nsims = 10,
                           emm = TRUE, alpha_level = 1.1))
  expect_warning(ANOVA_power(design, nsims = 10,
                           emm = TRUE, emm_p_adjust = "tukey"))
  
  design2 <- ANOVA_design(design = "2w", n = c(10,15), mu = c(0, 0), sd = 1, plot = FALSE)
  expect_error(ANOVA_power(design2, nsims = 10))
  
  design3 <- ANOVA_design(design = "2b", n = c(10,20), mu = c(0, 0), sd = 1, plot = FALSE)
  res = ANOVA_power(design3, nsims = 10,
                    emm = NULL)
  res = hush(ANOVA_power(design3, nsims = 10,
                    emm = NULL, verbose = TRUE))
  test = hush(print(res))
  
  
  # below run so coverage high when run on travis
  design <- ANOVA_design(design = "2w", n = 100, mu = c(0, 0.25), sd = 1, r = 0.5, plot = FALSE)
  p <- ANOVA_power(design, nsims = 10, verbose = FALSE)
  # Should not produce error
  c = confint(p)
  c = confint(p, parm = "manova_results")
  c = confint(p, parm = "pc_results")
  # Should produce error
  expect_error(confint(p, parm = "ancova"))
  expect_error(confint(p, parm = "emm_results"))
  
  design <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8,
                         labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"),
                         plot = FALSE)
  p <- ANOVA_power(design, nsims = 10, verbose = FALSE)
  
  design <- ANOVA_design(design = "2b*2b*2b",
                         n = 80,
                         mu = c(2, 2, 6, 1, 6, 6, 1, 8),
                         sd = 10,
                         plot = FALSE)

  p <- ANOVA_power(design, alpha_level = 0.05, nsims = 10, verbose = FALSE) 
  mu <- c(-0.25, 0.25, 0.25, -0.25)
  n <- 23
  sd <- 1
  r <- 0.5
  design = "2w*2b"
  alpha_level <- 0.05
  p_adjust = "none"
  labelnames = c("age", "old", "young", "color", "blue", "red")
  design <- ANOVA_design(design = design,
                         n = n,
                         mu = mu,
                         sd = sd,
                         r = r,
                         labelnames = labelnames,
                         plot = FALSE)
  
  p <- ANOVA_power(design, alpha_level = 0.05, nsims = 10, verbose = FALSE)
  
  
})

#2w
test_that("2w", {
  skip_on_cran()
  design <- ANOVA_design(design = "2w", n = 100, mu = c(0, 0.25), sd = 1, r = 0.5, plot = FALSE)
  set.seed(86753)
  p <- ANOVA_power(design, nsims = 50, verbose = FALSE)

  comp <- list()
  comp$main_results <- data.frame(
    power = c(70),
    effect_size = c(0.06913817),
    row.names = c("anova_a")
  )

  comp$pc_results <- data.frame(
    power = c(70),
    effect_size = c(0.2545745),
    row.names = c("p_a_a1_a_a2")
  )

    expect_equal(p$main_results, comp$main_results, tolerance = .02)
    expect_equal(p$pc_results, comp$pc_results, tolerance = .02)
    expect_equal(p$p_adjust, "none")
    expect_equal(p$nsims, 50)
})

# 2w*2w
test_that("2w*2w", {
  skip_on_cran()
  #skip_on_travis()
  skip_on_ci() 
  design <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 1, 0), sd = 2, r = 0.8,
                         labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"),
                         plot = FALSE)

  set.seed(8675309)

  p <- ANOVA_power(design, nsims = 50, verbose = FALSE)

  comp <- list()
  comp$main_results <- data.frame(
    power = c(6, 100, 6),
    effect_size = c(0.02226488, 0.54204284, 0.02393630),
    row.names = c("anova_condition", "anova_voice", "anova_condition:voice")
  )

  comp$pc_results <- data.frame(
    power = c(100, 4, 100, 100, 4, 100),
    effect_size = c(-0.766498975, 0.009485897, -0.750025879, 0.803554808, 0.024381057, -0.794985335),
    row.names = c(
      "p_condition_cheerful_voice_human_condition_cheerful_voice_robot",
      "p_condition_cheerful_voice_human_condition_sad_voice_human",
      "p_condition_cheerful_voice_human_condition_sad_voice_robot",
      "p_condition_cheerful_voice_robot_condition_sad_voice_human",
      "p_condition_cheerful_voice_robot_condition_sad_voice_robot",
      "p_condition_sad_voice_human_condition_sad_voice_robot"
    )
  )
  
  expect_equal(p$main_results$power, comp$main_results$power, tolerance = .05)
  expect_equal(p$pc_results$power, comp$pc_results$power, tolerance = .05)
  expect_equal(p$main_results$effect_size, comp$main_results$effect_size, tolerance = .05)
  expect_equal(p$pc_results$effect_size, comp$pc_results$effect_size, tolerance = .05)
  expect_equal(p$p_adjust, "none")
  expect_equal(p$nsims, 50)
})

#2b long simulation
test_that("2b long", {
  skip_on_cran()
  skip_on_ci()
design <- ANOVA_design(design = "2b",
                       n = 100,
                       mu = c(24, 26.2),
                       sd = 6.4,
                       labelnames = c("condition", "control", "pet"),
                       plot = FALSE)
set.seed(644)

p <- ANOVA_power(design,
                 alpha_level = 0.05,
                 nsims = 1000,
                 emm=TRUE,
                 verbose = FALSE)
c = confint(p, parm = "emm_results")
expect_error(confint(p,parm = "manova_results"))

pe <- ANOVA_exact(design, verbose = FALSE)

p2 <- pwr::pwr.t.test(d = 2.2/6.4,
                      n = 100,
                      sig.level = 0.05,
                      type = "two.sample",
                      alternative = "two.sided")$power

expect_equal(p$main_results$power/100, p2, tolerance = .02)

expect_equal(pe$main_results$power/100, p2, tolerance = .02)
})

#3 way between
test_that("3 way between long", {
  skip_on_cran()
  skip_on_ci()

  labelnames <- c("Size", "b", "s", "Color", "g", "r",
                  "Load", "pres", "abs") #

  design <- ANOVA_design(design = "2b*2b*2b",
                         n = 80,
                         mu = c(2, 2, 6, 1, 6, 6, 1, 8),
                         sd = 10,
                         labelnames = labelnames,
                         plot = FALSE)

  set.seed(8224)

  p <- ANOVA_power(design, alpha_level = 0.05, nsims = 4000, verbose = FALSE)

  pe <- ANOVA_exact(design, alpha_level = 0.05, verbose = FALSE)

  power_analytic <- power_threeway_between(design)



  expect_equal(p$main_results$power,
               c(power_analytic$power_A, power_analytic$power_B, power_analytic$power_C,
                 power_analytic$power_AB, power_analytic$power_AC, power_analytic$power_BC,
                 power_analytic$power_ABC), tolerance = .2)
  expect_equal(p$main_results$power,
               pe$main_results$power, tolerance = .2)
  expect_equal(p$pc_results$power,
               pe$pc_results$power, tolerance = .2)

  expect_equal(rownames(p$pc_results),
               rownames(pe$pc_results))
})


#2 way mixed
test_that("2x2 mixed long", {
  skip_on_cran()
  skip_on_ci()

  mu <- c(-0.25, 0.25, 0.25, -0.25)
  n <- 23
  sd <- 1
  r <- 0.5
  design = "2w*2b"
  alpha_level <- 0.05
  p_adjust = "none"
  labelnames = c("age", "old", "young", "color", "blue", "red")
  design <- ANOVA_design(design = design,
                         n = n,
                         mu = mu,
                         sd = sd,
                         r = r,
                         labelnames = labelnames,
                         plot = FALSE)

  set.seed(435)

  p <- ANOVA_power(design, alpha_level = 0.05, nsims = 1000, verbose = FALSE)
  pe <- ANOVA_exact(design, verbose = FALSE)



  p_inter <- 0.9124984 #power obtained from GPower https://github.com/Lakens/ANOVA_power_simulation/blob/master/validation_files/3.1_validation_power_between_within_2x2.md

  expect_equal(p$main_results$power[3]/100, p_inter, tolerance = .02)
  expect_equal(pe$main_results$power[3]/100, p$main_results$power[3]/100, tolerance = .02)
})



