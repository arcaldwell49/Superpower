context("test-anova_power")



# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2w", n = 10, mu = c(0, 0), sd = 1, plot = FALSE)

  expect_error(ANOVA_power(), "argument \"design_result\" is missing, with no default")
  expect_error(ANOVA_power(design, nsims = -1), "The number of repetitions in simulation must be at least 10; suggested at least 1000 for accurate results")
  expect_error(ANOVA_power(design, nsims = 10, p_adjust = "BEEFERONNI"), "p_adjust must be of an acceptable adjustment method: see ?p.adjust",
               fixed = TRUE)
})

#2w
test_that("2w", {
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

  expect_equal(p$main_results, comp$main_results)
  expect_equal(p$pc_results, comp$pc_results)
  expect_equal(p$p_adjust, "none")
  expect_equal(p$nsims, 50)
})

#2w long
test_that("2w long", {
  skip_on_cran()

  design <- ANOVA_design(design = "2w", n = 100, mu = c(0, 0.25), sd = 1, r = 0.5, plot = FALSE)

  set.seed(86753)

  system.time(
    p <- ANOVA_power(design, nsims = 1000, verbose = FALSE)
  )
  pe <- ANOVA_exact(design, verbose = FALSE)

  p2 <- pwr::pwr.t.test(n = 100, d = 0.25, type = "paired")

  expect_equal(p$main_results$power/100, p2$power, tolerance = .02)
  expect_equal(pe$main_results$power/100, p2$power, tolerance = .02)
  expect_equal(p$pc_results$power/100, p2$power, tolerance = .02)
  expect_equal(pe$pc_results$power/100, p2$power, tolerance = .02)
  expect_equal(p$pc_results$effect_size, p2$d, tolerance = .02)
  expect_equal(pe$pc_results$effect_size, p2$d, tolerance = .02)
})

#2b long simulation
test_that("2b long", {
  skip_on_cran()
design <- ANOVA_design(design = "2b",
                       n = 100,
                       mu = c(24, 26.2),
                       sd = 6.4,
                       labelnames = c("condition", "control", "pet"),
                       plot = FALSE)
set.seed(644)
system.time(
  p <- ANOVA_power(design, alpha_level = 0.05, nsims = 1000, verbose = FALSE)
)

pe <- ANOVA_exact(design, verbose = FALSE)

p2 <- pwr::pwr.t.test(d = 2.2/6.4,
                      n = 100,
                      sig.level = 0.05,
                      type = "two.sample",
                      alternative = "two.sided")$power

expect_equal(p$main_results$power/100, p2, tolerance = .02)

expect_equal(pe$main_results$power/100, p2, tolerance = .02)
})

#3b long simulation
test_that("3b long", {
  skip_on_cran()
  design <- ANOVA_design(design = "3b",
                         n = 50,
                         mu = c(24, 26.2, 26.6),
                         sd = 6.4,
                         labelnames = c("condition", "control", "cat", "dog"),
                         plot = FALSE)
  set.seed(123)
  system.time(
    p <- ANOVA_power(design, alpha_level = 0.05, nsims = 5000, verbose = FALSE)
  )
  pe <- ANOVA_exact(design, alpha_level = 0.05, verbose = FALSE)
  pc_1 <- pwr::pwr.t.test(d = 2.2/6.4,
                          n = 50,
                          sig.level = 0.05,
                          type = "two.sample",
                          alternative = "two.sided")$power

  pc_2 <- pwr::pwr.t.test(d = 2.6 / 6.4,
                     n = 50,
                     sig.level = 0.05,
                     type = "two.sample",
                     alternative = "two.sided")$power

  pc_3 <- pwr::pwr.t.test(d = 0.4/6.4,
                     n = 50,
                     sig.level = 0.05,
                     type = "two.sample",
                     alternative = "two.sided")$power

  pmain <- pwr::pwr.anova.test(k = 3, n = 50, f = 0.1786086, sig.level = 0.05)$power #f obtained from GPower

  expect_equal(p$main_results$power/100, pmain, tolerance = .02)
  expect_equal(p$main_results$power/100, pe$main_results$power/100, tolerance = .02)
  expect_equal(p$pc_results$power/100, pe$pc_results$power/100, tolerance = .02)
  expect_equal(p$pc_results$power/100, c(pc_1, pc_2, pc_3), tolerance = .02)
})

#3 way between
test_that("3 way between long", {
  skip_on_cran()

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



  expect_equal(p$main_results$power/100,
               c(power_analytic$power_A, power_analytic$power_B, power_analytic$power_C,
                 power_analytic$power_AB, power_analytic$power_AC, power_analytic$power_BC,
                 power_analytic$power_ABC), tolerance = .02)
  expect_equal(p$main_results$power/100,
               pe$main_results$power/100, tolerance = .02)
  expect_equal(p$pc_results$power/100,
               pe$pc_results$power/100, tolerance = .02)

  expect_equal(rownames(p$pc_results),
               rownames(pe$pc_results))
})


#2 way mixed
test_that("2x2 mixed long", {
  skip_on_cran()

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
