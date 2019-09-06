context("Simulated Data")
library(Superpower)
library(reshape2)

test_that("simulated correlations fit expected values", {
  design_result <- ANOVA_design(design = "2b*2w",
                                n = 1000000,
                                mu = c(0, 0, 0, 0),
                                sd = 2,
                                r = 0.8,
                                labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"),
                                plot = FALSE)

  design_result$cor_mat

  data_wide <- dcast(design_result$dataframe, subject ~ cond, value.var="y")
  a1 <- data_wide[,2][!is.na(data_wide[,2])]
  a2 <- data_wide[,3][!is.na(data_wide[,3])]
  b1 <- data_wide[,4][!is.na(data_wide[,4])]
  b2 <- data_wide[,5][!is.na(data_wide[,5])]

  res1 <- cor(a1, a2)
  res2 <- cor(a1, b1)
  res3 <- cor(a1, b2)
  res4 <- cor(a2, b1)
  res5 <- cor(a2, b2)
  res6 <- cor(b1, b2)

  res1i <- cor(a1, a1)
  res2i <- cor(a2, a2)
  res3i <- cor(b1, b1)
  res4i <- cor(b2, b2)

  expect_equal(res1, 0.8, tolerance = .003)
  expect_equal(res2, 0, tolerance = .003)
  expect_equal(res3, 0, tolerance = .003)
  expect_equal(res4, 0, tolerance = .003)
  expect_equal(res5, 0, tolerance = .003)
  expect_equal(res6, 0.8, tolerance = .003)
  expect_equal(res1i, 1, tolerance = .003)
  expect_equal(res2i, 1, tolerance = .003)
  expect_equal(res3i, 1, tolerance = .003)
  expect_equal(res4i, 1, tolerance = .003)
})

