context("test-anova_design")

# error messages ----
test_that("errors", {
  # missing arguments
  expect_error(ANOVA_design(),
               "argument \"design\" is missing, with no default")
  expect_error(ANOVA_design("2w*2b", n =20, sd = 1),
               "argument \"mu\" is missing, with no default")
  expect_error(ANOVA_design("2w*2b", mu = c(0, 0, 0, 0)),
               "argument \"sd\" is missing, with no default")
  expect_error(ANOVA_design("2w*2b", mu = c(0, 0, 0, 0), sd = 1),
               "argument \"n\" is missing, with no default")
  
  expect_error(ANOVA_design("100b", mu = c(1:100),
                            sd =1,
                            n = 150))

  # passing bad arguments: *SHOULD BE AN ERROR*
  expect_error(ANOVA_design("2w*2b", n = 100, mu = c(0, 0, 0, 0), sd = -1),
               "Standard deviation (sd) is less than or equal to zero; input a value greater than zero", fixed = TRUE)
  expect_error(ANOVA_design("2F", n = 10, mu = 1:2, sd = 1),
               "Problem in the design argument: must input number of levels as integer (2-999) and factor-type (between or within) as lower case b (between) or w (within)",
               fixed = TRUE)
  expect_error(ANOVA_design("1001b", n = 10, mu = 1:1001, sd = 1),
               "Problem in the design argument: must input number of levels as integer (2-999) and factor-type (between or within) as lower case b (between) or w (within)",
               fixed = TRUE)

  # bad arguments
  expect_error(ANOVA_design("wrong design"),
               "Problem in the design argument: must input number of levels as integer (2-999) and factor-type (between or within) as lower case b (between) or w (within)",
               fixed = TRUE)
  expect_error(ANOVA_design("2w*2b", n = "A", mu = 1:4, sd = 1),
               "non-numeric argument to binary operator")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = c("A", "B", "C", "D"), sd = 1),
               "non-numeric argument to binary operator")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = "A"),
               "requires numeric/complex matrix/vector arguments")
  expect_error(ANOVA_design("2w*2b", n = "A", mu = 1:4, sd = 1),
               "non-numeric argument to binary operator")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1, r = "A"),
               "Correlation must be greater than -1 and less than 1",
               fixed = TRUE)
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1, labelnames = c("A", "B")),
               "Variable 'design' does not match the length of the labelnames")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1,
                            labelnames = c("W factor", "W 1", "W 2", "B factor", "B 1", "B 2")),
               "unexpected symbol")


  # inconsistent arguments
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 0, sd = 1),
               "the length of the vector with means does not match the study design")
  #expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = c(1,2)),
  #             "The SD must be a length of 1 or match the length of the study design")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = c(1,1)),
               "the length of sd_for_sigma must be 1 or vars")
  expect_error(ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1, r = c(.5, 0, .4)),
               "object 'cor_mat' not found")
})


# test_that("2W", {
#   # fix uppercase letters are design = 0
#   d <- ANOVA_design("2W", n = 100, mu = c(0,0), sd = 1)
#   expect_equal(d$design, 1)
# })
#
# # test grep for strings ----
# test_that("test grep for strings", {
#   pattern <- "^([2-9](w|b)\\*){0,2}[2-9](w|b)$"
#
#   # should work
#   good_strings <- c("2w", "2b", "2W", "2B", "2w*2b", "3w*3b*2w")
#   for (x in good_strings) {
#     find <- grep(pattern, x, ignore.case = TRUE, perl = TRUE)
#     #expect_equal(find, 1)
#     if (length(find) == 0 || find != 1) { print(x) }
#   }
#
#
#   # should not work
#   bad_strings <- c("2a", "w2", "b2", "0w", "0b", "1w", "1b", "2b+2w", "2b*2b*2b*2b")
#   for (x in bad_strings) {
#     find <- grep(pattern, x, ignore.case = TRUE, perl = TRUE)
#     #expect_equal(find, integer(0))
#     if (length(find) != 0) { print(x) }
#   }
# })

# 2w defaults ----
test_that("2w defaults", {
  d <- ANOVA_design("2w", n = 100, mu = c(0,0), sd = 1, plot = FALSE)
  expect_equal(d$design_factors, 1)
  expect_equal(d$design_list, c("a1", "a2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ a + Error(subject/a))
  expect_equal(d$frml2, ~a)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "a1" = c(1, 0),
    "a2" = c(0, 1),
    row.names = c("a1", "a2")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2w")
  expect_equal(d$labelnameslist, list(c("a1", "a2")))
  expect_equal(d$factornames, "a")
})

# 2b defaults----
test_that("2b defaults", {
  d <- ANOVA_design("2b", n = 100, mu = c(0,0), sd = 1, plot = FALSE)
  expect_equal(d$design_factors, 0)
  expect_equal(d$design_list, c("a1", "a2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ a + Error(1 | subject))
  expect_equal(d$frml2, ~a)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "a1" = c(1, 0),
    "a2" = c(0, 1),
    row.names = c("a1", "a2")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2b")
  expect_equal(d$labelnameslist, list(c("a1", "a2")))
  expect_equal(d$factornames, "a")
})

# 2w set r & labels ----
test_that("2w set r & labels", {
  d <- ANOVA_design("2w", n = 100, mu = c(0,0), sd = 1, r = 0.5, labelnames = c("W", "W1", "W2"), plot = FALSE)
  expect_equal(d$design_factors, 1)
  expect_equal(d$design_list, c("W1", "W2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ W + Error(subject/W))
  expect_equal(d$frml2, ~W)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "W1" = c(1, 0.5),
    "W2" = c(0.5, 1),
    row.names = c("W1", "W2")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2w")
  expect_equal(d$labelnameslist, list(c("W1", "W2")))
  expect_equal(d$factornames, "W")
})

# 2b set r & labels----
test_that("2b set r & labels", {
  d <- ANOVA_design("2b", n = 100, mu = c(0,0), sd = 1, r = 0.5, labelnames = c("B", "B1", "B2"), plot = FALSE)
  expect_equal(d$design_factors, 0)
  expect_equal(d$design_list, c("B1", "B2"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ B + Error(1 | subject))
  expect_equal(d$frml2, ~B)
  expect_equal(d$mu, c(0, 0))
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "B1" = c(1, 0),
    "B2" = c(0, 1),
    row.names = c("B1", "B2")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2b")
  expect_equal(d$labelnameslist, list(c("B1", "B2")))
  expect_equal(d$factornames, "B")
})

# 4w
test_that("4w", {
  d <- ANOVA_design("4w", n = 100, mu = 1:4, sd = 1:4, r = 0.5,
                    labelnames = c("W", "W1", "W2", "W3", "W4"), plot = FALSE)
  expect_equal(d$design_factors, 1)
  expect_equal(d$design_list, c("W1", "W2", "W3", "W4"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ W + Error(subject/W))
  expect_equal(d$frml2, ~W)
  expect_equal(d$mu, 1:4)
  expect_equal(d$sd, 1:4)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "W1" = c(1, 0.5, 0.5, 0.5),
    "W2" = c(0.5, 1, 0.5, 0.5),
    "W3" = c(0.5, 0.5, 1, 0.5),
    "W4" = c(0.5, 0.5, 0.5, 1),
    row.names = c("W1", "W2", "W3", "W4")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  sigma <- as.matrix(mat) * (1:4 %*% t(1:4))
  expect_true(dplyr::all_equal(d$sigmatrix, as.data.frame(sigma)))

  expect_equal(d$design, "4w")
  expect_equal(d$labelnameslist, list(c("W1", "W2", "W3", "W4")))
  expect_equal(d$factornames, "W")
})

# 4b
test_that("4b", {
  d <- ANOVA_design("4b", n = 100, mu = 1:4, sd = 1:4, r = 0.5,
                    labelnames = c("B", "B1", "B2", "B3", "B4"), plot = FALSE)
  expect_equal(d$design_factors, 0)
  expect_equal(d$design_list, c("B1", "B2", "B3", "B4"))
  expect_equal(d$factors, 1)
  expect_equal(d$frml1, y ~ B + Error(1 | subject))
  expect_equal(d$frml2, ~B)
  expect_equal(d$mu, 1:4)
  expect_equal(d$sd, 1:4)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "B1" = c(1, 0, 0, 0),
    "B2" = c(0, 1, 0, 0),
    "B3" = c(0, 0, 1, 0),
    "B4" = c(0, 0, 0, 1),
    row.names = c("B1", "B2", "B3", "B4")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  sigma <- as.matrix(mat) * (1:4 %*% t(1:4))
  expect_true(dplyr::all_equal(d$sigmatrix, as.data.frame(sigma)))

  expect_equal(d$design, "4b")
  expect_equal(d$labelnameslist, list(c("B1", "B2", "B3", "B4")))
  expect_equal(d$factornames, "B")
})


# 2w*2b----
test_that("2w*2b", {
  d <- ANOVA_design("2w*2b", n = 100, mu = 1:4, sd = 1, r = 0.5,
                    labelnames = c("W", "W1", "W2", "B", "B1", "B2"), plot = FALSE)
  expect_equal(d$design_factors, c(1, 0))
  expect_equal(d$design_list, c("W1_B1", "W1_B2", "W2_B1", "W2_B2"))
  expect_equal(d$factors, 2)
  expect_equal(d$frml1, y ~ W * B + Error(subject/W))
  expect_equal(d$frml2, ~W + B)
  expect_equal(d$mu, 1:4)
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "W1_B1" = c(1, .5, 0, 0),
    "W2_B1" = c(.5, 1, 0, 0),
    "W1_B2" = c(0, 0, 1, .5),
    "W2_B2" = c(0, 0, .5, 1),
    row.names = c("W1_B1", "W2_B1", "W1_B2", "W2_B2")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2w*2b")
  expect_equal(d$labelnameslist, list(c("W1", "W2"), c("B1", "B2")))
  expect_equal(d$factornames, c("W", "B"))
})

# 2b*2w----
test_that("2b*2w", {
  d <- ANOVA_design("2b*2w", n = 100, mu = 1:4, sd = 1, r = 0.5,
                    labelnames = c("B", "B1", "B2", "W", "W1", "W2"), plot = FALSE)
  expect_equal(d$design_factors, c(0, 1))
  expect_equal(d$design_list, c("B1_W1", "B1_W2", "B2_W1", "B2_W2"))
  expect_equal(d$factors, 2)
  expect_equal(d$frml1, y ~ B * W + Error(subject/W))
  expect_equal(d$frml2, ~B + W)
  expect_equal(d$mu, 1:4)
  expect_equal(d$sd, 1)
  expect_equal(d$r, 0.5)
  expect_equal(d$n, 100)

  mat <- data.frame(
    "B1_W1" = c(1, 0, .5, 0),
    "B2_W1" = c(0, 1, 0, .5),
    "B1_W2" = c(.5, 0, 1, 0),
    "B2_W2" = c(0, .5, 0, 1),
    row.names = c("B1_W1", "B2_W1", "B1_W2", "B2_W2")
  )
  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2b*2w")
  expect_equal(d$labelnameslist, list(c("B1", "B2"), c("W1", "W2")))
  expect_equal(d$factornames, c("B", "W"))
})

#Add three way designs
test_that("2b*2b*2b", {
  d <- ANOVA_design("2b*2b*2b", n = 100, mu = 1:8, sd = 1, plot = FALSE)
  expect_equal(d$design_factors, c(0, 0, 0))
  expect_equal(d$design_list, c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))
  expect_equal(d$factors, 3)
  expect_equal(d$frml1, y ~ a * b * c + Error(1 | subject))
  expect_equal(d$frml2, ~ a + b + c)
  expect_equal(d$mu, 1:8)
  expect_equal(d$sd, 1)
  expect_equal(d$n, 100)


  mat <- data.frame(
    "a1_b1_c1" = c(1, 0, 0, 0, 0, 0, 0, 0),
    "a1_b1_c2" = c(0, 1, 0, 0, 0, 0, 0, 0),
    "a1_b2_c1" = c(0, 0, 1, 0, 0, 0, 0, 0),
    "a1_b2_c2" = c(0, 0, 0, 1, 0, 0, 0, 0),
    "a2_b1_c1" = c(0, 0, 0, 0, 1, 0, 0, 0),
    "a2_b1_c2" = c(0, 0, 0, 0, 0, 1, 0, 0),
    "a2_b2_c1" = c(0, 0, 0, 0, 0, 0, 1, 0),
    "a2_b2_c2" = c(0, 0, 0, 0, 0, 0, 0, 1),
    row.names = c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))

  expect_true(dplyr::all_equal(d$cor_mat, mat))
  expect_true(dplyr::all_equal(d$sigmatrix, mat))

  expect_equal(d$design, "2b*2b*2b")

})

test_that("2w*2w*2w", {
  d <- ANOVA_design("2w*2w*2w", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  expect_equal(d$design_factors, c(1, 1, 1))
  expect_equal(d$design_list, c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))
  expect_equal(d$factors, 3)
  expect_equal(d$frml1, y ~ a * b * c + Error(subject/a * b * c))
  expect_equal(d$frml2, ~ a + b + c)
  expect_equal(d$mu, 1:8)
  expect_equal(d$sd, 2)
  expect_equal(d$n, 100)
  expect_equal(d$r, 0.65)

  mat <- data.frame(
    "a1_b1_c1" = c(1.00, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65),
    "a1_b1_c2" = c(0.65, 1.00, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65),
    "a1_b2_c1" = c(0.65, 0.65, 1.00, 0.65, 0.65, 0.65, 0.65, 0.65),
    "a1_b2_c2" = c(0.65, 0.65, 0.65, 1.00, 0.65, 0.65, 0.65, 0.65),
    "a2_b1_c1" = c(0.65, 0.65, 0.65, 0.65, 1.00, 0.65, 0.65, 0.65),
    "a2_b1_c2" = c(0.65, 0.65, 0.65, 0.65, 0.65, 1.00, 0.65, 0.65),
    "a2_b2_c1" = c(0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 1.00, 0.65),
    "a2_b2_c2" = c(0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 1.00),
    row.names = c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))

  expect_true(dplyr::all_equal(d$cor_mat, mat))

  var <- (2)^2
  covar <- var*as.numeric(0.65)

  sig_mat <- data.frame(
    "a1_b1_c1" = c(var, covar, covar, covar, covar, covar, covar, covar),
    "a1_b1_c2" = c(covar, var, covar, covar, covar, covar, covar, covar),
    "a1_b2_c1" = c(covar, covar, var, covar, covar, covar, covar, covar),
    "a1_b2_c2" = c(covar, covar, covar, var, covar, covar, covar, covar),
    "a2_b1_c1" = c(covar, covar, covar, covar, var, covar, covar, covar),
    "a2_b1_c2" = c(covar, covar, covar, covar, covar, var, covar, covar),
    "a2_b2_c1" = c(covar, covar, covar, covar, covar, covar, var, covar),
    "a2_b2_c2" = c(covar, covar, covar, covar, covar, covar, covar, var),
    row.names = c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))
  expect_true(dplyr::all_equal(d$sigmatrix, sig_mat))

  expect_equal(d$design, "2w*2w*2w")

})

test_that("2b*2b*2w", {
  d <- ANOVA_design("2b*2b*2w", n = 100, mu = 1:8, sd = 1.5, r = .68, plot = FALSE)
  expect_equal(d$design_factors, c(0, 0, 1))
  expect_equal(d$design_list, c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))
  expect_equal(d$factors, 3)
  expect_equal(d$frml1, y ~ a * b * c + Error(subject/c))
  expect_equal(d$frml2, ~ a + b + c)
  expect_equal(d$mu, 1:8)
  expect_equal(d$sd, 1.5)
  expect_equal(d$n, 100)
  expect_equal(d$r, 0.68)

  mat <- data.frame(
    "a1_b1_c1" = c(1.00, 0.68, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
    "a1_b1_c2" = c(0.68, 1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
    "a1_b2_c1" = c(0.00, 0.00, 1.00, 0.68, 0.00, 0.00, 0.00, 0.00),
    "a1_b2_c2" = c(0.00, 0.00, 0.68, 1.00, 0.00, 0.00, 0.00, 0.00),
    "a2_b1_c1" = c(0.00, 0.00, 0.00, 0.00, 1.00, 0.68, 0.00, 0.00),
    "a2_b1_c2" = c(0.00, 0.00, 0.00, 0.00, 0.68, 1.00, 0.00, 0.00),
    "a2_b2_c1" = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 1.00, 0.68),
    "a2_b2_c2" = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.68, 1.00),
    row.names = c("a1_b1_c1", "a1_b1_c2", "a1_b2_c1", "a1_b2_c2", "a2_b1_c1", "a2_b1_c2", "a2_b2_c1", "a2_b2_c2"))

  expect_true(dplyr::all_equal(d$cor_mat, mat))

  sig_mat <- mat
  sig_mat[mat == 0.68] <- (1.5^2)*0.68
  sig_mat[mat == 1.00] <- (1.5^2)

  expect_true(dplyr::all_equal(d$sigmatrix, sig_mat))

  expect_equal(d$design, "2b*2b*2w")

})

# Add final for complete coverage

test_that("Other 3 way designs",{
  # Needed to cover lines after factors == 3
  d <- ANOVA_design("2b*2w*2w", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  d <- ANOVA_design("2w*2b*2w", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  d <- ANOVA_design("2w*2w*2b", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  d <- ANOVA_design("2b*2b*2w", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  d <- ANOVA_design("2b*2b*2b", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  d <- ANOVA_design("2w*2b*2b", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  d <- ANOVA_design("2w*2w*2b", n = 100, mu = 1:8, sd = 2, r = .65, plot = FALSE)
  
  d <- ANOVA_design("2w*9b", n = 100, mu = 1:18, sd = 2, r = .65, plot = FALSE)
})
