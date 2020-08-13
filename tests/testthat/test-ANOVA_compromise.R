context("test-ANOVA_compromise")

# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)

  expect_error(ANOVA_compromise())



})


test_that("example #1",{
  design_result <- ANOVA_design(design = "3b*2b",
  n = 6,
  mu = c(1, 2, 2, 3, 3, 4),
  sd = 3,
  plot = FALSE)
  example = ANOVA_compromise(design_result)
})