context("test-plot_power")

hush=function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}


# error messages
test_that("error messages", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 7,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  expect_error(plot_power(), "argument \"design_result\" is missing, with no default")
  expect_error(plot_power(design),
               "plot_power must have an ANOVA_design object with n > the product of the factors; please increase the n in ANOVA_design function.",
               fixed = TRUE)
})

test_that("test multi functions", {
  design <- ANOVA_design(design = "2b*4w",
                         n = 70,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  p <- hush(plot_power(design, emm = TRUE, min_n = 8, max_n = 100))
  
  p_ex1 = ANOVA_exact(design, emm = TRUE, verbose = FALSE)
  
  res70 = p$power_df_emm[p$power_df_emm$n == 70, ]
  res70 = as.numeric(res70[,-1])
  expect_setequal(round(res70,1), round(p_ex1$emm_results$power,1))
  expect_equal(p$power_df_manova[1,3],41.9,tolerance = .01)
  expect_equal(p$power_df_manova[1,2],41.9,tolerance = .01)
  expect_equal(p$power_df_manova[1,4],5,tolerance = .01)
  expect_equal(p$power_df_manova[1,5],5,tolerance = .01)
  expect_equal(p$power_df_manova[93,3],99.9,tolerance = .01)
})


test_that("test 2b", {
  design <- ANOVA_design(design = "2b",
                         n = 7,
                         mu = c(0,.5),
                         sd = 1,
                         plot = FALSE)
  
  p = hush(plot_power(design, emm = TRUE, min_n = 7, max_n = 100,
                      verbose = TRUE))
  
  p_ex1 = ANOVA_exact(design, emm = TRUE, verbose = FALSE)
  res7 = p$power_df_emm[p$power_df_emm$n == 7, ]
  res7 = as.numeric(res7[,-1])
  expect_setequal(round(res7,1), round(p_ex1$emm_results$power,1))
  expect_equal(p$power_df[1,2], 13.8,tolerance = .05)
  expect_equal(p$power_df[94,2],96,tolerance = .01)
  
})


test_that("match",{
  design <- ANOVA_design(design = "3b*2b",
                         n = 20,
                         mu = c(1, 2, 2, 3, 3, 4),
                         sd = 3,
                         plot = FALSE)
  x1 = ANOVA_exact(design, verbose = FALSE)
  p1 = plot_power(design,
                  min_n = 10, max_n = 100,
                  desired_power = 90, plot = FALSE,
                  verbose = FALSE)
  design2 <- ANOVA_design(design = "3b*2b",
                          n = 80,
                          mu = c(1, 2, 2, 3, 3, 4),
                          sd = 3,
                          plot = FALSE)
  x2 = ANOVA_exact(design2, verbose = FALSE)
  p2 = plot_power(design2,
                  min_n = 10, max_n = 100,
                  desired_power = 90, plot = FALSE,
                  verbose = FALSE)
  
  design3 <- ANOVA_design(design = "3b*2b",
                          n = 180,
                          mu = c(1, 2, 2, 3, 3, 4),
                          sd = 3,
                          plot = FALSE)
  x3 = ANOVA_exact(design3, verbose = FALSE)
  expect_setequal(round(p1$power_df$b,2),round(p2$power_df$b,2))
  
  expect_setequal(round(p1$power_df$a,2),round(p2$power_df$a,2))
  
  expect_setequal(round(p1$power_df$`a:b`,2),round(p2$power_df$`a:b`,2))
})
