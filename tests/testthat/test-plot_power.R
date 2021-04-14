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
               "plot_power must have an ANOVA_design object with n > the product of the factors; please set exact2 argument to TRUE",
               fixed = TRUE)
  

  expect_error(plot_power(design,
                           emm = TRUE,
                           emm_model = "NOT"))
  
  expect_error(plot_power(design,
                           emm = TRUE,
                           contrast_type = "NOT"))
  
  expect_error(plot_power(design,
                           emm = TRUE,
                           correction = "none1"))
  
  expect_error(plot_power(design,
                           emm = TRUE,
                           alpha_level = 1.05))
})

test_that("test multi functions", {
  skip_on_cran()
  design <- ANOVA_design(design = "2b*4w",
                         n = 70,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  p <- hush(plot_power(design, emm = TRUE, min_n = 9, max_n = 100, plot=FALSE))
  # Test exact2 function
  p_exact2 <- hush(plot_power(
    design,
    emm = TRUE,
    min_n = 9,
    max_n = 100,
    plot = FALSE,
    exact2 = TRUE
  ))
  
  p_ex1 = ANOVA_exact(design, emm = TRUE, verbose = FALSE)
  
  res70 = p$power_df_emm[p$power_df_emm$n == 70, ]
  res70 = as.numeric(res70[,-1])
  expect_setequal(round(res70,1), round(p_ex1$emm_results$power,1))
  
  design9 <- ANOVA_design(design = "2b*4w",
                         n = 9,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  design100 <- ANOVA_design(design = "2b*4w",
                         n = 100,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  ex9 = ANOVA_exact(design9, emm = TRUE, verbose = FALSE)
  
  ex100 = ANOVA_exact(design100, emm = TRUE, verbose = FALSE)
  
  res9 = p$power_df_manova[p$power_df_manova$n == 9, ]
  res9 = as.numeric(res9[,-1])
  
  expect_setequal(round(res9,1), round(ex9$manova_results$power,1))
  
  res100 = p$power_df_manova[p$power_df_manova$n == 100, ]
  res100 = as.numeric(res100[,-1])
  
  expect_setequal(round(res100,1), round(ex100$manova_results$power,1))
  
  # Old tests
  #expect_equal(p$power_df_manova[1,3],41.9,tolerance = .01)
  #expect_equal(p$power_df_manova[1,2],41.9,tolerance = .01)
  #expect_equal(p$power_df_manova[1,4],5,tolerance = .01)
  #expect_equal(p$power_df_manova[1,5],5,tolerance = .01)
  #expect_equal(p$power_df_manova[93,3],99.9,tolerance = .01)
})

test_that("test multi functions: exact2", {
  skip_on_cran()
  design <- ANOVA_design(design = "2b*4w",
                         n = 70,
                         mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                         sd = 1,
                         plot = FALSE)
  
  p <- hush(plot_power(design, emm = TRUE, min_n = 9, max_n = 100, plot=FALSE, exact2 = TRUE))
  # Test exact2 function
  
  p_ex1 = ANOVA_exact2(design, emm = TRUE, verbose = FALSE)
  
  res70 = p$power_df_emm[p$power_df_emm$n == 70, ]
  res70 = as.numeric(res70[,-1])
  expect_setequal(round(res70,1), round(p_ex1$emm_results$power,1))
  
  design9 <- ANOVA_design(design = "2b*4w",
                          n = 9,
                          mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                          sd = 1,
                          plot = FALSE)
  
  design100 <- ANOVA_design(design = "2b*4w",
                            n = 100,
                            mu = c(0,0,0,0,0.5,0.5,0.5,0.5),
                            sd = 1,
                            plot = FALSE)
  
  ex9 = ANOVA_exact2(design9, emm = TRUE, verbose = FALSE)
  
  ex100 = ANOVA_exact2(design100, emm = TRUE, verbose = FALSE)
  
  res9 = p$power_df_manova[p$power_df_manova$n == 9, ]
  res9 = as.numeric(res9[,-1])
  
  expect_setequal(round(res9,1), round(ex9$manova_results$power,1))
  
  res100 = p$power_df_manova[p$power_df_manova$n == 100, ]
  res100 = as.numeric(res100[,-1])
  
  expect_setequal(round(res100,1), round(ex100$manova_results$power,1))

})


test_that("test 2b", {
  skip_on_cran()
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
  expect_equal(p$power_df[1,2], 13.85,tolerance = .001)
  expect_equal(p$power_df[94,2],94.04,tolerance = .001)
  
})


test_that("match",{
  skip_on_cran()
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
