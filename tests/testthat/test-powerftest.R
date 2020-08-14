context("test-power.ftest")

# error messages
test_that("error messages", {
  design_result <- ANOVA_design(design = "2b",
  n = 65,
  mu = c(0,.5),
  sd = 1,
  plot = FALSE)
  
  x1 = ANOVA_exact2(design_result, verbose = FALSE)
  
  ex = power.ftest(num_df = x1$anova_table$num_df, 
  den_df = x1$anova_table$den_df, 
  cohen_f = x1$main_result$cohen_f,
  alpha_level = 0.05,
  liberal_lambda = FALSE)
  
  expect_error(power.ftest(
    num_df = x1$anova_table$num_df,
    den_df = x1$anova_table$den_df,
    cohen_f = x1$main_result$cohen_f,
    alpha_level = 0.05,
    beta_level = .15,
    liberal_lambda = FALSE
  ))
  
  expect_error(power.ftest(
    num_df = x1$anova_table$num_df,
    den_df = x1$anova_table$den_df,
    cohen_f = -1,
    alpha_level = 0.05,
    liberal_lambda = FALSE
  ))
  
  expect_error(power.ftest(
    num_df = x1$anova_table$num_df,
    den_df = 0,
    cohen_f = x1$main_result$cohen_f,
    alpha_level = 0.05,
    liberal_lambda = FALSE
  ))
  
  expect_error(power.ftest(
    num_df = 0,
    den_df = x1$anova_table$den_df,
    cohen_f = x1$main_result$cohen_f,
    alpha_level = 0.05,
    liberal_lambda = FALSE
  ))
  
  expect_error(power.ftest(
    num_df = x1$anova_table$num_df,
    den_df = x1$anova_table$den_df,
    cohen_f = x1$main_result$cohen_f,
    alpha_level = 5,
    liberal_lambda = FALSE
  ))
  
  expect_error(power.ftest(
    num_df = x1$anova_table$num_df,
    den_df = x1$anova_table$den_df,
    cohen_f = x1$main_result$cohen_f,
    beta_level = 15,
    liberal_lambda = FALSE
  ))
  
  
  
  
  
})

test_that("Test power.ftest", {
  design_result <- ANOVA_design(design = "2b",
                                n = 65,
                                mu = c(0,.5),
                                sd = 1,
                                plot = FALSE)
  
  x1 = ANOVA_exact2(design_result, verbose = FALSE)
  beta_test = 1 - x1$main_results$power/100
  
  #beta
  ex = power.ftest(num_df = x1$anova_table$num_df, 
                   den_df = x1$anova_table$den_df, 
                   cohen_f = x1$main_result$cohen_f,
                   alpha_level = 0.05,
                   beta_level = NULL,
                   liberal_lambda = FALSE)
  
  expect_equal(ex$beta_level,beta_test)
  
  #cohen_f
  ex = power.ftest(num_df = x1$anova_table$num_df, 
                   den_df = x1$anova_table$den_df, 
                   alpha_level = 0.05,
                   beta_level = beta_test,
                   liberal_lambda = FALSE)
  
  expect_equal(x1$main_result$cohen_f, ex$cohen_f, tolerance = .0001)
  
  #num_df
  ex = power.ftest(den_df = x1$anova_table$den_df, 
                   cohen_f = x1$main_result$cohen_f,
                   alpha_level = 0.05,
                   beta_level = beta_test,
                   liberal_lambda = FALSE)
  
  expect_equal(ex$num_df,x1$anova_table$num_df, tolerance = .001)
  

  #den_df
  ex = power.ftest(num_df = x1$anova_table$num_df, 
                   cohen_f = x1$main_result$cohen_f,
                   alpha_level = 0.05,
                   beta_level = beta_test,
                   liberal_lambda = FALSE)
  
  expect_equal(ex$den_df,x1$anova_table$den_df)
  
  #alpha_level
  ex = power.ftest(den_df = x1$anova_table$den_df,
                   num_df = x1$anova_table$num_df, 
                   cohen_f = x1$main_result$cohen_f,
                   beta_level = beta_test,
                   alpha_level = NULL,
                   liberal_lambda = FALSE)
  
  expect_equal(ex$den_df,x1$anova_table$den_df)
  


})

