context("test-oneway_ancova")

# error messages
test_that("error messages", {
  
  ex = power_oneway_ancova(mu = c(400, 450, 500), 
                           n = c(21,21,21),
                           r2 = .1^2, sd = 100)
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100,
    alpha_level = 0.05,
    beta_level = .15
  ))
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21),
    r2 = .1^2, sd = 100,
    alpha_level = 0.05
  ))
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100,
    alpha_level = -0.05
  ))
  
  
  expect_error(power_oneway_ancova(
    mu = c(400, 450, 500), 
    n = c(21,21,21),
    r2 = .1^2, sd = 100,
    alpha_level = NULL,
    beta_level = -1
  ))
  
  
  
})

test_that("Match Table 1 Shieh 2020", {
  
  # Exact match
  mu1 = c(400,450,500)
  sd1 = 100
  r2 = 0.1^2
  
  n_cov1 = c(1:10)
  n1 = c(63, 63, 66, 66, 66, 69, 69, 69, 72, 72)
  pow1 = c(81.15,
           80.38,
           81.76,
           81.04,
           80.29,
           81.66,
           80.93,
           80.18,
           81.56,
           80.83)
  
  df1 = data.frame(n_cov = n_cov1,
                   n = n1,
                   power_shieh = pow1)
  
  df1$power = NA
  for(i in 1:10){
    #print(i)
   df1$power[i] =  round(power_oneway_ancova(n = rep(df1$n[i]/3,3),
                                  n_cov = df1$n_cov[i],
                                  mu = mu1,
                                  r2 = r2,
                                  sd = sd1,
                                  type = "exact")$power,2)
  }

   
   expect_equal(df1$power,df1$power_shieh)
   
   df1$beta = 1-(df1$power/100)
   
   df1$alpha = NA
   for(i in 1:10){
     df1$alpha[i] =  round(power_oneway_ancova(n = rep(df1$n[i]/3,3),
                                               n_cov = df1$n_cov[i],
                                               mu = mu1,
                                               r2 = r2,
                                               sd = sd1,
                                               alpha_level = NULL,
                                               beta_level = df1$beta[i],
                                               type = "exact")$alpha_level,2)
   }
   expect_equal(df1$alpha, rep(.05,10))
   
   df1$r_2 = NA
   for(i in 1:10){
     df1$r_2[i] =  round(power_oneway_ancova(n = rep(df1$n[i]/3,3),
                                               n_cov = df1$n_cov[i],
                                               mu = mu1,
                                               r2 = NULL,
                                               sd = sd1,
                                               alpha_level = 0.05,
                                               beta_level = df1$beta[i],
                                               type = "exact")$r2,2)
   }
   expect_equal(df1$r_2, rep(r2,10))
   # Approx ---
   
   n_cov2 = c(1:10)
   n2 = rep(63,10)
   pow2 = c(81.85, 81.81, 81.78, 81.74, 81.70,
            81.66, 81.61, 81.57, 81.52, 81.47)
   
   df2 = data.frame(n_cov = n_cov2,
                    n = n2,
                    power_keppel = pow2)
   
  
  
   df2$power = NA
   for(i in 1:10){
     #print(i)
     df2$power[i] =  round(power_oneway_ancova(n = rep(df2$n[i]/3,3),
                                               n_cov = df2$n_cov[i],
                                               mu = mu1,
                                               r2 = r2,
                                               sd = sd1,
                                               type = "approx")$power,2)
   }
   
   expect_equal(df2$power, df2$power_keppel)
   
   df2$beta = 1-(df2$power/100)
   
   df2$alpha = NA
   for(i in 1:10){
     df2$alpha[i] =  round(power_oneway_ancova(n = rep(df2$n[i]/3,3),
                                               n_cov = df2$n_cov[i],
                                               mu = mu1,
                                               r2 = r2,
                                               sd = sd1,
                                               alpha_level = NULL,
                                               beta_level = df2$beta[i],
                                               type = "approx")$alpha_level,2)
   }
   expect_equal(df2$alpha, rep(.05,10))
   
   df2$r_2 = NA
   for(i in 1:10){
     df2$r_2[i] =  round(power_oneway_ancova(n = rep(df2$n[i]/3,3),
                                             n_cov = df2$n_cov[i],
                                             mu = mu1,
                                             r2 = NULL,
                                             sd = sd1,
                                             alpha_level = 0.05,
                                             beta_level = df2$beta[i],
                                             type = "approx")$r2,2)
   }
   expect_equal(df2$r_2, rep(r2,10))
  

  
  
})

test_that("Match Table 3 Shieh 2020", {
  
  # Exact match
  mu1 = c(400,450,500)
  sd1 = 100
  r2 = 0.9^2
  
  n_cov1 = c(1:10)
  n1 = c(18, 18, 18, 21, 21, 
         24, 24, 24, 27, 27)
  pow1 = c(87.51, 84.09, 80.07, 86.14, 82.72, 
           88.07, 85.16, 81.73, 87.35, 84.42)
  
  df1 = data.frame(n_cov = n_cov1,
                   n = n1,
                   power_shieh = pow1)
  
  df1$power = NA
  for(i in 1:10){
    #print(i)
    df1$power[i] =  round(power_oneway_ancova(n = rep(df1$n[i]/3,3),
                                              n_cov = df1$n_cov[i],
                                              mu = mu1,
                                              r2 = r2,
                                              sd = sd1,
                                              type = "exact")$power,2)
  }
  
  
  expect_equal(df1$power,df1$power_shieh)
  
  n_cov2 = c(1:10)
  n2 = c(15, 18, 18, 18, 18, 18, 18, 18, 21, 21)
  pow2 = c(81.08, 89.34, 88.67, 87.84, 86.81, 
           85.48, 83.73, 81.36, 90.48, 89.04)
  
  df2 = data.frame(n_cov = n_cov2,
                   n = n2,
                   power_keppel = pow2)
  
  
  
  df2$power = NA
  for(i in 1:10){
    #print(i)
    df2$power[i] =  round(power_oneway_ancova(n = rep(df2$n[i]/3,3),
                                              n_cov = df2$n_cov[i],
                                              mu = mu1,
                                              r2 = r2,
                                              sd = sd1,
                                              type = "approx")$power,2)
  }
  
  expect_equal(df2$power, df2$power_keppel)
  
  
  
  
})
