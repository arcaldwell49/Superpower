context("test-ancova_analytic")

hush=function(code){
  sink("NUL") # use /dev/null in UNIX
  tmp = code
  sink()
  return(tmp)
}

# error messages
test_that("error messages", {
  expect_error(ANCOVA_analytic())
  
  des1 = ANOVA_design("2w",
                      n = 10,
                      mu = c(0,1),
                      sd = 1,
                      r = .75)
  expect_error(ANCOVA_analytic(design_result = des1,
                               r2 = .25))
  expect_error(ANCOVA_analytic(design_result = des1,
                               r2 = .25, n_cov = 1))
  
  des2 = ANOVA_design("2b",
                     n = 10,
                     mu = c(0,1),
                     sd = 1)
  
  expect_error(ANCOVA_analytic("2b",
                               n = 10,
                               mu = c(0,1,2),
                               sd = 1, 
                               n_cov = 1,
                               r2 = .25))
  expect_error(ANCOVA_analytic("9999b",
                               n = 10,
                               mu = c(0,1),
                               sd = 1, 
                               n_cov = 1,
                               r2 = .25))
  
  expect_error(ANCOVA_analytic("2b",
                               n = 10,
                               mu = c(0,1),
                               sd = -1, 
                               n_cov = 1,
                               r2 = .25))
  expect_error(ANCOVA_analytic("2b",
                               n = 10,
                               mu = c(0,1),
                               sd = 1, 
                               n_cov = 1,
                               r2 = -.25))
  
  expect_error(ANCOVA_analytic("2b",
                               n = c(10,10,99),
                               mu = c(0,1),
                               sd = 1, 
                               n_cov = 1,
                               r2 = .25))
})


test_that("match simulations - one way",
          {
            expect_equal(.8148, 
                         ANCOVA_analytic(design = "3b",
                                         mu = c(400,450,500),
                                         n = 21,
                                         sd = 100,
                                         r2 = .1^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power/100, 
                         tolerance = .01)
            
            expect_equal(.8, 
                         ANCOVA_analytic(design = "3b",
                                         mu = c(400,450,500),
                                         n = 48/3,
                                         sd = 100,
                                         r2 = .5^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power/100, 
                         tolerance = .01)
            
            expect_equal(.875, 
                         ANCOVA_analytic(design = "3b",
                                         mu = c(400,450,500),
                                         n = 6,
                                         sd = 100,
                                         r2 = .9^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power/100, 
                         tolerance = .01)

          })


test_that("match simulations - two way",
          {
            hush(
              ANCOVA_analytic(
                design = "2b*2b",
                mu = c(1, 0, 1, 0),
                n = 20,
                sd = 2.5,
                r2 = .1 ^ 2,
                n_cov = 1,
                alpha_level = .05,
                beta_level = NULL
              )
            )
            expect_equal(.05, 
                         ANCOVA_analytic(design = "2b*2b",
                                         mu = c(1,0,1,0),
                                         n = 20,
                                         sd = 2.5,
                                         r2 = .1^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power[1]/100, 
                         tolerance = .01)
            
            expect_equal(.42, 
                         ANCOVA_analytic(design = "2b*2b",
                                         mu = c(1,0,1,0),
                                         n = 20,
                                         sd = 2.5,
                                         r2 = .1^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power[2]/100, 
                         tolerance = .01)
            
            expect_equal(.718, 
                         ANCOVA_analytic(design = "2b*3b",
                                         mu = c(0,1,2,0,1.5,3),
                                         n = 33,
                                         sd = 1.5,
                                         r2 = .4^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power[1]/100, 
                         tolerance = .01)
            
            expect_equal(.99, 
                         ANCOVA_analytic(design = "2b*3b",
                                         mu = c(0,1,2,0,1.5,3),
                                         n = 33,
                                         sd = 1.5,
                                         r2 = .4^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power[2]/100, 
                         tolerance = .01)
            
            expect_equal(.44, 
                         ANCOVA_analytic(design = "2b*3b",
                                         mu = c(0,1,2,0,1.5,3),
                                         n = 33,
                                         sd = 1.5,
                                         r2 = .4^2,
                                         n_cov = 1,
                                         alpha_level = .05,
                                         beta_level = NULL)$main_results$power[3]/100, 
                         tolerance = .01)
            
            expect_equal(.726, 
                         ANCOVA_analytic(
                           design = "3b*3b",
                           mu = c(1, 1, 1, 1.5, 1.5, 1.5, 2, 2, 2),
                           n = 17,
                           sd = 1.78,
                           r2 = .2 ^ 2,
                           n_cov = 1,
                           alpha_level = .05,
                           beta_level = NULL)$main_results$power[1]/100, 
                         tolerance = .015)
            
            expect_equal(.05, 
                         ANCOVA_analytic(
                           design = "3b*3b",
                           mu = c(1, 1, 1, 1.5, 1.5, 1.5, 2, 2, 2),
                           n = 17,
                           sd = 1.78,
                           r2 = .2 ^ 2,
                           n_cov = 1,
                           alpha_level = .05,
                           beta_level = NULL
                         )$main_results$power[2]/100, 
                         tolerance = .01)
            
            expect_equal(.05, 
                         ANCOVA_analytic(                 
                           design = "3b*3b",
                           mu = c(1, 1, 1, 1.5, 1.5, 1.5, 2, 2, 2),
                           n = 17,
                           sd = 1.78,
                           r2 = .2 ^ 2,
                           n_cov = 1,
                           alpha_level = .05,
                           beta_level = NULL)$main_results$power[3]/100, 
                         tolerance = .01)
            
          })

test_that("match simulations - three way", {
  
  expect_equal(.4588, 
               ANCOVA_analytic(design = "2b*2b*2b",
                               mu = c(1,1,1,1,0,0,0,0),
                               n = 10,
                               sd = 2.5,
                               r2 = .33^2,
                               n_cov = 1,
                               alpha_level = .05,
                               beta_level = NULL)$main_results$power[1]/100, 
               tolerance = .01)
  
  expect_equal(80, 
               ANCOVA_analytic(design = "2b*2b*2b",
                               mu = c(1,1,1,1,0,0,0,0),
                               #n = 10,
                               sd = 2.5,
                               r2 = .33^2,
                               n_cov = 1,
                               alpha_level = .05,
                               beta_level = 1-.4588)$main_results$N_tot[1])
  
  expect_equal(.33^2, 
               ANCOVA_analytic(design = "2b*2b*2b",
                               mu = c(1,1,1,1,0,0,0,0),
                               n = 10,
                               sd = 2.5,
                               #r2 = .33^2,
                               n_cov = 1,
                               alpha_level = .05,
                               beta_level = 1-.4588)$main_results$r2[1],
               tolerance = .0001)
  
  expect_equal(.05, 
               ANCOVA_analytic(design = "2b*2b*2b",
                               mu = c(1,1,1,1,0,0,0,0),
                               n = 10,
                               sd = 2.5,
                               r2 = .33^2,
                               n_cov = 1,
                               alpha_level = .05,
                               beta_level = NULL)$main_results$power[2]/100, 
               tolerance = .01)
  
  expect_equal(.05, 
               ANCOVA_analytic(design = "2b*2b*2b",
                               mu = c(1,1,1,1,0,0,0,0),
                               n = 10,
                               sd = 2.5,
                               r2 = .33^2,
                               n_cov = 1,
                               alpha_level = .05,
                               beta_level = NULL)$main_results$power[7]/100, 
               tolerance = .01)
  
  
})


test_that("contrasts",
          {
            res1 = ANCOVA_analytic(design = "2b",
                            mu = c(0,1),
                            n = 15,
                            cmats = list(test = matrix(c(-1,1),
                                                          nrow = 1)),
                            label_list = list(a = c(1,2)),
                            sd = 1,
                            r2 = .2,
                            n_cov = 1)
            
            res2 = ANCOVA_contrast(cmat = c(-1,1),
                                   n = 15,
                                   mu = c(0,1),
                                   sd = 1,
                                   r2 = .2,
                                   n_cov = 1)
            expect_equal(res1$con_list$test$power,res2$power)
            
            res3 = ANCOVA_analytic(design = "2b",
                                   mu = c(0,1),
                                   n = 15,
                                   cmats =  list(test = matrix(c(-1,1),
                                                                 nrow = 1)),
                                   label_list = list(a = c(1,2)),
                                   alpha_level = NULL,
                                   beta_level = res2$beta_level,
                                   sd = 1,
                                   r2 = .2,
                                   n_cov = 1)
            
            res4 = ANCOVA_contrast(cmat = c(-1,1),
                                   n = 15,
                                   mu = c(0,1),
                                   sd = 1,
                                   r2 = .2,
                                   n_cov = 1,
                                   alpha_level = NULL,
                                   res2$beta_level)
            expect_equal(res3$con_list$test$n, res1$con_list$test$n)
            
            expect_equal(res4$n, res2$n)
            
            res5 = ANCOVA_analytic(design = "2b",
                                   mu = c(0,1),
                                   #n = 15,
                                   cmats =  list(test = matrix(c(-1,1),
                                                                 nrow = 1)),
                                   label_list = list(a = c(1,2)),
                                   alpha_level = .05,
                                   beta_level = res2$beta_level,
                                   sd = 1,
                                   r2 = .2,
                                   n_cov = 1)
            
            res6 = ANCOVA_contrast(cmat = c(-1,1),
                                   #n = 15,
                                   mu = c(0,1),
                                   sd = 1,
                                   r2 = .2,
                                   n_cov = 1,
                                   alpha_level = .05,
                                   beta_level = res2$beta_level)
            
            expect_equal(res6$N,30)
            expect_equal(res5$con_list$test$N, 30)
            
            res6 = ANCOVA_analytic(design = "2b",
                                   mu = c(0,1),
                                   n = 15,
                                   cmats =  list(test = matrix(c(-1,1),
                                                                 nrow = 1)),
                                   label_list = list(a = c(1,2)),
                                   alpha_level = .05,
                                   beta_level = res2$beta_level,
                                   sd = 1,
                                   r2 = NULL,
                                   n_cov = 1)
            
            res7 = ANCOVA_contrast(cmat = c(-1,1),
                                   n = 15,
                                   mu = c(0,1),
                                   sd = 1,
                                   r2 = NULL,
                                   n_cov = 1,
                                   alpha_level = .05,
                                   beta_level = res2$beta_level)
            
            expect_equal(res7$r2, .2)
            expect_equal(res6$con_list$test$r2, .2,tolerance = .00001)
            
            res8 = ANCOVA_contrast(cmat = c(-1,1),
                                   n = 15,
                                   mu = c(0,1),
                                   sd = 1,
                                   r2 = NULL,
                                   n_cov = 2,
                                   alpha_level = .05,
                                   beta_level = res2$beta_level)
            
            
          })
