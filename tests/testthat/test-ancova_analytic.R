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
