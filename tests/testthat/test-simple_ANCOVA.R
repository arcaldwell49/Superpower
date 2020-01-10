context("test-power_oneway_between")

# error messages
test_that("error messages", {
  
  
  expect_error(simple_ANCOVA(r=1.1))
  
  expect_error(simple_ANCOVA(sd=-1))
  
})

#Function check
test_that("PASS", {
  
  #Compared to results in PASS
  set.seed(894126547)
  ANCOVA_result = simple_ANCOVA(post_diff = .5, sd = 1, r = .65,
                                alpha_level = .15,
                                MOE = 1,
                                min_n = 10,
                                max_n = 15)
  expect_length(ANCOVA_result$sim_results$power_ANCOVA, 6)
  expect_lt(max(ANCOVA_result$sim_results$power_ANCOVA),64.5)
  expect_gt(min(ANCOVA_result$sim_results$power_ANCOVA),47.5)
  

})
