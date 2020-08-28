context("test-mu_from_ES")

# error messages
test_that("error messages", {

  expect_error(mu_from_ES(K=11, ES=0.05), "Number of levels (k) must be 2, 3, or 4", fixed = TRUE)
  expect_error(mu_from_ES(K=3, ES=5), "the ES (partial eta squared) must be less than 1 and greater than zero", fixed = TRUE)
  expect_error(mu_from_ES(), "argument \"ES\" is missing, with no default" )
  
  expect_error(mu_from_ES(K=3, ES=1.1))

})

#Function check
test_that("2b and 3b", {
  expect_equal(mu_from_ES(K=2, ES=0.0503911)*6.4, c(-1.474295, 1.474295), tolerance = .001) #example from validation files
  expect_equal(mu_from_ES(K=3, ES=0.07928127)*6.4, c(-2.300104, 0.00000, 2.300104), tolerance = .001) #example from validation files
  expect_equal(mu_from_ES(K=4, ES=.5),c(-1,-1,1,1))
})
