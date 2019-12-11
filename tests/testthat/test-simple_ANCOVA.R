context("test-power_oneway_between")

# error messages
test_that("error messages", {
  
  
  expect_error(simple_ANCOVA(r=1.1))
  
  expect_error(simple_ANCOVA(sd=-1))
  
})

#Function check
#test_that("ttest", {
  
  #10.1016/j.jclinepi.2007.02.006
  

#})
