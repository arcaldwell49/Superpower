context("test-power_2x2_within")

# error messages
test_that("error messages", {


  expect_error(power_2x2_within(), "argument \"design_result\" is missing, with no default" )

  design_result1 <- ANOVA_design(design = "2b*2b*2w",
                                n = 100,
                                mu = c(24, 26.2, 27, 28,
                                       24, 26.2, 27, 28),
                                sd = 6.4,
                                plot = FALSE)



  expect_error(power_2x2_within(design_result1), "Only 2x2 within designs allowed for this function")

  design_result2 <- ANOVA_design(design = "2w*2b",
                                n = 100,
                                mu = c(24, 26.2, 27, 28),
                                sd = 6.4,
                                r=0.5,
                                plot = FALSE)

  expect_error(power_2x2_within(design_result2), "Only 2x2 within designs allowed for this function")

})

#Function check ###function not working yet
#test_that("2x2 design", {

  #From ANOVA_power_simulation validation file https://github.com/Lakens/ANOVA_power_simulation/blob/master/validation_files/3.2_validation_power_within_within_2x2_Amsel.md

 # design_result <- ANOVA_design(design = "2w*2w",
  #                              n = 25,
   #                             mu = c(700, 670, 670, 700),
    #                            sd = 150,
     #                           r = 0.75)


 # power <- power_2x2_within(design_result)



 # expect_equal(c(power$power_A,power$power_A,power$power_AB), c(.05,.05,.49), tolerance = .01)
#
#})
