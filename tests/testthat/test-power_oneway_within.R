context("test-power_oneway_within")

# error messages
test_that("error messages", {


  expect_error(power_oneway_within(), "argument \"design_result\" is missing, with no default" )

})

#Function check
test_that("2w and 3w", {
  K <- 2
  n <- 34
  sd <- 1
  r <- 0.5
  alpha = 0.05
  f <- 0.25
  f2 <- f^2
  ES <- f2/(f2+1)

  mu <- mu_from_ES(K = K, ES = ES)

  design = paste(K,"w",sep="")

  design_result1 <- ANOVA_design(design = design,
                                 n = n,
                                 mu = mu,
                                 sd = sd,
                                 r = r, plot = FALSE)

  expect_equal(power_oneway_within(design_result1, alpha_level = 0.05)$power,
               pwr::pwr.t.test(d =  0.5,
                               n = 34,
                               sig.level = 0.05,
                               type = "paired",
                               alternative = "two.sided")$power*100,
               tolerance = .001) #example from validation files

  K <- 3
  n <- 20
  sd <- 1
  r <- 0.8
  f <- 0.25
  f2 <- f^2
  ES <- f2 / (f2 + 1)
  mu <- mu_from_ES(K = K, ES = ES)
  design = paste(K,"w",sep = "")

  design_result2 <- ANOVA_design(design = design,
                                 n = n,
                                 mu = mu,
                                 sd = sd,
                                 r = r, plot = FALSE)

  #Formula used by G*Power for within design
  f <- 0.25 #Cohen's f
  k <- 1 #number of groups
  m <- 3 #number of measures
  n <- 20 #total sample size
  e <- 1 #non-spericity correction
  r <- 0.8 #correlation between dependent variables
  alpha <- 0.05 #alpha level

  df1 <- (m - 1) * e #calculate degrees of freedom 1
  df2 <- (n - k) * (m - 1) * e #calculate degrees of freedom 2

  lambda <- (n * m * f^2) / (1 - r) # lambda for within ANOVA

  F_critical <- qf(alpha, # critical F-vaue
                   df1,
                   df2,
                   lower.tail = FALSE)

  pow <- pf(qf(alpha, #power
               df1,
               df2,
               lower.tail = FALSE),
            df1,
            df2,
            lambda,
            lower.tail = FALSE)


  expect_equal(power_oneway_within(design_result2, alpha_level = 0.05)$power,
               pow*100,
               tolerance = .01) #example from validation files
})
