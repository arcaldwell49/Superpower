Factorial ANCOVA Validation
================

``` r
knitr::opts_chunk$set(echo = TRUE)
library(simstudy)
library(data.table)
library(Superpower)
```

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

``` r
library(car)
```

    ## Loading required package: carData

``` r
library(testthat)
set.seed(61036)
```

# Simulation 1

``` r
nsim = 5000
res_group = vector()
res_aov = vector()

for(i in 1:nsim){
  # Generate outcome and covariate matrix
  dt <- genCorData(
    21 * 3,
    mu = c(0, 400),
    sigma = 100,
    rho = .1,
    corstr = "cs",
    cnames = c("cov", "y1")
  )

# generate random treatment assignment
study1 = trtAssign(dt, 
                   n = 3, 
                   balanced = TRUE,
                   grpName = "group")

# add treatment effect
study1[, y := ifelse(group == 3, y1+100, 
                     ifelse(group == 2, y1+50,
                            y1))]
# factorize group
study1[, grp := as.factor(group)]

a = as.data.frame(Anova(lm(y ~ cov + grp, data = study1),
                        type ='III'))

res_group = append(a["grp",]$`Pr(>F)`,
                   res_group)

a2 = as.data.frame(Anova(lm(y ~ grp, data = study1),
                         type ='III'))

res_aov = append(a2["grp",]$`Pr(>F)`, 
                 res_aov)

}
```

``` r
test_that("Simulation 1", {
  # Both should be about .81
  # Check ANCOVA result
  expect_equal(mean(res_group < .05), 
               ANCOVA_analytic(design = "3b",
                mu = c(400,450,500),
                n = 21,
                sd = 100,
                r2 = .1^2,
                n_cov = 1,
                alpha_level = .05,
                beta_level = NULL)$main_result$power/100, 
               tolerance = .01)
  # Check ANOVA result
  expect_equal(mean(res_aov < .05), .8148,
               tolerance = .01) 
})
```

    ## Test passed 🎉

# Simulation 2

``` r
res_group2 = vector()

for(i in 1:nsim){
  # Generate outcome and covariate matrix
  dt <- genCorData(
    48,
    mu = c(0, 400),
    sigma = 100,
    rho = .5,
    corstr = "cs",
    cnames = c("cov", "y1")
  )
  
  # generate random treatment assignment
  study1 = trtAssign(dt, 
                     n = 3, 
                     balanced = TRUE,
                     grpName = "group")
  
  # add treatment effect
  study1[, y := ifelse(group == 3, y1+100, 
                       ifelse(group == 2, y1+50,
                              y1))]
  # factorize group
  study1[, grp := as.factor(group)]
  
  a = as.data.frame(Anova(lm(y ~ cov + grp, data = study1),
                          type ='III'))
  
  res_group2 = append(a["grp",]$`Pr(>F)`,
                     res_group2)
  
}
```

``` r
test_that("Simulation 2",{
  # Again should be ~80%
  # Check ANCOVA result
    expect_equal(mean(res_group2 < .05), 
               ANCOVA_analytic(design = "3b",
                mu = c(400,450,500),
                n = 48/3,
                sd = 100,
                r2 = .5^2,
                n_cov = 1,
                alpha_level = .05,
                beta_level = NULL)$main_result$power/100, 
               tolerance = .01)
  
})
```

    ## Test passed 😀

# Simulation 3

``` r
res_group3 = vector()

for(i in 1:nsim){
  # Generate outcome and covariate matrix
  dt <- genCorData(
    18,
    mu = c(0, 400),
    sigma = 100,
    rho = .9,
    corstr = "cs",
    cnames = c("cov", "y1")
  )
  
  # generate random treatment assignment
  study1 = trtAssign(dt, 
                     n = 3, 
                     balanced = TRUE,
                     grpName = "group")
  
  # add treatment effect
  study1[, y := ifelse(group == 3, y1+100, 
                       ifelse(group == 2, y1+50,
                              y1))]
  # factorize group
  study1[, grp := as.factor(group)]
  
  a = as.data.frame(Anova(lm(y ~ cov + grp, data = study1),
                          type ='III'))
  
  res_group3 = append(a["grp",]$`Pr(>F)`,
                     res_group3)
  
}
```

``` r
test_that("Simulation 3",{
  # Again should be ~80%
  # Check ANCOVA result
    expect_equal(mean(res_group3 < .05), 
               ANCOVA_analytic(design = "3b",
                mu = c(400,450,500),
                n = 6,
                sd = 100,
                r2 = .9^2,
                n_cov = 1,
                alpha_level = .05,
                beta_level = NULL)$main_result$power/100, 
               tolerance = .01)
  
})
```

    ## Test passed 🌈
