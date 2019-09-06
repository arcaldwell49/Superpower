library(afex)
library(MASS)
library(ggplot2)
library(reshape2)
library(ANOVApower)

devtools::install_github("Lakens/ANOVApower")
devtools::build_vignettes()
devtools::document()
devtools::build_manual()


design = "2b*2w"
n = 40
mu = c(1, 0, 1, 0)
sd = 2
r = 0.8
labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot")

design_result <- ANOVA_design(design = "2w*2w", n = 40, mu = c(1, 0, 0, 0.5), sd = 2, r = 0.8, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
design_result$cor_mat

power_result <- ANOVA_power(design_result, alpha_level = 0.05,
                            p_adjust = "none", nsims = 1000)

power_result$plot1

design_result <- ANOVA_design(design = "2w*2b", n = 40, mu = c(0, 0, 0, 0), sd = 2, r = 0.8, labelnames = c("condition", "cheerful", "sad", "voice", "human", "robot"))
design_result$cor_mat

power_result <- ANOVA_power(design_result, alpha_level = 0.05,
                            p_adjust = "none", nsims = 1000)

power_result$plot1

design_result_new$frml1 == design_result_old$frml1
design_result_new$frml2 == design_result_old$frml2

design_result_new$cor_mat == design_result_old$cor_mat

xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
for(j in 1:factors){
  xxx <- cbind(xxx, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                       sep="_")),
                                         each = prod(factor_levels)/prod(factor_levels[1:j]),
                                         times = prod(factor_levels)/prod(factor_levels[j:factors])
  ))))
}
xxx$cond <- as.character(interaction(xxx[, 1:factors], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)


xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
for(j in 1:factors){
  xxx <- cbind(xxx, labelnameslist[[j]])
}
xxx$cond <- as.character(interaction(xxx[, 1:factors], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)

library(ANOVApower)
#test twoway within function
design_result <- ANOVA_design(design = "2w*2w", n = 20, mu = c(1, 0, 0, 1), sd = 2, r = 0.0)
power_res <- power_2x2_within_2(design_result)
power_res <- power_2x2_within(design_result)
power_res$mean_mat
power_res$power_A
power_res$power_B
power_res$power_AB
power_result <- ANOVA_power(design_result, alpha_level = 0.05,
                            p_adjust = "none", nsims = 1000)







mu = c(2,1,4,2)
n <- 20
sd <- 5
r <- 0.8

string = "2w*2w"
labelnames = c("A", "a1", "a2", "B", "b1", "b2")
design_result <- ANOVA_design(string = string,
                              n = n,
                              mu = mu,
                              sd = sd,
                              r = r,
                              labelnames = labelnames)
simulation_result <- ANOVA_power(design_result, nsims = 1000)

power_res <- power_2x2_within(design_result = design_result)
power_res$power_A
power_res$power_B
power_res$power_AB

power_res <- power_twoway_within(design_result = design_result)
power_res$power_A
power_res$power_B
power_res$power_AB

design_result <- ANOVA_design(string = "2w*2w", n = 20, mu = c(1, 0, 0, 0), sd = 2, r = 0.6)
power_res <- power_twoway_within_2(design_result)
power_res$mean_mat
power_res$power_A
power_res$power_B
power_res$power_AB
power_result <- ANOVA_power(design_result, alpha_level = 0.05, p_adjust = "none", nsims = 1000)

ANOVA_exact(design_result)

library(ANOVApower)
design_result <- ANOVA_design(design = "2w*2w", n = 20, mu = c(1, 0, 0, 0), sd = 2, r = 0.6)
ANOVA_exact(design_result)
power_res <- power_2x2_within(design_result)
power_res$power_A
power_res$power_B
power_res$power_AB
set.seed(2019)
ANOVA_power(design_result, alpha_level = 0.05, p_adjust = "none", nsims = 100)
ANOVA_power(design_result, alpha_level = 0.05, p_adjust = "none", nsims = 100, seed = 2019)


