
#' @importFrom stats lm confint
#' @importFrom magrittr '%>%'
#' @importFrom dplyr select mutate everything

ancova_gen <- function(post_diff, sd, r, alpha_level, conf_level, grp_n) {
  cor_mat = matrix(c(1,r,
                     r,1),
                   nrow = 2)
  
  #Need this to avoid "undefined" global error from occuring
  id <- variable <- NULL
  
  cov_mat = cor_mat*(sd)^2
  
  df_1 <- as.data.frame(mvrnorm(n = grp_n,
                                      mu = c(0, post_diff),
                                      Sigma = cov_mat))
  colnames(df_1) = c("y_pre", "y_post")
  
  df_1$id = rownames(df_1)
  df_1$id = paste0("g1_",df_1$id)
  
  df_2 <- as.data.frame(mvrnorm(n = grp_n,
                                      mu = c(0, 0),
                                      Sigma = cov_mat)) 
  colnames(df_2) = c("y_pre", "y_post") 
  
  df_2$id = rownames(df_2)
  df_2$id = paste0("g2_",df_2$id)
  
  df_sim = rbind(df_1,df_2)
  
  df_sim <- melt(df_sim, id.vars = c("id")) %>%
    mutate(group = ifelse(grepl("g1",id),"group1","group2"),
           time = ifelse(grepl("pre",variable),"pre","post"))
  
  tmp = dcast(df_sim, id+group ~ time, value.var = "value")
  tmp$diff = tmp$post-tmp$pre
  
  
  fit_lm <- lm(post ~ pre + group, tmp)
  width_ac <- diff(confint(fit_lm, level = (conf_level))[3,])[[1]]/2
  p_ac = as.numeric(summary(fit_lm)[["coefficients"]][, "Pr(>|t|)"][3])
  fit_aov <- lm(diff ~ group, tmp)
  width_aov <- diff(confint(fit_aov, level = (conf_level))[2,])[[1]]/2
  p_aov = as.numeric(summary(fit_aov)[["coefficients"]][, "Pr(>|t|)"][2])
  
  ancova_df = data.frame(width_ac = width_ac, p_ac = p_ac,
                         width_aov = width_aov, p_aov = p_aov)
} 