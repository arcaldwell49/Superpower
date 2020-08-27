# Effect sizes
# Changed conf.level to alpha_level

effect_size_d <- function(x, y, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  n1 <- length(x) #number of pairs
  n2 <- length(y) #number of pairs
  df <- n1 + n2 - 2
  m_diff <- mean(y) - mean(x)
  sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 - 2)))) #pooled standard deviation
  j <- (1 - 3/(4 * (n1 + n2 - 2) - 1)) #Calculate Hedges' correction.
  t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
  p_value = 2*pt(-abs(t_value), df = df)
  d <- m_diff / sd_pooled #Cohen's d
  d_unb <- d*j #Hedges g, of unbiased d
  
  invisible(list(d = d,
                 d_unb = d_unb,
                 p_value = p_value))
}

effect_size_d_paired <- function(x, y, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(x - y) #standard deviation of the difference scores
  N <- length(x) #number of pairs
  df = N - 1
  s_av <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2) #averaged standard deviation of both measurements
  
  #Cohen's d_av, using s_av as standardizer
  m_diff <- mean(y - x)
  d_av <- m_diff / s_av
  d_av_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_av
  
  #get the t-value for the CI
  t_value <- m_diff / (s_diff / sqrt(N))
  p_value = 2 * pt(-abs(t_value), df = df)
  
  #Cohen's d_z, using s_diff as standardizer
  d_z <- t_value / sqrt(N)
  d_z_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_z
  
  invisible(list(
    d_z = d_z,
    d_z_unb = d_z_unb,
    p_value = p_value
  ))
}

effect_size_d_exact <- function(x, y, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  n1 <- length(x) #number of pairs
  n2 <- length(y) #number of pairs
  df <- n1 + n2 - 2
  m_diff <- mean(y) - mean(x)
  sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 - 2)))) #pooled standard deviation
  j <- (1 - 3/(4 * (n1 + n2 - 2) - 1))  #Calculate Hedges' correction.
  t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
  p_value = 2*pt(-abs(t_value), df = df)
  #Calculate power
  power = power.t.test(
    n = n1,
    delta = m_diff,
    sd = sd_pooled,
    type = "two.sample",
    alternative = "two.sided",
    strict = TRUE,
    sig.level = alpha_level
  )$power
  
  d <- m_diff / sd_pooled #Cohen's d
  d_unb <- d*j #Hedges g, of unbiased d
  
  invisible(list(d = d,
                 d_unb = d_unb,
                 p_value = p_value,
                 power = power))
}

effect_size_d_paired_exact <- function(x, y, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(x - y) #standard deviation of the difference scores
  N <- length(x) #number of pairs
  df = N - 1
  s_av <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2) #averaged standard deviation of both measurements
  
  #Cohen's d_av, using s_av as standardizer
  m_diff <- mean(y - x)
  d_av <- m_diff / s_av
  d_av_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_av
  
  #get the t-value for the CI
  t_value <- m_diff / (s_diff / sqrt(N))
  p_value = 2 * pt(-abs(t_value), df = df)
  
  power = power.t.test(
    n = N,
    delta = m_diff,
    sd = s_diff,
    type = "paired",
    alternative = "two.sided",
    strict = TRUE,
    sig.level = alpha_level
  )$power
  
  #Cohen's d_z, using s_diff as standardizer
  d_z <- t_value / sqrt(N)
  d_z_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_z
  
  invisible(list(
    d_z = d_z,
    d_z_unb = d_z_unb,
    p_value = p_value,
    power = power
  ))
}

effect_size_d_exact2 <- function(x, y, sample_size, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  n1 <- sample_size
  n2 <- sample_size
  df <- n1 + n2 - 2
  m_diff <- mean(y) - mean(x)
  sd_pooled <- (sqrt((((n1 - 1) * ((sd1^2))) + (n2 - 1) * ((sd2^2))) / ((n1 + n2 - 2)))) #pooled standard deviation
  j <- (1 - 3/(4 * (n1 + n2 - 2) - 1))  #Calculate Hedges' correction.
  t_value <- m_diff / sqrt(sd_pooled^2 / n1 + sd_pooled^2 / n2)
  p_value = 2*pt(-abs(t_value), df = df)
  #Calculate power
  power = power.t.test(
    n = n1,
    delta = m_diff,
    sd = sd_pooled,
    type = "two.sample",
    alternative = "two.sided",
    strict = TRUE,
    sig.level = alpha_level
  )$power
  
  d <- m_diff / sd_pooled #Cohen's d
  d_unb <- d*j #Hedges g, of unbiased d
  
  invisible(list(d = d,
                 d_unb = d_unb,
                 p_value = p_value,
                 power = power))
}

effect_size_d_paired_exact2 <- function(x, y, sample_size, alpha_level){
  sd1 <- sd(x) #standard deviation of measurement 1
  sd2 <- sd(y) #standard deviation of measurement 2
  s_diff <- sd(x - y) #standard deviation of the difference scores
  N <- sample_size
  df = N - 1
  s_av <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2) #averaged standard deviation of both measurements
  
  #Cohen's d_av, using s_av as standardizer
  m_diff <- mean(y - x)
  d_av <- m_diff / s_av
  d_av_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_av
  
  #get the t-value for the CI
  t_value <- m_diff / (s_diff / sqrt(N))
  p_value = 2 * pt(-abs(t_value), df = df)
  
  power = power.t.test(
    n = N,
    delta = m_diff,
    sd = s_diff,
    type = "paired",
    alternative = "two.sided",
    strict = TRUE,
    sig.level = alpha_level
  )$power
  
  #Cohen's d_z, using s_diff as standardizer
  d_z <- t_value / sqrt(N)
  d_z_unb <- (1 - (3 / (4 * (N - 1) - 1))) * d_z
  
  invisible(list(
    d_z = d_z,
    d_z_unb = d_z_unb,
    p_value = p_value,
    power = power
  ))
}