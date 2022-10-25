

con_pow_2s = function(pval,
                      theta = 0.33,
                      n1k,
                      n2k,
                      n1,
                      n2,
                      alpha = .05) {
  
  z_k = qnorm(pval,lower.tail = FALSE)
  i_k = ((1/n1k) + (1/n2k))^(-1)
  i_f = ((1/n1) + (1/n2))^(-1)
  test1 = ((z_k) * sqrt(i_k) - qnorm(1-alpha/2) * sqrt(i_f) + theta * (i_f - i_k)) / (sqrt(i_f - i_k))
  
  test2 = (-1*(z_k) * sqrt(i_k) - qnorm(1-alpha/2) * sqrt(i_f) - theta * (i_f - i_k)) / (sqrt(i_f - i_k))
  
  pnorm(test1) + pnorm(test2)
}


pred_pow_2s = function(pval,
                       n1k,
                       n2k,
                       n1,
                       n2,
                       alpha = .05) { 
  
  z_k = qnorm(pval,lower.tail = FALSE)
  i_k = ((1/n1k) + (1/n2k))^(-1)
  i_f = ((1/n1) + (1/n2))^(-1)
  
  test1 = (abs(z_k) * sqrt(i_f) - qnorm(1-alpha/2) * sqrt(i_k) ) / (sqrt(i_f - i_k))
  
  test2 = (-1*abs(z_k) * sqrt(i_f) - qnorm(1-alpha/2) * sqrt(i_k) ) / (sqrt(i_f - i_k))
  
  pnorm(test1) + pnorm(test2)
}


