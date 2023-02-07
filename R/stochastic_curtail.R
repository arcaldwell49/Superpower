

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
                       n1k, # interim
                       n2k,
                       n1, # final
                       n2,
                       alpha = .05) { 
  
  z_k = qnorm(pval,lower.tail = FALSE)
  i_k = ((1/n1k) + (1/n2k))^(-1) # can be just reduced to fractions (I think)
  i_f = ((1/n1) + (1/n2))^(-1)
  
  test1 = (abs(z_k) * sqrt(i_f) - qnorm(1-alpha/2) * sqrt(i_k) ) / (sqrt(i_f - i_k))
  
  test2 = (-1*abs(z_k) * sqrt(i_f) - qnorm(1-alpha/2) * sqrt(i_k) ) / (sqrt(i_f - i_k))
  
  pnorm(test1) + pnorm(test2)
}


pf_curtail = function(z_interim,
                      i_interim,
                      i_final = 1,
                      z_type  = c("pvalue","zvalue"),
                      alpha_level = Superpower_options("alpha_level")) { 
  z_type = match.arg(z_type)
  
  if(z_type == "pvalue"){
    z_interim = p_to_z(z_interim)
    if(z_interim >= 1 || z_interim <=0 ){
      stop("For p-value input z_interim must be less than 1 and greater than zero.")
    }
  }
  z_final = p_to_z(alpha_level)
  i_k = i_interim
  i_f = i_final
  #z_k = qnorm(pval,lower.tail = FALSE)
  #i_k = ((1/n1k) + (1/n2k))^(-1) # can be just reduced to fractions (I think)
  #i_f = ((1/n1) + (1/n2))^(-1)
  
  test1 = (abs(z_interim) * sqrt(i_f) - z_final * sqrt(i_k) ) / (sqrt(i_f - i_k))
  
  test2 = (-1*abs(z_interim) * sqrt(i_f) - z_final * sqrt(i_k) ) / (sqrt(i_f - i_k))
  
  res = pnorm(test1) + pnorm(test2)

  METHOD <- "Parameter Free Predictive Power"
  structure(
    list(
      z_interim = z_interim,
      z_final = z_final,
      i_interim = i_k,
      i_final = i_final,
      power = res*100,
      alpha_level = alpha_level,
      method = METHOD
    ),
    class = "power.htest"
  )
}

p_to_z = function(x){
  return(-1*qnorm((x/2),FALSE))
}

z_to_p = function(x){
  return(2*pnorm(-abs(x)))
}
z_to_p(p_to_z(.01))
p_to_z(.0133616)
library(Superpower)
pf_curtail(z_interim = .5,i_interim = 25,
           i_final = 50,
           z_type = "p")
pred_pow_2s(.055,
            25,25,
            50,50)
