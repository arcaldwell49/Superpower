ANCOVA_approx <- function (design_result, # Vector of Group Means
                          n_cov, # Number of Covariates
                          r2, # Coefficient of Determination
                          alpha_level # Alpha level
) {
  mu = design_result$mu
  sd = design_result$sd
  n = design_result$n
  n_grp = length(mu)
  mu = mu
  if(length(n==1)){
    nvec = rep(n,length(mu))
  } else {nvec = n}
  
  var_e = (sd^2*(1-r2))
  frml3 = as.formula(paste(gsub("\\+", "*",design_result$frml2),collapse=" "))
  
  if(length(design_result$factornames) == 1){
    con_df = data.frame(a = 1:length(design_result$labelnameslist[[1]]))
    con_df$a = as.factor(con_df$a)
    contrasts(con_df$a) = "contr.sum"
    con_mat = model.matrix(frml3,con_df)
    colnames(con_mat) = attr(con_mat, "assign")
    cmat_a = con_mat[,which(colnames(con_mat)==1)]
    cmats = list(a = cmat_a)
  } else if(length(design_result$factornames) == 2){
    con_df = as.data.frame(expand.grid(a = 1:length(design_result$labelnameslist[[1]]),
                                       b = 1:length(design_result$labelnameslist[[2]])))
    con_df$a = as.factor(con_df$a)
    con_df$b = as.factor(con_df$b)
    contrasts(con_df$a) = "contr.sum"
    contrasts(con_df$b) = "contr.sum"
    #colnames(con_df) = design_result$factornames
    con_mat = model.matrix(~a*b,con_df)
    colnames(con_mat) = attr(con_mat, "assign")
    cmat_a = t(as.matrix(con_mat[,which(colnames(con_mat)==1)]))
    cmat_b = t(as.matrix(con_mat[,which(colnames(con_mat)==2)]))
    cmat_ab = t(as.matrix(con_mat[,which(colnames(con_mat)==3)]))
    cmats = list(a = cmat_a, b = cmat_b, ab=cmat_ab)
  } else {
    con_df = as.data.frame(expand.grid(a = 1:length(design_result$labelnameslist[[1]]),
                                       b = 1:length(design_result$labelnameslist[[2]]),
                                       c = 1:length(design_result$labelnameslist[[3]])))
    con_df$a = as.factor(con_df$a)
    con_df$b = as.factor(con_df$b)
    con_df$c = as.factor(con_df$c)
    contrasts(con_df$a) = "contr.sum"
    contrasts(con_df$b) = "contr.sum"
    contrasts(con_df$c) = "contr.sum"
    #colnames(con_df) = design_result$factornames
    con_mat = model.matrix(~a*b*c,con_df)
    colnames(con_mat) = attr(con_mat, "assign")
    cmat_a = t(as.matrix(con_mat[,which(colnames(con_mat)==1)]))
    cmat_b = t(as.matrix(con_mat[,which(colnames(con_mat)==2)]))
    cmat_c = t(as.matrix(con_mat[,which(colnames(con_mat)==3)]))
    cmat_ab = t(as.matrix(con_mat[,which(colnames(con_mat)==4)]))
    cmat_ac = t(as.matrix(con_mat[,which(colnames(con_mat)==5)]))
    cmat_bc = t(as.matrix(con_mat[,which(colnames(con_mat)==6)]))
    cmat_abc = t(as.matrix(con_mat[,which(colnames(con_mat)==7)]))
    cmats = list(a = cmat_a, b = cmat_b, c = cmat_c,
                 ab = cmat_ab, ac = cmat_ac, bc = cmat_bc,
                 abc = cmat_abc)
  }
  #cmat = t(contr.sum(length(nvec))) # Contrast matrix
  # Create matrix for Wald statistic (W star; eq 10 from Shieh)
  
  
  
  
  
}