model_matrix_from_design <- function(x) {
  design <- expand.grid(x)
  design_fomula <- as.formula(paste0("~", paste(names(design), collapse = "*")))
  
  design_contrasts <- lapply(x, function(x) "contr.sum")
  design_contrasts <- model.matrix(
    design_fomula
    , data = design
    , contrasts = design_contrasts
  )[, -1]
  
  colnames(design_contrasts) <- attr(terms(design_fomula), "term.labels")
  rownames(design_contrasts) <- apply(
    design
    , 1
    , function(x) paste(paste0(names(x), x), collapse = "_")
  )
  
  design_contrasts
}




mu_from_design <- function(x, f) {
  model_matrix <- model_matrix_from_design(x)
  
  model_matrix_subset <- model_matrix[, names(f)]
  mu <- t(t(model_matrix_subset) * unlist(f))
  
  rowSums(mu)
}

### Testing

f <- 0.1481313

mu_from_design(
  x = list(a = c("1", "2"), b = c("1", "2"))
  , f = list("a:b" = f)
)

mu_from_design(
  x = list(a = c("1", "2"), b = c("1", "2"))
  , f = list("b" = f, "a:b" = f)
)

design <- list(a = c("1", "2"), b = c("1", "2"))
design_string <- "2w*2b"

f <- list("b" = 0.1481313, "a:b" = 0.1481313 / 2)
n <- 75
r <- 0.5
alpha_level <- 0.05

mu <- mu_from_design(
  x = design
  , f = f
)

labelnames <- as.vector(
  rbind(
    names(design)
    , sapply(design, function(x) c(names(x), x))
  )
)

power_design <- ANOVA_design(
  design = design_string
  , n = n
  , mu = as.numeric(mu)
  , sd = 1
  , r = r,
  plot = TRUE
)

power <- ANOVA_exact(
  power_design
  , alpha_level = alpha_level
  , verbose = FALSE
  , emm = TRUE
)

power$main_results