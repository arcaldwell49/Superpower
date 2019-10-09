# Function to create data frame of MANOVA results


Anova_mlm_table <- function(x, ...)
{
  test <- x$test
  repeated <- x$repeated
  ntests <- length(x$terms)
  tests <- matrix(NA, ntests, 4)
  if (!repeated)
    SSPE.qr <- qr(x$SSPE)
  for (term in 1:ntests) {
    eigs <- Re(eigen(qr.coef(if (repeated)
      qr(x$SSPE[[term]])
      else
        SSPE.qr,
      x$SSP[[term]]), symmetric = FALSE)$values)
    tests[term, 1:4] <- switch(
      test,
      Pillai = Pillai(eigs,
                      x$df[term], x$error.df),
      Wilks = Wilks(eigs,
                    x$df[term], x$error.df),
      `Hotelling-Lawley` =
        HL(eigs,
           x$df[term], x$error.df),
      Roy = Roy(eigs,
                x$df[term], x$error.df)
    )
  }
  ok <- tests[, 2] >= 0 & tests[, 3] > 0 & tests[, 4] > 0
  ok <- !is.na(ok) & ok
  tests <- cbind(x$df, tests, pf(tests[ok, 2], tests[ok, 3],
                                 tests[ok, 4], lower.tail = FALSE))
  rownames(tests) <- x$terms
  colnames(tests) <- c("df", "test_stat", "approx_F", "num_Df",
                       "den_Df", "p.value")
  tests <- structure(as.data.frame(tests), heading = paste("\nType ",
                                                           x$type, if (repeated)
                                                             " Repeated Measures", " MANOVA Tests: ", test, " test
                                                           statistic",
                                                           sep = ""), class = c("anova", "data.frame"))
  invisible(tests)
}
