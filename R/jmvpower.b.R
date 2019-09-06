
jmvpowerClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "jmvpowerClass",
  inherit = jmvpowerBase,
  private = list(
    .init = function() {

      design <- self$options$design
      factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
      n_factors <- length(factor_levels)
      ncells <- cumprod(factor_levels)
      ncells <- ncells[length(ncells)]

      dt <- self$results$design$summary

      afmc <- list(
        `none`="None",
        `holm`="Holm-Bonferroni",
        `bonferroni` = "bonferroni",
        `fdr`="False Discovery Rate")[[self$options$p_adjust]]

      dt$setRow(rowNo=1, values=list(name='Design', value=design))
      dt$setRow(rowNo=2, values=list(name='Formula', value='.'))
      dt$setRow(rowNo=3, values=list(name='Sample size per cell', value=self$options$n))
      dt$setRow(rowNo=4, values=list(name='Adjustment for multiple corrections', value=afmc))

      mt <- self$results$design$matrix
      for (i in seq_len(ncells))
        mt$addColumn(name = paste(i), title = '')
      for (i in seq_len(ncells))
        mt$addRow(rowKey=i)

      rt <- self$results$sims$results
      ut <- self$results$sims$multi

      formula <- as.formula(paste('~', paste(paste0('`', seq_len(n_factors), '`'), collapse='*')))
      terms   <- attr(stats::terms(formula), 'term.labels')

      for (term in terms)
        rt$addRow(rowKey=term)

      n_pc <- (((prod(
        as.numeric(strsplit(design, "\\D+")[[1]])
      )) ^ 2) - prod(as.numeric(strsplit(design, "\\D+")[[1]])))/2

      for (i in seq_len(n_pc))
        ut$addRow(rowKey=i)

    },
    .run = function() {

      mu <- self$options$mu
      labelnames <- self$options$labelnames

      mu <- as.numeric(strsplit(mu, ',')[[1]])

      if (labelnames == '')
        labelnames <- NULL
      else
        labelnames <- trimws(strsplit(labelnames, ',')[[1]])

      design <- ANOVA_design(
        design = self$options$design,
        n = self$options$n,
        mu = mu,
        sd = self$options$sd,
        r = self$options$r,
        labelnames = labelnames,
        plot = FALSE)

      # self$results$text$setContent(design)

      formula <- paste(as.character(design$frml1)[2:3], collapse=' ~ ')
      dt <- self$results$design$summary
      dt$setRow(rowNo=2, values=list(value=formula))

      mt <- self$results$design$matrix
      for (i in seq_len(ncol(design$cor_mat))) {
        mt$addColumn(
          name = paste(i),
          title = ''
        )
      }

      for (i in seq_len(nrow(design$cor_mat))) {
        values <- design$cor_mat[i,]
        names(values) <- seq_len(ncol(design$cor_mat))
        mt$setRow(rowKey=i, values=values)
      }

      pt <- self$results$design$plot
      if (self$options$plot && pt$isNotFilled())
        pt$setState(design$meansplot)

      private$.checkpoint()

      if ( ! self$options$simulate)
        return()

      rt <- self$results$sims$results
      ut <- self$results$sims$multi

      if (rt$isFilled())
        return()

      results <- ANOVA_power(
        design,
        alpha_level = self$options$alpha_level,
        p_adjust = self$options$p_adjust,
        nsims = self$options$nsims)

      rt <- self$results$sims$results
      ut <- self$results$sims$multi

      main <- results$main_results
      names <- row.names(main)
      for (i in seq_len(nrow(main))) {
        rt$setRow(rowNo=i, values=list(
          name=names[i],
          power=main[i,1],
          es=main[i,2]
        ))
      }

      pc <- results$pc_results
      names <- row.names(pc)
      for (i in seq_len(nrow(pc))) {
        ut$setRow(rowNo=i, values=list(
          name=names[i],
          power=pc[i,1],
          es=pc[i,2]
        ))
      }

      # self$results$text$setContent(results)
    },
    .plot=function(image, ...) {
      image$state
    })
)
