NEWS
================

Updated Tue Jul 30 2024

# Superpower 0.2.3 – “Repli-Kate”

- Small updates to documentation to meet CRAN requirements

# Superpower 0.2.2 – “Jubilee”

- Updates to the package documentation

# Superpower 0.2.1 – “Jade”

- Minor fix to the plot_power function
- Major updates to the Shiny apps
  - Changes design input to a more friendly UI

# Superpower 0.2.0 – “Green Lantern”

- Added power_oneway_ancova & power_con_ancova to allow for a basic
  power analysis of an analysis of covariance (ANCOVA) for one-way,
  between group designs.
- Added ANCOVA_analytic and ANCOVA_contrast which allow for power
  analyses for factorial designs and user specified contrasts.
- Added the label_list argument to ANOVA_design and ANCOVA_analytic
  functions. Now labels can be assigned to factors and levels in a more
  sane fashion using named lists.

# Superpower 0.1.2 – “Rex Splode”

- Minor fixes to power_standardized_alpha to keep Superpower on CRAN

# Superpower 0.1.1 – “Black Canary”

- Added morey_plot functions.
  - Plot the effect size (x-axis) at different sample sizes (facets) and
    at different alpha levels (color).
  - These plots are helpful in determining the sensitivity of
    statistical tests (t-test and F-test) across a range of effect
    sizes.
- Added confint method for ANOVA_power produced objects
  - Calculates confidence level for binomial proportion (# of results
    that are below alpha level) confidence intervals (Wilson, 1927).
- Minor changes to Shiny apps to fix glitches.

# Superpower 0.1.0

- Added ANOVA_exact2 function as an extension of ANOVA_exact
  - Now functional across all sample sizes but does not return a
    dataframe of afex aov object
- liberal_lambda argument added: allows users to specify the type of
  lambda calculations
  - When liberal_lambda = TRUE; lambda = cohen_f^2 \* (num_df + den_df +
    1)
  - When liberal_lambda = FALSE; lambda = cohen_f^2 \* den_df
- Optimal alpha functions from JustifieR package added
- ANOVA_compromise function added which allows a compromise power
  analysis to be performed for all comparisons in a design
- ANOVA_design now returns as a class “design_aov” with specific print
  and plot methods see ?`design_aov-methods`
  - generate_cor_matrix function is now a non-exported function within
    the package (no longer contained within ANOVA_design)
- All simulation functions ANOVA_power, ANOVA_exact, and ANOVA_exact2
  now returns as a class “sim_result” with specific print and plot
  methods see ?`sim_result-methods`
- plot_power now has reduced sample size limitations -Option to use
  ANOVA_exact2 (exact2 argument) improves functionality (not limited to
  product of factors)
- Updated vignettes to include updated information on functions
  - New vignette “Introduction to Justifying Alpha Levels”
- New Shiny App: justify
  - Creates a UI for utilizing the ANOVA_compromise function via Shiny

# Superpower 0.0.5

- Superpower_options(“plot”) is now set to TRUE. Plots will, by default,
  be printed -Easily reset with Superpower_options(plot = FALSE)
- plot_power has new features -Plots now show desired power -min_n is
  now limited; smallest min_n allowed is equal to the product of the
  design (e.g., ’2b\*2b’ has a smallest min_n of 4)
- Small update to plot_power to fix minor error in original code -Error
  resulted in power estimates being ~0.1-0.5% off actual power estimate

# Superpower 0.0.4

- Added emmeans_power function
  - Documentation added to the vignette
- Small updates to the Shiny apps to fix typos

# Superpower 0.0.3

- Unequal sample size in the design is now permitted -Limited to the
  ANOVA_design and ANOVA_power functions

- Added estimated marginal means comparisons using `emmeans` R package.

  - `emm = TRUE` in the ANOVA_power, ANOVA_exact, and plot_power will
    result in emmeans being calculated
  - Default is all pairwise comparisons but this can be modified with
    `contrast_type` and `emm_comp` options

- Added global options

  - Options that have crossover between functions can now be set
    globally for the package
  - Includes: verbose, emm, emm_model, contrast_type, alpha_level, and
    plot
  - These global options can be seen with Superpower_options()

- Updated Shiny Apps

  - Unequal n allowed for ANOVA_power
  - Added numeric input for alpha level (no longer slider)
  - Now includes emmeans options
  - kableExtra, emmeans, magrittr, and dplyr packages now needed to knit
    markdown file in app.
