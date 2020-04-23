NEWS
================

Updated Wed Apr 22 2020

# Superpower 0.0.4

  - Added emmeans\_power function
      - Documentation added to the vignette
  - Small updates to the Shiny apps to fix typos

# Superpower 0.0.3

  - Unequal sample size in the design is now permitted -Limited to the
    ANOVA\_design and ANOVA\_power functions

  - Added estimated marginal means comparisons using `emmeans` R
    package.
    
      - `emm = TRUE` in the ANOVA\_power, ANOVA\_exact, and plot\_power
        will result in emmeans being calculated
      - Default is all pairwise comparisons but this can be modified
        with `contrast_type` and `emm_comp` options

  - Added global options
    
      - Options that have crossover between functions can now be set
        globally for the package
      - Includes: verbose, emm, emm\_model, contrast\_type,
        alpha\_level, and plot
      - These global options can be seen with Superpower\_options()

  - Updated Shiny Apps
    
      - Unequal n allowed for ANOVA\_power
      - Added numeric input for alpha level (no longer slider)
      - Now includes emmeans options
      - kableExtra, emmeans, magrittr, and dplyr packages now needed to
        knit markdown file in app.
