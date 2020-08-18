NEWS
================

Updated Tue Aug 18 2020

# Superpower 0.1.0

  - Added ANOVA\_exact2 function as an extension of ANOVA\_exact
      - Now functional across all sample sizes but does not return a
        dataframe of afex aov object
  - liberal\_lambda arugment added: allows users to specify the type of
    lambda calcuations
      - When liberal\_lambda = TRUE; lambda = cohen\_f^2 \* (num\_df +
        den\_df + 1)
      - When liberal\_lambda = FALSE; lambda = cohen\_f^2 \* den\_df
  - Optimal alpha functions from JustifieR package added
  - ANOVA\_compromise function added which allows a compromise power
    analysis to be performed for all comparisons in a design
  - ANOVA\_design now returns as a class “design\_aov” with specific
    print and plot methods see ?`design_aov-methods`
      - generate\_cor\_matrix function is now a non-exported function
        within the package (no longer contained within ANOVA\_design)
  - plot\_power now has reduced sample size limtations -Option to use
    ANOVA\_exact2 (exact2 argument) improves functionality (not limited
    to product of factors)
  - Updated vignettes to include new functions

# Superpower 0.0.5

  - Superpower\_options(“plot”) is now set to TRUE. Plots will, by
    default, be printed -Easily reset with Superpower\_options(plot =
    FALSE)
  - plot\_power has new features -Plots now show desired power -min\_n
    is now limited; smallest min\_n allowed is equal to the product of
    the design (e.g., ’2b\*2b’ has a smallest min\_n of 4)
  - Small update to plot\_power to fix minor error in original code
    -Error resulted in power estimates being \~0.1-0.5% off actual power
    estimate

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
