NEWS
================

Updated Tue Dec 07 2021

# Superpower 0.2.0 – “Green Lantern”

-   Added power\_oneway\_ancova & power\_con\_ancova to allow for a
    basic power analysis of an analysis of covariance (ANCOVA) for
    one-way, between group designs.
-   Added ANCOVA\_analytic and ANCOVA\_contrast which allow for power
    analyses for factorial designs and user specified contrasts.
-   Added the label\_list argument to ANOVA\_design and ANCOVA\_analytic
    functions. Now labels can be assigned to factors and levels in a
    more sane fashion using named lists.

# Superpower 0.1.2 – “Rex Splode”

-   Minor fixes to power\_standardized\_alpha to keep Superpower on CRAN

# Superpower 0.1.1 – “Black Canary”

-   Added morey\_plot functions.
    -   Plot the effect size (x-axis) at different sample sizes (facets)
        and at different alpha levels (color).
    -   These plots are helpful in determining the sensitivity of
        statistical tests (t-test and F-test) across a range of effect
        sizes.
-   Added confint method for ANOVA\_power produced objects
    -   Calculates confidence level for binomial proportion (\# of
        results that are below alpha level) confidence intervals
        (Wilson, 1927)).
-   Minor changes to Shiny apps to fix glitches.

# Superpower 0.1.0

-   Added ANOVA\_exact2 function as an extension of ANOVA\_exact
    -   Now functional across all sample sizes but does not return a
        dataframe of afex aov object
-   liberal\_lambda argument added: allows users to specify the type of
    lambda calculations
    -   When liberal\_lambda = TRUE; lambda = cohen\_f^2 \* (num\_df +
        den\_df + 1)
    -   When liberal\_lambda = FALSE; lambda = cohen\_f^2 \* den\_df
-   Optimal alpha functions from JustifieR package added
-   ANOVA\_compromise function added which allows a compromise power
    analysis to be performed for all comparisons in a design
-   ANOVA\_design now returns as a class “design\_aov” with specific
    print and plot methods see ?`design_aov-methods`
    -   generate\_cor\_matrix function is now a non-exported function
        within the package (no longer contained within ANOVA\_design)
-   All simulation functions ANOVA\_power, ANOVA\_exact, and
    ANOVA\_exact2 now returns as a class “sim\_result” with specific
    print and plot methods see ?`sim_result-methods`
-   plot\_power now has reduced sample size limitations -Option to use
    ANOVA\_exact2 (exact2 argument) improves functionality (not limited
    to product of factors)
-   Updated vignettes to include updated information on functions
    -   New vignette “Introduction to Justifying Alpha Levels”
-   New Shiny App: justify
    -   Creates a UI for utilizing the ANOVA\_compromise function via
        Shiny

# Superpower 0.0.5

-   Superpower\_options(“plot”) is now set to TRUE. Plots will, by
    default, be printed -Easily reset with Superpower\_options(plot =
    FALSE)
-   plot\_power has new features -Plots now show desired power -min\_n
    is now limited; smallest min\_n allowed is equal to the product of
    the design (e.g., ’2b\*2b’ has a smallest min\_n of 4)
-   Small update to plot\_power to fix minor error in original code
    -Error resulted in power estimates being \~0.1-0.5% off actual power
    estimate

# Superpower 0.0.4

-   Added emmeans\_power function
    -   Documentation added to the vignette
-   Small updates to the Shiny apps to fix typos

# Superpower 0.0.3

-   Unequal sample size in the design is now permitted -Limited to the
    ANOVA\_design and ANOVA\_power functions

-   Added estimated marginal means comparisons using `emmeans` R
    package.

    -   `emm = TRUE` in the ANOVA\_power, ANOVA\_exact, and plot\_power
        will result in emmeans being calculated
    -   Default is all pairwise comparisons but this can be modified
        with `contrast_type` and `emm_comp` options

-   Added global options

    -   Options that have crossover between functions can now be set
        globally for the package
    -   Includes: verbose, emm, emm\_model, contrast\_type,
        alpha\_level, and plot
    -   These global options can be seen with Superpower\_options()

-   Updated Shiny Apps

    -   Unequal n allowed for ANOVA\_power
    -   Added numeric input for alpha level (no longer slider)
    -   Now includes emmeans options
    -   kableExtra, emmeans, magrittr, and dplyr packages now needed to
        knit markdown file in app.
