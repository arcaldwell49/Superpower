# JustifieR
# ANOVA_compromise function Shiny app
#

library(shiny)
library(shinyjs)
library(shinyMatrix)
library(shinydashboard)
library(Superpower)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(kableExtra)

label_function <- function(design, labelnames = NULL) {
  #If labelnames are not provided, they are generated.
  #Store factor levels (used many times in the script, calculate once)
  factor_levels <- as.numeric(strsplit(design, "\\D+")[[1]])
  
  if (is.null(labelnames)) {
    for (i1 in 1:length(factor_levels)){
      labelnames <- append(labelnames,paste(paste(letters[i1]), sep = ""))
      for (i2 in 1:factor_levels[i1]){
        labelnames <- append(labelnames,paste(paste(letters[i1]), paste(i2), sep = ""))
      }
    }
  }
  
  if (length(labelnames) != length(factor_levels) + sum(factor_levels)) {
    stop("Variable 'design' does not match the length of the labelnames")
  }
  
  ###############
  # 1. Specify Design and Simulation----
  ###############
  # String used to specify the design
  # Add numbers for each factor with 2 levels, e.g., 2 for a factor with 2 levels
  # Add a 'w' after the number for within factors, and a 'b' for between factors
  # Separate factors with a * (asterisk)
  # Thus "2b*3w) is a design with 2 between levels, and 3 within levels
  
  
  #Check if the design and sd match (either 1 or length of design)
  #if(length(sd) != 1 && prod(factor_levels) != length(sd)){stop("The SD must be a length of 1 or match the length of the study design")}
  
  #Check if the factors are of an acceptable number of levels
  if(any(factor_levels <= 0) == TRUE | any(factor_levels > 99) ) {
    stop("Each factor can only have between 2 and 99 levels")
  }
  
  ###############
  # 2. Create Factors and Design ----
  ###############
  
  #Count number of factors in design
  factors <- length(factor_levels)
  
  #Get factor names and labelnameslist
  labelnames1 <- labelnames[(1 + 1):(1+factor_levels[1])]
  if(factors > 1){labelnames2 <- labelnames[(factor_levels[1] + 3):((factor_levels[1] + 3) + factor_levels[2] - 1)]}
  if(factors > 2){labelnames3 <- labelnames[(factor_levels[2] + factor_levels[1] + 4):((factor_levels[2] + factor_levels[1] + 4) + factor_levels[3] - 1)]}
  
  factornames1 <- labelnames[1]
  if(factors > 1){factornames2 <- labelnames[factor_levels[1] + 2]}
  if(factors > 2){factornames3 <- labelnames[factor_levels[2] + factor_levels[1] + 3]}
  
  if(factors == 1){labelnameslist <- list(labelnames1)}
  if(factors == 2){labelnameslist <- list(labelnames1,labelnames2)}
  if(factors == 3){labelnameslist <- list(labelnames1,labelnames2,labelnames3)}
  
  if(factors == 1){factornames <- c(factornames1)}
  if(factors == 2){factornames <- c(factornames1,factornames2)}
  if(factors == 3){factornames <- c(factornames1,factornames2,factornames3)}
  
  #Specify within/between factors in design: Factors that are within are 1, between 0
  design_factors <- strsplit(gsub("[^A-Za-z]","",design),"",fixed = TRUE)[[1]]
  design_factors <- as.numeric(design_factors == "w") #if within design, set value to 1, otherwise to 0
  
  #Specify design list (similar as below)
  xxx <- data.frame(matrix(NA, nrow = prod(factor_levels), ncol = 0))
  for(j in 1:factors){
    xxx <- cbind(xxx, as.factor(unlist(rep(as.list(paste(labelnameslist[[j]],
                                                         sep="_")),
                                           each = prod(factor_levels)/prod(factor_levels[1:j]),
                                           times = prod(factor_levels)/prod(factor_levels[j:factors])
    ))))
  }
  design_list <- as.character(interaction(xxx[, 1:factors], sep = "_"))
  paste(design_list)
}

Superpower_options(emm = TRUE,
                   verbose = FALSE,
                   plot = FALSE)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA_compromise"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info_tab", icon = icon("info-circle")),
      menuItem("Design", tabName = "design_tab", icon = icon("bezier-curve")),
      menuItem("Find Alpha", tabName = "exact_tab", icon = icon("search")),
      conditionalPanel("input.sim >=1",
                       downloadButton("report", "Download PDF Report"))
      
    )
  ),

  dashboardBody(
    useShinyjs(),

    tabItems(
      # Design content
      tabItem(tabName = "info_tab",
              box(
                title = "Using this App",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                h5("Citation: To be Added"),
                h5("This Shiny app is for performing finding the \"optimal\" alpha for ANOVA and estimated margina means.
                 This app will not allow you to vary the standard deviations or the correlations in within-subjects designs.
                 If you do need/want to violate these assumptions please use the ANOVA_power app."),
                a("Click here for the Monte Carlo power analysis app", href = "http://shiny.ieis.tue.nl/anova_power/"),
                a("Click here for the exact power analysis app", href = "http://shiny.ieis.tue.nl/anova_exact/"),
                h3("The Design Tab"),
                h5("You must start with the Design tab in order to perform a power analysis. At this stage you must establish the parameters of the design (sample size, standard deviation, etc). *Please note the sample size must be greater than the product of the cells \n For example, 2b*2w = 4 and requires a sample size of at least 5 per cell. If this is entered incorrectly, then the \'Submit Design\' button will be disabled. \n Once you click \'Submit Design\' the design details will appear and you can continue onto the power analysis."),
                h3("Find Alpha Tab"),
                h5("In this tab, you will setup a set of simulations to find the optimal alpha. At this stage you need to decide the estimated marinal means analysis (optional), the costs of Type 1 and Type 2 errors (ratio of T1:T2), the prior odds of the alternative hypothesis compared to the null hypothesis (ratio H1:H0), and how the combined error rate is set (balanced or minimal)."),
                h3("Download your Simulation"),
                h5("Once your simulation is completed a button a button will appear on the sidebar to download a PDF")
              ),              
              box(
                title = "NEWS",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                strong("Current updates to Superpower's ANOVA_compromise Shiny App"),
                h5("This is a new function so no updates yet!")
              )),
      tabItem(tabName = "design_tab",
              fluidRow(
                box(
                  title = "Design Input", status = "warning", solidHeader = TRUE,
                  strong("Specify the factorial design below"), br(),
                  "*Must be specified to continue*",

                  h5("Add numbers for each factor that specify the number of levels in the factors (e.g., 2 for a factor with 2 levels). Add a 'w' after the number for within factors, and a 'b' for between factors. Seperate factors with a * (asterisks). Thus '2b*3w' is a design with two factors, the first of which has 2 between levels, and the second of which has 3 within levels."),

                  textInput(inputId = "design", label = "Design Input",
                            value = "2b*2w"),
                  
                  selectInput("labelChoice", "Would you like to enter factor and level names?",
                              c("No" = "no",
                                "Yes" = "yes"
                                )),
                  conditionalPanel(condition = "input.labelChoice == 'yes'",
                  h5("Specify one word for each factor (e.g., AGE and SPEED) and the level of each factor (e.g., old and yound for a factor age with 2 levels)."),

                  textInput("labelnames", label = "Factor & level labels",
                            value = "AGE,old,young,SPEED,fast,slow")),

                  uiOutput("sample_size"),

                  numericInput(inputId = "sd", label = "Common Standard Deviation",
                            min = 0,
                            step = .01,
                            value = 1.03),
                  conditionalPanel(condition = "output.corr_display == true",
                  h5("Specify the correlation for within-subjects factors."),

                  #sliderInput("r",
                  #            label = "Common Correlation among Within-Subjects Factors",
                  #            min = 0, max = 1, value = 0.87),
                  numericInput("r", label = "Common Correlation among Within-Subjects Factors", value = .87, 
                               min = .0000000000000000000000000000000001, 
                               max = 1,
                               step = .001)),

                  h5("Note that for each cell in the design, a mean must be provided. Thus, for a '2b*3w' design, 6 means need to be entered. Means need to be entered in the correct order. The app provides a plot so you can check if you entered means correctly. The general principle has designated factors (i.e., AGE and SPEED) and levels (e.g., old, young)."),

                  #textInput("mu", label = "Vector of Means",
                  #          value = "1.03, 1.21, 0.98, 1.01"),
                  
                  strong("Means for Each Cell in the Design"),
                  
                  uiOutput("muMatrix"),

                  #Button to initiate the design
                  h5("Click the button below to set up the design - Check the output to see if the design is as you intended, then you can run the simulation in the next tab."),

                  actionButton("designBut","Set-Up Design",
                               icon = icon("check-square"))

              ),
              
              box(
                title = "Design Output", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                verbatimTextOutput("DESIGN"),
                plotOutput('plot'),
                tableOutput("corMat")
              )
      )
      ),

      # Exact Power content
      tabItem(tabName = "exact_tab",
              h2("Exact Power for Design"),

              fluidRow(
                box(
                  title = "Simulation Parameters", status = "warning", solidHeader = TRUE,

                  conditionalPanel("input.designBut >= 1",
                                   
                                   selectInput("emm", "Would you like to compare the estimated marginal means?",
                                               c("No" = "no",
                                                 "Yes" = "yes"
                                                 )),
                                   conditionalPanel("input.emm == 'yes'",
                                                    h5("Keeping the default settings will result in all pairwise comparisons being performed."),
                                                    selectInput("emm_model", "What model would you like to use for the estimated marginal means",
                                                                c("Univariate" = "univariate",
                                                                  "Multivariate" = "multivariate")),
                                                    selectInput("contrast_type", "What type of comparisons would you like to make?",
                                                                c("Pairwise" = "pairwise",
                                                                  "Polynomial contrast" = "poly",
                                                                  "Helmert" = "consec",
                                                                  "Compare each level with the average over all levels" = "eff")),
                                                    textInput(inputId = "emm_comp", 
                                                              label = "What comparisons would you like to make with estimated marginal means?",
                                                              value = "a + b"),
                                                    textOutput("emm_formula"),
                                                    h5("The addition sign ('+') will add factors for comparisons while factors after a vertical bar '|'  specifies the names of predictors to condition on"),
                                                    a("For more information on setting comparisons", href = "https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html#formulas")
                                                    ),
                                   

                  #sliderInput("sig",
                  #            label = "Alpha Level",
                  #            min = 0, max = .2, value = 0.05),
                  
                  numericInput("costs", label = "Relative cost of Type 1 errors vs. Type 2 errors. If type 1 errors are 4 times as costly as Type 2 errors then you would enter 4 here.", value = 1, 
                               min = 1, 
                               max = 100,
                               step = 1),
                  numericInput("hyp", label = "How much more likely a-priori is H1 (alternative) than H0 (null)? Default is 1: equally likely.", value = 1, 
                               min = 1, 
                               max = 100,
                               step = 1), #Either "minimal" to minimize error rates, or "balance" to balance error rates.
                  selectInput("err_bal", label = "How should the optimal alpha level be determined? Options are Either \"minimal\" to minimize error rates, or \"balance\" to balance error rates.", c(
                    "minimal" = "minimal",
                    "balance" = "balance"
                  )),

                  actionButton("sim", "Find the optimal alpha(s)",
                               icon = icon("print"))
                  )

                ),
                
                box(
                  title = "Optimal Error Rates Output", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput('tableMain'),
                  conditionalPanel("input.emm == 'yes'",
                  tableOutput('tableEMM'))
                  
                )
              )


      )
    ) #end tabItems
  ) #end dashboardBody
)


###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Define server logic
server <- function(input, output, session) {

  #Create set of reactive values
  values <- reactiveValues(design_result = 0,
                           power_result = 0,
                           emm_output = 0)
  observe({
    shinyjs::toggleState("designBut", input$sample_size >= (prod(
      as.numeric(
        unlist(
          regmatches
          (input$design,
            gregexpr("[[:digit:]]+",
                     input$design))))) + 1))
  })
  
  output$corr_display = reactive(grepl("w",as.character(input$design)))
  outputOptions(output, "corr_display", suspendWhenHidden = FALSE)
  
  output$sample_size <- renderUI({
    numericInput("sample_size", 
                 label = "Sample Size per Cell",
                 min = (prod(
                   as.numeric(
                     unlist(
                       regmatches
                       (input$design,
                         gregexpr("[[:digit:]]+",
                                  input$design))))) + 1),
                 max = 1000, value = 80, step = 1)
  })
  
  #Old sample size dynamic input
  #output$sample_size <- renderUI({sliderInput("sample_size",
  #            label = "Sample Size per Cell",
  #            min = prod(
  #              as.numeric(
  #                unlist(
  #                  regmatches
  #                  (input$design,
  #                    gregexpr("[[:digit:]]+",
  #                             input$design))))),
  #            max = 1000, value = 80)
  #})
  
  output$muMatrix <-  renderUI({matrixInput(
    "muMatrix",
    value = matrix(c(1), 1, 
                   prod(as.numeric(strsplit(input$design, "\\D+")[[1]])),
                   dimnames = list(c("mu"),
                                   c(label_function(input$design)))),
    rows = list(names = TRUE),
    cols = list(names = TRUE),
    copy = TRUE,
    paste = TRUE
  )
  })
  
  #Produce ANOVA design
  observeEvent(input$designBut, {values$design_result <- ANOVA_design(design = as.character(input$design),
                                                                      n = as.numeric(input$sample_size),
                                                                      mu = as.numeric(input$muMatrix),
                                                                      labelnames = if (input$labelChoice == "yes") {
                                                                        as.vector(unlist(strsplit(gsub("[[:space:]]", "",input$labelnames), ",")))
                                                                      }else{
                                                                        NULL
                                                                      },
                                                                      sd = as.numeric(input$sd),
                                                                      r = as.numeric(input$r),
                                                                      plot = FALSE)
  values$emm_output <- as.character(values$design_result$frml2)[2] 
  updateTextInput(session, "emm_comp", value = values$emm_output)

  })
  
  #observeEvent(input$designBut, {})

  
  
  
  output$emm_formula <- renderText({
    paste("Enter",as.character(values$design_result$frml2[2]), " above to receive results for all pairwise comparisons")})

  #Output text for ANOVA design
  output$DESIGN <- renderText({
    req(input$designBut)

    paste("The design is set as", values$design_result$string,
          "
          ",
          "Model formula: ", deparse(values$design_result$frml1),
          "
          ",
          "Sample size per cell n = ", values$design_result$n,
          "\n",
          ifelse(values$design_result$n < (prod(
            as.numeric(
              unlist(
                regmatches
                (input$design,
                  gregexpr("[[:digit:]]+",
                           input$design)))))+1), "WARNING: Sample Size must be greater than the product of the cells \n For example, 2b*2w = 4 and requires a sample size of at least 5 per cell", ""))
  })

  #Output of correlation and standard deviation matrix
  output$corMat <- renderTable(colnames = FALSE,
                               caption = "Variance-Covariance Matrix",
                               caption.placement = getOption("xtable.caption.placement", "top"),
                               {
                                 req(input$designBut)
                                 values$design_result$sigmatrix

                               })
  #Output plot of the design
  output$plot <- renderPlot({
    req(input$designBut)
    values$design_result$meansplot})

  #Runs EXACT simulation and saves result as reactive value
  observeEvent(input$sim, {values$power_result <-
    Superpower::ANOVA_compromise(
      values$design_result,
      correction = "none",
      emm = if (input$emm == "yes") {
        TRUE
      } else{
        FALSE
      },
      emm_model = as.character(input$emm_model),
      contrast_type = as.character(input$contrast_type),
      emm_comp = as.character(input$emm_comp),
      costT1T2 = as.numeric(input$costs),
      priorH1H0 = as.numeric(input$hyp),
      error = as.character(input$err_bal),
      liberal_lambda = FALSE
    )


  })


  
  #Table output of ANOVA level effects; rownames needed
  output$tableMain <-  renderTable({
    req(input$sim)
    values$power_result$aov_comp},
    caption = "Alpha-Beta Levels for ANOVA effects",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE)
  
  output$tableEMM <-  renderTable({
    req(input$sim)
    values$power_result$emmeans_comp},
    caption = "Alpha-Beta Levels for Estimated Marginal Means Comparisons",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE)




  
  #Create downloadable report in markdown TINYTEX NEEDS TO BE INSTALLED
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(tableMain = values$power_result$aov_comp,
                     tableEMM = values$power_result$emmeans_comp,
                     means_plot = values$design_result$meansplot,
                     n = values$design_result$n,
                     model = deparse(values$design_result$frml1),
                     design = values$design_result$string,
                     cor_mat = values$design_result$cor_mat,
                     sigmatrix = values$design_result$sigmatrix,
                     input_emm = input$emm)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Run the application
shinyApp(ui = ui, server = server)

