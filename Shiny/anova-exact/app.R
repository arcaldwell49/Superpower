# ANOVA_exact
# Exact function Shiny app
#
# Deploy ---

# rsconnect::deployApp(here::here("Shiny","anova-exact"))

# Packages -----
library(shiny)
library(shinyjs)
library(shinyMatrix)
library(shinydashboard)
library(Superpower)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(scales)
library(pander)
# functions ----
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
  

  # 1. Specify Design and Simulation----

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
  

  # 2. Create Factors and Design ----

  
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
                   plot = FALSE,
                   liberal_labmda = FALSE)

# UI -----
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "ANOVA_exact"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info_tab", icon = icon("info-circle")),
      menuItem("Design", tabName = "design_tab", icon = icon("bezier-curve")),
      menuItem("Exact Power", tabName = "exact_tab", icon = icon("equals")),
      conditionalPanel("input.sim >=1",
                       downloadButton("report", "Download PDF Report")),
      menuItem("Power Curve", tabName = "plot_tab", icon = icon("infinity"))
      
    )
  ),

  dashboardBody(
    useShinyjs(),

    tabItems(
      ## info page content -----
      tabItem(tabName = "info_tab",
              box(
                title = "Using this App",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                h5("Please cite as: Lakens D, Caldwell AR. Simulation-Based Power Analysis for Factorial Analysis of Variance Designs. Advances in Methods and Practices in Psychological Science. 2021;4(1). doi:10.1177/2515245920951503"),
                h5("This Shiny app is for performing 'exact' simuations of factorial experimental designs in order to estimate power for an ANOVA and follow-up pairwise comparisons.
                 This app will not allow you to vary the standard deviations or the correlations in within-subjects designs.
                 If you do need/want to violate these assumptions please use the ANOVA_power app."),
                a("Click here for the other app", href = "http://shiny.ieis.tue.nl/anova_power/"),
                h3("The Design Tab"),
                h5("You must start with the Design tab in order to perform a power analysis. At this stage you must establish the parameters of the design (sample size, standard deviation, etc). \n Once you click \'Submit Design\' the design details will appear and you can continue onto the Exact Power tab."),
                h3("Exact Power Tab"),
                h5("In this tab, you will setup an *exact* simulation. All you can do at this stage is set the alpha level (default=.05) and decide the estimated marinal means analysis (optional)"),
                h3("Power curve Tab"),
                h5("In this tab, you can \"find\" your desired power (default = 90%) power across a range of sample sizes. 
                   All you need to do is set a minimum and maximum sample size. 
                   This tab will also allow you to download csv files 
                   of the power curve results."),
                h3("Download your Simulation"),
                h5("Once your simulation is completed a button a button will appear on the sidebar to download a PDF")
              ),              
              box(
                title = "NEWS",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                strong("Current updates to Superpower's Exact Shiny App"),
                h5("Updated and streamlined design input. App now has an interactive menu for design and factor labels.")
              )),
      tabItem(tabName = "design_tab",
              fluidRow(
                box(
                  title = "Design Input", status = "warning", solidHeader = TRUE,
                  strong("Specify the factorial design below"), br(),
                  "*Must be specified to continue*",

                  h5("Select the type of ANOVA, and then selct the number and types of factors below."),
                  selectInput("DesignLevel", "Factors in ANOVA",
                              c("One-way" = 1,
                                "Two-way" = 2,
                                "Three-way" = 3
                              ),
                              selected = 2),
                  wellPanel(h4("Factor 1 (a)"),
                            fluidRow(
                              
                              column(5,
                                     numericInput(inputId = "fct1lvl", label = "# of Levels",
                                                  min = 2,
                                                  step = 1,
                                                  value = 2)),
                              column(7,
                                     selectInput("fct1type", "Factor Type",
                                                 c("Between" = "b",
                                                   "Within" = "w"
                                                 ))))),
                  conditionalPanel(condition = "input.DesignLevel != 1",
                                   
                                   wellPanel(h4("Factor 2 (b)"),
                                   fluidRow(
                                   
                                   column(5,
                                   numericInput(inputId = "fct2lvl", label = "# of Levels",
                                                min = 2,
                                                step = 1,
                                                value = 2)),
                                   column(7,
                                          selectInput("fct2type", "Factor Type",
                                                      c("Between" = "b",
                                                        "Within" = "w"
                                                      )))))),
                  conditionalPanel(condition = "input.DesignLevel == 3",
                                   
                                   wellPanel(h4("Factor 3 (c)"),
                                             fluidRow(
                                               
                                               column(5,
                                                      numericInput(inputId = "fct3lvl", label = "# of Levels",
                                                                   min = 2,
                                                                   step = 1,
                                                                   value = 2)),
                                               column(7,
                                                      selectInput("fct3type", "Factor Type",
                                                                  c("Between" = "b",
                                                                    "Within" = "w"
                                                                  )))))),
                  #textInput(inputId = "design", label = "Design Input",
                  #          value = "2b*2w"),
                  
                  selectInput("labelChoice", "Would you like to enter factor and level names?",
                              c("No" = "no",
                                "Yes" = "yes"
                                )),
                  conditionalPanel(condition = "input.labelChoice == 'yes'",
                  h5("Specify one word for each factor (e.g., AGE and SPEED) and the level of each factor (e.g., old and yound for a factor age with 2 levels)."),
                  ## Label input ----
                  #textInput("labelnames", label = "Factor & level labels",
                  #          value = "AGE,old,young,SPEED,fast,slow"),
                  #textOutput(outputId="test1"),
                  wellPanel(h4("Factor 1 (a) names & labels"),
                            fluidRow(
                              
                              column(5,
                                     textInput(inputId = "fct1names", 
                                               label = "Factor Name",
                                               value = "A")),
                              column(7,
                                     uiOutput("aMatrix")))),
                  conditionalPanel(condition = "input.DesignLevel != 1",
                                   
                                   wellPanel(h4("Factor 2 (b) names & labels"),
                                             fluidRow(
                                               
                                               column(5,
                                                      textInput(inputId = "fct2names", 
                                                                   label = "Factor Name",
                                                                   value = "B")),
                                               column(7,
                                                      uiOutput("bMatrix"))))),
                  conditionalPanel(condition = "input.DesignLevel == 3",
                                   
                                   wellPanel(h4("Factor 3 (c)  names & labels"),
                                             fluidRow(
                                               column(
                                                 5,
                                                 textInput(
                                                   inputId = "fct3name",
                                                   label = "Factor Name",
                                                   value = "C"
                                                 )
                                               ),
                                               column(7,
                                                      uiOutput("cMatrix"))
                                             ))), ), 
                  
                  wellPanel(
                    fluidRow(
                  column(6,uiOutput("sample_size")),

                  column(6,numericInput(inputId = "sd", label = "Standard Deviation",
                            min = 0,
                            step = .01,
                            value = 1.03)),
                  conditionalPanel(condition = "output.corr_display == true",
                  #h5("Specify the correlation for within-subjects factors."),

                  #sliderInput("r",
                  #            label = "Common Correlation among Within-Subjects Factors",
                  #            min = 0, max = 1, value = 0.87),
                  numericInput("r", label = "Correlation among Within-Subjects Factors", value = .87, 
                               min = .0000000000000000000000000000000001, 
                               max = 1,
                               step = .001))
                  )),

                  h5("Note that for each cell in the design, a mean must be provided. Means need to be entered in the correct order. The app provides a plot so you can check if you entered means correctly."),

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
                  
                  numericInput("sig", label = "Alpha Level", value = .05, 
                               min = .0000000000000000000000000000000001, 
                               max = 1,
                               step = .001),

                  actionButton("sim", "Show Results of Simulation",
                               icon = icon("print"))
                  )

                ),
                
                box(
                  title = "Power Analysis Output", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput('tableMain'),
                  conditionalPanel("input.emm == 'no'",
                  tableOutput('tablePC')),
                  conditionalPanel("input.emm == 'yes'",
                  tableOutput('tableEMM'))
                  
                )
              )


      ),
      # Plot Power content
      tabItem(tabName = "plot_tab",
              h2("Find Desired Power Across a Range of Sample Sizes"),
              fluidRow(
                box(
                  title = "Set Min and Max Sample Size",
                  status = "warning", solidHeader = TRUE,
                  conditionalPanel("input.sim >=1",
                                   h2("Enter Range of Sample Sizes"),
                                   numericInput("ss_2min", label = "Minimum Sample Size", value = 3, 
                                                min = 3, 
                                                max = 10000,
                                                step = 1),
                                   numericInput("ss_2max", label = "Maximum Sample Size", value = 100, 
                                                min = 3, 
                                                max = 10000,
                                                step = 1),
                                   numericInput("sig2", label = "Alpha Level", value = .05, 
                                                min = .0000000000000000000000000000000001, 
                                                max = 1,
                                                step = .001),
                                   numericInput("desired", label = "Desired Power", value = 90, 
                                                min = 5, 
                                                max = 99.99,
                                                step = 1),
                                   actionButton("sim_2", "Find Desired Power",
                                                icon = icon("chart-line")),
                                   h3("Note: No sphercity correction"),
                                   conditionalPanel("input.sim_2 >=1",
                                                    h5("Download Results for ANOVA and estimated marginal means"),
                                   downloadButton("dl_data", "Download ANOVA Results"),
                                   conditionalPanel("input.emm == 'yes' ",
                                   downloadButton("dl_data2", "Download EMMEANS Results")))
                  )
                ),
                
                box(
                  title = "ANOVA/MANOVA Effects",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  #renderPlot('plot_curve')#,
                  tableOutput('anova_n'),
                  tableOutput('manova_n')
                  
                ),
                box(
                  title = "Estimated Marginal Means",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput('emm_n')
                  
                )
              )
      )
    ) #end tabItems
  ) #end dashboardBody
)

# Server -----
# Define server logic
server <- function(input, output, session) {
  #Create set of reactive values
  values <- reactiveValues(design_result = 0,
                           power_result = 0,
                           power_curve = 0,
                           emm_output = 0,
                           des_string = 1)
  #observe({
  #  des_string = input$DesignLevel
  #})
  #des_log = reactive({input$DesignLevel})
## design string ----
des_string = reactive({
  des_string = 1
  if(input$DesignLevel == 1){
    # fct1lvl
    # fct1type
    des_string = paste0(as.numeric(input$fct1lvl),
                        as.character(input$fct1type))
  } else if(input$DesignLevel == 2){
    des_string = paste0(as.numeric(input$fct1lvl),
                        as.character(input$fct1type),
                        "*",
                        as.numeric(input$fct2lvl),
                        as.character(input$fct2type))
  } else {
    des_string = paste0(as.numeric(input$fct1lvl),
                        as.character(input$fct1type),
                        "*",
                        as.numeric(input$fct2lvl),
                        as.character(input$fct2type),
                        "*",
                        as.numeric(input$fct3lvl),
                        as.character(input$fct3type))
  }
  return(des_string)
})
  
  #observe({
  #  shinyjs::toggleState("designBut", input$sample_size >= (prod(
  #    as.numeric(
  #      unlist(
  #        regmatches
  #        (des_string(),
  #          gregexpr("[[:digit:]]+",
  #                   des_string()))))) + 1))
  #})
  
  output$corr_display = reactive(grepl("w",as.character(des_string() )))
  outputOptions(output, "corr_display", suspendWhenHidden = FALSE)
  
  output$sample_size <- renderUI({
    numericInput("sample_size", 
                 label = "Sample Size per Cell",
                 min = (prod(
                   as.numeric(
                     unlist(
                       regmatches
                       (des_string(),
                         gregexpr("[[:digit:]]+",
                                  des_string() ))))) + 1),
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
  ## Matrices ----
  output$muMatrix <-  renderUI({matrixInput(
    "muMatrix",
    value = matrix(c(1), 1, 
                   prod(as.numeric(strsplit(des_string(), "\\D+")[[1]])),
                   dimnames = list(c("mu"),
                                   c(label_function(des_string())))),
    rows = list(names = TRUE),
    cols = list(names = TRUE),
    copy = FALSE,
    paste = FALSE
  )
  })
  
  output$aMatrix <-  renderUI({matrixInput(
    "aMatrix",
    label = "Level Labels",
    value = matrix(seq(1,as.numeric(input$fct1lvl),1), 1, 
                   as.numeric(input$fct1lvl)),
    rows = list(names = FALSE),
    cols = list(names = FALSE),
    copy = FALSE,
    paste = FALSE
  )
  })
  
  output$bMatrix <-  renderUI({matrixInput(
    "bMatrix",
    label = "Level Labels",
    value = matrix(seq(1,as.numeric(input$fct2lvl),1), 1, 
                   as.numeric(input$fct2lvl)),
    rows = list(names = FALSE),
    cols = list(names = FALSE),
    copy = FALSE,
    paste = FALSE
  )
  })
  
  output$cMatrix <-  renderUI({matrixInput(
    "cMatrix",
    label = "Level Labels",
    value = matrix(seq(1,as.numeric(input$fct3lvl),1), 1, 
                   as.numeric(input$fct3lvl)),
    rows = list(names = FALSE),
    cols = list(names = FALSE),
    copy = FALSE,
    paste = FALSE
  )
  })
  
  ## label list -----
  ll_list = reactive({
    ll_list = 1
    if(input$DesignLevel == 1){
      # fct1lvl
      # fct1type
      fct1 = input$fct1names
      fct2 = input$fct2names
      fct3 = input$fct3names
      ll_list = setNames(list(input$aMatrix),
                         c(fct1))
    } else if(input$DesignLevel == 2){
      fct1 = input$fct1names
      fct2 = input$fct2names
      fct3 = input$fct3names
      ll_list = setNames(list(input$aMatrix,
                              input$bMatrix),
                         c(fct1,
                           fct2))
    } else {
      fct1 = input$fct1names
      fct2 = input$fct2names
      fct3 = input$fct3names
      ll_list = setNames(list(input$aMatrix,
                              input$bMatrix,
                              input$cMatrix),
                         c(fct1,
                           fct2,
                           fct3))
    }
    return(ll_list)
  })
  
  output$test1 <- renderPrint({print(ll_list())})
  #Produce ANOVA design
  observeEvent(input$designBut, {values$design_result <- ANOVA_design(design = as.character(des_string() ),
                                                                      n = as.numeric(input$sample_size),
                                                                      mu = as.numeric(input$muMatrix),
                                                                      #labelnames = if (input$labelChoice == "yes") {
                                                                      #  as.vector(unlist(strsplit(gsub("[[:space:]]", "",input$labelnames), ",")))
                                                                      #}else{
                                                                      #  NULL
                                                                      #},
                                                                      label_list = if (input$labelChoice == "yes") {ll_list()} else{
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
          "\n"#,
          #ifelse(values$design_result$n < (prod(
          #  as.numeric(
          #    unlist(
          #      regmatches
          #      (des_string(),
          #        gregexpr("[[:digit:]]+",
          #                 des_string()
          #                 )))))+1), "WARNING: Sample Size must be greater than the product of the cells \n For example, 2b*2w = 4 and requires a sample size of at least 5 per cell", "")
          )
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
  observeEvent(input$sim, {values$power_result <- ANOVA_exact2(values$design_result,
                                                              correction = "none",
                                                              alpha_level = input$sig,
                                                              verbose = FALSE,
                                                              emm = if (input$emm == "yes") {
                                                                TRUE
                                                              } else{FALSE},
                                                              emm_model = as.character(input$emm_model),
                                                              contrast_type = as.character(input$contrast_type),
                                                              emm_comp = as.character(input$emm_comp))


  })


  
  #Table output of ANOVA level effects; rownames needed
  output$tableMain <-  renderTable({
    req(input$sim)
    values$power_result$main_results},
    caption = "Power for ANOVA effects",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE)

  #Table output of pairwise comparisons; rownames needed
  output$tablePC <-  renderTable({
    req(input$sim)
    values$power_result$pc_result},
    caption = "Power for Pairwise Comparisons with t-tests",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = TRUE)
  
  output$tableEMM <-  renderTable({
    req(input$sim)
    values$power_result$emm_results},
    caption = "Power for Estimated Marginal Means Comparisons",
    caption.placement = getOption("xtable.caption.placement", "top"),
    rownames = FALSE)

  observeEvent(input$sim_2, {
    values$power_curve <- plot_power(
      values$design_result,
      min_n = input$ss_2min,
      max_n = input$ss_2max,
      desired_power = input$desired,
      alpha_level = input$sig2,
      emm = if (input$emm == "yes") {
        TRUE
      } else{FALSE},
      emm_model = as.character(input$emm_model),
      contrast_type = as.character(input$contrast_type),
      emm_comp = as.character(input$emm_comp),
      plot = FALSE,
      verbose = FALSE,
      exact2=TRUE
    )
  })


  #Output plot of the design
  output$plot_curve <- renderPlot({
    req(input$sim_2)
    values$power_curve$plot_ANOVA
  })
  

    output$anova_n = renderTable({
    req(input$sim_2)
    values$power_curve$anova_n
  }
  )
    
    output$manova_n = renderTable({
      req(input$sim_2)
      values$power_curve$manova_n
    }
    )
    
    output$emm_n = renderTable({
      req(input$sim_2)
      values$power_curve$emm_n
    }
    )
  #output$plot_curve_emm <- renderPlot({
  #  req(input$sim_2)
  #  values$power_curve$plot_emm
  #})
  
  output$dl_data <- downloadHandler(
    filename = function() {
      paste("ANOVA_power_df", ".csv", sep = "")
    },
    content = function(file2) {
      write.csv(values$power_curve$power_df, file2, row.names = FALSE)
    }
  )
  
  output$dl_data2 <- downloadHandler(
    filename = function() {
      paste("emm_power_df", ".csv", sep = "")
    },
    content = function(file3) {
      write.csv(values$power_curve$power_df_emm, file3, row.names = FALSE)
    }
  )
  
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
      params <- list(tablePC = values$power_result$pc_result,
                     tableMain = values$power_result$main_results,
                     tableEMM = values$power_result$emm_results,
                     means_plot = values$design_result$meansplot,
                     n = values$design_result$n,
                     model = deparse(values$design_result$frml1),
                     design = values$design_result$string,
                     cor_mat = values$design_result$cor_mat,
                     sigmatrix = values$design_result$sigmatrix,
                     alpha_level = values$power_result$alpha_level,
                     input_emm = input$emm,
                     session = sessionInfo())

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

# Serve ------
# Run the application
shinyApp(ui = ui, server = server)

