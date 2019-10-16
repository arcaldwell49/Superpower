# ANOVA_exact
# Exact function Shiny app
#

library(shiny)
library(shinyjs)
library(shinyMatrix)
library(shinydashboard)
library(Superpower)
library(ggplot2)
library(rmarkdown)
library(knitr)

Superpower_options(emm = TRUE,
                   verbose = FALSE,
                   plot = FALSE)

# Define UI for application
ui <- dashboardPage(

  dashboardHeader(title = "ANOVA_exact"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info_tab", icon = icon("info-circle")),
      menuItem("Design", tabName = "design_tab", icon = icon("bezier-curve")),
      menuItem("Exact Power", tabName = "exact_tab", icon = icon("equals")),
      menuItem("Plot Power", tabName = "plot_tab", icon = icon("infinity")),
      conditionalPanel("input.sim_2 >= 1",
                       downloadButton("report", "Download PDF Report")
      )
    )
  ),

  dashboardBody(

    tabItems(
      # Design content
      tabItem(tabName = "info_tab",
              box(
                title = "Using this App",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                h5("This Shiny app is for performing 'exact' simuations of factorial experimental designs in order to estimate power for an ANOVA and follow-up pairwise comparisons.
                 This app will not allow you to vary the standard deviations or the correlations in within-subjects designs.
                 If you do need/want to violate these assumptions please use the ANOVA_power app."),
                a("Click here for the other app", href = "http://shiny.ieis.tue.nl/anova_power/"),
                h3("The Design Tab"),
                h5("You must start with the Design tab in order to perform a power analysis. At this stage you must establish the parameters of the design (sample size, standard deviation, etc).
                 Once you click Submit Design the design details will be printed and you can continue onto the power analysis."),
                h3("Exact Power Tab"),
                h5("In this tab, you will setup the Monte Carlo simulation. All you can do at this stage is set the alpha level (default=.05)"),
                h3("Plot Power Tab"),
                h5("In this tab, you can see power across a range of sample sizes. All you need to do is set a minimum and maximum sample size"),
                h3("Download your Simulation"),
                h5("Once your simulation is completed a button a button will appear on the sidebar to download a PDF")
              ),              
              box(
                title = "NEWS",
                status = "danger",
                solidHeader = TRUE,
                collapsible = FALSE,
                strong("Current updates to Superpower's Exact Shiny App"),
                h5("Option for estimated marginal means added")
              )),
      tabItem(tabName = "design_tab",
              fluidRow(
                box(
                  title = "Design Input", status = "warning", solidHeader = TRUE,
                  strong("Specify the factorial design below"), br(),
                  "*Must be specficied to continue*",

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

                  textInput(inputId = "sd", label = "Common Standard Deviation",
                            value = 1.03),

                  h5("Specify the correlation for within-subjects factors."),

                  sliderInput("r",
                              label = "Common Correlation among Within-Subjects Factors",
                              min = 0, max = 1, value = 0.87),

                  h5("Note that for each cell in the design, a mean must be provided. Thus, for a '2b*3w' design, 6 means need to be entered. Means need to be entered in the correct order. The app provides a plot so you can check if you entered means correctly. The general principle has designated factors (i.e., AGE and SPEED) and levels (e.g., old, young)."),

                  #textInput("mu", label = "Vector of Means",
                  #          value = "1.03, 1.21, 0.98, 1.01"),
                  
                  strong("Vector of Means"),
                  
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
                                                    selectInput("contrast_type", "Would you like to compare the estimated marginal means?",
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
                                   

                  sliderInput("sig",
                              label = "Alpha Level",
                              min = 0, max = 1, value = 0.05),

                  actionButton("sim", "Print Results of Simulation",
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
              h2("Plot Power Across a Range of Sample Sizes"),
              fluidRow(
                box(
                  title = "Set Min and Max Sample Size",
                  status = "warning", solidHeader = TRUE,
                  conditionalPanel("input.sim >=1",
                                   sliderInput("ss_2",
                                               label = h3("Range of Sample Sizes"), min = 3,
                                               max = 500, value = c(3, 100)),
                                   actionButton("sim_2", "Plot Power",
                                                icon = icon("chart-line")),
                                   h3("Note: No sphercity correction")
                  )
                ),
                
                box(
                  title = "Power Curve across Sample Sizes",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput('plot_curve')
                  
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
                           power_curve = 0,
                           emm_output = 0)

  output$sample_size <- renderUI({sliderInput("sample_size",
              label = "Sample Size per Cell",
              min = prod(
                as.numeric(
                  unlist(
                    regmatches
                    (input$design,
                      gregexpr("[[:digit:]]+",
                               input$design))))),
              max = 1000, value = 80)
  })
  
  output$muMatrix <-  renderUI({matrixInput(
    "muMatrix",
    value = matrix(c(1), 1, prod(as.numeric(strsplit(input$design, "\\D+")[[1]]))),
    rows = list(names = FALSE),
    cols = list(names = FALSE),
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
  })


  
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
          "Sample size per cell n = ", values$design_result$n)
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
  observeEvent(input$sim, {values$power_result <- ANOVA_exact(values$design_result,
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

   reactive({
     values$emm_output <- values$design_result$frml2
     updateTextInput(session, "emm_comp", value = values$emm_output)
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
      min_n = input$ss_2[1],
      max_n = input$ss_2[2],
      alpha_level = input$sig,
      emm = if (input$emm == "yes") {
        TRUE
      } else{FALSE},
      emm_model = as.character(input$emm_model),
      contrast_type = as.character(input$contrast_type),
      emm_comp = as.character(input$emm_comp)
    )
  })


  #Output plot of the design
  output$plot_curve <- renderPlot({
    req(input$sim_2)
    values$power_curve$plot_ANOVA
  })

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
                     means_plot = values$power_result$plot,
                     n = values$design_result$n,
                     model = deparse(values$design_result$frml1),
                     design = values$design_result$string,
                     cor_mat = values$design_result$cor_mat,
                     sigmatrix = values$design_result$sigmatrix,
                     alpha_level = values$power_result$alpha_level,
                     power_curve = values$power_curve$plot_ANOVA,
                     power_curve_df = values$power_curve$power_df)

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

