#
# Exact function Shiny app
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(Superpower)
library(ggplot2)
library(rmarkdown)
library(knitr)

# Define UI for application
ui <- dashboardPage(

  dashboardHeader(title = "ANOVApower Exact"),
  dashboardSidebar(
    sidebarMenu(
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
      tabItem(tabName = "design_tab",
              fluidRow(
                box(
                  title = "Design Output", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput("DESIGN"),
                  plotOutput('plot'),
                  tableOutput("corMat")
                ),

                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  "Specify design for a factorial design below", br(),
                  "*Must be specficied to continue*",

                  h5("Add numbers for each factor that specify the number of levels in the factors (e.g., 2 for a factor with 2 levels). Add a 'w' after the number for within factors, and a 'b' for between factors. Seperate factors with a * (asterisks). Thus '2b*3w' is a design with two factors, the first of which has 2 between levels, and the second of which has 3 within levels."),

                  textInput(inputId = "design", label = "Design Input",
                            value = "2b*2w"),

                  h5("Specify one word for each factor (e.g., AGE and SPEED) and the level of each factor (e.g., old and yound for a factor age with 2 levels)."),

                  textInput("labelnames", label = "Factor & level labels",
                            value = "AGE,old,young,SPEED,fast,slow"),

                  uiOutput("sample_size"),

                  textInput(inputId = "sd", label = "Standard Deviation",
                            value = 1.03),

                  h5("Specify the correlation for within-subjects factors."),

                  sliderInput("r",
                              label = "Correlation",
                              min = 0, max = 1, value = 0.87),

                  h5("Note that for each cell in the design, a mean must be provided. Thus, for a '2b*3w' design, 6 means need to be entered. Means need to be entered in the correct order. The app provides a plot so you can check if you entered means correctly. The general principle has designated factors (i.e., AGE and SPEED) and levels (e.g., old, young)."),

                  textInput("mu", label = "Vector of Means",
                            value = "1.03, 1.21, 0.98, 1.01"),

                  #Button to initiate the design
                  h5("Click the button below to set up the design - Check the output to see if the design is as you intended, then you can run the simulation."),

                  actionButton("designBut","Set-Up Design",
                               icon = icon("check-square"))

              )
      )
      ),

      # Exact Power content
      tabItem(tabName = "exact_tab",
              h2("Exact Power for Design"),

              fluidRow(

                box(
                  title = "Power Analysis Output", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  tableOutput('tableMain'),

                  tableOutput('tablePC')

                ),

                box(
                  title = "Simulation Parameters", status = "primary", solidHeader = TRUE,

                  conditionalPanel("input.designBut >= 1",
                  selectInput("correction", "Sphericity Correction",
                              c("None" = "none",
                                "Greenhous-Geisser" = "GG",
                                "Huynh-Feldt" = "HF")),

                  sliderInput("sig",
                              label = "Alpha Level",
                              min = 0, max = 1, value = 0.05),

                  actionButton("sim", "Print Results of Simulation",
                               icon = icon("print"))

                  )

                )
              )


      ),
      # Plot Power content
      tabItem(tabName = "plot_tab",
              h2("Plot Power Across Range of Sample Sizes"),

              fluidRow(

                box(
                  title = "Power Curve across Sample Sizes",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput('plot_curve')

                ),

                box(
                  title = "Set Min and Max Sample Size",
                  status = "primary", solidHeader = TRUE,

                  conditionalPanel("input.sim >=1",

                                   sliderInput("ss_2",
                                               label = h3("Range of Sample Sizes"), min = 3,
                                               max = 500, value = c(3, 100)),


                                   actionButton("sim_2", "Plot Power",
                                                icon = icon("chart-line")),

                                   h3("Note: No sphercity correction")

                  )

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
server <- function(input, output) {

  #Create set of reactive values
  values <- reactiveValues(design_result = 0,
                           power_result = 0,
                           power_curve = 0)

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

  #Produce ANOVA design
  observeEvent(input$designBut, {values$design_result <- ANOVA_design(design = as.character(input$design),
                                                                      n = as.numeric(input$sample_size),
                                                                      mu = as.numeric(unlist(strsplit(input$mu, ","))),
                                                                      labelnames = as.vector(unlist(strsplit(gsub("[[:space:]]", "",input$labelnames), ","))),
                                                                      sd = as.numeric(input$sd),
                                                                      r = as.numeric(input$r),
                                                                      plot = FALSE)
  })


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
                                                              correction = input$correction,
                                                              alpha_level = input$sig,
                                                              verbose = FALSE)


  })

  #Table output of ANOVA level effects; rownames needed
  output$tableMain <-  renderTable({
    req(input$sim)
    values$power_result$main_results},
    rownames = TRUE)

  #Table output of pairwise comparisons; rownames needed
  output$tablePC <-  renderTable({
    req(input$sim)
    values$power_result$pc_result},
    rownames = TRUE)

  observeEvent(input$sim_2, {
    values$power_curve <- plot_power(
      values$design_result,
      min_n = input$ss_2[1],
      max_n = input$ss_2[2],
      alpha_level = input$sig,
      plot = FALSE
    )
  })


  #Output plot of the design
  output$plot_curve <- renderPlot({
    req(input$sim_2)
    values$power_curve$p1
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
                     power_curve = values$power_curve$p1,
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

