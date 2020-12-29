# Morey Power Sensitivity Plots
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

Superpower_options(verbose = FALSE,
                   plot = FALSE)

# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Morey Plots"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info_tab", icon = icon("info-circle")),
            menuItem("t-test", tabName = "ttest_tab", icon = icon("calculator")),
            menuItem("F-test", tabName = "ftest_tab", icon = icon("calculator"))
            
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
                        a("Inspired By: Morey, R.D. (2020). Power and precision Why the push for replacing power with precision is misguided.", href = "https://medium.com/@richarddmorey/power-and-precision-47f644ddea5e"),
                        h5("This Shiny app is for building power-sensitivity plots that were suggested by Richard D. Morey. These plots show power (y-axis) for various effect sizes (x-axis) at multiple sample sizes (panel) and alpha level (color)."),
                        a("Click here for the Monte Carlo power analysis app", href = "http://shiny.ieis.tue.nl/anova_power/"),
                        a("Click here for the exact power analysis app", href = "http://shiny.ieis.tue.nl/anova_exact/"),
                        h3("The t-test Tab"),
                        h5("In this tab the plots for a t-test can be created."),
                        h3("The F-test Tab"),
                        h5("In this tab, the plots for a F-test (ANOVA) can be created.")
                    ),              
                    box(
                        title = "NEWS",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        strong("Current updates to Superpower's Morey Plot Shiny App"),
                        h5("This is a new function; no updates yet!")
                    )),
            tabItem(tabName = "ttest_tab",
                    fluidRow(
                        box(
                            title = "t-test Inputs", 
                            status = "warning", 
                            solidHeader = TRUE,
                            selectInput("type_t", "Type of t-test:",
                                        c("Two independent samples" = "two.sample",
                                          "One sample" = "one.sample",
                                          "Paired samples" = "paired")),
                            selectInput("alt_t", "Alternative Hypothesis",
                                        c("Two Sided/Tailed" = "two.sided",
                                          "One Sided/Tailed" = "one.sided")),
                            numericInput("d_min", 
                                         label = "Minimum Cohen's d to Plot", 
                                         value = 0.01, 
                                         min = 0, 
                                         max = 10000,
                                         step = 0.01),
                            numericInput("d_max", 
                                         label = "Maximum Cohen's d to Plot", 
                                         value = 1, 
                                         min = 0, 
                                         max = 10000,
                                         step = 0.01),
                            numericInput("num_n", 
                                         label = "Number of Sample Sizes", 
                                         value = 1, 
                                         min = 1, 
                                         max = 10000,
                                         step = 1),
                            strong("Set Sample Size(s) per Group"),
                            uiOutput("t_nMatrix"),
                            numericInput("num_alpha_t", 
                                         label = "Number of Alpha Levels", 
                                         value = 1, 
                                         min = 1, 
                                         max = 100,
                                         step = 1),
                            strong("Set Alpha Levels"),
                            uiOutput("t_alphaMatrix"),
                            actionButton("ttestBut","Generate Plot!",
                                         icon = icon("check-square"))
                            
                        ),
                        
                        box(
                            title = "t-test plot", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput('plot'),
                        )
                    )
            ),
            
            # Exact Power content
            tabItem(tabName = "ftest_tab",
                    fluidRow(
                        box(
                            title = "F-test Inputs", status = "warning", solidHeader = TRUE
                            
                        ),
                        
                        box(
                            title = "F-test Plot", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE
                            
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
    
    output$t_nMatrix <-  renderUI({matrixInput(
        "t_nMatrix",
        value = matrix(c(15), 1, 
                       input$num_n,
                       dimnames = list(c("n"))),
        rows = list(names = TRUE),
        cols = list(names = FALSE),
        copy = TRUE,
        paste = TRUE
    )
    })
    
    #t_alphaMatrix
    output$t_alphaMatrix <-  renderUI({matrixInput(
        "t_alphaMatrix",
        value = matrix(c(.05), 1, 
                       input$num_alpha_t,
                       dimnames = list(c(expression(alpha)))),
        rows = list(names = TRUE),
        cols = list(names = FALSE),
        copy = TRUE,
        paste = TRUE
    )
    })
    
    #Create set of reactive values
    values <- reactiveValues(plot_ttest = 0,
                             power_result = 0,
                             emm_output = 0)
    
    
    #Produce ANOVA design
    observeEvent(input$ttestBut, {
        values$plot_ttest = morey_plot.ttest(es = seq(input$d_min,input$d_max,.01),
                                             n=as.numeric(input$t_nMatrix),
                                             alpha_level = as.numeric(input$t_alphaMatrix),
                                             type = "two.sample",
                                             alternative = "two.sided")
    
    })
    
    #
    #Output plot of the design
    output$plot <- renderPlot({
        req(input$ttestBut)
        values$plot_ttest})
    


    
    
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Run the application
shinyApp(ui = ui, server = server)

