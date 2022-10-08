# Morey Power Sensitivity Plots
#
# rsconnect::deployApp(here::here("Shiny","morey_plots"))

# libraries -----
library(shiny)
library(shinyjs)
library(shinydashboard)
library(Superpower)
library(rmarkdown)
library(kableExtra)
library(DT)
library(tidyverse)
library(shinyMatrix)
# functions ------
Superpower_options(verbose = FALSE,
                   plot = FALSE)

dt_output = function(title, id) {
    fluidRow(column(
        12, h5(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
        hr(), 
        DTOutput(id)
    ))
}
render_dt = function(data, server = TRUE, ...) {
    renderDT(data, selection = 'none', server = server, ...)
}

# UI -----
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
                        h5("In this tab, the plots for a F-test (ANOVA) can be created."),
                        strong("Calculating Degrees of Freedom"),
                        h5("Setting numerator degrees of freedom"),
                        helpText("For an ANOVA, it is the product of the number of levels (j) in each factor. For a main effect it would just be the number of levels minus 1.
                                     $$df_1 = \\prod_{i=1}^k j_i-1$$"),
                        h5("For example, a 2 x 2 ANOVA, would have a numerator degrees of freedom equal to 1, (2-1) x (2-1) for the interaction. The main effect(s) would also be equal to one (e.g., 2-1 = 1)."),
                        h5("Setting the denominator degrees of freedom"),
                        helpText("For an ANOVA, it is the total number of observations minus 1 and minus the sum of the numerator degrees of freedom.
                                     $$df_2 = N-1-\\sum_{i=1}^{k} df1_i $$"),
                        h5("In this case *N* would be the *total number of observations* in the sample. For example, if we have a 2 x 2 ANOVA with n=20 per group then N=80 and df=80-1-(1+1+1)=74")
                    ),              
                    box(
                        title = "NEWS",
                        status = "danger",
                        solidHeader = TRUE,
                        collapsible = FALSE,
                        strong("Current updates to Superpower's Morey Plot Shiny App"),
                        h5("This is a new function; no updates yet!") #,
                        #datatable(iris,filter = "bottom")
                    )),
            tabItem(tabName = "ttest_tab",
                    fluidRow(
                        box(
                            title = "t-test Inputs", 
                            status = "warning", 
                            solidHeader = TRUE,
                            selectInput("type_t", "Type of t-test",
                                        c("Two independent samples" = "two.sample",
                                          "One sample" = "one.sample",
                                          "Paired samples" = "paired")),
                            selectInput("alt_t", "Alternative Hypothesis",
                                        c("Two Sided/Tailed" = "two.sided",
                                          "One Sided/Tailed" = "one.sided")),
                            hr(style = "border-top: 1px solid #000000;"),
                            h5("Set Range of Effect Size"),
                            numericInput("d_min", 
                                         label = "Minimum Cohen's d", 
                                         value = 0.01, 
                                         min = 0, 
                                         max = 10000,
                                         step = 0.01),
                            numericInput("d_max", 
                                         label = "Maximum Cohen's d", 
                                         value = 1, 
                                         min = 0, 
                                         max = 10000,
                                         step = 0.01),
                            hr(style = "border-top: 1px solid #000000;"),
                            h5("Set Sample Sizes"),
                            numericInput("num_n", 
                                         label = "Number of Sample Sizes to Enter", 
                                         value = 1, 
                                         min = 1, 
                                         max = 10000,
                                         step = 1),
                            strong("Set Sample Size(s) per Group/Cell"),
                            uiOutput("t_nMatrix"),
                            hr(style = "border-top: 1px solid #000000;"),
                            h5('Set Alpha Level(s)'),
                            numericInput("num_alpha_t", 
                                         label = "Number of Alpha Levels to Enter", 
                                         value = 1, 
                                         min = 1, 
                                         max = 100,
                                         step = 1),
                            strong("Set Alpha Levels"),
                            uiOutput("t_alphaMatrix"),
                            hr(style = "border-top: 1px solid #000000;"),
                            actionButton("ttestBut","Generate Plot!",
                                         icon = icon("check-square"))
                            
                        ),
                        
                        box(
                            title = "t-test plot", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput('plot_ttest')
                        )
                        
                    ),
                    fluidRow(box(title = "t-test table", 
                                 width = 15,
                                 status = "primary", 
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 dt_output('Power-Sensitivity for a t-test', 'x1')
                    ))
            ),
            
            # Exact Power content
            tabItem(tabName = "ftest_tab",
                    fluidRow(
                        box(
                            title = "F-test Inputs", 
                            status = "warning", 
                            solidHeader = TRUE,
                            h5("Set Range of Effect Size"),
                            withMathJax(),
                            #div("Cohen's f can be derived from partial eta-squared"),
                            helpText('Cohen\'s f can be calculated from partial eta-squared:
           $$f = \\sqrt{\\frac{\\eta^2_p}{1-\\eta^2_p} }$$'),
                            numericInput("f_min", 
                                         label = "Minimum Cohen's f", 
                                         value = 0.01, 
                                         min = 0, 
                                         max = 10000,
                                         step = 0.01),
                            numericInput("f_max", 
                                         label = "Maximum Cohen's f", 
                                         value = 1, 
                                         min = 0, 
                                         max = 10000,
                                         step = 0.01),
                            hr(style = "border-top: 1px solid #000000;"),
                            h5("Set Degrees of Freedom Parameters"),
                            numericInput("num_df", 
                                         label = "Number of numerator df(s)", 
                                         value = 1, 
                                         min = 1, 
                                         max = 10000,
                                         step = 1),
                            h5("Set numerator dfs"),
                            helpText("For an ANOVA, it is the product of the number of levels in each factor (j) minus 1:
                                     $$df_1 = \\prod_{i=1}^k j_i-1$$"),
                            uiOutput("f_numMatrix"),
                            numericInput("den_df", 
                                         label = "Number of denominator df(s)", 
                                         value = 1, 
                                         min = 1, 
                                         max = 10000,
                                         step = 1),
                            h5("Set denominator dfs"),
                        helpText("For an ANOVA, it is the total number of observations minus 1, and minus the sum of number of the numerator degrees of freedom:
                                     $$df_2 = N-1-\\sum_{i=1}^{k} df1_i $$"),
                            uiOutput("f_denMatrix"),
                            hr(style = "border-top: 1px solid #000000;"),
                            h5('Set Alpha Level(s)'),
                            numericInput("num_alpha_f", 
                                         label = "Number of Alpha Levels to Enter", 
                                         value = 1, 
                                         min = 1, 
                                         max = 100,
                                         step = 1),
                            strong("Set Alpha Levels"),
                            uiOutput("f_alphaMatrix"),
                            hr(style = "border-top: 1px solid #000000;"),
                            actionButton("ftestBut","Generate Plot!",
                                         icon = icon("check-square"))
                            
                        ),
                        
                        box(
                            title = "F-test plot", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput('plot_ftest')
                        )
                        
                    ),
                    fluidRow(box(title = "F-test table", 
                                 width = 15,
                                 status = "primary", 
                                 solidHeader = TRUE,
                                 collapsible = TRUE,
                                 dt_output('Power-Sensitivity for an F-test', 'x2')
                    ))
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

# Server ------
# Define server logic
server <- function(input, output, session) {

    #output$x1 = DT::datatable(d1, editable = 'cell')
    output$t_nMatrix <-  renderUI({shinyMatrix::matrixInput(
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
    
    output$f_numMatrix <-  renderUI({shinyMatrix::matrixInput(
        "f_numMatrix",
        value = matrix(c(1), 1, 
                       input$num_df,
                       dimnames = list(c("num_df"))),
        rows = list(names = TRUE),
        cols = list(names = FALSE),
        copy = TRUE,
        paste = TRUE
    )
    })
    
    output$f_denMatrix <-  renderUI({shinyMatrix::matrixInput(
        "f_denMatrix",
        value = matrix(c(30), 1, 
                       input$den_df,
                       dimnames = list(c("den_df"))),
        rows = list(names = TRUE),
        cols = list(names = FALSE),
        copy = TRUE,
        paste = TRUE
    )
    })
    
    #t_alphaMatrix
    output$t_alphaMatrix <-  renderUI({shinyMatrix::matrixInput(
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
    # F alpha matrix
    output$f_alphaMatrix <-  renderUI({shinyMatrix::matrixInput(
        "f_alphaMatrix",
        value = matrix(c(.05), 1, 
                       input$num_alpha_f,
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
                             plot_ftest = 0,
                             emm_output = 0)
    
    
    #Produce ttest result
    observeEvent(input$ttestBut, {
        values$plot_ttest = morey_plot.ttest(es = seq(input$d_min,input$d_max,.01),
                                             n=as.numeric(input$t_nMatrix),
                                             alpha_level = as.numeric(input$t_alphaMatrix),
                                             type = input$type_t,
                                             alternative = input$alt_t)
        tab_ttest = values$plot_ttest$data %>%
            mutate(type = paste0(input$type_t,
                                 "; ",
                                 input$alt_t),
                   power = round(power,2)) %>%
            select(power,es,n,alpha,type) %>%
            rename(`Power (%)` = power,
                   `Cohen's d` = es,
                   `Sample Size per Group/Cell` = n)
        output$x1 = renderDT(tab_ttest, selection = 'none',
                              filter = "bottom",
                              rownames = FALSE)
    
    })
    
    # F-test plot button action
    observeEvent(input$ftestBut, {
        values$plot_ftest = morey_plot.ftest(es = seq(input$f_min,input$f_max,.01),
                                             num_df = as.numeric(input$f_numMatrix),
                                             den_df = as.numeric(input$f_denMatrix),
                                             alpha_level = as.numeric(input$f_alphaMatrix))
        
        tab_ftest = values$plot_ftest$data %>%
            mutate(power = round(power,2)) %>%
            select(power,cohen_f,num_df,den_df,alpha) %>%
            rename(`Power (%)` = power,
                   `Cohen's f` = cohen_f)
        output$x2 = renderDT(tab_ftest, 
                             selection = 'none',
                             filter = "bottom",
                             rownames = FALSE)
        
    })
    
    #
    #Output plot of the design
    output$plot_ttest <- renderPlot({
        req(input$ttestBut)
        values$plot_ttest})
    output$plot_ftest <- renderPlot({
        req(input$ftestBut)
        values$plot_ftest})
    


    
    
}

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# Run the application
shinyApp(ui = ui, server = server)

