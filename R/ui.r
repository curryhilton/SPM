
library(shiny)
library(shinythemes)

shinyUI(navbarPage("Statistical Process Measurement",
  theme = shinytheme("flatly"),
  tabPanel("Selection",

    fluidRow(
      column(8, wellPanel(
        selectInput("chart", "Control Chart",
                    c("Shewhart X-Bar Chart, Standards Given",
                      "Shewhart X-Bar Chart (R), No Standards Given",
                      "Shewhart X-Bar Chart (s), No Standards Given",
                      "Shewhart X-Bar Chart (MR), No Standards Given",
                      "R Chart, Standards Given",
                      "R Chart, No Standards Given",
                      "s Chart, Standards Given",
                      "s Chart, No Standards Given",
                      "p Chart, Standards Given",
                      "p Chart, No Standards Given",
                      "np Chart, Standards Given",
                      "np Chart, No Standards Given",
                      "u Chart, Standards Given",
                      "u Chart, No Standards Given",
                      "c Chart, Standards Given",
                      "c Chart, No Standards Given",
                      "EWMA",
                      "CUSUM"
                      ))
                         )
            )

          ),

    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose file to Upload',
                  accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )
               ),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ','),
        radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
        tags$hr(),
        p('Upload data frame with subgroup means and/or standard deviations/ranges')

          ),

        mainPanel(
            verbatimTextOutput("sum"),
            fluidRow(
              splitLayout(cellWidths = c("30%", "70%"),
                          tableOutput("table"),
                          plotOutput("hist")
              )
            )
                 )

             )

       ),

  tabPanel("Monitoring",
    sidebarLayout(
      sidebarPanel(
        h4("Parameters"),
        textInput("mu", "Mean/Proportion", "10"),
        textInput("sd", "Standard Deviation/Range", "1"),
        sliderInput("l", "L", min = 0, max=5, value = 3, step = 0.01),
        sliderInput("n", "Sample Size (n)", min = 0, max=25, value = 5)
                  ),

    mainPanel(
      tags$p("Control Chart:"),
      verbatimTextOutput("chart_type_text"),
      plotOutput("plot", width = "100%", height = "400px")

             )
                  )
          ),

  tabPanel("Performance")

  )
)


