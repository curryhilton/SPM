
library(shiny)
library(shinythemes)

shinyUI(navbarPage("Statistical Process Measurement",
  theme = shinytheme("flatly"),
  tabPanel("Selection",

    fluidRow(
      column(4, wellPanel(
        selectInput("chart", "Control Chart",
                    c("Shewhart X-Bar", "R Chart", "s Chart"))
                         )
            ),

      column(4, wellPanel(
        uiOutput("ui")
      ))
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
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), verbatimTextOutput("sum"),
                             tableOutput("table")
                             )
               )

             )
           )


       )
)
)


