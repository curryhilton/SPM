
library(shiny)

shinyUI(fluidPage(

  titlePanel(title = "Statistical Process Measurement"),
  sidebarLayout(
    sidebarPanel(
                 h4("Parameters"),
                 textInput("mu", "MU", "10"),
                 textInput("sd", "Standard Deviation", "1"),
                 sliderInput("l", "L", min = 0, max=8, value = 3),
                 sliderInput("m", "Rational Subgroups (m)", min = 0, max=100, value = 25),
                 sliderInput("n", "Sample Size (n)", min = 0, max=25, value = 5)
    ),
    mainPanel(
              tabsetPanel(type="tabs",
                tabPanel("X-Bar, s", plotOutput("xbar_s", width = "100%", height = "500px")),
                tabPanel("X-Bar, R", plotOutput("xbar_r", width = "100%", height = "500px"))
              ),
              plotOutput("oc", width = "100%", height = "275px"))

  )
)

)
