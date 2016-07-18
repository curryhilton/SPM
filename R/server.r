
library(shiny)
library(ggplot2)

options(shiny.maxRequestSize = 9*1024^2)

shinyServer(
  function(input, output){

    data <- reactive({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)

                    })

    dfx <- reactive({
      x <- data()[,1]
    })

    dfr <- reactive({
      r <- data()[,2]
    })

    dfs <- reactive({
      s <- data()[,2]
    })

    dfm <- reactive({
      m <- length(dfx())
    })

    output$file <- renderTable({
      if(is.null(data())){return()}
      input$file1
    })

    output$sum <- renderPrint({
      if(is.null(data())){return()}
      summary(data())
    })

    output$table <- renderTable({
      if(is.null(data())){return()}
      data()
    })

    output$chart_type_text <- renderText({
      input$chart
    })

    output$plot <- renderPlot({
      if(input$chart == "Shewhart X-Bar Chart, Standards Given") {
        mu <- as.numeric(input$mu)
        sd <- as.numeric(input$sd)
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        A <- L/(sqrt(n))
        uclx <- mu + A*sd
        lclx <- max(mu - A*sd,0)
        m <- seq(1:dfm())
        x <- dfx()
        plot(m, x)

      } else if (input$chart == "Shewhart X-Bar Chart (R), No Standards Given") {
        #plot()
      } else if (input$chart == "Shewhart X-Bar Chart (s), No Standards Given") {
        #plot()
      } else if (input$chart == "R Chart, Standards Given") {
        #plot()
      } else if (input$chart == "R Chart, No Standards Given") {
        #plot()
      } else if (input$chart == "s Chart, Standards Given") {
        #plot()
      } else if (input$chart == "s Chart, No Standards Given") {
        #plot()
      } else if (input$chart == "p Chart, Standards Given") {
        #plot()
      } else if (input$chart == "p Chart, No Standards Given") {
        #plot()
      } else if (input$chart == "np Chart, Standards Given") {
        #plot()
      } else if (input$chart == "np Chart, No Standards Given") {
        #plot()
      } else if (input$chart == "c Chart, Standards Given") {
        #plot()
      } else if (input$chart == "c Chart, No Standards Given") {
        #plot()
      } else if (input$chart == "CUSUM Chart, Target Given") {
        #plot()
      } else if (input$chart == "EWMA Chart, Target Given") {
        #plot()
      }

    })

  }
)

