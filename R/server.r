
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
        cl <- mu
        uclx <- cl + A*sd
        lclx <- max(cl - A*sd,0)
        m <- seq(1:dfm())
        x <- dfx()

        plot(m, x, xlab = "Subgroups", ylab = "X-bar")
          abline(h = cl, lwd = 2)
          abline(h = uclx, col = "red", lty = 2)
          abline(h = lclx, col = "red", lty = 2)

      } else if (input$chart == "Shewhart X-Bar Chart (R), No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        m <- seq(1:dfm())
        x <- dfx()                                   # define x as x in reactive expression
        r <- dfr()                                   # define r as r in reactive expression
        rbar <- mean(r)                              # calculate r-bar based on ranges of subgroups
        d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,   # define d2 control chart parameters
                2.704, 2.847, 2.970, 3.078, 3.173,   # Montgomery's textbook
                3.258, 3.336, 3.407, 3.472, 3.532,
                3.588, 3.640, 3.689, 3.735, 3.778,
                3.819, 3.858, 3.895, 3.931)

        A2 <- L/(d2[n-1]*sqrt(n))                    # calculate control chart parameter A2
        cl <- mean(x)                                # calculate centerline of x-bar chart based on mean of subgroups
        uclx <- cl + A2*rbar                         # calculate upper control chart limit for x-bar chart
        lclx <- cl - A2*rbar                         # calculate lower control chart limit for x-bar chart

        plot(m, x, xlab = "Subgroups", ylab = "X-bar", pch = 7)
        abline(h = cl, lwd = 2)
        abline(h = uclx, col = "red", lty = 2)
        abline(h = lclx, col = "red", lty = 2)

      } else if (input$chart == "Shewhart X-Bar Chart (s), No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        m <- seq(1:dfm())
        x <- dfx()                                  # define x as x in reactive expression
        s <- dfs()                                  # define s as s in reactive expression
        sbar <- mean(s)                             # calculate s-bar based on sd of subgroups
        c4 <- (4*(n-1))/(4*n-3)                     # calculate control chart constant c4
        cl <- mean(x)                               # calculate x-bar baed on mean of subgroups
        uclx <- cl + (L*sbar)/(c4*sqrt(n))          # calculate upper control chart limit for x-bar chart
        lclx <- cl - (L*sbar)/(c4*sqrt(n))          # calculate lower control chart limit for x-bar chart

        plot(m, x, xlab = "Subgroups", ylab = "X-bar", pch = 7)
        abline(h = cl, lwd = 2)
        abline(h = uclx, col = "red", lty = 2)
        abline(h = lclx, col = "red", lty = 2)

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

