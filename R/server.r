
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

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclx, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclx, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(x > uclx | x < lclx)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, x, xlab = "Subgroups", ylab = "X-bar", ylim = c(min(x) - sd, max(x) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclx, col = "red", lty = 2)
        abline(h = lclx, col = "red", lty = 2)
        points(x, pch = 20, type = "b", col = ifelse(x > uclx | x < lclx, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

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
        sd <- rbar/d2[n-1]                           # calculate sd

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclx, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclx, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(x > uclx | x < lclx)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, x, xlab = "Subgroups", ylab = "X-bar", pch = 7, type = "b", ylim = c(min(x) - rbar, max(x) + rbar))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclx, col = "red", lty = 2)
        abline(h = lclx, col = "red", lty = 2)
        points(x, pch = 20, type = "b", col = ifelse(x > uclx | x < lclx, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

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
        sd <- sbar/c4                               # calculate process sd

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclx, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclx, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(x > uclx | x < lclx)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, x, xlab = "Subgroups", ylab = "X-bar", pch = 7, type = "b", ylim = c(min(x) - sbar, max(x) + sbar))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclx, col = "red", lty = 2)
        abline(h = lclx, col = "red", lty = 2)
        points(x, pch = 20, type = "b", col = ifelse(x > uclx | x < lclx, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "R Chart, Standards Given") {
        sd <- as.numeric(input$sd)
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        m <- seq(1:dfm())
        r <- dfx()                                  # define x as r in reactive expression
        d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
                2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
                3.258, 3.336, 3.407, 3.472, 3.532,
                3.588, 3.640, 3.689, 3.735, 3.778,
                3.819, 3.858, 3.895, 3.931)
        d3 <- c(0.853, 0.888, 0.880, 0.864, 0.848,  # define d3 control chart parameters
                0.833, 0.820, 0.808, 0.797, 0.787,  # Montgomery's textbook
                0.778, 0.770, 0.763, 0.756, 0.750,
                0.744, 0.739, 0.734, 0.729, 0.724,
                0.720, 0.716, 0.712, 0.708)
        D1 = d2[n-1] - L*d3[n-1]        # calculate control chart constant D1
        D2 = d2[n-1] + L*d3[n-1]        # calculate control chart constant D2
        cl <- d2[n-1]*sd                # calculate centerline of R-bar chart
        uclr <- D2*sd                   # calculate upper control chart limit for R-bar chart
        lclr <- max(D1*sd, 0)           # calculate lower control chart limit for R-bar chart

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclr, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclr, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(r > uclr | r < lclr)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, r, xlab = "Subgroups", ylab = "X-bar", pch = 7, type = "b", ylim = c(min(r) - sd, max(r) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclr, col = "red", lty = 2)
        abline(h = lclr, col = "red", lty = 2)
        points(r, pch = 20, type = "b", col = ifelse(r > uclr | r < lclr, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "R Chart, No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        m <- seq(1:dfm())
        r <- dfx()
        d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
                2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
                3.258, 3.336, 3.407, 3.472, 3.532,
                3.588, 3.640, 3.689, 3.735, 3.778,
                3.819, 3.858, 3.895, 3.931)
        d3 <- c(0.853, 0.888, 0.880, 0.864, 0.848,  # define d3 control chart parameters
                0.833, 0.820, 0.808, 0.797, 0.787,  # Montgomery's textbook
                0.778, 0.770, 0.763, 0.756, 0.750,
                0.744, 0.739, 0.734, 0.729, 0.724,
                0.720, 0.716, 0.712, 0.708)
        D3 = 1-L*(d3[n-1]/d2[n-1])                  # calculate control chart constant D3
        D4 = 1+L*(d3[n-1]/d2[n-1])                  # calculate control chart constant D4
        cl <- mean(r)                               # calculate centerline of R-bar chart
        uclr <- D4*cl                               # calculate upper control chart limit for R-bar chart
        lclr <- max(D3*cl, 0)                       # calculate lower control chart limit for R-bar chart
        sd <- cl/d2[n-1]

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclr, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclr, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(r > uclr | r < lclr)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, r, xlab = "Subgroups", ylab = "X-bar", pch = 7, type = "b", ylim = c(min(r) - sd, max(r) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclr, col = "red", lty = 2)
        abline(h = lclr, col = "red", lty = 2)
        points(r, pch = 20, type = "b", col = ifelse(r > uclr | r < lclr, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "s Chart, Standards Given") {
        sd <- as.numeric(input$sd)
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        s <- dfx()
        m <- seq(1:dfm())
        c4 <- (4*(n-1))/(4*n-3)                    # calculate control chart constant c4
        cl <- c4*sd                                # calculate centerline of the s chart
        ucls <- c4*sd + L*sd*sqrt(1-c4^2)          # calculate upper control chart limit for the s chart
        lcls <- max(c4*sd - L*sd*sqrt(1-c4^2), 0)  # calculate lower control chart limit for the s chart

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lcls, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(ucls, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(s > ucls | s < lcls)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, s, xlab = "Subgroups", ylab = "s", pch = 7, type = "b", ylim = c(min(s) - sd, max(s) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = ucls, col = "red", lty = 2)
        abline(h = lcls, col = "red", lty = 2)
        points(s, pch = 20, type = "b", col = ifelse(s > ucls | s < lcls, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "s Chart, No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        s <- dfx()
        m <- seq(1:dfm())
        c4 <- (4*(n-1))/(4*n-3)          # calculate control chart constant c4
        B3 = 1-(L/c4)*(sqrt(1-c4^2))     # calculate control chart constant B3
        B4 = 1+(L/c4)*(sqrt(1-c4^2))     # calculate control chart constant B4
        cl <- mean(s)                    # calculate centerline of s chart
        ucls <- B4*cl                    # calculate upper control chart limit for s chart
        lcls <- max(B3*cl, 0)            # calculate lower control chart limit for s chart
        sd <- cl/c4                      # calculate standard deviation for porcess data

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lcls, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(ucls, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(s > ucls | s < lcls)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, s, xlab = "Subgroups", ylab = "s", pch = 7, type = "b", ylim = c(min(s) - sd, max(s) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = ucls, col = "red", lty = 2)
        abline(h = lcls, col = "red", lty = 2)
        points(s, pch = 20, type = "b", col = ifelse(s > ucls | s < lcls, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "p Chart, Standards Given") {
        mu <- as.numeric(input$mu)
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        p <- dfx()
        m <- seq(1:dfm())
        cl <- mu                            # define p based on user input
        uclp <- cl + L*sqrt((cl*(1-cl))/n)  # calculate upper control chart limit for p chart
        lclp <- max(cl - L*sqrt((cl*(1-cl))/n), 0)  # calculate lower control chart limit for p chart
        sd <- sd(p)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclp, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclp, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(p > uclp | p < lclp)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, p, xlab = "Subgroups", ylab = "p", pch = 7, type = "b", ylim = c(min(p) - 3*sd, max(p) + 3*sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclp, col = "red", lty = 2)
        abline(h = lclp, col = "red", lty = 2)
        points(p, pch = 20, type = "b", col = ifelse(p > uclp | p < lclp, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "p Chart, No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        p <- dfx()
        m <- seq(1:dfm())
        cl <- mean(p)                                  # define p based on user input
        uclp <- cl + L*sqrt((cl*(1-cl))/n)             # calculate upper control chart limit for p chart
        lclp <- max(cl - L*sqrt((cl*(1-cl))/n), 0)     # calculate lower control chart limit for p chart
        sd <- sd(p)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclp, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclp, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(p > uclp | p < lclp)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, p, xlab = "Subgroups", ylab = "p", pch = 7, type = "b", ylim = c(min(p) - 3*sd, max(p) + 3*sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclp, col = "red", lty = 2)
        abline(h = lclp, col = "red", lty = 2)
        points(p, pch = 20, type = "b", col = ifelse(p > uclp | p < lclp, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "np Chart, Standards Given") {
        mu <- as.numeric(input$mu)
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        np <- n*dfx()
        m <- seq(1:dfm())
        cl <- n*mu
        uclnp <- cl + L*sqrt(cl*(1-mu))              # calculate upper control chart limit for np chart
        lclnp <- max(cl - L*sqrt(cl*(1-mu)), 0)      # calculate lower control chart limit for np chart
        sd <- sd(np)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclnp, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclnp, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(np > uclnp | np < lclnp)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, np, xlab = "Subgroups", ylab = "np", pch = 7, type = "b", ylim = c(min(np) - sd, max(np) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclnp, col = "red", lty = 2)
        abline(h = lclnp, col = "red", lty = 2)
        points(np, pch = 20, type = "b", col = ifelse(np > uclnp | np < lclnp, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "np Chart, No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        p <- dfx()
        np <- n*p
        m <- seq(1:dfm())
        cl <- mean(np)
        uclnp <- cl + L*sqrt(cl*(1-mean(p)))              # calculate upper control chart limit for np chart
        lclnp <- max(cl - L*sqrt(cl*(1-mean(p))), 0)      # calculate lower control chart limit for np chart
        sd <- sd(np)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclnp, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclnp, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(np > uclnp | np < lclnp)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, np, xlab = "Subgroups", ylab = "np", pch = 7, type = "b", ylim = c(min(np) - sd, max(np) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclnp, col = "red", lty = 2)
        abline(h = lclnp, col = "red", lty = 2)
        points(np, pch = 20, type = "b", col = ifelse(np > uclnp | np < lclnp, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "c Chart, Standards Given") {
        mu <- as.numeric(input$mu)
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        c <- dfx()
        m <- seq(1:dfm())
        cl <- mu                            # define c
        uclc <- cl + L*sqrt(cl)             # calculate upper control chart limit for c chart
        lclc <- cl - L*sqrt(cl)             # calculate lower control chart limit for c chart
        sd <- sd(c)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclc, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclc, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(c > uclc | c < lclc)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, c, xlab = "Subgroups", ylab = "c", pch = 7, type = "b", ylim = c(min(c) - sd, max(c) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclc, col = "red", lty = 2)
        abline(h = lclc, col = "red", lty = 2)
        points(c, pch = 20, type = "b", col = ifelse(c > uclc | c < lclc, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "c Chart, No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        c <- dfx()
        m <- seq(1:dfm())
        cl <- mean(c)                       # define c
        uclc <- cl + L*sqrt(cl)             # calculate upper control chart limit for c chart
        lclc <- cl - L*sqrt(cl)             # calculate lower control chart limit for c chart
        sd <- sd(c)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclc, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclc, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(c > uclc | c < lclc)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, c, xlab = "Subgroups", ylab = "c", pch = 7, type = "b", ylim = c(min(c) - sd, max(c) + sd))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclc, col = "red", lty = 2)
        abline(h = lclc, col = "red", lty = 2)
        points(c, pch = 20, type = "b", col = ifelse(c > uclc | c < lclc, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "Shewhart X-Bar Chart (MR), No Standards Given") {
        L <- as.numeric(input$l)
        n <- as.numeric(input$n)
        m <- seq(1:dfm())
        x <- dfx()                                    # define x as x in reactive expression
        mr <- rep(NA, n)                              # function for MR
        for(j in 1:n+1){
          mr[j] = abs(x[j]-x[j-1])
        }
        mrbar <- mean(mr, na.rm = T)                  # calculate the mean of MR
        d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,    # define d2 control chart parameters
                2.704, 2.847, 2.970, 3.078, 3.173,    # Montgomery's textbook
                3.258, 3.336, 3.407, 3.472, 3.532,
                3.588, 3.640, 3.689, 3.735, 3.778,
                3.819, 3.858, 3.895, 3.931)
        cl <- mean(x)                                 # calculate the centerline for the IMR chart
        uclmr <- cl + L*(mrbar/d2[n-1])               # calculate upper control chart limit for IMR chart
        lclmr <- cl - L*(mrbar/d2[n-1])               # calculate lower control chart limit for IMR chart
        sd <- sd(x)

        size <- paste("Subgroup Size =", n)
        LCL <- paste("LCL =", round(lclmr, 2))
        CL <- paste("Center Line", round(cl, 2))
        UCL <- paste("UCL =", round(uclmr, 2))
        sub <- paste("Subgroups =", length(m))
        stdev <- paste("Standard Deviation =", round(sd, 2))
        count <- paste("Violations =", length(which(x > uclmr | x < lclmr)))

        par(bg="lightsteelblue2", mar = c(10, 5, 2, 2))
        plot(m, x, xlab = "Subgroups", ylab = "X-bar", pch = 7, type = "b", ylim = c(min(x) - mrbar, max(x) + mrbar))
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        abline(h = cl, lwd = 2)
        abline(h = uclmr, col = "red", lty = 2)
        abline(h = lclmr, col = "red", lty = 2)
        points(x, pch = 20, type = "b", col = ifelse(x > uclmr | x < lclmr, "red", "black"))
        mtext(size, at = m[1] + 1, side = 1, line = 5, adj = 0, font = 2)
        mtext(LCL, at = m[1] + 1, side = 1, line = 6, adj = 0, font = 2)
        mtext(CL, at = m[1] + 1, side = 1, line = 7, adj = 0, font = 2)
        mtext(UCL, at = m[1] + 1, side = 1, line = 8, adj = 0, font = 2)
        mtext(stdev, at = m[length(m)] - 1, side = 1, line = 5, adj = 1, font = 2)
        mtext(sub, at = m[length(m)] - 1, side = 1, line = 6, adj = 1, font = 2)
        mtext(count, at = m[length(m)] - 1, side = 1, line = 7, adj = 1, font = 2)

      } else if (input$chart == "EWMA Chart, Target Given") {
        #plot()

      } else if (input$chart == "CUSUM Chart, Target Given") {
        #plot()
      }
    })

  }
)

