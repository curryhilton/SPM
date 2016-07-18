tabPanel("X-Bar, s", plotOutput("xbar_s", width = "100%", height = "500px")),
tabPanel("X-Bar, R", plotOutput("xbar_r", width = "100%", height = "500px"))

library(shiny)
library(ggplot2)

options(shiny.maxRequestSize = 9*1024^2)

shinyServer(
  function(input, output){

    output$ui <- renderUI({
      if (is.null(input$chart))
        return()

      switch(input$chart,
             "Shewhart X-Bar Chart" = radioButtons("options", "Options",
                                                   choices = c("Both Standards Given",
                                                               "No Standards Given (R)",
                                                               "No Standards Given (s)")
             ),

             "R Chart" = radioButtons("options", "Options",
                                      choices = c("Standards Given",
                                                  "No Standards Given")
             ),

             "s Chart" = radioButtons("options", "Options",
                                      choices = c("Standards Given",
                                                  "No Standards Given")
             ),

             "p Chart" = radioButtons("options", "Options",
                                      choices = c("Standards Given",
                                                  "No Standards Given")
             ),

             "np Chart" = radioButtons("options", "Options",
                                       choices = c("Standards Given",
                                                   "No Standards Given")
             ),

             "c Chart" = radioButtons("options", "Options",
                                      choices = c("Standards Given",
                                                  "No Standards Given")
             ),

             "CUSUM Chart" = radioButtons("options", "Options",
                                          choices = c("Target Given")
             ),

             "EWMA Chart" = radioButtons("options", "Options",
                                         choices = c("Target Given")
             )
      )

    })

    output$chart_type_text <- renderText({
      input$chart
    })

    output$options <- renderText({
      input$options
    })

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

    output$norm <- renderPlot({
      if(is.null(dfx())){return()}
      qqnorm(dfx(), main = "Normal Probability Plot - X-bar")
      qqline(dfx())
    })

    output$uic <- renderUI({
      if (is.null(input$options))
        return()

      switch(input$options,
             output$xbar-kk <- renderPlot({
               mu <- as.numeric(input$mu)
               sd <- as.numeric(input$sd)
               L <- as.numeric(input$l)
               n <- as.numeric(input$n)
               m <- length(dfx())
               A <- L/(sqrt(n))
               uclx <- mu + A*sd
               lclx <- max(mu - A*sd,0)
               x_data <- data.frame(dfx, seq(1:m))

               xbar <- ggplot(x_data, aes(mn, x))+
                 geom_point(size=2, aes(color=x>uclx | x<lclx)) +
                 scale_colour_manual(values=c("black", "red")) +
                 guides(colour=FALSE) +
                 geom_hline(yintercept=mu) +
                 geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
                 geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
                 labs(x="Subgroup",y="X-Bar") +
                 scale_x_discrete(limits=seq(1:m))+
                 ggtitle("X-Bar Chart") +
                 theme(plot.title = element_text(size = 20))

               xbar
             })

      )
    })

    output$xbar_s <- renderPlot({
      mu <- as.numeric(input$mu)
      sd <- as.numeric(input$sd)
      L <- as.numeric(input$l)
      m <- as.numeric(input$m)
      n <- as.numeric(input$n)
      A <- L/(sqrt(n))
      uclx <- mu + A*sd
      lclx <- max(mu - A*sd,0)
      c4 <- (4*(n-1))/(4*n-3)
      B6 <- c4 + L/(sqrt(2*(n-1)))
      B5 <- c4 - L/(sqrt(2*(n-1)))
      ucls <- B6*sd
      lcls <- max(0,B5*sd)
      cl <- c4*sd
      x <- rnorm(m, mean=mu, sd=sd)
      mn <- seq(1:m)
      x_data <- data.frame(x,mn)
      sd_s <- (ucls-lcls)/(sd*L*2)
      s <- rnorm(m, mean=cl, sd=sd_s)
      s_data <- data.frame(s, mn)

      xbar <- ggplot(x_data, aes(mn, x))+
        geom_point(size=2, aes(color=x>uclx | x<lclx)) +
        scale_colour_manual(values=c("black", "red")) +
        guides(colour=FALSE) +
        geom_hline(yintercept=mu) +
        geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
        geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
        labs(x="Subgroup",y="X-Bar") +
        scale_x_discrete(limits=seq(1:m))+
        ggtitle("X-Bar Chart") +
        theme(plot.title = element_text(size = 20))

      s <- ggplot(s_data, aes(mn, s)) +
        geom_point(size=2) +
        geom_hline(yintercept=cl) +
        geom_hline(yintercept=ucls, linetype="dashed", color = "red") +
        geom_hline(yintercept=lcls, linetype="dashed", color = "red") +
        labs(x="Subgroup",y="s") +
        scale_x_discrete(limits=seq(1:m))+
        ggtitle("S Chart") +
        theme(plot.title = element_text(size = 20))

      grid.arrange(xbar, s, ncol = 1)
    })


    output$xbar_r <- renderPlot({
      mu <- as.numeric(input$mu)
      sd <- as.numeric(input$sd)
      L <- as.numeric(input$l)
      m <- as.numeric(input$m)
      n <- as.numeric(input$n)
      A <- L/sqrt(n)
      uclx <- mu + A*sd
      lclx <- max(mu - A*sd,0)
      nk <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
              24, 25)
      d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.97, 3.078, 3.173, 3.258, 3.336,
              3.407, 3.472, 3.532, 3.588, 3.64, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931)
      d3 <- c(0.853, 0.888, 0.88, 0.864, 0.848, 0.833, 0.82, 0.808, 0.797, 0.787, 0.778, 0.77, 0.763,
              0.756, 0.75, 0.744, 0.739, 0.734, 0.729, 0.724, 0.72, 0.716, 0.712, 0.708)
      table <- data.frame(nk, d2, d3)
      D1 <- table[n-1,2]-L*table[n-1,3]
      D2 <- table[n-1,2]+L*table[n-1,3]
      uclr <- D2*sd
      lclr <- max(0,D1*sd)
      clr <- table[n-1,2]*sd
      x <- rnorm(m, mean=mu, sd=sd)
      mn <- seq(1:m)
      x_data <- data.frame(x,mn)
      r_s <- (uclr-lclr)/(sd*L*2)
      r <- rnorm(m, mean=clr, sd=r_s)
      r_data <- data.frame(r, mn)

      xbar <- ggplot(x_data, aes(mn, x))+
        geom_point(size=2, aes(color=x>uclx | x<lclx)) +
        scale_colour_manual(values=c("black", "red")) +
        guides(colour=FALSE) +
        geom_hline(yintercept=mu) +
        geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
        geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
        labs(x="Subgroup",y="X-Bar") +
        scale_x_discrete(limits=seq(1:m))+
        ggtitle("X-Bar Chart") +
        theme(plot.title = element_text(size = 20))

      r <- ggplot(r_data, aes(mn, r))+
        geom_point(size=2) +
        geom_hline(yintercept=clr) +
        geom_hline(yintercept=uclr, linetype="dashed", color = "red") +
        geom_hline(yintercept=lclr, linetype="dashed", color = "red") +
        labs(x="Subgroup",y="R") +
        scale_x_discrete(limits=seq(1:m))+
        ggtitle("R Chart")+
        theme(plot.title = element_text(size = 20))

      grid.arrange(xbar, r, ncol = 1)
    })


    output$oc <- renderPlot({
      mu <- as.numeric(input$mu)
      sd <- as.numeric(input$sd)
      L <- as.numeric(input$l)
      n <- as.numeric(input$n)
      m <- as.numeric(input$m)
      k <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
      beta <- pnorm(L-k*sqrt(n),0,1)-pnorm(-L-k*sqrt(n))
      x <- rnorm(m, mean=mu, sd=sd)
      mn <- seq(1:m)
      xx <- data.frame(x, mn)
      A <- L/sqrt(n)
      uclx <- mu + A*sd
      lclx <- max(mu - A*sd,0)
      oc_data <- data.frame(beta, k)
      far <- round(pnorm((lclx-mu)/(sd/sqrt(n)), 0, 1) + pnorm((uclx-mu)/(sd/sqrt(n)), 0, 1, lower.tail = FALSE), 5)
      arl <- round(1/far, 5)
      Metric <- c("UCL", "CL", "LCL", "FAR", "ARL")
      Value <- c(uclx, mu, lclx, far, arl)
      t <-data.frame(Metric, Value)

      oc <- ggplot(oc_data, aes(k, beta)) +
        stat_smooth() +
        ggtitle("OC Function") +
        theme(plot.title = element_text(size = 20))

      qq <- ggplot(data=xx, aes(sample=x)) +
        stat_qq() +
        ggtitle("QQ - Plot") +
        theme(plot.title = element_text(size = 20))

      tb <- tableGrob(t)

      grid.arrange(oc, qq, tb, ncol=3)

    })

  }
)



column(4, wellPanel(
  uiOutput("ui")
))

mainPanel(
  fluidRow(
    splitLayout(cellWidths = c("50%", "50%"), verbatimTextOutput("sum"),
                tableOutput("table")
    )
  )

)

xbar <- ggplot(x_df, aes(x = scale, y = x))+
  geom_point(size=2, aes(color=x>uclx | x<lclx)) +
  scale_colour_manual(values=c("black", "red")) +
  guides(colour=FALSE) +
  geom_hline(yintercept=mu) +
  geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
  geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
  labs(x="Subgroup",y="X-Bar") +
  scale_x_discrete(limits=scale)+
  ggtitle("X-Bar Chart") +
  theme(plot.title = element_text(size = 20))

xbar

###########

output$chart_type_text <- renderText({
  input$chart
})

output$uic <- renderUI({
  if (is.null(input$options))
    return()

  switch(input$options,
         output$xbar-kk <- renderPlot({
           mu <- as.numeric(input$mu)
           sd <- as.numeric(input$sd)
           L <- as.numeric(input$l)
           n <- as.numeric(input$n)
           m <- length(dfx())
           A <- L/(sqrt(n))
           uclx <- mu + A*sd
           lclx <- max(mu - A*sd,0)
           x_data <- data.frame(dfx, seq(1:m))

           xbar <- ggplot(x_data, aes(mn, x))+
             geom_point(size=2, aes(color=x>uclx | x<lclx)) +
             scale_colour_manual(values=c("black", "red")) +
             guides(colour=FALSE) +
             geom_hline(yintercept=mu) +
             geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
             geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
             labs(x="Subgroup",y="X-Bar") +
             scale_x_discrete(limits=seq(1:m))+
             ggtitle("X-Bar Chart") +
             theme(plot.title = element_text(size = 20))

           xbar
         })

  )
})

output$xbar_s <- renderPlot({
  mu <- as.numeric(input$mu)
  sd <- as.numeric(input$sd)
  L <- as.numeric(input$l)
  m <- as.numeric(input$m)
  n <- as.numeric(input$n)
  A <- L/(sqrt(n))
  uclx <- mu + A*sd
  lclx <- max(mu - A*sd,0)
  c4 <- (4*(n-1))/(4*n-3)
  B6 <- c4 + L/(sqrt(2*(n-1)))
  B5 <- c4 - L/(sqrt(2*(n-1)))
  ucls <- B6*sd
  lcls <- max(0,B5*sd)
  cl <- c4*sd
  x <- rnorm(m, mean=mu, sd=sd)
  mn <- seq(1:m)
  x_data <- data.frame(x,mn)
  sd_s <- (ucls-lcls)/(sd*L*2)
  s <- rnorm(m, mean=cl, sd=sd_s)
  s_data <- data.frame(s, mn)

  xbar <- ggplot(x_data, aes(mn, x))+
    geom_point(size=2, aes(color=x>uclx | x<lclx)) +
    scale_colour_manual(values=c("black", "red")) +
    guides(colour=FALSE) +
    geom_hline(yintercept=mu) +
    geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
    geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="X-Bar") +
    scale_x_discrete(limits=seq(1:m))+
    ggtitle("X-Bar Chart") +
    theme(plot.title = element_text(size = 20))

  s <- ggplot(s_data, aes(mn, s)) +
    geom_point(size=2) +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucls, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcls, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="s") +
    scale_x_discrete(limits=seq(1:m))+
    ggtitle("S Chart") +
    theme(plot.title = element_text(size = 20))

  grid.arrange(xbar, s, ncol = 1)
})


output$xbar_r <- renderPlot({
  mu <- as.numeric(input$mu)
  sd <- as.numeric(input$sd)
  L <- as.numeric(input$l)
  m <- as.numeric(input$m)
  n <- as.numeric(input$n)
  A <- L/sqrt(n)
  uclx <- mu + A*sd
  lclx <- max(mu - A*sd,0)
  nk <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
          24, 25)
  d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534, 2.704, 2.847, 2.97, 3.078, 3.173, 3.258, 3.336,
          3.407, 3.472, 3.532, 3.588, 3.64, 3.689, 3.735, 3.778, 3.819, 3.858, 3.895, 3.931)
  d3 <- c(0.853, 0.888, 0.88, 0.864, 0.848, 0.833, 0.82, 0.808, 0.797, 0.787, 0.778, 0.77, 0.763,
          0.756, 0.75, 0.744, 0.739, 0.734, 0.729, 0.724, 0.72, 0.716, 0.712, 0.708)
  table <- data.frame(nk, d2, d3)
  D1 <- table[n-1,2]-L*table[n-1,3]
  D2 <- table[n-1,2]+L*table[n-1,3]
  uclr <- D2*sd
  lclr <- max(0,D1*sd)
  clr <- table[n-1,2]*sd
  x <- rnorm(m, mean=mu, sd=sd)
  mn <- seq(1:m)
  x_data <- data.frame(x,mn)
  r_s <- (uclr-lclr)/(sd*L*2)
  r <- rnorm(m, mean=clr, sd=r_s)
  r_data <- data.frame(r, mn)

  xbar <- ggplot(x_data, aes(mn, x))+
    geom_point(size=2, aes(color=x>uclx | x<lclx)) +
    scale_colour_manual(values=c("black", "red")) +
    guides(colour=FALSE) +
    geom_hline(yintercept=mu) +
    geom_hline(yintercept=uclx, linetype="dashed", color = "red") +
    geom_hline(yintercept=lclx, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="X-Bar") +
    scale_x_discrete(limits=seq(1:m))+
    ggtitle("X-Bar Chart") +
    theme(plot.title = element_text(size = 20))

  r <- ggplot(r_data, aes(mn, r))+
    geom_point(size=2) +
    geom_hline(yintercept=clr) +
    geom_hline(yintercept=uclr, linetype="dashed", color = "red") +
    geom_hline(yintercept=lclr, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="R") +
    scale_x_discrete(limits=seq(1:m))+
    ggtitle("R Chart")+
    theme(plot.title = element_text(size = 20))

  grid.arrange(xbar, r, ncol = 1)
})


output$oc <- renderPlot({
  mu <- as.numeric(input$mu)
  sd <- as.numeric(input$sd)
  L <- as.numeric(input$l)
  n <- as.numeric(input$n)
  m <- as.numeric(input$m)
  k <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
  beta <- pnorm(L-k*sqrt(n),0,1)-pnorm(-L-k*sqrt(n))
  x <- rnorm(m, mean=mu, sd=sd)
  mn <- seq(1:m)
  xx <- data.frame(x, mn)
  A <- L/sqrt(n)
  uclx <- mu + A*sd
  lclx <- max(mu - A*sd,0)
  oc_data <- data.frame(beta, k)
  far <- round(pnorm((lclx-mu)/(sd/sqrt(n)), 0, 1) + pnorm((uclx-mu)/(sd/sqrt(n)), 0, 1, lower.tail = FALSE), 5)
  arl <- round(1/far, 5)
  Metric <- c("UCL", "CL", "LCL", "FAR", "ARL")
  Value <- c(uclx, mu, lclx, far, arl)
  t <-data.frame(Metric, Value)

  oc <- ggplot(oc_data, aes(k, beta)) +
    stat_smooth() +
    ggtitle("OC Function") +
    theme(plot.title = element_text(size = 20))

  qq <- ggplot(data=xx, aes(sample=x)) +
    stat_qq() +
    ggtitle("QQ - Plot") +
    theme(plot.title = element_text(size = 20))

  tb <- tableGrob(t)

  grid.arrange(oc, qq, tb, ncol=3)

})

output$norm <- renderPlot({
  if(is.null(dfx())){return()}
  qqnorm(dfx(), main = "Normal Probability Plot - X-bar")
  qqline(dfx())
})
