library(shiny)
library(scales)

# Set up the normal distributed population
u<-2 #pop meand
sigma<-5 #pop sd
x<-seq(from = -50,to = 50,length.out = 1000)
y<-dnorm(x,mean=u,sd=sigma)
ymax=1.2*max(y)

server <- function(input, output) {
  
  ## set seed so that users are likely to get different results
  set.seed(as.numeric(Sys.time()))
  
  rv <- reactiveValues(sample = NULL, 
                       mean = NULL, 
                       lower = NULL,
                       upper = NULL
                       )
  
  observeEvent(input$takeSample, 
               {
                 # store in rv
                 rv$sample <- rnorm(input$n,mean=u,sd=sigma)
                 xbar<-mean(rv$sample)
                 rv$mean <- xbar
                 # make bounds for the 95% confidence interval
                 tMultiplier = qt(0.975, df = input$n - 1)
                 se = sd(rv$sample)/sqrt(input$n)
                 margin = tMultiplier * se
                 rv$lower <- xbar - margin
                 rv$upper <- xbar + margin
                 
               }
  )
  
  
  output$plotSample <- renderPlot({
    # the underlying population
    plot(x,y,type="l",lwd=3,col="red",
         main="Distribution of Population",
         xlab="",
         ylab="y",
         ylim = c(0,ymax))
    abline(v=u,lwd=2)
    # sample and interval
    if (input$takeSample) {
      # the sample interval
      intLevel <- 0.95*ymax
      segments(x0 = rv$lower, y0 = intLevel, x1 = rv$upper, y1 = intLevel, 
               col = "green", lwd = 3)
      points(rv$mean, intLevel, col = "blue", pch = 20,cex=2)
   
    }
  })  # end plotSample
} # end server
