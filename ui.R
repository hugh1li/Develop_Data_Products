ui<-fluidPage(
  titlePanel("95% Confidence Interval Simulation for Normal Distribution"),
  sidebarPanel(
    sliderInput(inputId="n","Sample Size n",value=11,min=2,max=50,step=1),
    actionButton("takeSample","Sample Now")
  ),
  mainPanel(
    plotOutput("plotSample")
  )
)