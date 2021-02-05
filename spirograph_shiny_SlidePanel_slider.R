library(shiny)
library(ggplot2)
library(plotly)
library(tibble)

spiro <- function(n1,n2,n3) {
  t <- seq(0,1,length.out=1000)
  z <- exp(1i*2*pi*n1*t) + exp(1i*2*pi*n2*t) + exp(1i*2*pi*n3*t)
  result <- tibble(x=Re(z),y=Im(z))
  return (result)
}

# Define UI for application that draws a spirograph
ui <- fluidPage(

  # Application title
  titlePanel("Spirograph"),

  sidebarLayout(

    sidebarPanel(
      # input sliders
      numericInput(inputId="n1",label="n1",value=13, step=1.5),
      numericInput(inputId="n2",label="n2",value=-7),
      numericInput(inputId="n3",label="n3",value=-3)
    ),

    mainPanel(
      # output plot
      plotlyOutput("spirograph")
    )
  )
  
)

# Define server logic required to draw a spirograph
server <- function(input, output) {
  output$spirograph <- renderPlotly({
    result <- spiro(as.integer(input$n1),as.integer(input$n2),as.integer(input$n3))
    ggplot(data=result,aes(x=x,y=y)) +
        geom_path() +
        xlab("Real(z)") +
        ylab("Imag(z)")
  })
}

# Run the application
shinyApp(ui,server)
