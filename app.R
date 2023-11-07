#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gganimate)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pour Point as a Total Flux"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(width = 4, wellPanel(
      sliderInput("tfc",
                  "Percentage of Rainfall as Throughfall:", 
                  min = 5, 
                  max = 95, 
                  value = 80),
      sliderInput("ntf",
                  "Number of Throughfall:",
                  min = 5,
                  max = 100,
                  value = 10),
      sliderInput("rfc",
                  "Depth times Rainfall:",
                  min = 1.5,
                  max = 15,
                  value = 4.0),
      sliderInput("npp",
                  "Number of Pour Points:",
                  min = 1,
                  max = 10,
                  value = 1)
    )),
    column(width = 4,
           plotOutput("ppflx")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$ppflx <- renderPlot({
    # generate bins based on input$bins from ui.R
    pptf <- (((input$ntf-input$npp)*input$tfc)+(input$npp*input$rfc*100))/input$ntf
    top <- data.frame(x = c("Pour Point + TF", "Background Throughall"),y= c(pptf,input$tfc))
    ggplot(top, aes(x,y)) + geom_bar(stat = "identity") + geom_text(aes(label=y), vjust=0) + 
      theme_classic() + ylab("% of Rainfall") + xlab(element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
