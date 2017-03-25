#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fma)
library(dygraphs)
library(magrittr)

##Define utility transformation function
transformation <-function(x, ts){
  ts <- eval(parse(text = ts))
  if(x ==1){
    data<- BoxCox(ts, lambda = .3)
    }
    else if(x == 2){
    data <- log(ts)
    }else if(x == 3){
    data <- log10(ts)
    }else if(x == 4){
    data <- diff(ts)
    }
  return(data)
  }

# Define UI for application that draws a time series plot and implements basic transformations
ui <- fluidPage(
   
   # Application title
   titlePanel("Time Series Playground"),
   
   # Sidebar with a select input for dataset and transformation 
   sidebarLayout(
      sidebarPanel(
         selectInput("ts",
                     "Time Series Data:",list("Dow Jones" = "dowjones", "Brick Production" = "bricksq", "US Deaths" = "usdeaths")),
         selectInput("transform","Transformation", list("BoxCox" = 1, "Natural Log" = 2, "Log10" = 3, "Difference" = 4)
      )),
        
      
      # Show a plot of the time series
      mainPanel(
         dygraphOutput("timePlot")
      )
   ))


# Define server logic required to draw a dygraph plot
server <- function(input, output) {
  
   #Build a reactive function calling our transformation utility so we can easily switch datasets and transformations
   data <- reactive({transformation(input$transform, input$ts)})
     
   output$timePlot <- renderDygraph({
      
      #Build a dygraph plot with a range selector so that the time series can be adjusted
      #Note calling data as a function data()
      dygraph(data()) %>% dyRangeSelector()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

