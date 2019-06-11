library(shiny)
library(jsonlite)
library(shinythemes)
wind <- read.csv("wind.csv", encoding = 'UTF-8', check.names = FALSE)
wind$Year <- as.Date(wind$Year, "%m/%d/%Y")

ui <- fluidPage(theme = shinytheme("darkly"),
  img(src='twb.png', align = "right"),
  titlePanel("World Indicators"),
  sidebarLayout(
    sidebarPanel(
      selectInput('cty_drop', label= h3("Select Countries"), choices = wind$Country, selected = 'United States', multiple = TRUE,
        selectize = TRUE
      ),
      
      selectInput("ind_drop", label = h3("Select Indicator"), choices = colnames(wind), selected = 'GDP', multiple = TRUE,
        selectize = TRUE
      ),
      
      dateRangeInput("dates", "Date range",
        start = min(wind$Year),
        end   = max(wind$Year),
        format = 'yyyy',
        startview='decade')
      ),
    mainPanel(
        plotOutput("lineChart")
    )   
  )      
)

server <- function(input, output) {
  
  output$lineChart <- renderPlot({  
    chartData <- switch(input$radio,
                        "Search" = dats,
                        "User" = datu,
                        "Contract" = datc
    )
    
    chartTitle <- switch(input$radio,
                         "Search" = "Search",
                         "User" = "User",
                         "Contract" = "Contract"
    )
    
    xrange <- range(chartData[[1]])
    yrange <- range(chartData[[2]])
    plot(xrange,yrange,type="n",xlab="",ylab="Total Actions",cex.lab=1.5,
         main=paste("User Actions Over Time - ", chartTitle))
    lines(chartData[[1]], chartData[[2]],col="#2862AB",lwd=3)
  },height = 500, width = 600)
  
}

shinyApp(ui = ui, server = server)