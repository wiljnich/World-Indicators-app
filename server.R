library(dplyr)
library(ggplot2)
library(scales)
wind <- as.data.frame(read.csv("wind.csv", encoding = 'UTF-8', check.names = FALSE))
wind$Year <- as.Date(wind$Year, "%m/%d/%Y")
cbPalette <- c("#0072B2", "#009E73", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#D55E00", "#CC79A7")

server <- function(input, output, session) {
  updateSelectizeInput(session, 'cty_drop', choices = wind$Country, server = TRUE,selected=c("United States", 'Norway'))
  clab <- function(x) { div <- findInterval(as.numeric(gsub("\\,", "", x)), 
                            c(0, 1e3, 1e6, 1e9, 1e12) )
                            paste(round( as.numeric(gsub("\\,","",x))/10^(3*(div-1)), 2), 
                            c("","K","M","B","T")[div] )}
  
  output$lineChart <- renderPlot({
    data1 <- select(filter(wind, Country %in% input$cty_drop), c(Country,Year, input$ind_drop))
    data1 <- select(filter(data1, Year >= min(input$dates)), c(Country,Year, input$ind_drop))
    data1 <- select(filter(data1, Year <= max(input$dates)), c(Country,Year, input$ind_drop))
    
    x_string <- data1$Year
    y_string <- data1[[3]]
    
    ggplot(data1, aes_string(x=x_string, y=y_string)) +
      scale_color_manual(values=cbPalette) +
      geom_line(aes(color=Country), size=2) +
      labs(x='Year', y=input$ind_drop) +
      scale_y_continuous(labels=clab) +
      labs(title='Indicators, 2000-2012', size=12) +
      labs(caption = '\nData courtesy of The World Bank and Tableau \n Not all data available for all countries') + 
      theme_bw() +
      theme(legend.title = element_text(color="#2D3E4E", face='bold', size=18)) +
      theme(legend.text = element_text(color="#2D3E4E", size=15)) +
      theme(legend.key.size = unit(1, "line")) +
      theme(plot.title = element_text(color="#2D3E4E", size=28, hjust=0)) +
      theme(axis.title = element_text(color="#2D3E4E", size=18)) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.text.x = element_text(color="#2D3E4E", size = 14), 
            axis.text.y = element_text(color="#2D3E4E", size = 14))
  },height = 500, width = 600)
}
