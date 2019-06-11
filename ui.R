library(shiny)
library(shinythemes)
wind <- read.csv("wind.csv", encoding = 'UTF-8', check.names = FALSE)
wind$Year <- as.Date(wind$Year, "%m/%d/%Y")
drops <- c("Country","Region", "Year")
cols <- wind[ , !(names(wind) %in% drops)]

ui <- fluidPage(theme = shinytheme("flatly"),
  img(src='twb.png', style='width:300px'),
  titlePanel("World Indicators"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput('cty_drop', label= h3("Select Countries"), choices = wind$Country, 
                     options = list(maxItems = 3, 
                                    placeholder = 'Select up to three countries')
      ),
      
      selectInput("ind_drop", label = h3("Select Indicator"), choices = colnames(cols), selected = 'GDP', multiple = FALSE,
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