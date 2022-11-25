library(shiny)
library(ggplot2)
library(tidyverse)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  img(src = "bc-liquor-stores-logo-vector.png"), # Feature 1: Add an image to the UI
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100,
                  value = c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(textOutput("ResultsNumber"),
              br(),
              plotOutput("coolplot"),
              br(),br(),
              DT::dataTableOutput("results")
    )
  ),
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", 
    "Link to the original data set")
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    } 
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput)
  })
  
  output$ResultsNumber <- renderText({
    paste("We found ", nrow(filtered()), "options for you:")
    }) # Feature 2: Add a change to the output: show the number of results found whenever the filters change
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- DT::renderDataTable({
    filtered() 
  }) # Feature 3: use the DT package to turn a static table into an interactive table
}

shinyApp(ui = ui, server = server)                                       