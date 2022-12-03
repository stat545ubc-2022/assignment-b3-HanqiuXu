library(shiny)
library(ggplot2)
library(tidyverse)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  img(src = "bc-liquor-stores-logo-vector.png"), 
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100,
                  value = c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),
      textInput("col", "Select colour for the plot", "lightblue"), # Feature 1: Add an input to choose color for the plot 
      downloadButton('download',"Download the filtered data"), # Feature 2: Add a download button to download the filtered data
      br(),br(),
      a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", 
        "Link to the original data set")
    ),
    mainPanel(textOutput("ResultsNumber"),
              br(),
              tabsetPanel(
                tabPanel("Plot", plotOutput("coolplot")),
                tabPanel("Table", DT::dataTableOutput("results")))  # Feature 3: Place plots and table in separate tabs
    )
  )
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
    }) 
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram(fill = input$col)
  })
  
  output$results <- DT::renderDataTable({
    filtered() 
  }) 
  
  output$dto <- renderDataTable({filtered()})
  output$download <- downloadHandler(
    filename = function(){"data.csv"}, 
    content = function(fname){
      write.csv(filtered(), fname)
    }
  )
}

shinyApp(ui = ui, server = server)                                       