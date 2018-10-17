#Author: Andrew Olson
#R Shiny - Project 2

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(htmltools)
library(httr)

#Building the WPRDC Get request
ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

#Building API query with date filter
url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20%22AGE%22%2C%20%22RACE%22%2C%20%22GENDER%22%2C%20%22ARRESTDATE%22%2C%20%22ARRESTLOCATION%22%2C%20%22OFFENSES%22%2C%20%22INCIDENTLOCATION%22%20FROM%20%22e03a89dd-134a-4ee8-a2bd-62c40aeebc6f%22%20WHERE%20%22Date%22%20%3E=%20%27", input$DateSelect[1], "%27%20AND%20%22Date%22%20%3C=%20%27", input$DateSelect[2], "%27")
# Variables (nottetaking purposes): AGE, RACE, GENDER, ARRESTDATE, ARRESTLOCATION, OFFENSES, INCIDENTLOCATION

#load and clean data
#data.load <- ckanSQL(url) %>%
#  mutate()

pdf(NULL)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      #Date Selecter
      dateRangeInput("DateSelect",
                     "Date range:",
                     start = Sys.Date()-30,
                     end = Sys.Date(),
                     format = "mm/dd/yyyy"),
      
      #Neighborhood select
      selectInput("input1Select",
                  "input1:",
                  choices = ,
                  multiple = ,
                  selectize = TRUE,
                  selected = c()),
      
      #Reset Filters button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 plotlyOutput("map")
        ),
        tabPanel("Plot1", 
                 plotlyOutput("plot1")
        ),
        tabPanel("Plot2", 
                 plotlyOutput("plot2")
        ),
        tabPanel("Table",
                 DT::dataTableOutput("table"),
                 
                 #Download data button
                 downloadButton("downloadData","label")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Employment data
  datInput <- reactive({
    data <- data.load %>%
      # Filter
      filter()
    return(data)
  })
#  output$graph <- 
    
  output$plot1 <- renderPlotly({
    dat <- datInput()
    ggplotly(
      ggplot())
  })
  output$plot2 <- renderPlotly({
    dat <- daInput()
    ggplotly(
      ggplot())
  })
  output$table <- DT::renderDataTable({
    datatable <- datInput()
    subset()
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("file-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(emInput(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "InputSelect", selected = c())
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)