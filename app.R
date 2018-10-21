#Author: Andrew Olson
#R Shiny - Project 2

require(shiny)
require(reshape2)
require(dplyr)
require(plotly)
require(htmltools)
require(httr)
require(rgdal)
require(leaflet)
require(leaflet.extras)
require(readxl)
require(stringr)

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

#Function to load GeoJson
ckanGeoSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  readOGR(json)
}

# Unique values for fields
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

Neighborhoods <- sort(ckanUniques("e03a89dd-134a-4ee8-a2bd-62c40aeebc6f", "INCIDENTNEIGHBORHOOD")$INCIDENTNEIGHBORHOOD)

pdf(NULL)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Pittsburgh Police Arrests"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      
      #Date Selecter
      dateRangeInput("DateSelect",
                     "Date range:",
                     start = Sys.Date()-150,
                     end = Sys.Date(),
                     format = "mm/dd/yyyy"),
      
      #Neighborhood select
      selectInput("HoodSelect",
                  "Neighborhood:",
                  choices = Neighborhoods,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Brookline", "Carrick", "Elliot", "Fineview", "Friendship")),
      
      #Age selet
      sliderInput("AgeSelect",
                  "Age:",
                  min = 14,
                  max = 90,
                  value = c(18, 40),
                  step = 1),
      
      #Race Select
      selectInput("RaceSelect",
                  "Race:",
                  choices = c("Black", "White", "Hispanic", "Asian", "American Indian", "Unknown"),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = c("Black", "White")),
      
      #Gender Select
      checkboxGroupInput("GenderSelect",
                         "Gender:",
                         choices = c("Male", "Female"),
                         selected = c("Male", "Female")),
      
      #Reset Filters button
      actionButton("reset", "Reset Filters", icon = icon("refresh"))
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map of Incidents", 
                 leafletOutput("map")
        ),
        tabPanel("Incidents by Race", 
                 plotlyOutput("raceplot")
        ),
        tabPanel("Incidents by Gender", 
                 plotlyOutput("genderplot")
        ),
        tabPanel("Data Table",
                 DT::dataTableOutput("table"),
                 
                 #Download data button
                 downloadButton("downloadData","Download Datatable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Police data
  datInput <- reactive({
    #building an IN selector
    hood_filter <- ifelse(length(input$HoodSelect) > 0, 
                           paste0("%20WHERE%20%22INCIDENTNEIGHBORHOOD%22%20IN%20(%27", paste(input$HoodSelect, collapse = "%27,%27"),"%27)"),
                                     "")
    #Building API query with neighborhood
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%22e03a89dd-134a-4ee8-a2bd-62c40aeebc6f%22", hood_filter)
    #url <- gsub(pattern = " ", replacement = "%20", x = url) # Removing this broke your neighborhood filter when the neighborhood has a space in it!

    #load & clean data
    dat <- ckanSQL(url) 
    dat <- dat %>% mutate(GENDER = recode(GENDER, "M" = "Male", "F" = "Female"),
              RACE = factor(recode(RACE, "B" = "Black",
                                   "W" = "White",
                                   "H" = "Hispanic",
                                   "A" = "Asian",
                                   "I" = "American Indian",
                                   "U" = "Unknown",
                                   "x" = NULL), levels = c("Black", "White", "Hispanic", "Asian", "American Indian", "Unknown")),
              AGE = as.numeric(AGE),
              ARRESTTIME = as.Date(ARRESTTIME, format = "%Y-%m-%d"))
    # So, you know you can filter these all at once right? no need to do it separately. No worries, it still works, but its a bit redundant
    #Date filter
    dat <- dat %>%
      filter(ARRESTTIME >= input$DateSelect[1] & ARRESTTIME <= input$DateSelect[2])
    # Age filter
    dat <- dat %>% 
      filter(AGE >= input$AgeSelect[1] & AGE <= input$AgeSelect[2])
    # Race Filter
    if (length(input$RaceSelect) > 0 ) {
      dat <- subset(dat, RACE %in% input$RaceSelect)
    }
    #Gender Filter
    if (length(input$GenderSelect) > 0 ) {
      dat <- subset(dat, GENDER %in% input$GenderSelect)
    }

    return(dat)
  })
  
  output$map <- renderLeaflet({
    map_data = datInput() 
    
    leaflet() %>%
      setView(lng = -79.997, lat = 40.432, zoom = 12) %>%
      # Basemaps
      addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(noWrap = TRUE)) %>%
      addHeatmap(data = map_data, lng = ~X, lat = ~Y, radius = 8)
    # You've only got one layer!
  })
    
  output$raceplot <- renderPlotly({
    dat <- datInput()
    ggplotly(
      ggplot(data = dat, aes(x = ARRESTTIME, fill = RACE)) +
        geom_bar(position = "dodge")+
        scale_fill_manual(values = c("darkslategray", "dodgerblue3", "gold3", "red", "chartreuse", "blueviolet", "gray")) +
        labs(x = "Date", title = "Police Arrests by Day") +
        guides(color = FALSE))
  })
  output$genderplot <- renderPlotly({
    dat <- datInput()
    ggplotly(
      ggplot(data = dat, aes(x = ARRESTTIME, fill = GENDER)) + 
        geom_bar(position = position_stack(reverse = T))+
        labs(x = "Date", title = "Police Arrests by Day") +
        guides(color = FALSE))
  })
  output$table <- DT::renderDataTable({
    subset(datInput(), select = c(AGE, RACE, GENDER, ARRESTTIME, ARRESTLOCATION, OFFENSES, INCIDENTNEIGHBORHOOD))
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PitArrests-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(datInput(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "RaceSelect", selected = c("Black", "White"))
    updateSliderInput(session, "AgeSelect", value = c(18, 40))
    updateDateRangeInput(session, "DateSelect", start = Sys.Date()-30, end = Sys.Date())
    updateCheckboxGroupInput(session, "GenderSelect", choices = c("Male", "Female"), selected = c("Male", "Female"))
    updateSelectInput(session, "HoodSelect", selected = c("Brookline", "Carrick", "Elliot", "Fineview", "Friendship"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)