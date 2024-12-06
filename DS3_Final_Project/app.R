#```{r, shiny, echo=FALSE, message=FALSE, warning=FALSE, include=TRUE}
#| standalone: true
#| viewerHeight: 800
library(shinylive)
library(shiny)
library(tidycensus)
library(tigris)
library(dplyr)
library(ggplot2)
library(sf)
library(shinythemes)

# Load the data (ensure that 'thd' and 'state_top_words' are preloaded)
geo_counties <- counties(cb = TRUE, year = 2022, progress_bar=FALSE)

# Rename columns to match
colnames(geo_counties)[colnames(geo_counties)=="NAME"] <- "COUNTY"
colnames(geo_counties)[colnames(geo_counties)=="STATE_NAME"] <- "STATE"

# Merge the data (assuming 'thd' is your dataset)
geo_thd <- full_join(thd, geo_counties, by = c("COUNTY", "STATE"))

# Count the number of threats for each county
geo_thd_counts <- geo_thd %>%
  group_by(COUNTY, STATE) %>%
  summarise(
    count = ifelse(sum(!is.na(DATA.ID)) > 0, sum(!is.na(DATA.ID)), 0),
    geometry = first(geometry),
    .groups = 'drop'
  )

# Convert to sf object
geo_thd_counts <- st_as_sf(geo_thd_counts)

# Define the UI for the combined Shiny app
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Cosmo theme
  
  tags$div(
    style = "text-align: center; margin-bottom: 20px;",
    titlePanel("Top Words and Threats Map by State")
  ),
  
  # Center the select input at the top
  div(
    style = "display: flex; justify-content: center; align-items: center; flex-direction: column; margin-bottom: 20px;",
    selectInput("stateInput", 
                label = "Select a State:", 
                choices = state_top_words$STATE,
                selected = state_top_words$STATE[1], 
                width = "50%")  # Adjust width of select box
  ),
  
  # Row to display Top Words and Map side by side
  fluidRow(
    # Column for the Top Words
    column(
      width = 4,  # Adjust the width as needed
      tags$div(
        style = "font-size: 16px; font-weight: normal; margin-top: 20px; text-align: center; color: #333;",
        "Top Words"
      ),
      tags$div(
        style = "background-color: #f0f0f0; padding: 20px; border-radius: 10px; font-size: 14px; font-weight: normal; text-align: center; color: #555; margin-top: 20px;",
        textOutput("topWordsText")
      )
    ),
    
    # Column for the Map
    column(
      width = 8,  # Adjust the width as needed
      plotOutput("threat_map", height = "500px")
    )
  )
)

# Define server logic for the combined app
server <- function(input, output) {
  
  # Render the top words for the selected state
  output$topWordsText <- renderText({
    req(input$stateInput)  # Ensure a state is selected
    
    # Get the Top_Words for the selected state
    state_data <- state_top_words %>% filter(STATE == input$stateInput)
    
    # Return the Top_Words
    state_data$Top_Words
  })
  
  # Reactive expression to filter data based on selected state
  filtered_data <- reactive({
    geo_thd_counts %>%
      filter(STATE == input$stateInput)
  })
  
  # Render the map
  output$threat_map <- renderPlot({
    # Get the bounding box of the selected state
    state_geom <- geo_thd_counts %>%
      filter(STATE == input$stateInput) %>%
      st_union()  # Combine geometries for the selected state
    
    bbox <- st_bbox(state_geom)  # Get the bounding box
    
    ggplot(data = filtered_data()) +
      geom_sf(aes(fill = count), color = "grey") +
      scale_fill_gradientn(colors = c("white", "orange", "orangered", "orangered4"),
                           na.value = "grey",
                           values = scales::rescale(c(0, 10, 25, 50)),
                           name = "Number of Threats") +
      labs(title = paste("Number of Threats in", input$stateInput, "by County")) +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
               ylim = c(bbox["ymin"], bbox["ymax"]),
               expand = FALSE) +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            plot.margin = margin(t = 10, r = 10, b = 10, l = 10))  # Adjusted plot margins
  })
}

# Run the combined app
shinyApp(ui = ui, server = server)
