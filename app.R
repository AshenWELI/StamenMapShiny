devtools::install_github("Elaineflying/Assignment5")
library(stamenmap)
library(shiny)
library(IRdisplay)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Stamen Map"),
  sidebarLayout(
    sidebarPanel(
      helpText("This is the app that can be visualize the map data what you searched location in the world."),
      selectInput("maptype",
                  label = "Choose a map type",
                  choices = list("terrain","terrain-background","terrain-labels",
                                 "terrain-lines", "toner", "toner-background",
                                 "toner-labels", "toner-lines", "toner-lite", "watercolor"),
                  selected = "watercolor"),
      textInput("address",
                label = "Type your city and country"
      ),
      sliderInput(inputId = "zoom",
                  label = "Zooming range:",
                  min = 1,
                  max = 18,
                  value = 10),
      actionButton("generateButton", "Get Stamen Image")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("mapImage"),
      plotOutput("scatter_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Create a reactive for generating maps
  generateMaps <- eventReactive(input$generateButton, {
    maptype <- input$maptype
    address <- input$address
    zoom <- input$zoom
    
    # Generate the Stamen map image info
    stamen_map_image <- generateStamenMap(address, maptype,zoom)
    
    # Load earthquake data (public data from kaggle)
    data("earthquake_data")
    
    return(list(map_image = stamen_map_image$map_image, address_lat_longs = stamen_map_image$lat_longs, world_earthquake_data = as.data.frame(earthquake_data)))
  })
  
  observeEvent(input$generateButton, {
    maps <- generateMaps()
    
    # Display the Stamen map
    output$mapImage <- renderUI({
      HTML(paste('<img src="data:image/png;base64,', base64enc::base64encode(maps$map_image), '">'))
    })
    
    # Display the scatter plot
    output$scatter_plot <- renderPlot({
      world_earthquake_data <- as.data.frame(maps$world_earthquake_data)
      address_lat_longs <- as.data.frame(maps$addess_lat_longs)
      scatter_plot <- ggplot(world_earthquake_data, aes(longitude, latitude), color = "grey99") +
        borders("world") + geom_point(data=world_earthquake_data,aes(x=longitude,y=latitude),colour = 'red',alpha=0.7) +
        geom_label(data = maps$address_lat_longs, aes(x = longitude, y = latitude, label = name),
                   size = 6, color = "black", fill = "white",
                   label.padding = unit(0.2, "lines"), label.r = unit(0.15, "lines")) +
        labs(title = "Earthquake Data on Map") + theme(plot.title = element_text(size = 20, hjust = 0.5))
      
      print(scatter_plot)
    })
  })
}

shinyApp(ui, server)
