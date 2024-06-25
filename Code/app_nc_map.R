library(shiny)
library(leaflet)
library(dplyr)
library(scales)
library(htmltools)
library(sf)

setwd("J:/ID/AMR_map_van_Duin/species maps/interactive_map/")

# Data sets
df_isolates <- readRDS("J:/ID/AMR_map_van_Duin/species maps/interactive_map/shiny_app/amr_zips_shp.rds")
df_livestock <- readRDS("J:/ID/AMR_map_van_Duin/species maps/interactive_map/shiny_app/livestock.rds")


# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Map of AMR in North Carolina"),
  tabsetPanel(
    tabPanel("Antimicrobial Resistance",
             sidebarLayout(
               sidebarPanel(
                 h5("Data are sourced from the UNC Health electronic health record system for years 2014-2023. 
                   Percentages represent the number of isolates that were resistant divided by the total 
                   number of isolates, per ZIP code. County names represent the primary county a ZIP code 
                   is located in. Data are included for ZIP codes with 10 or more isolates."),
                 selectInput("organism",
                             "Select organism",
                             choices = unique(df_isolates$organism))
               ),
               mainPanel(
                 leafletOutput("pct_amr_map", height = 900)
               )
             )),
    
    tabPanel("Livestock Operations",
             sidebarLayout(
               sidebarPanel(
                 h5("Data are sourced from the NC Department of Agriculture and Consumer Services: 
                    https://www.deq.nc.gov/about/divisions/water-resources/permitting/animal-feeding-operations/animal-facility-map."),
                 selectInput("species", "Choose operation type:", 
                             choices = unique(df_livestock$regulated_operation))
               ),
               mainPanel(
                 leafletOutput("livestock_map", height = 900)
               )
             ))
  )
)

# Define the server logic
server <- function(input, output, session) {

  # Reactive expression to filter AMR data based on selected organism
  org_amr <- reactive({
    df_filtered <- df_isolates %>% filter(organism == input$organism)
    return(df_filtered)
  })
  
  # Render AMR map
  output$pct_amr_map <- renderLeaflet({
    bins <- c(0, 10, 20, 30, 40, 50, 100)
    pal <- colorBin(palette = "OrRd", bins = bins, domain = df_isolates$pct)
    labels <- sprintf("<strong>%s</strong><br/<strong>%s</strong><br/>%g percent resistant<br/>%g total isolates",
                      org_amr()$county_prop, org_amr()$zip_code, org_amr()$pct, org_amr()$total) %>%
      lapply(htmltools::HTML)
    
    org_amr() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -79.7, lat = 35.3, zoom = 7.5) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(org_amr()$pct),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ pct,
                title = "Percent of isolates resistant",
                opacity = 0.7)
    
  })
  
  # Reactive expression to filter data based on selected species
  filtered_data <- reactive({
    df_livestock %>% filter(regulated_operation == input$species)
  })
  
  # Bin the counts and create a scaled size
  binned_data <- reactive({
    filtered_data() %>%
      mutate(count_bin = cut(allowable_count, 
                        breaks = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, 5000000), 
                        include.lowest = TRUE, labels = FALSE),
             count_label = cut(allowable_count, 
                          breaks = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, 5000000), 
                          include.lowest = TRUE, 
                          labels = c("<500", "500-1,000", "1,001-5,000", "5,001-10,000", 
                                     "10,001-50,000", "50,001-100,000", "100,001-500,000",
                                     ">500,000")),
             scaled_size = rescale(as.numeric(cut(allowable_count, 
                                             breaks = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, 5000000))), 
                                   to = c(1, 10)))
  })
  
  # Render the animal operations map
  output$livestock_map <- renderLeaflet({
    pal <- reactive({
      colorFactor(palette = "blue", domain = binned_data()$count_bin)
    })
    labels <- sprintf("Allowable count: <strong>%s</strong>", binned_data()$allowable_count) %>%
      lapply(htmltools::HTML)
    
    binned_data() %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -79.7, lat = 35.3, zoom = 7.5) %>%
      addCircleMarkers(label = labels,
                       lat = ~latitude,
                       lng = ~longitude,
                       radius = ~scaled_size,
                       color = ~pal()(count_bin),
                       fillOpacity = 0.3,
                       stroke = FALSE) %>%
      addCustomLegend()
  })

# Custom legend function
addCustomLegend <- function(map) {
  binned_data_sorted <- binned_data() %>% 
    arrange(as.numeric(as.factor(count_label))) %>%
    distinct(count_label, scaled_size)
  
  bins <- binned_data_sorted$count_label
  sizes <- binned_data_sorted$scaled_size
  
  legend_html <- paste0(
    "<div style='background: white; padding: 10px; border-radius: 8px;'>",
    "<strong>Allowable count</strong><br>",
    paste(
      mapply(function(size, bin) {
        paste0(
          "<div style='display: flex; align-items: center; margin-top: 5px;'>",
          "<svg height='", size * 2, "' width='", size * 2, "'>",
          "<circle cx='", size, "' cy='", size, "' r='", size, "' fill='blue' fill-opacity='0.3' /></svg>",
          "<span style='margin-left: 8px;'>", bin, "</span></div>"
        )
      }, sizes, bins),
      collapse = ""
    ),
    "</div>"
  )
  
  map %>%
    addControl(html = legend_html, position = "bottomright")
}
}


# Run the application
shinyApp(ui = ui, server = server)



