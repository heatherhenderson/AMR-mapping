library(shiny)
library(leaflet)
library(dplyr)
library(scales)
library(shinydashboard)
library(sf)
library(DT)
library(fontawesome)
library(ggplot2)
library(janitor)

# Datasets
df_isolates <- readRDS("J:/ID/AMR_map_van_Duin/species maps/interactive_map/shiny_app/amr_zips_shp.rds")
df_livestock <- readRDS("J:/ID/AMR_map_van_Duin/species maps/interactive_map/shiny_app/livestock.rds")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Map of AMR in North Carolina", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map"),
      menuItem("Data", tabName = "data")
    ),
    selectInput("organism", 
                "Select organism", 
                choices = unique(df_isolates$organism)),
    
    checkboxGroupInput("regulated_operation", 
                       "Choose operation type:", 
                       choiceNames = list(
                         tags$span("Swine", style = "color: lightblue; font-weight: bold; font-size: 18"),
                         tags$span("Cattle", style = "color: green; font-weight: bold; font-size: 18"), 
                         tags$span("Poultry", style = "color: yellow; font-weight: bold; font-size: 18")
                       ),
                       choiceValues = c("Swine", "Cattle", "Poultry"),
                       selected = c("Swine", "Cattle", "Poultry")),
    actionButton("update_map", "Update"),
    actionButton("data_source", "About the data")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(".main-sidebar { font-size: 18px; }"))),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "AMR Map", status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = TRUE, leafletOutput("pct_amr_map", height = 800)
                ),
                box(
                  textOutput("data_source_text")
                )
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "AMR data table", status = "primary", solidHeader = TRUE, width = 5, height = 500,
                  collapsible = TRUE, DT::dataTableOutput("amr_table")
                ),
                box(
                  title = "AMR histogram", status = "primary", solidHeader = TRUE, width = 5, height = 500,
                  collapsible = TRUE, plotOutput("amr_histogram")
                )),
              fluidRow(
                box(
                  title = "Livestock data table", status = "primary", solidHeader = TRUE, width = 5, height = 500,
                  collapsible = TRUE, DT::dataTableOutput("livestock_table")
                ),
                box(
                  title = "Livestock density table", status = "primary", solidHeader = TRUE, width = 5, height = 500,
                  collapsible = TRUE, DT::dataTableOutput("livestock_density")
                )
              )
            )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Filter AMR data by user-selected organism
  org_amr <- reactive({
    df_isolates %>% filter(organism == input$organism)
  })
  
  # Filter livestock data by user-selected operation type
  filtered_data <- reactive({
    df_livestock %>% filter(regulated_operation %in% input$regulated_operation)
  })
  
  # Bin operation head counts and create scaled size for markers
  bins_livestock <- reactive({
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
                                   to = c(4, 10)))
  })
  
  # Trigger map rendering when user clicks update button
  map_data <- eventReactive(input$update_map, {
    list(org_amr = org_amr(), bins_livestock = bins_livestock())
  })
  
  # Render leaflet map
  output$pct_amr_map <- renderLeaflet({
    data <- map_data()
    org_amr_data <- data$org_amr
    bins_livestock_data <- data$bins_livestock
    # Define categories for AMR percentages
    bins_amr <- c(0, 10, 20, 30, 40, 50, 100)
    pal_amr <- colorBin(palette = "OrRd", bins = bins_amr, domain = df_isolates$pct)
    pal_livestock <- colorFactor(palette = c("green", "yellow", "lightblue"), domain = df_livestock$regulated_operation)
    # popup labels for AMR map
    labels_amr <- sprintf("<strong> ZIP code %s</strong><br/>
                      <strong>%s County</strong><br/>
                      %g percent resistant<br/>
                      %g total isolates",
                          org_amr_data$zip_code, org_amr_data$county_prop, org_amr_data$pct, org_amr_data$total) %>%
      lapply(htmltools::HTML)
    
    leaflet(data = org_amr_data) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -79.7, lat = 35.3, zoom = 8) %>%
      addPolygons(data = org_amr_data,
                  label = labels_amr,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal_amr(pct),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = FALSE)) %>%
      addCircleMarkers(data = bins_livestock_data,
                       lat = ~latitude,
                       lng = ~longitude,
                       radius = ~scaled_size,
                       fillColor = ~pal_livestock(regulated_operation),
                       weight = .5,
                       stroke = TRUE,
                       popup = ~paste("Operation type:", regulated_activity, 
                                      "<br/>Allowed number of animals:", allowable_count, 
                                      "<br/>ZIP code:", zip)) %>%
      
      addCustomLegend() %>%
      
      addLegend("bottomright",
                pal = pal_amr,
                values = ~ pct,
                title = "Percent of isolates resistant",
                opacity = 0.7)
  })
  
  # Text for data source
  observeEvent(input$data_source, {
    showModal(modalDialog("Data from clinical bacterial cultures sourced from the UNC Health electronic health 
    record system, years 2014-2023. Percentages represent the number of isolates that were resistant divided 
    by the total number of isolates, per ZIP code of patient residence. County names represent the primary county a ZIP code 
    is located in. Data are included for ZIP codes with 10 or more isolates. Data for livestock feeding
    feeding operations souced from NC Department of Agriculture and Consumer Services 
    https://www.deq.nc.gov/about/divisions/water-resources/permitting/animal-feeding-operations/animal-facility-map."))
  })
  
  # Custom legend function
  addCustomLegend <- function(map) {
    binned_data_sorted <- bins_livestock() %>% 
      arrange(as.numeric(as.factor(count_label))) %>%
      distinct(count_label, scaled_size)
    
    bins <- binned_data_sorted$count_label
    sizes <- binned_data_sorted$scaled_size
    
    legend_html <- paste0(
      "<div style='background: white; padding: 10px; border-radius: 8px;'>",
      "<strong>Number of animals</strong><br>",
      paste(
        mapply(function(size, bin) {
          paste0(
            "<div style='display: flex; align-items: center; margin-top: 5px;'>",
            "<svg height='", size * 2, "' width='", size * 2, "'>",
            "<circle cx='", size, "' cy='", size, "' r='", size, "' fill='grey' fill-opacity='0.3' /></svg>",
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
  
  # Render AMR data table
  output$amr_table <- DT::renderDataTable({
    org_amr() %>%
      select(zip_code, county_prop, total, pct) %>%
      st_set_geometry(NULL) %>%
      datatable(colnames = c("ZIP code", "Primary county", "Total number of isolates for ZIP code", "Percent resistant"))
  })
  
  # Render AMR histogram
  output$amr_histogram <- renderPlot({
    org_amr() %>%
      ggplot2::ggplot(aes(x = pct)) +
      geom_histogram(bins = 20) +
      theme_classic() +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)) +
      xlab("Percent resistant") +
      ylab("Number of ZIP codes")
  })
  
  # Render livestock data table
  output$livestock_table <- DT::renderDataTable({
    filtered_data() %>%
      select(regulated_activity, allowable_count, zip) %>%
      datatable(options = list(pageLength = 5), colnames = c("Operation type", "Allowed number of animals", "ZIP code"))
  })
  
  # Render animal density table
  output$livestock_density <- DT::renderDataTable({
    filtered_data() %>%
      tabyl(allowable_count_binned) %>% 
      adorn_pct_formatting() %>%
      datatable(colnames = c("Allowed number of animals", "Number of operations", "Percent of total"))
  })
}

shinyApp(ui = ui, server = server)


# df_isolates %>%
#   select(zip_code, county_prop, total, pct) %>%
#   datatable(colnames = c("ZIP code", "Primary county", "Total number of isolates for ZIP code", "Percent resistant"))


    