library(shiny)
library(leaflet)
library(dplyr)
library(scales)
library(shinydashboard)
library(sf)
library(DT)
library(fontawesome)
library(ggplot2)

df_isolates <- readRDS("J:/ID/AMR_map_van_Duin/species maps/interactive_map/shiny_app/amr_zips_shp.rds")
df_livestock <- readRDS("J:/ID/AMR_map_van_Duin/species maps/interactive_map/shiny_app/livestock.rds")

ui <- dashboardPage(
  dashboardHeader(title = "Interactive Map of AMR in North Carolina", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Antimicrobial Resistance", tabName = "amr", icon = icon("bug")),
      menuItem("Livestock Operations", tabName = "livestock", icon = icon("cow"))
    ),
      selectInput("organism", "Select organism", choices = unique(df_isolates$organism)),

      checkboxGroupInput("species", "Choose operation type:", 
                         choices = c("Swine", "Cattle", "Poultry"),
                         selected = c("Swine", "Cattle", "Poultry"))
    ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "amr",
              fluidRow(
                box(
                  title = "Information", status = "primary", solidHeader = TRUE, width = 3,
                  h5("Data source: UNC Health electronic health record system, years 2014-2023. 
                   Percentages represent the number of isolates that were resistant divided by the total 
                   number of isolates, per ZIP code. County names represent the primary county a ZIP code 
                   is located in. Data are included for ZIP codes with 10 or more isolates.")
                ),
                box(
                  title = "AMR Map", status = "primary", solidHeader = TRUE, width = 8,
                  collapsible = TRUE, leafletOutput("pct_amr_map", height = 500)
                ),
                box(
                  title = "AMR data table", status = "primary", solidHeader = TRUE, width = 5,
                  collapsible = TRUE, DT::dataTableOutput("amr_table")
                ),
                box(
                  title = "AMR histogram", status = "primary", solidHeader = TRUE, width = 5, height = 5,
                  collapsible = TRUE, plotOutput("amr_histogram")
                  )
                )
              ),

      tabItem(tabName = "livestock",
              fluidRow(
                box(
                  title = "Information", status = "primary", solidHeader = TRUE, width = 3,
                  h5("Data source: NC Department of Agriculture and Consumer Services 
                    https://www.deq.nc.gov/about/divisions/water-resources/permitting/animal-feeding-operations/animal-facility-map.")
                ),
                box(
                  title = "Livestock Map", status = "primary", solidHeader = TRUE, width = 8,
                  collapsible = TRUE, leafletOutput("livestock_map", height = 500)
                ),
                box(
                  title = "Livestock data table", status = "primary", solidHeader = TRUE, width = 6,
                  collapsible = TRUE, DT::dataTableOutput("livestock_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filter AMR data by selected organism
  org_amr <- reactive({
    df_filtered <- df_isolates %>% filter(organism == input$organism)
    return(df_filtered)
  })
  
  # Render AMR map
  output$pct_amr_map <- renderLeaflet({
    bins <- c(0, 10, 20, 30, 40, 50, 100)
    pal <- colorBin(palette = "OrRd", bins = bins, domain = df_isolates$pct)
    labels <- sprintf("<strong>%s</strong><br/><strong>%s</strong><br/>%g percent resistant<br/>%g total isolates",
                      org_amr()$county_prop, org_amr()$zip_code, org_amr()$pct, org_amr()$total) %>%
      lapply(htmltools::HTML)
    
    leaflet(data = org_amr()) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -79.7, lat = 35.3, zoom = 7) %>%
      addPolygons(data = org_amr(),
                  label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(pct),
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
  
  # Filter livestock data by selected species
  filtered_data <- reactive({
    df_livestock %>% filter(regulated_operation %in% input$species)
  })
  
  # Bin counts and create scaled size
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
  
  # Render animal operations map
  output$livestock_map <- renderLeaflet({
    pal <- colorFactor(palette = c("blue", "green", "red"), domain = df_livestock$regulated_operation)
    labels <- sprintf("Allowable count: <strong>%s</strong>", binned_data()$allowable_count) %>%
      lapply(htmltools::HTML)
    
    leaflet(data = binned_data()) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -79.7, lat = 35.3, zoom = 7) %>%
      addCircleMarkers(lat = ~latitude,
                       lng = ~longitude,
                       radius = ~scaled_size,
                       color = ~pal(regulated_operation),
                       fillOpacity = 0.3,
                       stroke = FALSE,
                       label = labels) %>%
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
      select(zip_code, total, pct) %>%
      st_set_geometry(NULL) %>%
      datatable(options = list(pageLength = 5, autoWidth = TRUE))
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
      select(-c(latitude, longitude, allowable_count_binned)) %>%
      datatable(options = list(pageLength = 5, autoWidth = TRUE))
  })
}

shinyApp(ui = ui, server = server)

