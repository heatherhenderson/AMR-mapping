library(shiny)
library(leaflet)
library(tidyverse)
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
  dashboardHeader(title = "Interactive Map of AMR isolates and risk factors in North Carolina", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map"),
      menuItem("Data", tabName = "data")
    ),
    checkboxGroupInput("layers", "Choose data layers to display:",
                       choices = list("AMR Isolates" = "isolates", "Livestock Operations" = "livestock")),
    selectInput(inputId = "organism", 
                label = "Select resistant species", 
                choices = unique(df_isolates$organism)),
    checkboxGroupInput(inputId = "regulated_operation", 
                       label = "Choose livestock operation type:", 
                       choiceNames = c("Swine", "Cattle", "Poultry"),
                       choiceValues = c("Swine", "Cattle", "Poultry"),
                       selected = c("Swine", "Cattle", "Poultry")),
    actionButton("data_source", "About the data")
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-sidebar { font-size: 18px; }
        .resizable-box {
          position: relative;
          width: 100%;
          height: calc(100vh - 200px);
          margin-top: 20px;
        }
        .resizable-table {
          width: 100%;
          height: calc(100vh - 400px);
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "NC Map", status = "primary", solidHeader = TRUE, width = 10,
                  collapsible = TRUE, div(class = "resizable-box", leafletOutput("nc_map"))
                ),
                box(
                  textOutput("data_source_text")
                )
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "AMR data table", status = "primary", solidHeader = TRUE, width = 6,
                  collapsible = TRUE, div(class = "resizable-table", DT::dataTableOutput("amr_table"))
                ),
                box(
                  title = "AMR histogram", status = "primary", solidHeader = TRUE, width = 6,
                  collapsible = TRUE, plotOutput("amr_histogram", height = "auto")
                )
              ),
              fluidRow(
                box(
                  title = "Livestock data table", status = "primary", solidHeader = TRUE, width = 6,
                  collapsible = TRUE, div(class = "resizable-table", DT::dataTableOutput("livestock_table"))
                ),
                box(
                  title = "Livestock density table", status = "primary", solidHeader = TRUE, width = 6,
                  collapsible = TRUE, div(class = "resizable-table", DT::dataTableOutput("livestock_density"))
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  org_amr <- reactive({
    df_isolates %>% filter(organism == input$organism)
  })
  
  operation_type <- reactive({
    df_livestock %>% filter(regulated_operation %in% input$regulated_operation)
  })
  
  bins_amr <- c(0, 10, 20, 30, 40, 50, 100)
  
  bins_livestock <- reactive({
    operation_type() %>%
      mutate(count_bin = cut(allowable_count, 
                             breaks = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, 5000000), 
                             include.lowest = TRUE, labels = FALSE),
             count_label = cut(allowable_count, 
                               breaks = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, 5000000), 
                               include.lowest = TRUE, 
                               labels = c("<500", "500-1,000", "1,001-5,000", "5,001-10,000", 
                                          "10,001-50,000", "50,001-100,000", "100,001-500,000",
                                          ">500,000")),
             scaled_size = scales::rescale(as.numeric(cut(allowable_count, 
                                                          breaks = c(0, 500, 1000, 5000, 10000, 50000, 100000, 500000, 5000000))), 
                                           to = c(4, 10)))
  })
  
  pal_amr <- colorBin(palette = "OrRd", bins = bins_amr, domain = df_isolates$pct)
  pal_livestock <- colorFactor(palette = c("green", "yellow", "lightblue"), domain = df_livestock$regulated_operation)
  
  output$nc_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
      addTiles() %>%
      setView(lng = -79.7, lat = 35.3, zoom = 7.5)
  })
  
  observe({
    leafletProxy("nc_map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    if ("isolates" %in% input$layers) {
      leafletProxy("nc_map") %>%
        addPolygons(data = org_amr(),
                    label = sprintf("<strong> ZIP code %s</strong><br/>
                      <strong>%s County</strong><br/>
                      %g percent resistant<br/>
                      %g total isolates",
                                    org_amr()$zip_code, org_amr()$county_prop, org_amr()$pct, org_amr()$total) %>%
                      lapply(htmltools::HTML),
                    stroke = FALSE,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.7,
                    fillColor = ~ pal_amr(pct)) %>%
        
        addLegend("bottomright",
                  pal = pal_amr,
                  values = df_isolates$pct,
                  title = "Percent of isolates resistant",
                  opacity = 0.7)
    }
    
    if ("livestock" %in% input$layers) {
      livestock_data <- bins_livestock()
      
      leafletProxy("nc_map") %>%
        addCircleMarkers(data = livestock_data,
                         lat = ~latitude,
                         lng = ~longitude,
                         radius = ~scaled_size,
                         fillColor = ~pal_livestock(regulated_operation),
                         weight = .5,
                         stroke = TRUE,
                         popup = ~paste("Operation type:", regulated_activity, 
                                        "<br/>Allowed number of animals:", allowable_count, 
                                        "<br/>ZIP code:", zip)) %>%
        
        addLegend("bottomright",
                  pal = pal_livestock,
                  values = df_livestock$regulated_operation,
                  title = "Livestock Operation Type",
                  opacity = 0.7) %>%
        
        addCustomLegend(livestock_data)
    }
  })
  
  addCustomLegend <- function(map, livestock_data) {
    binned_data_sorted <- livestock_data %>% 
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
  
  output$amr_table <- DT::renderDataTable({
    org_amr() %>%
      st_drop_geometry() %>%
      select(zip_code, county_prop, total, pct) %>%
      datatable(colnames = c("ZIP code", "Primary county", "Total number of isolates for ZIP code", "Percent resistant"))
  })
  
  output$amr_histogram <- renderPlot({
    org_amr() %>%
      ggplot(aes(x = pct)) +
      geom_histogram(bins = 20) +
      theme_classic() +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14)) +
      xlab("Percent resistant") +
      ylab("Number of ZIP codes")
  }, height = function() { session$clientData$output_amr_histogram_width })
  
  output$livestock_table <- DT::renderDataTable({
    operation_type() %>%
      select(regulated_activity, allowable_count, zip) %>%
      datatable(colnames = c("Operation type", "Allowed number of animals", "ZIP code"))
  })
  
  output$livestock_density <- DT::renderDataTable({
    operation_type() %>%
      janitor::tabyl(allowable_count_binned) %>% 
      janitor::adorn_pct_formatting() %>%
      datatable(colnames = c("Allowed number of animals", "Number of operations", "Percent of total"))
  })
}
shinyApp(ui = ui, server = server)


    