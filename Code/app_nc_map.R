library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(scales)
library(htmltools)
library(janitor)
library(sf)
library(readxl)
library(plotly)

# Datasets
isolates <- readRDS("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/Maps/interactive_map/shiny_app/amr_zips_shp.rds")
livestock <- readRDS("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/Maps/interactive_map/shiny_app/livestock.rds")
hosp <- readRDS("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/Maps/interactive_map/shiny_app/nc_hospitals.rds")
adi_nc_zip <- read_excel("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/NC data/adi_nc_zip.xlsx", col_types = c("text", "numeric", "numeric"))
hosp_icon = makeIcon("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/Maps/interactive_map/shiny_app/hosp_icon.png", 
                     iconWidth = 20, iconHeight = 20)
df_isolates <- isolates %>% left_join(adi_nc_zip, relationship = "many-to-many")
df_livestock <- livestock %>% left_join(adi_nc_zip, relationship = "many-to-many")
df_hosp <- hosp %>% left_join(adi_nc_zip, relationship = "many-to-many")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Map of AMR isolates and risk factors in North Carolina", titleWidth = 700),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem("Map", tabName = "map"),
                     menuItem("Data", tabName = "data")
                   ),
                   # Data layer selection
                   checkboxGroupInput(
                     "layers", "Choose data layers:",
                     choices = c("AMR isolates" = "isolates", 
                                 "Livestock operations" = "livestock",
                                 "Hospitals" = "hospitals")
                     ),
                   # Conditional UI for organism selection
                   conditionalPanel(
                     condition = "input.layers.includes('isolates')",
                     selectInput(
                       inputId = "organism", 
                       label = "Select resistant species", 
                       choices = c("None selected" = "", 
                                   unique(df_isolates$organism)),
                       selected = "")
                   ),
                   # Conditional UI for livestock operation type selection
                   conditionalPanel(
                     condition = "input.layers.includes('livestock')",
                     checkboxGroupInput(inputId = "regulated_operation", 
                                        label = "Choose livestock operation type:", 
                                        choiceNames = c("Swine", "Cattle", "Poultry"),
                                        choiceValues = c("Swine", "Cattle", "Poultry"),
                                        selected = c("Swine", "Cattle", "Poultry"))
                   ),
                   # Conditional UI for hospital type selection
                   conditionalPanel(
                     condition = "input.layers.includes('hospitals')",
                     checkboxGroupInput(inputId = "hltype", 
                                        label = "Choose hospital type:", 
                                        choiceNames = c("Freestanding ED", "Acute care hospital", "Long-term acute care",
                                                        "Psychiatric", "Rehab", "Specialty", "VA hospital"),
                                        choiceValues = c("ED", "Hospital", "LTAC", "Psych", "Rehab", "S", "VA"),
                                        selected = c("ED", "Hospital", "LTAC", "Psych", "Rehab", "S", "VA"))
                   ),
                   # Slider for ADI decile variable
                   sliderInput(inputId = "decile_adi_range", 
                               label = tags$span(style = "font-weight: normal;", 
                                                 "Select ADI decile range to display"),
                               min = min(df_isolates$median_adi_decile, na.rm = TRUE), 
                               max = max(df_isolates$median_adi_decile, na.rm = TRUE),
                               value = c(min(df_isolates$median_adi_decile, na.rm = TRUE), max(df_isolates$median_adi_decile, na.rm = TRUE))),
                   actionButton("data_source", "About the data")
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "NC Map", status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = FALSE, leafletOutput("nc_map", height = "calc(100vh - 150px)")
                )
              )
      ),
      tabItem(tabName = "data",
              conditionalPanel(
                condition = "input.layers.includes('isolates')",
                fluidRow(
                  box(
                    title = "AMR data table", status = "primary", solidHeader = TRUE, width = 6,
                    collapsible = TRUE, div(class = "resizable-table", DT::dataTableOutput("amr_table"))
                  ),
                  box(
                    title = "AMR histogram", status = "primary", solidHeader = TRUE, width = 6,
                    collapsible = TRUE, plotlyOutput("amr_histogram", height = "auto")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.layers.includes('livestock')",
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
              ),
              conditionalPanel(
                condition = "input.layers.includes('hospitals')",
                fluidRow(
                  box(
                    title = "Hospital data table", status = "primary", solidHeader = TRUE, width = 6,
                    collapsible = TRUE, div(class = "resizable-table", DT::dataTableOutput("hospital_table"))
                  )
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
    req(input$organism)  # Ensure input$organism is available
    df_isolates %>% filter(organism == input$organism,
                           median_adi_decile >= input$decile_adi_range[1] & median_adi_decile <= input$decile_adi_range[2])
  })
  # Filter livestock data by user-selected operation type
  operation_type <- reactive({
    req(input$regulated_operation)  # Ensure input$regulated_operation is available
    df_livestock %>% filter(regulated_operation %in% input$regulated_operation,
                            median_adi_decile >= input$decile_adi_range[1] & median_adi_decile <= input$decile_adi_range[2])
  })
  # Bins for resistance percentages
  bins_amr <- c(0, 10, 20, 30, 40, 50, 100)
  # Bins for operation head counts and scaled size for markers
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
  # Filter hospital data by user-selected facility type
  hosp_type <- reactive({
    req(input$hltype)  # Ensure input$hltype is available
    df_hosp %>% filter(hltype %in% input$hltype,
                       median_adi_decile >= input$decile_adi_range[1] & median_adi_decile <= input$decile_adi_range[2])
  })
  # Color palettes for layers
  pal_amr <- colorBin(palette = "OrRd", bins = bins_amr, domain = df_isolates$pct)
  pal_livestock <- colorFactor(palette = c("green", "yellow", "lightblue"), domain = df_livestock$regulated_operation)
  
  # Render leaflet map
  output$nc_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -79.7, lat = 35.3, zoom = 7.5)
  })
  
  observe({
    leafletProxy("nc_map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    # Add AMR layer if selected and organism is chosen
    if ("isolates" %in% input$layers && !is.null(input$organism)) {
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
        # Legend for AMR percentages
        addLegend("bottomright",
                  pal = pal_amr,
                  values = df_isolates$pct,
                  title = "Percent of isolates resistant",
                  opacity = 0.7)
    }
    # Add livestock layer if selected and operation type is chosen
    if ("livestock" %in% input$layers && !is.null(input$regulated_operation)) {
      livestock_data <- bins_livestock()
      
      leafletProxy("nc_map") %>%
        addCircleMarkers(data = livestock_data,
                         lat = ~latitude,
                         lng = ~longitude,
                         radius = ~scaled_size,
                         fillColor = ~pal_livestock(regulated_operation),
                         weight = .5,
                         stroke = TRUE,
                         popup = ~paste("Operation type:", regulated_operation, 
                                        "<br/>Allowed number of animals:", allowable_count, 
                                        "<br/>ZIP code:", zip_code)) %>%
        # Legend for livestock palette
        addLegend("bottomright",
                  pal = pal_livestock,
                  values = df_livestock$regulated_operation,
                  title = "Livestock Operation Type",
                  opacity = 0.7) %>%
        # Legend for livestock bin sizes
        addCustomLegend(livestock_data)
    }
    # Add hospital layer if selected and type is chosen
    if ("hospitals" %in% input$layers && !is.null(input$hltype)) {
      hospital_data <- hosp_type()
      if (nrow(hospital_data) > 0) {
        leafletProxy("nc_map") %>%
          addMarkers(data = hospital_data,
                     lat = ~latitude,
                     lng = ~longitude,
                     icon = hosp_icon,
                     popup = ~paste("Facility name:", facility, 
                                                    "<br/>Number of beds:", hgenlic, 
                                                    "<br>Facility type:", hltype,
                                                    "<br/>County:", fcounty,
                                                    "<br>City:", fcity))
      } 
    }
  })
  
  observeEvent(input$data_source, {
    showModal(modalDialog(
      title = "About the data",
      HTML("<strong>Clinical bacterial cultures</strong> data sourced from the UNC Health electronic health 
      record system, years 2014-2023. Percentages represent the number of isolates that were resistant divided 
      by the total number of isolates, per ZIP code of patient residence. Data are included for ZIP codes with 
      10 or more isolates. County names represent the primary county a ZIP code is located in.<br>
      <br><strong>Livestock feeding operations</strong> data sourced from NC Department of Agriculture and Consumer Services: 
      https://www.deq.nc.gov/about/divisions/water-resources/permitting/animal-feeding-operations/animal-facility-map.<br>
      <br><strong>Hospital</strong> data sourced from NC OneMap: https://www.nconemap.gov/datasets/0b5a8fe009144b9bbeb7c4cee9ab7fa9/explore<br>
      <br><strong>Area Deprivation Index</strong> data sourced from University of Wisconsin: https://www.neighborhoodatlas.medicine.wisc.edu/.
      ADI decile defined for ZIP codes using the median value of ADI decile for the census block groups primarily 
      located within the ZIP code.")
    ))
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
  
  # Render AMR data table
  output$amr_table <- DT::renderDataTable({
    org_amr() %>%
      sf::st_drop_geometry() %>%
      select(zip_code, county_prop, total, pct) %>%
      datatable(colnames = c("ZIP code", "Primary county", "Total number of isolates for ZIP code", "Percent resistant"))
  })
  # Render AMR histogram
  output$amr_histogram <- renderPlotly({
    amr_data <- org_amr()
    
    p <- ggplot(amr_data, aes(x = pct, text = paste("Count:", ..count.., "<br>Percent:", ..x..))) +
      geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
      labs(title = "Distribution of Resistant Isolates",
           x = "Percent of isolates resistant",
           y = "Number of ZIP codes") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 100, by = 10))
    
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE)
  })
  # Render livestock data table
  output$livestock_table <- DT::renderDataTable({
    operation_type() %>%
      select(regulated_operation, allowable_count, zip_code) %>%
      datatable(colnames = c("Operation type", "Allowed number of animals", "ZIP code"))
  })
  # Render animal density table
  output$livestock_density <- DT::renderDataTable({
    operation_type() %>%
      janitor::tabyl(allowable_count_binned) %>% 
      janitor::adorn_pct_formatting() %>%
      datatable(colnames = c("Allowed number of animals", "Number of operations", "Percent of total"))
  })
  # Render hospital data table
  output$hospital_table <- DT::renderDataTable({
    hosp_type() %>%
      st_drop_geometry() %>%
      select(facility, hgenlic, hltype, fcounty, fcity) %>%
      datatable(colnames = c("Facility name", "Number of beds", "Facility type", "County", "City"))
  })
}

shinyApp(ui = ui, server = server)
