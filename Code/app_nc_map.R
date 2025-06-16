# app.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)
library(readxl)
library(htmltools)
library(rmapshaper)
library(janitor)
library(scales)

setwd("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/Maps/interactive_map/shiny_app/")

# Load data files
# Use ms_simplify to reduce complexity of spatial geometries for faster rendering
df_isolates <- readRDS("amr_zips_shp.rds") %>% ms_simplify(keep = 0.05, keep_shapes = TRUE)
df_livestock <- readRDS("livestock.rds")
df_hosp <- readRDS("nc_hospitals.rds")
adi_nc_zip <- read_excel("adi_nc_zip.xlsx", col_types = c("text", "numeric", "numeric"))

# Merge datasets with ADI data
df_isolates <- left_join(df_isolates, adi_nc_zip, by = c("zip_code" = "zip_code"))
df_livestock <- left_join(df_livestock, adi_nc_zip, by = c("zip_code" = "zip_code"))
df_hosp <- left_join(df_hosp, adi_nc_zip, by = c("zip_code" = "zip_code"))

# PNG icon for hospital marker
hosp_icon = makeIcon("C:/Users/henderh/OneDrive - University of North Carolina at Chapel Hill/Projects/AMR/AMR_mapping_project/Maps/interactive_map/shiny_app/hosp_icon.png", 
                     iconWidth = 20, iconHeight = 20)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Map of AMR isolates and risk factors in North Carolina", titleWidth = 700),
  
  # Set up the sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Map", tabName = "map"),
      menuItem("Data", tabName = "data")
    ),
    
    # Data layer user selection
    checkboxGroupInput(
      "layers", "Choose data layers:",
      choices = c("AMR isolates" = "isolates", 
                  "Livestock" = "livestock", 
                  "Hospitals" = "hospitals")
    ),
    
    # Conditional panel for organism type
    conditionalPanel(
      condition = "input.layers.includes('isolates')",
      selectInput(
        inputId = "organism", 
        label = "Select resistant species", 
        choices = c("None selected" = "", unique(df_isolates$organism)),
        selected = ""
      )
    ),
    
    # Conditional panel for livestock operation type
    conditionalPanel(
      condition = "input.layers.includes('livestock')",
      checkboxGroupInput(
        inputId = "regulated_operation", 
        label = "Choose livestock operation type:", 
        choiceNames = c("Cattle", "Poultry", "Swine"),
        choiceValues = c("Cattle", "Poultry", "Swine"),
        selected = ""
      )
    ),
    
    # Conditional panel for hospital type
    conditionalPanel(
      condition = "input.layers.includes('hospitals')",
      checkboxGroupInput(
        inputId = "hltype", 
        label = "Choose hospital type:", 
        choiceNames = c("Freestanding ED", "Acute care hospital", "Long-term acute care",
                        "Psychiatric", "Rehab", "Specialty", "VA hospital"),
        choiceValues = c("ED", "Hospital", "LTAC", "Psych", "Rehab", "S", "VA"),
        selected = ""
      )
    ),
    
    # ADI decile slider
    sliderInput(
      inputId = "decile_adi_range", 
      label = tags$span(style = "font-weight: normal;", "Select ADI decile range to display"),
      min = min(df_isolates$median_adi_decile, na.rm = TRUE), 
      max = max(df_isolates$median_adi_decile, na.rm = TRUE),
      value = c(min(df_isolates$median_adi_decile, na.rm = TRUE),
                max(df_isolates$median_adi_decile, na.rm = TRUE))
    ),
    
    # Action button for data description
    actionButton("data_source", "About the data")
  ),
  
  # Set up the dashboard body
  dashboardBody(
    tabItems(
      
      # Map view
      tabItem(
        tabName = "map", 
        fluidRow(
          title = "NC Map", status = "primary", solidHeader = TRUE, width = 12,
          collapsible = FALSE, leafletOutput("nc_map", height = "calc(100vh - 150px)")
          )
        ),
      
      # Data view
      tabItem(
        tabName = "data",
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
              collapsible = TRUE, div(class = "resizable-table", dataTableOutput("hospital_table"))
            )
          )
        )
      )
    )
  )
)

# Define the server
server <- function(input, output, session) {
  # Bins for AMR percentages to display
  bins_amr <- c(0, 10, 20, 30, 40, 50, 100)
  # Palette for AMR percentage bins
  pal_amr <- colorBin("OrRd", bins = bins_amr, domain = df_isolates$pct)
  # Palette for livestock operation type
  pal_livestock <- colorFactor(c("green", "yellow", "lightblue"), domain = df_livestock$regulated_operation)
  # Filter AMR data by user-selected organism
  filtered_amr <- reactive({
    req(input$organism)
    df_isolates %>% filter(organism == input$organism,
                           between(median_adi_decile, input$decile_adi_range[1], input$decile_adi_range[2]))
  })
  
  # Filter livestock data by user-selected operation type
  filtered_livestock <- reactive({
    req(input$regulated_operation)
    df_livestock %>% filter(regulated_operation %in% input$regulated_operation,
                            between(median_adi_decile, input$decile_adi_range[1], input$decile_adi_range[2])) %>%
      mutate(
        count_bin = cut(allowable_count, 
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
                                      to = c(4, 10))
      )
  })
  
  # Filter hospital data by user-selected facility type
  filtered_hosp <- reactive({
    req(input$hltype)
    df_hosp %>% filter(hltype %in% input$hltype,
                       between(median_adi_decile, input$decile_adi_range[1], input$decile_adi_range[2]))
  })
  
  # Set up map
  output$nc_map <- renderLeaflet({
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% setView(-79.7, 35.3, 7.5)
  })
  
  observe({
    leafletProxy("nc_map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    # Add AMR layer if selected and organism chosen
    if ("isolates" %in% input$layers && input$organism != "") {
      leafletProxy("nc_map") %>%
        addPolygons(data = filtered_amr(),
                    fillColor = ~pal_amr(pct),
                    fillOpacity = 0.7, 
                    weight = 0, 
                    smoothFactor = 0.5,
                    label = sprintf("<strong> ZIP code %s</strong><br/>
                      <strong>%s County</strong><br/>
                      %g percent resistant<br/>
                      %g total isolates",
                                    filtered_amr()$zip_code, filtered_amr()$county_prop, filtered_amr()$pct, filtered_amr()$total) %>%
                      lapply(htmltools::HTML)) %>%
        # Legend for AMR percentages
        addLegend("bottomright", 
                  pal = pal_amr, 
                  values = df_isolates$pct,
                  title = "% Resistant", 
                  opacity = 0.7)
    }
    
    # Add livestock layer if selected and operation type chosen
    if ("livestock" %in% input$layers && length(input$regulated_operation) > 0) {
      livestock_data <- filtered_livestock()
      
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
    
    # Add hospital layer if selected and type chosen
    if ("hospitals" %in% input$layers && length(input$hltype) > 0) {
      leafletProxy("nc_map") %>%
        addMarkers(data = filtered_hosp(), 
                   lng = ~longitude, 
                   lat = ~latitude, 
                   icon = hosp_icon,
                   popup = ~paste(facility, "<br>Type:", 
                                  hltype, "<br>Beds:",
                                  hgenlic))
    }
  })
  
  # Custom legend for size of livestock operations
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
  output$amr_table <- renderDT({
    filtered_amr() %>%
      st_drop_geometry() %>%
      select(zip_code, county_prop, total, pct) %>%
      datatable(colnames = c("ZIP code", "Primary county", "Total number of isolates for ZIP code", "Percent resistant"))
  })
  
  # Render AMR histogram
  output$amr_histogram <- renderPlotly({
    req(filtered_amr())
    ggplotly(
      ggplot(filtered_amr(), aes(pct)) +
        geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
        labs(title = "Percent Resistant by ZIP code", x = "Percent resistant", y = "Count") +
        theme_minimal()
    )
  })
  
  # Render livestock data table
  output$livestock_table <- renderDT({
    filtered_livestock()  %>%
      select(regulated_operation, allowable_count, zip_code) %>%
      datatable(colnames = c("Operation type", "Allowed number of animals", "ZIP code"))
  })
  
  # Render animal density table
  output$livestock_density <- renderDT({
    filtered_livestock() %>%
      tabyl(allowable_count_binned) %>% 
      adorn_pct_formatting() %>%
      datatable(colnames = c("Allowed number of animals", "Number of operations", "Percent of total"))
  })
  
  # Render hospital data table
  output$hospital_table <- renderDT({
    filtered_hosp() %>% st_drop_geometry() %>% select(facility, hgenlic, hltype, fcounty, fcity)
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
}

shinyApp(ui, server)














