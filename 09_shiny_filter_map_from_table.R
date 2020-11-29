
# Copied from https://github.com/tknoch8/travistats_blog/blob/master/table_map_app/table_map_app.R

# repo for run app function is at 
# https://github.com/tknoch8/linked_datatable_leaflet_app.git

devtools::load_all('C:/Users/matt.landis/OneDrive - Resource Systems Group, Inc/Git/tmrtools')

require(tidyverse)
require(shiny)
require(shinydashboard)
require(datasets)
require(DT)
require(leaflet)

# Define scales for map
pal = colorNumeric(get_rsg_palette('hot'),
  domain=c(min(quakes$mag), max(quakes$mag)))

rad_scale = function(mag){
  sizemin = 5
  sizemax = 15
  sizerange = sizemax - sizemin
  datarange = max(quakes$mag) - min(quakes$mag)
  scale = sizerange / datarange
  radius = (mag - min(quakes$mag)) * scale
  return(radius)
}

# ui
my_sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = "menu_1",
    br(),
    actionButton(
      "select_all_rows_button",
      "Select All Table Rows"
    ),
    br(),
    actionButton(
      "clear_rows_button",
      "Clear Table Selections"
    ),
    br(),
    sliderInput(
      inputId="mag",
      label = "Magnitude",
      min=min(quakes$mag),
      max=max(quakes$mag), 
      value = c(min(quakes$mag), max(quakes$mag)),
      step = 0.1)
  )
)

my_header <- dashboardHeader(title = "Fiji Earthquakes")

my_body <- dashboardBody(
  
  fluidRow(
    box(
      width = 6,
      height=750,
      solidHeader = TRUE,
      leafletOutput(
        "my_leaflet",
        height=700
      )
    ),
    
    box(
      width = 6,
      height=750,
      solidHeader = TRUE,
      DTOutput(
        "my_datatable",
        height=700
      )
    )
  ) # fluidRow
  
) # dashboardBody


my_ui <- dashboardPage(
  header = my_header,
  sidebar = my_sidebar,
  body = my_body
)


my_server <- function(session, input, output) {
  
  quakes_r <- reactive({
    quakes %>%
      as_tibble() %>%
      filter(
        mag > input$mag[1],
        mag < input$mag[2]
      )
  })
  
  output$my_datatable <- renderDT({
    
    quakes_r() %>% 
      datatable(
        rownames=FALSE,
        extensions="Scroller",
        style="bootstrap",
        class=c("compact", "display"),
        width="100%",
        fillContainer=TRUE,
        options=list(
          deferRender=TRUE,
          scrollY=300,
          scroller=TRUE,
          searching=FALSE))
    
  })
  
  
  # base map that we will add points to with leafletProxy()
  output$my_leaflet <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(
        provider = providers$CartoDB.Positron,
        options = providerTileOptions(
          noWrap = FALSE
        )
      ) %>% 
      setView(
        lat = -25.5,
        lng = 178.58,
        zoom = 4
      )
    
  })
  
  
  # Set map to show selected rows
  
  observeEvent(input$my_datatable_rows_selected, {
    
    selected_lats <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes_r()$lat[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_longs <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes_r()$long[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_depths <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes_r()$depth[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_mags <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes_r()$mag[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_stations <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes_r()$stations[c(unique(input$my_datatable_rows_selected))])
    })
    
    # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
    # as well as the popups when the points are hovered over
    map_df <- reactive({
      tibble(lat = unlist(selected_lats()),
        lng = unlist(selected_longs()),
        depth = unlist(selected_depths()),
        mag = unlist(selected_mags()),
        stations = unlist(selected_stations()))
    })
    
    leafletProxy("my_leaflet", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(
        data = map_df(),
        lng = ~lng,
        lat = ~lat,
        stroke = FALSE,
        color = ~pal(mag),
        radius = ~rad_scale(mag),
        fillOpacity = 0.8,
        popup = paste0(
          "depth: ", map_df()$depth, "<br>",
          "mag: ", map_df()$mag, "<br>",
          "stations: ", map_df()$stations))# %>%
      #addLegend(pal=pal, values=~mag, opacity=0.8)
    
  })
  
  # create a proxy to modify datatable without recreating it completely
  DT_proxy <- dataTableProxy("my_datatable")
  
  # clear row selections when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    selectRows(DT_proxy, NULL)
  })
  
  # clear markers from leaflet when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    clearMarkers(leafletProxy("my_leaflet", session))
  })
  
  # select all rows when select_all_rows_button is clicked
  observeEvent(input$select_all_rows_button, {
    selectRows(DT_proxy, input$my_datatable_rows_all)
  })
  
}



shinyApp(
  ui = my_ui,
  server = my_server
)