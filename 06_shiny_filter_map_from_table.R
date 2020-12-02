
# Copied from https://github.com/tknoch8/travistats_blog/blob/master/table_map_app/table_map_app.R

# repo for run app function is at 
# https://github.com/tknoch8/linked_datatable_leaflet_app.git

# Global ----------------------------------------------------------------------

require(data.table)
require(tidyverse)
require(shiny)
require(shinydashboard)
require(datasets)
require(DT)
require(leaflet)

source('tmrtools.R')

# Load data

hh_labeled = readRDS('data/tnc_bayarea_hh_labeled.rds')
hh_map = hh_labeled[
  !income_aggregate %in% c('Prefer not to answer', 'Missing: Non-response', 'Missing: Skip logic') & !is.na(income_aggregate)]


# Define map palette

income_levels = levels(factor(hh_map[, income_aggregate]))
pal = colorFactor(
  colorRampPalette(
    get_rsg_palette('hot')
  )(length(income_levels)),
  levels=income_levels)

box_height = 750
map_center = c(lon=-122.44365, lat=37.75841)

# ui --------------------------------------------------------------------------

ui_sidebar <- dashboardSidebar(
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
    checkboxGroupInput(
      inputId="income",
      label = "Income",
      choices = income_levels, 
      selected = income_levels)
  )
)

ui_header <- dashboardHeader(title = "HTS Households with Shiny")

ui_body <- dashboardBody(
  
  fluidRow(
    HTML('<strong>About:</strong> This is a demonstration project to show how to interact with household
      travel survey data.  The project and source code is available on 
      <a href="https://github.com/landisrm/rTED_2020">Github</a>.<br>
      Note: These data have been fully anonymized by randomizing the ids,
      locations, and descriptive variables.  In addition the actual locations 
      have been jittered. '),
    box(
      width = 12 * 0.5,
      height=box_height,
      solidHeader = TRUE,
      leafletOutput("map", height=box_height - 50),
    ),
    box(
      width = 12 * 0.5,
      height=box_height,
      solidHeader = TRUE,
      DTOutput("tbl", height=box_height - 50)
    )
  ) # fluidRow
  
) # dashboardBody


ui <- dashboardPage(
  header = ui_header,
  sidebar = ui_sidebar,
  body = ui_body
)


# server ---------------------------------------------------------------------

server <- function(session, input, output) {
  
  data_r = reactive({
    hh_map[income_aggregate %in% input$income]
  })
  
  output$tbl <- renderDT({
    
    data_r() %>% 
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
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(
        provider = providers$CartoDB.Positron,
        options = providerTileOptions(
          noWrap = FALSE
        )
      ) %>% 
      setView(
        lat = map_center['lat'],
        lng = map_center['lon'],
        zoom = 8
      ) # %>%
    # addLegend(pal=pal, values=~income_aggregate, opacity=0.5)
    
  })
  
  
  # Set map to show selected rows
  
  observeEvent(input$tbl_rows_selected, {
    
    selected_lat <- eventReactive(input$tbl_rows_selected, {
      as.list(data_r()$reported_home_lat[c(unique(input$tbl_rows_selected))])
    })
    
    selected_lng <- eventReactive(input$tbl_rows_selected, {
      as.list(data_r()$reported_home_lon[c(unique(input$tbl_rows_selected))])
    })
    
    selected_income_aggregate <- eventReactive(input$tbl_rows_selected, {
      as.list(data_r()$income_aggregate[c(unique(input$tbl_rows_selected))])
    })
      
    # Data for pop-up
    selected_hh_id <- eventReactive(input$tbl_rows_selected, {
      as.list(data_r()$hh_id[c(unique(input$tbl_rows_selected))])
    })
    
    selected_income <- eventReactive(input$tbl_rows_selected, {
      as.list(data_r()$income_detailed[c(unique(input$tbl_rows_selected))])
    })
    
    # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
    # as well as the popups when the points are hovered over
    map_df <- reactive({
      tibble(
        lat = unlist(selected_lat()),
        lng = unlist(selected_lng()),
        income_aggregate = unlist(selected_income_aggregate()),
        hh_id = unlist(selected_hh_id()),
        income = unlist(selected_income())
        )
    })
    
    leafletProxy("map", session) %>% 
      clearMarkers() %>% 
      addCircleMarkers(
        data=map_df(),
        lng=~lng,
        lat=~lat,
        stroke=FALSE,
        radius=5,
        fillOpacity=0.5,
        color=~pal(income_aggregate),
        popup = ~paste0(
           'hh_id: ', hh_id, '<br>',
           'income: ', income, '<br>')) # %>%
      # # addLegend(pal=pal, values=~income_aggregate, opacity=0.5)

  })
  
  # create a proxy to modify datatable without recreating it completely
  DT_proxy <- dataTableProxy("tbl")
  
  # clear row selections when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    selectRows(DT_proxy, NULL)
  })
  
  # clear markers from leaflet when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    clearMarkers(leafletProxy("map", session))
  })
  
  # select all rows when select_all_rows_button is clicked
  observeEvent(input$select_all_rows_button, {
    selectRows(DT_proxy, input$tbl_rows_all)
  })
  
}


# app -------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)