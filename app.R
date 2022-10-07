library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

ui <- fluidPage(

  titlePanel("Geography selector"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("lad"),
      uiOutput("ward"),
      uiOutput("msoa"),
      uiOutput("lsoa"),
      downloadButton("download",
                     label = "Download selection")
    ),

    mainPanel(
      p("This tool allows you to easily select a variety of geographies by clicking on them on the map below."),
      p("Start by choosing the local authorities within which you want to select smaller areas."),
      p("Then, select any or all of the map layers (wards, MSOAs, LSOAs) in turn and click on the areas you want to select or deselect."),
      p("When you've selected everything (you can remove things by deleting them in the side menu or clicking again), you can download a CSV file which will contain your selection."),
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {

  lsoa11 <- readRDS("data/lsoa11.rds")
  msoa11 <- readRDS("data/msoa11.rds")
  wd21 <- readRDS("data/wd21.rds")
  lad21 <- readRDS("data/lad21.rds")

  # Reactives

  lad <- reactive({
    filter(lad21, LAD21NM %in% input$lad)
  })

  ward <- reactive({
    filter(wd21, LAD21NM %in% input$lad) |>
      mutate(clicked_shape = ifelse(WD21NM.x %in% input$ward,
                                    TRUE,
                                    FALSE))
  })

  msoa <- reactive({
    filter(msoa11, LAD21NM %in% input$lad) |>
      mutate(clicked_shape = ifelse(msoa11hclnm %in% input$msoa,
                                    TRUE,
                                    FALSE))
  })

  lsoa <- reactive({
    filter(lsoa11, LAD21NM %in% input$lad) |>
      mutate(clicked_shape = ifelse(LSOA11NM %in% input$lsoa,
                                    TRUE,
                                    FALSE))
  })

  # UI components

  output$lad <- renderUI({
    selectizeInput("lad", "Local authority",
                   choices = lad21$LAD21NM,
                   multiple = TRUE)
  })

  output$ward <- renderUI({
    value <- isolate(input$ward)
    selectizeInput("ward", "Wards",
                   choices = ward()$WD21NM.x,
                   selected = value,
                   multiple = TRUE)
  })

  output$msoa <- renderUI({
    value <- isolate(input$msoa)
    selectizeInput("msoa", "MSOAs",
                   choices = msoa()$msoa11hclnm,
                   selected = value,
                   multiple = TRUE)
  })

  output$lsoa <- renderUI({
    value <- isolate(input$lsoa)
    selectizeInput("lsoa", "LSOAs",
                   choices = lsoa()$LSOA11NM,
                   selected = value,
                   multiple = TRUE)
  })

  output$download <- downloadHandler(
    filename = function() {"data.csv"},
    content = function(file) {

      l <- reactive({
        list(lad = input$lad,
             ward = input$ward,
             msoa = input$msoa,
             lsoa = input$lsoa)
      })

      cfun <- function(L) {
        pad.na <- function(x, len) {
          c(x, rep(NA, len - length(x)))
        }
        maxlen <- max(sapply(L, length))
        do.call(data.frame, lapply(L, pad.na, len = maxlen))
      }

      items <- reactive({
        cfun(l())
      })

      readr::write_csv(items(), file)
    }
  )

  # Observers

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    print(click) # for testing and debugging
    click_group <- click$group

    if (click$id %in% input[[click_group]]) {
      updateSelectInput(session, click$group,
                        selected = input[[click_group]][!input[[click_group]] %in% click])
    } else {
      updateSelectInput(session, click$group,
                        selected = c(input[[click_group]], click))

    }
  })

  observe(leafletProxy("map") |>
            addPolygons(data = lad(),
                        weight = 1,
                        layerId = ~LAD21NM,
                        group = "lad",
                        label = paste("LAD:", lad()$LAD21NM)) |>
            addPolygons(data = ward(),
                        weight = 1,
                        layerId = ~WD21NM.x,
                        fillColor = ~clicked_shape,
                        group = "ward",
                        label = paste("Ward:", ward()$WD21NM.x)) |>
            addPolygons(data = msoa(),
                        weight = 1,
                        layerId = ~msoa11hclnm,
                        fillColor = ~clicked_shape,
                        group = "msoa",
                        label = paste("MSOA:", msoa()$msoa11hclnm)) |>
            addPolygons(data = lsoa(),
                        weight = 1,
                        layerId = ~LSOA11NM,
                        fillColor = ~clicked_shape,
                        group = "lsoa",
                        label = paste("LSOA:", lsoa()$LSOA11NM)))

  # Map

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addPolygons(data = lad(),
                  weight = 1,
                  layerId = ~LAD21NM,
                  group = "lad",
                  label = paste("LAD:", lad()$LAD21NM)) |>
      # addPolygons(data = ward(),
      #             weight = 1,
      #             layerId = ~WD21NM.x,
      #             group = "ward",
      #             label = paste("Ward:", ward()$WD21NM.x)) |>
      # addPolygons(data = msoa11 |> filter(LAD21NM %in% input$lad),
      #             weight = 1,
      #             layerId = ~msoa11hclnm,
      #             group = "msoa",
      #             label = paste("MSOA:", msoa()$msoa11hclnm)) |>
      # addPolygons(data = lsoa(),
      #             weight = 1,
      #             layerId = ~LSOA11NM,
      #             group = "lsoa",
      #             label = paste("LSOA:", lsoa()$LSOA11NM)) |>
      addLayersControl(baseGroups = c("lad", "ward", "msoa", "lsoa"),
                       options = layersControlOptions(collapsed = FALSE))
  })
}

shinyApp(ui = ui, server = server)
