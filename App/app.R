library(shiny)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(DT)

library(dplyr)
library(rjson)
library(maps)
library(countrycode)
library(Polkanalyzer)


ui <- fluidPage(

  headerPanel(

    tags$h3("Polkanalyzer v1.0.0", style = "text-align:right; font-size:10px;")

  ),

  tags$head(
    includeCSS("www/font.css")
  ),

  tags$style(

    includeCSS("www/font.css")

  ),

  theme = shinytheme("darkly"),

  tags$h1("Polkanalyzer", style = "text-align:center;"),
  tags$h2("A Dashboard for Polkadot's Nominators", style = "text-align:center; font-size:20px;"),

  br(),

  fluidRow(

    column(width = 1),

    column(width = 10,

           sidebarLayout(

             # Sidebar panel for inputs ----
             sidebarPanel(

               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: orange} .js-irs-0 .irs-line {background: transparent}")),
               tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: orange} .js-irs-1 .irs-line {background: transparent}")),
               tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: orange} .js-irs-2 .irs-line {background: transparent}")),
               tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: orange} .js-irs-3 .irs-line {background: transparent}")),
               tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: orange} .js-irs-4 .irs-line {background: transparent}")),
               tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: orange} .js-irs-5 .irs-line {background: transparent}")),
               tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: orange} .js-irs-6 .irs-line {background: transparent}")),



               sliderInput(inputId = "look.back",
                           label = "Past Eras",
                           min = 3,
                           max = 60,
                           value = 30, step = 1, ticks = FALSE),

               sliderInput(inputId = "n.active",
                           label = "Eras Active",
                           min = 3,
                           max = 60,
                           value = 30, step = 1, ticks = FALSE),

               sliderInput(inputId = "self_stake",
                           label = "Self Stake (DOT)",
                           min = 0,
                           max = 20,
                           value = 6, post = "K", step = 0.1, ticks = FALSE),

               sliderInput(inputId = "total_stake",
                           label = "Total Stake (DOT)",
                           min = 1.7,
                           max = 2.5,
                           value = 2.13, post = "M", step = 0.01, ticks = FALSE),

               sliderInput(inputId = "comm",
                           label = "Commission",
                           min = 0,
                           max = 10,
                           value = 5, post = "%", step = 0.1, ticks = FALSE),

               sliderInput(inputId = "m_points",
                           label = "Avg. Points",
                           min = 0,
                           max = 100,
                           value = 50, post = "K", step = 1, ticks = FALSE),

               sliderInput(inputId = "max_points",
                           label = "Max. Points",
                           min = 0,
                           max = 110,
                           value = 100, post = "K", step = 1, ticks = FALSE)

             ),

             # Main panel for displaying outputs ----
             mainPanel(

               shinycssloaders::withSpinner(

                 plotlyOutput(outputId = "map", width = "100%", height = "600px"),

                 type = 5, color = "orange", size = 1

                 )
               )
             )
           ),

    column(width = 1)

  ),

  fluidRow(

    column(width = 1),

    column(width = 10,

           shinycssloaders::withSpinner(

             dataTableOutput("view"),
             type = 1, color = "orange", size = 1

             )

    ),

    column(width = 1)

  ),

  hr(),
  tags$h3("Copyright @filippoweb3", style = "text-align:center; font-size:10px;")

)


server <- function(input, output, session) {




  observeEvent(input$look.back, {

    updateSliderInput(session = session, inputId = "n.active", max = input$look.back)

  })

  datasetInput <- reactive({

    self_stake <- input$self_stake*10^3
    total_stake <- input$total_stake*10^6
    comm <- input$comm
    m_points <- input$m_points*10^3
    max_points <- input$max_points*10^3
    look_back <- input$look.back
    n_active <- input$n.active

    selection <- select_validator(data = eras_data, look.back = look_back,
                                  criteria = list(self_stake = self_stake,
                                                  total_stake = total_stake,
                                                  commission = comm,
                                                  n_active = n_active + 1,
                                                  mean_era_points = m_points,
                                                  max_era_points = max_points,
                                                  last_active = look_back + 1))

    selection <- merge(selection, candidates, by = "stash_address")

    selection <- selection[!selection$provider == "Hetzner Online GmbH" &
                             selection$id_verified == TRUE &
                             selection$democracyVoteCount >= 1 &
                             selection$councilVoteCount >= 1 &
                             selection$n_subid <= 3 &
                             selection$faluts <= 0 &
                             selection$offline <= 0,]

    val_names <- as.vector(na.omit(selection$validator_name))

    sync_val <- sync_validators(data = eras_data, names = val_names, look.back = look_back)

    selection <- merge(sync_val, selection,by = "validator_name")

    selection <- selection[order(selection$run, selection$coverage),]

  })




  output$map <- renderPlotly({ #renderPlot

    selection <- datasetInput()

    g <- list(

      shadowland = FALSE,
      landcolor = "rgba(0, 0, 0, 0)",
      showcountries = TRUE,
      showland = TRUE,
      showocean = TRUE,
      oceancolor = "rgba(0, 0, 0, 0)",
      bgcolor = "rgba(0, 0, 0, 0)",
      projection = list(
        type = 'orthographic',
        rotation = list(
          lon = 0,
          lat = 40,
          roll = 0
        )
      )
    )

    fig <- plot_geo(selection, lat = ~lat, lon = ~lon, marker = list(color = "orange")) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(c("zoomInGeo", "zoomOutGeo", "resetGeo")),
             displaylogo = FALSE)

    fig <- fig %>% add_markers(

      text = ~paste(validator_name, paste("Self: ",round(m_self/10^3, 1),"kDOT"), paste(location,"|", country), sep = "<br />"),
      symbol = I("circle"), hoverinfo = "text"

    )

    m <- list(
      l = 0,
      r = 0,
      b = 0,
      t = 0,
      pad = 0
    )

    fig <- fig %>% layout(

      geo = g, paper_bgcolor = "rgba(0, 0, 0, 0)", plot_bgcolor = "rgba(0, 0, 0, 0)",
      margin = m,
      modebar = list(bgcolor='transparent', color='orange', activecolor='white')

    )

    fig <- fig

    fig


  })





  output$view <- renderDataTable({

    selection <- datasetInput()

    selection <- na.omit(selection[,colnames(selection) %in% c("validator_name",
                                                               "run",
                                                               "coverage",
                                                               "m_era",
                                                               "max_era",
                                                               "n_active",
                                                               "m_comm",
                                                               "m_self",
                                                               "m_total",
                                                               "last_active",
                                                               "continent")])

    selection[,c(3:5,7:8)] <- round(selection[,c(4:5,8)]/10^3, 1)
    selection[,9] <- round(selection[,9]/10^6, 1)

    colnames(selection) <- c("Name",
                             "Run",
                             "Coverage",
                             "Avg. Points (kDOT)",
                             "Max Points (kDOT)",
                             "N Active",
                             "Comm.",
                             "Self (kDOT)",
                             "Total (MDOT)",
                             "Last Active",
                             "Continent")

    datatable(selection, rownames= F, extensions = "FixedColumns",
              selection = "none", filter = "none", fillContainer = TRUE,
              options = list(
                #scrollX = TRUE,
                scrollY = "450px",
                scrollCollapse = TRUE,
                paging = TRUE,
                lengthMenu = c(16, 32, 48),
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color':'rgba(48, 48, 48, 1)'});",
                  "$(this.api().table().container()).css({'font-size': '8pt'});",
                  "$('#DataTables_Table_0_length select').css('background-color', 'orange');",
                  "$('#DataTables_Table_0_filter input').css('background-color', 'rgba(48, 48, 48, 1)');",
                  "$('#DataTables_Table_0_filter input').css('color', 'orange');",
                  "}"),
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                ordering = TRUE
                #fixedColumns = list(leftColumns = 1)
                )

              ) %>% formatStyle(columns = 1, backgroundColor = "rgba(48, 48, 48, 1)")

  })

}

shinyApp(ui = ui, server = server)
