#library(remotes)
#install_github(repo = "filippoweb3/polkanalyzer")
#library(shiny)
library(Polkanalyzer)
library(shinythemes)
library(shinycssloaders)
library(plotly)
library(DT)
library(dplyr)

ui <- fluidPage(

  titlePanel(

    windowTitle = "Polkanalyzer",
    title = tags$head(tags$link(rel = "icon",
                                href = "logo.png", type = "image/png"))

  ),

  headerPanel(

    tags$h3("Polkanalyzer v1.0.0", style = "text-align:right; font-size:10px;")

  ),

  tags$head(
    includeCSS("www/font.css"),
    tags$script(src = "message-handler.js")
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

               column(12, align = "center", actionButton("do", "Update"), style='padding:20px;'),

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
                           value = 7.5, post = "K", step = 0.1, ticks = FALSE),

               sliderInput(inputId = "total_stake",
                           label = "Total Stake (DOT)",
                           min = 1.7,
                           max = 2.5,
                           value = 2.1, post = "M", step = 0.01, ticks = FALSE),

               sliderInput(inputId = "comm",
                           label = "Commission",
                           min = 0,
                           max = 20,
                           value = 5, post = "%", step = 0.1, ticks = FALSE),

               sliderInput(inputId = "m_points",
                           label = "Avg. Points",
                           min = 0,
                           max = 100,
                           value = 50, post = "K", step = 1, ticks = FALSE),

               sliderInput(inputId = "max_points",
                           label = "Max. Points",
                           min = 0,
                           max = round(max(eras_data$eras$era_points, na.rm = T)/10^3),
                           value = 80, post = "K", step = 1, ticks = FALSE),

               sliderInput(inputId = "n.fault",
                           label = "Faulty Events",
                           min = 0,
                           max = max(candidates$faluts, na.rm = T),
                           value = 0, step = 1, ticks = FALSE),

               sliderInput(inputId = "n.offline",
                           label = "Offline Events",
                           min = 0,
                           max = max(candidates$offline, na.rm = T),
                           value = 0, step = 1, ticks = FALSE),

               sliderInput(inputId = "n.subid",
                           label = "Sub-identities",
                           min = 0,
                           max = max(candidates$n_subid, na.rm = T),
                           value = 1, step = 1, ticks = FALSE),

               sliderInput(inputId = "n.runs",
                           label = "Sync Runs",
                           min = 1,
                           max = 20,
                           value = 3, step = 1, ticks = FALSE),

               checkboxInput("id", "Verified Identity", value = TRUE)

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

    column(width = 5,

           shinycssloaders::withSpinner(

              plotlyOutput(outputId = "plotA", width = "100%", height = "200px"),

              type = 1, color = "orange", size = 1

           )
    ),

    column(width = 5,

           shinycssloaders::withSpinner(

           plotlyOutput(outputId = "plotB", width = "100%", height = "200px"),

           type = 1, color = "orange", size = 1

           )
    ),

    column(width = 1)

  ),

  fluidRow(

    column(width = 1),

    column(width = 5,

           shinycssloaders::withSpinner(

           plotlyOutput(outputId = "plotC", width = "100%", height = "200px"),

           type = 1, color = "orange", size = 1

           ), style='padding:20px;'
    ),

    column(width = 5,

           shinycssloaders::withSpinner(

           plotlyOutput(outputId = "plotD", width = "100%", height = "200px"),

           type = 1, color = "orange", size = 1

           ), style='padding:20px;'
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

  load("eras_data.rda")
  load("candidates.rda")

  diff <- reactive({

    (Sys.Date() - 1) - as.Date("2020-06-02",format="%Y-%m-%d")

  })

  observeEvent(input$do, {

    if(eras_data$interval[2] < diff()){

      download.file("https://github.com/filippoweb3/polkanalyzer/blob/main/data/eras_data.rda?raw=true", destfile = "eras_data.rda", method = "libcurl")
      download.file("https://github.com/filippoweb3/polkanalyzer/blob/main/data/candidates.rda?raw=true", destfile = "candidates.rda", method = "libcurl")

      session$reload()

      load("eras_data.rda") #redundant, but makes sure testmessage prints the right era

      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("Updated to era ", eras_data$interval[2])
                                )

    } else {

      session$sendCustomMessage(type = 'testmessage',
                                message = "Data up-to-date"
      )

    }

  })

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
    n_fault <- input$n.fault
    n_offline <- input$n.offline
    n_subid <- input$n.subid
    ver_id <- input$id
    n_runs <- input$n.runs

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
                             selection$id_verified == ver_id &
                             selection$democracyVoteCount >= 1 &
                             selection$councilVoteCount >= 1 &
                             selection$n_subid <= n_subid &
                             selection$faluts <= n_fault &
                             selection$offline <= n_offline,]

    val_names <- as.vector(na.omit(selection$validator_name))

    sync_val <- sync_validators(data = eras_data, names = val_names, look.back = look_back, nruns = n_runs)

    selection.sync <- merge(sync_val, selection,by = "validator_name")

    selection.sync <- selection.sync[order(selection.sync$run, selection.sync$coverage),]

    selection <- list(raw = selection, sync = selection.sync)

  })



  output$map <- renderPlotly({ #renderPlot

    selection <- datasetInput()

    g <- list(

      shadowland = FALSE,
      landcolor = "rgba(48, 48, 48, 1)",
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

    fig <- plot_geo(selection$sync, lat = ~lat, lon = ~lon, marker = list(color = "white")) %>%
      config(displayModeBar = TRUE,
             modeBarButtons = list(c("zoomInGeo", "zoomOutGeo", "resetGeo")),
             displaylogo = FALSE)

    fig <- fig %>% add_markers(

      text = ~paste(validator_name, paste("Self: ",round(m_self/10^3, 1),"kDOT"), paste(location,"|", country), sep = "<br />"),
      symbol = I("circle"), hoverinfo = "text"

    )

    m <- list(l = 10, r = 10, b = 10, t = 10, pad = 10)

    fig <- fig %>% layout(

      geo = g, paper_bgcolor = "rgba(0, 0, 0, 0)", plot_bgcolor = "rgba(0, 0, 0, 0)",
      margin = m,
      modebar = list(bgcolor='transparent', color='white', activecolor='orange')

    )

    if(length(sel())){

      sel_dat <- selection$sync[selection$sync$validator_name == sel(),]

      fig <- fig %>% add_trace(data = sel_dat,
                               type = "scatter",
                               mode = "markers", lat = ~lat, lon = ~lon, marker = list(color = "orange", size = 10),
                               showlegend = FALSE, hoverinfo = "none")

    }

    fig

  })


  output$plotA <- renderPlotly({

    look_back <- input$look.back

    x.max <- max(eras_data$eras$era)
    x.min <- x.max - look_back

    sel_dat <- eras_data$eras[eras_data$eras$name == sel() &
                                eras_data$eras$era >= x.min,]

    m <- list(l = 10, r = 10, b = 10, t = 10, pad = 10)

    plot <- plot_ly(data = sel_dat, x = ~era, y = ~ era_points, color = "orange", colors = c("orange"),
                    type = "scatter", mode = "lines+markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "rgba(0, 0, 0, 0)",
             plot_bgcolor = "rgba(0, 0, 0, 0)",
             xaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Eras"),
                          range = c(x.min - 1, x.max + 1)
                          ),
             yaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Era Points"),
                          range = c(0, 150000)
                          ),
             font = list(color = "white"),
             margin = m,
             modebar = list(bgcolor='transparent', color='white', activecolor='orange')) %>%
      add_markers(

        text = ~paste(name, paste("Era: ", era), paste("Points: ",round(era_points/10^3, 1),"kDOT"), sep = "<br />"),
        hoverinfo = "text", showlegend = FALSE

      )

  })

  output$plotB <- renderPlotly({

    look_back <- input$look.back

    x.max <- max(eras_data$eras$era)
    x.min <- x.max - look_back

    sel_dat <- eras_data$eras[eras_data$eras$name == sel() &
                                eras_data$eras$era >= x.min,]

    m <- list(l = 10, r = 10, b = 10, t = 10, pad = 10)

    plot <- plot_ly(data = sel_dat, x = ~era, y = ~ self_stake/10^10, color = "orange", colors = c("orange"),
                    type = "scatter", mode = "lines+markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "rgba(0, 0, 0, 0)",
             plot_bgcolor = "rgba(0, 0, 0, 0)",
             xaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Eras"),
                          range = c(x.min - 1, x.max + 1)
                          ),
             yaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Self Stake"),
                          range = c(0, 20000)
                          ),
             font = list(color = "white"),
             margin = m,
             modebar = list(bgcolor='transparent', color='white', activecolor='orange')) %>%
      add_markers(

        text = ~paste(name, paste("Era: ", era), paste("Self Stake: ",round(self_stake/10^13, 1),"kDOT"), sep = "<br />"),
        hoverinfo = "text", showlegend = FALSE

      )

  })


  output$plotC <- renderPlotly({

    look_back <- input$look.back

    x.max <- max(eras_data$eras$era)
    x.min <- x.max - look_back

    sel_dat <- eras_data$eras[eras_data$eras$name == sel() &
                                eras_data$eras$era >= x.min,]

    m <- list(l = 10, r = 10, b = 10, t = 10, pad = 10)

    plot <- plot_ly(data = sel_dat, x = ~era, y = ~ commission_percent, color = "orange", colors = c("orange"),
                    type = "scatter", mode = "lines+markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "rgba(0, 0, 0, 0)",
             plot_bgcolor = "rgba(0, 0, 0, 0)",
             xaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Eras"),
                          range = c(x.min - 1, x.max + 1)
             ),
             yaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Commission (%)"),
                          range = c(0, 10)
             ),
             font = list(color = "white"),
             margin = m,
             modebar = list(bgcolor='transparent', color='white', activecolor='orange')) %>%
      add_markers(

        text = ~paste(name, paste("Era: ", era), paste("Self Stake: ",round(commission_percent, 1),"%"), sep = "<br />"),
        hoverinfo = "text", showlegend = FALSE

      )

  })

  output$plotD <- renderPlotly({

    look_back <- input$look.back

    x.max <- max(eras_data$eras$era)
    x.min <- x.max - look_back

    sel_dat <- eras_data$eras[eras_data$eras$name == sel() &
                                eras_data$eras$era >= x.min,]

    m <- list(l = 10, r = 10, b = 10, t = 10, pad = 10)

    plot <- plot_ly(data = sel_dat, x = ~era, y = ~ total_stake/10^10, color = "orange", colors = c("orange"),
                    type = "scatter", mode = "lines+markers") %>%
      config(displayModeBar = FALSE) %>%
      layout(paper_bgcolor = "rgba(0, 0, 0, 0)",
             plot_bgcolor = "rgba(0, 0, 0, 0)",
             xaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Eras"),
                          range = c(x.min - 1, x.max + 1)
             ),
             yaxis = list(zerolinecolor = "white",
                          gridcolor = "white",
                          title = list(text = "Total Stake"),
                          range = c(0, 3000000)
             ),
             font = list(color = "white"),
             margin = m,
             modebar = list(bgcolor='transparent', color='white', activecolor='orange')) %>%
      add_markers(

        text = ~paste(name, paste("Era: ", era), paste("Self Stake: ",round(total_stake/10^16, 3),"MDOT"), sep = "<br />"),
        hoverinfo = "text", showlegend = FALSE

      )

  })


  summaryTable <- reactive({

    selection <- datasetInput()

    data <- selection$sync

    data <- na.omit(data[,colnames(data) %in% c("validator_name",
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


    data[,c(3,7)] <- round(data[,c(3,7)], 1)
    data[,c(4:5,8)] <- round(data[,c(4:5,8)]/10^3, 1)
    data[,9] <- round(data[,9]/10^6, 3)

    colnames(data) <- c("Name",
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

    data

  })


  output$view <- renderDataTable({

    data <- summaryTable()

    datatable(data, rownames= F, extensions = "FixedColumns",
              selection = "single", filter = "none", fillContainer = TRUE,
              options = list(
                scrollX = TRUE,
                scrollY = "450px",
                scrollCollapse = TRUE,
                paging = TRUE,
                lengthMenu = c(16, 32, 48),
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                ordering = TRUE,
                fixedColumns = list(leftColumns = 1)
                )

              ) %>% formatStyle(columns = 1, backgroundColor = "rgba(48, 48, 48, 1)")

  })

  sel <- reactive({

    sel_row <- input$view_rows_selected

    data <- summaryTable()

    data[sel_row,]$Name

  })

}

shinyApp(ui = ui, server = server)
