library(shiny)
library(shinythemes)
library(dplyr)
library(maps)
library(countrycode)
library(Polkanalyzer)

ui <- fluidPage(

  theme = shinytheme("darkly"),

  titlePanel("Polkanalyzer"),

  fluidRow(

    column(3,

           sliderInput(inputId = "look.back",
                       label = "Past Eras",
                       min = 3,
                       max = 60,
                       value = 30, step = 1, ticks = FALSE),

    ),

    column(3,

           sliderInput(inputId = "self_stake",
                       label = "Self Stake (DOT)",
                       min = 0,
                       max = 20,
                       value = 5, post = "K", step = 0.1, ticks = FALSE),

           sliderInput(inputId = "total_stake",
                       label = "Total Stake (DOT)",
                       min = 1.7,
                       max = 2.5,
                       value = 2.13, post = "M", step = 0.001, ticks = FALSE),

           sliderInput(inputId = "comm",
                       label = "Commission",
                       min = 0,
                       max = 10,
                       value = 5, post = "%", step = 0.1, ticks = FALSE)

           ),

    column(3,

           sliderInput(inputId = "m_points",
                       label = "Avg. Points",
                       min = 0,
                       max = 100,
                       value = 50, post = "K", step = 1, ticks = FALSE),

           sliderInput(inputId = "max_points",
                       label = "Max. Points",
                       min = 0,
                       max = 110,
                       value = 80, post = "K", step = 1, ticks = FALSE)

    )

  ),

  fluidRow(

    column(width = 12,

           plotOutput(outputId = "map", fill = TRUE )

           )
    ),

  fluidRow(

    column(width = 12,

           tableOutput("view")

           )
    )
  )



server <- function(input, output) {



  datasetInput <- reactive({

    self_stake <- input$self_stake*10^3
    total_stake <- input$total_stake*10^6
    comm <- input$comm
    m_points <- input$m_points*10^3
    max_points <- input$max_points*10^3
    look_back <- input$look.back

    selection <- select_validator(data = eras_data, look.back = look_back,
                                  criteria = list(self_stake = self_stake,
                                                  total_stake = total_stake,
                                                  commission = comm,
                                                  n_active = look_back + 1,
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

  })



  output$map <- renderPlot({

    selection <- datasetInput()

    map("world", fill = FALSE,
        col = "grey40", bg = NULL, ylim =c(-60,70))

    points(selection$lon, selection$lat, pch = "+", col = "yellow")

  }, bg = "transparent")



  output$view <- renderTable({

    selection <- datasetInput()

    selection <- na.omit(selection[,colnames(selection) %in% c("validator_name",
                                                               "m_era",
                                                               "max_era",
                                                               "n_active",
                                                               "m_comm",
                                                               "m_self",
                                                               "m_total",
                                                               "last_active",
                                                               "continent")])

    colnames(selection) <- c("Name",
                             "Avg. Points",
                             "Max Points",
                             "N Active",
                             "Comm.",
                             "Self",
                             "Total",
                             "Last Active",
                             "Continent")

    selection

  })

}


shinyApp(ui = ui, server = server)
