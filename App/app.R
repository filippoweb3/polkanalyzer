library(shiny)
library(dplyr)
library(maps)
library(countrycode)
library(Polkanalyzer)

ui <- fluidPage(

  titlePanel("Polkanalyzer"),

  sidebarLayout(

    sidebarPanel(width = 5,

      sliderInput(inputId = "self_stake",
                  label = "Self Stake (DOT)",
                  min = 0,
                  max = 20,
                  value = 5, post = "K"),

      sliderInput(inputId = "total_stake",
                  label = "Total Stake (DOT)",
                  min = 1.7,
                  max = 2.5,
                  value = 2.13, post = "M"),

      sliderInput(inputId = "comm",
                  label = "Commission",
                  min = 0,
                  max = 10,
                  value = 5, post = "%"),

      sliderInput(inputId = "m_points",
                  label = "Avg. Points",
                  min = 0,
                  max = 100,
                  value = 60, post = "K"),

      sliderInput(inputId = "max_points",
                  label = "Max. Points",
                  min = 0,
                  max = 110,
                  value = 100, post = "K")

    ),

    mainPanel(width = 12,

      plotOutput(outputId = "map"),

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

    selection <- select_validator(data = eras_data, look.back = 30,
                                  criteria = list(self_stake = self_stake,
                                                  total_stake = total_stake,
                                                  commission = comm,
                                                  n_active = 31,
                                                  mean_era_points = m_points,
                                                  max_era_points = max_points,
                                                  last_active = 31))

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

    map("world", fill = FALSE, col = rgb(0,0,0,0.2), bg = "white")

    points(selection$lon, selection$lat, pch = "+")

  })

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

    selection

  })

}


shinyApp(ui = ui, server = server)
