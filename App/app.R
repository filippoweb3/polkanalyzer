library(shiny)
library(dplyr)
library(maps)
library(countrycode)
library(Polkanalyzer)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Polkanalyzer"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 5,

      # Input: Slider for the number of bins ----
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
                  value = 5, post = "%")

    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 12,

      tableOutput("view")

    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {


  output$view <- renderTable({

    self_stake <- input$self_stake
    total_stake <- input$total_stake
    comm <- input$comm

    selection <- select_validator(data = eras_data, look.back = 30,
                                  criteria = list(self_stake = self_stake*10^3,
                                                  total_stake = total_stake*10^6,
                                                  commission = comm,
                                                  n_active = 31,
                                                  mean_era_points = 60000,
                                                  max_era_points = 100000,
                                                  last_active = 31))

    selection <- merge(selection, candidates, by = "stash_address")

    selection <- selection[!selection$provider == "Hetzner Online GmbH" &
                             selection$id_verified == TRUE &
                             selection$democracyVoteCount >= 1 &
                             selection$councilVoteCount >= 1 &
                             selection$n_subid <= 3 &
                             selection$faluts <= 0 &
                             selection$offline <= 0,]

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
