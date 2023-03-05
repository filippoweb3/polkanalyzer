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
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "self_stake",
                  label = "Self Stake",
                  min = 0,
                  max = 20,
                  value = 5, post = "K")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tableOutput("view")

    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {


  output$view <- renderTable({

    self_stake <- input$self_stake

    selection <- select_validator(data = eras_data, look.back = 30,
                                  criteria = list(self_stake = self_stake*1000,
                                                  total_stake = 2130000,
                                                  commission = 5,
                                                  n_active = 31,
                                                  mean_era_points = 60000,
                                                  max_era_points = 100000,
                                                  last_active = 31))

    head(selection)

  })

}


shinyApp(ui = ui, server = server)
