# Load packages
library(shiny)
library(ggplot2)
library(shinyalert)

# Initialise parameters
start_num_infections = 4
symptom_lambda = 5
recovery_lambda = 14
I = 20;
J = 20;
perc = 0.5;
infection_levels = c("S", "I", "R")
infection_labels = c("Susceptible", "Infected", "Recovered")
test_levels = c("tested", "unknown")
test_labels = c("Tested", "Not Tested")
quarantine_levels = c("Yes", "No")
quarantine_labels = c("Quarantined", "No Restrictions")
default_prob = 0.2

# Load functions
source("initialisePersonStatuses.r")
source("produceGameBoard.r")
source("getNeighbours.r")
source("updatePersonStatuses.r")

# User Interface
ui <- basicPage(
  useShinyalert(),
  verbatimTextOutput("summaryText"),
  plotOutput("plotBoard", click = "plot_click"),
  sliderInput("prob", "Infection Probability:",
              min = 0, max = 0.5, value = default_prob),
  verbatimTextOutput("rules")
)

# Server
server <- function(input, output) {
  
  # Set up game board
  init_data = initialise_person_statuses(I, J, start_num_infections,
                                         symptom_lambda, recovery_lambda)
  
  # Set up reactive values
  counter <- reactiveValues(countervalue = 0)
  test_coord <- reactiveValues(x = NULL, y = NULL)
  person_data <- reactiveValues(infections = init_data)
  game_summary <- reactiveValues(num_I_hidden = start_num_infections,
                                 num_I_shown = 0,
                                 num_R = 0)
  setup <- reactiveValues(prob = default_prob)
  
  # Set up difficulty levels
  observeEvent(input$prob,{
    person_data$infections = initialise_person_statuses(I, J, start_num_infections,
                                                        symptom_lambda, recovery_lambda)
    counter$countervalue = 0
    game_summary$num_I_hidden = 4
    game_summary$num_I_shown = 0
    game_summary$num_R = 0
  })
  
  # Prompt updates with mouse click
  observeEvent(input$plot_click, {

    # Get test coordinate
    x = round(as.numeric(input$plot_click$x))
    x = max(1, x);
    x = min(x, I);
    test_coord$x = x
    
    y = round(as.numeric(input$plot_click$y))
    y = max(1, y);
    y = min(y, J);
    test_coord$y = y
    
    # Increase counter on infection period
    counter$countervalue <- counter$countervalue + 1
    
    # Simulate new infections and update statuses
    person_data$infections = update_person_statuses(person_data$infections, I, J, input$prob,
                           test_coord$x, test_coord$y)
    
    # Game stats
    game_summary$num_I_shown = sum(person_data$infections$shown == "I")
    game_summary$num_I_hidden = sum(person_data$infections$hidden == "I")
    game_summary$num_R = sum(person_data$infections$shown == "R")
    game_win = (game_summary$num_I_shown == game_summary$num_I_hidden)
    game_loss = ((game_summary$num_I_hidden + game_summary$num_R) > I*J*perc)

    # Game end
    if(game_win)
      shinyalert(title = "You did it! \n All infections are quarantined", type = "success")
    if(game_loss)
      shinyalert(title = "Sorry you failed. \n The spread escaped", type = "info")

  })

  # Game summary text 
  output$summaryText <- renderText({
    paste("Days:", counter$countervalue,
          " Shown:", game_summary$num_I_shown,
          " Hidden:", game_summary$num_I_hidden - game_summary$num_I_shown,
          " Recovered:", game_summary$num_R)
  })
  
  # Plot of the game board
  output$plotBoard <- renderPlot({

    produce_board_plot(person_data$infections,
                       quarantine_labels, quarantine_levels, 
                       infection_labels, infection_levels,
                       test_labels, test_levels)
    
  })

  # Game rules
  output$rules <- renderText({

    paste0(
      "Goal: Find all the infected people before more than ", round(I*J*perc) , " get infected!",
      "\n",
      "\n Game Setup:",
      "\n - The game starts with 4 infected people",
      "\n - By clicking on people you can test if they are infected",
      "\n - Upon each new mouse click new people can become infected",
      "\n - People can only be infected by their 4 direct neighbours",
      "\n",
      "\n To help you out:",
      "\n - Infected people show symptoms on average after 5 days",
      "\n - A person showing symptoms is quarantined, along with thier neighbours.",
      "\n - Once a person is in quarantine they stop spreading the virus",
      "\n - However, that doesn't mean they haven't already infected someone!",
      "\n",
      "\n Beware:",
      "\n - People who have been tested can still catch the virus in the future!",
      "\n - Infected people will recover on average after 14 days",
      "\n - And once recovered those people have immunity")
  })

}

shinyApp(ui, server)

# Possible additions here:
# - Variable time in quarantine
# - Number of asymptotmatic people


