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
prob_infections = c(0.15, 0.2, 0.25)
game_levels = c("Easy", "Medium", "Hard")
default_index = 2
default_level = game_levels[default_index]
default_prob = prob_infections[default_index]

# Load functions
source("initialisePersonStatuses.r")
source("produceGameBoard.r")
source("getNeighbours.r")
source("updatePersonStatuses.r")

# UI
ui <- basicPage(
  useShinyalert(),
  verbatimTextOutput("summaryText"),
  plotOutput("plotBoard", click = "plot_click"),
  selectInput("level", "Difficulty:",
              choices = game_levels, selected = default_level),
  verbatimTextOutput("rules")
)

# Server
server <- function(input, output) {
  
  # Set up game board
  init_data = initialise_person_statuses(I, J, start_num_infections,
                                   symptom_lambda, recovery_lambda)
  
  # Set probability of infection
  setup <- reactiveValues(prob = default_prob)
  observeEvent(input$level,{
    i = which(input$level == game_levels)
    setup$prob = prob_infections[i]
  })

  # Possible additions here:
  # - Variable time in quarantine
  # - Number of asymptotmatic people

  # Set up reactive values
  counter <- reactiveValues(countervalue = 0)
  x_coord <- reactiveValues(ref = NULL)
  y_coord <-reactiveValues(ref = NULL)
  df <- reactiveValues(infections = init_data)
  game_summary <- reactiveValues(win = NA,
                                 num_I_hidden = start_num_infections,
                                 num_I_shown = 0,
                                 num_R = 0)
  
  # Prompt updates with mouse click
  observeEvent(input$plot_click, {

    # Order:
    # * Get test coordinate
    # * Increase counter on infection period
    # * Get possible exposures
    # * Update infections
    # * Reveal those with symptoms
    # * Reveal those who recovered
    # * Reveal status of the person tested
    # * Quarantine those exposed
    # * Label those exposed

    # Get test coordinate
    x = round(as.numeric(input$plot_click$x))
    x = max(1, x);
    x = min(x, I);
    x_coord$ref = x
    
    y = round(as.numeric(input$plot_click$y))
    y = max(1, y);
    y = min(y, J);
    y_coord$ref = y
    
    # Increase counter on infection period
    counter$countervalue <- counter$countervalue + 1
    
    df$infections = update_person_statuses(df$infections, I, J, setup$prob,
                           x_coord$ref, y_coord$ref)
    
    # Game stats
    game_summary$num_I_shown = sum(df$infections$shown == "I")
    game_summary$num_I_hidden = sum(df$infections$hidden == "I")
    game_summary$num_R = sum(df$infections$shown == "R")
    game_win = (game_summary$num_I_shown == game_summary$num_I_hidden)
    game_loss = ((game_summary$num_I_hidden + game_summary$num_R) > I*J*perc)

    if(game_win)
      shinyalert(title = "You did it! \n All infections are quarantined", type = "success")
    if(game_loss)
      shinyalert(title = "Sorry you failed. \n The spread escaped", type = "info")

  })

  output$summaryText <- renderText({
    paste("Days:", counter$countervalue,
          " Shown:", game_summary$num_I_shown,
          " Hidden:", game_summary$num_I_hidden - game_summary$num_I_shown,
          " Recovered:", game_summary$num_R)
  })
  
  # Plot of the game board
  output$plotBoard <- renderPlot({

    produce_board_plot(df$infections,
                       quarantine_labels, quarantine_levels, 
                       infection_labels, infection_levels,
                       test_labels, test_levels)
    
  })

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
      "\n - People who have been tested can still catch the virus!",
      "\n - Infected people will recover on average after 14 days",
      "\n - And once infected people have immunity")
  })

}

shinyApp(ui, server)

# Issues:
# Fix the wrapping
# Update the probability of infection to better odds of winning / losing
# hard variables in rules 5 and 14
# Add a time in quarantine
# Easy, medium, hard probabilities
# Change board for social distancing
# Mobile capability
# Exposed factor level
# Write vignette about game play
# shiny tutorials

