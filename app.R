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
prob_infections = c(0.15, 0.2, 0.25)
game_levels = c("Easy", "Medium", "Hard")
default_index = 2
default_level = game_levels[default_index]
default_prob = prob_infections[default_index]

# Load functions
# source("initialisePersonStatuses.r")
# A function to initialise the game board
initialise_person_statuses <- function(I, J, start_num_infections, 
                                       symptom_lambda, recovery_lambda){
  
  # Initialise grid and displayed infection status
  init_data <- expand.grid(X = 1:I, Y = 1:J)
  init_data$shown <- rep("S", I*J)
  
  # Initialise quarantine state
  init_data$quarantined <- rep("No", I*J)
  
  # Initialise test state
  init_data$tested <- rep("unknown", I*J)
  
  # Randomly initialise the hidden infections
  init_data$hidden <- rep("S", I*J)
  initial_infections <- sample(1:(I*J), start_num_infections)
  init_data$hidden[initial_infections] <- "I"
  
  # Initialise the infection time
  init_data$infection_period <- rep(NA, I*J)
  init_data$infection_period[initial_infections] <- 0
  
  # Randomly initialise the time until symptoms show
  init_data$symptom_time <- rpois(I*J, lambda = symptom_lambda)
  
  # Randomly initialise the time until recovery
  init_data$recovery_time <- init_data$symptom_time + rpois(I*J, lambda = recovery_lambda)
  
  return(init_data)
  
}

# source("produceGameBoard.r")
# A function to plot the game board
produce_board_plot <- function(plot_df, 
                               quarantine_labels, quarantine_levels, 
                               infection_labels, infection_levels,
                               test_labels, test_levels){
  
  plot_df$shown <- factor(plot_df$shown, levels = infection_levels)
  plot_df$hidden <- factor(plot_df$hidden, levels = infection_levels)
  plot_df$quarantined <- factor(plot_df$quarantined, levels = quarantine_levels)
  plot_df$tested <- factor(plot_df$tested, levels = test_levels)
  
  pnt_size = 3.5
  board_plot <- ggplot(plot_df) +
    geom_point(aes(x = X, y = Y, col = quarantined),
               shape = 15, size = pnt_size + 2, alpha = 0.8) +
    scale_color_manual("EXPOSURE STATUS",
                       labels = quarantine_labels,
                       values = c("No" = "lightgray", "Yes" = "coral"),
                       drop = FALSE) +
    geom_point(aes(X, Y, fill = shown),
               shape = 21, size = pnt_size) +
    scale_fill_manual("INFECTION STATUS",
                      labels = infection_labels,
                      values = c("S" = "gray", "I" = "red", "R" = "blue"),
                      drop = FALSE) +
    geom_point(aes(X, Y, shape = tested), size = pnt_size, stroke = 1) +
    scale_shape_manual("TEST STATUS",
                       labels = test_labels,
                       values = c("unknown" = 1, "tested" = 13),
                       drop = FALSE) +
    coord_fixed() +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.title = element_blank(),
          legend.text = element_text(size = 14),
          title = element_text(size = 16, hjust = 0.5)) +
    guides(fill = guide_legend(order=1),
           col = guide_legend(order=2),
           shape = guide_legend(order=3))
  
  return(board_plot)
  
}

# source("getNeighbours.r")
# A function to get the neighbours
get_neighbours <- function(i, I, J){
  # indexing is by column first then rows 
  row_index = i%%I + I*(i%%I == 0); 
  col_index = ceiling(i/I)
  left_nbr   = i - I + (col_index == 1)*(I*J)
  right_nbr  = i + I - (col_index == J)*(I*J)
  top_nbr    = i - 1 + (row_index == 1)*I
  bottom_nbr = i + 1 - (row_index == I)*I
  # later part of each nbr equation addresses boundary cases
  # the board wraps like on a taurus
  nbrs <- c(left_nbr, right_nbr, top_nbr, bottom_nbr)
  return(nbrs)
}

# source("updatePersonStatuses.r")
update_person_statuses <- function(person_data, I, J, 
                                   infection_prob,
                                   test_x, test_y){
  
  # Increase counter on infection period
  infectious_cases = (person_data$hidden == "I")
  person_data$infection_period[infectious_cases] =
    person_data$infection_period[infectious_cases] + 1
  
  # Get neighbours of infected people
  i = which(person_data$hidden == "I" & 
              person_data$quarantined == "No")
  neighbours = get_neighbours(i, I, J)
  already_quarantined = which(person_data$quarantined == "Yes")
  exceptions = c(i, already_quarantined)
  contacts = neighbours[!(neighbours %in% exceptions)]
  
  # Update with new infections
  new_infections = rbinom(length(contacts), 1, infection_prob)
  infected_contacts = unique(contacts[which(new_infections == 1)])
  person_data$hidden[infected_contacts] = "I"
  person_data$infection_period[infected_contacts] = 0
  
  # Reveal those with symptoms
  known_cases = (person_data$infection_period > person_data$symptom_time)
  person_data$shown[known_cases] = "I"
  
  # Reveal those who recovered
  recovered_cases = (person_data$infection_period > person_data$recovery_time)
  person_data$hidden[recovered_cases] = "R"
  person_data$shown[recovered_cases] = "R"
  
  # Reveal status of the person tested
  vec_ref = (test_y-1)*J + test_x
  person_data$tested[vec_ref] = "tested"
  person_data$shown[vec_ref] = person_data$hidden[vec_ref]
  
  # Quarantine those exposed
  i = which(person_data$shown == "I")
  isolating = c(i, get_neighbours(i, I, J))
  person_data$quarantined[isolating] = "Yes"
  
  return(person_data)
  
}

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
  level <- reactiveValues(prob = default_prob)
  observeEvent(input$level,{
    i = which(input$level == game_levels)
    level$prob = prob_infections[i]
    person_data$infections = initialise_person_statuses(I, J, start_num_infections,
                                                        symptom_lambda, recovery_lambda)
  })

  # Set up reactive values
  counter <- reactiveValues(countervalue = 0)
  test_coord <- reactiveValues(x = NULL, y = NULL)
  person_data <- reactiveValues(infections = init_data)
  game_summary <- reactiveValues(num_I_hidden = start_num_infections,
                                 num_I_shown = 0,
                                 num_R = 0)
  
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
    person_data$infections = update_person_statuses(person_data$infections, I, J, level$prob,
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


