warning("Should have used exponential not Poisson variables!")

library(shiny)
library(ggplot2)
library(shinyalert)
library(patchwork)

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
default_level = game_levels[2]
default_prob = prob_infections[2]

# UI
ui <- basicPage(
  useShinyalert(),
  verbatimTextOutput("summaryText"),
  plotOutput("plot1", click = "plot_click"),
  selectInput("level", "Difficulty:",
              choices = game_levels, selected = default_level),
  verbatimTextOutput("rules")
)

# Server
server <- function(input, output) {

  # Initialise grid and infection status
  init_data <- expand.grid(X = 1:I, Y = 1:J)
  init_data$shown <- rep("S", I*J)
  init_data$hidden <- rep("S", I*J)
  initial_infections <- sample(1:(I*J), start_num_infections)
  init_data$hidden[initial_infections] <- "I"
  init_data$hidden <- factor(init_data$hidden, levels = infection_levels)
  init_data$shown <- factor(init_data$shown, levels = infection_levels)

  # Infection time
  init_data$infection_period <- rep(NA, I*J)
  init_data$infection_period[initial_infections] <- 0

  # Time til symptoms show
  init_data$symptom_time <- rpois(I*J, lambda = symptom_lambda)

  # Time til recovery
  init_data$recovery_time <- init_data$symptom_time + rpois(I*J, lambda = recovery_lambda)

  # In Quarantine
  init_data$quarantined <- rep("No", I*J)
  init_data$quarantined <- factor(init_data$quarantined, levels = quarantine_levels)

  # Tested
  init_data$tested <- rep("unknown", I*J)
  init_data$tested <- factor(init_data$tested, levels = test_levels)

  # Set probability of infection
  setup <- reactiveValues(prob = default_prob)
  observeEvent(input$level,{
    i = which(input$level == game_levels)
    setup$prob = prob_infections[i]
  })

  # Time in quarantine
  # Add this

  # # Number of asymptotmatic people
  # init_data$symptom_time[sample(I*J, 0.05*I*J)] <- NA

  counter <- reactiveValues(countervalue = 0)
  x_coord <- reactiveValues(ref = NULL)
  y_coord <-reactiveValues(ref = NULL)
  df <- reactiveValues(infections = init_data)
  game_summary <- reactiveValues(win = NA,
                                 num_I_hidden = start_num_infections,
                                 num_I_shown = 0,
                                 num_R = 0)

  observeEvent(input$plot_click, {

    counter$countervalue <- counter$countervalue + 1

    x = round(as.numeric(input$plot_click$x))
    x = max(1, x);
    x = min(x, I);
    x_coord$ref = x

    y = round(as.numeric(input$plot_click$y))
    y = max(1, y);
    y = min(y, J);
    y_coord$ref = y

    # Order:
    # * Update infections
    # * Increase counter on infection period
    # * Reveal those with symptoms
    # * Reveal those who recovered
    # * Reveal status of the person tested
    # * Quarantine those exposed
    # * Label those exposed

    # Update with new infections
    i = which(df$infections$hidden == "I" &
                df$infections$quarantined == "No" ) #&
                # df$infections$shown != "R")
    contacts = c((i - 1),
              (i + 1),
              (i - I),
              (i + I))
    contacts = setdiff(contacts, i)
    #BUG!!!
    # Fix needed here, I can't infect people in quarantine with someone not in quarantine
    contacts = contacts[which(contacts > 0 & contacts <= I*J)]
    new_infections = rbinom(length(contacts), 1, setup$prob)
    infected_contacts = contacts[which(new_infections == 1)]
    df$infections$hidden[infected_contacts] = "I"
    df$infections$infection_period[infected_contacts]  = -1

    # Increase counter on infection period
    infectious_cases = df$infections$hidden == "I"
    df$infections$infection_period[infectious_cases] =
      df$infections$infection_period[infectious_cases] + 1

    # Reveal those with symptoms
    known_cases = df$infections$infection_period > df$infections$symptom_time
    df$infections$shown[known_cases] = "I"

    # Reval those who recovered
    recovered_cases = df$infections$infection_period > df$infections$recovery_time
    df$infections$hidden[recovered_cases] = "R"
    df$infections$shown[recovered_cases] = "R"

    # Reveal status of the person tested
    vec_ref = (y_coord$ref-1)*J + x_coord$ref
    df$infections$tested[vec_ref] = "tested"
    hidden_status = df$infections$hidden[vec_ref]
    if(hidden_status %in% c("I", "R"))
      df$infections$shown[vec_ref] = df$infections$hidden[vec_ref]

    # Quarantine those exposed
    i = which(df$infections$shown == "I")
    isolating = c(i,
                 (i - 1),
                 (i + 1),
                 (i - I),
                 (i + I))
    isolating = isolating[which(isolating > 0 & isolating <= I*J)]
    df$infections$quarantined[isolating] = "Yes"

    # # Label those exposed
    # exposed_cases = (df$infections$shown == "S" &
    #                    df$infections$quarantined == "Yes")
    # df$infections$shown[exposed_cases] = "E"

    # Game stats
    game_summary$num_I_shown = sum(df$infections$shown == "I")
    game_summary$num_I_hidden = sum(df$infections$hidden == "I")
    game_summary$num_R = sum(df$infections$shown == "R")

    # i = which(df$infections$hidden == "I")
    # game_summary$num_I_quarantined = sum(df$infections$quarantined[i] == "Yes")
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

  output$plot1 <- renderPlot({

    pnt_size = 3.5
    ggplot(df$infections) +
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
            legend.title = element_blank(), #element_text(size = 14),
            legend.text = element_text(size = 14),
            title = element_text(size = 16, hjust = 0.5)) +
      guides(fill = guide_legend(order=1),
              col = guide_legend(order=2),
              shape = guide_legend(order=3))
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

