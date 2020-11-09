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