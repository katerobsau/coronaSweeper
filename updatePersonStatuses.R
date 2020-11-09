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
  contacts = setdiff(neighbours, exceptions)

  # Update with new infections
  new_infections = rbinom(length(contacts), 1, infection_prob)
  infected_contacts = contacts[which(new_infections == 1)]
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