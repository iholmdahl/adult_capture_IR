
## build sensitivity and specificity functions

sensitivity <- function(data, resistance_cutoff){
  
  ## if it's resistant, what is the probability test will say resistant?
  data %>%
    filter(resistance > resistance_cutoff) %>%
    # select(n, corrected_adult_survival, corrected_larval_survival)
    group_by(n) %>%
    mutate(sensitive_adult = case_when(corrected_adult_survival > resistance_cutoff ~ 1, 
                                       TRUE ~ 0), 
           sensitive_larval = case_when(corrected_larval_survival > resistance_cutoff ~ 1, 
                                        TRUE ~ 0)) %>% 
    summarise(sensitivity_adult = sum(sensitive_adult)/n(), 
              sensitivity_larval = sum(sensitive_larval)/n())
  
}


specificity <- function(data, resistance_cutoff){
  
  ## if it's not resistant, what is the probability test will say not resistant?
  
  data %>%
    filter(resistance < resistance_cutoff) %>%
    group_by(n) %>%
    mutate(specific_adult = case_when(corrected_adult_survival < resistance_cutoff ~ 1, 
                                      TRUE ~ 0), 
           specific_larval = case_when(corrected_larval_survival < resistance_cutoff ~ 1, 
                                       TRUE ~ 0)) %>%
    summarise(specificity_adult = sum(specific_adult)/n(), 
              specificity_larval = sum(specific_larval/n()))
  
}


positive_predictive_value <- function(data, resistance_cutoff){
  
  ## if the test says resistant, what is the probability it's really resistant?
  adult <- data %>%
    filter(corrected_adult_survival > resistance_cutoff) %>%
    mutate(positive = case_when(resistance > resistance_cutoff ~ 1, 
                                 TRUE ~ 0)) %>%
    group_by(n) %>%
    summarise(PPV_adults = sum(positive)/n())
  
  larval <- data %>%
    filter(corrected_larval_survival > resistance_cutoff) %>%
    mutate(positive = case_when(resistance > resistance_cutoff ~ 1, 
                                  TRUE ~ 0)) %>%
    group_by(n) %>%
    summarise(PPV_larval = sum(positive)/n())
  
  full_join(adult, larval, by = "n")
  
}


negative_predictive_value <- function(data, resistance_cutoff){
  
  ## if the test says not resistant, what is the probability it's really not resistant?
  
  adult <- data %>%
    filter(corrected_adult_survival < resistance_cutoff) %>%
    group_by(n) %>%
    mutate(negative = case_when(resistance < resistance_cutoff ~ 1, 
                                 TRUE ~ 0)) %>%
    summarise(NPV_adult = sum(negative)/n())
  
  larval <- data %>%
    filter(corrected_larval_survival/100 < resistance_cutoff) %>%
    group_by(n) %>%
    mutate(negative = case_when(resistance < resistance_cutoff ~ 1, 
                                  TRUE ~ 0)) %>%
    summarise(NPV_larval = sum(negative)/n())
  
  full_join(adult, larval, by = "n")
  
}

sensitivity(wide_results, 0.1)
specificity(wide_results, 0.1)

sensitivity(wide_results, 0.02)
specificity(wide_results, 0.02)

positive_predictive_value(wide_results, 0.1)
negative_predictive_value(wide_results, 0.1)

positive_predictive_value(wide_results, 0.02)
negative_predictive_value(wide_results, 0.02)

