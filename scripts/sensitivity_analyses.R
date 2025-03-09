################################################################################
## larval and adult scatter plot - with full data 
################################################################################
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
library(scales)
library(foreach)
library(doParallel)

## load data 
analyses <- c("main", 
              "mortality",
              "three", 
              "int_mortality", 
              "larvae_0.01", 
              "larvae_0.05", 
              "dominance_low", 
              "dominance_high", 
              "longevity")


# numCores <- detectCores()-1
# registerDoParallel(numCores)

# foreach(analysis=analyses, .combine=rbind) %dopar% {

for(analysis in analyses){
  
  summary_data <- c()
  pop_data <- readRDS(paste0("results/population_",analysis,".rds"))

  if(analysis == "longevity"){
    source("./scripts/sampling_functions_longevity.R")
    } else{
    source("./scripts/sampling_functions.R")
  }

  pop_data %>% 
    filter(day>365) %>%
    mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
    filter(resistance < 0.99) %>%
    group_by(spatial_coefficient, coverage) %>%
    sample_n(10, replace=TRUE) -> non_equilibrium_sample
  
  #check length
  nrow(non_equilibrium_sample)
  
  ## then run sampling functions
  assay_results <- NULL
  control_results <- NULL
  
  for(i in 1:nrow(non_equilibrium_sample)){
    
    working_data <- non_equilibrium_sample[i,]
    c <- working_data$coverage
    s <- working_data$spatial_coefficient
    
    assay_results <- rbind(assay_results, resistance.assay(sample.type="adult", c, 100, 100, working_data))
    assay_results <- rbind(assay_results, resistance.assay(sample.type="larval", c, 100, 100, working_data))
    
    control_results <- rbind(control_results, control.assay(sample.type="adult", c, 100, 100, working_data))
    control_results <- rbind(control_results, control.assay(sample.type="larval", c, 100, 100, working_data))
    
  }
  
  ## group_by coverage, spatial_structure and sample type to summarize mean, variance
  assay_results %>%
    rowid_to_column("ID") %>%
    rename(test_survival = survival) %>%
    select(-c(test))-> assay_results
  
  control_results %>%
    rowid_to_column("ID") %>%
    rename(control_survival = survival) %>%
    select(-c(test)) -> control_results
  
  full_results <- full_join(assay_results, control_results)
  
  full_results %>% 
    group_by(spatial_coefficient, coverage, sample) %>%
    summarize(test_survival = mean(test_survival), 
              control_survival = mean(control_survival),
              resistance = mean(resistance)) -> assay_summary
  
  ## next use abbot correction to get means
  full_results$corrected_survival<- mapply(control.adjustment, 100*full_results$test_survival, 100*full_results$control_survival)
  
  ## then get difference between larval and adult results
  full_results %>% 
    filter(sample=="adult") %>%
    select(-c("ID", "sample")) %>%
    rowid_to_column("ID") %>%
    rename(control_adult_survival = control_survival,
           test_adult_survival = test_survival,
           corrected_adult_survival = corrected_survival) -> temp
  
  full_results %>% 
    filter(sample=="larval") %>%
    select(-c("ID", "sample")) %>%
    rowid_to_column("ID") %>%
    rename(control_larval_survival = control_survival,
           test_larval_survival = test_survival,
           corrected_larval_survival = corrected_survival) -> temp2
  
  wide_results <- full_join(temp, temp2) %>%
    mutate(survival_difference = corrected_adult_survival - corrected_larval_survival)
  
  readr::write_csv(wide_results, paste0("output/sensitivity_analyses/wide_results_",analysis,".csv"))
  # wide_results <- readr::read_csv(paste0("output/sensitivity_analyses/wide_results_",analysis,".csv"))
  
  summary_data <- wide_results %>%
    group_by(spatial_coefficient, coverage, resistance) %>%
    summarize(corrected_adult_survival = mean(corrected_adult_survival)/100,
              corrected_larval_survival = mean(corrected_larval_survival)/100,
              survival_difference = mean(survival_difference)/100)
  
  summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
  difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)
  
  summary_data %>%
    mutate(predicted_resistance = difference.lm_quad$coefficients["(Intercept)"] 
           + difference.lm_quad$coefficients["corrected_adult_survival"]*corrected_adult_survival 
           + difference.lm_quad$coefficients["corrected_adult_survival2"]*corrected_adult_survival2) -> summary_data
  
  
  readr::write_csv(summary_data, paste0("output/sensitivity_analyses/summary_data_",analysis,".csv"))
  
}

compiled_data <- NULL

for(analysis in analyses){
  
  upload_data <- read_csv(paste0("output/sensitivity_analyses/summary_data_",analysis,".csv"))

  compiled_data <- upload_data %>%
    mutate(Simulation = analysis) %>%
    bind_rows(compiled_data) 
  
}

compiled_data <- compiled_data %>%
  mutate(corrected_larval_survival2 = corrected_adult_survival**2)

write.csv(compiled_data, "output/sensitivity_analyses/compiled_data.csv")
# compiled_data <- readr::read_csv("output/sensitivity_analyses/compiled_data.csv")

sensitivity_curves <- c()
for(analysis in analyses){
  
  subset <- compiled_data %>%
    filter(Simulation == analysis)
    
  adult_fit_sensitivity = lm(resistance ~ corrected_adult_survival + corrected_adult_survival2, data=subset)
  larval_fit_sensitivity = lm(resistance ~ corrected_larval_survival + corrected_larval_survival2, data=subset)
    
  sensitivity_curves <- data_frame(observed_resistance = seq(0, 1, by = 0.01)) %>%
    mutate(resistance_adult = (adult_fit_sensitivity$coefficients["(Intercept)"] 
                                   + adult_fit_sensitivity$coefficients["corrected_adult_survival"]*observed_resistance 
                                   + adult_fit_sensitivity$coefficients["corrected_adult_survival2"]*(observed_resistance**2)), 
           resistance_larval = (larval_fit_sensitivity$coefficients["(Intercept)"] 
                               + larval_fit_sensitivity$coefficients["corrected_larval_survival"]*observed_resistance 
                               + larval_fit_sensitivity$coefficients["corrected_larval_survival2"]*(observed_resistance**2)),
           Simulation = analysis) %>%
    bind_rows(sensitivity_curves)
  
}

write_csv(sensitivity_curves, "output/sensitivity_analysis_data.csv")
