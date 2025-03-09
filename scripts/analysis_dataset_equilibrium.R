library(tidyverse)

pop_data_baseline <- readRDS("results/population_main.rds")

source("scripts/sampling_functions.R")

# ###########################################################################
# ## sample from equilibrium
# ##########################################################################

## restrict to a single day at equilibrium (second to last day in simulation)
equilibrium_population <- pop_data_baseline %>% 
  filter(day == (365*25)) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  group_by(spatial_coefficient, coverage)
  
## then run sampling functions and conduct adult and larval capture sampling on each of these days
assay_results_eq <- NULL
control_results_eq <- NULL

for(i in 1:nrow(equilibrium_population)){
  
  working_data <- equilibrium_population[i,]
  c <- working_data$coverage
  s <- working_data$spatial_coefficient
  
  assay_results_eq <- bind_rows(assay_results_eq, c(resistance.assay(sample.type="adult", c, 100, 1000, working_data), n = 100))
  control_results_eq <- bind_rows(control_results_eq, c(control.assay(sample.type="adult", c, 100, 1000, working_data), n = 100))
  
  assay_results_eq <- bind_rows(assay_results_eq, c(resistance.assay(sample.type="larval", c, 100, 1000, working_data), n = 100))
  control_results_eq <- bind_rows(control_results_eq, c(control.assay(sample.type="larval", c, 100, 1000, working_data), n = 100))
}

readr::write_csv(assay_results_eq, "output/assay_results_eq.csv")
readr::write_csv(control_results_eq, "output/control_results_eq.csv")


## group_by coverage, spatial_structure, sample type, and n() to summarize mean, variance for each "sampling day"
assay_results_eq_cleaned <- assay_results_eq %>%
  rename(test_survival = survival) %>%
  rowid_to_column() %>%
  select(-c(test))

control_results_eq_cleaned <- control_results_eq %>%
  rename(control_survival = survival) %>%
  rowid_to_column() %>%
  select(-c(test))


## each line in the "full results" table is a single assay, drawn with `n` mosquitoes in each group, with a single 
## `coverage` and `spatial coefficient`, and a given `sample` type (either adult or larval)
full_results <- full_join(control_results_eq_cleaned,
                          assay_results_eq_cleaned,
                          by = c("rowid",
                                 "sample",
                                 "coverage",
                                 "spatial_coefficient",
                                 "resistance",
                                 "n")
                          )

## next use abbot correction to get means
full_results$corrected_survival <- mapply(control.adjustment, 
                                          100*full_results$test_survival, 
                                          100*full_results$control_survival)/100

readr::write_csv(full_results, "output/full_results_equilibrium.csv")
full_results <- readr::read_csv("output/full_results_equilibrium.csv")

## write a summary - each line gives the mean test survival, control survival, and observed
## resistance for a given coverage / sample / spatial coefficient / resistance level
assay_summary <- full_results %>%
  group_by(spatial_coefficient, coverage, sample, n) %>%
  summarize(test_survival = mean(test_survival),
            control_survival = mean(control_survival),
            resistance = mean(resistance), 
            observed_resistance = mean(corrected_survival))

## then get difference between larval and adult results
temp <- full_results %>% 
  filter(sample=="adult") %>%
  select(coverage, 
         spatial_coefficient, 
         resistance, 
         n, 
         adult_resistance = corrected_survival) %>%
  group_by(coverage, spatial_coefficient, resistance, n) %>%
  mutate(counter = row_number())
  
temp2 <- full_results %>% 
  filter(sample=="larval") %>%
  select(coverage, 
         spatial_coefficient, 
         resistance, 
         n, 
         larval_resistance = corrected_survival) %>%
  group_by(coverage, spatial_coefficient, resistance, n) %>%
  mutate(counter = row_number())

wide_results <- full_join(temp, temp2, by = c("coverage", 
                                              "spatial_coefficient", 
                                              "resistance", 
                                              "n",
                                              "counter")) %>%
  mutate(survival_difference = adult_resistance - larval_resistance, 
         adult_error = adult_resistance - resistance, 
         larval_error = larval_resistance - resistance)

readr::write_csv(wide_results, "output/wide_results_equilibrium.csv")

summary_data <- wide_results %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  summarise(mean_adult_resistance = mean(adult_resistance),
            mean_larval_resistance = mean(larval_resistance),
            mean_survival_difference = mean(survival_difference)) %>%
  mutate(mean_adult_error = mean_adult_resistance - resistance, 
         mean_larval_error = mean_larval_resistance - resistance)

readr::write_csv(summary_data, "output/summary_data_equilibrium.csv")
