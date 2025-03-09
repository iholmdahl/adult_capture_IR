library(tidyverse)

pop_data_baseline <- readRDS("results/population_main.rds")

source("scripts/sampling_functions.R")

# ###########################################################################
# ## sample from between 1 and 99 percent resistance
# ##########################################################################

## restrict distinct data to when RR% <= 99, at least 1 year after initiation

low_R_sample <- pop_data_baseline %>% 
  filter(day>365) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  filter(resistance >= 0.01 & resistance <= 0.99) %>%
  group_by(spatial_coefficient, coverage) %>%
  slice_sample(n=100, replace=TRUE)

  
## then run sampling functions and conduct adult and larval capture sampling on each of these days

assay_results <- NULL
control_results <- NULL

for(i in 1:nrow(low_R_sample)){
  
  working_data <- low_R_sample[i,]
  c <- working_data$coverage
  s <- working_data$spatial_coefficient
  
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="adult", c, 50, 100, working_data), n = 50))
  control_results <- bind_rows(control_results, c(control.assay(sample.type="adult", c, 50, 100, working_data), n = 50))
  
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="larval", c, 50, 100, working_data), n = 50))
  control_results <- bind_rows(control_results, c(control.assay(sample.type="larval", c, 50, 100, working_data), n = 50))
  
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="adult", c, 100, 100, working_data), n = 100))
  control_results <- bind_rows(control_results, c(control.assay(sample.type="adult", c, 100, 100, working_data), n = 100))
  
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="larval", c, 100, 100, working_data), n = 100))
  control_results <- bind_rows(control_results, c(control.assay(sample.type="larval", c, 100, 100, working_data), n = 100))
}

readr::write_csv(assay_results, "output/assay_results.csv")
readr::write_csv(control_results, "output/control_results.csv")


## group_by coverage, spatial_structure, sample type, and n() to summarize mean, variance for each "sampling day"
assay_results_cleaned <- assay_results %>%
  rename(test_survival = survival) %>%
  rowid_to_column() %>%
  select(-c(test))

control_results_cleaned <- control_results %>%
  rename(control_survival = survival) %>%
  rowid_to_column() %>%
  select(-c(test))


## each line in the "full results" table is a single assay, drawn with `n` mosquitoes in each group, with a single 
## `coverage` and `spatial coefficient`, and a given `sample` type (either adult or larval)
full_results <- full_join(control_results_cleaned,
                          assay_results_cleaned,
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

readr::write_csv(full_results, "output/full_results.csv")
full_results <- readr::read_csv("output/full_results.csv")

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

readr::write_csv(wide_results, "output/wide_results.csv")

summary_data <- wide_results %>%
  filter(n == 100) %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  summarise(mean_adult_resistance = mean(adult_resistance),
            mean_larval_resistance = mean(larval_resistance),
            mean_survival_difference = mean(survival_difference)) %>%
  mutate(mean_adult_error = mean_adult_resistance - resistance, 
         mean_larval_error = mean_larval_resistance - resistance)

readr::write_csv(summary_data, "output/summary_data.csv")

