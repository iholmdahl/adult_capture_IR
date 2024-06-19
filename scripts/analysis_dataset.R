library(tidyverse)

pop_data_baseline <- readRDS("results/population_main.rds")

source("scripts/sampling_functions.R")

# ###########################################################################
# ## sample from between 1 and 99 percent resistance
# ##########################################################################

## restrict distinct data to when RR% <= 99, at least 1 year after initiation

pop_data_baseline %>% 
  filter(day>365) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  filter(resistance >= 0.01 & resistance <= 0.99) %>%
  group_by(spatial_coefficient, coverage) %>%
  slice_sample(n=1000, replace=FALSE) -> low_R_sample

## then run sampling functions

assay_results <- NULL
control_results <- NULL

for(i in 1:nrow(low_R_sample)){
  
  working_data <- low_R_sample[i,]
  c <- working_data$coverage
  s <- working_data$spatial_coefficient
  
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="adult", c, 50, 100, working_data), n = 50))
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="larval", c, 50, 1, working_data), n = 50))
  
  control_results <- bind_rows(control_results, c(control.assay(sample.type="adult", c, 50, 1, working_data), n = 50))
  control_results <- bind_rows(control_results, c(control.assay(sample.type="larval", c, 50, 1, working_data), n = 50))
  
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="adult", c, 100, 1, working_data), n = 100))
  assay_results <- bind_rows(assay_results, c(resistance.assay(sample.type="larval", c, 100, 1, working_data), n = 100))
  
  control_results <- bind_rows(control_results, c(control.assay(sample.type="adult", c, 100, 1, working_data), n = 100))
  control_results <- bind_rows(control_results, c(control.assay(sample.type="larval", c, 100, 1, working_data), n = 100))
}

## group_by coverage, spatial_structure and sample type to summarize mean, variance
assay_results %>%
  rename(test_survival = survival) %>%
  select(-c(test))-> assay_results

control_results %>%
  rename(control_survival = survival) %>%
  select(-c(test)) -> control_results

full_results <- full_join(assay_results, control_results, by = c("sample", 
                                                                 "coverage", 
                                                                 "spatial_coefficient", 
                                                                 "resistance", 
                                                                 "n"))

full_results %>%
  group_by(spatial_coefficient, coverage, sample, n) %>%
  summarize(test_survival = mean(test_survival),
            control_survival = mean(control_survival),
            resistance = mean(resistance)) -> assay_summary

## next use abbot correction to get means
full_results$corrected_survival <- mapply(control.adjustment, 
                                          100*full_results$test_survival, 
                                          100*full_results$control_survival)/100

## then get difference between larval and adult results
full_results %>% 
  filter(sample=="adult") %>%
  select(-c("sample")) %>%
  rename(control_adult_survival = control_survival,
         test_adult_survival = test_survival,
         corrected_adult_survival = corrected_survival) -> temp

full_results %>% 
  filter(sample=="larval") %>%
  select(-c("sample")) %>%
  rename(control_larval_survival = control_survival,
         test_larval_survival = test_survival,
         corrected_larval_survival = corrected_survival) -> temp2

wide_results <- full_join(temp, temp2, by = c("coverage", 
                                              "spatial_coefficient", 
                                              "resistance", 
                                              "n")) %>%
  mutate(survival_difference = corrected_adult_survival - corrected_larval_survival, 
         unadjusted_survival_difference = test_adult_survival - test_larval_survival)

# write.csv(wide_results, "wide_results.csv")
# wide_results <- read.csv("wide_results.csv")

wide_results %>%
  filter(n == 100) %>%
  group_by(spatial_coefficient, coverage, resistance, n) %>%
  mutate(corrected_adult_survival = corrected_adult_survival,
         corrected_larval_survival = corrected_larval_survival,
         survival_difference = survival_difference, 
         unadjusted_survival_difference = unadjusted_survival_difference) -> summary_data

wide_results %>%
  filter(n == 100) %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  mutate(corrected_adult_survival = mean(corrected_adult_survival),
         corrected_larval_survival = mean(corrected_larval_survival),
         survival_difference = mean(survival_difference), 
         unadjusted_survival_difference = mean(unadjusted_survival_difference)) -> summarized_data

# write.csv(summary_data, "output/summary_data.csv")

