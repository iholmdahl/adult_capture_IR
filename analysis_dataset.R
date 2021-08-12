library(tidyverse)

pop_data_baseline <- readRDS("pop_data_baseline")

# ###########################################################################
# ## sample from between 1 and 99 percent resistance
# ##########################################################################

## restrict distinct data to when RR% < 99, at least 1 year after initiation

pop_data_baseline %>% 
  filter(day>365) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  filter(resistance >= 0.01 & resistance <= 0.99) %>%
  group_by(spatial_coefficient, coverage) %>%
  slice_sample(n=10, replace=FALSE) -> low_R_sample

## then run sampling functions

assay_results <- NULL
control_results <- NULL

for(i in 1:nrow(low_R_sample)){
  
  working_data <- low_R_sample[i,]
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
  group_by(spatial_coefficient, coverage, sample, day) %>%
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

# write.csv(wide_results, "wide_results.csv")
# wide_results <- read.csv("wide_results.csv")

wide_results %>%
  group_by(spatial_coefficient, coverage, resistance, day) %>%
  summarize(corrected_adult_survival = mean(corrected_adult_survival)/100,
            corrected_larval_survival = mean(corrected_larval_survival)/100,
            survival_difference = mean(survival_difference)/100) -> summary_data

# write.csv(summary_data, "summary_data.csv")

