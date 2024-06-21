library(dplyr)
library(tidyr)
library(stringr)
library(viridis)

summary_data <- read.csv("output/summary_data.csv")
pop_data_baseline <- readRDS("results/population_main.rds")

source("./sampling_functions.R")

equilibrium_data <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day==max(day)-1) 

################################################################################
##### construct data frame for Fig 2 (heatmaps)
################################################################################

time_cutoffs <- c(365*25)
time_cutoff_labels <- paste0(time_cutoffs/365," years")

pop_data_baseline %>% 
  filter(day %in% (time_cutoffs)) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) -> non_equilibrium_sample

nrow(non_equilibrium_sample)

## then run sampling functions
assay_results_low <- NULL
control_results_low <- NULL

for(i in 1:nrow(non_equilibrium_sample)){
  
  working_data <- non_equilibrium_sample[i,]
  c <- working_data$coverage
  s <- working_data$spatial_coefficient
  
  assay_results_low <- rbind(assay_results_low, resistance.assay(sample.type="adult", c, 100, 100, working_data))
  assay_results_low <- rbind(assay_results_low, resistance.assay(sample.type="larval", c, 100, 100, working_data))
  
  control_results_low <- rbind(control_results_low, control.assay(sample.type="adult", c, 100, 100, working_data))
  control_results_low <- rbind(control_results_low, control.assay(sample.type="larval", c, 100, 100, working_data))
  
}

readr::write_csv(assay_results_low, "output/assay_results_equilibrium.csv")
readr::write_csv(control_results_low, "output/control_results_equilibrium.csv")

## group_by coverage, spatial_structure and sample type to summarize mean, variance
assay_results_low %>%
  rowid_to_column("ID") %>%
  rename(test_survival = survival) %>%
  select(-c(test))-> assay_results_low

control_results_low %>%
  rowid_to_column("ID") %>%
  rename(control_survival = survival) %>%
  select(-c(test)) -> control_results_low

full_results_low <- full_join(assay_results_low, control_results_low)

full_results_low %>% 
  group_by(spatial_coefficient, coverage, sample) %>%
  summarize(test_survival = mean(test_survival), 
            control_survival = mean(control_survival),
            resistance = mean(resistance)) -> assay_summary_low

## next use abbot correction to get means
full_results_low$corrected_survival<- mapply(control.adjustment, 100*full_results_low$test_survival, 100*full_results_low$control_survival)

## then get difference between larval and adult results
full_results_low %>% 
  filter(sample=="adult") %>%
  select(-c("ID", "sample")) %>%
  rowid_to_column("ID") %>%
  rename(control_adult_survival = control_survival,
         test_adult_survival = test_survival,
         mean_adult_resistance = corrected_survival) -> temp

full_results_low %>% 
  filter(sample=="larval") %>%
  select(-c("ID", "sample")) %>%
  rowid_to_column("ID") %>%
  rename(control_larval_survival = control_survival,
         test_larval_survival = test_survival,
         mean_larval_resistance = corrected_survival) -> temp2

wide_results_low <- full_join(temp, temp2) %>%
  mutate(survival_difference = mean_adult_resistance - mean_larval_resistance)

wide_results_low %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  summarize(mean_adult_resistance = mean(mean_adult_resistance)/100,
            mean_larval_resistance = mean(mean_larval_resistance)/100,
            survival_difference = mean(survival_difference)/100) -> summary_data_low

working_data <- non_equilibrium_sample %>%
  group_by(coverage, spatial_coefficient)

readr::write_csv(working_data, "output/fig2_data.csv")
