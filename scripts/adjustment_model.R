library(dplyr)
library(tidyr)
library(ggpubr)
library(tibble)

source("./sampling_functions.R")

## load data from mosquito model

pop_data_baseline <- readRDS("results/population_main.rds")
summary_data <- read_csv("output/summary_data.csv")

larval.lm = lm(corrected_larval_survival ~ resistance, data=summary_data)
larval_r.squared <- summary(larval.lm)$r.squared  

adult.lm = lm(corrected_adult_survival ~ resistance, data=summary_data)
adult_r.squared <- summary(adult.lm)$r.squared  

### 
### non-linear fit
###

summary_data$resistance2 = summary_data$resistance**2

larval.lm_quad = lm(corrected_larval_survival ~ resistance + resistance2, data=summary_data)
larval_r.squared_quad <- summary(larval.lm_quad)$r.squared  

adult.lm_quad = lm(corrected_adult_survival ~ resistance + resistance2, data=summary_data)
adult_r.squared_quad <- summary(adult.lm_quad)$r.squared  


### tests of difference against adult survival
###

difference.lm = lm(survival_difference ~ corrected_adult_survival, data=summary_data)
difference_r.squared <- summary(difference.lm)$r.squared  

summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)
difference_r.squared_quad <- summary(difference.lm_quad)$r.squared  


###########################################################################
## set up new adjustment formula (use difference quad lm) 
###########################################################################

summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)

adult_adjustment <-  function(corrected_adult_survival){
  
  estimated_error <-  (difference.lm_quad$coefficients[["(Intercept)"]]
                          + difference.lm_quad$coefficients[["corrected_adult_survival"]]*corrected_adult_survival 
                          + difference.lm_quad$coefficients[["corrected_adult_survival2"]]*corrected_adult_survival**2)
  
  predicted_resistance = corrected_adult_survival- estimated_error
  
  return(pmin(pmax(predicted_resistance,0), 1))
}

adult_adjustment(0.02)

###########################################################################
## randomly sample and use larval, adult, or adult adjusted
##########################################################################

# 1000 samples from each type
## construct testing data frame 

test_data <- pop_data_baseline %>% 
  filter(day>500) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  filter(resistance < 0.22) %>%
  group_by(spatial_coefficient, coverage) %>%
  sample_n(1, replace=FALSE) # %>% 
  # bind_rows(equilibrium_sample)

assay_results_test <- NULL
control_results_test <- NULL

for(i in 1:nrow(test_data)){
  
  working_data <- test_data[i,]
  c <- working_data$coverage
  s <- working_data$spatial_coefficient
  
  assay_results_test <- rbind(assay_results_test, resistance.assay(sample.type="adult", c, 100, 100, working_data))
  assay_results_test <- rbind(assay_results_test, resistance.assay(sample.type="larval", c, 100, 100, working_data))
  
  control_results_test <- rbind(control_results_test, control.assay(sample.type="adult", c, 100, 100, working_data))
  control_results_test <- rbind(control_results_test, control.assay(sample.type="larval", c, 100, 100, working_data))
  
}

## group_by coverage, spatial_structure and sample type to summarize mean, variance
assay_results_test %>%
  rowid_to_column("ID") %>%
  rename(test_survival = survival) %>%
  select(-c(test))-> assay_results_test

control_results_test %>%
  rowid_to_column("ID") %>%
  rename(control_survival = survival) %>%
  select(-c(test)) -> control_results_test

full_results_test <- full_join(assay_results_test, control_results_test)

## next use abbot correction to get means
full_results_test$corrected_survival<- mapply(control.adjustment, 100*full_results_test$test_survival, 100*full_results_test$control_survival)

## then get difference between larval and adult results
full_results_test %>% 
  filter(sample=="adult") %>%
  select(-c("ID", "sample")) %>%
  rowid_to_column("ID") %>%
  rename(control_adult_survival = control_survival,
         test_adult_survival = test_survival,
         corrected_adult_survival = corrected_survival) -> temp

full_results_test %>% 
  filter(sample=="larval") %>%
  select(-c("ID", "sample")) %>%
  rowid_to_column("ID") %>%
  rename(control_larval_survival = control_survival,
         test_larval_survival = test_survival,
         corrected_larval_survival = corrected_survival) -> temp2

wide_results_test <- full_join(temp, temp2) %>%
  mutate(predicted_resistance = adult_adjustment(corrected_adult_survival/100),
         corrected_adult_survival = corrected_adult_survival/100,
         corrected_larval_survival = corrected_larval_survival/100)

write.csv(wide_results_test, "output/wide_results_test.csv")

wide_results_test %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  summarize(corrected_adult_survival = mean(corrected_adult_survival),
            corrected_larval_survival = mean(corrected_larval_survival),
            predicted_resistance = mean(predicted_resistance)) -> summary_data_test



###########################################################################
## then calculate the mean square difference between sample and true resistance
##########################################################################

wide_results_test <- wide_results_test %>% 
  mutate(difference_adult = (corrected_adult_survival-resistance),
         difference_larval = (corrected_larval_survival-resistance),
         difference_predicted = (predicted_resistance-resistance), 
         square_difference_adult = difference_adult**2, 
         square_difference_larval = difference_larval**2, 
         square_difference_predicted = difference_predicted**2)

wide_results_test %>%
  summarize(MSE_adult = mean(difference_adult),
            MSE_larval = mean(square_difference_larval),
            MSE_pred = mean(square_difference_predicted), 
            mean_difference_adult = mean(difference_adult),
            mean_difference_larval = mean(difference_larval),
            mean_difference_pred = mean(difference_predicted), 
            root_MSE_adult = sqrt(MSE_adult),
            root_MSE_larval = sqrt(MSE_larval),
            root_MSE_pred = sqrt(MSE_pred)) %>%
  t()  -> MSE_summary


long_results_test <- wide_results_test %>%
  select(square_difference_adult, square_difference_larval, square_difference_predicted) %>%
  pivot_longer(cols=square_difference_adult:square_difference_predicted, names_to = c("sample")) %>%
  as_data_frame()

long_results_test %>%
  ggplot() + 
  geom_histogram(aes(x=value), fill="blue", alpha=0.5) + 
  xlim(0,0.05)+
  facet_wrap(~sample)+
  theme_bw()
