library(dplyr)
library(tidyr)
library(ggpubr)
library(tibble)

setwd("/Users/Inga/Documents/GitHub/mosquitoIR")
source("./sampling_functions.R")

## load data from mosquito model

pop_data <- readRDS("pop_data_baseline")

summary_data <- read_csv("summary_data.csv")

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


### 
### tests of larval and adult sample regressions agaisnt x=y
###

larval_plot <- summary_data %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=corrected_larval_survival), color = "#009432", alpha = 0.7) + 
  geom_smooth(aes(x=resistance, y=corrected_larval_survival), method = "lm", se = FALSE, color = "black", size=1) + 
  scale_x_continuous(name = "Resistance", limits=c(0,1), labels = scales::percent)+
  scale_y_continuous(name = "Survival", limits = c(0,1), labels = scales::percent)+
  theme_bw()+
  labs(title=paste0("Linear model, r^2 = ", round(summary(larval.lm)$r.squared, digits=3)))

larval_plot_quad <- summary_data %>%
  mutate(predicted_resistance = 0.007127+ 0.965532*resistance +0.021571*resistance2) %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=corrected_larval_survival), color = "#009432", alpha = 0.7) + 
  geom_line(aes(x=resistance, y=predicted_resistance), size=1) + 
  scale_x_continuous(name = "Resistance", limits=c(0,1), labels = scales::percent)+
  scale_y_continuous(name = "Survival", limits = c(0,1), labels = scales::percent)+
  theme_bw()+
  labs(title=paste0("Quadratic model, r^2 = ", round(summary(larval.lm_quad)$r.squared, digits=3)))

larval_plots <- ggarrange(larval_plot, larval_plot_quad)
larval_plots
ggsave(larval_plots, file="larval_plots.png", unit="in", width = 12, height = 5)


adult_plot <- summary_data %>%
  mutate(predicted_resistance = 0.062226 +  0.939545*resistance -0.115192*resistance2) %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=corrected_adult_survival), color = "#0652DD", alpha = 0.7) + 
  geom_smooth(aes(x=resistance, y=corrected_adult_survival), method = "lm", se = FALSE, color = "black", size=1) + 
  scale_x_continuous(name = "Resistance", limits=c(0,1), labels = scales::percent)+
  scale_y_continuous(name = "Survival", limits = c(0,1), labels = scales::percent)+
  theme_bw()+
  labs(title=paste0("Linear model, r^2 = ", round(summary(adult.lm)$r.squared, digits=3)))

adult_plot_quad <- summary_data %>%
  mutate(predicted_resistance = 0.062226 +  0.939545*resistance -0.115192*resistance2) %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=corrected_adult_survival), color = "#0652DD", alpha = 0.7) + 
  geom_line(aes(x=resistance, y=predicted_resistance), size=1) + 
  scale_x_continuous(name = "Resistance", limits=c(0,1), labels = scales::percent)+
  scale_y_continuous(name = "Survival", limits = c(0,1), labels = scales::percent)+
  theme_bw()+
  labs(title=paste0("Quadratic model, r^2 = ", round(summary(adult.lm_quad)$r.squared, digits=3)))

adult_plots <- ggarrange(adult_plot, adult_plot_quad)
adult_plots
ggsave(adult_plots, file="adult_plots.png", unit="in", width = 12, height = 5)


## 
### tests of difference against adult survival
###

difference.lm = lm(survival_difference ~ corrected_adult_survival, data=summary_data)
difference_r.squared <- summary(difference.lm)$r.squared  

summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)
difference_r.squared_quad <- summary(difference.lm_quad)$r.squared  

difference_scatter <- summary_data %>%
  mutate(predicted_resistance = difference.lm$coefficients["(Intercept)"] 
         + difference.lm$coefficients["corrected_adult_survival"]*corrected_adult_survival) %>%
  ggplot()+
  geom_point(aes(x=corrected_adult_survival, y=-survival_difference), color="darkgrey", alpha = 0.7) + 
  geom_line(aes(x=corrected_adult_survival, y=-predicted_resistance), size=1)+
  scale_x_continuous(limits=c(0,1), name = "Adult capture survival", 
                     breaks = c(0:10/10), labels = scales::percent)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-6)/100, labels = scales::percent)+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.2)) +
  geom_hline(yintercept=0, size=0.5)+
  labs(title=paste0("Linear model, r^2 = ", round(summary(difference.lm)$r.squared, digits=3)))


difference_scatter_quad <- summary_data %>%
  mutate(predicted_resistance = difference.lm_quad$coefficients["(Intercept)"] 
         + difference.lm_quad$coefficients["corrected_adult_survival"]*corrected_adult_survival 
         + difference.lm_quad$coefficients["corrected_adult_survival2"]*corrected_adult_survival2) %>%
  ggplot()+
  geom_point(aes(x=corrected_adult_survival, y=-survival_difference), color="darkgrey", alpha = 0.7) + 
  geom_line(aes(x=corrected_adult_survival, y=-predicted_resistance), size=1)+
  scale_x_continuous(limits=c(0,1), name = "Adult capture survival", 
                     breaks = c(0:10/10), labels = scales::percent)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-6)/100, labels = scales::percent)+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.2)) +
  geom_hline(yintercept=0, size=0.5)+
  labs(title=paste0("Quadratic model, r^2 = ", round(summary(difference.lm_quad)$r.squared, digits=3)))

difference_plots <- ggarrange(difference_scatter, difference_scatter_quad)
difference_plots
ggsave(difference_plots, file="difference_plots.png", unit="in", width = 12, height = 5)




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

pop_data %>% 
  filter(day>500) %>%
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  filter(resistance < 0.22) %>%
  group_by(spatial_coefficient, coverage) %>%
  sample_n(1, replace=FALSE) %>%
  bind_rows(equilibrium_sample) -> test_data

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

write.csv(wide_results_test, "wide_results_test.csv")

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
         difference_predicted = (predicted_resistance-resistance))

wide_results_test %>%
  summarize(MSE_adult = mean(square_difference_adult),
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
