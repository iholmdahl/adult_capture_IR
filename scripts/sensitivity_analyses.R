################################################################################
## larval and adult scatter plot - with full data 
################################################################################
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
library(scales)

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

for(analysis in analyses){
  
  summary_data <- c()
  pop_data <- readRDS(paste0("results/population_",analysis,".rds"))

  # if(analysis == "longevity"){
  #   source("./scripts/sampling_functions_longevity.R")
  #   } else{
  #   source("./scripts/sampling_functions.R")
  # }
  # 
  pop_data %>% 
    filter(day>365) %>%
    mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
    filter(resistance < 0.99) %>%
    group_by(spatial_coefficient, coverage) %>%
    sample_n(1, replace=FALSE) -> non_equilibrium_sample
  
  #check length
  nrow(non_equilibrium_sample)
  
  ## then run sampling functions
  assay_results <- NULL
  control_results <- NULL
  
  for(i in 1:nrow(non_equilibrium_sample)){
    
    working_data <- non_equilibrium_sample[i,]
    c <- working_data$coverage
    s <- working_data$spatial_coefficient
    
    assay_results <- rbind(assay_results, resistance.assay(sample.type="adult", c, 50, 100, working_data))
    assay_results <- rbind(assay_results, resistance.assay(sample.type="larval", c, 50, 100, working_data))
    
    control_results <- rbind(control_results, control.assay(sample.type="adult", c, 50, 100, working_data))
    control_results <- rbind(control_results, control.assay(sample.type="larval", c, 50, 100, working_data))
    
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
  
  # write.csv(wide_results, paste0("wide_results_",analysis,".csv"))
  #wide_results <- read.csv("wide_results.csv")
  
  summary_data<-wide_results %>%
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
  
  
  write.csv(summary_data, paste0("output/summary_data_",analysis,".csv"))
  
}


temp1 <- read_csv("output/summary_data_dominance_high.csv")
temp2 <- read_csv("output/summary_data_dominance_low.csv")


compiled_data <- NULL

for(analysis in analyses){
  
  upload_data <- read_csv(paste0("output/summary_data_",analysis,".csv"))

  compiled_data <- upload_data %>%
    mutate(Simulation = analysis) %>%
    bind_rows(compiled_data) 
  
}

write.csv(compiled_data, "output/compiled_data.csv")
#compiled_data <- read.csv("output/compiled_data.csv")

compiled_data %>% 
  filter(Simulation == "main") -> baseline_data

difference_scatter_subset <- compiled_data %>%
  mutate(Simulation = factor(Simulation, levels = c("main", 
                                                       "larvae_0.01", 
                                                       "larvae_0.05", 
                                                       "three", 
                                                       "mortality", 
                                                       "int_mortality", 
                                                       "longevity", 
                                                       "dominance_high", 
                                                       "dominance_low"))) %>%
  filter(Simulation %in% c("main", "larvae_0.05", "three", "mortality", "dominance_low")) %>%
  ggplot()+
  geom_line(aes(x=corrected_adult_survival, 
                y=-predicted_resistance, 
                group=Simulation, 
                color=Simulation), alpha = 0.75)+
  geom_line(data=baseline_data, aes(x=corrected_adult_survival, 
                                    y=-predicted_resistance), 
            color="black", size=0.6)+
  scale_x_continuous(limits=c(0,1), name = "Modeled adult capture bioassay survival", 
                     breaks = c(0:10/5), labels = scales::percent)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-15)/100, 
                     labels = scales::percent) +
  scale_color_manual(name = "Model",
                     values=c("#000000", 
                              # "#B79F00", 
                              "#F8766D", 
                              "#00BA38", 
                              "#00BFC4", 
                              # "#619CFF", 
                              # "#F564E3", 
                              # "#f1c40f", 
                              "#5f27cd"),
                     labels=c("Baseline parameterization",
                              # "Low larvicide exposure",
                              "High larvicide exposure",
                              "3-day gonotrophic cycle",
                              "No mortality cost",
                              # "Intermediate mortality cost",
                              # "No longevity effect",
                              # "75% dominance of resistance", 
                              "25% dominance of resistance"))+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)

ggsave(difference_scatter_subset, 
       file="output/difference_scatter_simulations.tiff", 
       unit="in", width = 6, height = 3.5)

ggsave(difference_scatter_subset, 
       file="output/difference_scatter_simulations.png", 
       unit="in", width = 6, height = 3.5)

################################################################################
# full plot for supplement
################################################################################

difference_scatter_simulations <- compiled_data %>%
  mutate(Simulation = factor(Simulation, levels = c("main", 
                                                    "larvae_0.01", 
                                                    "larvae_0.05", 
                                                    "three", 
                                                    "mortality", 
                                                    "int_mortality", 
                                                    "longevity", 
                                                    "dominance_high", 
                                                    "dominance_low"))) %>%
  ggplot()+
  geom_line(aes(x=corrected_adult_survival, 
                y=-predicted_resistance, 
                group=Simulation, 
                color=Simulation), alpha = 0.75)+
  geom_line(data=baseline_data, aes(x=corrected_adult_survival, 
                                    y=-predicted_resistance), 
            color="black", size=0.6)+
  scale_x_continuous(limits=c(0,1), name = "Modeled adult capture bioassay survival", 
                     breaks = c(0:10/5), labels = scales::percent)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-15)/100, 
                     labels = scales::percent) +
  scale_color_manual(name = "Model",
                     values=c("#000000", 
                              "#B79F00", 
                              "#F8766D", 
                              "#00BA38", 
                              "#00BFC4", 
                              "#619CFF", 
                              "#F564E3", 
                              "#f1c40f", 
                              "#5f27cd"),
                     labels=c("Baseline parameterization",
                              "Low larvicide exposure",
                              "High larvicide exposure",
                              "3-day gonotrophic cycle",
                              "No mortality cost",
                              "Intermediate mortality cost",
                              "No longevity effect",
                              "75% dominance of resistance", 
                              "25% dominance of resistance"))+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)

ggsave(difference_scatter_simulations, 
       file="output/difference_scatter_simulations_supplement.tiff", 
       unit="in", width = 6, height = 3.5)

ggsave(difference_scatter_simulations, 
       file="output/difference_scatter_simulations_supplement.png", 
       unit="in", width = 6, height = 3.5)
