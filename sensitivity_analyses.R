########################################################################################################################
## larval and adult scatter plot - with full data 
########################################################################################################################
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(viridis)
library(scales)

setwd("/Users/Inga/Documents/GitHub/mosquitoIR")

## load data 
analyses <- c("baseline", "mortality", "three", "longevity", "int_mortality", "larvae_0.01", "larvae_0.05")

for(analysis in analyses){
  
  if(analysis == "longevity"){
    source("./sampling_functions_longevity.R")
  }else{
    source("./sampling_functions.R")
  }
  
  pop_data <- readRDS(paste0("pop_data_",analysis))
  
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
  
  write.csv(wide_results, paste0("wide_results_",analysis,".csv"))
  
  #wide_results <- read.csv("wide_results.csv")
  
  wide_results %>%
    group_by(spatial_coefficient, coverage, resistance) %>%
    summarize(corrected_adult_survival = mean(corrected_adult_survival)/100,
              corrected_larval_survival = mean(corrected_larval_survival)/100,
              survival_difference = mean(survival_difference)/100) -> summary_data
  
  summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
  difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)
  
  summary_data %>%
    mutate(predicted_resistance = difference.lm_quad$coefficients["(Intercept)"] 
           + difference.lm_quad$coefficients["corrected_adult_survival"]*corrected_adult_survival 
           + difference.lm_quad$coefficients["corrected_adult_survival2"]*corrected_adult_survival2) -> summary_data
  
  
  write.csv(summary_data, paste0("summary_data_",analysis,".csv"))
  
  ## compare difference plots
  
  sample_comparison_scatter <- summary_data %>%
    ggplot()+
    geom_abline(intercept=0, slope=1, color="grey")+
    geom_vline(aes(xintercept=0.22), color="black", size=0.2) +
    geom_point(aes(x=resistance, y=corrected_larval_survival, color = "#009432"), alpha = 0.7) + 
    geom_smooth(aes(x=resistance, y=corrected_larval_survival), method = "lm",
                se = FALSE, color = "black", size=0.5) + 
    geom_point(aes(x=resistance, y=corrected_adult_survival, color = "#0652DD"), alpha = 0.7) + 
    geom_smooth(aes(x=resistance, y=corrected_adult_survival), method = "lm",
                se = FALSE, color = "black", size=0.5) + 
    scale_x_continuous(name = "Resistance", limits=c(0,1), labels = scales::percent, breaks = c(0:10/10))+
    scale_y_continuous(name = "Survival", limits = c(0,1), labels = scales::percent)+
    # annotate("text", x=.2, y=0.95, label= paste0("larval r^2 = ", round(larval_r.squared, digits=3))) + 
    # annotate("text", x=.2, y=1, label= paste0("adult r^2 = ", round(adult_r.squared, digits=3))) + 
    scale_colour_manual(name = 'Sample\nType', 
                        values =c('#009432'='#009432','#0652DD'='#0652DD'), labels = c('larval','adult'))+
    theme_bw() 
  
  sample_comparison_scatter
  
  ############################################################
  ## paper figures: difference scatter plot
  ############################################################
  
  difference.lm = lm(survival_difference ~ corrected_adult_survival, data=summary_data)
  difference_r.squared <- summary(difference.lm)$r.squared  
  
  summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
  difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)
  difference_r.squared_quad <- summary(difference.lm_quad)$r.squared  
  
  
  summary_data <- summary_data %>%
    mutate(predicted_resistance = difference.lm_quad$coefficients["(Intercept)"] 
           + difference.lm_quad$coefficients["corrected_adult_survival"]*corrected_adult_survival 
           + difference.lm_quad$coefficients["corrected_adult_survival2"]*corrected_adult_survival2, 
           predicted_resistance_linear = difference.lm$coefficients["(Intercept)"] 
           + difference.lm$coefficients["corrected_adult_survival"]*corrected_adult_survival)
  
  
  difference_scatter_quad <- summary_data %>%
    ggplot()+
    geom_point(aes(x=corrected_adult_survival, y=-survival_difference), color="darkgrey", alpha = 0.7) + 
    geom_line(aes(x=corrected_adult_survival, y=-predicted_resistance), size=1)+
    # geom_line(aes(x=corrected_adult_survival, y=-predicted_resistance_linear), size=1, color="red")+
    scale_x_continuous(limits=c(0,1), name = "Adult capture survival", 
                       breaks = c(0:10/10), labels = scales::percent)+
    scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-15)/100, labels = scales::percent)+
    theme_bw() + 
    theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
    geom_hline(yintercept=0, size=0.5)
  # annotate("text", x=0.1, y=0.14, label= paste0("r^2 = ", round(summary(difference.lm_quad)$r.squared, digits=3))) 
  
  difference_scatter_quad
  # scatterplots <- ggarrange(sample_comparison_scatter, difference_scatter_quad, widths = c(1.15,1), labels = "AUTO")
  # scatterplots
  # 
  # ggsave(scatterplots, file="scatterplots_three.png", unit="in", width = 9.6, height = 4)
  
}

compiled_data <- NULL

for(analysis in analyses){
  
  upload_data <- read.csv(paste0("summary_data_",analysis,".csv"))

  summary_data %>%
  mutate(Simulation = analysis) %>%
  rbind(compiled_data) -> compiled_data
}

#write.csv(compiled_data, "compileddata.csv")
#compiled_data <- read.csv("compileddata.csv")

compiled_data %>% 
  filter(Simulation == "baseline") -> baseline_data

difference_scatter_simulations <- compiled_data %>%
  mutate(Simulation = factor(Simulation, levels =c("baseline", "larvae_0.01", "larvae_0.05", "three", "mortality", 
                                                   "int_mortality", "longevity"))) %>%
  ggplot()+
  geom_line(aes(x=corrected_adult_survival, y=-predicted_resistance, group=Simulation, color=Simulation))+
  geom_line(data=baseline_data, aes(x=corrected_adult_survival, y=-predicted_resistance), color="black", size=0.6)+
  scale_x_continuous(limits=c(0,1), name = "Adult capture survival", 
                     breaks = c(0:10/10), labels = scales::percent)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-15)/100, labels = scales::percent)+
  scale_color_manual(name = "Model", values=c("#000000", "#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3"), 
                     labels=c("Original model", "Low larvicide exposure", "High larvicide exposure", "3-day gonotrophic cycle",
                              "No mortality cost","Intermediate mortality cost", 
                              "No longevity effect"))+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)

difference_scatter_simulations

ggsave(difference_scatter_simulations, file="difference_scatter_simulations.tiff", unit="in", width = 6, height = 3.5)

