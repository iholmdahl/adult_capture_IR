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

summary_data_plot <- bind_rows(summary_data, summary_data)

### look at results
sample_comparison_scatter <- summary_data %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=corrected_larval_survival, color = "#009432"), alpha = 0.5, size=0.7) + 
  geom_smooth(aes(x=resistance, y=corrected_larval_survival), method = "lm",
               se = FALSE, color = "black", size=0.5) + 
  geom_point(aes(x=resistance, y=corrected_adult_survival, color = "#0652DD"), alpha = 0.5, size=0.7) + 
  geom_smooth(aes(x=resistance, y=corrected_adult_survival), method = "lm",
              se = FALSE, color = "black", size=0.5) + 
  scale_x_continuous(name = "Resistance", limits=c(0,1), labels = scales::percent, breaks = c(0:10/10))+
  scale_y_continuous(name = "Survival", limits = c(0,1), labels = scales::percent, breaks = c(0:10/10))+
  # annotate("text", x=.2, y=0.95, label= paste0("larval r^2 = ", round(larval_r.squared, digits=3))) + 
  # annotate("text", x=.2, y=1, label= paste0("adult r^2 = ", round(adult_r.squared, digits=3))) + 
  scale_colour_manual(name = 'Sample\nType', 
                      values =c('#009432'='#009432','#0652DD'='#0652DD'), labels = c('larval','adult'))+
  theme_bw() 

sample_comparison_scatter



difference.lm_baseline = lm(survival_difference ~ corrected_adult_survival, data=summary_data)
difference_r.squared_baseline <- summary(difference.lm_baseline)$r.squared  

summary_data$corrected_adult_survival2 = summary_data$corrected_adult_survival**2
difference.lm_quad_baseline = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=summary_data)
difference_r.squared_quad_baseline <- summary(difference.lm_quad_baseline)$r.squared  

difference_scatter_quad <- summary_data %>%
  mutate(predicted_resistance = difference.lm_quad_baseline$coefficients["(Intercept)"] 
         + difference.lm_quad_baseline$coefficients["corrected_adult_survival"]*corrected_adult_survival 
         + difference.lm_quad_baseline$coefficients["corrected_adult_survival2"]*corrected_adult_survival2) %>%
  ggplot()+
  geom_point(aes(x=corrected_adult_survival, y=-survival_difference), color="darkgrey", alpha = 0.7, size = 0.7) + 
  geom_line(aes(x=corrected_adult_survival, y=-predicted_resistance), size=1)+
  scale_x_continuous(limits=c(0,1), name = "Adult capture survival", 
                     breaks = c(0:10/10), labels = scales::percent)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-6)/100, labels = scales::percent)+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)
# annotate("text", x=0.1, y=0.14, label= paste0("r^2 = ", round(summary(difference.lm_quad)$r.squared, digits=3))) 

scatterplots <- ggarrange(sample_comparison_scatter, difference_scatter_quad, widths = c(1.15,1), labels = "AUTO")
scatterplots

ggsave(scatterplots, file="scatterplots.png", unit="in", width = 9.6, height = 4)


