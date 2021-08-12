library(dplyr)
library(tidyr)
library(stringr)
library(viridis)

setwd("/Users/Inga/Documents/GitHub/mosquitoIR")

summary_data <- read.csv("summary_data.csv")
pop_data_baseline <- readRDS("pop_data_baseline")

source("./sampling_functions.R")

wide_results %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  summarize(corrected_adult_survival = mean(corrected_adult_survival)/100,
            corrected_larval_survival = mean(corrected_larval_survival)/100,
            survival_difference = mean(survival_difference)/100) -> summary_data

equilibrium_data <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day==max(day)-1) 

#######################################################
##### paper figures: Fig 2 (heatmaps)
#######################################################

time_cutoffs <- c(365*25)
time_cutoff_labels <- c("25 years")

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
         corrected_adult_survival = corrected_survival) -> temp

full_results_low %>% 
  filter(sample=="larval") %>%
  select(-c("ID", "sample")) %>%
  rowid_to_column("ID") %>%
  rename(control_larval_survival = control_survival,
         test_larval_survival = test_survival,
         corrected_larval_survival = corrected_survival) -> temp2

wide_results_low <- full_join(temp, temp2) %>%
  mutate(survival_difference = corrected_adult_survival - corrected_larval_survival)

wide_results_low %>%
  group_by(spatial_coefficient, coverage, resistance) %>%
  summarize(corrected_adult_survival = mean(corrected_adult_survival)/100,
            corrected_larval_survival = mean(corrected_larval_survival)/100,
            survival_difference = mean(survival_difference)/100) -> summary_data_low

working_data <- non_equilibrium_sample %>%
  group_by(coverage, spatial_coefficient)

working_data %>%
  ggplot() +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=(RR_POP+0.5*SR_POP)/POP)) +
  scale_x_continuous(name="Spatial clustering", breaks = c(0:10)/10) +
  scale_y_continuous(name="Coverage", limits=c(0.025, 0.975), breaks = c(1:9)/10, labels = scales::percent)+
  scale_fill_viridis(name="Resistance", option="D", breaks=seq(0,10)/10, labels = scales::percent, limits=c(0,1.0), oob=squish) +
  coord_flip() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        legend.key.height = unit(1, "cm")) -> plot

working_summary_data <- summary_data_low %>%
  group_by(coverage, spatial_coefficient)

working_summary_data %>%
  ggplot() +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=survival_difference)) +
  scale_x_continuous(name="Spatial clustering", breaks = c(0:10)/10) +
  scale_y_continuous(name="Coverage", limits=c(0.025, 0.975), breaks = c(1:9)/10, labels = scales::percent)+
  scale_fill_distiller(palette="PiYG", limits = c(-15, 15)/100, breaks=c(-15,-10,-5, 0, 5, 10, 15)/100, 
                       name="Survival\nDifference", labels = scales::percent) +
  coord_flip() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        legend.key.height = unit(1, "cm")) -> plot2

assign(paste0("baseline_difference_plot_",i), plot2)

summary_data_low %>%
  ggplot() +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=survival_difference), fill="white") +
  annotate("segment", x = 0, xend = 0, y = 5, yend = 15, colour = "black", size=1, alpha=1, arrow=arrow())+
  annotate("segment", x = 0, xend = 0, y = 15, yend = 5, colour = "black", size=1, alpha=1, arrow=arrow())+
  scale_y_continuous(limits=c(0, 20))+
  scale_x_continuous(limits=c(-1,2))+
  theme_void() +
  annotate("text", x=0, y=3.5, label= "greater \nsurvival in \nlarval-capture", size = 2.5) + 
  annotate("text", x=0, y=16.7, label= "greater \nsurvival in \nadult-capture", size = 2.5) + 
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank()) -> arrow

plot_w_arrow <- ggarrange(plot2, arrow, widths = c(10,2))

eq_fig <- ggarrange(plot, plot_w_arrow, widths = c(4.2,5), ncol=2, labels = "AUTO")
eq_fig
ggsave(eq_fig, file="heatmaps_fig.tiff", unit="in", width = 10.4, height = 3.7)


############################################################
## paper figures: Fig 3A (sample comparison)
############################################################

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
## paper figures: Fig 3B (difference scatter plot)
############################################################

difference.lm = lm(survival_difference ~ corrected_adult_survival, data=combined_summary)
difference_r.squared <- summary(difference.lm)$r.squared  

combined_summary$corrected_adult_survival2 = combined_summary$corrected_adult_survival**2
difference.lm_quad = lm(survival_difference ~ corrected_adult_survival + corrected_adult_survival2, data=combined_summary)
difference_r.squared_quad <- summary(difference.lm_quad)$r.squared  

difference_scatter_quad <- combined_summary %>%
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
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)
  # annotate("text", x=0.1, y=0.14, label= paste0("r^2 = ", round(summary(difference.lm_quad)$r.squared, digits=3))) 
  
scatterplots <- ggarrange(sample_comparison_scatter, difference_scatter_quad, widths = c(1.15,1), labels = "AUTO")
scatterplots

ggsave(scatterplots, file="scatterplots.png", unit="in", width = 9.6, height = 4)


#######################################################
##### paper figures: Fig 4 (sensitivity analyses)
#######################################################

## see file "sensitivity_analyses.R"

#################################################################################
##### SUPPLEMENTAL FIGURES
#################################################################################

#################################################################################
## Fig S2: Mortality
#################################################################################

source("./sampling_functions.R") 

survival.figure <- cbind(as.data.frame(1- sampling.mortality[1,]), as.data.frame(sampling.mortality[3,]))
survival.figure <- cbind(rownames(survival.figure), data.frame(survival.figure, row.names=NULL))
colnames(survival.figure) <- c("compartment", "control_survival", "test_survival")

survival.figure <- survival.figure %>%
  separate(compartment, c("genotype", "exposure", "age"), sep="_") %>%
  mutate(age = as.numeric(gsub(age, pattern = "d", replacement = "")))

colors <- c("R Allele Frequency"="#D8412C", "Adult (Non-standardized) assay"="#A3CB38", "Larval (Age standardized) assay"="#3B528B",
            "Malaria Prevalence"="grey")

control_survival_plot <- survival.figure %>%
  group_by(genotype, exposure) %>%
  filter(exposure != "larvae") %>%
  ggplot()+
  geom_line(aes(x=age, y=control_survival, group=interaction(genotype, exposure), 
                color=genotype, linetype=exposure), position=position_dodge(width=0.3), size = 0.35) +
  scale_y_continuous(name="Survival in controls")+
  scale_x_continuous(name="Age (days)")+
  scale_colour_manual(values = c("RR" = "#F8766D", "SR" = "#00BA38", "SS"= "#3B528B"))+
  theme_bw()

test_survival_plot <- survival.figure %>%
  group_by(genotype, exposure) %>%
  filter(exposure != "larvae") %>%
  ggplot()+
  geom_line(aes(x=age, y=test_survival, group=interaction(genotype, exposure), color=genotype, linetype=exposure), 
            position=position_dodge(width=0.3), size = 0.35) +
  scale_y_continuous(name="Survival in exposed")+
  scale_x_continuous(name="Age (days)")+
  scale_colour_manual(values = c("RR" = "#F8766D", "SR" = "#00BA38", "SS"= "#3B528B"))+
  theme_bw()

mortality_plots <- ggarrange(control_survival_plot, test_survival_plot, ncol=2, nrow=1, common.legend = TRUE, legend = "bottom", labels = "AUTO")
mortality_plots
ggsave("mortality_plot.png", mortality_plots, unit="in", width = 7, height = 3.5)


#################################################################################
## plot true probability of exposure by spatial coefficient, coverage
#################################################################################

equilibrium_data <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day==365) %>% #|day==max(day)-2|day==max(day)-3|day==max(day)-4)
  filter(spatial_coefficient %in% c(seq(0,10,2)/10))
  
equilibrium_data %>% 
  ggplot() +
  geom_line(aes(x=coverage, y=pr_exposed, color=factor(spatial_coefficient)))+
  theme_bw() + 
  ylim(0,1) + 
  xlim(0,1) + 
  labs(color="Spatial \nclustering", x = "Coverage", 
       y = "Proportion feeding \nmosquitoes exposed per day") -> exposure_plot_1

equilibrium_data <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day==365*25)  %>% #|day==max(day)-2|day==max(day)-3|day==max(day)-4)
  filter(spatial_coefficient %in% c(seq(0,10,2)/10))

equilibrium_data %>% 
  ggplot() +
  geom_line(aes(x=coverage, y=pr_exposed, color=factor(spatial_coefficient)))+
  theme_bw() + 
  ylim(0,1) + 
  xlim(0,1) + 
  labs(color="Spatial \nclustering", x = "Coverage", 
       y = "Proportion feeding \nmosquitoes exposed per day") -> exposure_plot_25


exposure_plot <- ggarrange(exposure_plot_1, exposure_plot_25, ncol=2, labels = "AUTO")
exposure_plot
ggsave(exposure_plot, file="exposure_plot.tiff", unit="in", width = 7.8, height = 3)


