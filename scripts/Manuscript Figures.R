library(dplyr)
library(tidyr)
library(stringr)
library(viridis)

## raw population output
pop_data_baseline <- readRDS("results/population_main.rds")

## resistance sampling results
wide_results <- readr::read_csv("output/wide_results.csv")
summary_data <- readr::read_csv("output/summary_data.csv")
equilibrium_summary_data <- readr::read_csv("output/summary_data_equilibrium.csv")

## resistance sampling results: sensitivity analyses
sensitivity_curves <- readr::read_csv("output/sensitivity_analysis_data.csv")

source("scripts/sampling_functions.R")

################################################################################
##### Fig 2 (heatmap of measurement error at equilibrium)
################################################################################

equilibrium_summary_data %>%
  ggplot() +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=mean_survival_difference)) +
  scale_x_continuous(name="Spatial clustering", breaks = c(0:10)/10) +
  scale_y_continuous(name="Coverage", limits=c(0.025, 0.975), breaks = c(1:9)/10, labels = scales::percent)+
  # scale_fill_distiller(palette="RdBu", limits = c(-15, 15)/100, breaks=c(-15,-10,-5, 0, 5)/100, 
  #                      name="Survival\nDifference", labels = scales::percent) +
  scale_fill_viridis(labels = scales::percent, option = "mako",
                     begin = 0, end = 0.9, 
                     name = "Survival\nDifference", 
                     limits = c(NA, 5/100))+
  coord_flip() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        legend.key.height = unit(1, "cm")) -> plot2

equilibrium_summary_data %>%
  ggplot() +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=survival_difference), fill="white") +
  annotate("segment", x = 0, xend = 0, y = 5, yend = 15, colour = "black", size=1, alpha=1, arrow=arrow())+
  annotate("segment", x = 0, xend = 0, y = 15, yend = 5, colour = "black", size=1, alpha=1, arrow=arrow())+
  scale_y_continuous(limits=c(0, 20))+
  scale_x_continuous(limits=c(-1,2))+
  theme_void() +
  annotate("text", x=0, y=3.5, label= "greater \nsurvival in \nlarval-capture", size = 3) + 
  annotate("text", x=0, y=16.7, label= "greater \nsurvival in \nadult-capture", size = 3) + 
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank()) -> arrow

plot_w_arrow <- ggpubr::ggarrange(plot2, arrow, widths = c(10,2))

plot_w_arrow

ggsave(plot_w_arrow, file=paste0("output/figures/figure_2.tiff"), unit="in", width = 7, height = 4.5, bg = "white")
ggsave(plot_w_arrow, file=paste0("output/figures/figure_2.png"), unit="in", width = 7, height = 4.5, bg = "white")

################################################################################
## paper figures: Fig 3A (sample comparison)
################################################################################

color_key <- c("Adult" = "#D41159", "Larval" = "#1A85FF")

comparison_scatter <- wide_results %>%
  filter(n == 100) %>%
  group_by(coverage, spatial_coefficient) %>%
  slice_sample(n = round(nrow(wide_results)/200000)) %>%
  mutate(n = paste0("Number sampled in each group = ", n)) %>%
  group_by(n) %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=adult_resistance, color = "Adult"),
             size = 0.65, alpha = 0.3) +
  geom_point(aes(x=resistance, y=larval_resistance, color = "Larval"), 
             size = 0.65, alpha = 0.3) + 
  geom_smooth(aes(x=resistance, y=adult_resistance), method = "lm",
              se = FALSE, color = "black", size=0.5) +  #se = FALSE, 
  geom_smooth(aes(x=resistance, y=larval_resistance), method = "lm",
              se = FALSE, color = "black", size=0.5) + 
  scale_x_continuous(limits=c(0,1), labels = scales::percent, breaks = seq(0, 1, by = .2))+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  scale_colour_manual(name="Sample\ntype",values=color_key) + 
  labs(x = "Resistance allele frequency", y = "Modeled bioassay survival") +
  theme_bw()

comparison_scatter

################################################################################
## paper figures: Fig 3B (difference scatter plot)
################################################################################
difference_data <- summary_data
difference.lm = lm(mean_survival_difference ~ mean_adult_resistance, 
                   data=difference_data)
# difference_r.squared <- summary(difference.lm)$r.squared  

difference_data$mean_adult_resistance2 = difference_data$mean_adult_resistance**2
difference.lm_quad = lm(mean_survival_difference ~ mean_adult_resistance + mean_adult_resistance2, 
                        data=difference_data)
# difference_r.squared_quad <- summary(difference.lm_quad)$r.squared  

difference_scatter_quad <- difference_data %>%
  ungroup() %>%
  slice_sample(n = round(nrow(summary_data)/10), replace = FALSE) %>%
  mutate(predicted_resistance = difference.lm_quad$coefficients["(Intercept)"] 
         + difference.lm_quad$coefficients["mean_adult_resistance"]*mean_adult_resistance 
         + difference.lm_quad$coefficients["mean_adult_resistance2"]*mean_adult_resistance2) %>%
  ggplot()+
  geom_point(aes(x=mean_adult_resistance, y=-mean_survival_difference), 
             color="#73D055FF", size = 0.65, alpha = 0.3) + 
  geom_line(aes(x=mean_adult_resistance, y=-predicted_resistance), linewidth=0.5)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-6)/100, 
                     labels = scales::percent)+
  scale_x_continuous(name = "Modeled Adult-capture bioassay survival",
                     limits=c(0,1), labels = scales::percent, breaks = seq(0, 1, by = .2))+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.3)

difference_scatter_quad

scatterplots <- ggpubr::ggarrange(comparison_scatter, 
                                  difference_scatter_quad, 
                                  widths = c(1.25,1), labels = "AUTO")
scatterplots

ggsave(scatterplots, file="output/figures/scatterplot_fig3.png", unit="cm", width = 20, height = 8)
ggsave(scatterplots, file="output/figures/scatterplot_fig3.tiff", unit="cm", width = 20, height = 8)

################################################################################
##### paper figures: Fig 4
################################################################################

plot_curves <- sensitivity_curves %>%
  group_by(observed_resistance) %>%
  summarise(max_adult = max(max(resistance_adult), 0),
            mean_adult = max(mean(resistance_adult), 0),
            min_adult = max(min(resistance_adult), 0),
            max_larval = max(max(resistance_larval), 0),
            mean_larval = max(mean(resistance_larval), 0),
            min_larval = max(min(resistance_larval), 0))

wide_results$adult_resistance2 = wide_results$adult_resistance**2
adult_fit = lm(resistance ~ adult_resistance + adult_resistance2, 
               data = wide_results)

wide_results$larval_resistance2 = wide_results$larval_resistance**2
larval_fit = lm(resistance ~ larval_resistance + larval_resistance2, 
                data = wide_results)

curve_df <- data_frame(adult_resistance = seq(0, 1, by = 0.01)) %>%
  mutate(resistance_adult = (adult_fit$coefficients["(Intercept)"] 
                             + adult_fit$coefficients["adult_resistance"]*adult_resistance 
                             + adult_fit$coefficients["adult_resistance2"]*(adult_resistance**2)),
         resistance_larval = (larval_fit$coefficients["(Intercept)"] 
                              + larval_fit$coefficients["larval_resistance"]*adult_resistance
                              + larval_fit$coefficients["larval_resistance2"]*adult_resistance**2))

### try again - make categorical 
points_figure <- curve_df %>%
  mutate(adult_resistance_censored = case_when(resistance_adult <= 0 ~ 0, 
                                               TRUE ~ resistance_adult), 
         larval_resistance_censored = case_when(resistance_larval <= 0 ~ 0, 
                                                TRUE ~ resistance_larval)) %>%
  ggplot()+
  # geom_abline(slope = 1, intercept = 0, color = "grey") + 
  geom_ribbon(data = plot_curves,
              aes(x = observed_resistance, ymin = min_adult, ymax = max_adult),
              fill = "blue", alpha = 0.2) +
  # geom_ribbon(data = plot_curves,
  #             aes(x = observed_resistance, ymin = min_larval, ymax = max_larval),
  #             fill = "orange", alpha = 0.2) +
  geom_point(aes(x = adult_resistance, y = adult_resistance_censored), color = "darkblue") +
  # geom_point(aes(x = adult_resistance, y = larval_resistance_censored, color = "Larval")) +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = .02)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = .02)) +
  # scale_color_manual(name="Sample type", values = color_key) + 
  coord_cartesian(xlim = c(0, 0.2), ylim = c(0, 0.2)) + 
  theme_bw() + 
  labs(x = "Adult resistance assay outcome",
       y = "Inference of resistance allele frequency in population")

points_figure

ggsave(points_figure, file="output/figures/figure_4.png", unit="in", width = 7, height = 4.5)
ggsave(points_figure, file="output/figures/figure_4.tiff", unit="in", width = 7, height = 4.5)

points_figure +
  labs(title = "Interpretation of true resistance from adjusted assay survival",
       caption = "Points show modeled mean resistance allele frequency for each adjusted survival % under the standard model parameterization.
                  Range ribbon shows the variation in the mean resistance allele frequency for each observed survival across all sensitivity analyses.")


#################################################################################
##### SUPPLEMENTARY FIGURES
#################################################################################

#################################################################################
## Fig S2: Mortality
#################################################################################

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

mortality_plots <- ggpubr::ggarrange(control_survival_plot, 
                                     test_survival_plot, 
                                     ncol=2, nrow=1, 
                                     common.legend = TRUE, 
                                     legend = "bottom", labels = "AUTO")
mortality_plots
ggsave("output/figures/supplement_mortality_plot.png", mortality_plots, unit="in", width = 7, height = 3.5)
ggsave("output/figures/supplement_mortality_plot.tiff", mortality_plots, unit="in", width = 7, height = 3.5)

#################################################################################
## Figure S3: true probability of exposure by spatial coefficient, coverage
#################################################################################

equilibrium_data <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day==365) %>% #|day==max(day)-2|day==max(day)-3|day==max(day)-4)
  filter(spatial_coefficient %in% c(seq(0,10,2)/10)) |> 
  mutate(time = "A) 1 year after resistance emergence")
  
equilibrium_data <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day==365*25)  %>% #|day==max(day)-2|day==max(day)-3|day==max(day)-4)
  filter(spatial_coefficient %in% c(seq(0,10,2)/10)) |>
  mutate(time = "B) At resistance equilibrium") |>
  bind_rows(equilibrium_data)

equilibrium_data %>% 
  ggplot() +
  geom_line(aes(x=coverage, y=pr_exposed, color=factor(spatial_coefficient)))+
  theme_bw() + 
  ylim(0,1) + 
  xlim(0,1) + 
  labs(color="Heterogeneity \nparameter", x = "Coverage", 
       y = "Proportion feeding \nmosquitoes exposed per day") +
  facet_wrap(~time) -> exposure_plot

exposure_plot
ggsave(exposure_plot, file="output/figures/supplement_exposure_plot.png", unit="in", width = 7.8, height = 3.5)
ggsave(exposure_plot, file="output/figures/supplement_exposure_plot.tiff", unit="in", width = 7.8, height = 3.5)

################################################################################
##### Figure S5: heatmap of time to 50% resistance
################################################################################

time_to_resistance <- pop_data_baseline %>% 
  mutate(resistance = (0.5*SR_POP + RR_POP)/POP) %>%
  filter(resistance >= 0.5) %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day == min(day))

## make grey background

background <- pop_data_baseline %>%
  group_by(coverage, spatial_coefficient) %>%
  filter(day == min(day))

time_to_resistance_plot <- time_to_resistance %>%
  ggplot() +
  geom_tile(data = background, 
            aes(x=spatial_coefficient, y=coverage), fill="grey") +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=sqrt(day/365))) +
  scale_x_continuous(name="Exposure Heterogeneity", breaks = c(0:10)/10) +
  scale_y_continuous(name="Coverage", limits=c(0.025, 0.975), breaks = c(1:9)/10, 
                     labels = scales::percent)+
  scale_fill_viridis_c(name="Years to 50%\nresistance", option="D", 
                       limits = c(0, NA), 
                       breaks = c(0, 1, 2, 3, 4), 
                       labels = c(0, 1, 4, 9, 16)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        legend.key.height = unit(1, "cm"))

ggsave(time_to_resistance_plot, file=paste0("output/figures/supplement_time_to_resistance.tiff"),
       unit="in", width = 5, height = 3.7)
ggsave(time_to_resistance_plot, file=paste0("output/figures/supplement_time_to_resistance.png"),
       unit="in", width = 5, height = 3.7)

################################################################################
##### Figure S6: sample size comparison
################################################################################
color_key <- c("Adult" = "#D41159", "Larval" = "#1A85FF")

small_sample_scatter <- wide_results %>%
  group_by(n, coverage, spatial_coefficient) %>%
  slice_sample(n = round(nrow(wide_results)/200000)) %>%
  mutate(n = paste0("Number sampled in each group = ", n)) %>%
  group_by(n) %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=adult_resistance, color = "Adult"),
             size = 0.65, alpha = 0.3) +
  geom_point(aes(x=resistance, y=larval_resistance, color = "Larval"), 
             size = 0.65, alpha = 0.3) + 
  geom_smooth(aes(x=resistance, y=adult_resistance), method = "lm",
              se = FALSE, color = "black", size=0.5) +  #se = FALSE, 
  geom_smooth(aes(x=resistance, y=larval_resistance), method = "lm",
              se = FALSE, color = "black", size=0.5) + 
  scale_x_continuous(limits=c(0,1), labels = scales::percent, breaks = seq(0, 1, by = .2))+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  scale_colour_manual(name="Sample\ntype",values=color_key) + 
  labs(x = "Resistance allele frequency", y = "Modeled bioassay survival") +
  theme_bw() +
  facet_wrap(~n)

small_sample_scatter

ggsave(small_sample_scatter, file=paste0("output/figures/supplement_scatter_two_samplesizes.png"), 
       unit="cm", width = 20, height = 9)
ggsave(small_sample_scatter, file=paste0("output/figures/supplement_scatter_two_samplesizes.tiff"), 
       unit="cm", width = 20, height = 9)

#################################################################################
## Figure S7: full sensitivity analysis results
#################################################################################

difference_scatter_simulations <- sensitivity_curves %>%
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
  geom_line(aes(x=observed_resistance, 
                y=resistance_adult-resistance_larval, 
                group=Simulation, 
                color=Simulation), alpha = 0.75)+
  geom_line(data=filter(sensitivity_curves, Simulation == "main"),
            aes(x=observed_resistance,
                y=resistance_adult-resistance_larval),
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

difference_scatter_simulations

ggsave(difference_scatter_simulations, 
       file="output/figures/supplement_difference_scatter_simulations.tiff", 
       unit="in", width = 6, height = 3.5)

ggsave(difference_scatter_simulations, 
       file="output/figures/supplement_difference_scatter_simulations.png", 
       unit="in", width = 6, height = 3.5)
