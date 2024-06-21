library(dplyr)
library(tidyr)
library(stringr)
library(viridis)

summary_data <- readr::read_csv("output/summary_data.csv")
pop_data_baseline <- readRDS("results/population_main.rds")
working_data <- readr::read_csv("output/fig2_data.csv")

source("./sampling_functions.R")

################################################################################
##### Fig 2 (heatmaps)
################################################################################

working_data %>%
  ggplot() +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=(RR_POP+0.5*SR_POP)/POP)) +
  scale_x_continuous(name="Spatial clustering", breaks = c(0:10)/10) +
  scale_y_continuous(name="Coverage", limits=c(0.025, 0.975), breaks = c(1:9)/10, 
                     labels = scales::percent)+
  scale_fill_viridis_c(name="Resistance\nallele\nfrequency", option="D", breaks=seq(0,10)/10, 
                     labels = scales::percent, limits=c(0,1.0), oob=scales::squish) +
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

plot_w_arrow <- ggpubr::ggarrange(plot2, arrow, widths = c(10,2))

eq_fig <- ggpubr::ggarrange(plot, plot_w_arrow, widths = c(4.2,5), ncol=2, labels = "AUTO")
eq_fig
ggsave(eq_fig, file=paste0("output/heatmaps_fig_",time_cutoffs/365,"_years.tiff"), unit="in", width = 10.4, height = 3.7)
ggsave(eq_fig, file=paste0("output/heatmaps_fig_",time_cutoffs/365,"_years.png"), unit="in", width = 10.4, height = 3.7)

################################################################################
## paper figures: Alt version of figure 2 is just the right panel, add contour line separately
################################################################################

plot_w_arrow <- ggpubr::ggarrange(plot2_viridis, arrow, widths = c(10,2))

ggsave(plot_w_arrow, file=paste0("output/alt_figure_2_viridis.tiff"), unit="in", width = 6, height = 4, bg = "white")
ggsave(plot_w_arrow, file=paste0("output/alt_figure_2.pdf"), unit="in", width = 6, height = 4, bg = "white")
ggsave(plot_w_arrow, file=paste0("output/alt_figure_2.png"), unit="in", width = 6, height = 3.7, bg = "white")


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
             size = 0.9, alpha = 0.3) +
  geom_point(aes(x=resistance, y=larval_resistance, color = "Larval"), 
             size = 0.9, alpha = 0.3) + 
  geom_smooth(aes(x=resistance, y=adult_resistance), method = "lm",
              se = FALSE, color = "black", size=1) +  #se = FALSE, 
  geom_smooth(aes(x=resistance, y=larval_resistance), method = "lm",
              se = FALSE, color = "black", size=1) + 
  scale_x_continuous(limits=c(0,1), labels = scales::percent, breaks = seq(0, 1, by = .2))+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  scale_colour_manual(name="Sample\ntype",values=color_key) + 
  # 
  # scale_colour_manual(name = 'Sample\ntype', 
  #                     values =c('#D41159'='#D41159',
  #                               '#1A85FF'='#1A85FF'), 
  #                     labels =c("Adult", 
  #                               "Larval")) +
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
difference.lm_quad = lm(mean_survival_difference ~ 
                          mean_adult_resistance + mean_adult_resistance2, 
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
             color="#73D055FF", alpha = 0.3) + 
  geom_line(aes(x=mean_adult_resistance, y=-predicted_resistance), linewidth=1)+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-6)/100, 
                     labels = scales::percent)+
  scale_x_continuous(name = "Modeled Adult-capture bioassay survival",
                     limits=c(0,1), labels = scales::percent, breaks = seq(0, 1, by = .2))+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)

difference_scatter_quad

scatterplots <- ggpubr::ggarrange(comparison_scatter, 
                                  difference_scatter_quad, 
                                  widths = c(1.15,1), labels = "AUTO")
scatterplots

ggsave(scatterplots, file="output/scatterplot_fig3.png", unit="cm", width = 20, height = 8)
ggsave(scatterplots, file="output/scatterplot_fig3.tiff", unit="cm", width = 20, height = 4)

################################################################################
## paper figures: Alt Fig 3B (difference scatter plot)
################################################################################

difference_data <- summary_data
difference.lm = lm(mean_survival_difference ~ mean_adult_resistance, 
                   data=difference_data)
# difference_r.squared <- summary(difference.lm)$r.squared  

difference_data$mean_adult_resistance2 = difference_data$mean_adult_resistance**2
difference.lm_quad = lm(mean_survival_difference ~ 
                          mean_adult_resistance + mean_adult_resistance2, 
                        data=difference_data)
# difference_r.squared_quad <- summary(difference.lm_quad)$r.squared  

## make fake dataframe so i can apply this to range from 0 to 20%
curve_df <- data_frame(resistance = seq(0, 1, by = 0.01)) %>%
  mutate(resistance_adjustment = -(difference.lm_quad$coefficients["(Intercept)"] 
         + difference.lm_quad$coefficients["mean_adult_resistance"]*resistance 
         + difference.lm_quad$coefficients["mean_adult_resistance2"]*(resistance**2)))

difference_scatter <- difference_data %>%
  ungroup() %>%
  slice_sample(n = round(nrow(summary_data)/10), replace = FALSE) %>%
  mutate(predicted_resistance = difference.lm_quad$coefficients["(Intercept)"] 
         + difference.lm_quad$coefficients["mean_adult_resistance"]*mean_adult_resistance 
         + difference.lm_quad$coefficients["mean_adult_resistance2"]*mean_adult_resistance2) %>%
  ggplot()+
  geom_point(aes(x=mean_adult_resistance, y=-mean_survival_difference), 
             color="#73D055FF", alpha = 0.3) + 
  geom_line(data = curve_df,
            aes(x=resistance, y=resistance_adjustment), linewidth=1)+
  scale_x_continuous(name = "Modeled adult-capture bioassay survival",
                     limits=c(0,1), labels = scales::percent, breaks = c(0:10/10))+
  scale_y_continuous(name = "Assay adjustment", minor_breaks = c(15:-6)/100, 
                     labels = scales::percent)+
  theme_bw() + 
  theme(panel.grid.major.x = element_line(colour = "black", size = 0.1)) +
  geom_hline(yintercept=0, size=0.5)

scatterplots_alt <- ggpubr::ggarrange(comparison_scatter, 
                                  difference_scatter, 
                                  widths = c(1.15,1), labels = "AUTO")
scatterplots_alt

ggsave(scatterplots_alt, file="output/scatterplot_fig3_alt.png", unit="cm", width = 20, height = 8)
ggsave(scatterplots_alt, file="output/scatterplot_fig3_alt.tiff", unit="cm", width = 20, height = 8)


################################################################################
##### paper figures: Fig 4 (sensitivity analyses)
################################################################################

## see file "sensitivity_analyses.R"


#################################################################################
##### SUPPLEMENTARY FIGURES
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

mortality_plots <- ggpubr::ggarrange(control_survival_plot, 
                                     test_survival_plot, 
                                     ncol=2, nrow=1, 
                                     common.legend = TRUE, 
                                     legend = "bottom", labels = "AUTO")
mortality_plots
ggsave("mortality_plot.png", mortality_plots, unit="in", width = 7, height = 3.5)


#################################################################################
## Figure S3: true probability of exposure by spatial coefficient, coverage
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


exposure_plot <- ggpubr::ggarrange(exposure_plot_1, 
                                   exposure_plot_25, 
                                   ncol=2, labels = "AUTO")
exposure_plot
ggsave(exposure_plot, file="exposure_plot.tiff", unit="in", width = 7.8, height = 3)


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

time_to_resistance %>%
  ggplot() +
  geom_tile(data = background, 
            aes(x=spatial_coefficient, y=coverage), fill="grey") +
  geom_tile(aes(x=spatial_coefficient, y=coverage, fill=sqrt(day/365))) +
  scale_x_continuous(name="Spatial clustering", breaks = c(0:10)/10) +
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

ggsave(file=paste0("output/time_to_resistance.tiff"), unit="in", width = 5, height = 3.7)
ggsave(file=paste0("output/time_to_resistance.png"), unit="in", width = 5, height = 3.7)

################################################################################
##### Figure S6: heatmap of time to 50% resistance
################################################################################

small_sample_scatter <- wide_results %>%
  group_by(n, coverage, spatial_coefficient) %>%
  slice_sample(n = round(nrow(wide_results)/200000)) %>%
  mutate(n = paste0("Number sampled in each group = ", n)) %>%
  group_by(n) %>%
  ggplot()+
  geom_abline(intercept=0, slope=1, color="grey")+
  geom_point(aes(x=resistance, y=adult_resistance, color = "Adult"),
             size = 0.9, alpha = 0.3) +
  geom_point(aes(x=resistance, y=larval_resistance, color = "Larval"), 
             size = 0.9, alpha = 0.3) + 
  geom_smooth(aes(x=resistance, y=adult_resistance), method = "lm",
              se = FALSE, color = "black", size=1) +  #se = FALSE, 
  geom_smooth(aes(x=resistance, y=larval_resistance), method = "lm",
              se = FALSE, color = "black", size=1) + 
  scale_x_continuous(limits=c(0,1), labels = scales::percent, breaks = c(0:10/10))+
  scale_y_continuous(limits = c(0,1), labels = scales::percent)+
  scale_colour_manual(name="Sample\ntype",values=color_key) + 
  labs(x = "Resistance allele frequency", y = "Modeled bioassay survival") +
  theme_bw() +
  facet_wrap(~n)

small_sample_scatter

ggsave(small_sample_scatter, file=paste0("output/scatter_two_samplesizes.png"), 
       unit="in", width = 8.6, height = 4)
ggsave(small_sample_scatter, file=paste0("output/scatter_two_samplesizes.tiff"), 
       unit="in", width = 8.6, height = 4)
