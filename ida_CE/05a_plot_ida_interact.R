############################
####### FOREST PLOTS #######
############################

#### Forest plot for interaction ####

datIDA <- data.frame( value = c(posterior_samples(XP03_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP10_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(IDA_resp, "b")$"b_usvalence:RWAscore"
),
type = c(rep("Experiment 3",4000),rep("Experiment 11",4000),
         rep("Integrative analysis",4000)
)
)


# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

hdi03 <- HDInterval::hdi(posterior_samples(XP03_resp, "b")$"b_usvalence:RWAscore",
                credMass = 0.95)
hdi03m <- mean(posterior_samples(XP03_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi10 <- HDInterval::hdi(posterior_samples(XP10_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi10m <- mean(posterior_samples(XP10_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdIDA <- HDInterval::hdi(posterior_samples(IDA_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdIDAm <- mean(posterior_samples(IDA_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)


nbppt03 <- length(unique(XP03$ppt))
nbppt10 <- length(unique(XP10$ppt))
nbpptIDA <- length(unique(IDA$ppt))


plot_IDA <- datIDA %>% 
  mutate(type = factor(type, levels = rev(unique(datIDA$type)))) %>% 
  ggplot(aes(x=value, y=type, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      scale = 0.9, rel_min_height = 0.01, color = "#A0A0A0A0") +
  scale_fill_manual(name = "Probability", values = c("#A0A0A0A0", "#A0A0A0A0", "#A0A0A0A0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  annotate("rect", xmin = -0.13, xmax = 0.13, ymin = 0.9, ymax = 3.8, fill = "blue", alpha = 0.2) +
  #annotate("rect", xmin = -0.05, xmax = 0.55, ymin = 1.945, ymax = 1.955, fill = "gray70", alpha = 0.7) +
  annotate("errorbarh", y = 3, xmin = hdi03[1], xmax = hdi03[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 3, x = hdi03m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 2, xmin = hdi10[1], xmax = hdi10[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 2, x = hdi10m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 1, xmin = hdIDA[1], xmax = hdIDA[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 1, x = hdIDAm, colour = "grey10", size = 1.2) +
  annotate("text", label = paste("N = ", nbppt03), x = 0.65, y = 03, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt10), x = 0.65, y = 02, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbpptIDA), x = 0.65, y = 01, colour = "grey10", size = 3.2, family = font_rc_light) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") + 
  labs(x = expression(beta[RWA%*%"Valence"]),
       y = "Experiment")+
  scale_x_continuous(breaks=seq(-0.05, 0.55, 0.1)) +
  scale_y_discrete(expand = c(-0.9, 0.8)) +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position="none")

plot_IDA
ggsave("plots/forest_interact_EC_eng.jpg", width = 35, height = 20, units = "cm")
ggsave("plots/forest_interact_EC_eng.png", width = 35, height = 20, units = "cm")
ggsave("plots/forest_interact_EC_eng.pdf", width = 35, height = 20, units = "cm")
