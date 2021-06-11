############################
####### FOREST PLOTS #######
############################

#### Forest plot for interaction ####

datIDAcc <- data.frame( value = c(posterior_samples(XP01cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP02cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP03cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP04cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP05cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP06cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP07cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP08cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP09cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(XP11cc_resp, "b")$"b_usvalence:RWAscore",
                                posterior_samples(IDAcc_resp, "b")$"b_usvalence:RWAscore"
),
type = c(rep("Experiment 1",4000),rep("Experiment 2",4000),
         rep("Experiment 3",4000),rep("Experiment 4",4000),
         rep("Experiment 5",4000),rep("Experiment 6",4000),
         rep("Experiment 7",4000),rep("Experiment 8",4000),
         rep("Experiment 9",4000),rep("Experiment 10",4000),
         rep("Integrative analysis",4000)
)
)


# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

hdi01 <- HDInterval::hdi(posterior_samples(XP01cc_resp, "b")$"b_usvalence:RWAscore",
                credMass = 0.95)
hdi01m <- mean(posterior_samples(XP01cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi02 <- HDInterval::hdi(posterior_samples(XP02cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi02m <- mean(posterior_samples(XP02cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi03 <- HDInterval::hdi(posterior_samples(XP03cc_resp, "b")$"b_usvalence:RWAscore",
                        credMass = 0.95)
hdi03m <- mean(posterior_samples(XP03cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi04 <- HDInterval::hdi(posterior_samples(XP04cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi04m <- mean(posterior_samples(XP04cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi05 <- HDInterval::hdi(posterior_samples(XP05cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi05m <- mean(posterior_samples(XP05cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi06 <- HDInterval::hdi(posterior_samples(XP06cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi06m <- mean(posterior_samples(XP06cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi07 <- HDInterval::hdi(posterior_samples(XP07cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi07m <- mean(posterior_samples(XP07cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi08 <- HDInterval::hdi(posterior_samples(XP08cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi08m <- mean(posterior_samples(XP08cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi09 <- HDInterval::hdi(posterior_samples(XP09cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi09m <- mean(posterior_samples(XP09cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdi11 <- HDInterval::hdi(posterior_samples(XP11cc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdi11m <- mean(posterior_samples(XP11cc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

hdIDAcc <- HDInterval::hdi(posterior_samples(IDAcc_resp, "b")$"b_usvalence:RWAscore",
                         credMass = 0.95)
hdIDAccm <- mean(posterior_samples(IDAcc_resp, "b")$"b_usvalence:RWAscore",
               credMass = 0.95)

nbppt01 <- length(unique(XP01$ppt))
nbppt02 <- length(unique(XP02$ppt))
nbppt03cc <- length(unique(XP03cc$ppt))
nbppt04 <- length(unique(XP04$ppt))
nbppt05 <- length(unique(XP05$ppt))
nbppt06 <- length(unique(XP06$ppt))
nbppt07 <- length(unique(XP07$ppt))
nbppt08 <- length(unique(XP08$ppt))
nbppt09 <- length(unique(XP09$ppt))
nbppt10 <- length(unique(XP11$ppt))
nbpptIDAcc <- length(unique(IDAcc$ppt))


plot_IDAcc <- datIDAcc %>% 
  mutate(type = factor(type, levels = rev(unique(datIDAcc$type)))) %>% 
  ggplot(aes(x=value, y=type, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      scale = 0.9, rel_min_height = 0.01, color = "#A0A0A0A0") +
  scale_fill_manual(name = "Probability", values = c("#A0A0A0A0", "#A0A0A0A0", "#A0A0A0A0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  annotate("rect", xmin = -0.13, xmax = 0.13, ymin = 0.8, ymax = 11.5, fill = "blue", alpha = 0.2) +
  #annotate("rect", xmin = -0.9, xmax = 1.3, ymin = 1.94, ymax = 1.96, fill = "black", alpha = 0.7) +
  annotate("errorbarh", y = 11, xmin = hdi01[1], xmax = hdi01[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 11, x = hdi01m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 10, xmin = hdi02[1], xmax = hdi02[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 10, x = hdi02m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 9, xmin = hdi03[1], xmax = hdi03[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 9, x = hdi03m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 8, xmin = hdi04[1], xmax = hdi04[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 8, x = hdi04m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 7, xmin = hdi05[1], xmax = hdi05[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 7, x = hdi05m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 6, xmin = hdi06[1], xmax = hdi06[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 6, x = hdi06m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 5, xmin = hdi07[1], xmax = hdi07[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 5, x = hdi07m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 4, xmin = hdi08[1], xmax = hdi08[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 4, x = hdi08m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 3, xmin = hdi09[1], xmax = hdi09[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 3, x = hdi09m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 2, xmin = hdi11[1], xmax = hdi11[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 2, x = hdi11m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 1, xmin = hdIDAcc[1], xmax = hdIDAcc[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 1, x = hdIDAccm, colour = "grey10", size = 1.2) +
  annotate("text", label = paste("N = ", nbppt01), x = 1.4, y = 11, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt02), x = 1.4, y = 10, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt03cc), x = 1.4, y = 09, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt04), x = 1.4, y = 08, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt05), x = 1.4, y = 07, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt06), x = 1.4, y = 06, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt07), x = 1.4, y = 05, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt08), x = 1.4, y = 04, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt09), x = 1.4, y = 03, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt10), x = 1.4, y = 02, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbpptIDAcc), x = 1.4, y = 01, colour = "grey10", size = 3.2, family = font_rc_light) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") + 
  labs(x = expression(beta[RWA%*%"Valence"]),
       y = "Experiment")+
  scale_x_continuous(breaks=seq(-1, 1.25, 0.25))+
  scale_y_discrete(expand = c(-0.9, 1)) +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position="none")

plot_IDAcc
ggsave("plots/forest_interact_ECC_eng.jpg", width = 35, height = 20, units = "cm")
ggsave("plots/forest_interact_ECC_eng.png", width = 35, height = 20, units = "cm")
ggsave("plots/forest_interact_ECC_eng.pdf", width = 35, height = 20, units = "cm")
