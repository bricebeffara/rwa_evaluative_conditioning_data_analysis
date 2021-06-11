############################
####### FOREST PLOTS #######
############################

# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggridges,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#### Forest plot for RWA simple slopes in negative condition ####

datIDAccneg <- data.frame( value = c(posterior_samples(XP01cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP02cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP03cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP04cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP05cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP06cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP07cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP08cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP09cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(XP11cc_resp_neg, "b")$"b_RWAscore",
                                posterior_samples(IDAcc_resp_neg, "b")$"b_RWAscore"
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

hdi_neg01 <- HDInterval::hdi(posterior_samples(XP01cc_resp_neg, "b")$"b_RWAscore",
                credMass = 0.95)
hdi_neg01m <- mean(posterior_samples(XP01cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg02 <- HDInterval::hdi(posterior_samples(XP02cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg02m <- mean(posterior_samples(XP02cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg03 <- HDInterval::hdi(posterior_samples(XP03cc_resp_neg, "b")$"b_RWAscore",
                        credMass = 0.95)
hdi_neg03m <- mean(posterior_samples(XP03cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg04 <- HDInterval::hdi(posterior_samples(XP04cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg04m <- mean(posterior_samples(XP04cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg05 <- HDInterval::hdi(posterior_samples(XP05cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg05m <- mean(posterior_samples(XP05cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg06 <- HDInterval::hdi(posterior_samples(XP06cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg06m <- mean(posterior_samples(XP06cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg07 <- HDInterval::hdi(posterior_samples(XP07cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg07m <- mean(posterior_samples(XP07cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg08 <- HDInterval::hdi(posterior_samples(XP08cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg08m <- mean(posterior_samples(XP08cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg09 <- HDInterval::hdi(posterior_samples(XP09cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg09m <- mean(posterior_samples(XP09cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_neg11 <- HDInterval::hdi(posterior_samples(XP11cc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_neg11m <- mean(posterior_samples(XP11cc_resp_neg, "b")$"b_RWAscore",
               credMass = 0.95)

hdi_negIDA <- HDInterval::hdi(posterior_samples(IDAcc_resp_neg, "b")$"b_RWAscore",
                         credMass = 0.95)
hdi_negIDAm <- mean(posterior_samples(IDAcc_resp_neg, "b")$"b_RWAscore",
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


plot_IDAccneg <- datIDAccneg %>% 
  mutate(type = factor(type, levels = rev(unique(datIDAccneg$type)))) %>% 
  ggplot(aes(x=value, y=type, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      scale = 0.9, rel_min_height = 0.01, color = "#A0A0A0A0") +
  scale_fill_manual(name = "Probability", values = c("#A0A0A0A0", "#A0A0A0A0", "#A0A0A0A0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  #annotate("rect", xmin = -0.7, xmax = 0.8, ymin = 1.94, ymax = 1.96, fill = "black", alpha = 0.7) +
  annotate("rect", xmin = -0.13, xmax = 0.13, ymin = 0.8, ymax = 11.5, fill = "blue", alpha = 0.2) +
  annotate("errorbarh", y = 11, xmin = hdi_neg01[1], xmax = hdi_neg01[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 11, x = hdi_neg01m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 10, xmin = hdi_neg02[1], xmax = hdi_neg02[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 10, x = hdi_neg02m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 9, xmin = hdi_neg03[1], xmax = hdi_neg03[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 9, x = hdi_neg03m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 8, xmin = hdi_neg04[1], xmax = hdi_neg04[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 8, x = hdi_neg04m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 7, xmin = hdi_neg05[1], xmax = hdi_neg05[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 7, x = hdi_neg05m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 6, xmin = hdi_neg06[1], xmax = hdi_neg06[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 6, x = hdi_neg06m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 5, xmin = hdi_neg07[1], xmax = hdi_neg07[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 5, x = hdi_neg07m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 4, xmin = hdi_neg08[1], xmax = hdi_neg08[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 4, x = hdi_neg08m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 3, xmin = hdi_neg09[1], xmax = hdi_neg09[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 3, x = hdi_neg09m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 2, xmin = hdi_neg11[1], xmax = hdi_neg11[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 2, x = hdi_neg11m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 1, xmin = hdi_negIDA[1], xmax = hdi_negIDA[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 1, x = hdi_negIDAm, colour = "grey10", size = 1.2) +
  annotate("text", label = paste("N = ", nbppt01), x = 0.85, y = 11, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt02), x = 0.85, y = 10, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt03cc), x = 0.85, y = 09, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt04), x = 0.85, y = 08, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt05), x = 0.85, y = 07, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt06), x = 0.85, y = 06, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt07), x = 0.85, y = 05, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt08), x = 0.85, y = 04, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt09), x = 0.85, y = 03, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt10), x = 0.85, y = 02, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbpptIDAcc), x = 0.85, y = 01, colour = "grey10", size = 3.2, family = font_rc_light) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") + 
  labs(x = expression(beta[RWA["Negative EC"]]),
       y = "Experiment")+
  scale_x_continuous(breaks=seq(-0.75, 0.75, 0.25))+
  scale_y_discrete(expand = c(-0.9, 1)) +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position="none")

plot_IDAccneg
ggsave("plots/forest_simple_neg_ECC_eng.jpg", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_neg_ECC_eng.png", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_neg_ECC_eng.pdf", width = 35, height = 20, units = "cm")
