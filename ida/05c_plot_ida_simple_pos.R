############################
####### FOREST PLOTS #######
############################

# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggridges,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#### Forest plot for RWA simple slopes in posative condition ####

datIDAccpos <- data.frame( value = c(posterior_samples(XP01cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP02cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP03cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP04cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP05cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP06cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP07cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP08cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP09cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP11cc_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(IDAcc_resp_pos, "b")$"b_RWAscore"
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

hdi_pos01 <- HDInterval::hdi(posterior_samples(XP01cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos01m <- mean(posterior_samples(XP01cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos02 <- HDInterval::hdi(posterior_samples(XP02cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos02m <- mean(posterior_samples(XP02cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos03 <- HDInterval::hdi(posterior_samples(XP03cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos03m <- mean(posterior_samples(XP03cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos04 <- HDInterval::hdi(posterior_samples(XP04cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos04m <- mean(posterior_samples(XP04cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos05 <- HDInterval::hdi(posterior_samples(XP05cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos05m <- mean(posterior_samples(XP05cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos06 <- HDInterval::hdi(posterior_samples(XP06cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos06m <- mean(posterior_samples(XP06cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos07 <- HDInterval::hdi(posterior_samples(XP07cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos07m <- mean(posterior_samples(XP07cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos08 <- HDInterval::hdi(posterior_samples(XP08cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos08m <- mean(posterior_samples(XP08cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos09 <- HDInterval::hdi(posterior_samples(XP09cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos09m <- mean(posterior_samples(XP09cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos11 <- HDInterval::hdi(posterior_samples(XP11cc_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos11m <- mean(posterior_samples(XP11cc_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_posIDA <- HDInterval::hdi(posterior_samples(IDAcc_resp_pos, "b")$"b_RWAscore",
                              credMass = 0.95)
hdi_posIDAm <- mean(posterior_samples(IDAcc_resp_pos, "b")$"b_RWAscore",
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


plot_IDAccpos <- datIDAccpos %>% 
  mutate(type = factor(type, levels = rev(unique(datIDAccpos$type)))) %>% 
  ggplot(aes(x=value, y=type, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      scale = 0.9, rel_min_height = 0.01, color = "#A0A0A0A0") +
  scale_fill_manual(name = "Probability", values = c("#A0A0A0A0", "#A0A0A0A0", "#A0A0A0A0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  #annotate("rect", xmin = -0.7, xmax = 0.8, ymin = 1.94, ymax = 1.96, fill = "black", alpha = 0.7) +
  annotate("rect", xmin = -0.13, xmax = 0.13, ymin = 0.8, ymax = 11.5, fill = "blue", alpha = 0.2) +
  annotate("errorbarh", y = 11, xmin = hdi_pos01[1], xmax = hdi_pos01[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 11, x = hdi_pos01m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 10, xmin = hdi_pos02[1], xmax = hdi_pos02[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 10, x = hdi_pos02m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 9, xmin = hdi_pos03[1], xmax = hdi_pos03[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 9, x = hdi_pos03m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 8, xmin = hdi_pos04[1], xmax = hdi_pos04[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 8, x = hdi_pos04m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 7, xmin = hdi_pos05[1], xmax = hdi_pos05[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 7, x = hdi_pos05m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 6, xmin = hdi_pos06[1], xmax = hdi_pos06[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 6, x = hdi_pos06m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 5, xmin = hdi_pos07[1], xmax = hdi_pos07[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 5, x = hdi_pos07m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 4, xmin = hdi_pos08[1], xmax = hdi_pos08[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 4, x = hdi_pos08m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 3, xmin = hdi_pos09[1], xmax = hdi_pos09[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 3, x = hdi_pos09m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 2, xmin = hdi_pos11[1], xmax = hdi_pos11[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 2, x = hdi_pos11m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 1, xmin = hdi_posIDA[1], xmax = hdi_posIDA[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 1, x = hdi_posIDAm, colour = "grey10", size = 1.2) +
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
  labs(x = expression(beta[RWA["Positive EC"]]),
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

plot_IDAccpos
ggsave("plots/forest_simple_pos_ECC_eng.jpg", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_pos_ECC_eng.png", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_pos_ECC_eng.pdf", width = 35, height = 20, units = "cm")
