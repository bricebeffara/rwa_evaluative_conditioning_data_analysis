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

datIDApos <- data.frame( value = c(posterior_samples(XP03_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(XP10_resp_pos, "b")$"b_RWAscore",
                                   posterior_samples(IDA_resp_pos, "b")$"b_RWAscore"
),
type = c(rep("Experiment 3",4000),rep("Experiment 11",4000),
         rep("Integrative analysis",4000)
)
)


# load fonts and themes
hrbrthemes::import_roboto_condensed()
loadfonts()

hdi_pos03 <- HDInterval::hdi(posterior_samples(XP03_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos03m <- mean(posterior_samples(XP03_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_pos10 <- HDInterval::hdi(posterior_samples(XP10_resp_pos, "b")$"b_RWAscore",
                             credMass = 0.95)
hdi_pos10m <- mean(posterior_samples(XP10_resp_pos, "b")$"b_RWAscore",
                   credMass = 0.95)

hdi_posIDA <- HDInterval::hdi(posterior_samples(IDA_resp_pos, "b")$"b_RWAscore",
                              credMass = 0.95)
hdi_posIDAm <- mean(posterior_samples(IDA_resp_pos, "b")$"b_RWAscore",
                    credMass = 0.95)

nbppt03 <- length(unique(XP03$ppt))
nbppt10 <- length(unique(XP10$ppt))
nbpptIDA <- length(unique(IDA$ppt))


plot_IDApos <- datIDApos %>% 
  mutate(type = factor(type, levels = rev(unique(datIDApos$type)))) %>% 
  ggplot(aes(x=value, y=type, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.025, 0.975),
                      scale = 0.9, rel_min_height = 0.01, color = "#A0A0A0A0") +
  scale_fill_manual(name = "Probability", values = c("#A0A0A0A0", "#A0A0A0A0", "#A0A0A0A0"),
                    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")) +
  annotate("rect", xmin = -0.13, xmax = 0.13, ymin = 0.9, ymax = 3.8, fill = "blue", alpha = 0.2) +
  #annotate("rect", xmin = -0.05, xmax = 0.65, ymin = 1.945, ymax = 1.955, fill = "gray30", alpha = 0.7) +
  annotate("errorbarh", y = 3, xmin = hdi_pos03[1], xmax = hdi_pos03[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 3, x = hdi_pos03m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 2, xmin = hdi_pos10[1], xmax = hdi_pos10[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 2, x = hdi_pos10m, colour = "grey10", size = 1.2) +
  annotate("errorbarh", y = 1, xmin = hdi_posIDA[1], xmax = hdi_posIDA[2], colour = "grey10", size = 0.6, height = 0) +
  annotate("point", y = 1, x = hdi_posIDAm, colour = "grey10", size = 1.2) +
  annotate("text", label = paste("N = ", nbppt03), x = 0.65, y = 03, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbppt10), x = 0.65, y = 02, colour = "grey10", size = 3.2, family = font_rc_light) +
  annotate("text", label = paste("N = ", nbpptIDA), x = 0.65, y = 01, colour = "grey10", size = 3.2, family = font_rc_light) +
  geom_vline(xintercept = 0, linetype="dashed", color = "red") + 
  labs(x = expression(beta[RWA["Positive EC"]]),
       y = "Experiment")+
  scale_x_continuous(breaks=seq(-0.05, 0.65, 0.1)) +
  scale_y_discrete(expand = c(-0.9, 0.8)) +
  theme_ipsum_rc(base_size = 13,
                 subtitle_size = 20,
                 axis_title_size = 15) +
  theme(strip.background = element_blank(),
        panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        legend.position="none")

plot_IDApos
ggsave("plots/forest_simple_pos_EC_eng.jpg", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_pos_EC_eng.png", width = 35, height = 20, units = "cm")
ggsave("plots/forest_simple_pos_EC_eng.pdf", width = 35, height = 20, units = "cm")
