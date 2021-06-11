# Loading packages needed (and installing if necessary) for this part

if (!require("pacman")) install.packages("pacman")
p_load(ggExtra,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

#ida_df$Valence <- ifelse (ida_df$usvalence == -0.5, "Négative", "Positive")

# data plot -----------------------------------------------------------

#data_plot_cc = ida_df %>%
#  ggplot(aes(x = response, y = RWAscore,
#             fill = Valence, color = Valence)) +
#  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
#                                             dodge.width = -0.75), alpha = 0.4, size = 1,
#             shape = 19, inherit.aes = TRUE) +
#  labs(x = 'Évaluations', y = 'RWA') +
#  scale_fill_manual(values=c("#73a5ff", "#50ce76")) +
#  scale_color_manual(values= c("#73a5ff", "#50ce76"), guide = "none") +
#  coord_cartesian(ylim=c(1,9)) +
#  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand=c(0.01,0)) +
#  scale_x_continuous(limits=c(1, 9), breaks=seq(1, 9, 1)) +
#  labs(color="Valence conditionnement",
#       fill="Valence conditionnement") +
#  theme_ipsum_rc(base_size = 13,
#                 subtitle_size = 20,
#                 axis_title_size = 15) +
#  guides(fill = guide_legend(override.aes = list(linetype = 0)),
#         color = guide_legend(override.aes = list(linetype = 0))) +
#  coord_flip()
#
#data_plot_cc <- ggMarginal(data_plot_cc, margins = "x", alpha = 0.6,
#                              type = "histogram", size = 4, fill = "gray", colour = "lightgray")
#
#data_plot_cc <- ggarrange(data_plot_cc,
 #                          ncol = 1, nrow = 1)

data_plot_cc = IDAcc %>%
  ggplot(aes(x = response, y = RWAscore,
             fill = Valence, color = Valence)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.05, 
                                             dodge.width = 0.7), alpha = 0.2, size = 1,
             shape = 19, inherit.aes = TRUE) +
  labs(x = 'Evaluation', y = 'RWA') +
  scale_fill_manual(values=c("#CC79A7", "#0072B2")) +
  scale_color_manual(values= c("#CC79A7", "#0072B2"), guide = "none") +
  #  coord_cartesian(ylim=c(1,9)) +
  #scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(-2.715, 3.21)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits=c(-3.615, 4.115)) +
  scale_x_continuous(limits=c(0.45, 9.55), breaks=seq(1, 9, 1)) +
  labs(color="Conditioning valence",
       fill="Conditioning valence") +
  theme_ipsum_rc(base_size = 23,
                 subtitle_size = 38,
                 axis_title_size = 29) +
  guides(fill = guide_legend(override.aes = list(linetype = 0, shape = 15, alpha = 0.9, size = 3), reverse = TRUE),
         color = guide_legend(override.aes = list(linetype = 0, shape = 15, alpha = 0.9), reverse = TRUE)) +
  #  + ylim(0, 20)
  coord_flip()

# uncomment to display and save plot
# data_plot_cc
# ggsave("plots/data_IDA_french.jpg", width = 25, height = 15, units = "cm")
# ggsave("plots/data_IDA_french.pdf", width = 25, height = 15, units = "cm")
# ggsave("plots/data_IDA_french.tex", width = 25, height = 15, units = "cm")

# Combine with spaghetti
data_spag_all_cc <- ggarrange(marg_plot_cc,
                           data_plot_cc,
                           ncol = 2, nrow = 1,
                           widths = c(0.68,1))

# uncomment to display and save plot
# data_spag_all_cc
# ggsave("plots/data_spag_IDA_cc.jpg", width = 50, height = 15, units = "cm")
# ggsave("plots/data_spag_IDA_cc.png", width = 50, height = 15, units = "cm")
# ggsave("plots/data_spag_IDA_french.pdf", width = 50, height = 15, units = "cm")
# ggsave("plots/data_spag_IDA_french.tex", width = 50, height = 15, units = "cm")

