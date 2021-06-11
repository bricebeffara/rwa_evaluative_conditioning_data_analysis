if (!require("pacman")) install.packages("pacman")
p_load(brms, # main package for models
       htmlTable, # helps to extract results
       xtable,
       install = TRUE,
       gridExtra,
       sjstats,
       sjmisc,
       update = getOption("pac_update"),
       character.only = FALSE)

#------------------------------------------------------------------------------------
# First we define priors for our models
#------------------------------------------------------------------------------------

priors <- c(
  prior(normal(0, 10), class = Intercept, coef = ""),
  prior(normal(0, 0.5), class = b),
  prior(exponential(2), class = sd) )

# In case we want to save summaries
# col2keep <- c("Estimate", "l-95% CI", "u-95% CI")

# Define ROPE for effects involving RWA
# We compute a global maximum range of RWA

rrwa <- round(max ( IDA$RWAscore) - min ( IDA$RWAscore))

# ROPE = 1 (minimum change on rating scale) / rrwa (global RWA range) / 2 (e.g. see Kruschke, 2018 p.6)
#(rounded in order to have integers simpler to visualise. We don't need very high precision)
# (here we devide or smallest possible effect by two in order to take variations into account,
# e.g. RWA range above limits
# and avoid to be too conservative just above/below lower/upperlimits.
# e.g. we consider that somme intermediate values could be of interest on the latent metric scale (i.e. between 0.5 and 1))

roperwaN <- c( round( -1/rrwa/2, digits = 1), round( 1/rrwa/2, digits = 1))
roperwaC <- paste( roperwaN[1], ", ", roperwaN[2], sep = "")

#------------------------------------------------------------------------------------
# XP03
#------------------------------------------------------------------------------------

# model
XP03_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP03, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP03 <- summary(XP03_resp_neg)$fixed[,col2keep]
# model_neg_XP03 <- round(model_neg_XP03, 3)

# arrange output
model_neg_XP03 <- tidy_stan(XP03_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP03.png", height=480, width=720)
p<-tableGrob(model_neg_XP03)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP03_rwa <- equi_test(XP03_resp_neg, rope = roperwaN)
equi_neg_XP03_rwa <- equi_neg_XP03_rwa[c(10,11),]
equi_neg_XP03_rwa$ROPE <- roperwaC

equi_neg_XP03_0.5 <- equi_test(XP03_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP03_0.5 <- equi_neg_XP03_0.5[9,]
equi_neg_XP03_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP03 <- rbind(equi_neg_XP03_rwa, equi_neg_XP03_0.5)
equi_neg_XP03[,c(3:5)] <- round(equi_neg_XP03[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP03 <- equi_neg_XP03[ordrow,]

# export test
png("tables/rope/equi_neg_XP03.png", height=480, width=720)
p<-tableGrob(equi_neg_XP03)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP10
#------------------------------------------------------------------------------------

# model
XP10_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP10, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP10 <- summary(XP10_resp_neg)$fixed[,col2keep]
# model_neg_XP10 <- round(model_neg_XP10, 3)

# arrange output
model_neg_XP10 <- tidy_stan(XP10_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP10.png", height=480, width=720)
p<-tableGrob(model_neg_XP10)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP10_rwa <- equi_test(XP10_resp_neg, rope = roperwaN)
equi_neg_XP10_rwa <- equi_neg_XP10_rwa[c(10,11),]
equi_neg_XP10_rwa$ROPE <- roperwaC

equi_neg_XP10_0.5 <- equi_test(XP10_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP10_0.5 <- equi_neg_XP10_0.5[9,]
equi_neg_XP10_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP10 <- rbind(equi_neg_XP10_rwa, equi_neg_XP10_0.5)
equi_neg_XP10[,c(3:5)] <- round(equi_neg_XP10[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP10 <- equi_neg_XP10[ordrow,]

# export test
png("tables/rope/equi_neg_XP10.png", height=480, width=720)
p<-tableGrob(equi_neg_XP10)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# IDA
#------------------------------------------------------------------------------------

# model
IDA_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1) + (1|XP),
                data = IDA, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.9999, max_treedepth = 10),
                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_IDA <- summary(ida_resp_neg)$fixed[,col2keep]
# model_neg_IDA <- round(model_neg_IDA, 3)

# arrange output
model_neg_IDA <- model_parameters(IDA_resp_neg,
                                 centrality = "median",
                                 dispersion = TRUE,
                                 ci = .95,
                                 ci_method = "HDI",
                                 rope_range = c(-0.13, 0.13),
                                 rope_ci = 1)


# export output
png("tables/brms/model_neg_IDA.png", height=480, width=720)
p<-tableGrob(model_neg_IDA)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_IDA_rwa <- equi_test(IDA_resp_neg, rope = roperwaN)
equi_neg_IDA_rwa <- equi_neg_IDA_rwa[c(10,11),]
equi_neg_IDA_rwa$ROPE <- roperwaC

equi_neg_IDA_0.5 <- equi_test(IDA_resp_neg, rope = c(-0.5, 0.5))
equi_neg_IDA_0.5 <- equi_neg_IDA_0.5[9,]
equi_neg_IDA_0.5$ROPE <- "-0.5, 0.5"

equi_neg_IDA <- rbind(equi_neg_IDA_rwa, equi_neg_IDA_0.5)
equi_neg_IDA[,c(3:5)] <- round(equi_neg_IDA[,c(3:5)], 2)
ordrow <- c(3, 1, 2)
equi_neg_IDA <- equi_neg_IDA[ordrow,]

# export test
png("tables/rope/equi_neg_IDA.png", height=480, width=720)
p<-tableGrob(equi_neg_IDA)
grid.arrange(p)
dev.off()
