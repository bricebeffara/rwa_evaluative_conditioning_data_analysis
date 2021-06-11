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

rrwa <- round(max ( IDAcc$RWAscore) - min ( IDAcc$RWAscore))

# ROPE = 1 (minimum change on rating scale) / rrwa (global RWA range) / 2 (e.g. see Kruschke, 2018 p.6)
#(rounded in order to have integers simpler to visualise. We don't need very high precision)
# (here we devide or smallest possible effect by two in order to take variations into account,
# e.g. RWA range above limits
# and avoid to be too conservative just above/below lower/upperlimits.
# e.g. we consider that somme intermediate values could be of interest on the latent metric scale (i.e. between 0.5 and 1))

roperwaN <- c( round( -1/rrwa/2, digits = 1), round( 1/rrwa/2, digits = 1))
roperwaC <- paste( roperwaN[1], ", ", roperwaN[2], sep = "")

#------------------------------------------------------------------------------------
# XP01
#------------------------------------------------------------------------------------

# model
XP01cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP01, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP01 <- summary(XP01cc_resp_neg)$fixed[,col2keep]
# model_neg_XP01 <- round(model_neg_XP01, 3)

# arrange output
model_neg_XP01 <- tidy_stan(XP01cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP01.png", height=480, width=720)
p<-tableGrob(model_neg_XP01)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP01_rwa <- equi_test(XP01cc_resp_neg, rope = roperwaN)
equi_neg_XP01_rwa <- equi_neg_XP01_rwa[c(10,11),]
equi_neg_XP01_rwa$ROPE <- roperwaC

equi_neg_XP01_0.5 <- equi_test(XP01cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP01_0.5 <- equi_neg_XP01_0.5[9,]
equi_neg_XP01_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP01 <- rbind(equi_neg_XP01_rwa, equi_neg_XP01_0.5)
equi_neg_XP01[,c(3:5)] <- round(equi_neg_XP01[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP01 <- equi_neg_XP01[ordrow,]

# export test
png("tables/rope/equi_neg_XP01.png", height=480, width=720)
p<-tableGrob(equi_neg_XP01)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP02
#------------------------------------------------------------------------------------

# model
XP02cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP02, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP02 <- summary(XP02cc_resp_neg)$fixed[,col2keep]
# model_neg_XP02 <- round(model_neg_XP02, 3)

# arrange output
model_neg_XP02 <- tidy_stan(XP02cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP02.png", height=480, width=720)
p<-tableGrob(model_neg_XP02)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP02_rwa <- equi_test(XP02cc_resp_neg, rope = roperwaN)
equi_neg_XP02_rwa <- equi_neg_XP02_rwa[c(10,11),]
equi_neg_XP02_rwa$ROPE <- roperwaC

equi_neg_XP02_0.5 <- equi_test(XP02cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP02_0.5 <- equi_neg_XP02_0.5[9,]
equi_neg_XP02_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP02 <- rbind(equi_neg_XP02_rwa, equi_neg_XP02_0.5)
equi_neg_XP02[,c(3:5)] <- round(equi_neg_XP02[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP02 <- equi_neg_XP02[ordrow,]

# export test
png("tables/rope/equi_neg_XP02.png", height=480, width=720)
p<-tableGrob(equi_neg_XP02)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP03
#------------------------------------------------------------------------------------

# model
XP03cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP03cc, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP03 <- summary(XP03cc_resp_neg)$fixed[,col2keep]
# model_neg_XP03 <- round(model_neg_XP03, 3)

# arrange output
model_neg_XP03 <- tidy_stan(XP03cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP03.png", height=480, width=720)
p<-tableGrob(model_neg_XP03)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP03_rwa <- equi_test(XP03cc_resp_neg, rope = roperwaN)
equi_neg_XP03_rwa <- equi_neg_XP03_rwa[c(10,11),]
equi_neg_XP03_rwa$ROPE <- roperwaC

equi_neg_XP03_0.5 <- equi_test(XP03cc_resp_neg, rope = c(-0.5, 0.5))
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
# XP04
#------------------------------------------------------------------------------------

# model
XP04cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP04, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP04 <- summary(XP04cc_resp_neg)$fixed[,col2keep]
# model_neg_XP04 <- round(model_neg_XP04, 3)

# arrange output
model_neg_XP04 <- tidy_stan(XP04cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP04.png", height=480, width=720)
p<-tableGrob(model_neg_XP04)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP04_rwa <- equi_test(XP04cc_resp_neg, rope = roperwaN)
equi_neg_XP04_rwa <- equi_neg_XP04_rwa[c(10,11),]
equi_neg_XP04_rwa$ROPE <- roperwaC

equi_neg_XP04_0.5 <- equi_test(XP04cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP04_0.5 <- equi_neg_XP04_0.5[9,]
equi_neg_XP04_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP04 <- rbind(equi_neg_XP04_rwa, equi_neg_XP04_0.5)
equi_neg_XP04[,c(3:5)] <- round(equi_neg_XP04[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP04 <- equi_neg_XP04[ordrow,]

# export test
png("tables/rope/equi_neg_XP04.png", height=480, width=720)
p<-tableGrob(equi_neg_XP04)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP05
#------------------------------------------------------------------------------------

# model
XP05cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP05, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP05 <- summary(XP05cc_resp_neg)$fixed[,col2keep]
# model_neg_XP05 <- round(model_neg_XP05, 3)

# arrange output
model_neg_XP05 <- tidy_stan(XP05cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP05.png", height=480, width=720)
p<-tableGrob(model_neg_XP05)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP05_rwa <- equi_test(XP05cc_resp_neg, rope = roperwaN)
equi_neg_XP05_rwa <- equi_neg_XP05_rwa[c(10,11),]
equi_neg_XP05_rwa$ROPE <- roperwaC

equi_neg_XP05_0.5 <- equi_test(XP05cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP05_0.5 <- equi_neg_XP05_0.5[9,]
equi_neg_XP05_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP05 <- rbind(equi_neg_XP05_rwa, equi_neg_XP05_0.5)
equi_neg_XP05[,c(3:5)] <- round(equi_neg_XP05[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP05 <- equi_neg_XP05[ordrow,]

# export test
png("tables/rope/equi_neg_XP05.png", height=480, width=720)
p<-tableGrob(equi_neg_XP05)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP06
#------------------------------------------------------------------------------------

# model
XP06cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP06, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP06 <- summary(XP06cc_resp_neg)$fixed[,col2keep]
# model_neg_XP06 <- round(model_neg_XP06, 3)

# arrange output
model_neg_XP06 <- tidy_stan(XP06cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP06.png", height=480, width=720)
p<-tableGrob(model_neg_XP06)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP06_rwa <- equi_test(XP06cc_resp_neg, rope = roperwaN)
equi_neg_XP06_rwa <- equi_neg_XP06_rwa[c(10,11),]
equi_neg_XP06_rwa$ROPE <- roperwaC

equi_neg_XP06_0.5 <- equi_test(XP06cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP06_0.5 <- equi_neg_XP06_0.5[9,]
equi_neg_XP06_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP06 <- rbind(equi_neg_XP06_rwa, equi_neg_XP06_0.5)
equi_neg_XP06[,c(3:5)] <- round(equi_neg_XP06[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP06 <- equi_neg_XP06[ordrow,]

# export test
png("tables/rope/equi_neg_XP06.png", height=480, width=720)
p<-tableGrob(equi_neg_XP06)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP07
#------------------------------------------------------------------------------------

# model
XP07cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP07, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP07 <- summary(XP07cc_resp_neg)$fixed[,col2keep]
# model_neg_XP07 <- round(model_neg_XP07, 3)

# arrange output
model_neg_XP07 <- tidy_stan(XP07cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP07.png", height=480, width=720)
p<-tableGrob(model_neg_XP07)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP07_rwa <- equi_test(XP07cc_resp_neg, rope = roperwaN)
equi_neg_XP07_rwa <- equi_neg_XP07_rwa[c(10,11),]
equi_neg_XP07_rwa$ROPE <- roperwaC

equi_neg_XP07_0.5 <- equi_test(XP07cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP07_0.5 <- equi_neg_XP07_0.5[9,]
equi_neg_XP07_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP07 <- rbind(equi_neg_XP07_rwa, equi_neg_XP07_0.5)
equi_neg_XP07[,c(3:5)] <- round(equi_neg_XP07[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP07 <- equi_neg_XP07[ordrow,]

# export test
png("tables/rope/equi_neg_XP07.png", height=480, width=720)
p<-tableGrob(equi_neg_XP07)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP08
#------------------------------------------------------------------------------------

# model
XP08cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP08, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP08 <- summary(XP08cc_resp_neg)$fixed[,col2keep]
# model_neg_XP08 <- round(model_neg_XP08, 3)

# arrange output
model_neg_XP08 <- tidy_stan(XP08cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP08.png", height=480, width=720)
p<-tableGrob(model_neg_XP08)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP08_rwa <- equi_test(XP08cc_resp_neg, rope = roperwaN)
equi_neg_XP08_rwa <- equi_neg_XP08_rwa[c(10,11),]
equi_neg_XP08_rwa$ROPE <- roperwaC

equi_neg_XP08_0.5 <- equi_test(XP08cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP08_0.5 <- equi_neg_XP08_0.5[9,]
equi_neg_XP08_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP08 <- rbind(equi_neg_XP08_rwa, equi_neg_XP08_0.5)
equi_neg_XP08[,c(3:5)] <- round(equi_neg_XP08[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP08 <- equi_neg_XP08[ordrow,]

# export test
png("tables/rope/equi_neg_XP08.png", height=480, width=720)
p<-tableGrob(equi_neg_XP08)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP09
#------------------------------------------------------------------------------------

# model
XP09cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP09, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP09 <- summary(XP09cc_resp_neg)$fixed[,col2keep]
# model_neg_XP09 <- round(model_neg_XP09, 3)

# arrange output
model_neg_XP09 <- tidy_stan(XP09cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP09.png", height=480, width=720)
p<-tableGrob(model_neg_XP09)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP09_rwa <- equi_test(XP09cc_resp_neg, rope = roperwaN)
equi_neg_XP09_rwa <- equi_neg_XP09_rwa[c(10,11),]
equi_neg_XP09_rwa$ROPE <- roperwaC

equi_neg_XP09_0.5 <- equi_test(XP09cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP09_0.5 <- equi_neg_XP09_0.5[9,]
equi_neg_XP09_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP09 <- rbind(equi_neg_XP09_rwa, equi_neg_XP09_0.5)
equi_neg_XP09[,c(3:5)] <- round(equi_neg_XP09[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP09 <- equi_neg_XP09[ordrow,]

# export test
png("tables/rope/equi_neg_XP09.png", height=480, width=720)
p<-tableGrob(equi_neg_XP09)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP11
#------------------------------------------------------------------------------------

# model
XP11cc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP11, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_XP11 <- summary(XP11cc_resp_neg)$fixed[,col2keep]
# model_neg_XP11 <- round(model_neg_XP11, 3)

# arrange output
model_neg_XP11 <- tidy_stan(XP11cc_resp_neg,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_neg_XP11.png", height=480, width=720)
p<-tableGrob(model_neg_XP11)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_XP11_rwa <- equi_test(XP11cc_resp_neg, rope = roperwaN)
equi_neg_XP11_rwa <- equi_neg_XP11_rwa[c(10,11),]
equi_neg_XP11_rwa$ROPE <- roperwaC

equi_neg_XP11_0.5 <- equi_test(XP11cc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_XP11_0.5 <- equi_neg_XP11_0.5[9,]
equi_neg_XP11_0.5$ROPE <- "-0.5, 0.5"

equi_neg_XP11 <- rbind(equi_neg_XP11_rwa, equi_neg_XP11_0.5)
equi_neg_XP11[,c(3:5)] <- round(equi_neg_XP11[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_neg_XP11 <- equi_neg_XP11[ordrow,]

# export test
png("tables/rope/equi_neg_XP11.png", height=480, width=720)
p<-tableGrob(equi_neg_XP11)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# IDAcc
#------------------------------------------------------------------------------------

# model
IDAcc_resp_neg <- brm(response ~ usvalence_neg  * RWAscore + (1|ppt) + (1|stim1) + (1|XP),
                data = IDAcc, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.8, max_treedepth = 10),
                sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_neg_IDAcc <- summary(IDAcc_resp_neg)$fixed[,col2keep]
# model_neg_IDAcc <- round(model_neg_IDAcc, 3)

# arrange output
model_neg_IDAcc <- model_parameters(IDAcc_resp_neg,
                                    centrality = "median",
                                    dispersion = TRUE,
                                    ci = .95,
                                    ci_method = "HDI",
                                    rope_range = c(-0.13, 0.13),
                                    rope_ci = 1)

# export output
png("tables/brms/model_neg_IDAcc.png", height=480, width=720)
p<-tableGrob(model_neg_IDAcc)
grid.arrange(p)
dev.off()

# test with rope
equi_neg_IDAcc_rwa <- equi_test(IDAcc_resp_neg, rope = roperwaN)
equi_neg_IDAcc_rwa <- equi_neg_IDAcc_rwa[c(10,11),]
equi_neg_IDAcc_rwa$ROPE <- roperwaC

equi_neg_IDAcc_0.5 <- equi_test(IDAcc_resp_neg, rope = c(-0.5, 0.5))
equi_neg_IDAcc_0.5 <- equi_neg_IDAcc_0.5[9,]
equi_neg_IDAcc_0.5$ROPE <- "-0.5, 0.5"

equi_neg_IDAcc <- rbind(equi_neg_IDAcc_rwa, equi_neg_IDAcc_0.5)
equi_neg_IDAcc[,c(3:5)] <- round(equi_neg_IDAcc[,c(3:5)], 2)
ordrow <- c(3, 1, 2)
equi_neg_IDAcc <- equi_neg_IDAcc[ordrow,]

# export test
png("tables/rope/equi_neg_IDAcc.png", height=480, width=720)
p<-tableGrob(equi_neg_IDAcc)
grid.arrange(p)
dev.off()
