if (!require("pacman")) install.packages("pacman")
p_load(brms, # main package for models
       htmlTable, # helps to extract results
       xtable,
       effectsize,
       install = TRUE,
       gridExtra,
       sjstats,
       sjmisc,
       bayestestR,
       see,
       parameters,
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
XP03_resp <- brm(response ~ usvalence  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP03, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_XP03 <- summary(XP03_resp)$fixed[,col2keep]
# model_gen_XP03 <- round(model_gen_XP03, 3)

# arrange output
model_gen_XP03 <- tidy_stan(XP03_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_gen_XP03.png", height=480, width=720)
p<-tableGrob(model_gen_XP03)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_XP03_rwa <- equivalence_test(XP03_resp, rope = roperwaN)
equi_gen_XP03_rwa <- as.data.frame(equi_gen_XP03_rwa[c(10,11),])
equi_gen_XP03_rwa$ROPE <- roperwaC

equi_gen_XP03_0.5 <- equivalence_test(XP03_resp, rope = c(-0.5, 0.5))
equi_gen_XP03_0.5 <- as.data.frame(equi_gen_XP03_0.5[9,])
equi_gen_XP03_0.5$ROPE <- "-0.5, 0.5"

equi_gen_XP03 <- rbind(equi_gen_XP03_rwa, equi_gen_XP03_0.5)
equi_gen_XP03[,c(3:5)] <- round(equi_gen_XP03[,c(3:5)], 2)
equi_gen_XP03[,c(7:8)] <- round(equi_gen_XP03[,c(7:8)], 2)
ordrow <- c("10", "9", "11")
equi_gen_XP03 <- equi_gen_XP03[ordrow,]

# export test
png("tables/rope/equi_gen_XP03.png", height=480, width=720)
p<-tableGrob(equi_gen_XP03)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# XP10
#------------------------------------------------------------------------------------

# model
XP10_resp <- brm(response ~ usvalence  * RWAscore + (1|ppt) + (1|stim1),
                 data = XP10, 
                 family = cumulative (link = "logit", threshold = "flexible"),
                 prior = priors,
                 warmup = 1000, iter = 2000,
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.8, max_treedepth = 10),
                 sample_prior = TRUE)

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_XP10 <- summary(XP10_resp)$fixed[,col2keep]
# model_gen_XP10 <- round(model_gen_XP10, 3)

# arrange output
model_gen_XP10 <- tidy_stan(XP10_resp,
                            typical = "mean",
                            prob = .95)


# export output
png("tables/brms/model_gen_XP10.png", height=480, width=720)
p<-tableGrob(model_gen_XP10)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_XP10_rwa <- equivalence_test(XP10_resp, rope = roperwaN)
equi_gen_XP10_rwa <- as.data.frame(equi_gen_XP10_rwa[c(10,11),])
equi_gen_XP10_rwa$ROPE <- roperwaC

equi_gen_XP10_0.5 <- equivalence_test(XP10_resp, rope = c(-0.5, 0.5))
equi_gen_XP10_0.5 <- as.data.frame(equi_gen_XP10_0.5[9,])
equi_gen_XP10_0.5$ROPE <- "-0.5, 0.5"

equi_gen_XP10 <- rbind(equi_gen_XP10_rwa, equi_gen_XP10_0.5)
equi_gen_XP10[,c(3:5)] <- round(equi_gen_XP10[,c(3:5)], 2)
equi_gen_XP10[,c(7:8)] <- round(equi_gen_XP10[,c(7:8)], 2)
ordrow <- c("10", "9", "11")
equi_gen_XP10 <- equi_gen_XP10[ordrow,]

# export test
png("tables/rope/equi_gen_XP10.png", height=480, width=720)
p<-tableGrob(equi_gen_XP10)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# IDA
#------------------------------------------------------------------------------------

# model
IDA_resp <- brm(response ~ usvalence * RWAscore + (1|ppt) + (1|stim1) + (1|XP),
                data = IDA, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.9999, max_treedepth = 10),
                sample_prior = TRUE)

IDA_resp_woint <- brm(response ~ usvalence + RWAscore + (1|ppt) + (1|stim1) + (1|XP),
                        data = IDA, 
                        family = cumulative (link = "logit", threshold = "flexible"),
                        prior = priors,
                        warmup = 1000, iter = 2000,
                        chains = 4, cores = parallel::detectCores(),
                        control = list(adapt_delta = 0.8, max_treedepth = 10),
                        sample_prior = TRUE)

IDA_resp_bckup01 <- IDA_resp
IDA_resp_bckup02 <- IDA_resp
  
# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_IDA <- summary(ida_resp)$fixed[,col2keep]
# model_gen_IDA <- round(model_gen_IDA, 3)

# arrange output
model_gen_IDA <- model_parameters(IDA_resp,
                                  centrality = "median",
                                  dispersion = TRUE,
                                  ci = .95,
                                  ci_method = "HDI",
                                  rope_range = c(-0.13, 0.13),
                                  rope_ci = 1)

model_gen_IDA_woint <- model_parameters(IDA_resp_woint,
                                  centrality = "median",
                                  dispersion = TRUE,
                                  ci = .95,
                                  ci_method = "HDI",
                                  rope_range = c(-0.13, 0.13),
                                  rope_ci = 1)


# export output
png("tables/brms/model_gen_IDA.png", height=480, width=720)
p<-tableGrob(model_gen_IDA)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_IDA_rwa <- equi_test(ida_resp, rope = roperwaN)
equi_gen_IDA_rwa <- equi_gen_IDA_rwa[c(10,11),]
equi_gen_IDA_rwa$ROPE <- roperwaC

equi_gen_IDA_0.5 <- equi_test(ida_resp, rope = c(-0.5, 0.5))
equi_gen_IDA_0.5 <- equi_gen_IDA_0.5[9,]
equi_gen_IDA_0.5$ROPE <- "-0.5, 0.5"

equi_gen_IDA <- rbind(equi_gen_IDA_rwa, equi_gen_IDA_0.5)
equi_gen_IDA[,c(3:5)] <- round(equi_gen_IDA[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_gen_IDA <- equi_gen_IDA[ordrow,]

# export test
png("tables/rope/equi_gen_IDA.png", height=480, width=720)
p<-tableGrob(equi_gen_IDA)
grid.arrange(p)
dev.off()

#------------------------------------------------------------------------------------
# IDA valence = qualitative label (positive vs. negative)
#------------------------------------------------------------------------------------

# model
IDA_resp_qual <- brm(response ~ Valence * RWAscore + (1|ppt) + (1|stim1) + (1|XP),
                data = IDA, 
                family = cumulative (link = "logit", threshold = "flexible"),
                prior = priors,
                warmup = 1000, iter = 2000,
                chains = 4, cores = parallel::detectCores(),
                control = list(adapt_delta = 0.9999, max_treedepth = 10),
                sample_prior = TRUE)

IDA_resp_bckup01 <- IDA_resp
IDA_resp_bckup02 <- IDA_resp

# In case we want to save summary ! change the name if you export both summary and tidy_stan
# model_gen_IDA <- summary(ida_resp)$fixed[,col2keep]
# model_gen_IDA <- round(model_gen_IDA, 3)

# arrange output
model_gen_IDA_qual <- model_parameters(IDA_resp_qual,
                           typical = "mean",
                           prob = .89)


# export output
png("tables/brms/model_gen_IDA.png", height=480, width=720)
p<-tableGrob(model_gen_IDA)
grid.arrange(p)
dev.off()

# test with rope
equi_gen_IDA_rwa <- equi_test(ida_resp, rope = roperwaN)
equi_gen_IDA_rwa <- equi_gen_IDA_rwa[c(10,11),]
equi_gen_IDA_rwa$ROPE <- roperwaC

equi_gen_IDA_0.5 <- equi_test(ida_resp, rope = c(-0.5, 0.5))
equi_gen_IDA_0.5 <- equi_gen_IDA_0.5[9,]
equi_gen_IDA_0.5$ROPE <- "-0.5, 0.5"

equi_gen_IDA <- rbind(equi_gen_IDA_rwa, equi_gen_IDA_0.5)
equi_gen_IDA[,c(3:5)] <- round(equi_gen_IDA[,c(3:5)], 2)
ordrow <- c("3", "1", "2")
equi_gen_IDA <- equi_gen_IDA[ordrow,]

# export test
png("tables/rope/equi_gen_IDA.png", height=480, width=720)
p<-tableGrob(equi_gen_IDA)
grid.arrange(p)
dev.off()

