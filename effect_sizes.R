source('~/Documents/RStudioFolders/rwa_attitude_change/Bayes_R2_MZ_function.R', echo=TRUE)

## R2 for evaluative conditioning

# Full model
r2_full <- Bayes_R2_MZ(IDA_resp)

# Without interaction
r2_full_woint <- Bayes_R2_MZ(IDA_resp_woint)

#R2 of the interaction (parameter fo interest)
r2_interact <- round(r2_full[1]-r2_full_woint[1], 3)
r2_interact

## d of the interaction (parameter fo interest)
d_interact <- round(model_parameters(IDA_resp) [11, 2] / (pi/sqrt(3)), 2)
d_interact

#---
                
## R2 for evaluative counter-conditioning

# Full model
r2cc_full <- Bayes_R2_MZ(IDAcc_resp)

# Without interaction
r2cc_full_woint <- Bayes_R2_MZ(IDAcc_resp_woint)

#R2 of the interaction (parameter fo interest)
r2cc_interact <- round(r2cc_full[1]-r2cc_full_woint[1], 3)
r2cc_interact

##d of the interaction (parameter fo interest)
dcc_interact <- round(model_parameters(IDAcc_resp) [11, 2] / (pi/sqrt(3)), 2)
dcc_interact

## check
(model_parameters(IDA_resp) [11, 2] / (pi/sqrt(3))/(sqrt(model_parameters(IDA_resp) [11, 2] / (pi/sqrt(3))^2+4)))^2
r2_full[1]-r2_full_woint[1]


(model_parameters(IDAcc_resp) [11, 2] / (pi/sqrt(3))/(sqrt(model_parameters(IDAcc_resp) [11, 2] / (pi/sqrt(3))^2+4)))^2
r2cc_full[1]-r2cc_full_woint[1]
