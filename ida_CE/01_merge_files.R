if (!require("pacman")) install.packages("pacman")
p_load(dplyr, 
       stringi,
       data.table,
       reshape2,
       rlist,
       readr,
       install = TRUE,
       update = getOption("pac_update"),
       character.only = FALSE)

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(curwd)

XP03 <- read_csv("XP03.csv")
XP10 <- read_csv("XP10.csv")
XP10$XP <- "XP10"


IDA <- rbind(XP03,
             XP10)

XP03$usvalence_neg <- ifelse (XP03$usvalence == -0.5, 0, 1)
XP10$usvalence_neg <- ifelse (XP10$usvalence == -0.5, 0, 1)
IDA$usvalence_neg <- ifelse (IDA$usvalence == -0.5, 0, 1)

XP03$usvalence_pos <- ifelse (XP03$usvalence == 0.5, 0, 1)
XP10$usvalence_pos <- ifelse (XP10$usvalence == 0.5, 0, 1)
IDA$usvalence_pos <- ifelse (IDA$usvalence == 0.5, 0, 1)

IDA$Valence <- ifelse (IDA$usvalence == -0.5, "Negative", "Positive")

# Number of ppt

#length(unique(IDA$ppt))

#length(unique(XP03$ppt)) + length(unique(XP10$ppt))
