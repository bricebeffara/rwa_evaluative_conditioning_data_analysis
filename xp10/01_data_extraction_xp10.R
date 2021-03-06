###########
#### RWA & spreading of attitudes
###########

# Data preparation --------------------------------------------------------

# Optional generic preliminaries:

#graphics.off() # This closes all of R's graphics windows.
#rm(list=ls())  # Careful! This clears all of R's memory!

## install, load and update necessary packages if required##

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

# We get the current working directory and it will be used to load the data

curwd <- dirname(rstudioapi::getActiveDocumentContext()$path)

# set the working directory to the folder containing the data

setwd(paste(curwd,"/raw_data/",sep=""))

#-----------------------------------
## We did this only once for pseudonymisation
## Not necessary to run this
## uncomment if you want to do it anyway

# create a list with files names contained in the folder
#files <- list.files()

# read these files
#files_read <- lapply(files, function(i){read.csv(i, header=FALSE, stringsAsFactors=F)})

# pseudonymisation of id and ip
#pseudon = function(x) {
#  x[2,c(8,10)] <- "deleted for pseudonymisation"
#  return(x)
#}
#files_read <- lapply(files_read, pseudon)

# remove original files
#file.remove(files)

# create new ones with random name and makes id/ip
#lapply(files_read, function (x) write.table(x, file=paste(stri_rand_strings(1, 12, '[A-Z]'), "csv", sep=".") ) )
#-----------------------------------

# create a list with files names contained in the folder
files <- list.files()

create_dfs <- function(x) {
  out <- read.table(x, skip = 3, header = TRUE)
  out <- out[,-1]
  names <- gsub(".csv", "", x)
  cbind(ppt=names, out)
}

all_raw <- lapply(files, create_dfs)

dat_all <- rbindlist(all_raw)

setwd("../")
write.csv(dat_all,file="table_all_data.csv", row.names=F)

##Keep relevant columns
df <- data.table(select(dat_all, ppt, subjectGroup, condition1, stim1, stim2, response, RT,stimFormat, trialNo))

##Recode families of Greebles
df$family<-df$stim1
df[family %in% c("M1","M2", "M3","M4","M5","M6","M7","M8"), family := "M"]
df[family %in% c("B1","B2", "B3","B4","B5","B6","B7","B8"), family := "B"]
df[family %in% c("O1","O2", "O3","O4","O5","O6","O7","O8"), family := "O"]
df[family %in% c("P1","P2", "P3","P4","P5","P6","P7","P8"), family := "P"]

## Recode US valence: 
# Subject group 1: BP - MO / Bpos - Mneg
# Subject group 2: BP - MO / Bneg - Mpos
# Subject group 3: BO - MP / Bpos - Mneg
# Subject group 4: BO - MP / Bneg - Mpos

df$usvalence[df$subjectGroup == "1" & df$family == "B"] <- 0.5
df$usvalence[df$subjectGroup == "2" & df$family == "B"] <- -0.5
df$usvalence[df$subjectGroup == "3" & df$family == "B"] <- 0.5
df$usvalence[df$subjectGroup == "4" & df$family == "B"] <- -0.5
df$usvalence[df$subjectGroup == "1" & df$family == "M"] <- -0.5
df$usvalence[df$subjectGroup == "2" & df$family == "M"] <- 0.5
df$usvalence[df$subjectGroup == "3" & df$family == "M"] <- -0.5
df$usvalence[df$subjectGroup == "4" & df$family == "M"] <- 0.5

df$usvalence[df$subjectGroup == "1" & df$family == "O"] <- -0.5
df$usvalence[df$subjectGroup == "2" & df$family == "O"] <- 0.5
df$usvalence[df$subjectGroup == "3" & df$family == "O"] <- 0.5
df$usvalence[df$subjectGroup == "4" & df$family == "O"] <- -0.5
df$usvalence[df$subjectGroup == "1" & df$family == "P"] <- 0.5
df$usvalence[df$subjectGroup == "2" & df$family == "P"] <- -0.5
df$usvalence[df$subjectGroup == "3" & df$family == "P"] <- -0.5
df$usvalence[df$subjectGroup == "4" & df$family == "P"] <- 0.5

##Recode for actual pairing vs. spreading condition


df[family %in% c("O","P"), spreading := "1"] #level 2
df[family %in% c("B","M"), spreading := "-1"] #level 1

###### Create RWA levels dataframe ######

RWA <- filter(df, condition1 == "RWA")

##Long to wide dataframe
RWA <- data.table(dcast(RWA, ppt ~ trialNo, value.var="response"))
colnames(RWA) <- paste("RWA", colnames(RWA), sep = "_")
colnames(RWA)[1] <- "ppt"

#Transform variables to numeric format
RWA[, (2:16) := lapply(.SD, as.numeric), .SDcols = (2:16)]

#reversed items. RWA items 2, 4, 6, 10, 12, 14.
RWA<- within(RWA,{
  RWA2R<- 8-RWA_2
  RWA4R<- 8-RWA_4
  RWA6R<- 8-RWA_6
  RWA10R<- 8-RWA_10
  RWA8R<-8-RWA_8
  RWA12R<- 8-RWA_12
  RWA14R<- 8-RWA_14
})

#RWA score
RWA<- within(RWA,{
  RWAscore <- rowSums(RWA[, c("RWA_1","RWA10R","RWA_11","RWA12R","RWA_13","RWA14R","RWA_15","RWA2R","RWA_3","RWA4R", "RWA_5","RWA6R","RWA_7","RWA8R","RWA_9")], na.rm=TRUE)
})

#Check data & See data

#boxplot(RWA$RWAscore)
#hist(RWA$RWAscore)

quart1 <- quantile(RWA$RWAscore)[2]
quart3 <- quantile(RWA$RWAscore)[4]
iqr <- IQR(RWA$RWAscore)

#which(RWA$RWAscore<quart1-3*iqr)
#which(RWA$RWAscore>quart3+3*iqr)

# Remove highly extreme values of RWA
RWA$RWAscore <-  ifelse (RWA$RWAscore<quart1-3*iqr | RWA$RWAscore>quart3+3*iqr, NA, RWA$RWAscore)
RWA <- RWA[which(!is.na(RWA$RWAscore)),]

# scale RWA score
RWA$RWAscore<-scale(RWA$RWAscore, center = TRUE, scale = TRUE)


###### Create evaluative ratings dataframe ######

ratings <- df[condition1 %in% c("ratings1","ratings2","ratings3","ratings4"), ]


##### create learning accuracy data frame

learning <- filter(df, condition1 =="apprentissage")
learning <- data.table(learning[!is.na(learning$spreading),])

## Code correct response

learning[family %in% "B", acc := "1"]
learning[family %in% "M", acc := "2"]
learning[family %in% "O", acc := "3"]
learning[family %in% "P", acc := "4"]

learning[, learing_acc := as.numeric(response) - as.numeric(acc)]
learning[learing_acc %in% "0", accuracy := 1]
learning[learing_acc %in% c("-3","-2", "-1","1","2","3"), accuracy := 0]

###### Creare memory performance dataframe

mem <- df[condition1 %in% c("memory1", "memory2", "memory3", "memory4"),]

# Correct responses (P = 3 / O = 1)

# Subject group 1: BP - MO 
# Subject group 2: BP - MO 
# Subject group 3: BO - MP 
# Subject group 4: BO - MP 

mem[subjectGroup %in% c("1","2") & family %in% "B", memory := "3"]
mem[subjectGroup %in% c("1","2") & family %in% "M", memory := "1"]
mem[subjectGroup %in% c("3","4") & family %in% "B", memory := "1"]
mem[subjectGroup %in% c("3","4") & family %in% "M", memory := "3"]

# Accuracy (actual vs. correct responses)

mem[, accuracy := as.numeric(memory) - as.numeric(response)]
mem[, accuracy := ifelse(accuracy == "0", 1, 0) ]
mem <- select(mem, ppt, stim1, accuracy)

###### Merge RWA with ratings dataframe ######

spreading<-full_join(ratings, RWA)
spreading<-full_join(spreading, mem)

## recoding & transforming for anaylisis

spreading$spreading <- ifelse (spreading$spreading == "1", 0.5, -0.5)
spreading$response <- as.numeric(as.character(spreading$response))

spreading$spr <- spreading$spreading

#for IDA CE

setwd("../ida_CE")
XP10 <- filter(spreading, spr == -0.5)
col2k <- c("ppt", "stim1", "RWAscore", "usvalence", "response")
XP10 <- XP10[,col2k]

write.csv(XP10,file="XP10.csv", row.names=F)
