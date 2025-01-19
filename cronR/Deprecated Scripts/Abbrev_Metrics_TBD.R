source("config.R")
install.packages("jsonlite")  # JSON manipulation
install.packages("tidyverse") # Data Manipulation
install.packages("reshape2") 
install.packages("dplyr")
install.packages("knitr")     # Pretty HTML Tables    # Breakdown further elements
install.packages("dplyr")     # Data Manipulation
install.packages("tidyr")     # The Janitor is this guy
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}
library(readr)
library(tibble)    # Breakdown further elements
library(dplyr)     # Data Manipulation
library(tidyr)
#Refresh KPI DFs####
currentDate <- Sys.Date()
YesterdayDate <- Sys.Date()-1
Yesterdays_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"BuyList_History.csv", sep ="")
Yesterdays_Vendor_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"Vendor_History.csv", sep ="")
Yesterdays_TCG_Ranks <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"TCG_History.csv", sep ="")
Yesterdays_CK_Ranks <- paste("/home/cujo253/Metrics/TBD Updated Roster/",YesterdayDate,"CK_History.csv", sep ="")

Buylist_Tracker <- read_csv(Yesterdays_Buylist_Tracker, col_types = cols(Foil = col_character()))
Vendor_Tracker <- read_csv(Yesterdays_Vendor_Tracker,col_types = cols(.default = "c"))
Vendor_Tracker[6:ncol(Vendor_Tracker)] <- lapply(Vendor_Tracker[6:ncol(Vendor_Tracker)], factor)
TCG_Ranks <- read_csv(Yesterdays_TCG_Ranks,col_types = cols(Foil = col_character()))
CK_Ranks <- read_csv(Yesterdays_CK_Ranks,col_types = cols(Foil = col_character()))


#Data from Spiders to individual Trackers####
TodaysPremium <- paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv", sep="")

Data <- read_csv(TodaysPremium,col_types = cols(`F/NF` = col_character(), Sellers = col_character(), TCG_Rank = col_character(),BL_QTY = col_character()))

#Updates####
Buylist_Tracker$V1 <- Data$BL[match(Buylist_Tracker$Key,Data$Key)]
Vendor_Tracker$V1 <- Data$Sellers[match(Buylist_Tracker$Key,Data$Key)]
TCG_Ranks$V1 <- Data$TCG_Rank[match(Buylist_Tracker$Key,Data$Key)]
CK_Ranks$V1 <- Data$CK_ADJ_Rank[match(Buylist_Tracker$Key,Data$Key)]

formatted_date <- format(currentDate, format = "%Y-%m-%d")

names(Buylist_Tracker)[ncol(Buylist_Tracker)] <- formatted_date
names(Vendor_Tracker)[ncol(Vendor_Tracker)] <- formatted_date
names(TCG_Ranks)[ncol(TCG_Ranks)] <- formatted_date
names(CK_Ranks)[ncol(CK_Ranks)] <- formatted_date


Buylist_Tracker[is.na(Buylist_Tracker)] <- ""
Vendor_Tracker[is.na(Vendor_Tracker)] <- ""
TCG_Ranks[is.na(TCG_Ranks)] <- ""
CK_Ranks[is.na(CK_Ranks)] <- ""


Buylist_Tracker$Foil <- ifelse(Buylist_Tracker$Foil == "FOIL", Buylist_Tracker$Foil, "")
Vendor_Tracker$Foil <- ifelse(Vendor_Tracker$Foil == "FOIL", Vendor_Tracker$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "FOIL", TCG_Ranks$Foil, "")
CK_Ranks$Foil <- ifelse(CK_Ranks$Foil == "FOIL", CK_Ranks$Foil, "")


#Exports####
setwd("/home/cujo253/Metrics/TBD Updated Roster")
csvFileName <- paste(currentDate,"BuyList_History",".csv",sep="")
write.csv(Buylist_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"Vendor_History",".csv",sep="")
write.csv(Vendor_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"TCG_History",".csv",sep="")
write.csv(TCG_Ranks, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"CK_History",".csv",sep="")
write.csv(CK_Ranks, file=csvFileName, row.names = FALSE) 

#Buy List Binary Conversion####
MBT <- Buylist_Tracker[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
Sys.sleep(sample(1:2, 1))
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,1,ifelse(New[,c(2:ncol(New))]<0,-1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key
#csvFileName <- paste(currentDate,"Binary_Form",".csv",sep="")
#write.csv(Binary_Form, file=csvFileName, row.names = FALSE)
#Binary_Form$Name <- Buylist_Tracker$`Card Name`
#View(Binary_Form)
#sumup <- sum(Binary_Form[1,c((ncol(Binary_Form)-6):(ncol(Binary_Form)-1))])
#15 Days
Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
SPS_21 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-21):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_21 <- rbind(SPS_21,sumup)
}

SPS_21 <- as.data.frame(SPS_21)
summary(SPS_21$sumup)
Sys.sleep(sample(1:3, 1))
#10 Days
SPS_7 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-7):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_7 <- rbind(SPS_7,sumup)
}
SPS_7 <- as.data.frame(SPS_7)
summary(SPS_7$sumup)
Sys.sleep(sample(1:3, 1))
#5 Days
SPS_15 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-15):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_15 <- rbind(SPS_15,sumup)
}
SPS_15 <- as.data.frame(SPS_15)
Sys.sleep(sample(1:3, 1))
summary(SPS_15$sumup)
nrow(SPS_21)
nrow(SPS_15)
nrow(SPS_7)
bl_metrics <- data.frame(SPS_21$sumup,SPS_15$sumup,SPS_7$sumup)
bl_metrics$Key <- Buylist_Tracker$Key
bl_metrics <- bl_metrics[moveme(names(bl_metrics), "Key first")]
#bl_metrics$Key <- factor(bl_metrics$Key)
#bl_metrics$SPS_21.sumup <- factor(bl_metrics$SPS_21.sumup)
#bl_metrics$SPS_15.sumup <- factor(bl_metrics$SPS_15.sumup)
#bl_metrics$SPS_7.sumup <- factor(bl_metrics$SPS_7.sumup)
tbl_metrics <- t(bl_metrics)
tbl_metrics <- as.data.frame(tbl_metrics)
#tbl_metrics <- sapply(tbl_metrics, as.factor)
tbl_metrics <- as.data.frame(tbl_metrics)
tbl_numbers <- (tbl_metrics[-1,])
tbl_numbers <- lapply(tbl_numbers,as.character)
tbl_numbers <- lapply(tbl_numbers,as.numeric)
tbl_numbers <- data.frame(tbl_numbers)
#tbl_numbers <- as.numeric.factor(tbl_metrics[-1,])
#tbl_numbers <- as.data.frame(tbl_numbers)
#tbl_numbers <- lapply(tbl_numbers,as.numeric)
#tbl_numbers <- as.data.frame(tbl_numbers)
why <- NULL
for (i in 1:ncol(tbl_numbers)){
  sumup <- sum(tbl_numbers[i])
  sumup <- as.data.frame(sumup)
  why <- rbind(why,sumup)
}
Sys.sleep(sample(1:5, 1))
BL_Final <- data.frame(Buylist_Tracker[,1:5],bl_metrics$SPS_21.sumup,bl_metrics$SPS_15.sumup,bl_metrics$SPS_7.sumup,why)
colnames(BL_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
BL_Final <- BL_Final[order(-BL_Final$Rank_Sums),]
BL_Final_M <- BL_Final[which(BL_Final$Rarity == "M"),]
BL_Final_R <- BL_Final[which(BL_Final$Rarity == "R"),]
BL_Final_U <- BL_Final[which(BL_Final$Rarity == "U"),]
BL_Final_C <- BL_Final[which(BL_Final$Rarity == "C"),]

#Google Sheets for Buy List####
# library(devtools)
# devtools::install_github("tidyverse/googlesheets4")
# library(googlesheets4)
# library(googledrive)
# library(gargle)
# library(httr)
# options(httr_oob_default=TRUE)
# options(gargle_oauth_email = "pachun95@gmail.com")
# drive_auth(email = "pachun95@gmail.com")
# drive_auth()
# 
# currentDate <- Sys.Date()
# my_dfs <- list(BL_Final_M,BL_Final_R,BL_Final_U,BL_Final_C)
# sheets_create(
#   paste(currentDate,"_BL_Rankings_Review",sep=""),
#   sheets = my_dfs
# )
Sys.sleep(15)
#Vendor Trends####
MBT <-Vendor_Tracker[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
Sys.sleep(sample(5:15, 1))
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key
#csvFileName <- paste(currentDate,"Binary_Form",".csv",sep="")
#write.csv(Binary_Form, file=csvFileName, row.names = FALSE)
Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
SPS_21 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-21):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_21 <- rbind(SPS_21,sumup)
}

SPS_21 <- as.data.frame(SPS_21)
Sys.sleep(sample(1:3, 1))
SPS_7 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-7):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_7 <- rbind(SPS_7,sumup)
}
SPS_7 <- as.data.frame(SPS_7)
summary(SPS_7$sumup)
Sys.sleep(sample(1:3, 1))
#5 Days
SPS_15 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-15):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_15 <- rbind(SPS_15,sumup)
}
SPS_15 <- as.data.frame(SPS_15)
Sys.sleep(sample(1:3, 1))
summary(SPS_15$sumup)
nrow(SPS_21)
nrow(SPS_15)
nrow(SPS_7)
ven_metrics <- data.frame(SPS_21$sumup,SPS_15$sumup,SPS_7$sumup)
ven_metrics$Key <- Vendor_Tracker$Key
ven_metrics <- ven_metrics[moveme(names(ven_metrics), "Key first")]
tven_metrics <- t(ven_metrics)
tven_metrics <- as.data.frame(tven_metrics)
tven_metrics <- as.data.frame(tven_metrics)
tven_numbers <- (tven_metrics[-1,])
tven_numbers <- lapply(tven_numbers,as.character)
tven_numbers <- lapply(tven_numbers,as.numeric)
tven_numbers <- data.frame(tven_numbers)

why <- NULL
for (i in 1:ncol(tven_numbers)){
  sumup <- sum(tven_numbers[i])
  sumup <- as.data.frame(sumup)
  why <- rbind(why,sumup)
}
Sys.sleep(sample(5:15, 1))
VEN_Final <- data.frame(Vendor_Tracker[,1:5],ven_metrics$SPS_21.sumup,ven_metrics$SPS_15.sumup,ven_metrics$SPS_7.sumup,why)
colnames(VEN_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
VEN_Final <- VEN_Final[order(-VEN_Final$Rank_Sums),]
VEN_Final_M <- VEN_Final[which(VEN_Final$Rarity == "M"),]
VEN_Final_R <- VEN_Final[which(VEN_Final$Rarity == "R"),]
VEN_Final_U <- VEN_Final[which(VEN_Final$Rarity == "U"),]
VEN_Final_C <- VEN_Final[which(VEN_Final$Rarity == "C"),]
#Goolge Sheets - Vendors####
# options(httr_oob_default=TRUE)
# options(gargle_oauth_email = "pachun95@gmail.com")
# drive_auth(email = "pachun95@gmail.com")
# drive_auth()
# 
# 
# my_dfs <- list(VEN_Final_M,VEN_Final_R,VEN_Final_U,VEN_Final_C)
# sheets_create(
#   paste(currentDate,"_VEN_Rankings_Review",sep=""),
#   sheets = my_dfs
# )


#TCG DEMAND Binary Conversion####
MBT <- TCG_Ranks[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
Sys.sleep(sample(5:15, 1))
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key
csvFileName <- paste(currentDate,"Binary_Form",".csv",sep="")
write.csv(Binary_Form, file=csvFileName, row.names = FALSE)
#Binary_Form$Name <- Buylist_Tracker$`Card Name`
#View(Binary_Form)
#sumup <- sum(Binary_Form[1,c((ncol(Binary_Form)-6):(ncol(Binary_Form)-1))])
#15 Days
Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
SPS_21 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-21):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_21 <- rbind(SPS_21,sumup)
}

SPS_21 <- as.data.frame(SPS_21)
summary(SPS_21$sumup)
Sys.sleep(sample(5:15, 1))
#10 Days
SPS_7 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-7):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_7 <- rbind(SPS_7,sumup)
}
SPS_7 <- as.data.frame(SPS_7)
summary(SPS_7$sumup)
Sys.sleep(sample(5:15, 1))
#5 Days
SPS_15 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-15):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_15 <- rbind(SPS_15,sumup)
}
SPS_15 <- as.data.frame(SPS_15)
summary(SPS_15$sumup)
Sys.sleep(sample(5:15, 1))
nrow(SPS_21)
nrow(SPS_15)
nrow(SPS_7)
tcg_metrics <- data.frame(SPS_21$sumup,SPS_15$sumup,SPS_7$sumup)
tcg_metrics$Key <- TCG_Ranks$Key
tcg_metrics <- bl_metrics[moveme(names(bl_metrics), "Key first")]
#bl_metrics$Key <- factor(bl_metrics$Key)
#bl_metrics$SPS_21.sumup <- factor(bl_metrics$SPS_21.sumup)
#bl_metrics$SPS_15.sumup <- factor(bl_metrics$SPS_15.sumup)
#bl_metrics$SPS_7.sumup <- factor(bl_metrics$SPS_7.sumup)
ttcg_metrics <- t(bl_metrics)
ttcg_metrics <- as.data.frame(ttcg_metrics)
#ttcg_metrics <- sapply(ttcg_metrics, as.factor)
ttcg_metrics <- as.data.frame(ttcg_metrics)
ttcg_numbers <- (ttcg_metrics[-1,])
ttcg_numbers <- lapply(ttcg_numbers,as.character)
ttcg_numbers <- lapply(ttcg_numbers,as.numeric)
ttcg_numbers <- data.frame(ttcg_numbers)
#tbl_numbers <- as.numeric.factor(tbl_metrics[-1,])
#tbl_numbers <- as.data.frame(tbl_numbers)
#tbl_numbers <- lapply(tbl_numbers,as.numeric)
#tbl_numbers <- as.data.frame(tbl_numbers)
why <- NULL
for (i in 1:ncol(ttcg_numbers)){
  sumup <- sum(ttcg_numbers[i])
  sumup <- as.data.frame(sumup)
  why <- rbind(why,sumup)
}
Sys.sleep(sample(5:15, 1))
TCG_Final <- data.frame(TCG_Ranks[,1:5],tcg_metrics$SPS_21.sumup,tcg_metrics$SPS_15.sumup,tcg_metrics$SPS_7.sumup,why)
colnames(TCG_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
TCG_Final <- TCG_Final[order(-TCG_Final$Rank_Sums),]
TCG_Final_M <- TCG_Final[which(TCG_Final$Rarity == "M"),]
TCG_Final_R <- TCG_Final[which(TCG_Final$Rarity == "R"),]
TCG_Final_U <- TCG_Final[which(TCG_Final$Rarity == "U"),]
TCG_Final_C <- TCG_Final[which(TCG_Final$Rarity == "C"),]

#Google Sheets for TCG Demand####
# library(devtools)
# devtools::install_github("tidyverse/googlesheets4")
# library(googlesheets4)
# library(googledrive)
# library(gargle)
# library(httr)
# options(httr_oob_default=TRUE)
# options(gargle_oauth_email = "pachun95@gmail.com")
# drive_auth(email = "pachun95@gmail.com")
# drive_auth()
# 
# currentDate <- Sys.Date()
# my_dfs <- list(TCG_Final_M,TCG_Final_R,TCG_Final_U,TCG_Final_C)
# sheets_create(
#   paste(currentDate,"_TCG_Rankings_Review",sep=""),
#   sheets = my_dfs
# )
Sys.sleep(15)
#CK DEMAND Binary Conversion####
MBT <-  CK_Ranks[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key

Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
SPS_21 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-21):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_21 <- rbind(SPS_21,sumup)
}

SPS_21 <- as.data.frame(SPS_21)
summary(SPS_21$sumup)
Sys.sleep(sample(5:15, 1))

SPS_7 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-7):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_7 <- rbind(SPS_7,sumup)
}
SPS_7 <- as.data.frame(SPS_7)
summary(SPS_7$sumup)
Sys.sleep(sample(5:15, 1))
SPS_15 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-15):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_15 <- rbind(SPS_15,sumup)
}
SPS_15 <- as.data.frame(SPS_15)
summary(SPS_15$sumup)
Sys.sleep(sample(5:15, 1))
nrow(SPS_21)
nrow(SPS_15)
nrow(SPS_7)
ck_metrics <- data.frame(SPS_21$sumup,SPS_15$sumup,SPS_7$sumup)
ck_metrics$Key <- CK_Ranks$Key
ck_metrics <- ck_metrics[moveme(names(ck_metrics), "Key first")]
#bl_metrics$Key <- factor(bl_metrics$Key)
#bl_metrics$SPS_21.sumup <- factor(bl_metrics$SPS_21.sumup)
#bl_metrics$SPS_15.sumup <- factor(bl_metrics$SPS_15.sumup)
#bl_metrics$SPS_7.sumup <- factor(bl_metrics$SPS_7.sumup)
tck_metrics <- t(ck_metrics)
tck_metrics <- as.data.frame(tck_metrics)
#ttcg_metrics <- sapply(ttcg_metrics, as.factor)
tck_metrics <- as.data.frame(tck_metrics)
tck_numbers <- (tck_metrics[-1,])
tck_numbers <- lapply(tck_numbers,as.character)
tck_numbers <- lapply(tck_numbers,as.numeric)
tck_numbers <- data.frame(tck_numbers)
#tbl_numbers <- as.numeric.factor(tbl_metrics[-1,])
#tbl_numbers <- as.data.frame(tbl_numbers)
#tbl_numbers <- lapply(tbl_numbers,as.numeric)
#tbl_numbers <- as.data.frame(tbl_numbers)
why <- NULL
for (i in 1:ncol(tck_numbers)){
  sumup <- sum(tck_numbers[i])
  sumup <- as.data.frame(sumup)
  why <- rbind(why,sumup)
}
Sys.sleep(sample(5:15, 1))
CK_Final <- data.frame(CK_Ranks[,1:5],ck_metrics$SPS_21.sumup,ck_metrics$SPS_15.sumup,ck_metrics$SPS_7.sumup,why)
colnames(CK_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
CK_Final <- CK_Final[order(-CK_Final$Rank_Sums),]
CK_Final_M <- CK_Final[which(CK_Final$Rarity == "M"),]
CK_Final_R <- CK_Final[which(CK_Final$Rarity == "R"),]
CK_Final_U <- CK_Final[which(CK_Final$Rarity == "U"),]
CK_Final_C <- CK_Final[which(CK_Final$Rarity == "C"),]

#Google Sheets for CK Demand####
# library(devtools)
# devtools::install_github("tidyverse/googlesheets4")
# library(googlesheets4)
# library(googledrive)
# library(gargle)
# library(httr)
# options(httr_oob_default=TRUE)
# options(gargle_oauth_email = "pachun95@gmail.com")
# drive_auth(email = "pachun95@gmail.com")
# drive_auth()
# 
# currentDate <- Sys.Date()
# my_dfs <- list(CK_Final_M,CK_Final_R,CK_Final_U,CK_Final_C)
# sheets_create(
#   paste(currentDate,"_CK_Rankings_Review",sep=""),
#   sheets = my_dfs
# )
Sys.sleep(15)
#Aggr List####
BL_Final$Rank_Groups <- as.numeric(as.factor(BL_Final$Rank_Sums))
VEN_Final$Rank_Groups <- as.numeric(as.factor(VEN_Final$Rank_Sums))
TCG_Final$Rank_Groups <- as.numeric(as.factor(TCG_Final$Rank_Sums))
CK_Final$Rank_Groups <- as.numeric(as.factor(CK_Final$Rank_Sums))

BL_Upper_Esch <- BL_Final[which(BL_Final$Rank_Groups >= (max(BL_Final$Rank_Groups)-9)),]
VEN_Upper_Esch <- VEN_Final[which(VEN_Final$Rank_Groups >= (max(VEN_Final$Rank_Groups)-9)),]
TCG_Upper_Esch <- TCG_Final[which(TCG_Final$Rank_Groups >= (max(TCG_Final$Rank_Groups)-9)),]
CK_Upper_Esch <- CK_Final[which(CK_Final$Rank_Groups >= (max(CK_Final$Rank_Groups)-9)),]

Combined_Upper_Esch <- rbind(BL_Upper_Esch[,1:5], VEN_Upper_Esch[,1:5], TCG_Upper_Esch[,1:5], CK_Upper_Esch[,1:5])
Combined_Upper_Esch[,5][is.na(Combined_Upper_Esch[,5])] <- ""
Unique_Combined_Upper_Esch <- unique(Combined_Upper_Esch)
#View(Unique_Combined_Upper_Esch)
nrow(Combined_Upper_Esch)
nrow(Unique_Combined_Upper_Esch)

CUE <- NULL
BLUE <- NULL
VENUE <- NULL
TCGUE <- NULL
CKUE <- NULL
library(dplyr)
CUE <- Combined_Upper_Esch %>% group_by(Key) %>% add_tally()
BLUE <- BL_Upper_Esch %>% group_by(Key) %>% add_tally()
VENUE <- VEN_Upper_Esch %>% group_by(Key) %>% add_tally()
TCGUE <- TCG_Upper_Esch %>% group_by(Key) %>% add_tally()
CKUE <- CK_Upper_Esch %>% group_by(Key) %>% add_tally()

Unique_Combined_Upper_Esch$Total_KPI_CT <- CUE$n[match(Unique_Combined_Upper_Esch$Key,CUE$Key)]
Unique_Combined_Upper_Esch$BL_KPI <- BLUE$n[match(Unique_Combined_Upper_Esch$Key,BLUE$Key)]
Unique_Combined_Upper_Esch$VEN_KPI <- VENUE$n[match(Unique_Combined_Upper_Esch$Key,VENUE$Key)]
Unique_Combined_Upper_Esch$TCG_KPI <- TCGUE$n[match(Unique_Combined_Upper_Esch$Key,TCGUE$Key)]
Unique_Combined_Upper_Esch$CK_KPI <- CKUE$n[match(Unique_Combined_Upper_Esch$Key,CKUE$Key)]

Unique_Combined_Upper_Esch$BL_Bracket <- BL_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,BL_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$VEN_Bracket <- VEN_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,VEN_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$TCG_Bracket <- TCG_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,TCG_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$CK_Bracket <- CK_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,CK_Upper_Esch$Key)]

Unique_Combined_Upper_Esch$BL_Bracket <- ifelse(Unique_Combined_Upper_Esch$BL_Bracket == max(BL_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$VEN_Bracket <- ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == max(VEN_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$TCG_Bracket <- ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == max(TCG_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$CK_Bracket <- ifelse(Unique_Combined_Upper_Esch$CK_Bracket == max(CK_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-10),10,""))))))))))

Buylist_Tiers <- max(BL_Final$Rank_Groups)
Vendor_Tiers <- max(VEN_Final$Rank_Groups)
TCG_Demand_Tiers <- max(TCG_Final$Rank_Groups)
CK_Demand_Tiers <- max(CK_Final$Rank_Groups)


Unique_Combined_Upper_Esch[,c(11:14)][is.na(Unique_Combined_Upper_Esch[c(11:14)])] <- 10
ncol(Unique_Combined_Upper_Esch)
Unique_Combined_Upper_Esch[is.na(Unique_Combined_Upper_Esch)] <- ""
library(magrittr)
Unique_Combined_Upper_Esch$WMS <-   Unique_Combined_Upper_Esch[,c(11:14)] %>% 
  rowwise() %>% # compute for each row
  do(data.frame(
    WMS=weighted.mean(
      x=c(as.numeric(.$BL_Bracket),as.numeric(.$VEN_Bracket),as.numeric(.$TCG_Bracket),as.numeric(.$CK_Bracket)),
      w=c(.35,.4,.22,.03)
    )
  )
  ) %>% 
  ungroup() %>% # undo row groups
  use_series("WMS")

Unique_Combined_Upper_Esch <- Unique_Combined_Upper_Esch[order(Unique_Combined_Upper_Esch$WMS),]
Unique_Combined_Upper_Esch$Ranking <- seq.int(nrow(Unique_Combined_Upper_Esch))
OVR_KPI_DF <- data.frame(Unique_Combined_Upper_Esch[,1:5],Unique_Combined_Upper_Esch[,16])
colnames(OVR_KPI_DF) <- c("Key","Name","Set","Rarity","F/NF","Ranking")
OVR_KPI_DF$Retail <- Data$MKT[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF$Buylist <- Data$BL[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF$Vendors <- Data$Sellers[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF[is.na(OVR_KPI_DF)] <- ""
M_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "M"),]
R_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "R"),]
U_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "U"),]
C_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "C"),]
M_KPI$Ranking <- seq.int(nrow(M_KPI))
R_KPI$Ranking <- seq.int(nrow(R_KPI))
U_KPI$Ranking <- seq.int(nrow(U_KPI))
C_KPI$Ranking <- seq.int(nrow(C_KPI))
nrow(M_KPI)
nrow(R_KPI)
nrow(U_KPI)
nrow(C_KPI)

View(OVR_KPI_DF)
#Export Master KPI Report####
setwd("/home/cujo253/Reports/KPI/Master")
csvFileName <- paste(currentDate,"_Master_KPI",".csv",sep="")
write.csv(OVR_KPI_DF, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Mythic")
csvFileName <- paste(currentDate,"_Mythic_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Rare")
csvFileName <- paste(currentDate,"_Rare_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Uncommon")
csvFileName <- paste(currentDate,"_Uncommon_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Common")
csvFileName <- paste(currentDate,"_Common_KPI",".csv",sep="")
write.csv(C_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/")


library(devtools)
#install.packages("googlesheets4")
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
drive_deauth()
sheets_auth(token = drive_token())
options(gargle_oauth_email = "pachun95@gmail.com",
        gargle_oauth_cache = "/home/cujo253/cronR/")

drive_auth()
MKPI <- list(OVR_KPI_DF,M_KPI,R_KPI,U_KPI,C_KPI)
sheets_auth()
sheets_create(
  paste(currentDate,"_Master_KPI_Review",sep=""),
  sheets = MKPI
)


