Updated_Tracking_Keys <- read_csv("/home/cujo253/Reports/All_Cards_MB1.csv", col_types = cols(`F/NF` = col_character()))
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")

setwd("/home/cujo253/Funny Money/")
currentDate <- Sys.Date()
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
CK_Market__Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$CK_MKT[match(CK_Market__Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  CK_Market__Tracker <- cbind(CK_Market__Tracker, New_Info[1])
}

CK_Market__Tracker[6:ncol(CK_Market__Tracker)] <- sapply(CK_Market__Tracker[6:ncol(CK_Market__Tracker)], as.character)
CK_Market__Tracker[6:ncol(CK_Market__Tracker)] <- sapply(CK_Market__Tracker[6:ncol(CK_Market__Tracker)], as.numeric)
#CK_Market__Tracker <- as.data.frame(CK_Market__Tracker)

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
TCG_Market_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$TCG_MKT[match(TCG_Market_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  TCG_Market_Tracker <- cbind(TCG_Market_Tracker, New_Info[1])
}

TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)] <- sapply(TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)], as.character)
TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)] <- sapply(TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)], as.numeric)
range = ncol(TCG_Market_Tracker)

CK_Retail_Comparison <- TCG_Market_Tracker[1:5]
for (i in 6:range){
CK_Retail_Comparison[i] <- round(CK_Market__Tracker[i] -  TCG_Market_Tracker[i],2)
}

#Binary COunt####
MBT <- CK_Retail_Comparison[,-c(1:5)]
MBT <- sapply(MBT,as.numeric)
New <- CK_Retail_Comparison$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}

New <- as.data.frame(New)
New[is.na(New)] <- 0
New <- New[-1]
New <- sapply(New, as.numeric)

Up_Down <- NULL
for (i in 1:nrow(New)){
  sumup <- round(sum(New[i,c(1:ncol(New))]),2)
  sumup <- as.data.frame(sumup)
  Up_Down <- rbind(Up_Down,sumup)
}

Up_Down_Three_Weeks <- NULL
for (i in 1:nrow(New)){
  sumup <- round(sum(New[i,c((ncol(New)-21):ncol(New))]),2)
  sumup <- as.data.frame(sumup)
  Up_Down_Three_Weeks <- rbind(Up_Down_Three_Weeks,sumup)
}

Up_Down_Seven_Days <- NULL
for (i in 1:nrow(New)){
  sumup <- round(sum(New[i,c((ncol(New)-7):ncol(New))]),2)
  sumup <- as.data.frame(sumup)
  Up_Down_Seven_Days <- rbind(Up_Down_Seven_Days,sumup)
}

CK_Retail_Comparison$All_Time <- Up_Down$sumup
CK_Retail_Comparison$Three_Weeks <- Up_Down_Three_Weeks$sumup
CK_Retail_Comparison$One_Week <- Up_Down_Seven_Days$sumup

CK_Retail_Comparison_AT <- CK_Retail_Comparison[order(CK_Retail_Comparison$All_Time),]
CK_Retail_Comparison_AT <- CK_Retail_Comparison_AT[c(1:200),]
CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison[order(CK_Retail_Comparison$Three_Weeks),]
CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison_Three_Weeks[c(1:200),]
CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison[order(CK_Retail_Comparison$One_Week),]
CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison_One_Weeks[c(1:200),]


AT_CK <- data.frame(CK_Retail_Comparison_AT[1:5],CK_Retail_Comparison_AT[ncol(CK_Retail_Comparison_AT)-2])
Three_Week_CK <- data.frame(CK_Retail_Comparison_Three_Weeks[1:5],CK_Retail_Comparison_Three_Weeks[ncol(CK_Retail_Comparison_Three_Weeks)-1])
One_Week_CK <- data.frame(CK_Retail_Comparison_One_Weeks[1:5],CK_Retail_Comparison_One_Weeks[ncol(CK_Retail_Comparison_One_Weeks)])

AT_CK$Rank <- seq(nrow(AT_CK))
Three_Week_CK$Rank <-seq(nrow(Three_Week_CK))
One_Week_CK$Rank <- seq(nrow(One_Week_CK))

AT_CK$CK_Retail <- CK_Market__Tracker$`2020-03-22`[match(AT_CK$Key,CK_Market__Tracker$Key)]
AT_CK$TCG_Retail <- TCG_Market_Tracker$`2020-03-22`[match(AT_CK$Key, TCG_Market_Tracker$Key)]

Three_Week_CK$CK_Retail<- CK_Market__Tracker$`2020-03-22`[match(Three_Week_CK$Key,CK_Market__Tracker$Key)]
Three_Week_CK$TCG_Retail<- TCG_Market_Tracker$`2020-03-22`[match(Three_Week_CK$Key, TCG_Market_Tracker$Key)]

One_Week_CK$CK_Retail<- CK_Market__Tracker$`2020-03-22`[match(One_Week_CK$Key,CK_Market__Tracker$Key)]
One_Week_CK$TCG_Retail<- TCG_Market_Tracker$`2020-03-22`[match(One_Week_CK$Key, TCG_Market_Tracker$Key)]

Mystery_Booster_Reprint <- Updated_Card_Roster[which(Updated_Card_Roster$Set == "Mystery Booster"),]
AT_CK$`MB1` <- Mystery_Booster_Reprint$Set[match(AT_CK$name,Mystery_Booster_Reprint$name)]
Three_Week_CK$`MB1`<- Mystery_Booster_Reprint$Set[match(Three_Week_CK$name,Mystery_Booster_Reprint$name)]
One_Week_CK$`MB1`<- Mystery_Booster_Reprint$Set[match(One_Week_CK$name,Mystery_Booster_Reprint$name)]
AT_CK$`MB1`[is.na(AT_CK$`MB1`) == T] <- ""
Three_Week_CK$`MB1`[is.na(Three_Week_CK$`MB1`) == T] <- ""
One_Week_CK$`MB1`[is.na(One_Week_CK$`MB1`)==T] <- ""

#Google Components####
library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
#library(googlesheets)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
ss <- drive_get("CK_VS_TCG_Review")

sheets_auth()
sheets_write(
  AT_CK,
  ss = ss,
  sheet = "3_Months"
)
sheets_write(
  Three_Week_CK,
  ss = ss,
  sheet = "3_Weeks"
)
sheets_write(
  One_Week_CK,
  ss = ss,
  sheet = "Prior_Week"
)

