source("config.R")
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
View(TCG_Market_Tracker)
range = ncol(TCG_Market_Tracker)

CK_Retail_Comparison <- TCG_Market_Tracker[1:5]
for (i in 6:range){
CK_Retail_Comparison[i] <- round(CK_Market__Tracker[i] -  TCG_Market_Tracker[i],2)
}
View(CK_Retail_Comparison)
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
View(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
Binary_Form <- as.data.frame(Binary_Form)
#All-time
AllTime <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c(1:ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  AllTime <- rbind(AllTime,sumup)
}

AllTime <- as.data.frame(AllTime)

#Three weeks
SPS_21 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-21):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_21 <- rbind(SPS_21,sumup)
}

SPS_21 <- as.data.frame(SPS_21)

SPS_7 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-7):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_7 <- rbind(SPS_7,sumup)
}

Mkt_metrics <- data.frame(AllTime$sumup,SPS_21$sumup, SPS_7$sumup)
Mkt_metrics$Key <- CK_Retail_Comparison$Key
Mkt_metrics <- Mkt_metrics[moveme(names(Mkt_metrics), "Key first")]

Binary_Form$AllTime <- Mkt_metrics$AllTime.sumup
Binary_Form$Three_Weeks <- Mkt_metrics$SPS_21.sumup
Binary_Form$Seven_Days <- Mkt_metrics$SPS_7.sumup

Mkt_metrics <- data.frame(CK_Retail_Comparison[1:5],Binary_Form)
View(Mkt_metrics)
CK_Retail_Comparison$All_Time <- Mkt_metrics$AllTime
CK_Retail_Comparison$Three_Weeks <- Mkt_metrics$Three_Weeks
CK_Retail_Comparison$Seven_Days <- Mkt_metrics$Seven_Days
AT <- as.data.frame(unique(CK_Retail_Comparison$All_Time))
Three <- as.data.frame(unique(CK_Retail_Comparison$Three_Weeks))
One <- as.data.frame(unique(CK_Retail_Comparison$Seven_Days))

AT <- as.data.frame(AT[order(-AT$`unique(CK_Retail_Comparison$All_Time)`),])
Three <- as.data.frame(Three[order(-Three$`unique(CK_Retail_Comparison$Three_Weeks)`),])
One <- as.data.frame(One[order(-One$`unique(CK_Retail_Comparison$Seven_Days)`),])


AT$Row <- seq(nrow(AT)) 
Three$Row <- seq(nrow(Three))
One$Row <- seq(nrow(One))

colnames(AT) <- c("Values","Row")
colnames(Three) <- c("Values","Row")
colnames(One) <- c("Values","Row")

CK_Retail_Comparison$AT_Rank <- AT$Row[match(CK_Retail_Comparison$All_Time,AT$Values)]
CK_Retail_Comparison$`3W_Rank` <- Three$Row[match(CK_Retail_Comparison$Three_Weeks, Three$Values)]
CK_Retail_Comparison$`7Day` <- One$Row[match(CK_Retail_Comparison$Seven_Days, One$Values)]


All_Time_Tiers <- CK_Retail_Comparison[which(CK_Retail_Comparison$AT_Rank<=10),]
Three_Week_Tiers <- CK_Retail_Comparison[which(CK_Retail_Comparison$`3W_Rank`<=5),]
Seven_Day_Tiers <- CK_Retail_Comparison[which(CK_Retail_Comparison$`7Day`<=3),]


