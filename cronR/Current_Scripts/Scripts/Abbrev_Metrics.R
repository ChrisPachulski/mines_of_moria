library(readr)
#Data from Spiders to individual Trackers####
Data <- read_csv("/cloud/project/Reports/High Confidence Reps/2020-01-21_Premium.csv", 
                 col_types = cols(`F/NF` = col_character(), 
                                  Sellers = col_character(), TCG_Rank = col_character()))
d
#If Refresh Needed####
Buylist_Tracker <- read_csv("/cloud/project/Metrics/2020-01-13/2020-01-17BuyList_History.csv", 
                              col_types = cols(Foil = col_character()))
Buylist_Tracker$CK_Key<- trimws(Buylist_Tracker$CK_Key)

Vendor_Tracker <- read_csv("/cloud/project/Metrics/2020-01-13/2020-01-17Vendor_History.csv", 
                           col_types = cols(Foil = col_character()))
Vendor_Tracker$CK_Key <- trimws(Vendor_Tracker$CK_Key)
TCG_Ranks <- read_csv("/cloud/project/Metrics/2020-01-13/2020-01-17TCG_History.csv", 
                      col_types = cols(Foil = col_character()))
TCG_Ranks$CK_Key <- trimws(TCG_Ranks$CK_Key)
CK_Ranks <- read_csv("/cloud/project/Metrics/2020-01-13/2020-01-17CK_History.csv", 
                     col_types = cols(Foil = col_character()))
CK_Ranks$CK_Key <- trimws(CK_Ranks$CK_Key)

#Updates####
Buylist_Tracker$`2020-01-21` <- Data$BL[match(Buylist_Tracker$CK_Key,Data$Key)]
Vendor_Tracker$`2020-01-21` <- Data$Sellers[match(Buylist_Tracker$CK_Key,Data$Key)]
TCG_Ranks$`2020-01-21` <- Data$TCG_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]
CK_Ranks$`2020-01-21` <- Data$CK_ADJ_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]

Buylist_Tracker[is.na(Buylist_Tracker)] <- ""
Vendor_Tracker[is.na(Vendor_Tracker)] <- ""
TCG_Ranks[is.na(TCG_Ranks)] <- ""
CK_Ranks[is.na(CK_Ranks)] <- ""


Buylist_Tracker$Foil <- ifelse(Buylist_Tracker$Foil == "FOIL", Buylist_Tracker$Foil, "")
Vendor_Tracker$Foil <- ifelse(Vendor_Tracker$Foil == "FOIL", Vendor_Tracker$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "FOIL", TCG_Ranks$Foil, "")
CK_Ranks$Foil <- ifelse(CK_Ranks$Foil == "FOIL", CK_Ranks$Foil, "")


#Exports####
setwd("/cloud/project/Metrics/2020-01-21")
currentDate <- Sys.Date()
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
New <- Buylist_Tracker$CK_Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,1,ifelse(New[,c(2:ncol(New))]<0,-1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$CK_Key
csvFileName <- paste(currentDate,"Binary_Form",".csv",sep="")
write.csv(Binary_Form, file=csvFileName, row.names = FALSE)
#Below is broke for some reason####
#Binary_Form$Name <- Buylist_Tracker$`Card Name`
#View(Binary_Form)
#sumup <- sum(Binary_Form[1,c((ncol(Binary_Form)-6):(ncol(Binary_Form)-1))])
#15 Days
SPS_11 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-11):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_11 <- rbind(SPS_11,sumup)
}

SPS_11 <- as.data.frame(SPS_11)
summary(SPS_11$sumup)
#10 Days
SPS_7 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-7):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_7 <- rbind(SPS_7,sumup)
}
SPS_7 <- as.data.frame(SPS_7)
summary(SPS_7$sumup)
#5 Days
SPS_3 <- NULL
for (i in 1:nrow(Binary_Form)){
  sumup <- sum(Binary_Form[i,c((ncol(Binary_Form)-3):ncol(Binary_Form))])
  sumup <- as.data.frame(sumup)
  SPS_3 <- rbind(SPS_3,sumup)
}
SPS_3 <- as.data.frame(SPS_3)
View(SPS_3)
summary(SPS_3$sumup)
#SPS_5$Name <- Buylist_Tracker$`Card Name`
#SPS_5 <- SPS_5[order(-SPS_5$sumup),]

Buylist_Tracker_Metrics <- Buylist_Tracker[,-1]
Buylist_Tracker_Metrics$`29 Days` <- SPS_19$sumup
Buylist_Tracker_Metrics$`15 Days` <- SPS_15$sumup
Buylist_Tracker_Metrics$`5 Days` <- SPS_5$sumup
Buylist_Tracker_Metrics$Foil <- as.factor(Buylist_Tracker_Metrics$Foil)
Buylist_Tracker_Metrics <- as.data.frame(Buylist_Tracker_Metrics)

BTM_Mythics <- Buylist_Tracker_Metrics[which(Buylist_Tracker_Metrics$Rarity == "M"),]
BTM_Rares <- Buylist_Tracker_Metrics[which(Buylist_Tracker_Metrics$Rarity == "R"),]
BTM_Uncommons <- Buylist_Tracker_Metrics[which(Buylist_Tracker_Metrics$Rarity == "U"),]
BTM_Commons <- Buylist_Tracker_Metrics[which(Buylist_Tracker_Metrics$Rarity == "C"),]

View(BTM_Mythics)

BTM_Mythics_NF <- BTM_Mythics[which(is.na(BTM_Mythics$Foil) == TRUE),]
BTM_Rares_NF <- BTM_Rares[which(is.na(BTM_Rares$Foil) == TRUE),]
BTM_Uncommons_NF <- BTM_Uncommons[which(is.na(BTM_Uncommons$Foil) == TRUE),]
BTM_Commons_NF <- BTM_Commons[which(is.na(BTM_Commons$Foil) == TRUE),]

BTM_Mythics_F <- BTM_Mythics[which(is.na(BTM_Mythics$Foil) == FALSE),]
BTM_Rares_F <- BTM_Rares[which(is.na(BTM_Rares$Foil) == FALSE),]
BTM_Uncommons_F <- BTM_Uncommons[which(is.na(BTM_Uncommons$Foil) == FALSE),]
BTM_Commons_F <- BTM_Commons[which(is.na(BTM_Commons$Foil) == FALSE),]

BTM_Mythics_NF$`5 Days` <- as.numeric(BTM_Mythics_NF$`10 Days`)
BTM_Mythics_NF <- BTM_Mythics_NF[order(-BTM_Mythics_NF$`10 Days`),]

View(BTM_Mythics_NF)
