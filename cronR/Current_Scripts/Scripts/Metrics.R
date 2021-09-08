library(readr)
#Data from Spiders to individual Trackers####
Data <- read_csv("/cloud/project/Reports/High Confidence Reps/2020-01-17_Premium.csv", 
                 col_types = cols(`F/NF` = col_character()))
d
#If Refresh Needed####
Buylist_Tracker <- read_csv("/cloud/project/Metrics/2020-01-13/2020-01-17BuyList_History.csv", 
                              col_types = cols(Foil = col_character()))
Buylist_Tracker$CK_Key<- trimws(Buylist_Tracker$CK_Key)
#W_Rank_Tracker <- read_csv("/cloud/project/Metrics/2020-01-06/2020-01-07W_Rank_History.csv", 
                           #col_types = cols(Foil = col_character()))
#W_Rank_Tracker$CK_Key <- trimws(W_Rank_Tracker$CK_Key)
#Arbit_Tracker <- read_csv("/cloud/project/Metrics/2020-01-06/2020-01-07Arbit_History.csv", 
                          #col_types = cols(Foil = col_character()))
#Arbit_Tracker$CK_Key <- trimws(Arbit_Tracker$CK_Key)
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
Buylist_Tracker$`2020-01-17` <- Data$BL[match(Buylist_Tracker$CK_Key,Data$Key)]
#W_Rank_Tracker$`2020-01-06` <- Data$OVR_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]
#Arbit_Tracker$`2020-01-06` <-Data$Arb[match(Buylist_Tracker$CK_Key,Data$Key)]
Vendor_Tracker$`2020-01-17` <- Data$Sellers[match(Buylist_Tracker$CK_Key,Data$Key)]
TCG_Ranks$`2020-01-17` <- Data$TCG_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]
CK_Ranks$`2020-01-17` <- Data$CK_ADJ_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]

Buylist_Tracker[is.na(Buylist_Tracker)] <- ""
#W_Rank_Tracker[is.na(W_Rank_Tracker)] <- ""
#Arbit_Tracker[is.na(Arbit_Tracker)] <- ""
Vendor_Tracker[is.na(Vendor_Tracker)] <- ""
TCG_Ranks[is.na(TCG_Ranks)] <- ""
CK_Ranks[is.na(CK_Ranks)] <- ""


Buylist_Tracker$Foil <- ifelse(Buylist_Tracker$Foil == "FOIL", Buylist_Tracker$Foil, "")
#W_Rank_Tracker$Foil <- ifelse(W_Rank_Tracker$Foil == "FOIL", W_Rank_Tracker$Foil, "")
#Arbit_Tracker$Foil <- ifelse(Arbit_Tracker$Foil == "FOIL", Arbit_Tracker$Foil, "")
Vendor_Tracker$Foil <- ifelse(Vendor_Tracker$Foil == "FOIL", Vendor_Tracker$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "FOIL", TCG_Ranks$Foil, "")
CK_Ranks$Foil <- ifelse(CK_Ranks$Foil == "FOIL", CK_Ranks$Foil, "")


#Exports####
setwd("/cloud/project/Metrics/2020-01-13")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"BuyList_History",".csv",sep="")
write.csv(Buylist_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"W_Rank_History",".csv",sep="")
write.csv(W_Rank_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"Arbit_History",".csv",sep="")
write.csv(Arbit_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"Vendor_History",".csv",sep="")
write.csv(Vendor_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"TCG_History",".csv",sep="")
write.csv(TCG_Ranks, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"CK_History",".csv",sep="")
write.csv(CK_Ranks, file=csvFileName, row.names = FALSE) 

#Buylist Levels####
Buylist_Numbers <- Buylist_Tracker[c(-2,-3)]
Buylist_Numbers$CK_Key <- as.factor(Buylist_Numbers$CK_Key)
Buylist_Numbers$Foil <- as.factor(Buylist_Numbers$Foil)
Buylist_Numbers$Rarity <- as.factor(Buylist_Numbers$Rarity)
#Mythics_Buylists####
Buylist_Numbers_Mythics <- Buylist_Numbers[which(Buylist_Numbers$Rarity == "M"),]
Buylist_Numbers_Mythics <- Buylist_Numbers_Mythics[which(Buylist_Numbers_Mythics$Foil == ""),]
Buylist_Numbers_Mythics <- Buylist_Numbers_Mythics[c(-2,-3)]
Number_Of_Tracked_Mythics <- nrow(Buylist_Numbers_Mythics)
Buylist_Numbers_Mythics[2:ncol(Buylist_Numbers_Mythics)] <- sapply(Buylist_Numbers_Mythics[2:ncol(Buylist_Numbers_Mythics)],as.numeric)
#Buylist_Numbers_Mythics <- as.data.frame(t(Buylist_Numbers_Mythics))
#Rares_Buylists####
Buylist_Numbers_Rares <- Buylist_Numbers[which(Buylist_Numbers$Rarity == "R"),]
Buylist_Numbers_Rares <- Buylist_Numbers_Rares[which(Buylist_Numbers_Rares$Foil == ""),]
Buylist_Numbers_Rares <- Buylist_Numbers_Rares[c(-2,-3)]
Number_Of_Tracked_Rares <- nrow(Buylist_Numbers_Rares)
Buylist_Numbers_Rares[2:ncol(Buylist_Numbers_Rares)] <- sapply(Buylist_Numbers_Rares[2:ncol(Buylist_Numbers_Rares)],as.numeric)
#Buylist_Numbers_Rares <- as.data.frame(t(Buylist_Numbers_Rares))
#Uncommons_Buylists####
Buylist_Numbers_Uncommons <- Buylist_Numbers[which(Buylist_Numbers$Rarity == "U"),]
Buylist_Numbers_Uncommons <- Buylist_Numbers_Uncommons[which(Buylist_Numbers_Uncommons$Foil == ""),]
Buylist_Numbers_Uncommons <- Buylist_Numbers_Uncommons[c(-2,-3)]
Number_Of_Tracked_Uncommons <- nrow(Buylist_Numbers_Uncommons)
Buylist_Numbers_Uncommons[2:ncol(Buylist_Numbers_Uncommons)] <- sapply(Buylist_Numbers_Uncommons[2:ncol(Buylist_Numbers_Uncommons)],as.numeric)
#Buylist_Numbers_Uncommons <- as.data.frame(t(Buylist_Numbers_Uncommons))
#Commons_Buylists####
Buylist_Numbers_Commons <- Buylist_Numbers[which(Buylist_Numbers$Rarity == "C"),]
Buylist_Numbers_Commons <- Buylist_Numbers_Commons[which(Buylist_Numbers_Commons$Foil == ""),]
Buylist_Numbers_Commons <- Buylist_Numbers_Commons[c(-2,-3)]
Number_Of_Tracked_Commons <- nrow(Buylist_Numbers_Commons)
Buylist_Numbers_Commons[2:ncol(Buylist_Numbers_Commons)] <- sapply(Buylist_Numbers_Commons[2:ncol(Buylist_Numbers_Commons)],as.numeric)
#Buylist_Numbers_Commons <- as.data.frame(t(Buylist_Numbers_Commons))

#Buylist Level Exports####
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"Buylist_Mythics",".csv",sep="")
write.csv(Buylist_Numbers_Mythics, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"BuyList_Rare",".csv",sep="")
write.csv(Buylist_Numbers_Rares, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"BuyList_Uncommon",".csv",sep="")
write.csv(Buylist_Numbers_Uncommons, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"BuyList_Common",".csv",sep="")
write.csv(Buylist_Numbers_Commons, file=csvFileName, row.names = FALSE)
#Vendor Levels####
#View(Vendor_Tracker_1)
Vendor_Tracker_1 <- Vendor_Tracker[,c(-1)]
Vendor_Numbers <- Vendor_Tracker_1[c(-2,-3)]
Vendor_Numbers$CK_Key <- as.factor(Vendor_Numbers$CK_Key)
Vendor_Numbers$Foil <- as.factor(Vendor_Numbers$Foil)
Vendor_Numbers$Rarity <- as.factor(Vendor_Numbers$Rarity)
#Vendor_Mythics####
Vendor_Numbers_Mythics <- Vendor_Numbers[which(Vendor_Numbers$Rarity == "M"),]
Vendor_Numbers_Mythics <- Vendor_Numbers_Mythics[which(Vendor_Numbers_Mythics$Foil == ""),]
Vendor_Numbers_Mythics <- Vendor_Numbers_Mythics[c(-2,-3)]
Vendor_Tracked_Mythics <- nrow(Vendor_Numbers_Mythics)
Vendor_Numbers_Mythics[2:ncol(Vendor_Numbers_Mythics)] <- sapply(Vendor_Numbers_Mythics[2:ncol(Vendor_Numbers_Mythics)],as.numeric)
Vendor_Numbers_Mythics[Vendor_Numbers_Mythics==500]<-""
Vendor_Numbers_Mythics[is.na(Vendor_Numbers_Mythics) == TRUE]<-""
#Vendor_Numbers_Mythics <- as.data.frame(t(Vendor_Numbers_Mythics))
#Vendor_Rares####
Vendor_Numbers_Rares <- Vendor_Numbers[which(Vendor_Numbers$Rarity == "R"),]
Vendor_Numbers_Rares <- Vendor_Numbers_Rares[which(Vendor_Numbers_Rares$Foil == ""),]
Vendor_Numbers_Rares <- Vendor_Numbers_Rares[c(-2,-3)]
Vendor_Tracked_Rares <- nrow(Vendor_Numbers_Rares)
Vendor_Numbers_Rares[2:ncol(Vendor_Numbers_Rares)] <- sapply(Vendor_Numbers_Rares[2:ncol(Vendor_Numbers_Rares)],as.numeric)
Vendor_Numbers_Rares[Vendor_Numbers_Rares==500]<-""
Vendor_Numbers_Rares[is.na(Vendor_Numbers_Rares) == TRUE]<-""
#Vendor_Numbers_Rares <- as.data.frame(t(Vendor_Numbers_Rares))
#Vendor_Uncommons####
Vendor_Numbers_Uncommons <- Vendor_Numbers[which(Vendor_Numbers$Rarity == "U"),]
Vendor_Numbers_Uncommons <- Vendor_Numbers_Uncommons[which(Vendor_Numbers_Uncommons$Foil == ""),]
Vendor_Numbers_Uncommons <- Vendor_Numbers_Uncommons[c(-2,-3)]
Vendor_Tracked_Uncommons <- nrow(Vendor_Numbers_Uncommons)
Vendor_Numbers_Uncommons[2:ncol(Vendor_Numbers_Uncommons)] <- sapply(Vendor_Numbers_Uncommons[2:ncol(Vendor_Numbers_Uncommons)],as.numeric)
Vendor_Numbers_Uncommons[Vendor_Numbers_Uncommons==500]<-""
Vendor_Numbers_Uncommons[is.na(Vendor_Numbers_Uncommons) == TRUE]<-""
#Vendor_Numbers_Uncommons <- as.data.frame(t(Vendor_Numbers_Uncommons))
#Vendor_Commons####
Vendor_Numbers_Commons <- Vendor_Numbers[which(Vendor_Numbers$Rarity == "C"),]
Vendor_Numbers_Commons <- Vendor_Numbers_Commons[which(Vendor_Numbers_Commons$Foil == ""),]
Vendor_Numbers_Commons <- Vendor_Numbers_Commons[c(-2,-3)]
Vendor_Tracked_Commons <- nrow(Vendor_Numbers_Commons)
Vendor_Numbers_Commons[2:ncol(Vendor_Numbers_Commons)] <- sapply(Vendor_Numbers_Commons[2:ncol(Vendor_Numbers_Commons)],as.numeric)
Vendor_Numbers_Commons[Vendor_Numbers_Commons==500]<-""
Vendor_Numbers_Commons[is.na(Vendor_Numbers_Commons) == TRUE]<-""
#Vendor_Numbers_Commons <- as.data.frame(t(Vendor_Numbers_Commons))
#Vendor Level Exports####
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"Vendor_Mythics",".csv",sep="")
write.csv(Vendor_Numbers_Mythics, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"Vendor_Rare",".csv",sep="")
write.csv(Vendor_Numbers_Rares, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"Vendor_Uncommon",".csv",sep="")
write.csv(Vendor_Numbers_Uncommons, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"Vendor_Common",".csv",sep="")
write.csv(Vendor_Numbers_Commons, file=csvFileName, row.names = FALSE)
#TCG Ranks (Demand)####
#View(TCG_Ranks)
TCG_Tracker_1 <- TCG_Ranks[,c(-1)]
TCG_Numbers <- TCG_Tracker_1[c(-2,-3)]
TCG_Numbers$CK_Key <- as.factor(TCG_Numbers$CK_Key)
TCG_Numbers$Foil <- as.factor(TCG_Numbers$Foil)
TCG_Numbers$Rarity <- as.factor(TCG_Numbers$Rarity)
#TCG_Mythics####
TCG_Numbers_Mythics <- TCG_Numbers[which(TCG_Numbers$Rarity == "M"),]
TCG_Numbers_Mythics <- TCG_Numbers_Mythics[which(TCG_Numbers_Mythics$Foil == ""),]
TCG_Numbers_Mythics <- TCG_Numbers_Mythics[c(-2,-3)]
TCG_Tracked_Mythics <- nrow(TCG_Numbers_Mythics)
TCG_Numbers_Mythics[2:ncol(TCG_Numbers_Mythics)] <- sapply(TCG_Numbers_Mythics[2:ncol(TCG_Numbers_Mythics)],as.numeric)
#TCG_Numbers_Mythics[TCG_Numbers_Mythics==500]<-""
TCG_Numbers_Mythics[is.na(TCG_Numbers_Mythics) == TRUE]<-""
#TCG_Numbers_Mythics <- as.data.frame(t(TCG_Numbers_Mythics))
#TCG_Rares####
TCG_Numbers_Rares <- TCG_Numbers[which(TCG_Numbers$Rarity == "R"),]
TCG_Numbers_Rares <- TCG_Numbers_Rares[which(TCG_Numbers_Rares$Foil == ""),]
TCG_Numbers_Rares <- TCG_Numbers_Rares[c(-2,-3)]
TCG_Tracked_Rares <- nrow(TCG_Numbers_Rares)
TCG_Numbers_Rares[2:ncol(TCG_Numbers_Rares)] <- sapply(TCG_Numbers_Rares[2:ncol(TCG_Numbers_Rares)],as.numeric)
#TCG_Numbers_Rares[TCG_Numbers_Rares==500]<-""
TCG_Numbers_Rares[is.na(TCG_Numbers_Rares) == TRUE]<-""
#TCG_Numbers_Rares <- as.data.frame(t(TCG_Numbers_Rares))
#TCG_Uncommons####
TCG_Numbers_Uncommons <- TCG_Numbers[which(TCG_Numbers$Rarity == "U"),]
TCG_Numbers_Uncommons <- TCG_Numbers_Uncommons[which(TCG_Numbers_Uncommons$Foil == ""),]
TCG_Numbers_Uncommons <- TCG_Numbers_Uncommons[c(-2,-3)]
TCG_Tracked_Uncommons <- nrow(TCG_Numbers_Uncommons)
TCG_Numbers_Uncommons[2:ncol(TCG_Numbers_Uncommons)] <- sapply(TCG_Numbers_Uncommons[2:ncol(TCG_Numbers_Uncommons)],as.numeric)
#TCG_Numbers_Uncommons[TCG_Numbers_Uncommons==500]<-""
TCG_Numbers_Uncommons[is.na(TCG_Numbers_Uncommons) == TRUE]<-""
#TCG_Numbers_Uncommons <- as.data.frame(t(TCG_Numbers_Uncommons))
#TCG_Commons####
TCG_Numbers_Commons <- TCG_Numbers[which(TCG_Numbers$Rarity == "C"),]
TCG_Numbers_Commons <- TCG_Numbers_Commons[which(TCG_Numbers_Commons$Foil == ""),]
TCG_Numbers_Commons <- TCG_Numbers_Commons[c(-2,-3)]
TCG_Tracked_Commons <- nrow(TCG_Numbers_Commons)
TCG_Numbers_Commons[2:ncol(TCG_Numbers_Commons)] <- sapply(TCG_Numbers_Commons[2:ncol(TCG_Numbers_Commons)],as.numeric)
#TCG_Numbers_Commons[TCG_Numbers_Commons==500]<-""
TCG_Numbers_Commons[is.na(TCG_Numbers_Commons) == TRUE]<-""
#TCG_Numbers_Commons <- as.data.frame(t(TCG_Numbers_Commons))


#TCG Level Exports####
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"TCG_Mythics",".csv",sep="")
write.csv(TCG_Numbers_Mythics, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"TCG_Rare",".csv",sep="")
write.csv(TCG_Numbers_Rares, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"TCG_Uncommon",".csv",sep="")
write.csv(TCG_Numbers_Uncommons, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"TCG_Common",".csv",sep="")
write.csv(TCG_Numbers_Commons, file=csvFileName, row.names = FALSE)
#CK Ranks (Demand)####
#View(CK_Ranks)
CK_Tracker_1 <- CK_Ranks[,c(-1,-2,-3)]
CK_Numbers <- CK_Tracker_1[c(-2,-3)]
CK_Numbers$CK_Key <- as.factor(CK_Numbers$CK_Key)
CK_Numbers$Foil <- as.factor(CK_Numbers$Foil)
CK_Numbers$Rarity <- as.factor(CK_Numbers$Rarity)
#CK_Mythics####
CK_Numbers_Mythics <- CK_Numbers[which(CK_Numbers$Rarity == "M"),]
CK_Numbers_Mythics <- CK_Numbers_Mythics[which(CK_Numbers_Mythics$Foil == ""),]
CK_Numbers_Mythics <- CK_Numbers_Mythics[c(-2,-3)]
CK_Tracked_Mythics <- nrow(CK_Numbers_Mythics)
CK_Numbers_Mythics[2:ncol(CK_Numbers_Mythics)] <- sapply(CK_Numbers_Mythics[2:ncol(CK_Numbers_Mythics)],as.numeric)
#CK_Numbers_Mythics[CK_Numbers_Mythics==500]<-""
CK_Numbers_Mythics[is.na(CK_Numbers_Mythics) == TRUE]<-""
#CK_Numbers_Mythics <- as.data.frame(t(CK_Numbers_Mythics))
#CK_Rares####
CK_Numbers_Rares <- CK_Numbers[which(CK_Numbers$Rarity == "R"),]
CK_Numbers_Rares <- CK_Numbers_Rares[which(CK_Numbers_Rares$Foil == ""),]
CK_Numbers_Rares <- CK_Numbers_Rares[c(-2,-3)]
CK_Tracked_Rares <- nrow(CK_Numbers_Rares)
CK_Numbers_Rares[2:ncol(CK_Numbers_Rares)] <- sapply(CK_Numbers_Rares[2:ncol(CK_Numbers_Rares)],as.numeric)
#CK_Numbers_Rares[CK_Numbers_Rares==500]<-""
CK_Numbers_Rares[is.na(CK_Numbers_Rares) == TRUE]<-""
#CK_Numbers_Rares <- as.data.frame(t(CK_Numbers_Rares))
#CK_Uncommons####
CK_Numbers_Uncommons <- CK_Numbers[which(CK_Numbers$Rarity == "U"),]
CK_Numbers_Uncommons <- CK_Numbers_Uncommons[which(CK_Numbers_Uncommons$Foil == ""),]
CK_Numbers_Uncommons <- CK_Numbers_Uncommons[c(-2,-3)]
CK_Tracked_Uncommons <- nrow(CK_Numbers_Uncommons)
CK_Numbers_Uncommons[2:ncol(CK_Numbers_Uncommons)] <- sapply(CK_Numbers_Uncommons[2:ncol(CK_Numbers_Uncommons)],as.numeric)
#CK_Numbers_Uncommons[CK_Numbers_Uncommons==500]<-""
CK_Numbers_Uncommons[is.na(CK_Numbers_Uncommons) == TRUE]<-""
#CK_Numbers_Uncommons <- as.data.frame(t(CK_Numbers_Uncommons))
#CK_Commons####
CK_Numbers_Commons <- CK_Numbers[which(CK_Numbers$Rarity == "C"),]
CK_Numbers_Commons <- CK_Numbers_Commons[which(CK_Numbers_Commons$Foil == ""),]
CK_Numbers_Commons <- CK_Numbers_Commons[c(-2,-3)]
CK_Tracked_Commons <- nrow(CK_Numbers_Commons)
CK_Numbers_Commons[2:ncol(CK_Numbers_Commons)] <- sapply(CK_Numbers_Commons[2:ncol(CK_Numbers_Commons)],as.numeric)
#CK_Numbers_Commons[CK_Numbers_Commons==500]<-""
CK_Numbers_Commons[is.na(CK_Numbers_Commons) == TRUE]<-""
#CK_Numbers_Commons <- as.data.frame(t(CK_Numbers_Commons))



#CK Level Exports####
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"CK_Mythics",".csv",sep="")
write.csv(CK_Numbers_Mythics, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"CK_Rare",".csv",sep="")
write.csv(CK_Numbers_Rares, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"CK_Uncommon",".csv",sep="")
write.csv(CK_Numbers_Uncommons, file=csvFileName, row.names = FALSE)

csvFileName <- paste(currentDate,"CK_Common",".csv",sep="")
write.csv(CK_Numbers_Commons, file=csvFileName, row.names = FALSE)

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
