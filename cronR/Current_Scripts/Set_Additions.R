Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")

setwd("/home/cujo253/Reports/High Confidence Reps/")
currentDate <- Sys.Date()
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
Buylist_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$BL[match(Buylist_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  Buylist_Tracker <- cbind(Buylist_Tracker, New_Info[1])
}

Buylist_Tracker[6:ncol(Buylist_Tracker)] <- sapply(Buylist_Tracker[6:ncol(Buylist_Tracker)], as.character)
Buylist_Tracker[6:ncol(Buylist_Tracker)] <- sapply(Buylist_Tracker[6:ncol(Buylist_Tracker)], as.numeric)
#Buylist_Tracker <- as.data.frame(Buylist_Tracker)

Vendor_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$Sellers[match(Vendor_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  Vendor_Tracker <- cbind(Vendor_Tracker, New_Info[1])
}

Vendor_Tracker$Foil <- as.factor(Vendor_Tracker$Foil)
Vendor_Tracker = Vendor_Tracker[which(is.na(Vendor_Tracker$Foil) == T),]
Vendor_Tracker[6:ncol(Vendor_Tracker)] <- sapply(Vendor_Tracker[6:ncol(Vendor_Tracker)], as.character)
Vendor_Tracker[6:ncol(Vendor_Tracker)] <- sapply(Vendor_Tracker[6:ncol(Vendor_Tracker)], as.numeric)
Vendor_Tracker$`2020-01-20` <- Vendor_Tracker$`2020-01-21`
Vendor_Tracker[6:22][Vendor_Tracker[6:22] == 500] <- NA

#Buylist_Tracker <- as.data.frame(Buylist_Tracker)

TCG_Ranks <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$TCG_Rank[match(TCG_Ranks$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  TCG_Ranks <- cbind(TCG_Ranks, New_Info[1])
}

TCG_Ranks$Foil <- as.factor(TCG_Ranks$Foil)
TCG_Ranks = TCG_Ranks[which(is.na(TCG_Ranks$Foil) == T),]
TCG_Ranks[6:ncol(TCG_Ranks)] <- sapply(TCG_Ranks[6:ncol(TCG_Ranks)], as.character)
TCG_Ranks[6:ncol(TCG_Ranks)] <- sapply(TCG_Ranks[6:ncol(TCG_Ranks)], as.numeric)

#Buylist_Tracker <- as.data.frame(Buylist_Tracker)

CK_Ranks <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$CK_ADJ_Rank[match(CK_Ranks$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  CK_Ranks <- cbind(CK_Ranks, New_Info[1])
}

CK_Ranks$Foil <- as.factor(CK_Ranks$Foil)
CK_Ranks = CK_Ranks[which(is.na(CK_Ranks$Foil) == T),]
CK_Ranks[6:ncol(CK_Ranks)] <- sapply(CK_Ranks[6:ncol(CK_Ranks)], as.character)
CK_Ranks[6:ncol(CK_Ranks)] <- sapply(CK_Ranks[6:ncol(CK_Ranks)], as.numeric)



Buylist_Tracker$Foil <- ifelse(Buylist_Tracker$Foil == "Foil", Buylist_Tracker$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "Foil", TCG_Ranks$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "Foil", TCG_Ranks$Foil, "")
CK_Ranks$Foil <- ifelse(CK_Ranks$Foil == "Foil", CK_Ranks$Foil, "")

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
