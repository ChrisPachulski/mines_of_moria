library(readr)
#Data from Spiders to individual Trackers####
Data <- read_csv("/cloud/project/Old Trackers/High Confidence Reps/2019-12-15_Premium.csv", 
                 col_types = cols(`F/NF` = col_character()))

#If Refresh Needed####
#Buylist_Tracker <- read_csv("/cloud/project/Metrics/2019-12-15/BuyList_History2019-12-15.csv", 
#                               col_types = cols(Foil = col_character()))
#W_Rank_Tracker <- read_csv("/cloud/project/Metrics/2019-12-15/W_Rank_History2019-12-15.csv", 
#                           col_types = cols(Foil = col_character()))
#Arbit_Tracker <- read_csv("/cloud/project/Metrics/2019-12-15/Arbit_History2019-12-15.csv", 
#                          col_types = cols(Foil = col_character()))
#Vendor_Tracker <- read_csv("/cloud/project/Metrics/2019-12-15/Vendor_History2019-12-15.csv", 
#                           col_types = cols(Foil = col_character()))
#TCG_Ranks <- read_csv("/cloud/project/Metrics/2019-12-15/TCG_History2019-12-15.csv", 
#                      col_types = cols(Foil = col_character()))
#CK_Ranks <- read_csv("/cloud/project/Metrics/2019-12-15/CK_History2019-12-15.csv", 
#                     col_types = cols(Foil = col_character()))


#Updates####
Buylist_Tracker$`2019-12-15` <- Data$BL[match(Buylist_Tracker$CK_Key,Data$Key)]
W_Rank_Tracker$`2019-12-15` <- Data$OVR_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]
Arbit_Tracker$`2019-12-15` <-Data$Arb[match(Buylist_Tracker$CK_Key,Data$Key)]
Vendor_Tracker$`2019-12-15` <- Data$Sellers[match(Buylist_Tracker$CK_Key,Data$Key)]
TCG_Ranks$`2019-12-15` <- Data$TCG_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]
CK_Ranks$`2019-12-15` <- Data$CK_Rank[match(Buylist_Tracker$CK_Key,Data$Key)]

#Exports####
setwd("/cloud/project/Metrics/2019-12-15")
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

