source("config.R")
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
#Exclusions Entered - Proceed#
currentDate <- Sys.Date()
#Set Today's Date

setwd("/home/cujo253/Reports/KPI/Master")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)-1
currentDate <- Sys.Date()

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(8:12)]
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Updated_Tracking_Keys$Key <- trimws(Updated_Tracking_Keys$Key)
#View(Updated_Tracking_Keys)
New_Roster <- Updated_Tracking_Keys

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default =  "c"))
  #tmp$Sellers <- gsub("[^0-9.-]", NA,as.character(tmp$Sellers))
  Updated_Tracking_Keys$New <- tmp$Ranking[match(Updated_Tracking_Keys$Key,tmp$Key)]
  Updated_Tracking_Keys$New <- as.numeric(as.character(Updated_Tracking_Keys$New))
  New_Roster <- cbind(New_Roster,Updated_Tracking_Keys$New) 
}

C20_Updated_Roster <- New_Roster
ncol(New_Roster)
colnames(New_Roster) <- c("Key","Name","Set","Rarity","Foil",format(seq(from = as.Date("2020-03-05"), to = currentDate, by = 'day'),format = "%Y-%m-%d") )
New_Roster$Foil <- as.character(New_Roster$Foil)

New_Roster$Foil[is.na(New_Roster$Foil)==T] <- ""
#New_Roster[is.na(New_Roster)==T] <- 0

setwd("/home/cujo253/Reports/KPI/TS_Base")
csvFileName <- paste("KPI_History",".csv",sep="")
write.csv(New_Roster, file=csvFileName, row.names = FALSE) 


Current_Buylist_Tracker <- New_Roster

#Three_Week_Movers <- read_csv(Three_Week_Movers)
#Keys_Of_Interest <- Three_Week_Movers$Key
Keys_Of_Interest <- Current_Buylist_Tracker$Key
Keys_Of_Interest <- as.data.frame(Keys_Of_Interest)
colnames(Keys_Of_Interest) <- c("Key")
Keys_Of_Interest <- merge(Keys_Of_Interest,Current_Buylist_Tracker, by="Key")
#Pull it into a variable (ensure foil is as character)
data <- Keys_Of_Interest
#Convert to "data" for simplicity

nrow(data)
data$Foil <- ifelse(data$Foil == "", "NF", data$Foil)

data <- data[which(data$Foil == "NF"),]
#data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
#Remove the Foils bc foils are gross
data <- data[which(data$Rarity != "U"),] #<- Rarity Selection Tool
data <- data[which(data$Rarity != "C"),] 
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)

headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
#Store the headers away to be repulled back in after transformation is complete
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))

abbrev_mean <- meaned_data[(nrow(meaned_data)-10):nrow(meaned_data),]

headers <- lapply(headers, as.character)
Recombined_data <- rbind(headers, abbrev_mean)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Recombined_data <- Recombined_data[,colSums(is.na(Recombined_data))<(nrow(Recombined_data)-15)]
title_Pull <- Recombined_data[c(1:5),]
data_pull <- as.data.frame(apply(Recombined_data[c(6:nrow(Recombined_data)),], 2, as.numeric))
#data_pull <- round(na.aggregate(data_pull),0)
Recombined_data <- rbind(title_Pull,data_pull)
Recombined_data <- as.data.frame(unique(t(Recombined_data)))

Recombined_data <- Recombined_data[order(as.numeric(as.character(Recombined_data$`11`)) ),]
colnames(Recombined_data)[4] <- "Rarity"
View(Recombined_data[which(Recombined_data$Rarity == "M"),])
