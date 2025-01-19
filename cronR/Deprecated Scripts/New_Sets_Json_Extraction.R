source("config.R")
library(jsonlite)
library(readr)
library(mailR)
#API ABuse####
Mystery_Booster_Json <- fromJSON("https://www.mtgjson.com/json/MB1.json")
Mystery_Booster_Json <- flatten(Mystery_Booster_Json)

n <- length(Mystery_Booster_Json[[1]])
DF <- (structure(Mystery_Booster_Json, row.names = c(NA, -n), class = "data.frame"))
DF <- as.data.frame(DF)

#Individual Sets Dig####
EDHrec_Rank <- DF[[7]]
Card_Name <- DF[[17]]
Purchase_URL <- DF[[21]]
Rarity <- DF[[22]]
Set <- DF[[51]]

MB1 <- data.frame(Purchase_URL, Card_Name, Set, Rarity)
MB1 <- MB1[,-2]
summary(MB1$Rarity)
MB1$Rarity <- ifelse(MB1$Rarity == "common", "C",ifelse(MB1$Rarity == "uncommon", "U", ifelse(MB1$Rarity == "rare", "R", ifelse(MB1$Rarity == "mythic", "M",""))))
MB1$Rarity <- as.factor(MB1$Rarity)
MB1$Set <- ifelse(MB1$Set == "MB1", "Mystery Booster", "")
MB1$Set <- as.factor(MB1$Set)
MB1$tcgplayer <- paste(MB1$Card_Name,MB1$Set,MB1$Rarity,sep="")
View(MB1)

setwd("/home/cujo253/")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_FMB_Set",".csv",sep="")
write.csv(MB1, file=csvFileName, row.names = FALSE)
