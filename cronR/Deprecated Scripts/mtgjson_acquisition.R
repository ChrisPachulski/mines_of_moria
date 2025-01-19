source("config.R")
library(tidyjson)
library(jsonlite)

content <- fromJSON("https://mtgjson.com/api/v5/AllPrintings.json")
sets_of_interest <- content$data %>% names()


Card_Dictionary <- NULL
for(set in sets_of_interest){
  temp <- fromJSON(paste("https://mtgjson.com/api/v5/",i,".json",sep=""))
  if(temp$data$isOnlineOnly == F){
  rdate <- temp$data$releaseDate
  if(is.null(rdate)==T){rdate = temp$data$releaseDate}
  uuid <- temp$data$cards$uuid
  if(is.null(uuid)==T){uuid = NA}
  scryfall_id <- temp$data$cards$identifiers$scryfallId
  if(is.null(scryfall_id)==T){scryfall_id = NA}
  mcmid <- temp$data$cards$identifiers$mcmId
  if(is.null(mcmid)==T){mcmid = NA}
  tcg_ID <- temp$data$cards$identifiers$tcgplayerProductId
  if(is.null(tcg_ID)==T){tcg_ID = NA}
  card <- temp$data$cards$name
  if(is.null(card)==T){card = NA}
  set <- temp$data$name
  if(is.null(set)==T){set = NA}
  abbr <- temp$data$code
  if(is.null(abbr)==T){abbr = NA}
  rarity <- temp$data$cards$rarity
  if(is.null(rarity)==T){rarity = NA}
  number <- temp$data$cards$number
  if(is.null(number)==T){number = NA}
  types <- temp$data$cards$types
  if(is.null(types)==T){types = NA}
  manaCost <- temp$data$cards$convertedManaCost
  if(is.null(manaCost)==T){manaCost = NA}
  colors <- temp$data$cards$colors
  if(is.null(colors)==T){colors = NA}
  keywords <- temp$data$cards$keywords
  if(is.null(keywords)==T){keywords = NA}
  hasFoil <- temp$data$cards$hasFoil
  if(is.null(hasFoil)==T){hasFoil = NA}
  hasNonFoil <- temp$data$cards$hasNonFoil
  if(is.null(hasNonFoil)==T){hasNonFoil = NA}
  isAlternative <- temp$data$cards$isAlternative
  if(is.null(isAlternative)==T){isAlternative = NA}
  # variations <- temp$cards$variations
  # if(is.null(variations)==T){variations = NA}
  standard <- temp$cards$legalities$standard
  if(is.null(standard)==T){standard = NA}
  pioneer <- temp$data$cards$legalities$pioneer
  if(is.null(pioneer)==T){pioneer = NA}
  modern <- temp$data$cards$legalities$modern
  if(is.null(modern)==T){modern = NA}
  legacy <- temp$data$cards$legalities$legacy
  if(is.null(legacy)==T){legacy = NA}
  commander <- temp$data$cards$legalities$commander
  if(is.null(commander)==T){commander = NA}
  pauper <- temp$data$cards$legalities$pauper
  if(is.null(pauper)==T){pauper = NA}
  ckid <- temp$data$cards$identifiers$cardKingdomId
  if(is.null(ckid)==T){ckid = NA}
  ckid_f <- temp$data$cards$identifiers$cardKingdomFoilId
  if(is.null(ckid_f)==T){ckid_f = NA}
  info <- cbind(rdate,uuid,scryfall_id,mcmid,tcg_ID,card,set,abbr,rarity,number,types,manaCost,colors,hasFoil,hasNonFoil,isAlternative,standard,pioneer,modern,legacy,commander,pauper,ckid,ckid_f)
  Card_Dictionary <- rbind(Card_Dictionary,info)
    }
  }

setwd("/home/cujo253/Essential_Referential_CSVS/Set_Json/")
temp <- list.files(pattern="*.json")
Number_Of_Files <- length(temp)
fromJSON(temp[i])$data$cards
#fromJSON(temp[i])$cards
Card_Dictionary <- NULL
for (i in 1:Number_Of_Files){
  rdate <- fromJSON(temp[i])$data$releaseDate
  if(is.null(rdate)==T){rdate = fromJSON(temp[i])$data$releaseDate}
  uuid <- fromJSON(temp[i])$data$cards$uuid
  if(is.null(uuid)==T){uuid = NA}
  scryfall_id <- fromJSON(temp[i])$data$cards$identifiers$scryfallId
  if(is.null(scryfall_id)==T){scryfall_id = NA}
  mcmid <- fromJSON(temp[i])$data$cards$identifiers$mcmId
  if(is.null(mcmid)==T){mcmid = NA}
  tcg_ID <- fromJSON(temp[i])$data$cards$identifiers$tcgplayerProductId
  if(is.null(tcg_ID)==T){tcg_ID = NA}
  card <- fromJSON(temp[i])$data$cards$name
  if(is.null(card)==T){card = NA}
  set <- fromJSON(temp[i])$data$name
  if(is.null(set)==T){set = NA}
  abbr <- fromJSON(temp[i])$data$code
  if(is.null(abbr)==T){abbr = NA}
  rarity <- fromJSON(temp[i])$data$cards$rarity
  if(is.null(rarity)==T){rarity = NA}
  number <- fromJSON(temp[i])$data$cards$number
  if(is.null(number)==T){number = NA}
  types <- fromJSON(temp[i])$data$cards$types
  if(is.null(types)==T){types = NA}
  manaCost <- fromJSON(temp[i])$data$cards$convertedManaCost
  if(is.null(manaCost)==T){manaCost = NA}
  colors <- fromJSON(temp[i])$data$cards$colors
  if(is.null(colors)==T){colors = NA}
  keywords <- fromJSON(temp[i])$data$cards$keywords
  if(is.null(keywords)==T){keywords = NA}
  hasFoil <- fromJSON(temp[i])$data$cards$hasFoil
  if(is.null(hasFoil)==T){hasFoil = NA}
  hasNonFoil <- fromJSON(temp[i])$data$cards$hasNonFoil
  if(is.null(hasNonFoil)==T){hasNonFoil = NA}
  isAlternative <- fromJSON(temp[i])$data$cards$isAlternative
  if(is.null(isAlternative)==T){isAlternative = NA}
  # variations <- fromJSON(temp[i])$cards$variations
  # if(is.null(variations)==T){variations = NA}
  standard <- fromJSON(temp[i])$cards$legalities$standard
  if(is.null(standard)==T){standard = NA}
  pioneer <- fromJSON(temp[i])$data$cards$legalities$pioneer
  if(is.null(pioneer)==T){pioneer = NA}
  modern <- fromJSON(temp[i])$data$cards$legalities$modern
  if(is.null(modern)==T){modern = NA}
  legacy <- fromJSON(temp[i])$data$cards$legalities$legacy
  if(is.null(legacy)==T){legacy = NA}
  commander <- fromJSON(temp[i])$data$cards$legalities$commander
  if(is.null(commander)==T){commander = NA}
  pauper <- fromJSON(temp[i])$data$cards$legalities$pauper
  if(is.null(pauper)==T){pauper = NA}
  ckid <- fromJSON(temp[i])$data$cards$identifiers$cardKingdomId
  if(is.null(ckid)==T){ckid = NA}
  ckid_f <- fromJSON(temp[i])$data$cards$identifiers$cardKingdomFoilId
  if(is.null(ckid_f)==T){ckid_f = NA}
  info <- cbind(rdate,uuid,scryfall_id,mcmid,tcg_ID,card,set,abbr,rarity,number,types,manaCost,colors,hasFoil,hasNonFoil,isAlternative,standard,pioneer,modern,legacy,commander,pauper,ckid,ckid_f)
  Card_Dictionary <- rbind(Card_Dictionary,info)
}
# summary(as.factor(unlist(Card_Dictionary$set)))
# View(Card_Dictionary[which(Card_Dictionary$set )])
Card_Dictionary_backup <- Card_Dictionary
#Card_Dictionary <- Card_Dictionary_backup
#Card_Dictionary <- unlist(Card_Dictionary[15])
Card_Dictionary <- as.data.frame(Card_Dictionary)
Card_Dictionary$rdate <- unlist(Card_Dictionary[1])
Card_Dictionary$uuid <- unlist(Card_Dictionary[2])
Card_Dictionary$scryfall_id <- unlist(Card_Dictionary[3])
Card_Dictionary$mcmid <- unlist(Card_Dictionary[4])
Card_Dictionary$tcg_ID<- unlist(Card_Dictionary[5])
Card_Dictionary$card <- unlist(Card_Dictionary[6])
Card_Dictionary$set <- unlist(Card_Dictionary[7])
Card_Dictionary$abbr <- unlist(Card_Dictionary[8])
Card_Dictionary$rarity <- unlist(Card_Dictionary[9])
Card_Dictionary$number <- unlist(Card_Dictionary[10])
Card_Dictionary$types <- unlist(ifelse(str_count(Card_Dictionary$types,'"') >=3,"Multiple",Card_Dictionary$types))

Card_Dictionary$manaCost <- unlist(Card_Dictionary[12])
#Card_Dictionary$colors <- unlist(ifelse(identical(Card_Dictionary$colors,character(0))==T,NA,Card_Dictionary$colors))
Card_Dictionary$hasFoil <- unlist(Card_Dictionary[14])
Card_Dictionary$hasNonFoil <- unlist(Card_Dictionary[15])
Card_Dictionary$isAlternative <- unlist(Card_Dictionary[16])
#Card_Dictionary$variations <- unlist(Card_Dictionary[15])
Card_Dictionary$standard <- unlist(Card_Dictionary[17])
Card_Dictionary$pioneer <- unlist(Card_Dictionary[18])
Card_Dictionary$modern <- unlist(Card_Dictionary[19])
Card_Dictionary$legacy <- unlist(Card_Dictionary[20])
Card_Dictionary$commander <- unlist(Card_Dictionary[21])
Card_Dictionary$pauper <- unlist(Card_Dictionary[22])
Card_Dictionary$ckid <- unlist(Card_Dictionary[23])
Card_Dictionary$ckid_f <- unlist(Card_Dictionary[24])
#Card_Dictionary <- Card_Dictionary[-11]
Card_Dictionary <- Card_Dictionary[-13]
Card_Dictionary$rarity <- ifelse(Card_Dictionary$rarity == "mythic","M",
                                 ifelse(Card_Dictionary$rarity == "rare","R",
                                        ifelse(Card_Dictionary$rarity == "uncommon","U",
                                               ifelse(Card_Dictionary$rarity == "common","C", Card_Dictionary$rarity))))

Special_Card_Dictionary <- Card_Dictionary[grepl("\\★",Card_Dictionary$number),]
Nonfoil_Card_Dictionary <- Card_Dictionary[!grepl("\\★",Card_Dictionary$number),]
# setwd("/home/cujo253/Essential_Referential_CSVS/")
# 
# 
# csvFileName <- paste("Special_Card_Dictionary",".csv",sep="")
# write.csv(Special_Card_Dictionary, file=csvFileName, row.names = FALSE)
# 
# csvFileName <- paste("Nonfoil_Card_Dictionary",".csv",sep="")
# write.csv(Nonfoil_Card_Dictionary, file=csvFileName, row.names = FALSE)
# 

Nonfoil_Only <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == F),]
Nonfoil_Only$hasFoil <- ""
Nonfoil_Only$hasNonFoil <- ""
Foil_Only <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == F & Nonfoil_Card_Dictionary$hasFoil == T),]
Foil_Only$hasFoil <- " FOIL"
Foil_Only$hasNonFoil <- ""
Nonfoil_Halfs <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == T),]
Nonfoil_Halfs$hasFoil <- ""
Nonfoil_Halfs$hasNonFoil <- ""
Foil_Halfs <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == T),]
Foil_Halfs$hasFoil <- " FOIL"
Foil_Halfs$hasNonFoil <- ""

Entire_Dictionary <- rbind(Nonfoil_Only, Foil_Only)
Entire_Dictionary <- rbind(Entire_Dictionary,Nonfoil_Halfs)
Entire_Dictionary <- rbind(Entire_Dictionary,Foil_Halfs)
Entire_Dictionary$Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,Entire_Dictionary$number,sep="")
Entire_Dictionary$Working_Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,sep="")
colnames(Entire_Dictionary[c(1,2,3,4,5,8,10,24,6,7,9,11,12,13,22,23)])

Shortened_Dictionary <- Entire_Dictionary[c(1,2,3,4,5,8,10,24,6,7,9,13,11,12,22,23)]
Shortened_Dictionary$Key <- paste(Shortened_Dictionary$card,Shortened_Dictionary$set,Shortened_Dictionary$rarity,Shortened_Dictionary$hasFoil,sep="")
View(Shortened_Dictionary)
summary(as.factor(unlist(Shortened_Dictionary$types)))
setwd("/home/cujo253/Essential_Referential_CSVS/")
csvFileName <- paste("C20_Addition",".csv",sep="")
write.csv(Shortened_Dictionary, file=csvFileName, row.names = FALSE)
