install.packages("pacman")
pacman::p_load(tidyverse,httr,bigrquery,lubridate,jsonlite,quantmod,RSelenium,rvest,janitor,googlesheets4,googledrive)
gaeas_cradle <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
}
mtgjson_roster_update = function() {
    content <- fromJSON("https://mtgjson.com/api/v5/AllPrintings.json")
    #library(tidyjson)
    sets_of_interest <- content$data %>% names()
    set = "STA"
    Card_Dictionary <- NULL
    for(set in sets_of_interest){
        temp <- fromJSON(paste("https://mtgjson.com/api/v5/",set,".json",sep=""))
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
    
    Card_Dictionary_backup <- Card_Dictionary
    Card_Dictionary <- Card_Dictionary_backup
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
    return(Entire_Dictionary)
}
ban_data_retrieval = function(id){
    BAN_data = GET(paste("https://www.mtgban.com/api/mtgban/all.json?id=",id,"&sig=QVBJPUFMTF9BQ0NFU1MmQVBJbW9kZT1hbGwmRXhwaXJlcz0xNjUzNDAxMzEzJlNpZ25hdHVyZT1kbjRUSWd2Q3hTWEpCZmtYS0JpRGNhRmNpZXMlM0QmVXNlckVtYWlsPXdvbGYlNDBtdGdiYW4uY29t",sep="")) %>%
        content("parsed")
    
    
    for(v in 1:2){
        if(v == 1){
            
            semi_unnested = map_df(BAN_data$buylist, ~ replace(.x, is.null(.x), NA), .id = "uuid") 
            buylist_master_tbl = NULL
            
            buylist_vendor_tbl = semi_unnested %>% select(-uuid) %>% 
                colnames() %>% as_tibble() %>% 
                mutate(id = seq(nrow(.))) %>% 
                rename(vendor=value) %>%
                select(id,vendor) %>%
                mutate(description = ifelse(id == 1, "ABU Buylist",
                                            ifelse(id == 2, "Card Kingdom Buylist",
                                                   ifelse(id == 3, "Cool Stuff Inc Buylist",
                                                          ifelse(id == 4, "MTG Seattle Buylist",
                                                                 ifelse(id == 5, "Star City Games Buylist",
                                                                        ifelse(id == 6, "Troll & Toad Buylist",
                                                                               ifelse(id == 7, "TCG Buylist",
                                                                                      ifelse(id == 8, "95 Buylist",
                                                                                             ifelse(id == 9, "Cardsphere Offers",
                                                                                                    ifelse(id == 10, "Mythic MTG Buylist",
                                                                                                           ifelse(id == 11, "Strikezone Buylist",
                                                                                                                  ifelse(id == 12, "Hareruya Buylist",
                                                                                                                         ifelse(id == 13, "Blue Print Bulk Buylist",""
                                                                                                                         ))))))))))))))
            for (i in 2:(nrow(buylist_vendor_tbl)+1)) {
                information = semi_unnested[,c(1,i)] %>% # creates the 'value' as a `list` column
                    mutate(uuid = uuid,
                           name =  map(enframe(semi_unnested[[i]])$name, as.character),
                           value = map(enframe(semi_unnested[[i]])$value, as.character)) %>% 
                    select(uuid,name,value) %>% distinct() %>%
                    unnest(cols = c(name, value)) %>% rename(hasFoil = name, offer =value) %>%
                    mutate(Date = ymd(Sys.Date()),
                           offer = as.numeric(offer),
                           hasFoil = ifelse(hasFoil == "foil",1,0),
                           vendor =  i - 1) %>%
                    select(Date, everything())
                if(i == 2){
                    abu_bl = information
                }else if (i == 3) {
                    ck_bl = information
                }else if (i == 4) {
                    csi_bl = information
                }else if (i == 5) {
                    ms_bl = information
                }else if (i == 6) {
                    scg_bl = information
                }else if (i == 7) {
                    tat_bl = information
                }else if (i == 8) {
                    tcg_bl = information
                }else if (i == 9) {
                    `95_bl` = information
                }else if (i == 10) {
                    cs_bl = information
                }else if (i == 11) {
                    mmtg_f = information
                }else if (i == 12) {
                    sz_bl = information
                }else if (i == 13) {
                    ha_bl = information
                }else if (i == 14) {
                    bp_bl = information
                }
                
                buylist_master_tbl = rbind(buylist_master_tbl,information)
                
            }
        } else{
            
            semi_unnested = map_df(BAN_data$retail, ~ replace(.x, is.null(.x), NA), .id = "uuid") 
            
            retail_master_tbl = NULL
            
            retail_vendor_tbl = semi_unnested %>% select(-uuid) %>% 
                colnames() %>% as_tibble() %>% 
                mutate(id = seq(nrow(.))) %>% 
                rename(vendor=value) %>%
                select(id,vendor) %>%
                mutate(description = ifelse(id == 1, "Cardkingom Retail",
                                            ifelse(id == 2, "Cardtrader Retail",
                                                   ifelse(id == 3, "Magic Cardmarket's lowest price. No info on conditions,language,or shipping: the cheapest copy available",
                                                          ifelse(id == 4, "Magic Cardmarket's trending value",
                                                                 ifelse(id == 5, "Star City Games Retail",
                                                                        ifelse(id == 6, "Troll & Toad Retail",
                                                                               ifelse(id == 7, "TCG Direct Retail, near mint prices only",
                                                                                      ifelse(id == 8, "TCG Low Retail, No info on conditions,language,or shipping: the cheapest copy available",
                                                                                             ifelse(id == 9, "TCG Market Retail. TCG trending value displayed for most users on main GUI",
                                                                                                    ifelse(id == 10, "TCG Retail. Lowest near mint with shipping included",
                                                                                                           ifelse(id == 11, "95 Retail",
                                                                                                                  ifelse(id == 12, "ABU Retail",
                                                                                                                         ifelse(id == 13, "Mythic MTG Retail",
                                                                                                                                ifelse(id == 14, "Amazon Retail",
                                                                                                                                       ifelse(id == 15, "Cool Stuff Inc Retail",
                                                                                                                                              ifelse(id == 16, "MTG Seattle Retail",
                                                                                                                                                     ifelse(id == 17, "Strikezone Retail","")
                                                                                                                                              )))))))))))))))))
            
            i =2
            for (i in 2:14) {
                information = semi_unnested[,c(1,i)] %>% # creates the 'value' as a `list` column
                    mutate(uuid = uuid,
                           name =  map(enframe(semi_unnested[[i]])$name, as.character),
                           value = map(enframe(semi_unnested[[i]])$value, as.character)) %>% 
                    select(uuid,name,value) %>% distinct() %>%
                    unnest(cols = c(name, value)) %>% rename(hasFoil = name, retail =value)%>%
                    mutate(Date = ymd(Sys.Date()),
                           retail = as.numeric(retail),
                           hasFoil = ifelse(hasFoil == "foil",1,0),
                           vendor =  i- 1) %>%
                    select(Date, everything())
                
                if(i == 2){
                    ck_mkt = information
                }else if (i == 3) {
                    ct_mkt = information
                }else if (i == 4) {
                    mkm_low_mkt = information
                }else if (i == 5) {
                    mkm_trend_mkt = information
                }else if (i == 6) {
                    scg_mkt = information
                }else if (i == 7) {
                    tat_mkt = information
                }else if (i == 8) {
                    tcg_d_mkt = information
                }else if (i == 9) {
                    tcg_low = information
                }else if (i == 10) {
                    tcg_mkt = information
                }else if (i == 11) {
                    tcg = information
                }else if (i == 12) {
                    `95_mkt` = information
                }else if (i == 13) {
                    abu_mkt = information
                }else if (i == 14) {
                    mmtg_mkt = information
                }else if (i == 15) {
                    amz_mkt = information
                }else if (i == 16) {
                    csi_mkt = information
                }else if (i == 17) {
                    ms_mkt = information
                }else if (i == 18) {
                    sz_mkt = information
                }
                
                retail_master_tbl = rbind(retail_master_tbl,information)   
            }
            
        }
    }
    
    output = list(retail_vendor_tbl,buylist_vendor_tbl,retail_master_tbl, buylist_master_tbl)
    return(output)
}
tcg_best_sellers = function(){A <- 0
B <- 100
C <- 100
TCG__Best_Sellers <- NULL
body <- paste('{
    "algorithm": "salesrel",
        "from": "',A,'",
        "size": "',B,'",
    "context": {
          "cart": {},
          "shippingCountry": "US"
              },
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
              sep="")
A <- B 
B <- 200
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
for(i in 1:C){
    Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
    Set <- TCG_Results_1[[1]]$results[[i]]$setName
    Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
    Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
    MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
    Max_Quant_Fillable = TCG_Results_1[[1]]$results[[i]]$maxFulfillableQuantity
    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
    hasFoil = ""
    Direct_Listings <- 0
    Total_Copies <- NULL
    Potential_Direct_Copies <- NULL
    limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
    if(limit >0){
        for(j in 1:limit){
            if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                    dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                    Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                else{
                    Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                    Total_Copies <- rbind(Total_Copies,Copies)
                }
            }
        }
    }
    Potential_Direct_Copies <- sum(Potential_Direct_Copies)
    Total_Copies <- sum(Total_Copies)
    if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
    Line_Item <- cbind(Name,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies,Max_Quant_Fillable)
    TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}

for(i in 1:97){
    tryCatch({
        body <- paste('{
          "algorithm": "salesrel",
        "from": "',A,'",
        "size": "',B,'",
              "context": {
                "cart": {},
                "shippingCountry": "US"
                    },
          "filters": {
              "range": {},
              "term": {
                  "productLineName": [
                      "magic"
                  ],
                  "productTypeName": [
                      "Cards"
                  ],
                  "rarityName": [
                      "Mythic",
                      "Uncommon",
                      "Rare"
                  ]
              }
          }
      }',
                      sep="")
        A <- A + 100
        B <- 100
        C <- 100
        TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
        TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
        repeat{
            if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
            if((length(TCG_Results_1[[1]]$results) != 0)) break
        }
        for(i in 1:C){
            Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
            Set <- TCG_Results_1[[1]]$results[[i]]$setName
            Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
            Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
            MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
            Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
            Max_Quant_Fillable = TCG_Results_1[[1]]$results[[i]]$maxFulfillableQuantity
            MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
            Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
            Direct_Listings <- 0
            hasFoil = ""
            Total_Copies <- NULL
            Potential_Direct_Copies <- NULL
            limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
            if(limit >0){
                for(j in 1:limit){
                    if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                        Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                            dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                        else{
                            Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Total_Copies <- rbind(Total_Copies,Copies)
                        }
                    }
                }
            }
            Potential_Direct_Copies <- sum(Potential_Direct_Copies)
            Total_Copies <- sum(Total_Copies)
            if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
            Line_Item <- cbind(Name,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies,Max_Quant_Fillable)
            TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
        }
    }, error = function(e){print("Loop error")})
    #if(A >= 9990) break
    Sys.sleep(.25)
}

A <- 0
B <- 100
C <- 100
body <- paste('{
    "algorithm": "salesrel",
        "from": "',A,'",
        "size": "',B,'",
    "context": {
          "cart": {},
          "shippingCountry": "US"
              },
    "filters": {
        "range": {},
        "term": {
            "printing": [
            "Foil"
            ],
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
              sep="")
A <- B 
B <- 200
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
i = 1
for(i in 1:C){
    Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
    Set <- TCG_Results_1[[1]]$results[[i]]$setName
    Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
    Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
    MKT_EST <- NA
    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
    Max_Quant_Fillable = TCG_Results_1[[1]]$results[[i]]$maxFulfillableQuantity
    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
    Direct_Listings <- 0
    hasFoil = "FOIL"
    Total_Copies <- NULL
    Potential_Direct_Copies <- NULL
    limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
    if(limit >0){
        for(j in 1:limit){
            MKT_EST <- (TCG_Results_1[[1]]$results[[i]]$listings[[j]]$score) + (TCG_Results_1[[1]]$results[[i]]$listings[[j]]$shippingPrice)
            if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                    dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                    Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                else{
                    Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                    Total_Copies <- rbind(Total_Copies,Copies)
                }
            }
        }
    }
    Potential_Direct_Copies <- sum(Potential_Direct_Copies)
    MKT_EST <- round(mean(MKT_EST),2)
    Total_Copies <- sum(Total_Copies)
    if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
    Line_Item <- cbind(Name,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies,Max_Quant_Fillable)
    TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}

for(i in 1:97){
    tryCatch({
        body <- paste('{
        "algorithm": "salesrel",
        "from": "',A,'",
        "size": "',B,'",
            "context": {
              "cart": {},
              "shippingCountry": "US"
                  },
        "filters": {
            "range": {},
            "term": {
                "printing": [
                "Foil"
                ],
                "productLineName": [
                    "magic"
                ],
                "productTypeName": [
                    "Cards"
                ],
                "rarityName": [
                    "Mythic",
                    "Uncommon",
                    "Rare"
                ]
            }
        }
    }',
                      sep="")
        A <- A + 100
        B <- 100
        C <- 100
        TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
        TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
        repeat{
            if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
            if((length(TCG_Results_1[[1]]$results) != 0)) break
        }
        for(i in 1:C){
            Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
            Set <- TCG_Results_1[[1]]$results[[i]]$setName
            Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
            Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
            MKT_EST <- NA
            Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
            Max_Quant_Fillable = TCG_Results_1[[1]]$results[[i]]$maxFulfillableQuantity
            MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
            Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
            Direct_Listings <- 0
            hasFoil = "FOIL"
            Total_Copies <- NULL
            Potential_Direct_Copies <- NULL
            limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
            if(limit >0){
                for(j in 1:limit){
                    MKT_EST <- (TCG_Results_1[[1]]$results[[i]]$listings[[j]]$score) + (TCG_Results_1[[1]]$results[[i]]$listings[[j]]$shippingPrice)
                    if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                        Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                            dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                        else{
                            Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Total_Copies <- rbind(Total_Copies,Copies)
                        }
                    }
                }
            }
            Potential_Direct_Copies <- sum(Potential_Direct_Copies)
            MKT_EST = round(mean(MKT_EST),2)
            Total_Copies <- sum(Total_Copies)
            if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
            Line_Item <- cbind(Name,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies,Max_Quant_Fillable)
            TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
        }
        #if(A >= 9990) break
    }, error = function(e){print("Loop error")})
    Sys.sleep(.25)
}

TCG__Best_Sellers <- unique(TCG__Best_Sellers)
TCG__Best_Sellers <- TCG__Best_Sellers %>% as.data.frame() %>% mutate(Rank = seq(nrow(TCG__Best_Sellers))) %>%
    mutate(Rarity = ifelse(Rarity == "Mythic","M", ifelse(Rarity == "Rare", "R", ifelse(Rarity == "Uncommon", "U", ifelse(Rarity == "Common", "C", Rarity))))) %>%
    mutate(Key = trimws(paste(Name,Set,Rarity,hasFoil,sep=""))) %>% relocate(Key, .before = Name)
return(TCG__Best_Sellers)

}
invisible(chrome <-function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr
})
all_tcg_card_info = function(ip){
    remDr <- chrome(ip)
    remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=magic&page=1")
    Sys.sleep(4)
    remDr$findElement('xpath','//*[@id="app"]/div/section[2]/div/div[1]/button')$clickElement()
    Sys.sleep(4)
    
    tryCatch({remDr$findElement('xpath','/html/body/div[5]/div/div/div/div/button/span')$clickElement()}, 
             error = function(e){print("No msg box popped up")
             })
    Sys.sleep(2)
    tryCatch({remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[2]/div[2]/div[2]')$clickElement()}, 
             error = function(e){remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[3]/div[2]/div[2]')$clickElement()
             })
    Sys.sleep(2)
    
    # webElem <- remDr$findElements("css", "iframe")
    # remDr$switchToFrame(webElem[[1]])
    stacked_text <- NULL
    stacked_qty <- NULL
    numbers <- remDr$findElements('css','.search-filter__option-count')
    Sys.sleep(4)
    for(i in 1:length(numbers)){
        text <- numbers[[i]]$getElementText()
        stacked_qty <- rbind(stacked_qty,text)}
    Sys.sleep(4)
    options <- remDr$findElements('css','.checkbox__option-value')
    for(i in 1:length(options)){
        text <- options[[i]]$getElementText()
        stacked_text <- rbind(stacked_text,text)}
    
    stacked_text <- cbind(stacked_text,stacked_qty)
    stacked_backup <- stacked_text
    #stacked_text <- stacked_backup
    
    stacked_text <- stacked_text %>% as.data.frame() %>% dplyr::slice(-c(1:45))
    rownames(stacked_text) <- seq(nrow(stacked_text))
    cutoff <- which(grepl("^Cards$",stacked_text$V1))
    stacked_text <- tryCatch({stacked_text %>% unnest(cols = c(V1,V2)) %>% dplyr::slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("V1" = "editions", "V2" = "qty"))},error = function(e){stacked_text %>% unnest(cols = c(V1,V2)) %>% dplyr::slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("editions" = "V1", "qty" = "V2"))}) %>% 
        mutate(editions  = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions))))))) %>% mutate(qty = as.numeric(qty))
    stacked_text = stacked_text %>% na.omit()
    groupings <- NULL
    groupings <- as.numeric(stacked_text$qty[1])
    groupings <- rbind(groupings,(stacked_text$qty[1] + stacked_text$qty[2]))
    for(i in 3:nrow(stacked_text)){
        groupings <- rbind(groupings,(groupings[i-1] + stacked_text$qty[i]))
        if(groupings[i-1] >= 9000){groupings[i] = stacked_text$qty[i]}
    }
    groupings <- as.data.frame(groupings) %>% mutate(groups = NA)
    ID = 1
    try(for(i in 1:nrow(groupings)){
        if(groupings$V1[i+1]>=groupings$V1[i]){groupings$groups[i] = ID}else{groupings$groups[i] = ID}
        if(groupings$V1[i+1]<=groupings$V1[i]){ID = ID + 1}
    }, silent = T)
    groupings[nrow(groupings),2] <- max(groupings$groups, na.rm = T)
    
    stacked_text <- stacked_text %>% mutate(groups = groupings$groups,
                                            editions = paste('"',editions,'", ',sep=""))
    bb <- stacked_text
    
    json_body_contents = stacked_text %>% 
        group_by(groups) %>% 
        mutate(new_editions = paste0(unique(unlist(strsplit(editions,split=' '))),collapse=" ")) %>%
        select(new_editions, groups) %>% 
        ungroup() %>% distinct() %>% 
        mutate(editions = new_editions) %>% select(editions)#do(summarise(.),paste0(unique(unlist(strsplit(editions,split=' '))),collapse=" ")) %>% paste0(unique(unlist(strsplit(editions,split=' '))),collapse=" "))
    
    colnames(json_body_contents) <- "stringeroonies"
    json_body_contents$stringeroonies <- gsub(",$","",json_body_contents$stringeroonies)
    coacross <- function(...) {
        coalesce(!!!across(...))
    }
    
    stop_points <- tryCatch({stacked_text %>% group_by(groups) %>% 
            summarise(grp_one = sum(qty[groups == 1]) %>% replace(. == 0, NA),
                      grp_two = sum(qty[groups == 2]) %>% replace(. == 0, NA),
                      grp_three = sum(qty[groups == 3]) %>% replace(. == 0, NA),
                      grp_four = sum(qty[groups == 4]) %>% replace(. == 0, NA),
                      grp_five = sum(qty[groups == 5]) %>% replace(. == 0, NA),
                      grp_six = sum(qty[groups == 6]) %>% replace(. == 0, NA))  %>% mutate(grp_amts = coacross(-groups)) %>% 
            as.data.frame() %>%
            select(groups,grp_amts)}, 
            error = function(e){stacked_text %>% 
                    group_by(groups) %>% summarise(grp_one = sum(qty[groups == 1]) %>% replace(. == 0, NA),
                                                   grp_two = sum(qty[groups == 2]) %>% replace(. == 0, NA),
                                                   grp_three = sum(qty[groups == 3]) %>% replace(. == 0, NA),
                                                   grp_four = sum(qty[groups == 4]) %>% replace(. == 0, NA),
                                                   grp_five = sum(qty[groups == 5]) %>% replace(. == 0, NA),
                                                   grp_six = sum(qty[groups == 6]) %>% replace(. == 0, NA))  %>% mutate(grp_amts = coacross(-groups)) %>% as.data.frame() %>%
                    select(groups,grp_amts)})
    q = 1
    All_TCG_Sets <- NULL
    for(q in 1:max(groupings$groups)){
        tryCatch({
            A <- 0
            B <- 100
            C <- 100
            All_TCG <- NULL
            body <- paste('{
      "algorithm": "salesrel",
      "context": {
            "cart": {},
            "shippingCountry": "US"
                },
      "from": "',A,'",
      "size": "',B,'",
      "filters": {
          "range": {},
          "term": {
              "productLineName": [
                  "magic"
              ],
              "productTypeName": [
                  "Cards"
              ],
              "setName": [
                  ',json_body_contents$stringeroonies[q],'
              ]
          }
      }
  }',
                          sep="")
            A <- B 
            B <- 200
            TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
            TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
            if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
            i = 1
            for(i in 1:C){
                Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
                Set <- TCG_Results_1[[1]]$results[[i]]$setName
                Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
                Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
                MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
                Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
                Max_Quant_Fillable = TCG_Results_1[[1]]$results[[i]]$maxFulfillableQuantity
                MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
                Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
                Direct_Listings <- 0
                hasFoil <- ""
                Total_Copies <- NULL
                Potential_Direct_Copies <- NULL
                limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
                if(limit >0){
                    for(j in 1:limit){
                        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                            Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                            if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                                dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                                Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                            else{
                                Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                                Total_Copies <- rbind(Total_Copies,Copies)
                            }
                        }
                    }
                }
                Potential_Direct_Copies <- sum(Potential_Direct_Copies)
                Total_Copies <- sum(Total_Copies)
                if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
                Line_Item <- cbind(Name,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies,Max_Quant_Fillable)
                All_TCG <- rbind(All_TCG, Line_Item)
            }
            
            for(i in 1:(round(stop_points$grp_amts[q],-2)/100)-3) {
                body <- paste('{
          "algorithm": "salesrel",
              "context": {
                "cart": {},
                "shippingCountry": "US"
                    },
          "from": "',A,'",
          "size": "',B,'",
          "filters": {
              "range": {},
              "term": {
                  "productLineName": [
                      "magic"
                  ],
                  "productTypeName": [
                      "Cards"
                  ],
                  "setName": [
                  ',json_body_contents$stringeroonies[q],'
              ]
              }
          }
      }',
                              sep="")
                A <- A + 100
                B <- 100
                TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
                TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
                # repeat{
                #   if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
                #   if((length(TCG_Results_1[[1]]$results) != 0)) break
                # }
                C = length(TCG_Results_1[[1]]$results)
                if(C == 0)break
                for(i in 1:C){
                    Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
                    Set <- TCG_Results_1[[1]]$results[[i]]$setName
                    Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
                    Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
                    MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
                    Listings <-length(TCG_Results_1[[1]]$results[[i]]$listings)
                    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
                    Max_Quant_Fillable = TCG_Results_1[[1]]$results[[i]]$maxFulfillableQuantity
                    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
                    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
                    Direct_Listings <- 0
                    hasFoil <- ""
                    Total_Copies <- NULL
                    Potential_Direct_Copies <- NULL
                    limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
                    if(limit >0){
                        for(j in 1:limit){
                            if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                                Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                                if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                                    dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                                    Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                                else{
                                    Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                                    Total_Copies <- rbind(Total_Copies,Copies)
                                }
                            }
                        }
                    }
                    Potential_Direct_Copies <- sum(Potential_Direct_Copies)
                    Total_Copies <- sum(Total_Copies)
                    if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
                    Line_Item <- cbind(Name,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies,Max_Quant_Fillable)
                    All_TCG <- rbind(All_TCG, Line_Item)
                    if(A >= round(stop_points$grp_amts[q],-2)) break
                    #round(stop_points$grp_amts[q],-2)
                }
                #if(A >= 9990) break
                Sys.sleep(.25)
            }
        }, error = function(e){print("Loop error")})
        All_TCG_Sets <- rbind(All_TCG_Sets,All_TCG)
    }
    
    output = list(All_TCG_Sets %>% as_tibble())
    return(output)
}
ck_best_sellers = function(){
  
  gc()
  Limit <- data.frame(raw_elements = read_html("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=1") %>% html_nodes("a") %>% html_attr("href")) %>%  filter(grepl("page=",raw_elements)) %>%as.data.frame() %>% dplyr::slice(4:20) %>% lapply(as.character) %>% as.data.frame() %>% mutate(pages = parse_number(str_sub(raw_elements,-5,-1))) %>% mutate(pages = as.numeric(pages)) %>% as.data.frame() %>% select(pages) %>% max()    
  #cl <- makeCluster(2, type = "FORK")
  
  #registerDoParallel(cl)
  Sys.sleep(3)
  Start_Time <- Sys.time()
  CK_Prices_df <- NULL
  for(i in 1:Limit){
    tryCatch({
      tryCatch({
        CK_Results <- GET(paste("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep=""))#, body = body)
        Card <- content(CK_Results,"text") %>% read_html %>% html_nodes(".productDetailTitle") %>% html_text()
        Set <- gsub(" \\([A-Z]\\)$","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())) 
        Rarity <- gsub("\\)","",gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())))
        Price <- as.numeric(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))[seq(1, length(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))),4)])
        key <- paste(Card, Set, Rarity,sep="")
        Results <- data.frame(key,Card,Set,Rarity,Price,i)
        CK_Prices_df <- rbind(CK_Prices_df,Results)
        Sys.sleep(.9)}, error = function(e){
          Sys.sleep(120)
          CK_Results <- GET(paste("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep=""))#, body = body)
          Card <- content(CK_Results,"text") %>% read_html %>% html_nodes(".productDetailTitle") %>% html_text()
          Set <- gsub(" \\([A-Z]\\)$","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())) 
          Rarity <- gsub("\\)","",gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())))
          Price <- as.numeric(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))[seq(1, length(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))),4)])
          key <- paste(Card, Set, Rarity,sep="")
          Results <- data.frame(key,Card,Set,Rarity,Price,i)
          CK_Prices_df <- rbind(CK_Prices_df,Results)
          Sys.sleep(.9)
        })}, error = function(e){
          next}
    )
  }
  
  CK_Smaller_List <- fromJSON("https://api.cardkingdom.com/api/pricelist")                                                       %>% 
    as.data.frame() %>% 
    filter(grepl("^F",data.sku) == F) %>%
    select(data.id,data.name,data.variation,data.edition,data.qty_retail,data.price_buy,data.qty_buying) %>%
    mutate(key = paste(data.name,data.variation,data.edition,sep=""),
           data.qty_retail = as.numeric(data.qty_retail),
           data.price_buy = as.numeric(data.price_buy),
           data.qty_buying = as.numeric(data.qty_buying)) %>%
    select(key,everything())
  
  Anchor_CK_price <- CK_Prices_df[c(3:nrow(CK_Prices_df)),] %>% filter(Price > .01) %>% mutate(i = seq(nrow(.))) %>% arrange(i) %>% select(Price) %>% dplyr::slice(1) %>% as.numeric()
  
  ck_data <- CK_Prices_df[c(3:nrow(CK_Prices_df)),] %>% filter(Price > .01) %>% 
    filter(grepl("\\sToken$",trimws(Card) )==F) %>%
    mutate(i = round(((Price/Anchor_CK_price)*i),5)) %>% arrange(i) %>% mutate(i = seq(nrow(.)))
  
  ck_best_sellers = ck_data %>% 
    mutate(Card = trimws(Card)) %>% 
    separate(Card,into=c("Card","Variation"),sep="\\(") %>% 
    mutate(Card = trimws(Card),
           Variation = gsub("Foil-Etched - ","",gsub("\\)","",Variation)),
           Variation = ifelse(is.na(Variation),"",Variation),
           key = paste(Card,Variation,Set,sep="")) %>%
    left_join(CK_Smaller_List %>% select(key,data.id,data.qty_retail,data.price_buy,data.qty_buying),by=c("key"="key")) %>%
    distinct() %>%
    mutate(rank = seq(nrow(.))) %>%
    #select(data.id,Price,data.qty_retail,data.qty_buying,rank) %>%
    mutate(uuid = mtgjson_roster$uuid[match(data.id, mtgjson_roster$ckid)],
           uuid_2 = mtgjson_roster$uuid[match(data.id, mtgjson_roster$ckid_f)],
           uuid = ifelse(is.na(uuid),uuid_2,uuid)) %>%
    select(-uuid_2) %>% clean_names() %>%
    select(uuid,data_id,price,data_qty_retail,data_price_buy,data_qty_buying,rank) %>%
    rename(qty_retail = data_qty_retail, qty_buying = data_qty_buying, bl_offer = data_price_buy)
  
  undervalued_ck = ck_data %>% 
    mutate(Card = trimws(Card)) %>% 
    separate(Card,into=c("Card","Variation"),sep="\\(") %>% 
    mutate(Card = trimws(Card),
           Variation = gsub("Foil-Etched - ","",gsub("\\)","",Variation)),
           Variation = ifelse(is.na(Variation),"",Variation),
           key = paste(Card,Variation,Set,sep="")) %>%
    left_join(CK_Smaller_List %>% select(key,data.id,data.qty_retail,data.price_buy,data.qty_buying),by=c("key"="key")) %>%
    distinct() %>%
    mutate(rank = seq(nrow(.))) %>%
    #select(data.id,Price,data.qty_retail,data.qty_buying,rank) %>%
    mutate(uuid = mtgjson_roster$uuid[match(data.id, mtgjson_roster$ckid)],
           uuid_2 = mtgjson_roster$uuid[match(data.id, mtgjson_roster$ckid_f)],
           uuid = ifelse(is.na(uuid),uuid_2,uuid),
           ratio = round(data.price_buy/Price,2) ) %>%
    select(-uuid_2) %>% clean_names() %>% 
    mutate(rank_tiles = ntile(rank,20),
           tile = round(rank_tiles - ratio,2),
           est_new_price = round( (ratio - .52)*price ,1) + price ,
           retail_chg = round( ((ratio - .52)*price)+price ,2) - price ) %>% 
    filter(data_qty_buying >= 5 & price >=5 & retail_chg > 0) %>% 
    arrange(desc(retail_chg),tile, desc(rank_tiles)) %>%
    select(uuid,card,set,rarity,price,est_new_price,retail_chg)  %>% drop_na() %>%
    left_join(ban_data[[3]] %>%as_tibble() %>% filter(vendor == 10) %>% filter(hasFoil == 0) %>% select(-Date,-vendor,-hasFoil) %>% distinct() ,by=c("uuid"="uuid")) %>%
    drop_na() %>% mutate(ck_undervalued = ifelse(price <= (retail * 1.2),1,0)) %>%
    filter(ck_undervalued == 1) 
  
  
  
  # ck_data %>% 
  #   mutate(Card = trimws(Card)) %>% 
  #   separate(Card,into=c("Card","Variation"),sep="\\(") %>% 
  #   mutate(Card = trimws(Card),
  #          Variation = gsub("Foil-Etched - ","",gsub("\\)","",Variation)),
  #          Variation = ifelse(is.na(Variation),"",Variation),
  #          key = paste(Card,Variation,Set,sep="")) %>%
  #   left_join(CK_Smaller_List %>% select(key,data.id,data.qty_retail,data.price_buy,data.qty_buying),by=c("key"="key")) %>%
  #   distinct() %>%
  #   mutate(rank = seq(nrow(.))) %>%
  #   #select(data.id,Price,data.qty_retail,data.qty_buying,rank) %>%
  #   mutate(uuid = mtgjson_roster$uuid[match(data.id, mtgjson_roster$ckid)],
  #          uuid_2 = mtgjson_roster$uuid[match(data.id, mtgjson_roster$ckid_f)],
  #          uuid = ifelse(is.na(uuid),uuid_2,uuid),
  #          ratio = round(data.price_buy/Price,2) ) %>%
  #   select(-uuid_2) %>% clean_names() %>% filter(price >= 0 & price <= 1) %>%
  #   summarize(ratio_avg = round(mean(ratio,na.rm=T),2))
  
  output = list(ck_best_sellers,undervalued_ck)
    
  return(output)
}

con <- gaeas_cradle("wolfoftinstreet@gmail.com")

mtgjson_roster = mtgjson_roster_update()

ban_data = ban_data_retrieval(id="mtgjson")

tcg_low_nm_retail = ban_data[[3]] %>% filter(vendor == 10) %>% 
  left_join(mtgjson_roster %>% select(uuid,tcg_ID),by=c("uuid"="uuid")) %>% distinct() %>%
  select(Date,tcg_ID,hasFoil,retail,vendor)

tcg_best_sellers_tbl = tcg_best_sellers()


all_tcg_info = all_tcg_card_info("159.65.219.70")


ban_tcg_data = tcg_low_nm_retail %>% filter(hasFoil != 1) %>%
  left_join(all_tcg_info[[1]] %>% select(Product_ID,Listings),by=c("tcg_ID"="Product_ID")) %>%
  left_join(tcg_best_sellers_tbl %>% filter(hasFoil == "") %>% select(Product_ID,Rank) , by=c("tcg_ID"="Product_ID")) %>%
  left_join(mtgjson_roster %>% select(uuid,tcg_ID) %>% distinct(),by=c("tcg_ID"="tcg_ID")) %>%
  distinct() %>% mutate(hasFoil = 0) %>% select(Date, uuid, hasFoil, retail, Listings, Rank, vendor) %>% arrange(desc(Rank)) %>% 
  filter(!is.na(Listings))

mybq <- bq_table(project = "gaeas-cradle", dataset = "ban_tcg", table = paste(gsub("-","_",Sys.Date()),"_listings",sep=""))
bq_table_upload(x=mybq, values = ban_tcg_data, fields=as_bq_fields(ban_tcg_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")


ban_ck_data = ck_best_sellers()
ban_ck_data_final = ban_ck_data[[1]]

mybq <- bq_table(project = "gaeas-cradle", dataset = "ban_ck", table = paste(gsub("-","_",Sys.Date()),"_rank",sep=""))
bq_table_upload(x=mybq, values = ban_ck_data_final, fields=as_bq_fields(ban_ck_data_final),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("Cardkingdom Opps")

ban_ck_opps = ban_ck_data[[2]]

sheet_write(
  ban_ck_opps,
  ss = ss,
  sheet = "ck"
)
