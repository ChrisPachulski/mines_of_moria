pacman::p_load(tidyverse, lubridate, anytime, bigrquery, rvest, httr,jsonlite,googleAuthR)
patches = read_json("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/personal_data.json")

gaeas_cradle <- function(){
    
    service_account_file = '/home/cujo253/mines_of_moria/Essential_Referential_CSVS/gaeas-cradle.json'
    gar_auth_service(service_account_file)
    
    bq_auth(path = service_account_file)
    
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    #bq_auth(email = patches$patches, use_oob = TRUE)
    options(scipen = 20)
    con
    
}

tcgplayer_best_sellers = function(){
    gc()
    Start_Time <- Sys.time()
    A <- 0
    B <- 100
    C <- 100
    TCG__Best_Sellers <- NULL
    body <- paste('{
    "algorithm": "sales_exp_fields_experiment",
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
    TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body)
    TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
    if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(30);TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
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
          "algorithm": "sales_exp_fields_experiment",
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
            TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body)
            TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
            repeat{
                if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
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
    "algorithm": "sales_exp_fields_experiment",
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
    TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body)
    TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
    if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
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
        "algorithm": "sales_exp_fields_experiment",
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
            TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body)
            TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
            repeat{
                if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(30);TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
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
    return(TCG__Best_Sellers %>% as_tibble())
}

real_tcgplayer_best_sellers = function(){
    con <- gaeas_cradle()
    
    statement = str_glue('
            SELECT a.tcg_id,a.card_name,a.set,a.rarity,a.number,a.version,sum(sell_price) ovr_revenue,sum(sold_quantity) ovr_qty, value as todays_value  
            FROM `gaeas-cradle.mtg_basket.*` a 
            LEFT JOIN (
                    SELECT tcg_id,card_name,a.set,rarity,number,version,avg(sell_price) value  
                    FROM `gaeas-cradle.mtg_basket.*` a 
                    WHERE _TABLE_SUFFIX BETWEEN
                FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 120 DAY)) AND
                FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY))
                GROUP BY tcg_id,card_name,a.set,rarity,number,version
            ) b on b.tcg_id=a.tcg_id and b.card_name=a.card_name and b.set=a.set and b.rarity=a.rarity and b.number=a.number and b.version=a.version
            WHERE _TABLE_SUFFIX BETWEEN
                FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 120 DAY)) AND
                FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) AND
                regexp_contains("(NM|LP)",condition)
            GROUP BY a.tcg_id,a.card_name,a.set,rarity,a.number,a.version, value
        ')
    tcg_churners = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
    
    return(tcg_churners)
}

real_pricing = function(){
    con <- gaeas_cradle()
    
    statement = str_glue('
        SELECT DISTINCT Date as event_date,rdate, vendor, card, b.set, abbr, CASE WHEN a.hasFoil = 1 THEN "FOIL" ELSE "" END as hasFoil, max(mkt_value) mkt_value, CAST(b.tcg_id as int) as tcg_id, b.set as mtgjson_id, rarity,number, types, manacost 
        FROM `gaeas-cradle.ban_retail.{gsub("-","_",Sys.Date()-1)}_retail` a
        LEFT JOIN `gaeas-cradle.roster.mtgjson_ban` b on a.tcg_id=b.uuid
        WHERE a.vendor like "TCG Low"
        GROUP BY Date, rdate, vendor, card, b.set, abbr,hasFoil,tcg_id, b.set, rarity,number, types, manacost
        ')
    proper_retail_data = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
    
    return(proper_retail_data)
}

ck_real_pricing = function(){
    con <- gaeas_cradle()
    
    statement = str_glue('
        SELECT DISTINCT Date as event_date,rdate, vendor, card, b.set, abbr, CASE WHEN a.hasFoil = 1 THEN "FOIL" ELSE "" END as hasFoil, max(mkt_value) ck_mkt_value, CAST(b.tcg_id as int) as tcg_id, b.set as mtgjson_id, rarity,number, types, manacost 
        FROM `gaeas-cradle.ban_retail.{gsub("-","_",Sys.Date()-1)}_retail` a
        LEFT JOIN `gaeas-cradle.roster.mtgjson_ban` b on a.tcg_id=b.uuid
        WHERE a.vendor like "CK"
        GROUP BY Date, rdate, vendor, card, b.set, abbr,hasFoil,tcg_id, b.set, rarity,number, types, manacost
        ')
    proper_retail_data = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
    
    return(proper_retail_data)
}

ck_bl_evaluation = function(){
    con <- gaeas_cradle()
    
    statement = str_glue('
        SELECT DISTINCT Date as event_date,rdate, vendor, card, b.set, abbr, CASE WHEN a.hasFoil = 1 THEN "FOIL" ELSE "" END as hasFoil, max(offer) ck_bl_value, CAST(b.tcg_id as int) as tcg_id, b.set as mtgjson_id, rarity,number, types, manacost 
        FROM `gaeas-cradle.ban_buylist.{gsub("-","_",Sys.Date()-1)}_buylist` a
        LEFT JOIN `gaeas-cradle.roster.mtgjson_ban` b on a.tcg_id=b.uuid
        WHERE a.vendor like "Card Kingdom Buylist"
        GROUP BY Date, rdate, vendor, card, b.set, abbr,hasFoil,tcg_id, b.set, rarity,number, types, manacost
        ')
    ck_buylist_data = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
    
    return(ck_buylist_data)
}

edhrec_ranks = function(){
    goodies = GET('https://json.edhrec.com/static/sets') %>% content() %>% .[2] %>% .[[1]]
 
    expedition_map =  tibble(editions = map(goodies, "name"), codes = map(goodies, "code")) %>% 
       mutate(editions = map_chr(editions, 1),codes = map_chr(codes, 1)) %>% drop_na()

    pb <- txtProgressBar(min   = 0,      
                         max   = nrow(expedition_map), 
                         style = 3,    
                         width = 50,   
                         char  = "?") 
    
    
   azcantas_secrets = NULL    
   for(i in 1:nrow(expedition_map)){
        
       tryCatch({
       opaque_name = GET(str_glue("https://json.edhrec.com/pages/sets/{expedition_map$codes[i]}.json")) %>% content() %>% 
               .[1] %>% .[[1]] %>% .[3] %>% .[[1]] %>% .[[1]] %>% .[[2]] %>% .[1] %>% .[[1]]
           
       azcanta_cavern = tibble(card_name       = map(opaque_name, "name"), 
                        regexed_name    = map(opaque_name, "sanitized"),
                        inclusion       = map(opaque_name, "inclusion"),
                        potential_decks = map(opaque_name, "potential_decks"),
                        label           = map(opaque_name, "label")) %>% 
           
           mutate(card_name       = map_chr(card_name, 1),
                  regexed_name    = map_chr(regexed_name, 1),
                  inclusion       = as.numeric(map_chr(inclusion, 1)),
                  potential_decks = as.numeric(map_chr(potential_decks, 1)),
                  label           = map_chr(label, 1)
                 )
       
       azcantas_secrets = rbind(azcantas_secrets,azcanta_cavern)%>% distinct() },
       error = function(e){print(str_glue('{expedition_map$name[i]} is a bullshit edition, move on!'))})
       
       Sys.sleep(sample(seq(0.1, 1.6, by = 0.1))[1])
       
       setTxtProgressBar(pb, i)
   }
   
   return(azcantas_secrets %>% arrange(desc(inclusion)))
    
 }

