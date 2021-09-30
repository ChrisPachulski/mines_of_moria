pacman::p_load(ggpmisc,ggrepel,ggpubr,devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest,gmailr,googledrive,janitor,futile.logger)

#Functions & packages####
invisible(right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}) #Recreating the right function from Excel 
invisible(left <- function(text, num_char) {
  substr(text, 1, num_char)
}) #Recreating the left function from Excel 

invisible(gaeas_cradle <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "gaeas-cradle",
    dataset = "premiums",
    billing = "gaeas-cradle"
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
}) # DB connection

invisible(chrome <-function(ip){
  remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr
}) # Selenium driver
#install.packages("pacman")
retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors, 
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}
library(pacman)
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium,lubridate,anytime)

Sets <- read.csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
#View(Sets)
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

# You don't need the roster refresh. I have these in all my scripts to make sure each droplet keeps an updating roster
# Roster Refresh ----------------------------------------------------------

content <- fromJSON("https://mtgjson.com/api/v5/AllPrintings.json")
#library(tidyjson)
sets_of_interest <- content$data %>% names()

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
Entire_Dictionary$Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,Entire_Dictionary$number,sep="")
Entire_Dictionary$Working_Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,sep="")

Shortened_Dictionary <- Entire_Dictionary[c(1,2,3,4,5,8,10,24,6,7,9,13,11,12,22,23)]
Shortened_Dictionary$Key <- paste(Shortened_Dictionary$card,Shortened_Dictionary$set,Shortened_Dictionary$rarity,Shortened_Dictionary$hasFoil,sep="")

setwd("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/")
csvFileName <- paste("C20_Addition",".csv",sep="")
write.csv(Shortened_Dictionary, file=csvFileName, row.names = FALSE)


# Roster Load in ----------------------------------------------------------


tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS//C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
  #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
  rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
  mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))})

Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                        Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                        Semi = paste(name,Set,sep="")) 


# TCG ID Retrievals -------------------------------------------------------

# This snippet will pull every mtg set that TCG has, ensuring as soon as pre-sales begin we capture those sales immediately
remDr <- chrome("138.68.229.207")
remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=magic&page=1")
Sys.sleep(4)
remDr$findElement('xpath','//*[@id="app"]/div/section[2]/div/div[1]/button')$clickElement()
Sys.sleep(4)

tryCatch({remDr$findElement('xpath','/html/body/div[5]/div/div/div/div/button/span')$clickElement()}, 
         error = function(e){print("No msg box popped up")
         })
Sys.sleep(4)
tryCatch({remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[2]/div[2]/div[2]')$clickElement()}, 
         error = function(e){remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[3]/div[2]/div[2]')$clickElement()
         })
Sys.sleep(4)

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
stacked_text <- stacked_text %>% as.data.frame() %>% slice(-c(1:45))
rownames(stacked_text) <- seq(nrow(stacked_text))
cutoff <- which(grepl("^Cards$",stacked_text$V1))
stacked_text <- tryCatch({stacked_text %>% unnest(cols = c(V1,V2)) %>% slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("V1" = "editions", "V2" = "qty"))},error = function(e){stacked_text %>% unnest(cols = c(V1,V2)) %>% slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("editions" = "V1", "qty" = "V2"))}) %>% 
  mutate(api_editions  = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions))))))) %>% mutate(qty = ceiling(as.numeric(qty)/100) ) %>%
  mutate(api_editions = paste('"',api_editions,'"',sep=""))
stacked_text = stacked_text[!grepl( "\"challenger-decks\"",stacked_text$api_editions),]
stacked_text = stacked_text[!grepl("\"spellslinger-starter-kit\"",stacked_text$api_editions),]
stacked_text = stacked_text[!grepl("\"box-sets\"",stacked_text$api_editions),]
stacked_text = stacked_text[!grepl("\"magic-the-gathering-apparel\"",stacked_text$api_editions),]
stacked_text = stacked_text %>% na.omit()
length(stacked_text$editions)

stacked_text %>% view()

# Begin Pull --------------------------------------------------------------
#old = all_data
#Empty dictionary for all the goodies
all_data = NULL
q = 12
tcg_data_grab = function(input = q){
  Sys.sleep(1.5)
  gc()
  Best_Sellers_SR <- NULL 
  A <- 0
  B <- 100
  # You know this part
  for(p in 1:stacked_text$qty[q]){
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
                        ',stacked_text$api_editions[q],'
                    ]
                }
            }
        }',
                  sep="")
    A <- A + 100
    B <- 100
    C <- 100
    TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
    while(TCG_Results$status_code != 200){
      Sys.sleep(4)
      TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
    }
    TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
    C = length(TCG_Results_1[[1]]$results)
    if(C == 0)break
    for(i in 1:C){
      Name <- TCG_Results_1[[1]]$results[[i]]$productName
      Set <- TCG_Results_1[[1]]$results[[i]]$setName
      Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
      Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
      MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
      Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
      MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
      Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
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
      Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 0, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                              Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
      Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
    }
    Sys.sleep(.25)
  }
  
  Best_Sellers_SR = rbind(Best_Sellers_SR,Best_Sellers_SR %>% mutate(isFoil = 1)) %>% distinct()
  
  # Sometimes the requests come in too quickly and tcg will forbid access, or
  # they're working on the backend and it results in a 403 call, account here to just 
  # continues loop if this element might fail
  if(is.null(Best_Sellers_SR)){next}
  # Get a list of tcgIds from whatever source you have
  # I adjust to greater than $5 because I act on this information and I'd like to get it in a timely manner, 
  # entirely arbitrary filter
  tcg_ids_of_interest = Best_Sellers_SR %>% as.data.frame() %>% filter(MKT >=5) %>% select(Product_ID) %>% rename(tcg_id = Product_ID) %>% distinct()
  
  # Some sets don't have any cards greater than $5, continue loop if so
  if(nrow(tcg_ids_of_interest)==0){print(paste("No Cards in",stacked_text$editions[q],"Moving On!"))}
  
  # Offsets should be outside the loop honestly, but here we are
  # Latest sales only come in buckets of 25, you can't extend
  offsets = c(0,25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400,425,450,475,500,525,550,575,600,625,650,675,700,
              725,750,775,800,825,850,875,900,925,950,975,1000)
  
  
  all_set_sales = NULL
  all_line_items = NULL
  all_cards_inventory = NULL
  i = 1
  suppressMessages(for(i in 1:nrow(tcg_ids_of_interest)) {
    #Test loop for qc work
    #suppressMessages(for(i in 1:5) {
    
    Sys.sleep(.005)
    seller_bricks = NULL
    #Test to see if direct inventory exists
    tryCatch({
      body = '{"filters":{"term":{"sellerStatus":"Live","channelId":0,"direct-seller":true},"range":{"quantity":{"gte":1},"directInventory":{"gte":1}},"exclude":{"channelExclusion":0}},"from":0,"size":50,"sort":{"field":"price+shipping","order":"asc"},"context":{"shippingCountry":"US","cart":{}},"aggregations":["listingType"]}'
      
      Sys.sleep(.2)
      direct_check = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/listings",sep=""), content_type_json(), body = body) %>% content("parsed", encoding = "UTF-8")
      
      direct_copies = length(direct_check$results[[1]]$results)}, 
      error = function(e){direct_copies = 0})
    # Grab overall inventory
    body <- paste('{"filters":{"term":{"sellerStatus":"Live","channelId":0},"range":{"quantity":{"gte":1}},"exclude":{"channelExclusion":0}},"from":0,"size":1000,"sort":{"field":"price+shipping","order":"asc"},"context":{"shippingCountry":"US","cart":{}},"aggregations":["listingType"]}',
                  sep="")    
    #
    Sys.sleep(.2)
    all_listings = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/listings",sep=""), content_type_json(), body = body) %>% content("parsed", encoding = "UTF-8")
    if(length(all_listings)==0){Sys.sleep(300)}
    #
    c = length(all_listings$results[[1]]$results)
    #
    if(c == 0){next}
    #
    for(d in 1:c ){
      listings_bricks = map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()
      seller_bricks = rbind(seller_bricks,listings_bricks) %>% as_tibble()}
    
    direct = seller_bricks %>% filter(directSeller == TRUE) %>% 
      group_by(productId,printing,condition) %>% 
      summarize(d_quantity = sum(quantity), d_seller_ct = n()) %>%
      ungroup()
    
    # Combine all and direct
    card_inventory = seller_bricks %>% 
      group_by(productId,printing,condition) %>% 
      summarize(all_quantity = sum(quantity), all_seller_ct = n(), largest_seller = max(quantity)) %>%
      ungroup() %>%
      left_join(direct,by=c("productId"="productId",
                            "printing"="printing",
                            "condition"="condition")) %>%
      replace(is.na(.),0) %>%
      mutate(direct_avail = ifelse(direct_copies == 0,0,1)) %>%
      rename(version = printing) %>%
      mutate(version = ifelse(version == "Foil",1,0),
             condition = ifelse(condition == "Near Mint","NM",
                                ifelse(condition == "Lightly Played", "LP",
                                       ifelse(condition == "Moderately Played","MP",
                                              ifelse(condition == "Heavily Played","HP",
                                                     ifelse(condition == "Damaged","D",""
                                                     )))))) %>% 
      left_join( seller_bricks %>% group_by(productId,printing,condition) %>%
                   summarize(max_single_qty = max(quantity),
                             sellerName = sellerName,
                             score = score + shippingPrice) %>% ungroup() %>%
                   group_by(productId,printing,condition,max_single_qty) %>%
                   summarize(score_to_beat = min(score))%>% ungroup() %>%
                   rename(version = printing) %>%
                   mutate(version = ifelse(version == "Foil",1,0),
                          condition = ifelse(condition == "Near Mint","NM",
                                             ifelse(condition == "Lightly Played", "LP",
                                                    ifelse(condition == "Moderately Played","MP",
                                                           ifelse(condition == "Heavily Played","HP",
                                                                  ifelse(condition == "Damaged","D",""
                                                                  )))))),by=c("productId"="productId","version"="version","condition"="condition"))
    
    all_cards_inventory =rbind(all_cards_inventory,card_inventory)
    
    
    b = 1
    # Retrieve the latest sales for just yesterday
    # Doeesn't make sense to pick today as sales aren't completed
    # And going further back, while possible, very time consuming
    for(b in 1:length(offsets)){
      
      all_sales_for_card = NULL
      
      Sys.sleep(.10)
      
      body = "{}"
      

      recent_sales_raw_list = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales?offset=",offsets[b],"&limit=25",sep=""),content_type_json(),body=body)
      
      
      test_value = 0
      
      if(recent_sales_raw_list$status_code == 500){break}
      if(recent_sales_raw_list$status_code == 403){Sys.sleep(30);next}
      try({if(length(recent_sales_raw_list %>% content("parsed") %>% .[[5]])==0){next}})
      
      try({if(identical(recent_sales_raw_list %>% content("parsed") %>% .[[5]], list()) ){next}})
      
      tryCatch({recent_sales_raw_list = recent_sales_raw_list %>% content("parsed") %>% .[[5]]}, error = function(e){test_value = 1})
      
      if(test_value == 0){
        
        loop_limit = recent_sales_raw_list %>% length()
        
        line_items = NULL
        if(loop_limit > 0){
          for(z in 1:loop_limit){
            line_item = cbind(
              tcg_ids_of_interest$tcg_id[i],
              #recent_sales_raw_list[[z]]$skuId,
              recent_sales_raw_list[[z]]$variant,
              recent_sales_raw_list[[z]]$condition,
              recent_sales_raw_list[[z]]$language,
              recent_sales_raw_list[[z]]$quantity,
              recent_sales_raw_list[[z]]$listingType,
              
              recent_sales_raw_list[[z]]$orderDate,
              recent_sales_raw_list[[z]]$purchasePrice,
              recent_sales_raw_list[[z]]$shippingPrice)
            line_items = rbind(line_items,line_item)
          }
        }
        
        
        line_items = line_items %>% as_tibble() %>% 
          rename(
            tcg_id = V1,
            version = V2,
            condition = V3,
            language = V4,
            sold_quantity = V5,
            listing_type = V6,
            dop = V7,
            sell_price = V8,
            shipping = V9
          )
        
        
        #Let's wrangle this shit!
        cleaned_line_items = line_items %>% 
          as_tibble() %>%
          mutate(dop = anytime(as.character(dop),tz = "EST" ),
                 sold_quantity = as.numeric(sold_quantity),
                 sell_price = as.numeric(sell_price),
                 shipping = as.numeric(shipping)) %>%
          #mutate(dop = anytime(dop) ) %>%
          mutate(sell_price = sell_price + shipping) %>%
          select(-shipping) %>%
          mutate(version = ifelse(version == "Foil",1,0),
                 condition = ifelse(condition == "Near Mint","NM",
                                    ifelse(condition == "Lightly Played", "LP",
                                           ifelse(condition == "Moderately Played","MP",
                                                  ifelse(condition == "Heavily Played","HP",
                                                         ifelse(condition == "Damaged","D",""
                                                         ))))) ,
                 listing_type = ifelse(listing_type=="ListingWithoutPhotos",1,0)
          ) %>% mutate(dop = floor_date(dop,unit="day")) %>%
          filter(dop < Sys.Date())
        
        min_date = cleaned_line_items %>% select(dop) %>% summarize(dop = min(dop)) %>% .[[1]]
        
        if(min_date == Sys.Date()){cleaned_line_items = cleaned_line_items%>% filter(dop < Sys.Date())}
        if(nrow(cleaned_line_items %>% filter(dop == Sys.Date()-1))==0){cleaned_line_items = cleaned_line_items %>% 
          select(tcg_id,version,condition,language) %>% 
          distinct() %>% 
          mutate(sold_quantity = 0,
                 listing_type = 1,
                 dop = Sys.Date()-1,
                 sell_price = 0)}
        if(min_date < Sys.Date() - 1){cleaned_line_items = cleaned_line_items%>% filter(dop > Sys.Date()-2);cleaned_line_items = cleaned_line_items%>% filter(dop < Sys.Date())}
        
        
        condition_item_info = cleaned_line_items %>% group_by(version,condition,dop) %>% summarize(
          #con_orders = n(),
          max_order_size = max(sold_quantity),
          med_order_size = median(sold_quantity)) %>% ungroup() 
        
        cleaned_line_items = cleaned_line_items  %>%
          filter(dop < Sys.Date()) %>%
          group_by(tcg_id,condition,version,dop) %>%
          summarize(sold_quantity = sum(sold_quantity),
                    avg_sell_price = mean(sell_price),
                    orders = n()) %>%
          ungroup() %>%
          left_join(condition_item_info,by=c("version"="version","condition"="condition","dop"="dop")) %>%
          mutate(#con_orders = ifelse(max_order_size == 0,0,con_orders),
                 orders = ifelse(max_order_size == 0,0,orders),
                 loop = i)
        
        all_sales_for_card = rbind(all_sales_for_card,cleaned_line_items) %>% distinct()
        
        all_line_items = rbind(all_line_items,all_sales_for_card)  %>%
          group_by(tcg_id,condition,version,dop) %>%
          summarize(sold_quantity = sum(sold_quantity),
                    avg_sell_price = max(avg_sell_price),
                    orders = sum(orders),
                    max_order_size = max(max_order_size),
                    med_order_size = median(med_order_size),
                    loop = loop) %>% ungroup() %>%
          filter(dop == Sys.Date()-1) %>% distinct()
        
        if(min_date < Sys.Date() - 1){break}
        
      }else{all_line_items = rbind(all_line_items,all_sales_for_card) %>%
        group_by(tcg_id,condition,version,dop) %>%
        summarize(sold_quantity = sum(sold_quantity),
                  avg_sell_price = max(avg_sell_price),
                  orders = sum(orders),
                  max_order_size = max(max_order_size),
                  med_order_size = median(med_order_size),
                  loop = loop) %>% ungroup() %>%
        filter(dop == Sys.Date()-1)  %>% distinct() }
      
      
    }
    
    
    
  })
  # Continue loop again if no results/recent sales/orinventory
  if(is.null(all_line_items)){next}
  
  # Aggregate recent sales
  all_sales_for_card =suppressMessages(all_line_items %>% group_by(tcg_id,version,dop,condition) %>%
                                         summarize(avg_sell_price = mean(avg_sell_price,na.rm=T), 
                                                   sold_quantity  = sum(sold_quantity)          ,
                                                   orders         = sum(orders)                 ,
                                                   #con_orders     = sum(con_orders)             ,
                                                   max_order_size = max(max_order_size)         , 
                                                   med_order_size = max(med_order_size)) %>%
                                         ungroup()) %>%
    filter(avg_sell_price != 0)
  
  
  rankings = Best_Sellers_SR %>% 
    select(Product_ID,isFoil,Vendor_Listings,MKT_EST,MKT) %>%
    mutate(set_rank = seq(nrow(.)))
  
  if(is.null(all_sales_for_card)){all_sales_for_card = data.frame(tcg_id = tcg_ids_of_interest$tcg_id[i],
                                                                  version = 0,
                                                                  condition = "NM", 
                                                                  dop = Sys.Date() -1,
                                                                  "sold_quantity"=0,
                                                                  "avg_sell_price"=0,
                                                                  "orders"=0,
                                                                  "con_orders"=0,
                                                                  "max_order_size"=0,
                                                                  "med_order_size"=0)}
  
  if(nrow(all_sales_for_card)==0){next}
  
  all_sales_for_card %>% filter(version ==1)
  
  #Bring all elements together
  card_review = all_cards_inventory %>% 
    full_join(all_sales_for_card %>% mutate(tcg_id = as.numeric(tcg_id)),
              by=c("productId"="tcg_id",
                   "version"="version",
                   "condition"="condition")) %>%
    fill(dop,.direction=c("updown")) %>%
    replace(is.na(.),0) %>%
    left_join(rankings %>% mutate(Product_ID = as.numeric(Product_ID)),by = c("productId"="Product_ID")) %>%
    rename(Date = dop) %>%
    select(Date, productId, everything()) %>%
    filter(!(all_quantity ==0 & sold_quantity == 0))
  
  
  # Append all info and repeat
  all_set_sales = rbind(all_set_sales,card_review)
  return(all_set_sales)
}

total = length(stacked_text$editions) 
pb <- txtProgressBar(min=0, max = total, style = 3)
Q <- 1

for(q in 1:length(stacked_text$editions)){

  suppressMessages(gc())
  all_set_sales = tryCatch({
                    tryCatch({tcg_data_grab(q)}, 
                         error = function(e){suppressMessages(retry(expr = tcg_data_grab(q), maxErrors = 1, sleep=2))}
                    )},error=function(e){NULL
                  })
  all_data = rbind(all_data,all_set_sales)
  Sys.sleep(sample(.09:.23, 1))
  setTxtProgressBar(pb,Q)
  Q <- Q+1
}

#retry(tcg_data_grab(5),maxErrors = 3, sleep = 2)
#all_data %>% filter(all_quantity != 0) %>% filter(avg_sell_price != 0)

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "gaeas-cradle", dataset = "mtg_churn", table = paste(gsub("-","_",Sys.Date()),"_mtg_churn",sep=""))
bq_table_upload(x=mybq, values = all_data, fields=as_bq_fields(all_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

all_data %>% arrange(desc(orders))
#all_data %>% arrange(desc(orders)) %>% view()

