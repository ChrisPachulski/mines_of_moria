install.packages("pacman")
pacman::p_load(httr,jsonlite,tidyverse,bigrquery,RSelenium,rvest,googlesheets4,googledrive,anytime,lubridate,janitor)
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
chrome <-function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr
}
currentDate <- Sys.Date()
drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)

# Set Retrieval for FaB ---------------------------------------------------
remDr <- chrome("64.225.20.203")
remDr$navigate("https://www.tcgplayer.com/search/flesh-and-blood-tcg/product?productLineName=flesh-and-blood-tcg&page=1")
Sys.sleep(4)
remDr$findElement('xpath','//*[@id="app"]/div/section[2]/div/div[1]/button')$clickElement()
Sys.sleep(4)

tryCatch({remDr$findElement('xpath','/html/body/div[5]/div/div/div/div/button/span')$clickElement()}, 
         error = function(e){print("No msg box popped up")
         })
Sys.sleep(2)
tryCatch({
    tryCatch({remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[2]/div[2]/div[2]')$clickElement()}, 
             error = function(e){remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[3]/div[2]/div[2]')$clickElement()
             })}, 
    error = function(e){print("No Drop Down")})
Sys.sleep(2)

# webElem <- remDr$findElements("css", "iframe")
# remDr$switchToFrame(webElem[[1]])
stacked_text <- NULL
stacked_qty <- NULL
page_source = remDr$getPageSource()
stacked_text <- page_source %>% .[[1]] %>% read_html() %>% html_nodes(".checkbox__option-value") %>% html_text() %>% trimws()
stacked_qty = page_source %>% .[[1]] %>% read_html() %>% html_nodes(".search-filter__option-count") %>% html_text() %>% trimws() %>% as.numeric()
Sys.sleep(4)

stacked_text <- cbind(stacked_text,stacked_qty)
stacked_backup <- stacked_text
#stacked_text <- stacked_backup
stacked_text <- stacked_text %>% as_tibble() %>% mutate(stacked_qty = as.numeric(stacked_qty))%>% 
    slice(which.max(stacked_text == "Monarch") : n()) %>% 
    mutate(case = ifelse(stacked_text == "Cards",1,NA)) %>%
    fill(.,case,.direction = c("down")) %>% 
    filter(is.na(case)) %>%
    select(-case) %>%
    rename("editions"="stacked_text", "qty"="stacked_qty") %>%
    mutate(editions = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions)))))))

stacked_text = stacked_text %>% na.omit()
q = 1
tcg_data_grab = function(input = q){
    Sys.sleep(1.5)
    gc()
    Best_Sellers_SR <- NULL 
    A <- 0
    B <- 100
    # You know this part
    p = 1
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
                        "flesh-and-blood-tcg"
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
            Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
            Version <- gsub("\\(","",gsub("\\)","",str_extract(Name,"\\([A-Za-z]+\\)")))
            Name <- trimws(gsub("\\([A-Za-z]+\\)","",Name))
            Set <- TCG_Results_1[[1]]$results[[i]]$setName
            Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
            Number <- str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")
            MKT_EST <- if(is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice)==T){0}else{TCG_Results_1[[1]]$results[[i]]$marketPrice}
            Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
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
            
            description = TCG_Results_1[[1]]$results[[i]]$customAttributes$description
            if(is.null(description)){description = NA}
            class = TCG_Results_1[[1]]$results[[i]]$customAttributes$class %>% unlist()
            if(length(class)>1){class = paste(c(class[1],class[2]), collapse=' - ' )}
            if(is.null(class)){class = NA}
            cost = TCG_Results_1[[1]]$results[[i]]$customAttributes$cost
            if(is.null(cost)){cost = NA}
            pitchValue = TCG_Results_1[[1]]$results[[i]]$customAttributes$pitchValue
            if(is.null(pitchValue)){pitchValue = NA}
            defenseValue = TCG_Results_1[[1]]$results[[i]]$customAttributes$defenseValue
            if(is.null(defenseValue)){defenseValue = NA}
            power = TCG_Results_1[[1]]$results[[i]]$customAttributes$power
            if(is.null(power)){power = NA}
            cardType = TCG_Results_1[[1]]$results[[i]]$customAttributes$cardType %>% unlist()
            if(length(cardType)>1){cardType = paste(c(cardType[1],cardType[2]), collapse=' - ' )}
            if(is.null(cardType)){cardType = NA}
            cardSubType = TCG_Results_1[[1]]$results[[i]]$customAttributes$cardSubType
            if(is.null(cardSubType)){cardSubType = NA}
            
            Line_Item <- cbind(Name,Version,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,
                               Total_Copies,class,cost,pitchValue,defenseValue,power,cardType,cardSubType,description)
            Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
        }
        Sys.sleep(.25)
    }
    Best_Sellers_SR = as_tibble(Best_Sellers_SR) %>% 
        mutate(hasFoil = ifelse(hasFoil=="",0,1)) %>%
        mutate(across(Number:Total_Copies,as.numeric)) %>%
        mutate(across(pitchValue:power,as.numeric))
    
    Best_Sellers_SR = rbind(Best_Sellers_SR,Best_Sellers_SR %>% mutate(hasFoil = 1)) %>% distinct()

    # Sometimes the requests come in too quickly and tcg will forbid access, or
    # they're working on the backend and it results in a 403 call, account here to just 
    # continues loop if this element might fail
    if(is.null(Best_Sellers_SR)){next}
    # Get a list of tcgIds from whatever source you have
    # I adjust to greater than $5 because I act on this information and I'd like to get it in a timely manner, 
    # entirely arbitrary filter
    tcg_ids_of_interest = Best_Sellers_SR %>% as.data.frame() %>% filter(MKT >= .5) %>% select(Product_ID) %>% rename(tcg_id = Product_ID) %>% distinct()
    
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
            rename(version = printing) %>%
            mutate(direct_avail = ifelse(direct_copies == 0,0,1))%>%
            mutate(hasFoil = ifelse(grepl("Rainbow Foil",version),1,0),
                   hasFoil = ifelse(grepl("Cold Foil",hasFoil),2,hasFoil),
                   version = gsub("\\s(Rainbow|Cold|Normal).*","",version))  %>%
            select(productId,version,hasFoil,everything()) %>%
            mutate(condition = ifelse(condition == "Near Mint","NM",
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
                           #mutate(direct_avail = ifelse(direct_copies == 0,0,1))%>%
                           mutate(hasFoil = ifelse(grepl("Rainbow Foil",version),1,0),
                                  hasFoil = ifelse(grepl("Cold Foil",hasFoil),2,hasFoil),
                                  version = gsub("\\s(Rainbow|Cold|Normal).*","",version)) %>%
                           mutate(condition = ifelse(condition == "Near Mint","NM",
                                                     ifelse(condition == "Lightly Played", "LP",
                                                            ifelse(condition == "Moderately Played","MP",
                                                                   ifelse(condition == "Heavily Played","HP",
                                                                          ifelse(condition == "Damaged","D",""
                                                                          )))))),by=c("productId"="productId","version"="version","hasFoil"="hasFoil","condition"="condition"))
        
        all_cards_inventory =rbind(all_cards_inventory,card_inventory)
        
        
        b = 2
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
                    mutate(hasFoil = ifelse(grepl("Rainbow Foil",version),1,0),
                           hasFoil = ifelse(grepl("Cold Foil",hasFoil),2,0),
                           version = gsub("\\s(Rainbow|Cold|Normal).*","",version)) %>%
                    select(tcg_id,version,hasFoil,everything()) %>%
                    mutate(condition = ifelse(condition == "Near Mint","NM",
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
                    select(tcg_id,version,hasFoil,condition,language) %>% 
                    distinct() %>% 
                    mutate(sold_quantity = 0,
                           listing_type = 1,
                           dop = Sys.Date()-1,
                           sell_price = 0)}
                if(min_date < Sys.Date() - 1){cleaned_line_items = cleaned_line_items%>% filter(dop > Sys.Date()-2);cleaned_line_items = cleaned_line_items%>% filter(dop < Sys.Date())}
                
                
                condition_item_info = cleaned_line_items %>% group_by(tcg_id,version,hasFoil,condition,dop) %>% summarize(
                    #con_orders = n(),
                    max_order_size = max(sold_quantity),
                    med_order_size = median(sold_quantity)) %>% ungroup() 
                
                cleaned_line_items = cleaned_line_items  %>%
                    filter(dop < Sys.Date()) %>%
                    group_by(tcg_id,condition,hasFoil,version,dop) %>%
                    summarize(sold_quantity = sum(sold_quantity),
                              avg_sell_price = mean(sell_price),
                              orders = n()) %>%
                    ungroup() %>%
                    left_join(condition_item_info,by=c("tcg_id"="tcg_id",
                                                       "hasFoil"="hasFoil",
                                                       "version"="version",
                                                       "condition"="condition",
                                                       "dop"="dop")) %>%
                    mutate(#con_orders = ifelse(max_order_size == 0,0,con_orders),
                        orders = ifelse(max_order_size == 0,0,orders),
                        loop = i)
                
                all_sales_for_card = rbind(all_sales_for_card,cleaned_line_items) %>% distinct()
                
                all_line_items = rbind(all_line_items,all_sales_for_card)  %>%
                    group_by(tcg_id,condition,hasFoil,version,dop) %>%
                    summarize(sold_quantity = sum(sold_quantity),
                              avg_sell_price = max(avg_sell_price),
                              orders = sum(orders),
                              max_order_size = max(max_order_size),
                              med_order_size = median(med_order_size),
                              loop = loop) %>% ungroup() %>%
                    filter(dop == Sys.Date()-1) %>% distinct()
                
                if(min_date < Sys.Date() - 1){break}
                
            }else{all_line_items = rbind(all_line_items,all_sales_for_card) %>%
                group_by(tcg_id,condition,hasFoil,version,dop) %>%
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
    all_sales_for_card =suppressMessages(all_line_items %>% group_by(tcg_id,condition,hasFoil,version,dop) %>%
                                             summarize(avg_sell_price = mean(avg_sell_price,na.rm=T), 
                                                       sold_quantity  = sum(sold_quantity)          ,
                                                       orders         = sum(orders)                 ,
                                                       #con_orders     = sum(con_orders)             ,
                                                       max_order_size = max(max_order_size)         , 
                                                       med_order_size = max(med_order_size)) %>%
                                             ungroup()) %>%
        filter(avg_sell_price != 0)
    
    
    rankings = Best_Sellers_SR %>% 
        select(Product_ID,Version,hasFoil,Listings,MKT_EST,MKT) %>%
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
    
    #Bring all elements together
    card_review = all_cards_inventory %>%
        left_join(all_sales_for_card %>% mutate(tcg_id = as.numeric(tcg_id)),
                  by=c("productId"="tcg_id",
                       "version"="version",
                       "hasFoil"="hasFoil",
                       "condition"="condition"))  %>%
        distinct() %>%
        fill(dop,.direction=c("updown")) %>%
        replace(is.na(.),0) %>%
        left_join(rankings %>% mutate(Product_ID = as.numeric(Product_ID)),by = c("productId"="Product_ID","hasFoil"="hasFoil")) %>%
        rename(Date = dop) %>%
        select(Date, productId, everything()) %>%
        filter(!(all_quantity ==0 & sold_quantity == 0))

    
    # Append all info and repeat
    all_set_sales = rbind(all_set_sales,card_review)
    return(all_set_sales)
}
roster_grab = function(input = q){
    Sys.sleep(1.5)
    gc()
    Best_Sellers_SR <- NULL 
    A <- 0
    B <- 100
    # You know this part
    p = 1
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
                        "flesh-and-blood-tcg"
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
            Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
            Version <- gsub("\\(","",gsub("\\)","",str_extract(Name,"\\([A-Za-z]+\\)")))
            Name <- trimws(gsub("\\([A-Za-z]+\\)","",Name))
            Set <- TCG_Results_1[[1]]$results[[i]]$setName
            Rarity <- TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName
            Number <- str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")
            MKT_EST <- if(is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice)==T){0}else{TCG_Results_1[[1]]$results[[i]]$marketPrice}
            Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
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
            
            description = TCG_Results_1[[1]]$results[[i]]$customAttributes$description
            if(is.null(description)){description = NA}
            class = TCG_Results_1[[1]]$results[[i]]$customAttributes$class %>% unlist()
            if(length(class)>1){class = paste(c(class[1],class[2]), collapse=' - ' )}
            if(is.null(class)){class = NA}
            cost = TCG_Results_1[[1]]$results[[i]]$customAttributes$cost
            if(is.null(cost)){cost = NA}
            pitchValue = TCG_Results_1[[1]]$results[[i]]$customAttributes$pitchValue
            if(is.null(pitchValue)){pitchValue = NA}
            defenseValue = TCG_Results_1[[1]]$results[[i]]$customAttributes$defenseValue
            if(is.null(defenseValue)){defenseValue = NA}
            power = TCG_Results_1[[1]]$results[[i]]$customAttributes$power
            if(is.null(power)){power = NA}
            cardType = TCG_Results_1[[1]]$results[[i]]$customAttributes$cardType %>% unlist()
            if(length(cardType)>1){cardType = paste(c(cardType[1],cardType[2]), collapse=' - ' )}
            if(is.null(cardType)){cardType = NA}
            cardSubType = TCG_Results_1[[1]]$results[[i]]$customAttributes$cardSubType
            if(is.null(cardSubType)){cardSubType = NA}
            
            Line_Item <- cbind(Name,Version,Set,Rarity,Number,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,
                               Total_Copies,class,cost,pitchValue,defenseValue,power,cardType,cardSubType,description)
            Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
        }
        Sys.sleep(.25)
    }
    Best_Sellers_SR = as_tibble(Best_Sellers_SR) %>% 
        mutate(hasFoil = ifelse(hasFoil=="",0,1)) %>%
        mutate(across(Number:Total_Copies,as.numeric)) %>%
        mutate(across(pitchValue:power,as.numeric))
    
    Best_Sellers_SR = rbind(Best_Sellers_SR,Best_Sellers_SR %>% mutate(hasFoil = 1)) %>% distinct()
    
    roster_bucket = Best_Sellers_SR %>% rename(tcg_id = Product_ID) %>%
        clean_names() %>%
        mutate(hasFoil = ifelse(grepl("Rainbow Foil",version),1,0),
               hasFoil = ifelse(grepl("Cold Foil",hasFoil),2,0),
               version = gsub("\\s(Rainbow|Cold|Normal).*","",version)) %>% 
        select(tcg_id,version,hasFoil,everything()) %>%
        select(tcg_id,
               version,
               name,
               set,
               rarity,
               class,
               cost,
               pitch_value,
               defense_value,
               power,
               card_type,
               card_sub_type,
               description)
    ovr_roster = rbind(ovr_roster,roster_bucket)
    
    return(ovr_roster)
}

ovr_roster = NULL
all_data = NULL
for(q in 1:length(stacked_text$editions)){
    
    suppressMessages(gc())
    all_set_sales = tryCatch({
        tryCatch({tcg_data_grab(q)}, 
                 error = function(e){suppressMessages(retry(expr = tcg_data_grab(q), maxErrors = 1, sleep=2))}
        )},error=function(e){NULL
        })
    roster_bucket = roster_grab(q)
    
    ovr_roster = rbind(ovr_roster,roster_bucket)
    all_data = rbind(all_data,all_set_sales)
    Sys.sleep(sample(.29:1.63, 1))
    setTxtProgressBar(pb,Q)
    Q <- Q+1
}

# DB Export ---------------------------------------------------------------

all_data = all_data %>% rename(color = Version) %>% clean_names()

con <- gaeas_cradle("wolfoftinstreet@gmail.com")


mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("fabroster",sep=""))
bq_table_upload(x=mybq, values = ovr_roster, fields=as_bq_fields(ovr_roster),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB Roster Upload Successful!")

# mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_best_sellers", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
# bq_table_upload(x=mybq, values = TCG__Best_Sellers, fields=as_bq_fields(TCG__Best_Sellers),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
# print("BQ FAB best sellers Upload Successful!")
# 
# mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_scorecard", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
# bq_table_upload(x=mybq, values = ovr_score_card, fields=as_bq_fields(ovr_score_card),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
# print("BQ FAB Scorecard Upload Successful!")
# 
# mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_granular", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
# bq_table_upload(x=mybq, values = final_granular, fields=as_bq_fields(final_granular),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
# print("BQ FAB Granular Upload Successful!")

mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_churn", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
bq_table_upload(x=mybq, values = all_data, fields=as_bq_fields(all_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB Churn Upload Successful!")
