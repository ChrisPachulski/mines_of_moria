pacman::p_load(lubridate,anytime,tidyverse,rvest,jsonlite,anytime,httr,RSelenium,bigrquery,googlesheets4,googledrive,googlesheets,janitor)
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
})
invisible(chrome <-function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr
})

get_tcgp_sl_roster = function(){
    remDr <- chrome("159.203.123.73")
    remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=magic")
    Sys.sleep(4)
    try({remDr$findElement('xpath','//*[@id="app"]/div/section[2]/div/div[1]/button')$clickElement()})
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
        slice(which.max(stacked_text == "Prerelease Cards") : n()) %>% 
        mutate(case = ifelse(stacked_text == "Cards",1,NA)) %>%
        fill(.,case,.direction = c("down")) %>% 
        filter(is.na(case)) %>%
        select(-case) %>%
        rename("editions"="stacked_text", "qty"="stacked_qty") %>%
        mutate(editions = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions)))))))
    
    stacked_text = stacked_text %>% na.omit() %>% 
        mutate(api_editions  = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions))))))) %>% mutate(qty = ceiling(as.numeric(qty)/100) ) %>%
        mutate(api_editions = paste('"',api_editions,'"',sep=""))
    
    stacked_text = stacked_text %>% filter(grepl("secret.*lair",editions))
    
    return(stacked_text)
}
tcg_data_grab = function(){
    best_sellers = NULL
    tcg_ids_of_interest = NULL
    desired_range = range(1:nrow(stacked_text))
    #q = 2
    for(q in desired_range){
        Sys.sleep(1.5)
        gc()
        Best_Sellers_SR <- NULL 
        A <- 0
        B <- 100
        # You know this part
        Sys.sleep(2)
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
                        "Sealed Products", "Cards"
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
            i = 33
            for(i in 1:C){
                Name <- TCG_Results_1[[1]]$results[[i]]$productName
                Set <- TCG_Results_1[[1]]$results[[i]]$setName
                Rarity <- if(is.null(TCG_Results_1[[1]]$results[[i]]$rarityName)==T){"S"}else{ TCG_Results_1[[1]]$results[[i]]$rarityName}
                Number <- if(identical(as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+")), numeric(0)) ){NA}else{as.numeric(str_extract(TCG_Results_1[[1]]$results[[i]]$customAttributes$number,"\\d+"))}
                MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
                Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
                MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
                Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
                
                printing <- if(TCG_Results_1[[1]]$results[[i]]$sealed == T){"Sealed"}else{NA}
                
                printing = if(is.na(printing)){
                    if(TCG_Results_1[[1]]$results[[i]]$foilOnly == T){"Foil Only"}else{"Both"}
                } else(printing)
                
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
                
                if(printing == "Sealed"){
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 2, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)}
                
                if(printing == "Foil Only"){
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 1, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)}
                
                if(printing == "Both"){
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 1, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
                    
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 0, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
                    
                }
                
            }
            Sys.sleep(.25)
        }
        
        best_sellers = rbind(best_sellers,Best_Sellers_SR)
        #Best_Sellers_SR = rbind(Best_Sellers_SR,Best_Sellers_SR #%>% mutate(isFoil = 1)) %>% distinct()
        
        # Sometimes the requests come in too quickly and tcg will forbid access, or
        # they're working on the backend and it results in a 403 call, account here to just 
        # continues loop if this element might fail
        if(is.null(Best_Sellers_SR)){next}
        # Get a list of tcgIds from whatever source you have
        # I adjust to greater than $5 because I act on this information and I'd like to get it in a timely manner, 
        # entirely arbitrary filter
        tcg_id_of_interest = Best_Sellers_SR %>% as.data.frame() %>% #filter(MKT >=1) %>% 
            select(Product_ID) %>% rename(tcg_id = Product_ID) %>% distinct()
        
        tcg_ids_of_interest= rbind(tcg_id_of_interest,tcg_ids_of_interest)
        
        # Some sets don't have any cards greater than $5, continue loop if so
        if(nrow(tcg_ids_of_interest)==0){print(paste("No Cards in",stacked_text$editions[q],"Moving On!"))}
        
    }
    # Offsets should be outside the loop honestly, but here we are
    # Latest sales only come in buckets of 25, you can't extend
    offsets = NULL
    start = 0
    for(i in 1:10000){
        offsets = rbind(offsets,start)
        start = start + 25
    }
    
    all_sales = NULL
    all_set_sales = NULL
    all_line_items = NULL
    all_cards_inventory = NULL
    i = 2
    #tcg_ids_of_interest[1,] = 205238
    suppressMessages(for(i in 1:nrow(tcg_ids_of_interest)) {
        #Test loop for qc work
        #suppressMessages(for(i in 1:5) {
        
        Sys.sleep(.25)
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
            listings_bricks = tryCatch({map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()%>% select(-soldDate,-listedDate)}, error = function(e){map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()})
            
            seller_bricks = rbind(seller_bricks,listings_bricks) %>% as_tibble()}
        
        direct = seller_bricks %>% filter(directSeller == TRUE) %>% 
            group_by(productId,printing,condition) %>% 
            summarize(d_quantity = sum(quantity), d_seller_ct = n()) %>%
            ungroup()
        
        # Combine all and direct
        card_inventory = seller_bricks %>% 
            #filter(languageAbbreviation == "EN") %>%
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
        
        
        # Retrieve the latest sales for just yesterday
        # Doeesn't make sense to pick today as sales aren't completed
        # And going further back, while possible, very time consuming
        b = 1
        
        for(b in 1:nrow(offsets)){
            
            all_sales_for_card_inside_loop = NULL
            
            Sys.sleep(.1)
            
            body = paste0('{listingType: "All", offset: ',offsets[b],', limit: 25}')
            
            
            recent_sales_raw_list = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep=""),content_type_json(),body=body)
            
            test_value = 0
            
            if(recent_sales_raw_list$status_code == 500){break}
            if(recent_sales_raw_list$status_code == 400){break}
            if(recent_sales_raw_list$status_code == 403){Sys.sleep(30);next}
            
            
            if(identical(recent_sales_raw_list %>% content("parsed") %>% .[[5]],list())){break}
            if(length(recent_sales_raw_list %>% content("parsed") %>% .[[5]])==0){break}
            
            tryCatch({overall_sales_number = recent_sales_raw_list %>% content("parsed") %>% .[[4]]; recent_sales_raw_list = recent_sales_raw_list %>% content("parsed") %>% .[[5]]}, error = function(e){test_value = 1})
            
            
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
                
                if(min_date < Sys.Date() - 30){cleaned_line_items = cleaned_line_items%>% filter(dop > Sys.Date()-31);cleaned_line_items = cleaned_line_items%>% filter(dop < Sys.Date())}
                
                
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
                        all_order_window = overall_sales_number)
                
                all_sales_for_card_inside_loop = rbind(all_sales_for_card_inside_loop,cleaned_line_items) %>% distinct()
                
                all_line_items = rbind(all_line_items,all_sales_for_card_inside_loop)  %>%
                    group_by(tcg_id,condition,version,dop) %>%
                    summarize(sold_quantity = sum(sold_quantity),
                              avg_sell_price = max(avg_sell_price),
                              orders = sum(orders),
                              max_order_size = max(max_order_size),
                              med_order_size = median(med_order_size),
                              all_order_window = all_order_window) %>% ungroup() %>%
                    #filter(dop == Sys.Date()-1) %>% 
                    distinct()
                
                if(min_date < Sys.Date() - 30){break}
                
            }else{all_line_items = rbind(all_line_items,all_sales_for_card_inside_loop) %>%
                group_by(tcg_id,condition,version,dop) %>%
                summarize(sold_quantity = sum(sold_quantity),
                          avg_sell_price = max(avg_sell_price),
                          orders = sum(orders),
                          max_order_size = max(max_order_size),
                          med_order_size = median(med_order_size),
                          all_order_window = all_order_window) %>% ungroup() %>%
                #filter(dop == Sys.Date()-1)  %>%
                distinct() }
            
            
        }
        
        if(is.null(all_line_items)){next}
        if(is.null(all_sales_for_card_inside_loop)){next}
        
        # Aggregate recent sales
        all_sales_for_card =suppressMessages(all_line_items %>% group_by(tcg_id,version,dop,condition) %>%
                                                 summarize(avg_sell_price = mean(avg_sell_price,na.rm=T), 
                                                           sold_quantity  = sum(sold_quantity)          ,
                                                           orders         = sum(orders)                 ,
                                                           all_order_window = all_order_window          ,
                                                           max_order_size = max(max_order_size)         , 
                                                           med_order_size = max(med_order_size)) %>%
                                                 ungroup()) %>%
            filter(avg_sell_price != 0) 
        
        
        card_dimensions = all_sales_for_card %>%  
            filter(tcg_id == tcg_ids_of_interest$tcg_id[i]) %>%
            filter(condition == "NM" | condition == "") %>% 
            select(tcg_id,version) %>% 
            distinct() %>% 
            mutate(tcg_id = as.numeric(tcg_id))
        
        if(is.null(card_dimensions)){next}
        
        abcd = 1
        tryCatch({
        for(abcd in 1:length(card_dimensions$version)){
            NEW_MKT = all_sales_for_card %>% 
                filter(tcg_id == tcg_ids_of_interest$tcg_id[i]) %>%
                filter(version == card_dimensions$version[abcd]) %>%
                filter(dop == max(dop)) %>%
                select(avg_sell_price) %>%
                unlist()
            
            est_row_check = nrow(all_sales_for_card %>% 
                                       filter(tcg_id == tcg_ids_of_interest$tcg_id[i]) %>%
                                       filter(version == card_dimensions$version[abcd]) )
            
            if(est_row_check < 5 ){
                NEW_MKT_EST = all_sales_for_card %>% 
                    filter(tcg_id == tcg_ids_of_interest$tcg_id[i]) %>%
                    filter(version == card_dimensions$version[abcd]) %>%
                    .[(nrow(.)-est_row_check):nrow(.),] %>%
                    mutate(placeholder = avg_sell_price * sold_quantity) %>%
                    summarize(tcg_id = tcg_id,
                              version = version,
                              mkt_est = round(sum(placeholder)/sum(sold_quantity),2)) %>%
                    distinct() %>%
                    select(mkt_est) %>%
                    unlist()
            }else{
                NEW_MKT_EST = all_sales_for_card %>% 
                    filter(tcg_id == tcg_ids_of_interest$tcg_id[i]) %>%
                    filter(version == card_dimensions$version[abcd]) %>%
                    .[(nrow(.)-4):nrow(.),] %>%
                    mutate(placeholder = avg_sell_price * sold_quantity) %>%
                    summarize(tcg_id = tcg_id,
                              version = version,
                              mkt_est = round(sum(placeholder)/sum(sold_quantity),2)) %>%
                    distinct() %>%
                    select(mkt_est) %>%
                    unlist()
            }
            
            rankings = best_sellers %>% 
                rename(version = isFoil) %>%
                select(Product_ID,version,Vendor_Listings,MKT_EST,MKT) %>%
                mutate(MKT = ifelse( (Product_ID == card_dimensions$tcg_id) & (version == card_dimensions$version[abcd]),NEW_MKT, MKT ), 
                       MKT_EST = ifelse( (Product_ID == card_dimensions$tcg_id) & (version == card_dimensions$version[abcd]),NEW_MKT_EST, MKT_EST ))
        }
        },error=function(e){print(paste0("No Sales for ",tcg_ids_of_interest$tcg_id[i]," in the past 30 days."))})
        
        if(is.null(all_sales_for_card)){next}
        
        if(nrow(all_sales_for_card)==0){next}
        
        
        #Bring all elements together
        card_review = all_cards_inventory %>% 
            filter(productId == tcg_ids_of_interest$tcg_id[i]) %>%
            full_join(all_sales_for_card %>% mutate(tcg_id = as.numeric(tcg_id)),
                      by=c("productId"="tcg_id",
                           "version"="version",
                           "condition"="condition")) %>%
            filter(!is.na(dop)) %>%
            fill(dop,.direction=c("updown")) %>%
            replace(is.na(.),0) %>%
            #filter(productId != 231816) %>%
            left_join(rankings %>% mutate(Product_ID = as.numeric(Product_ID)),by = c("productId"="Product_ID", "version"="version")) %>%
            rename(Date = dop) %>%
            select(Date, productId, everything()) %>%
            filter(!(all_quantity ==0 & sold_quantity == 0)) %>% distinct() %>%
            filter(!is.na(Vendor_Listings))
        
        # Append all info and repeat
        all_set_sales = rbind(all_set_sales,card_review) %>% distinct()
        
        all_sales = rbind(all_sales,all_set_sales) %>% distinct()
        
        paste(i,nrow(tcg_ids_of_interest),scales::percent(i/nrow(tcg_ids_of_interest)))
        
    })
    
    #all_sales = rbind(all_sales,all_set_sales) %>% distinct()
    
    # Continue loop again if no results/recent sales/orinventory
    all_values = list(all_sales,best_sellers)
    
    return(all_values)
}

ban_set_api_bl=function(edition){
    BAN_SL_data = GET(paste0("https://www.mtgban.com/api/mtgban/buylist/",edition,".json?id=tcg&sig=QVBJPUFMTF9BQ0NFU1MmQVBJbW9kZT1hbGwmRXhwaXJlcz0xNjY1NjEzODA2JlNpZ25hdHVyZT04NVljMHhORUdXaXduQVNVSDUwQ0NUSE1ZR1UlM0QmVXNlckVtYWlsPUtxJTJCWUslNURHcnN%2BaDNxRTQ%3D"),content_type_json()) %>% 
        content("parsed")
    
    full_item_tbl = NULL
    
    for(aa in 1:length(BAN_SL_data$buylist %>% names())){
        for(bbb in 1:length(BAN_SL_data$buylist[[aa]] %>% names())){ 
            for(cccc in 1:length(BAN_SL_data$buylist[[aa]] %>% .[[bbb]] %>% names())){
                
                uuid = BAN_SL_data$buylist %>% .[aa] %>% names()
                vendor = BAN_SL_data$buylist[[aa]] %>% .[bbb] %>% names()
                type = BAN_SL_data$buylist[[aa]] %>% .[[bbb]] %>% .[cccc] %>% names()
                value = BAN_SL_data$buylist[[aa]] %>% .[[bbb]] %>% .[[cccc]]
                
                line_item = cbind(vendor,uuid,type,value)
                
                full_item_tbl = rbind(full_item_tbl,line_item)
            }
        }
    }
    
    
    buylist_master_tbl = full_item_tbl %>% as_tibble() %>%
        mutate(value = as.numeric(value))  %>% 
        mutate(id = ifelse(grepl("^ABU$",vendor),1,
                           ifelse(grepl("^CK$",vendor),2,
                                  ifelse(grepl("^CSI$",vendor),3,
                                         ifelse(grepl("^MS$",vendor),4,
                                                ifelse(grepl("^SCG$",vendor),5,
                                                       ifelse(grepl("^TAT$",vendor),6,
                                                              ifelse(grepl("^TCGMkt$",vendor),7,
                                                                     ifelse(grepl("^95$",vendor),8,
                                                                            ifelse(grepl("^CS$",vendor),9,
                                                                                   ifelse(grepl("^MMTG$",vendor),10,
                                                                                          ifelse(grepl("^SZ$",vendor),11,
                                                                                                 ifelse(grepl("^HA$",vendor),12,
                                                                                                        ifelse(grepl("^BP$",vendor),13,0))))))))))))) ) %>%
        
        mutate_if(is.character,as.factor) %>%
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
                                                                                                                 )))))))))))))) %>%
        rename(hasFoil=type) %>%
        mutate(hasFoil = ifelse(hasFoil == "foil",1,ifelse(hasFoil == "etched",2,0)),
               Date = ymd(Sys.Date())) %>%
        select(Date,everything()) %>%
        select(-vendor) %>%
        rename(tcg_id = uuid, vendor = description,offer = value)
    
    return(buylist_master_tbl)
}
ban_set_api_mkt=function(edition){
    BAN_data = GET(paste0("https://www.mtgban.com/api/mtgban/retail/",edition,".json?id=tcg&sig=QVBJPUFMTF9BQ0NFU1MmQVBJbW9kZT1hbGwmRXhwaXJlcz0xNjY1NjEzODA2JlNpZ25hdHVyZT04NVljMHhORUdXaXduQVNVSDUwQ0NUSE1ZR1UlM0QmVXNlckVtYWlsPUtxJTJCWUslNURHcnN%2BaDNxRTQ%3D"),content_type_json()) %>% 
        content("parsed")
    
    full_item_tbl = NULL
    for(aa in 1:length(BAN_data$retail %>% names())){
        for(bbb in 1:length(BAN_data$retail[[aa]] %>% names())){ 
            for(cccc in 1:length(BAN_data$retail[[aa]] %>% .[[bbb]] %>% names())){
                
                uuid = BAN_data$retail %>% .[aa] %>% names()
                vendor = BAN_data$retail[[aa]] %>% .[bbb] %>% names()
                type = BAN_data$retail[[aa]] %>% .[[bbb]] %>% .[cccc] %>% names()
                value = BAN_data$retail[[aa]] %>% .[[bbb]] %>% .[[cccc]]
                
                line_item = cbind(vendor,uuid,type,value)
                
                full_item_tbl = rbind(full_item_tbl,line_item)
            }
        }
    }
    
    retail_master_tbl = full_item_tbl %>% as_tibble() %>%
        mutate(value = as.numeric(value))  %>% 
        mutate(id = ifelse(grepl("^CK$",vendor),1,
                           ifelse(grepl("^CT$",vendor),2,
                                  ifelse(grepl("^MKM Low$",vendor),3,
                                         ifelse(grepl("^MKM Trend$",vendor),4,
                                                ifelse(grepl("^SCG$",vendor),5,
                                                       ifelse(grepl("^TAT$",vendor),6,
                                                              ifelse(grepl("^TCG Direct Low$",vendor),7,
                                                                     ifelse(grepl("^TCG Low$",vendor),8,
                                                                            ifelse(grepl("^TCG Market$",vendor),9,
                                                                                   ifelse(grepl("^TCG Player$",vendor),10,
                                                                                          ifelse(grepl("^95$",vendor),11,
                                                                                                 ifelse(grepl("^ABU$",vendor),12,
                                                                                                        ifelse(grepl("^MMTG$",vendor),13,
                                                                                                               ifelse(grepl("^AMZ$",vendor),14,
                                                                                                                      ifelse(grepl("^CSI$",vendor),15,
                                                                                                                             ifelse(grepl("^MS$",vendor),16,
                                                                                                                                    ifelse(grepl("^SZ$",vendor),17,0))))))))))))))))) ) %>%
        rename(hasFoil=type) %>%
        mutate(hasFoil = ifelse(hasFoil == "foil",1,ifelse(hasFoil == "etched",2,0)),
               Date = ymd(Sys.Date())) %>%
        select(Date,everything()) %>%
        select(-id) %>%
        rename(tcg_id = uuid,offer = value)
    
    return(retail_master_tbl)
}

# This snippet will pull every mtg set that TCG has, ensuring as soon as pre-sales begin we capture those sales immediately

stacked_text = get_tcgp_sl_roster()

all_data = NULL
all_values = tcg_data_grab()

naming_tbl = all_values[[2]] %>% 
    as_tibble()              %>%  
    select(Product_ID,
           Card_name,
           Set,
           Rarity,
           number,
           isFoil)           %>% 
    rename(version=isFoil)   %>% 
    clean_names() %>%
    distinct()



final_tbl = naming_tbl                          %>%
    left_join(all_values[[1]]                   %>% 
                  clean_names(),
              by=c("product_id"="product_id",
                   "version"="version"))    %>%
    #select(-version_y,-version_x) %>%
    arrange(product_id,
            version,
            condition,
            desc(date)
    )                                   %>%
    filter(!is.na(date)
    )                                    %>%
    #select(-set_rank)                           %>%
    rename(current_available_copies=all_quantity,
           current_vendors = all_seller_ct,
           most_copies_single_seller=largest_seller,
           mkt_low = score_to_beat,
           date_of_sale=date,
           product_name=card_name,
           quantity_sold = sold_quantity)             %>%
    select(product_id,
           product_name,
           set,
           rarity,
           number,
           version,
           date_of_sale,
           condition,
           everything())                      %>%
    select(-contains("d_"),
           -max_single_qty)                   %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    filter(condition == "NM" | condition == "") %>%
    filter(mkt_low >0) 


ban_sl_bl_data = ban_set_api_bl("sld") %>% 
    rbind(.,ban_set_api_bl("slu") )    %>% 
    mutate(tcg_id = as.numeric(as.character(tcg_id)))

buylist_sl_tbl = ban_sl_bl_data %>% left_join(naming_tbl,by=c("tcg_id"="product_id","hasFoil"="version")) %>%rename(card = card_name) %>% select(tcg_id,vendor,card,set,hasFoil,rarity,number,offer)  
versioning_error_save = buylist_sl_tbl %>% filter(is.na(card)) %>% select(-card,-set,-rarity,-number) %>% left_join(naming_tbl,by=c("tcg_id"="product_id"))  %>%rename(card = card_name) %>% select(tcg_id,vendor,card,set,hasFoil,rarity,number,offer)  
buylist_sl_tbl = buylist_sl_tbl%>% filter(!is.na(card)) %>% rbind(.,versioning_error_save)


sl_wider_bl_tbl = buylist_sl_tbl %>% 
    mutate(offer_source = gsub(" Buylist","",vendor)) %>%
    distinct() %>%
    pivot_wider(id_cols = c("tcg_id","card","set","hasFoil","rarity","number"),
                names_from = "offer_source",
                values_from = "offer") %>%
    clean_names() %>% 
    filter(!is.na(card)) %>%
    replace(is.na(.),0)  %>%
    mutate(across(where(is.numeric), round, 2))


ban_sl_mkt_data = ban_set_api_mkt("sld") %>% 
    rbind(.,ban_set_api_mkt("slu") )    %>% 
    mutate(tcg_id = as.numeric(as.character(tcg_id)))

market_sl_tbl = ban_sl_mkt_data %>% left_join(naming_tbl,by=c("tcg_id"="product_id","hasFoil"="version")) %>%rename(card = card_name) %>% select(tcg_id,vendor,card,set,hasFoil,rarity,number,offer)  
versioning_error_save = market_sl_tbl %>% filter(is.na(card)) %>% select(-card,-set,-rarity,-number) %>% left_join(naming_tbl,by=c("tcg_id"="product_id"))  %>%rename(card = card_name) %>% select(tcg_id,vendor,card,set,hasFoil,rarity,number,offer)  
market_sl_tbl = market_sl_tbl%>% filter(!is.na(card)) %>% rbind(.,versioning_error_save) %>% distinct()


sl_wider_mkt_tbl = market_sl_tbl %>% 
    pivot_wider(id_cols = c("tcg_id","card","set","hasFoil","rarity","number"),
                names_from = "vendor",
                values_from = "offer") %>%
    clean_names() %>%
    filter(!is.na(card)) %>%
    replace(is.na(.),0) %>%
    mutate(across(where(is.numeric), round, 2))


#final_tbl %>% view()
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

ss <- drive_get("TCGPlayer SL Pricing Sheet")

sheet_write(ss='1ZzzteWD2mxK1OWKzvNitASlaovNZ0nKepUwOusLJ64Q',
            sheet= "Wolfs_tcg_churn",
            final_tbl)

sheet_write(ss='1ZzzteWD2mxK1OWKzvNitASlaovNZ0nKepUwOusLJ64Q',
            sheet= "BAN_api_mkt",
            sl_wider_mkt_tbl)

sheet_write(ss='1ZzzteWD2mxK1OWKzvNitASlaovNZ0nKepUwOusLJ64Q',
            sheet= "BAN_api_bl",
            sl_wider_bl_tbl)



