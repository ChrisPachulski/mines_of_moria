source("config.R")
pacman::p_load(ggpmisc,ggrepel,ggpubr,devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest,gmailr,googledrive,janitor,futile.logger,reshape2,arules)

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
retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=2, sleep=0) {
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


# TCG ID Retrievals -------------------------------------------------------

# This snippet will pull every mtg set that TCG has, ensuring as soon as pre-sales begin we capture those sales immediately
remDr <- chrome("64.225.20.203")
remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=pokemon")
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
stacked_text <- page_source %>% .[[1]] %>% read_html() %>% html_nodes('.tcg-input-checkbox') %>% html_text()
stacked_qty = page_source %>% .[[1]] %>% read_html() %>% html_nodes(".search-filter__option-count") %>% html_text() %>% trimws() %>% as.numeric() %>% .[-1]

Sys.sleep(4)

stacked_text <- cbind(stacked_text,stacked_qty)
stacked_backup <- stacked_text
stacked_text <- stacked_backup
stacked_text <- stacked_text %>% as_tibble() %>% mutate(stacked_qty = as.numeric(stacked_qty))%>% 
    slice(which.max(stacked_text == "League & Championship Cards") : n()) %>% 
    mutate(case = ifelse(stacked_text == "Cards",1,NA)) %>%
    fill(.,case,.direction = c("down")) %>% 
    filter(is.na(case)) %>%
    select(-case) %>%
    rename("editions"="stacked_text", "qty"="stacked_qty") %>%
    mutate(editions = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions)))))))

stacked_text = stacked_text %>% na.omit() %>% 
    mutate(api_editions  = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions))))))) %>% mutate(qty = ceiling(as.numeric(qty)/50) ) %>%
    mutate(api_editions = paste('"',api_editions,'"',sep=""))
length(stacked_text$editions)
#stacked_text = stacked_text %>% filter(grepl("strixhaven-school",editions))
# Begin Pull --------------------------------------------------------------
#old = all_data
#Empty dictionary for all the goodies
all_data = NULL
basket_data = NULL
tcg_roster = NULL

tcg_ids = NULL

A <- 0
B <- 100
q = 1

tcg_ids_column = NULL
for(q in 1:length(stacked_text$editions)){
    Best_Sellers_SR = NULL
    tcg_roster = NULL
    A <- 0
    B <- 100
    tryCatch({
        Sys.sleep(1.5)
        gc()
        # p = 1
        Sys.sleep(2)
        for(p in 1:stacked_text$qty[q]){
            body <- paste('{
            "algorithm": "sales_exp_fields_experiment",
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
                        "pokemon"
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
            TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body)
            while(TCG_Results$status_code != 200){
                Sys.sleep(10)
                TCG_Results <- POST("https://mp-search-api.tcgplayer.com/v1/search/request?q=&isList=false", content_type_json(), body = body)
                print("Stuck on While Loop")
            }
            TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
            C = length(TCG_Results_1[[1]]$results)
            if(C == 0)break
            
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
                
                formats = TCG_Results_1[[1]]$results[[i]]$customAttributes$formats %>% paste(.,collapse = ",")
                
                description = TCG_Results_1[[1]]$results[[i]]$customAttributes$description
                
                coloring = TCG_Results_1[[1]]$results[[i]]$customAttributes$color %>% paste(.,collapse = ",")
                
                full_type = TCG_Results_1[[1]]$results[[i]]$customAttributes$fullType
                
                typing = TCG_Results_1[[1]]$results[[i]]$customAttributes$cardType %>% paste(.,collapse = ",")
                
                cmc = TCG_Results_1[[1]]$results[[i]]$customAttributes$convertedCost %>% paste(.,collapse = ",") %>% as.numeric()
                
                flavor = TCG_Results_1[[1]]$results[[i]]$customAttributes$flavorText %>% paste(.,collapse = ",")
                
                
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
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 2, 
                                            cmc = cmc, typing = typing, coloring = coloring, description = description,
                                            flavor = flavor, formats = formats,
                                            
                                            MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,
                                            Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)}
                
                if(printing == "Foil Only"){
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 1, 
                                            cmc = cmc, typing = typing, coloring = coloring, description = description,
                                            flavor = flavor, formats = formats,
                                            MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)}
                
                if(printing == "Both"){
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 1, 
                                            cmc = cmc, typing = typing, coloring = coloring, description = description,
                                            flavor = flavor, formats = formats,
                                            MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
                    
                    Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = Number,isFoil = 0, 
                                            cmc = cmc, typing = typing, coloring = coloring, description = description,
                                            flavor = flavor, formats = formats,
                                            MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                            Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
                    Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
                    
                }
                
            }
            Sys.sleep(.25)
        }
        
        
        roster_grab = Best_Sellers_SR %>% select(Product_ID,Card_name,Set,Rarity,number,isFoil,cmc,typing,coloring,description, flavor, formats) %>% rename(tcg_id = Product_ID)
        
        tcg_roster = rbind(tcg_roster,roster_grab) %>% distinct()
        
        setwd(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "temp_poke_rosters/"))
        write_csv(tcg_roster,file = paste0(gsub("-","_",stacked_text$editions[q]),".csv"))
        
        # Sometimes the requests come in too quickly and tcg will forbid access, or
        # they're working on the backend and it results in a 403 call, account here to just 
        # continues loop if this element might fail
        if(is.null(Best_Sellers_SR)){next}
        # Get a list of tcgIds from whatever source you have
        # I adjust to greater than $5 because I act on this information and I'd like to get it in a timely manner, 
        # entirely arbitrary filter
        tcg_ids_of_interest = Best_Sellers_SR %>% as.data.frame() %>% select(Product_ID) %>% rename(tcg_id = Product_ID) %>% distinct()
        
        
        tcg_ids = rbind(tcg_ids,tcg_ids_of_interest)
        # Some sets don't have any cards greater than $5, continue loop if so
        if(nrow(tcg_ids_of_interest)==0){print(paste("No Cards in",stacked_text$editions[q],"Moving On!"))}}, error = function(e){NULL})
    
    tcg_ids_column = rbind(tcg_ids,tcg_ids_column)
    print(q)
}
# Offsets should be outside the loop honestly, but here we are
# Latest sales only come in buckets of 25, you can't extend

offsets = NULL
start = 0
for(i in 1:10000){
    offsets = rbind(offsets,start)
    start = start + 25
}

tcg_ids_of_interest = tcg_ids_column %>% distinct()
tcg_ids_of_interest %>% colnames()
i_number = 0
grouping = 0
five_hundred_group = NULL 

#Sys.sleep(600)
i = 1336
suppressMessages(for(i in 1:nrow(tcg_ids_of_interest)) {
    tryCatch({
        all_basket_date_cleanse_tbl = NULL
        b= 1
        for(b in 1:length(offsets)){
            
            all_sales_for_card = NULL
            
            Sys.sleep(.2)
            
            body = paste0('{listingType: "All", offset: ',offsets[b],', limit: 25}')
            
            
            recent_sales_raw_list = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep=""),content_type_json(),body=body)
            
            test_value = 0
            
            code = as.numeric(recent_sales_raw_list$status_code)
            if(recent_sales_raw_list$status_code == 500){break}
            if(recent_sales_raw_list$status_code == 403){Sys.sleep(1);next}
            
            if(recent_sales_raw_list$status_code == 400){Sys.sleep(.01);break}
            if(length(recent_sales_raw_list %>% content("parsed") %>% .[[5]])==0){break}
            
            if(identical(recent_sales_raw_list %>% content("parsed") %>% .[[5]], list()) ){break}
            
            tryCatch({overall_sales_number = recent_sales_raw_list %>% content("parsed") %>% .[[4]]; recent_sales_raw_list = recent_sales_raw_list %>% content("parsed") %>% .[[5]]}, error = function(e){test_value = 1})
            
            suppressMessages(gc())
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
                
                
                #Let's wrangle this shit
                
                basket_line_items = line_items %>% 
                    as_tibble() %>%
                    mutate(dop = anytime(as.character(dop),tz = "EST" ),
                           sold_quantity = as.numeric(sold_quantity),
                           sell_price = as.numeric(sell_price),
                           shipping = as.numeric(shipping)) %>%
                    #mutate(dop = anytime(dop) ) %>%
                    mutate(sell_price = sell_price + shipping) %>%
                    select(-shipping) %>%
                    mutate(version = ifelse(grepl("(F|f)oil",version),1,0),
                           condition = ifelse(condition == "Near Mint","NM",
                                              ifelse(condition == "Lightly Played", "LP",
                                                     ifelse(condition == "Moderately Played","MP",
                                                            ifelse(condition == "Heavily Played","HP",
                                                                   ifelse(condition == "Damaged","D",""
                                                                   ))))) ,
                           listing_type = ifelse(listing_type=="ListingWithoutPhotos",1,0)
                    ) %>% mutate(dop = floor_date(dop,unit="second")) %>%
                    filter(dop < Sys.Date()) %>%
                    group_by(tcg_id,version,condition,language,dop,listing_type) %>%
                    summarize(sold_quantity = sum(sold_quantity),
                              sell_price = round(mean(sell_price),2)) %>%
                    ungroup() %>% arrange(desc(dop))
                
                min_date = basket_line_items %>% select(dop) %>% summarize(dop = min(dop)) %>% .[[1]]
                
                days_back = Sys.Date() - 23
                if(min_date == days_back){basket_line_items = basket_line_items%>% filter(dop < days_back)}
                
                if(min_date < days_back){basket_line_items = basket_line_items%>% filter(dop > days_back)}
                
                
                
                all_basket_date_cleanse_tbl = rbind(all_basket_date_cleanse_tbl,basket_line_items)
                
                
                if(min_date < days_back){break}
            }
            
        }
        
        
        if( (is.null(all_basket_date_cleanse_tbl)) & (code == 200) ){
            next
        }else if( (code != 200) & (code != 500) & (code != 400)){
            print(paste0("Status Code: ",code," : Failed. Hibernating for 5 minutes."))
            Sys.sleep(10)
        }
        
        
        i_number = i_number + 1
        five_hundred_group = rbind(five_hundred_group,all_basket_date_cleanse_tbl)
        if(i_number == 500) {
            grouping = grouping + 1
            setwd(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "temp_poke_cards"))
            write_csv(five_hundred_group,file = paste0("mtg_grouping_",grouping,".csv"))
            i_number = 0
            five_hundred_group = NULL
        }
        
        if(identical(all_basket_date_cleanse_tbl %>% select(tcg_id) %>% distinct() %>% unlist(),character(0)) ){next}
        if(tcg_ids_of_interest[i,] != all_basket_date_cleanse_tbl %>% select(tcg_id) %>% distinct() %>% unlist() ){
            print(paste0("The desired tcg_id of ",tcg_ids_of_interest[i,],
                         " does not match the id found in the packet return ( ",
                         all_basket_date_cleanse_tbl %>% select(tcg_id) %>% distinct() %>% unlist(),
                         " ). Instituting custom sleep until match has been re-established."));
            Sys.sleep(60)
        }
        
        print(paste(i,nrow(tcg_ids_of_interest),scales::percent(i/nrow(tcg_ids_of_interest))))
    },error = function(e){print(paste("Error on id #:",i,sep=" "))})
})

setwd(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "temp_poke_rosters"))
set_rosters <-
    list.files(pattern = "*.csv") %>% 
    map_df(~read_csv(.))

tcg_ids_of_interest$tcg_id[1336]

set_baskets = NULL
setwd(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "temp_poke_cards"))
set_baskets <-
    list.files(pattern = "*.csv") %>% 
    map_df(~read_csv(.,col_types = cols(.default = "c"))) %>%
    as_tibble() 

set_baskets = set_baskets %>%
    mutate(version = as.numeric(version),
           dop = anytime(dop),
           listing_type = as.numeric(listing_type),
           sold_quantity = as.numeric(sold_quantity),,
           sell_price = as.numeric(sell_price))



distinct_basket_dates = set_baskets  %>% mutate(date = floor_date(dop,"day")) %>% select(date) %>% distinct() %>% arrange(date) %>%
    filter(date != min(date)) %>% 
    filter(date != max(date)) %>% 
    filter(date != max(date)) %>%
    distinct()

distinct_basket_dates %>% arrange(desc(date))

days = 0

for(i in distinct_basket_dates$date){
    baskets_bucket = NULL
    days = days + 1
    basket_number = 0
    
    value = set_baskets %>% 
        mutate(date = floor_date(dop,"day")) %>%
        filter( (date >= i) & (date < i+1) )
    
    for(q in unique(value$dop)){
        basket_number = basket_number + 1
        
        value_1 =  value%>%
            filter( dop %in% q) %>%
            mutate(basket = basket_number)
        
        baskets_bucket = rbind(baskets_bucket,value_1)
    }
    
    
    print(paste(days,length(distinct_basket_dates$date)))
    
    gc()
    expanded_baskets_tbl = baskets_bucket %>%
        mutate(tcg_id = as.numeric(tcg_id),
               date = as.Date(anytime(i)),
               daily_basket_number = basket) %>%
        left_join(set_rosters, by=c("tcg_id"="tcg_id","version"="isFoil")) %>%
        select(date,tcg_id,Card_name,Set,Rarity,number,version,condition,language,
               cmc,typing,coloring,description,flavor,
               listing_type,sold_quantity,dop,sell_price,daily_basket_number,basket) %>%
        distinct()
    
    
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    mybq <- bq_table(project = "gaeas-cradle", dataset = "poke_basket", table = paste(gsub("-","_",expanded_baskets_tbl %>% select(date)%>%  mutate(date = as.Date(anytime(date))) %>%distinct() %>% .[[1]]) ,"_poke_basket",sep=""))
    bq_table_upload(x=mybq, values = expanded_baskets_tbl, fields=as_bq_fields(expanded_baskets_tbl),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
    
}
