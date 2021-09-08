install.packages("pacman")
pacman::p_load(httr,jsonlite,tidyverse,bigrquery,RSelenium,rvest,googlesheets4,googledrive,anytime,lubridate)
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
currentDate <- Sys.Date()
drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
Start_Time <- Sys.time()
A <- 0
B <- 100
C <- 100
TCG__Best_Sellers <- NULL
body <- paste('{
    "algorithm": "",
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
    TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
    
}

TCG__Best_Sellers %>% nrow()

for(j in 1:97){
    Sys.sleep(.5)
    body <- paste('{
        "algorithm": "",
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
            ]
        }
    }
}',
                  sep="")
    A <- A + 100
    B <- 100
    if(A >= 2000) break
    TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
    TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
    
    C = length(TCG_Results_1[[1]]$results)
    if(C == 0) break
    
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
        TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
        
    }
    #if(is.na(Loop_Over)){break}
    Sys.sleep(.25)
}



TCG__Best_Sellers <- unique(TCG__Best_Sellers) %>% as.data.frame() %>% mutate(Rank = seq.int(nrow(.)))

TCG__Best_Sellers = TCG__Best_Sellers %>% mutate( Version = ifelse(is.na(Version),"",Version),
                                                  Date_Time = Sys.time() )



#Japanese Palantir####

TCG_IDs <- TCG__Best_Sellers %>% #filter( Rarity == "Fabled" |Rarity =="Legendary"|Rarity =="Super Rare"|Rarity =="Majestic"|Rarity =="Rare") %>% 
    select(Product_ID)
TCG_IDs <- na.omit(TCG_IDs)

# seller_bricks = NULL
# z = 0
# for(a in 1:nrow(TCG_IDs)){
#     gc()
#     tryCatch({
#         #
#         body <- paste('{"filters":{"term":{"sellerStatus":"Live","channelId":0,"language":["English"]},"range":{"quantity":{"gte":1}},"exclude":{"channelExclusion":0}},"from":0,"size":1000,"sort":{"field":"price+shipping","order":"asc"},"context":{"shippingCountry":"US","cart":{}},"aggregations":["listingType"]}',
#                       sep="")    
#         #
#         Sys.sleep(.2)
#         all_listings = POST(paste("https://mpapi.tcgplayer.com/v2/product/",TCG_IDs$Product_ID[a],"/listings",sep=""), content_type_json(), body = body) %>% content("parsed", encoding = "UTF-8")
#         if(length(all_listings)==0){Sys.sleep(.5)}
#         #
#         c = length(all_listings$results[[1]]$results)
#         #
#         if(c == 0){next}
#         #
#         for(d in 1:c ){
#             listings_bricks = map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()
#             seller_bricks = rbind(seller_bricks,listings_bricks) %>% as_tibble()
#         }
#     },error = function(e){
#         Sys.sleep(.3)
#         #
#         body <- paste('{"filters":{"term":{"sellerStatus":"Live","channelId":0,"language":["English"]},"range":{"quantity":{"gte":1}},"exclude":{"channelExclusion":0}},"from":0,"size":1000,"sort":{"field":"price+shipping","order":"asc"},"context":{"shippingCountry":"US","cart":{}},"aggregations":["listingType"]}',
#                       sep="")      
#         #
#         Sys.sleep(.5)
#         all_listings = POST(paste("https://mpapi.tcgplayer.com/v2/product/",TCG_IDs$Product_ID[a],"/listings",sep=""), content_type_json(), body = body) %>% content("parsed", encoding = "UTF-8")
#         if(length(all_listings)==0){Sys.sleep(3)}
#         #
#         c = length(all_listings$results[[1]]$results)
#         #
#         if(c == 0){next}
#         #
#         for(d in 1:c ){
#             listings_bricks = map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()
#             seller_bricks = rbind(seller_bricks,listings_bricks) %>% as_tibble()}
#     })
#     z = z + 1
#     if(z == 100){
#         seller_bricks = seller_bricks %>% as_tibble() %>% mutate(Date = Sys.Date()) %>% select(-customData)
#         con <- gaeas_cradle("wolfoftinstreet@gmail.com")
#         mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_listings_granular", table = paste(gsub("-","_",Sys.Date()),"_fab_lg",sep=""))
#         bq_table_upload(x=mybq, values = seller_bricks, fields=as_bq_fields(seller_bricks),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
#     }
#     if(z == 100){seller_bricks = NULL}
#     if(z == 100){z = 0}
# }



Start_Time <- Sys.time()

remDr = remoteDriver(remoteServerAddr = "167.172.233.212", port = 4445, browser = "chrome")
remDr$open()
remDr$maxWindowSize()

tcg_copies = function(page_count){
    remDr$navigate(paste("https://shop.tcgplayer.com/productcatalog/product/getpricetable?captureFeaturedSellerData=True&pageSize=50&productId=",TCG_IDs$Product_ID[i],"&gameName=magic&useV2Listings=true&_=1589663962070&page=",page_count,sep = ""))
    Price <- tryCatch(expr = {(as.numeric(gsub('\\$','',unlist(remDr$findElement('xpath','//*[@id="priceTable"]/div[1]/div[3]/span[1]')$getElementText()))))}, error = function(e){Price <- NA})
    Total_Listings <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.sort-toolbar__total-item-count') %>% html_text()
    if(Total_Listings == "No Results"){Total_Listings = 0}else{
        Total_Listings <- data.frame(do.call('rbind', strsplit(as.character(Total_Listings),' ',fixed=TRUE)))
        Total_Listings <- as.numeric(as.character(Total_Listings$X4))
    }
    Copies_ind <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__qty-available') %>% html_text()
    unique_ind <- as.vector(as.numeric(gsub(" of ","",Copies_ind)))
    
    condition <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.condition') %>% html_text() %>% as.vector()
    
    price <- gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__price') %>% html_text() %>% as.vector())
    
    shipping <- as.numeric(gsub("^0*","",str_extract(gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__shipping') %>% html_text() %>% as.vector()),"\\d+\\.\\d+")) %>% replace(is.na(.),0))
    
    page_ledger = data.frame(param = param,copies = unique_ind,condition = condition,price=price,shipping=shipping)
    
    Copies_ind <- sum(as.numeric(gsub(" of ","",Copies_ind)))
    
    pages = rbind(pages,page_ledger)
    
    
    Copies_ind_2 <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__qty-available') %>% html_text()
    Copies_ind_2 <- sum(as.numeric(gsub(" of ","",Copies_ind_2)))
    
    Copies <- Copies + Copies_ind_2
    
    lowest_price <- tryCatch(expr = {(as.numeric(gsub('\\$','',unlist(remDr$findElement('xpath','//*[@id="priceTable"]/div[1]/div[3]/span[1]')$getElementText()))))}, error = function(e){Price <- NA})
    
    card_info <- cbind(param,lowest_price,Total_Listings,Copies)
    
    output =  list(pages,card_info)
    
    #N- Please note, the end user will have to specify results[[2]] given this output to get what was requested
    #N -But I believe the extra context is helpful
    return(output)
}

card_score_card = NULL
card_granular = NULL
i = 1
for(i in 1:nrow(TCG_IDs)){
    remDr$navigate(paste("https://shop.tcgplayer.com/productcatalog/product/getpricetable?captureFeaturedSellerData=True&pageSize=50&productId=",TCG_IDs$Product_ID[i],"&gameName=magic&useV2Listings=true&_=1589663962070",sep = ""))
    Total_Listings <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.sort-toolbar__total-item-count') %>% html_text()
    if(Total_Listings == "No Results"){Total_Listings = 0}else{
        Total_Listings <- data.frame(do.call('rbind', strsplit(as.character(Total_Listings),' ',fixed=TRUE)))
        Total_Listings <- as.numeric(as.character(Total_Listings$X4))
    }
    if(Total_Listings == 0){next}
    page_count = ifelse(Total_Listings <= 50, 1,
                        ifelse((Total_Listings > 50) & (Total_Listings <= 100), 2,
                               ifelse((Total_Listings > 100) & (Total_Listings <= 150), 3,
                                      ifelse((Total_Listings > 150) & (Total_Listings <= 200), 4,
                                             ifelse((Total_Listings > 200) & (Total_Listings <= 250), 5,
                                                    ifelse((Total_Listings > 250) & (Total_Listings <= 300), 6,
                                                           ifelse((Total_Listings > 300) & (Total_Listings <= 350), 7,
                                                                  ifelse((Total_Listings > 350) & (Total_Listings <= 400), 8,
                                                                         ifelse((Total_Listings > 400) & (Total_Listings <= 550), 9,10)))))))))
    
    pages = NULL
    Copies = 0
    for(q in 1:page_count){
        remDr$navigate(paste("https://shop.tcgplayer.com/productcatalog/product/getpricetable?captureFeaturedSellerData=True&pageSize=50&productId=",TCG_IDs$Product_ID[i],"&gameName=magic&useV2Listings=true&_=1589663962070&page=",page_count,sep = ""))
        Price <- tryCatch(expr = {(as.numeric(gsub('\\$','',unlist(remDr$findElement('xpath','//*[@id="priceTable"]/div[1]/div[3]/span[1]')$getElementText()))))}, error = function(e){Price <- NA})
        Total_Listings <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.sort-toolbar__total-item-count') %>% html_text()
        if(Total_Listings == "No Results"){Total_Listings = 0}else{
            Total_Listings <- data.frame(do.call('rbind', strsplit(as.character(Total_Listings),' ',fixed=TRUE)))
            Total_Listings <- as.numeric(as.character(Total_Listings$X4))
        }
        Copies_ind <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__qty-available') %>% html_text()
        unique_ind <- as.vector(as.numeric(gsub(" of ","",Copies_ind)))
        
        condition <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.condition') %>% html_text() %>% as.vector()
        
        price <- gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__price') %>% html_text() %>% as.vector())
        
        shipping <- as.numeric(gsub("^0*","",str_extract(gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__shipping') %>% html_text() %>% as.vector()),"\\d+\\.\\d+")) %>% replace(is.na(.),0))
        
        page_ledger = data.frame(param = TCG_IDs$Product_ID[i],condition = condition,copies = unique_ind,price=price,shipping=shipping)
        
        Copies_ind <- sum(as.numeric(gsub(" of ","",Copies_ind)))
        
        pages = rbind(pages,page_ledger)
        
        
        Copies_ind_2 <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.product-listing__qty-available') %>% html_text()
        Copies_ind_2 <- sum(as.numeric(gsub(" of ","",Copies_ind_2)))
        
        Copies <- Copies + Copies_ind_2
        
        lowest_price <- tryCatch(expr = {(as.numeric(gsub('\\$','',unlist(remDr$findElement('xpath','//*[@id="priceTable"]/div[1]/div[3]/span[1]')$getElementText()))))}, error = function(e){Price <- NA})
        
        card_info <- data.frame(param = TCG_IDs$Product_ID[i],lowest_price = lowest_price,Total_Listings = Total_Listings,Copies = Copies)
        
    }
    #N- Please note, the end user will have to specify results[[2]] given this output to get what was requested
    #N -But I believe the extra context is helpful
    
    # card_findings = tcg_copies(page_count)
    names(card_info)[1] <- "param"
    names(pages)[1] <- "param"
    
    card_score_card = rbind(card_score_card,card_info)
    card_granular = rbind(card_granular,pages )
    
}


End_Time <- Sys.time()
print(paste("FAB Lasted:",round(End_Time - Start_Time,2)))

unlist_df = function(df){data.frame(lapply(df, function(x) unlist(x)))}

roster_info = TCG__Best_Sellers %>% select(Product_ID,Name,Version,Set,Rarity,Number,hasFoil) %>% unlist_df() %>% distinct()

TCG__Best_Sellers = TCG__Best_Sellers %>% unlist_df()

grouped_granular = card_granular %>% mutate(price = as.numeric(gsub("\\,","",price) ),
                                            total_cost = price + shipping) %>%
    group_by(param,condition) %>% summarize(buyout_cost = sum(total_cost),
                                            average_cost = mean(total_cost),
                                            min_cost = min(total_cost),
                                            total_copies = sum(copies))

min_granular_tibble = card_granular %>% mutate(price = as.numeric(gsub("\\,","",price) ),
                                               total_cost = price + shipping) %>%
    group_by(param,condition) %>% summarize(min_cost = min(total_cost),
                                            copies = copies) %>% distinct()

grouped_granular$copies_min_cost = min_granular_tibble$copies[match(paste(grouped_granular$param,grouped_granular$condition),paste(min_granular_tibble$param,min_granular_tibble$condition))]


# Final Outputs -----------------------------------------------------------

ovr_score_card = card_score_card %>% as_tibble() %>% 
    mutate(lowest_price = as.numeric(lowest_price),
           Total_Listings  = as.numeric(Total_Listings ),
           Copies = as.numeric(Copies)) %>%
    left_join(roster_info, by = c("param"="Product_ID")) %>%
    select(param,Name,Set,Version,Rarity,Number,hasFoil,lowest_price,Total_Listings,Copies) %>%
    mutate(Number = as.numeric(Number)) %>%
    left_join(TCG__Best_Sellers %>% mutate(Rank = seq(nrow(TCG__Best_Sellers))) %>% select(Product_ID, Rank), by = c("param"="Product_ID")) %>%
    distinct()

final_granular = grouped_granular %>%
    left_join(roster_info, by = c("param"="Product_ID")) %>%
    select(param,Name,Set,Version,condition,Rarity,Number,everything()) %>%
    select(-hasFoil) %>%
    mutate(Number = as.numeric(Number)) %>%
    left_join(TCG__Best_Sellers %>% mutate(Rank = seq(nrow(TCG__Best_Sellers))) %>% select(Product_ID, Rank), by = c("param"="Product_ID")) %>%
    distinct()


FAB_Roster = TCG__Best_Sellers %>% select(-hasFoil,-MKT_EST,-Listings,-MKT,-Direct_Listings,-Potential_Direct_Copies,-Total_Copies,-Date_Time) %>%
    select(Product_ID, everything())






# Churn Rate --------------------------------------------------------------

# Get a list of tcgIds from whatever source you have
tcg_ids_of_interest = final_granular %>% rename(tcg_id = param) %>% select(tcg_id) %>% distinct()
line_items = NULL

for(i in 1:nrow(tcg_ids_of_interest)) {
    
    #The hilariously exposed api that TCG has left exposed for latest sales
    recent_sales_raw_list = GET(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep="")) %>% content("parsed")
    
    if(recent_sales_raw_list %>% content("parsed")== "Internal Server Error"){next}
    
    if(recent_sales_raw_list$status_code != 200){Sys.sleep(5); recent_sales_raw_list = GET(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep=""))}
    
    recent_sales_raw_list = recent_sales_raw_list %>% content("parsed")
    
    if(recent_sales_raw_list == "Internal Server Error"){next}
    #Each card has a different length of recent sales, for the next sub loop we need this limit
    loop_limit = recent_sales_raw_list %>% length()
    #Need a conditional if there are ZERO recent sales. It will NA (or NaN) for those without.
    #Otherwise, pull all listed recent sales
    if(loop_limit > 0){
        for(q in 1:loop_limit){
            line_item = cbind(
                tcg_ids_of_interest$tcg_id[i],
                recent_sales_raw_list[[q]]$skuId,
                recent_sales_raw_list[[q]]$printing,
                recent_sales_raw_list[[q]]$condition,
                recent_sales_raw_list[[q]]$language,
                recent_sales_raw_list[[q]]$orderDate,
                recent_sales_raw_list[[q]]$purchasePrice,
                recent_sales_raw_list[[q]]$shippingPrice)
            line_items = rbind(line_items,line_item)
        }
    }
}
#as_tibble is R's way of converting to a data frame, and then we'll rename
#the matrix headers from default to stuff that makes sense for the column
line_items = line_items %>% as_tibble() %>% 
    rename(
        tcg_id = V1,
        version = V2,
        condition = V3,
        language = V4,
        dop = V5,
        price = V6,
        shipping = V7
    )
#Let's wrangle this shit!
cleaned_line_items = line_items %>% 
    # dop is returned as 13(14?) character string that needs to be in numeric format for conversion to a date
    # price and shipping (even though they don't reveal shipping, guess what, it's there, and we're going to use it!)
    # also need to be numerics instead of characters
    mutate(dop = anytime(as.character(dop)) ,
           price = as.numeric(price),
           shipping = as.numeric(shipping)) %>%
    #TCG tracks these sales events down to the micro second (woohoo)
    #So we need to convert it with that expectation. If you wish to double check
    #this... use your eyeballs for the particular ID and what it shows on the website
    mutate(dop = anytime(dop) ) %>%
    #let's be real, price + shipping is the actual price
    mutate(price = price + shipping) %>%
    #Now we don't need shipping, but you could keep it if it turns your crank
    select(-shipping) %>%
    #I also dgaf about non - near mint / Lightly played stuff so we'll drop that here
    filter(grepl("(Near Mint|Lightly Played)",condition)) %>%
    # We only want English stuff
    filter(language  == "English")

#Review a summary of our madness
cleaned_line_items %>% mutate_if(is.character,as.factor) %>% summary()

unlimited_nonfoil_line_items = cleaned_line_items %>%
    filter(version == "Unlimited Edition Normal")

first_nonfoil_line_items = cleaned_line_items %>%
    filter(version == "1st Edition Normal")

first_rainbow_line_items = cleaned_line_items %>%
    filter(version == "1st Edition Rainbow Foil")

unlimited_rainbow_line_items = cleaned_line_items %>%
    filter(version == "Unlimited Edition Rainbow Foil")

cold_foil_line_items = cleaned_line_items %>%
    filter(version == "1st Edition Cold Foil")

unlimited_nonfoil_tcg_churn_tbl = NULL

for(i in 1:length(unique(unlimited_nonfoil_line_items$tcg_id))){
    tcg_id_churn_line = unlimited_nonfoil_line_items %>% 
        filter(tcg_id == unique(unlimited_nonfoil_line_items$tcg_id)[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours"),
               minute_diff =  difftime(dop, lead(dop,1), units = "mins")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = round(as.numeric(mean(day_diff,na.rm=T)),0),
            hour_diff = round(as.numeric(mean(hour_diff,na.rm=T)),0),
            minute_diff = round(as.numeric(mean(minute_diff,na.rm=T)),0),
            earliest_dop = as.Date(round_date(min(dop),unit = "day"), "%Y-%m-%d"),
            latest_dop = as.Date(round_date(max(dop), unit = "day"), "%Y-%m-%d"),
            number_of_sales = n()
        )
    unlimited_nonfoil_tcg_churn_tbl = rbind(unlimited_nonfoil_tcg_churn_tbl,tcg_id_churn_line)
}

unlimited_nonfoil_tcg_churn_tbl = unlimited_nonfoil_tcg_churn_tbl %>% distinct()

first_nonfoil_tcg_churn_tbl = NULL

for(i in 1:length(unique(first_nonfoil_line_items$tcg_id))){
    tcg_id_churn_line = first_nonfoil_line_items %>% 
        filter(tcg_id == unique(first_nonfoil_line_items$tcg_id)[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours"),
               minute_diff =  difftime(dop, lead(dop,1), units = "mins")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = round(as.numeric(mean(day_diff,na.rm=T)),0),
            hour_diff = round(as.numeric(mean(hour_diff,na.rm=T)),0),
            minute_diff = round(as.numeric(mean(minute_diff,na.rm=T)),0),
            earliest_dop = as.Date(round_date(min(dop),unit = "day"), "%Y-%m-%d"),
            latest_dop = as.Date(round_date(max(dop), unit = "day"), "%Y-%m-%d"),
            number_of_sales = n()
        )
    first_nonfoil_tcg_churn_tbl = rbind(first_nonfoil_tcg_churn_tbl,tcg_id_churn_line)
}

first_nonfoil_tcg_churn_tbl = first_nonfoil_tcg_churn_tbl %>% distinct()

first_rainbow_tcg_churn_tbl = NULL

for(i in 1:length(unique(first_rainbow_line_items$tcg_id))){
    tcg_id_churn_line = first_rainbow_line_items %>% 
        filter(tcg_id == unique(first_rainbow_line_items$tcg_id)[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours"),
               minute_diff =  difftime(dop, lead(dop,1), units = "mins")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = round(as.numeric(mean(day_diff,na.rm=T)),0),
            hour_diff = round(as.numeric(mean(hour_diff,na.rm=T)),0),
            minute_diff = round(as.numeric(mean(minute_diff,na.rm=T)),0),
            earliest_dop = as.Date(round_date(min(dop),unit = "day"), "%Y-%m-%d"),
            latest_dop = as.Date(round_date(max(dop), unit = "day"), "%Y-%m-%d"),
            number_of_sales = n()
        )
    first_rainbow_tcg_churn_tbl = rbind(first_rainbow_tcg_churn_tbl,tcg_id_churn_line)
}

first_rainbow_tcg_churn_tbl = first_rainbow_tcg_churn_tbl %>% distinct()

unlimited_rainbow_tcg_churn_tbl = NULL

for(i in 1:length(unique(unlimited_rainbow_line_items$tcg_id))){
    tcg_id_churn_line = unlimited_rainbow_line_items %>% 
        filter(tcg_id == unique(unlimited_rainbow_line_items$tcg_id)[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours"),
               minute_diff =  difftime(dop, lead(dop,1), units = "mins")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = round(as.numeric(mean(day_diff,na.rm=T)),0),
            hour_diff = round(as.numeric(mean(hour_diff,na.rm=T)),0),
            minute_diff = round(as.numeric(mean(minute_diff,na.rm=T)),0),
            earliest_dop = as.Date(round_date(min(dop),unit = "day"), "%Y-%m-%d"),
            latest_dop = as.Date(round_date(max(dop), unit = "day"), "%Y-%m-%d"),
            number_of_sales = n()
        )
    unlimited_rainbow_tcg_churn_tbl = rbind(unlimited_rainbow_tcg_churn_tbl,tcg_id_churn_line)
}

unlimited_rainbow_tcg_churn_tbl = unlimited_rainbow_tcg_churn_tbl %>% distinct()

cold_foil_tcg_churn_tbl = NULL

for(i in 1:length(unique(cold_foil_line_items$tcg_id))){
    tcg_id_churn_line = cold_foil_line_items %>% 
        filter(tcg_id == unique(cold_foil_line_items$tcg_id)[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours"),
               minute_diff =  difftime(dop, lead(dop,1), units = "mins")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = round(as.numeric(mean(day_diff,na.rm=T)),0),
            hour_diff = round(as.numeric(mean(hour_diff,na.rm=T)),0),
            minute_diff = round(as.numeric(mean(minute_diff,na.rm=T)),0),
            earliest_dop = as.Date(round_date(min(dop),unit = "day"), "%Y-%m-%d"),
            latest_dop = as.Date(round_date(max(dop), unit = "day"), "%Y-%m-%d"),
            number_of_sales = n()
        )
    cold_foil_tcg_churn_tbl = rbind(cold_foil_tcg_churn_tbl,tcg_id_churn_line)
}

cold_foil_tcg_churn_tbl = cold_foil_tcg_churn_tbl %>% distinct()


churn_tbl = rbind(unlimited_nonfoil_tcg_churn_tbl,first_nonfoil_tcg_churn_tbl,
                  first_rainbow_tcg_churn_tbl, unlimited_rainbow_tcg_churn_tbl,
                  cold_foil_tcg_churn_tbl) %>% ungroup()


final_churn_tbl = final_granular %>%
    filter(grepl("Near Mint",condition)) %>%
    mutate(condition = gsub("Near Mint ","",condition)) %>%
    left_join(churn_tbl,by=c("param"="tcg_id","condition"="version"))

# DB Export ---------------------------------------------------------------



con <- gaeas_cradle("wolfoftinstreet@gmail.com")


mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("fabroster",sep=""))
bq_table_upload(x=mybq, values = FAB_Roster, fields=as_bq_fields(FAB_Roster),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB Roster Upload Successful!")

mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_best_sellers", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
bq_table_upload(x=mybq, values = TCG__Best_Sellers, fields=as_bq_fields(TCG__Best_Sellers),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB best sellers Upload Successful!")

mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_scorecard", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
bq_table_upload(x=mybq, values = ovr_score_card, fields=as_bq_fields(ovr_score_card),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB Scorecard Upload Successful!")

mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_granular", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
bq_table_upload(x=mybq, values = final_granular, fields=as_bq_fields(final_granular),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB Granular Upload Successful!")

mybq <- bq_table(project = "gaeas-cradle", dataset = "fab_tcg_churn", table = paste(gsub("-","_",currentDate),"_tcg_fab",sep=""))
bq_table_upload(x=mybq, values = final_churn_tbl, fields=as_bq_fields(final_churn_tbl),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ FAB Churn Upload Successful!")
