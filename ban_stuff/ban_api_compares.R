install.packages("pacman")
pacman::p_load(tidyverse,httr,bigrquery,lubridate,jsonlite,quantmod,janitor)
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
ban_data_retrieval = function(){
    
    BAN_data = GET("https://www.mtgban.com/api/mtgban/all.json?id=mtgjson&sig=QVBJPUFMTF9BQ0NFU1MmQVBJbW9kZT1hbGwmRXhwaXJlcz0xNjUzNDAxMzEzJlNpZ25hdHVyZT1kbjRUSWd2Q3hTWEpCZmtYS0JpRGNhRmNpZXMlM0QmVXNlckVtYWlsPXdvbGYlNDBtdGdiYW4uY29t") %>%
        content("parsed")
    v = 1
    
    for(v in 1:2){
        if(v == 1){
            
            semi_unnested = map_df(BAN_data$buylist, ~ replace(.x, is.null(.x), NA), .id = "uuid") 
            buylist_master_tbl = NULL
            
            buylist_vendor_tbl = semi_unnested %>% select(-uuid) %>% 
                colnames() %>% as_tibble() %>% 
                mutate(id = ifelse(grepl("^ABU$",value),1,
                                   ifelse(grepl("^CK$",value),2,
                                          ifelse(grepl("^CSI$",value),3,
                                                 ifelse(grepl("^MS$",value),4,
                                                        ifelse(grepl("^SCG$",value),5,
                                                               ifelse(grepl("^TAT$",value),6,
                                                                      ifelse(grepl("^TCGMkt$",value),7,
                                                                             ifelse(grepl("^95$",value),8,
                                                                                    ifelse(grepl("^CS$",value),9,
                                                                                           ifelse(grepl("^MMTG$",value),10,
                                                                                                  ifelse(grepl("^SZ$",value),11,
                                                                                                         ifelse(grepl("^HA$",value),12,
                                                                                                                ifelse(grepl("^BP$",value),13,0))))))))))))) ) %>%  
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
            for (i in 2:(nrow(buylist_vendor_tbl)+1) ) {
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
                                                                                                                                            ifelse(grepl("^SZ$",vendor),17,0))))))))))))))))) )
            
            
            
            for (i in 2:18) {
                semi_unnested_filtered = semi_unnested %>% select(uuid,CK,CT,`MKM Low`,`MKM Trend`,SCG,TAT,`TCG Direct Low`,`TCG Low`,`TCG Market`,`TCG Player`,`95`,`ABU`,`MMTG`,`AMZ`,CSI,MS,SZ)
                information = semi_unnested_filtered[,c(1,i)] %>% # creates the 'value' as a `list` column
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
con <- gaeas_cradle("wolfoftinstreet@gmail.com")

ban_data = ban_data_retrieval()

mtgjson_roster = mtgjson_roster_update()


ck_retail = ban_data[[3]] %>% filter(vendor==1) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()
direct_retail = ban_data[[3]] %>% filter(vendor==7) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()
tcg_retail = ban_data[[3]] %>% filter(vendor==10) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()
mkm_retail = ban_data[[3]] %>% filter(vendor==4) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()
ct_retail = ban_data[[3]] %>% filter(vendor==2) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()

ck_direct_mkt_compare_tbl = ck_retail %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(direct_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x,ck_retail=retail.x,tcg_retail=retail.y) %>%
    filter(tcg_retail <= 500, ck_retail >= 25) %>%
    mutate(dollar_opp = tcg_retail - ck_retail) %>%
    arrange(desc(dollar_opp)) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01", hasFoil == 1) %>%
    rename(tcg_id = tcg_id.x) %>% select(-tcg_id.y,-rdate)


tcg_ids_of_interest = ck_direct_mkt_compare_tbl %>% filter(dollar_opp >= 5) %>% select(tcg_id) %>% distinct()
line_items = NULL

for(i in 1:nrow(tcg_ids_of_interest)) {

    recent_sales_raw_list = GET(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep="")) %>% content("parsed")
    loop_limit = recent_sales_raw_list %>% length()

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

line_items = line_items %>% as_tibble() %>% 
    rename(
        tcg_id = V1,
        sku_id = V2,
        version = V3,
        condition = V4,
        language = V5,
        dop = V6,
        price = V7,
        shipping = V8
    )

cleaned_line_items = line_items %>% 
    mutate(dop = as.numeric(dop),
           price = as.numeric(price),
           shipping = as.numeric(shipping)) %>%
    mutate(dop = as.POSIXct(dop/1000, origin="1970-01-01")) %>%
    mutate(price = price + shipping) %>%
    select(-shipping) %>%
    filter(grepl("(Near Mint|Lightly Played)",condition)) %>%
    filter(language  == "English")

nonfoil_line_items = cleaned_line_items %>%
    filter(version == "Normal")

foil_line_items = cleaned_line_items %>%
    filter(version == "Foil")

nonfoil_tcg_churn_tbl = NULL

for(i in 1:length(unique(nonfoil_line_items$tcg_id))){
    tcg_id_churn_line = nonfoil_line_items %>% 
        filter(tcg_id == nonfoil_line_items$tcg_id[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = mean(day_diff,na.rm=T),
            hour_diff = mean(hour_diff,na.rm=T)
        )
    nonfoil_tcg_churn_tbl = rbind(nonfoil_tcg_churn_tbl,tcg_id_churn_line)
}

nonfoil_tcg_churn_tbl = nonfoil_tcg_churn_tbl %>% distinct()

foil_tcg_churn_tbl = NULL

for(i in 1:length(unique(foil_line_items$tcg_id))){
    tcg_id_churn_line = foil_line_items %>% 
        filter(tcg_id == foil_line_items$tcg_id[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days"),
               hour_diff =  difftime(dop, lead(dop,1), units = "hours")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = mean(day_diff,na.rm=T),
            hour_diff = mean(hour_diff,na.rm=T)
        )
    foil_tcg_churn_tbl = rbind(foil_tcg_churn_tbl,tcg_id_churn_line)
}

foil_tcg_churn_tbl = foil_tcg_churn_tbl %>% distinct()


desired_day_diff = mean(foil_tcg_churn_tbl$day_diff,na.rm=T) 

churn_tbl = rbind(nonfoil_tcg_churn_tbl,foil_tcg_churn_tbl) %>%
    mutate(version = ifelse(version == "Foil",1,0)) %>% ungroup()

ck_direct_mkt_churn_tbl = ck_direct_mkt_compare_tbl %>%
    left_join(churn_tbl,by=c("tcg_id"="tcg_id" , "hasFoil"="version")) %>%
    drop_na() %>%
    mutate(price_chg = (tcg_retail - price)/price )

ck_direct_real_opps = ck_direct_mkt_churn_tbl %>% 
    filter(day_diff <= 90) %>%
    filter(price_chg <= 5) %>%
    mutate_if(is.difftime,as.numeric) %>%
    mutate(day_diff = round(day_diff,0),
           hour_diff = round(hour_diff,0))

ck_direct_real_opps %>% view()

ck_tcg_mkt_compare_tbl = ck_retail %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(tcg_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x,ck_retail=retail.x,tcg_retail=retail.y) %>%
    filter(tcg_retail <= 500, ck_retail >= 25) %>%
    mutate(dollar_opp = tcg_retail - ck_retail) %>%
    arrange(desc(dollar_opp)) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01", hasFoil == 1) %>%
    rename(tcg_id = tcg_id.x) %>% select(-tcg_id.y,-rdate)

tcg_ids_of_interest = ck_tcg_mkt_compare_tbl %>% filter(dollar_opp >= 5) %>% select(tcg_id) %>% distinct()
line_items = NULL

for(i in 1:nrow(tcg_ids_of_interest)) {
    
    recent_sales_raw_list = GET(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep="")) %>% content("parsed")
    loop_limit = recent_sales_raw_list %>% length()
    
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

line_items = line_items %>% as_tibble() %>% 
    rename(
        tcg_id = V1,
        sku_id = V2,
        version = V3,
        condition = V4,
        language = V5,
        dop = V6,
        price = V7,
        shipping = V8
    )

cleaned_line_items = line_items %>% 
    mutate(dop = as.numeric(dop),
           price = as.numeric(price),
           shipping = as.numeric(shipping)) %>%
    mutate(dop = as.POSIXct(dop/1000, origin="1970-01-01")) %>%
    mutate(price = price + shipping) %>%
    select(-shipping) %>%
    filter(grepl("(Near Mint|Lightly Played)",condition)) %>%
    filter(language  == "English")

nonfoil_line_items = cleaned_line_items %>%
    filter(version == "Normal")

foil_line_items = cleaned_line_items %>%
    filter(version == "Foil")

nonfoil_tcg_churn_tbl = NULL

for(i in 1:length(unique(nonfoil_line_items$tcg_id))){
    tcg_id_churn_line = nonfoil_line_items %>% 
        filter(tcg_id == nonfoil_line_items$tcg_id[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = mean(day_diff,na.rm=T),
            hour_diff = mean(hour_diff,na.rm=T)
        )
    nonfoil_tcg_churn_tbl = rbind(nonfoil_tcg_churn_tbl,tcg_id_churn_line)
}

nonfoil_tcg_churn_tbl = nonfoil_tcg_churn_tbl %>% distinct()

foil_tcg_churn_tbl = NULL

for(i in 1:length(unique(foil_line_items$tcg_id))){
    tcg_id_churn_line = foil_line_items %>% 
        filter(tcg_id == foil_line_items$tcg_id[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days"),
               hour_diff =  difftime(dop, lead(dop,1), units = "hours")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = mean(day_diff,na.rm=T),
            hour_diff = mean(hour_diff,na.rm=T)
        )
    foil_tcg_churn_tbl = rbind(foil_tcg_churn_tbl,tcg_id_churn_line)
}

foil_tcg_churn_tbl = foil_tcg_churn_tbl %>% distinct()


desired_day_diff = mean(foil_tcg_churn_tbl$day_diff,na.rm=T) 

churn_tbl = rbind(nonfoil_tcg_churn_tbl,foil_tcg_churn_tbl) %>%
    mutate(version = ifelse(version == "Foil",1,0)) %>% ungroup()

ck_tcg_mkt_churn_tbl = ck_direct_mkt_compare_tbl %>%
    left_join(churn_tbl,by=c("tcg_id"="tcg_id" , "hasFoil"="version")) %>%
    drop_na() %>%
    mutate(price_chg = (tcg_retail - price)/price )

ck_direct_real_opps = ck_tcg_mkt_churn_tbl %>% 
    filter(day_diff <= 90) %>%
    #filter(price_chg <= 5) %>%
    mutate_if(is.difftime,as.numeric) %>%
    mutate(day_diff = round(day_diff,0),
           hour_diff = round(hour_diff,0))

ck_direct_real_opps %>% view()


ct_tcg_mkt_compare_tbl = ct_retail %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(tcg_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x,ct_retail=retail.x,tcg_retail=retail.y) %>%
    filter(tcg_retail <= 500, ct_retail >= 25) %>%
    mutate(dollar_opp = tcg_retail - ct_retail) %>%
    arrange(desc(dollar_opp)) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01", hasFoil == 1)

ct_tcg_mkt_compare_tbl %>% view()

ban_data[[2]]
ck_buylist = ban_data[[4]] %>% filter(vendor==2)    

ck_buylist %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(tcg_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x) %>%
    #filter(retail <= 500, offer >= 25) %>%
    mutate(dollar_opp = offer - retail) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01", hasFoil != 1) %>% arrange(desc(dollar_opp))


ck_buylist %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(mkm_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x) %>%
    #filter(retail <= 500, offer >= 25) %>%
    mutate(dollar_opp = offer - retail) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01",hasFoil == 0) %>% arrange(desc(dollar_opp)) %>% view()

tcg_ct_mkt_compare_tbl = tcg_retail %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(ct_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x,ct_retail=retail.x,tcg_retail=retail.y) %>%
    filter(tcg_retail <= 500, ct_retail >= 25) %>%
    mutate(dollar_opp = ct_retail - tcg_retail) %>%
    arrange(desc(dollar_opp)) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01", hasFoil == 1) %>%
    rename(tcg_id = tcg_id.x) %>% select(-tcg_id.y,-rdate)


tcg_ids_of_interest = tcg_ct_mkt_compare_tbl %>% filter(dollar_opp >= 5) %>% select(tcg_id) %>% distinct()
line_items = NULL

for(i in 1:nrow(tcg_ids_of_interest)) {
    
    recent_sales_raw_list = GET(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[i],"/latestsales",sep="")) %>% content("parsed")
    loop_limit = recent_sales_raw_list %>% length()
    
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

line_items = line_items %>% as_tibble() %>% 
    rename(
        tcg_id = V1,
        sku_id = V2,
        version = V3,
        condition = V4,
        language = V5,
        dop = V6,
        price = V7,
        shipping = V8
    )

cleaned_line_items = line_items %>% 
    mutate(dop = as.numeric(dop),
           price = as.numeric(price),
           shipping = as.numeric(shipping)) %>%
    mutate(dop = as.POSIXct(dop/1000, origin="1970-01-01")) %>%
    mutate(price = price + shipping) %>%
    select(-shipping) %>%
    filter(grepl("(Near Mint|Lightly Played)",condition)) %>%
    filter(language  == "English")

nonfoil_line_items = cleaned_line_items %>%
    filter(version == "Normal")

foil_line_items = cleaned_line_items %>%
    filter(version == "Foil")

nonfoil_tcg_churn_tbl = NULL

for(i in 1:length(unique(nonfoil_line_items$tcg_id))){
    tcg_id_churn_line = nonfoil_line_items %>% 
        filter(tcg_id == nonfoil_line_items$tcg_id[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days") ,
               hour_diff =  difftime(dop, lead(dop,1), units = "hours")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = mean(day_diff,na.rm=T),
            hour_diff = mean(hour_diff,na.rm=T)
        )
    nonfoil_tcg_churn_tbl = rbind(nonfoil_tcg_churn_tbl,tcg_id_churn_line)
}

nonfoil_tcg_churn_tbl = nonfoil_tcg_churn_tbl %>% distinct()

foil_tcg_churn_tbl = NULL

for(i in 1:length(unique(foil_line_items$tcg_id))){
    tcg_id_churn_line = foil_line_items %>% 
        filter(tcg_id == foil_line_items$tcg_id[i]) %>%
        mutate(day_diff =  difftime(dop, lead(dop,1), units = "days"),
               hour_diff =  difftime(dop, lead(dop,1), units = "hours")) %>%
        group_by(tcg_id,version) %>%
        summarize(
            price = mean(price,na.rm=T),
            day_diff = mean(day_diff,na.rm=T),
            hour_diff = mean(hour_diff,na.rm=T)
        )
    foil_tcg_churn_tbl = rbind(foil_tcg_churn_tbl,tcg_id_churn_line)
}

foil_tcg_churn_tbl = foil_tcg_churn_tbl %>% distinct()


desired_day_diff = mean(foil_tcg_churn_tbl$day_diff,na.rm=T) 

churn_tbl = rbind(nonfoil_tcg_churn_tbl,foil_tcg_churn_tbl) %>%
    mutate(version = ifelse(version == "Foil",1,0)) %>% ungroup()

ck_tcg_mkt_churn_tbl = tcg_ct_mkt_compare_tbl %>%
    left_join(churn_tbl,by=c("tcg_id"="tcg_id" , "hasFoil"="version")) %>%
    #drop_na() %>%
    mutate(price_chg = (tcg_retail - price)/price )

ck_direct_real_opps = ck_tcg_mkt_churn_tbl %>% 
    filter(day_diff <= 10) %>%
    #filter(price_chg <= 5) %>%
    mutate_if(is.difftime,as.numeric) %>%
    mutate(day_diff = round(day_diff,0),
           hour_diff = round(hour_diff,0))

ck_direct_real_opps %>% view()


# Unpacking other ids from BAN api ----------------------------------------

id = "ck"
ban_data_retrieval = function(id="mtgjson"){
    
    BAN_data = GET(paste("https://www.mtgban.com/api/mtgban/all.json?id=",id,"&sig=QVBJPUFMTF9BQ0NFU1MmQVBJbW9kZT1hbGwmRXhwaXJlcz0xNjUzNDAxMzEzJlNpZ25hdHVyZT1kbjRUSWd2Q3hTWEpCZmtYS0JpRGNhRmNpZXMlM0QmVXNlckVtYWlsPXdvbGYlNDBtdGdiYW4uY29t",sep="")) %>%
        content("parsed")
    if(id == "mtgjson"){
        
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
                for (i in 2:14) {
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
    }else{
        
        for(v in 1:2){
            if(v == 1){
                buylist = enframe((BAN_data$buylist))
                
                semi_unnested = unnest_wider(buylist,value)
                
                
                buylist_master_tbl = NULL
                
                buylist_vendor_tbl = semi_unnested %>% select(-name) %>% 
                    colnames() %>% as_tibble() %>% 
                    mutate(id = ifelse(grepl("^ABU$",value),1,
                                       ifelse(grepl("^CK$",value),2,
                                              ifelse(grepl("^CSI$",value),3,
                                                     ifelse(grepl("^MS$",value),4,
                                                            ifelse(grepl("^SCG$",value),5,
                                                                   ifelse(grepl("^TAT$",value),6,
                                                                          ifelse(grepl("^TCGMkt$",value),7,
                                                                                 ifelse(grepl("^95$",value),8,
                                                                                        ifelse(grepl("^CS$",value),9,
                                                                                               ifelse(grepl("^MMTG$",value),10,
                                                                                                      ifelse(grepl("^SZ$",value),11,
                                                                                                             ifelse(grepl("^HA$",value),12,
                                                                                                                    ifelse(grepl("^BP$",value),13,0))))))))))))) ) %>% 
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
                                                                                                                             )))))))))))))) %>%
                    arrange(id)
                
                for (i in 2:14) {
                    information = semi_unnested[,c(1,i)] %>% # creates the 'value' as a `list` column
                        mutate(uuid = name,
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
                
                retail = enframe((BAN_data$retail))
                
                semi_unnested = unnest_wider(retail,value)
                
                retail_master_tbl = NULL
                
                retail_vendor_tbl = semi_unnested %>% select(-name) %>% 
                    colnames() %>% as_tibble() %>% 
                    mutate(id = ifelse(grepl("^CK$",value),1,
                                       ifelse(grepl("^CT$",value),2,
                                              ifelse(grepl("^MKM Low$",value),3,
                                                     ifelse(grepl("^MKM Trend$",value),4,
                                                            ifelse(grepl("^SCG$",value),5,
                                                                   ifelse(grepl("^TAT$",value),6,
                                                                          ifelse(grepl("^TCG Direct$",value),7,
                                                                                 ifelse(grepl("^TCG Low$",value),8,
                                                                                        ifelse(grepl("^TCG Market$",value),9,
                                                                                               ifelse(grepl("^TCG Player$",value),10,
                                                                                                      ifelse(grepl("^95$",value),11,
                                                                                                             ifelse(grepl("^ABU$",value),12,
                                                                                                                    ifelse(grepl("^MMTG$",value),13,
                                                                                                                           ifelse(grepl("^AMZ$",value),14,
                                                                                                                                  ifelse(grepl("^CSI$",value),15,
                                                                                                                                         ifelse(grepl("^MS$",value),16,
                                                                                                                                                ifelse(grepl("^SZ$",value),17,0))))))))))))))))) ) %>% 
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
                                                                                                                                                  ))))))))))))))))) %>%
                    arrange(id)
                
                i =2
                for (i in 2:14) {
                    information = semi_unnested[,c(1,i)] %>% # creates the 'value' as a `list` column
                        mutate(uuid = name,
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
}

ban_data = ban_data_retrieval("ck")

ck_buylist = ban_data[[4]] %>% filter(vendor==2) 

ck_buylist %>% filter(offer >= 50)

CK_Smaller_List <- fromJSON("https://api.cardkingdom.com/api/pricelist")                                                       %>% 
    as.data.frame()                                                                                                %>%
    mutate(data.edition = ifelse(data.edition == "Promotional",data.variation,data.edition))                       %>%
    mutate(data.edition = ifelse(grepl("The List",data.edition),gsub("\\/The List","",data.edition),data.edition)) %>%
    mutate(data.edition = ck_conversion$Standardized[match(data.edition,ck_conversion$CK)])                        %>%
    mutate(Semi = paste(data.name,data.edition, sep=""))                                                           %>%
    mutate(data.is_foil = ifelse(data.is_foil == "false", "", "FOIL"))                                             %>%
    mutate(rarity = Updated_Tracking_Keys$Rarity[match(Semi, Updated_Tracking_Keys$Semi)])                         %>%
    mutate(number = Updated_Tracking_Keys$number[match(Semi, Updated_Tracking_Keys$Semi)])                         %>%
    mutate(CK_Key = trimws(paste(data.name, data.edition, rarity," ",data.is_foil, sep="")))                       %>%
    select(data.id, CK_Key,data.name,data.edition,rarity, number,data.is_foil,data.price_buy,data.qty_buying)      %>%
    `colnames<-` (c("ckid","CK_Key","Card Name","Set","Rarity","number","NF/F","BL_Value","Qty_Des"))              %>%
    na.omit() %>% filter(`NF/F` == "") %>% mutate(BL_Value = as.numeric(BL_Value))

CK_Smaller_List %>%  filter(ckid == 110685)
