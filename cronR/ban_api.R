source("config.R")
require("pacman")
pacman::p_load(tidyverse,httr,bigrquery,lubridate,jsonlite,janitor)
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


ck_retail = ban_data[[3]] %>% filter(vendor==1) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()
direct_retail = ban_data[[3]] %>% filter(vendor==7) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()
tcg_retail = ban_data[[3]] %>% filter(vendor==10) %>% left_join(mtgjson_roster %>% clean_names() %>% select(uuid,tcg_id), by = c("uuid"="uuid")) %>% distinct()


ck_direct_mkt_compare_tbl = ck_retail %>% mutate(join_uuid = paste(uuid,hasFoil)) %>% left_join(tcg_retail %>% mutate(join_uuid = paste(uuid,hasFoil)),by=c("join_uuid"="join_uuid")) %>%
    select(-Date.y,-hasFoil.y,-uuid.y,-contains("vendor")) %>% 
    rename(uuid=uuid.x,Date=Date.x,hasFoil = hasFoil.x,ck_retail=retail.x,tcg_retail=retail.y) %>%
    filter(tcg_retail <= 500, ck_retail >= 25) %>%
    mutate(dollar_opp = tcg_retail - ck_retail) %>%
    arrange(desc(dollar_opp)) %>%
    left_join(mtgjson_roster %>% mutate(hasFoil = ifelse(hasFoil=="",0,1),join_uuid = paste(uuid,hasFoil)) %>% select(join_uuid,rdate,card,set,rarity,number),by=c("join_uuid"="join_uuid")) %>%
    select(Date,uuid,card,set,rarity,number,everything()) %>% distinct() %>% select(-join_uuid) %>%
    mutate(rdate = ymd(rdate)) %>% filter(rdate >= "2002-01-01") %>%
    rename(tcg_id = tcg_id.x) %>% select(-tcg_id.y,-rdate) %>%
    mutate(opp_perc = dollar_opp /ck_retail) %>%
    arrange(desc(opp_perc))

ck_direct_mkt_compare_tbl %>% view()

mtgjson_roster = mtgjson_roster_update()



mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("mtgjson_ban",sep=""))
bq_table_upload(x=mybq, values = mtgjson_roster, fields=as_bq_fields(mtgjson_roster),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("ban_retail_id",sep=""))
bq_table_upload(x=mybq, values = ban_data[[1]], fields=as_bq_fields(ban_data[[1]]),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("ban_buylist_id",sep=""))
bq_table_upload(x=mybq, values = ban_data[[2]], fields=as_bq_fields(ban_data[[2]]),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

mybq <- bq_table(project = "gaeas-cradle", dataset = "ban_retail", table = paste(gsub("-","_",Sys.Date()),"_retail",sep=""))
bq_table_upload(x=mybq, values = ban_data[[3]], fields=as_bq_fields(ban_data[[3]]),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

mybq <- bq_table(project = "gaeas-cradle", dataset = "ban_buylist", table = paste(gsub("-","_",Sys.Date()),"_buylist",sep=""))
bq_table_upload(x=mybq, values = ban_data[[4]], fields=as_bq_fields(ban_data[[4]]),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

# # Bullshit ----------------------------------------------------------------
# 
# 
# for(set in sets_of_interest){
#     temp <- fromJSON(paste("https://mtgjson.com/api/v5/",set,".json",sep=""))
# 
#     temp$data$cards %>% as_tibble() %>% mutate(colors = lapply(enframe(colors)$value, function(x) if(identical(x,character(0))) NA_character_ else x )%>% unlist(),
#                                                colorIdentity = map(lapply(enframe(colorIdentity)$value, function(x) if(identical(x,character(0))) NA_character_ else x ),as.character)) %>%
#         mutate(colorIdentity = unlist(colorIdentity))
#     
#     thing = temp$data$cards$colorIdentity %>% lapply(.,function(x) if(identical(x,character(0))) NA_character_ else x)
#     
#     purrr::flatten(thing)
#     
#     thing = lapply(enframe(temp$data$cards$colorIdentity)$value, function(x) if(identical(x,character(0))) NA_character_ else x ) %>% as_tibble() %>%
#         hoist(.,
#               login = c("owner", "login"),
#               name = "name",
#               homepage = "homepage",
#               watchers = "watchers_count"
#         )
#     
#     temp_tbl = map_df(temp$data$cards, ~ replace(.x, is.null(.x), NA), .id = "uuid")
#     
#     }
# 
# coll
#     temp$data$cards$uuid
# 
# 
# 
# 
# 
# 
# 
# vendor_names = semi_unnested %>% 
#     replace(is.null(.),NA) %>% 
#     colnames() %>% 
#     as_tibble() %>%
#     mutate(ct = seq(nrow(.)))
# 
# final_data = vendor_names
# 
# Start = Sys.time()
# all_bl_data = NULL
# for (i in 2:max(vendor_names$ct)){
#     row_assignment = i -1
#     vendor_bl = semi_unnested[,c(1,i)] %>% unnest_longer(vendor_names$value[i]%>% unlist()) %>% distinct()
#     
#     unique_uuids = vendor_bl %>% select(uuid) %>% distinct()
#     
#     regrouped = NULL
#     for(j in 1:nrow(unique_uuids)){
#         data_subbed = vendor_bl %>% filter(uuid == unique_uuids$uuid[j]) %>% mutate(ct = seq(nrow(.)))
#         regrouped = rbind(regrouped,data_subbed)
#     }
#     
#     Assigned_Buylist = regrouped                          %>% 
#         drop_na()                                    %>% 
#         mutate(hasFoil = ifelse(ct == 2, "FOIL","")) %>%
#         select(-ct)
#     
#     val = cbind(final_data[i,2], Assigned_Buylist %>% nest(data = everything())) %>% as_tibble()
#     
#     all_bl_data = rbind(all_bl_data,val)
# }
# End = Sys.time()
# End - Start
# 
# all_bl_data$vendor = vendor_names %>% filter(ct != 1) %>% select(value) %>% rename(vendor=value) %>% unlist()
# 
# bl_data = all_bl_data %>% select(vendor,data)
# 
# 
# semi_unnested[,c(1,i)] %>% select(BP) %>% pivot_wider(BP)
# 
# semi_unnested[,c(1,2)] %>% # creates the 'value' as a `list` column
#     mutate(uuid = uuid,
#            name =  map(enframe(semi_unnested$ABU)$name, as.character),
#         value = map(enframe(semi_unnested$ABU)$value, as.character)) %>% # change to single type
#     unnest(cols = c(ABU, name, value))

