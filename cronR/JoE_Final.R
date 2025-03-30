pacman::p_load(janitor,googlesheets4,googledrive,data.table,skimr)
source('/home/cujo253/mines_of_moria/cronR/journeys_end_functions.R')

prepare_column_names = function(df){
    df %>%
        rename_all(function(x) str_to_title(gsub('_', ' ', x)))
}


start_time = Sys.time()

#best_sellers_tbl = tcgplayer_best_sellers()

best_sellers_tbl = real_tcgplayer_best_sellers() 

best_sellers_tbl = best_sellers_tbl %>%
    filter(!grepl('(Plains|Mountain|Island|Swamp|Forest|Wastes)',card_name)) %>%
    arrange(desc(ovr_qty)) %>%
    mutate(rarity = ifelse(rarity == "Common","C",ifelse(rarity=="Uncommon","U",ifelse(rarity=="Rare","R",ifelse(rarity=="Mythic","M",ifelse(rarity=="Token","T","")))))) %>%
    mutate(rank = seq(nrow(.))) %>%
    head(50000) 

real_pricing_tbl = real_pricing()

ck_real_pricing_tbl = ck_real_pricing()

ck_bl_tbl = ck_bl_evaluation()

edhrec_tbl = edhrec_ranks()

sku_data = rbindlist(fromJSON('https://mtgjson.com/api/v5//TcgplayerSkus.json')$data, fill = T, idcol = NULL) %>%
    as_tibble() %>%
    select(-finish) %>%
    filter(condition == 'NEAR MINT') %>%
    filter(language == 'ENGLISH') %>%
    mutate(printing = ifelse(printing == 'NON FOIL', '', 'FOIL')) %>%
    clean_names() %>%
    select(product_id, printing, sku_id) 

bl_backed_cards = ck_bl_tbl %>%
    left_join(real_pricing_tbl %>% rename(source=vendor),by=c("event_date",'rdate','card','set','abbr','hasFoil','tcg_id', 'mtgjson_id', 'rarity','number','types','manacost'))  %>%
    left_join(best_sellers_tbl %>% mutate(hasFoil = ifelse(version == 0, "", "FOIL"), number = as.character(number)) %>% select(-version),by=c('card'='card_name','set','hasFoil','tcg_id','rarity','number')) %>%
    filter(!is.na(card)) %>%
    mutate(mkt_value = ifelse(!is.na(todays_value), todays_value, mkt_value)) %>%
    select(event_date,rdate,vendor,tcg_id,card,set,mtgjson_id,abbr,rarity,hasFoil,number,types,manacost,mkt_value,ck_bl_value)

chiseled_tbl = real_pricing_tbl %>% filter(!(tcg_id %in% bl_backed_cards$tcg_id & number %in% bl_backed_cards$number) ) %>%
    mutate(ck_bl_value = NA) %>%
    select(event_date,rdate,vendor,tcg_id,card,set,mtgjson_id,abbr,rarity,hasFoil,number,types,manacost,mkt_value,ck_bl_value) %>%
    bind_rows(bl_backed_cards) %>%
    select(-vendor) %>%
    mutate(bl_support = round(mkt_value/ck_bl_value,3)) %>%
    select(-bl_support) %>%
    left_join(best_sellers_tbl %>% mutate(version = ifelse(version == 0, "", "FOIL")) %>% select(tcg_id,version,rank) %>% rename(hasFoil=version,tcg_rank=rank)) %>% arrange((tcg_rank))


final = chiseled_tbl                        %>%
    filter(!is.na(tcg_rank))        %>%
    mutate(tcg_rank = seq(nrow(.)),
           mkt_value = round(mkt_value,2)) %>%
    bind_rows(chiseled_tbl %>%
                  filter(is.na(tcg_rank))) %>%
    left_join(sku_data,by=c("tcg_id"="product_id","hasFoil"="printing")) %>%
    select(-event_date,-mtgjson_id) %>%
    select(tcg_id,sku_id,everything()) %>%
    clean_names() 
    
    
sku_data = NULL
gc()

drive_auth(email = "wolfoftinstreet@gmail.com",use_oob=TRUE)
gs4_auth(email   = "wolfoftinstreet@gmail.com",use_oob=TRUE)

ss <- drive_get("JoE V3")

sheet_write(ss=ss,
            sheet= "New Method",
            final)

#rm(list = ls())
gc()




