source("config.R")
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium)
invisible(pokemon_ebay_db <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "pokemon-ebay",
        billing = "pokemon-ebay"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
})

# Ebay DB Pull ------------------------------------------------------------

currentDate <- Sys.Date()
con <- pokemon_ebay_db("wolfoftinstreet@gmail.com")

statement <- paste(
    "SELECT DISTINCT title_name,title_set,listing_psa,listing_total_price,title_language,listing_type,card_views,upload_date,sale_date,listing_completion_days   ",
    "FROM `pokemon-ebay.ebay_pokedex.kanto_grades`  a ",
    'WHERE listing_completion_days >= 3 AND title_language like "english"',
    sep = ""
)
todays_raw_ebay_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

auction_raw_ebay_tbl = todays_raw_ebay_tbl %>% filter(listing_type != "bin")

bin_raw_ebay_tbl = todays_raw_ebay_tbl %>% filter(listing_type == "bin")

meaned_auction_tbl = auction_raw_ebay_tbl %>% mutate(month = format(sale_date,"%B"),
                                year = format(sale_date,"%Y")) %>% 
    group_by(title_name,title_set,listing_psa, month, year) %>%
    summarize(avg_sale_price = round(mean(listing_total_price),2),
              avg_listing_days = round(mean(listing_completion_days),0),
              avg_card_views = round(mean(card_views),0)) %>% ungroup()

meaned_bin_tbl = bin_raw_ebay_tbl %>% mutate(month = format(sale_date,"%B"),
                                                     year = format(sale_date,"%Y")) %>% 
    group_by(title_name,title_set,listing_psa, month, year) %>%
    summarize(avg_sale_price = round(mean(listing_total_price),2),
              avg_listing_days = round(mean(listing_completion_days),0),
              avg_card_views = round(mean(card_views),0)) %>% ungroup()


tally_auction_tbl = auction_raw_ebay_tbl %>% mutate(month = format(sale_date,"%B"),
                                year = format(sale_date,"%Y")) %>% 
    group_by(title_name,title_set,listing_psa, month, year) %>% tally() %>% ungroup() %>%
    rename(cards_sold = n)

tally_bin_tbl = bin_raw_ebay_tbl %>% mutate(month = format(sale_date,"%B"),
                                                    year = format(sale_date,"%Y")) %>% 
    group_by(title_name,title_set,listing_psa, month, year) %>% tally() %>% ungroup() %>%
    rename(cards_sold = n)

ebay_auction_tbl = meaned_auction_tbl %>% mutate(Key = paste(title_name,title_set,month,year,sep="")) %>%
    left_join(tally_auction_tbl %>% mutate(Key = paste(title_name,title_set,month,year,sep="")) %>% select(Key,cards_sold),
              by = c("Key"="Key")) %>% select(-Key) %>% distinct()

ebay_bin_tbl = meaned_bin_tbl %>% mutate(Key = paste(title_name,title_set,month,year,sep="")) %>%
    left_join(tally_bin_tbl %>% mutate(Key = paste(title_name,title_set,month,year,sep="")) %>% select(Key,cards_sold),
              by = c("Key"="Key")) %>% select(-Key) %>% distinct()


# TCG DB Pull -------------------------------------------------------------

con <- pokemon_ebay_db("wolfoftinstreet@gmail.com")


statement <-paste("SELECT Key, Name, a.Set, Rarity, hasFoil, MKT_EST,Listings, MKT, Product_ID, Direct_Listings, Rank 
                    FROM `pokemon-ebay.tcg_best_seller.",gsub("-","_",Sys.Date()),"_tcg_best_sellers`a 
                    ORDER BY Rank ",sep="")

todays_raw_tcg_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT Key, Name, a.Set, Rarity, hasFoil, MKT_EST,Listings, MKT, Product_ID, Direct_Listings, Rank 
                    FROM `pokemon-ebay.tcg_best_seller.",gsub("-","_",Sys.Date()-1),"_tcg_best_sellers`a 
                    ORDER BY Rank ",sep="")

yesterday_raw_tcg_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT Key, Name, a.Set, Rarity, hasFoil, MKT_EST,Listings, MKT, Product_ID, Direct_Listings, Rank 
                    FROM `pokemon-ebay.tcg_best_seller.",gsub("-","_",Sys.Date()-7),"_tcg_best_sellers`a 
                    ORDER BY Rank ",sep="")

week_raw_tcg_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT Key, Name, a.Set, Rarity, hasFoil, MKT_EST,Listings, MKT, Product_ID, Direct_Listings, Rank 
                    FROM `pokemon-ebay.tcg_best_seller.",gsub("-","_",Sys.Date()-30),"_tcg_best_sellers`a 
                    ORDER BY Rank ",sep="")

month_raw_tcg_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)


combined_tcg_mkt_tbl = todays_raw_tcg_tbl %>% select(Product_ID,Name, Set, Rarity, hasFoil, MKT_EST) %>%
    left_join(yesterday_raw_tcg_tbl %>% select(Product_ID,MKT_EST), by = c("Product_ID" = "Product_ID"))  %>%
    left_join(week_raw_tcg_tbl %>% select(Product_ID,MKT_EST), by = c("Product_ID" = "Product_ID"))  %>%
    left_join(month_raw_tcg_tbl %>% select(Product_ID,MKT_EST), by = c("Product_ID" = "Product_ID")) %>%
    rename(Todays_Mkt = MKT_EST.x, Yesterday_Mkt = MKT_EST.y, Week_Ago_Mkt = MKT_EST.x.x,Month_Ago_Mkt = MKT_EST.y.y) %>%
    replace(is.na(.),0) %>%
    mutate( dod = (Todays_Mkt - Yesterday_Mkt)/Todays_Mkt,
            wow = (Todays_Mkt - Week_Ago_Mkt)/Todays_Mkt,
            mom = (Todays_Mkt - Month_Ago_Mkt)/Todays_Mkt) %>%
    mutate(dod = ifelse(dod == 1,0,dod),
           wow = ifelse(wow == 1,0,wow),
           mom = ifelse(mom == 1,0,mom)) %>%
    mutate(hasFoil = ifelse(grepl("Holo",Rarity)==T,"FOIL",""),
           Rarity = gsub("^Holo ","",Rarity)) %>% mutate_if(is.numeric, str_replace_all, pattern = "\\-Inf","0") %>%
    mutate(across(Todays_Mkt:mom, as.numeric))
    
mkt_dod_tbl = combined_tcg_mkt_tbl %>% arrange(desc(dod))
mkt_wow_tbl = combined_tcg_mkt_tbl %>% arrange(desc(wow))
mkt_mom_tbl = combined_tcg_mkt_tbl %>% arrange(desc(mom))


combined_tcg_listings_tbl = todays_raw_tcg_tbl %>% select(Product_ID,Name, Set, Rarity, hasFoil, Listings) %>%
    left_join(yesterday_raw_tcg_tbl %>% select(Product_ID,Listings), by = c("Product_ID" = "Product_ID"))  %>%
    left_join(week_raw_tcg_tbl %>% select(Product_ID,Listings), by = c("Product_ID" = "Product_ID"))  %>%
    left_join(month_raw_tcg_tbl %>% select(Product_ID,Listings), by = c("Product_ID" = "Product_ID")) %>%
    rename(Todays_Mkt = Listings.x, Yesterday_Mkt = Listings.y, Week_Ago_Mkt = Listings.x.x,Month_Ago_Mkt = Listings.y.y) %>%
    replace(is.na(.),0) %>%
    mutate( dod = (Todays_Mkt - Yesterday_Mkt)/Todays_Mkt,
            wow = (Todays_Mkt - Week_Ago_Mkt)/Todays_Mkt,
            mom = (Todays_Mkt - Month_Ago_Mkt)/Todays_Mkt) %>%
    mutate(dod = ifelse(dod == 1,0,dod),
           wow = ifelse(wow == 1,0,wow),
           mom = ifelse(mom == 1,0,mom)) %>%
    mutate(hasFoil = ifelse(grepl("Holo",Rarity)==T,"FOIL",""),
           Rarity = gsub("^Holo ","",Rarity)) %>% replace(is.na(.),0) %>% 
    filter(wow != "-Inf", mom != "-Inf") %>%
    mutate(dod = dod * -1,
           wow = wow * -1,
           mom = mom * -1) %>% mutate_if(is.numeric, str_replace_all, pattern = "\\-Inf","0") %>%
    mutate(across(Todays_Mkt:mom, as.numeric))

listings_dod_tbl = combined_tcg_listings_tbl %>% arrange(desc(dod)) %>% filter(Todays_Mkt > 3)
listings_wow_tbl = combined_tcg_listings_tbl %>% arrange(desc(wow)) %>% filter(Todays_Mkt > 3)
listings_mom_tbl = combined_tcg_listings_tbl %>% arrange(desc(mom)) %>% filter(Todays_Mkt > 3)


combined_tcg_rank_tbl = todays_raw_tcg_tbl %>% select(Product_ID,Name, Set, Rarity, hasFoil, Rank) %>%
    left_join(yesterday_raw_tcg_tbl %>% select(Product_ID,Rank), by = c("Product_ID" = "Product_ID"))  %>%
    left_join(week_raw_tcg_tbl %>% select(Product_ID,Rank), by = c("Product_ID" = "Product_ID"))  %>%
    left_join(month_raw_tcg_tbl %>% select(Product_ID,Rank), by = c("Product_ID" = "Product_ID")) %>%
    rename(Todays_Mkt = Rank.x, Yesterday_Mkt = Rank.y, Week_Ago_Mkt = Rank.x.x,Month_Ago_Mkt = Rank.y.y) %>%
    mutate(Yesterday_Mkt = ifelse(is.na(Yesterday_Mkt)==T, Todays_Mkt, Yesterday_Mkt),
           Week_Ago_Mkt = ifelse(is.na(Week_Ago_Mkt)==T, Yesterday_Mkt, Week_Ago_Mkt),
           Month_Ago_Mkt = ifelse(is.na(Month_Ago_Mkt)==T, Week_Ago_Mkt, Month_Ago_Mkt)) %>%
    mutate( dod = (Todays_Mkt - Yesterday_Mkt)/Todays_Mkt,
            wow = (Todays_Mkt - Week_Ago_Mkt)/Todays_Mkt,
            mom = (Todays_Mkt - Month_Ago_Mkt)/Todays_Mkt) %>%
    mutate(hasFoil = ifelse(grepl("Holo",Rarity)==T,"FOIL",""),
           Rarity = gsub("^Holo ","",Rarity)) %>%  
    filter(wow != "-Inf", mom != "-Inf") %>%
    mutate(dod = dod * -1,
           wow = wow * -1,
           mom = mom * -1) %>% mutate_if(is.numeric, str_replace_all, pattern = "\\-Inf","0") %>%
    mutate(across(Todays_Mkt:mom, as.numeric))

rank_dod_tbl = combined_tcg_rank_tbl %>% arrange(desc(dod))
rank_wow_tbl = combined_tcg_rank_tbl %>% arrange(desc(wow))
rank_mom_tbl = combined_tcg_rank_tbl %>% arrange(desc(mom))


# PSA DB Pull -------------------------------------------------------------

con <- pokemon_ebay_db("wolfoftinstreet@gmail.com")

statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_total_pokemon.",gsub("-","_",Sys.Date()),"_PSA` a 
                    ORDER BY Total desc ",sep="")

todays_raw_total_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_total_pokemon.",gsub("-","_",Sys.Date()-1),"_PSA` a 
                    ORDER BY Total desc ",sep="")

yesterday_raw_total_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_total_pokemon.",gsub("-","_",Sys.Date()-3),"_PSA` a 
                    ORDER BY Total desc ",sep="")

week_raw_total_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_total_pokemon.",gsub("-","_",Sys.Date()-3),"_PSA` a 
                    ORDER BY Total desc ",sep="")


month_raw_total_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

psa_singles_change_function = function(todays_raw_singles_psa_tbl, yesterday_raw_singles_psa_tbl){
    change = todays_raw_singles_psa_tbl %>% 
        mutate( new_key = paste(PSA_Bucket,Card,Set,sep="")) %>%
        left_join(yesterday_raw_singles_psa_tbl %>% mutate(new_key = paste(PSA_Bucket,Card,Set,sep="")) %>% filter(V1 == "Grade"),
                  by = c("new_key"="new_key")) %>%
        select(-new_key) %>% distinct() %>%
        mutate( `1` = round( (PSA_1.x - PSA_1.y) , 0),
                `1.5` = round( ( PSA_1_5.x - PSA_1_5.y), 0),
                `2` = round( (PSA_2.x - PSA_2.y), 0),
                `3` = round( (PSA_3.x - PSA_3.y), 0),
                `4` = round( (PSA_4.x - PSA_4.y), 0),
                `5` = round( (PSA_5.x - PSA_5.y), 0),
                `6` = round( (PSA_6.x - PSA_6.y), 0),
                `7` = round( (PSA_7.x - PSA_7.y), 0),
                `8` = round( (PSA_8.x - PSA_8.y), 0),
                `9` = round( (PSA_9.x - PSA_9.y), 0),
                `10` = round( (PSA_10.x - PSA_10.y), 0),
                Total_Chg = round( (Total.x - Total.y), 0),
                Yesterday = Total.y,
                Today = Total.x
        ) %>% select(PSA_Bucket.x,Card.x,Set.x,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,Total_Chg,Yesterday,Today) %>% replace(is.na(.),0) %>% 
        mutate_if(is.numeric, str_replace_all, pattern = "\\-Inf","0") %>%
        mutate(across(`1`:Today, as.numeric)) %>%
        rename(PSA_Class = PSA_Bucket.x, Card = Card.x, Set = Set.x)
    return(change)
}


psa_change_function = function(todays_raw_total_psa_tbl, week_raw_total_psa_tbl){
    change = todays_raw_total_psa_tbl %>% 
        left_join(week_raw_total_psa_tbl,by = c("Set"="Set")) %>%
        mutate( `1` = round( (PSA_1.x - PSA_1.y) , 0),
                `1.5` = round( ( PSA_1_5.x - PSA_1_5.y), 0),
                `2` = round( (PSA_2.x - PSA_2.y), 0),
                `3` = round( (PSA_3.x - PSA_3.y), 0),
                `4` = round( (PSA_4.x - PSA_4.y), 0),
                `5` = round( (PSA_5.x - PSA_5.y), 0),
                `6` = round( (PSA_6.x - PSA_6.y), 0),
                `7` = round( (PSA_7.x - PSA_7.y), 0),
                `8` = round( (PSA_8.x - PSA_8.y), 0),
                `9` = round( (PSA_9.x - PSA_9.y), 0),
                `10` = round( (PSA_10.x - PSA_10.y), 0),
                Total_Chg = round( (Total.x - Total.y), 0),
                Yesterday = Total.y,
                Today = Total.x
        ) %>% select(Set,`1`,`1.5`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,Total_Chg,Yesterday,Today) %>% replace(is.na(.),0) %>% 
    mutate_if(is.numeric, str_replace_all, pattern = "\\-Inf","0") %>%
        mutate(across(`1`:Today, as.numeric)) %>% mutate(chg = rowSums(.[2:12])) %>% mutate( keep = ifelse( (chg == 0) & (max(Yesterday) == max(Today)), "Keep","No") ) %>% 
         mutate(keep = paste(keep,chg,sep="")) %>% filter(keep != "No0") %>% group_by(Set) %>%
        summarize(`1` = min(abs(`1`)),
                  `1.5` = min(abs(`1.5`)),
                  `2` = min(abs(`2`)),
                  `3` = min(abs(`3`)),
                  `4` = min(abs(`4`)),
                  `5` = min(abs(`5`)),
                  `6` = min(abs(`6`)),
                  `7` = min(abs(`7`)),
                  `8` = min(abs(`8`)),
                  `9` = min(abs(`9`)),
                  `10` = min(abs(`10`)),
                  Total_Chg = min(abs(Total_Chg)),
                  Yesterday = max(Yesterday),
                  Today = max(Today)) %>%ungroup() %>% arrange(desc(Today))
    
    return(change)
}

dod_total_psa = psa_change_function(todays_raw_total_psa_tbl,yesterday_raw_total_psa_tbl)
wow_total_psa = psa_change_function(todays_raw_total_psa_tbl,week_raw_total_psa_tbl)
mom_total_psa = psa_change_function(todays_raw_total_psa_tbl,month_raw_total_psa_tbl)

con <- pokemon_ebay_db("wolfoftinstreet@gmail.com")

statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_singles_pokemon.",gsub("-","_",Sys.Date()),"_PSA` a 
                    ORDER BY Total desc ",sep="")

todays_raw_singles_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_singles_pokemon.",gsub("-","_",Sys.Date()-1),"_PSA` a 
                    ORDER BY Total desc ",sep="")

yesterday_raw_singles_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_singles_pokemon.",gsub("-","_",Sys.Date()-1),"_PSA` a 
                    ORDER BY Total desc ",sep="")

week_raw_singles_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <-statement <-paste("SELECT * 
                    FROM `pokemon-ebay.psa_singles_pokemon.",gsub("-","_",Sys.Date()-1),"_PSA` a 
                    ORDER BY Total desc ",sep="")


month_raw_singles_psa_tbl <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

psa_singles_change_function = function(todays_raw_singles_psa_tbl, yesterday_raw_singles_psa_tbl){
    change = todays_raw_singles_psa_tbl %>% 
        mutate( new_key = paste(PSA_Bucket,Card,Set,sep="")) %>%
        left_join(yesterday_raw_singles_psa_tbl %>% mutate(new_key = paste(PSA_Bucket,Card,Set,sep="")) %>% filter(V1 == "Grade"),
                  by = c("new_key"="new_key")) %>%
        select(-new_key) %>% distinct() %>%
        mutate( `1` = round( (PSA_1.x - PSA_1.y) , 0),
                                                  `1.5` = round( ( PSA_1_5.x - PSA_1_5.y), 0),
                                                  `2` = round( (PSA_2.x - PSA_2.y), 0),
                                                  `3` = round( (PSA_3.x - PSA_3.y), 0),
                                                  `4` = round( (PSA_4.x - PSA_4.y), 0),
                                                  `5` = round( (PSA_5.x - PSA_5.y), 0),
                                                  `6` = round( (PSA_6.x - PSA_6.y), 0),
                                                  `7` = round( (PSA_7.x - PSA_7.y), 0),
                                                  `8` = round( (PSA_8.x - PSA_8.y), 0),
                                                  `9` = round( (PSA_9.x - PSA_9.y), 0),
                                                  `10` = round( (PSA_10.x - PSA_10.y), 0),
                                                  Total_Chg = round( (Total.x - Total.y), 0),
                                                  Yesterday = Total.y,
                                                  Today = Total.x
    ) %>% select(PSA_Bucket.x,Card.x,Set.x,`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,Total_Chg,Yesterday,Today) %>% replace(is.na(.),0) %>% 
        mutate_if(is.numeric, str_replace_all, pattern = "\\-Inf","0") %>%
        mutate(across(`1`:Today, as.numeric)) %>%
        rename(PSA_Class = PSA_Bucket.x, Card = Card.x, Set = Set.x)
    return(change)
}


dod_singles_psa = psa_singles_change_function(todays_raw_singles_psa_tbl,yesterday_raw_singles_psa_tbl)
wow_singles_psa = psa_singles_change_function(todays_raw_singles_psa_tbl,week_raw_singles_psa_tbl)
mom_singles_psa = psa_singles_change_function(todays_raw_singles_psa_tbl,month_raw_singles_psa_tbl)


# Export ------------------------------------------------------------------

poke_news_export = function(Final_Export, sheet_name){
    options(httr_oob_default=TRUE) 
    options(gargle_oauth_email = "pachun95@gmail.com")
    drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
    gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)
    
    ss <- drive_get("Pokemon_Newspaper")
    
    sheet_write(
        Final_Export,
        ss = ss,
        sheet = sheet_name
    )
}

poke_news_export(ebay_auction_tbl,"ebay_auction_tbl")
poke_news_export(ebay_bin_tbl,"ebay_bin_tbl")

poke_news_export(mkt_dod_tbl,"mkt_dod_tbl")

poke_news_export(rank_dod_tbl,"rank_dod_tbl")

poke_news_export(todays_raw_total_psa_tbl,"todays_raw_total_psa_tbl")
poke_news_export(dod_total_psa,"dod_total_psa")
poke_news_export(wow_total_psa,"wow_total_psa")
poke_news_export(mom_total_psa,"mom_total_psa")

keys_of_interest = todays_raw_singles_psa_tbl %>% filter(Total >= 10) %>% select(PSA_Bucket)

poke_news_export(todays_raw_singles_psa_tbl %>% filter(Total >= 10),"todays_raw_singles_psa_tbl")
poke_news_export(dod_singles_psa %>% filter(PSA_Class %in% keys_of_interest$PSA_Bucket),"dod_singles_psa")
poke_news_export(wow_singles_psa %>% filter(PSA_Class %in% keys_of_interest$PSA_Bucket),"wow_singles_psa")
poke_news_export(mom_singles_psa %>% filter(PSA_Class %in% keys_of_interest$PSA_Bucket),"mom_singles_psa")

