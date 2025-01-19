source("config.R")
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium,lubridate,anytime)
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

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement = paste("
  SELECT DISTINCT Date, Product_ID, isFoil,round(price * .85,2) price, sales_per_day 
  FROM `gaeas-cradle.mtg_churn.",gsub("-","_",Sys.Date()-1),"_mtg_churn` c 
  LEFT JOIN (SELECT  PARSE_DATE('%Y-%m-%d',  rdate) as rdate, a.set FROM `gaeas-cradle.roster.mtgjson` a) b on b.set = c.set 
  ORDER BY sales_per_day desc ",sep="")

best_churners = dbSendQuery(con, 
                            statement = statement) %>% 
  dbFetch(., n = -1) 

tcg_ids_of_interest = best_churners %>% select(Product_ID) %>% distinct() %>% rename(tcg_id = Product_ID)

# house_of_cards = NULL

# a = 1
# reset_config()
# for(a in 1:nrow(tcg_ids_of_interest)){
#       body <- paste('{"filters":{"term":{"sellerStatus":"Live","channelId":0,"direct-seller":true,"listingType":"standard"},"range":{"quantity":{"gte":1},"directInventory":{"gte":1},"direct-inventory":{"gte":1}},"exclude":{"channelExclusion":0,"listingType":"custom"}},"from":0,"size":1,"context":{"shippingCountry":"US","cart":{}},"sort":{"field":"price+shipping","order":"asc"}}',
#                     sep="")
#       #
#       Sys.sleep(.1)
#       all_listings = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[a],"/listings",sep=""), 
#                           content_type_json(), 
#                           body = body
#                           ) %>% content("parsed",encoding = "UTF-8")
#       #
#       skip_check = tryCatch({map_df(all_listings$results, ~ replace(.x, is.null(.x), NA))},error=function(e){"skip"})
#       if(length(skip_check)==0){skip_check = "skip"}
#       if(skip_check=="skip"){next}
#       #
#       listings_df = map_df(all_listings$results, ~ replace(.x, is.null(.x), NA))
#       #
#       condition_brick = map_df(listings_df$aggregations$condition, ~ replace(.x, is.null(.x), NA)) %>% mutate(id = tcg_ids_of_interest$tcg_id[a],table = "condition")
#       #
#       quantity_brick = map_df(listings_df$aggregations$quantity, ~ replace(.x, is.null(.x), NA)) %>% mutate(id = tcg_ids_of_interest$tcg_id[a])
#       quantity_brick = data.frame(value = "copies", count = sum(quantity_brick$value * quantity_brick$count), id = unique(quantity_brick$id),table = "quantity")
#       #
#       language_brick = map_df(listings_df$aggregations$language, ~ replace(.x, is.null(.x), NA)) %>% mutate(id = tcg_ids_of_interest$tcg_id[a],table = "language")
#       #
#       printing_brick = map_df(listings_df$aggregations$printing, ~ replace(.x, is.null(.x), NA)) %>% mutate(id = tcg_ids_of_interest$tcg_id[a],table = "printing")
#       #
#       brick_house = rbind(condition_brick,quantity_brick,language_brick,printing_brick) %>% select(id,everything())
#       #
#       house_of_cards = rbind(house_of_cards,brick_house)
# } 
# 
# house_of_cards = house_of_cards %>% mutate(Date = Sys.Date())
# 
# con <- gaeas_cradle("wolfoftinstreet@gmail.com")
# mybq <- bq_table(project = "gaeas-cradle", dataset = "mtg_listings_summary", table = paste(gsub("-","_",Sys.Date()),"_mtg_listings_summary",sep=""))
# bq_table_upload(x=mybq, values = house_of_cards, fields=as_bq_fields(house_of_cards),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

(round(nrow(tcg_ids_of_interest)/200,0)+1)
a = 1
seller_bricks = NULL
z = 0
for(a in 1:nrow(tcg_ids_of_interest)){
  tryCatch({
    #
    body <- paste('{"filters":{"term":{"sellerStatus":"Live","channelId":0,"listingType":"standard"},"range":{"quantity":{"gte":1},"direct-inventory":{"gte":1}},"exclude":{"channelExclusion":0,"listingType":"custom"}},"from":0,"size":1000,"context":{"shippingCountry":"US","cart":{}},"sort":{"field":"price+shipping","order":"asc"}}',
                  sep="")    
    #
    Sys.sleep(.2)
    all_listings = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[a],"/listings",sep=""), content_type_json(), body = body) %>% content("parsed", encoding = "UTF-8")
    if(length(all_listings)==0){Sys.sleep(300)}
    #
    c = length(all_listings$results[[1]]$results)
    #
    if(c == 0){next}
    #
    for(d in 1:c ){
      listings_bricks = map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()
      seller_bricks = rbind(seller_bricks,listings_bricks) %>% as_tibble()
    }
  },error = function(e){
    Sys.sleep(300)
    #
    body <- paste('{"filters":{"term":{"sellerStatus":"Live","channelId":0,"listingType":"standard"},"range":{"quantity":{"gte":1},"direct-inventory":{"gte":1}},"exclude":{"channelExclusion":0,"listingType":"custom"}},"from":0,"size":1000,"context":{"shippingCountry":"US","cart":{}},"sort":{"field":"price+shipping","order":"asc"}}',
                  sep="")    
    #
    Sys.sleep(.2)
    all_listings = POST(paste("https://mpapi.tcgplayer.com/v2/product/",tcg_ids_of_interest$tcg_id[a],"/listings",sep=""), content_type_json(), body = body) %>% content("parsed", encoding = "UTF-8")
    if(length(all_listings)==0){Sys.sleep(300)}
    #
    c = length(all_listings$results[[1]]$results)
    #
    if(c == 0){next}
    #
    for(d in 1:c ){
      listings_bricks = map_df(all_listings$results[[1]]$results[[d %>% unlist()]], ~ replace(.x, is.null(.x), NA)) %>% as_tibble()
      seller_bricks = rbind(seller_bricks,listings_bricks) %>% as_tibble()}
  })
  z = z + 1
  if(z == 200){
    seller_bricks = seller_bricks %>% as_tibble() %>% mutate(Date = Sys.Date()) %>% select(-customData)
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    mybq <- bq_table(project = "gaeas-cradle", dataset = "mtg_listings_granular", table = paste(gsub("-","_",Sys.Date()),"_mtg_listings_granular",sep=""))
    bq_table_upload(x=mybq, values = seller_bricks, fields=as_bq_fields(seller_bricks),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  }
  if(z == 200){seller_bricks = NULL}
  if(z == 200){z = 0}
}