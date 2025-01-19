require(pacman)
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,bigrquery,broom,anytime,httr,googleAuthR,skimr)
invisible(clean_names  <- function(.data, unique = FALSE) {
    n <- if (is.data.frame(.data)) colnames(.data) else .data
    
    n <- gsub("%+", "_pct_", n)
    n <- gsub("\\$+", "_dollars_", n)
    n <- gsub("\\++", "_plus_", n)
    n <- gsub("-+", "_minus_", n)
    n <- gsub("\\*+", "_star_", n)
    n <- gsub("#+", "_cnt_", n)
    n <- gsub("&+", "_and_", n)
    n <- gsub("@+", "_at_", n)
    
    n <- gsub("[^a-zA-Z0-9_]+", "_", n)
    n <- gsub("([A-Z][a-z])", "_\\1", n)
    n <- tolower(trimws(n))
    
    n <- gsub("(^_+|_+$)", "", n)
    
    n <- gsub("_+", "_", n)
    
    if (unique) n <- make.unique(n, sep = "_")
    
    if (is.data.frame(.data)) {
        colnames(.data) <- n
        .data
    } else {
        n
    }
})
invisible(right        <- function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
}) #Recreating the right function from Excel 
invisible(left         <- function(text, num_char) {
    substr(text, 1, num_char)
})  #Recreating the left function from Excel 
invisible(moveme       <- function (invec, movecommand) {
    movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                   ",|\\s+"), function(x) x[x != ""])
    movelist <- lapply(movecommand, function(x) {
        Where <- x[which(x %in% c("before", "after", "first", 
                                  "last")):length(x)]
        ToMove <- setdiff(x, Where)
        list(ToMove, Where)
    })
    myVec <- invec
    for (i in seq_along(movelist)) {
        temp <- setdiff(myVec, movelist[[i]][[1]])
        A <- movelist[[i]][[2]][1]
        if (A %in% c("before", "after")) {
            ba <- movelist[[i]][[2]][2]
            if (A == "before") {
                after <- match(ba, temp) - 1
            }
            else if (A == "after") {
                after <- match(ba, temp)
            }
        }
        else if (A == "first") {
            after <- 0
        }
        else if (A == "last") {
            after <- length(myVec)
        }
        myVec <- append(temp, values = movelist[[i]][[1]], after = after)
    }
    myVec
})
invisible(gaeas_cradle <- function(){
    
    service_account_file = '/home/cujo253/mines_of_moria/Essential_Referential_CSVS/gaeas-cradle.json'
    gar_auth_service(service_account_file)
    
    bq_auth(path = service_account_file)
    
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    #bq_auth(email = patches$patches, use_oob = TRUE)
    options(scipen = 20)
    con
    
})


# ----
con <- gaeas_cradle()

statement <- paste(
    "SELECT scryfall_id as scryfall, tcg_id as param, ckid, ckid_f, card as name, a.set, rarity, 
  trim(hasfoil) as foil, number, concat(card,number,' ',trim(hasfoil)) as key, concat(card,a.set) as semi  ",
  "FROM `gaeas-cradle.roster.mtgjson` a ",
  sep = ""
)
roster <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1) %>%mutate(name = gsub("\\s\\/\\/.*","",name))


options(googleAuthR.json_path = 'gaeas-cradle.json')

drive_auth(path='/home/cujo253/mines_of_moria/Essential_Referential_CSVS/gaeas-cradle.json',cache=TRUE,use_oob = TRUE)
# Don't be a moron, save yourself 4 hours, and share the spreadsheet with the service account email address
gs4_auth(path='/home/cujo253/mines_of_moria/Essential_Referential_CSVS/gaeas-cradle.json',cache=TRUE,use_oob = TRUE)
gc()

ss <- drive_get("Sets")

editions <- read_sheet(ss,"Sets")   %>% 
    mutate_if(is.character,as.factor) %>% 
    clean_names()

ck_conversion <- read_sheet(ss,"mtgjson_ck_sets") %>%
    clean_names()


exclusion = editions         %>% 
    select(set_excl,excl_excl) %>% 
    as_tibble()                %>% 
    mutate_if(is.character,
              as.factor)

# You do not need a roster to perform this logic
printings = roster    %>% 
    group_by(name,foil) %>% 
    add_tally()         %>% 
    ungroup()           %>% 
    select(name,foil,n) %>% 
    rename(printings=n)

currentDate <- Sys.Date()
## ----
ck_buylist_raw <- fromJSON("https://api.cardkingdom.com/api/pricelist")

acquisition_time<- as.POSIXct(ck_buylist_raw$meta$created_at, tz = "America/Los_Angeles")

ck_buylist <- ck_buylist_raw$data                                                            %>% 
    as_tibble()                                                                                %>%
    mutate_if(is.integer,as.numeric)                                                           %>%
    clean_names()                                                                              %>%
    mutate_at(vars(starts_with("price")),as.numeric)                                           %>%
    mutate(edition = ifelse(edition == "Promotional",variation,edition))                       %>%
    mutate(edition = ifelse(grepl("The List",edition),gsub("\\/The List","",edition),edition)) %>%
    mutate(blended_edition = ck_conversion$standardized[match(edition,ck_conversion$ck)])      %>%
    mutate(key_name = ifelse(variation != '', paste0(name,' (',variation,')'),name))           %>%
    mutate(semi = paste(key_name,edition, sep=""))                                             %>%
    mutate(rarity_semi = paste(key_name,blended_edition, sep=""))                              %>%
    mutate(is_foil = ifelse(is_foil == "false", "", "FOIL"))                                   %>%
    mutate(name = gsub("(\\s+Extended Art|\\s+Showcase|\\s+Borderless|\\s+Surge)","",name) )                            %>%
    mutate(rarity = roster$rarity[match(rarity_semi, roster$semi)])                            %>%
    separate(sku,into=c('small_edition','number'),sep='-')                                     %>%
    mutate(number = as.factor(tolower(as.character(gsub('^0+','',number)))))                   %>%
    select(-rarity_semi)



# Breakdowns are never referenced again
# Room for further Exploration at this Juncture
ck_buylist_controlled_tbl = ck_buylist %>% filter(!is.na(blended_edition))
ck_buylist_controlled_tbl %>% group_by(blended_edition) %>% summarize(yeyeye=n()) %>% ungroup() %>% arrange(desc(yeyeye))

# Nested insanity
edition_breakdown <-suppressWarnings(summary(as.factor(ck_buylist_controlled_tbl$edition),maxsum = 5000) %>% tidy() %>% mutate(total = sum(x), comp = round(x/total,3) ) %>% arrange(desc(comp)))
nf_edition_breakdown = suppressWarnings(summary(as.factor((ck_buylist_controlled_tbl %>% filter(is_foil != 'FOIL'))$edition) ) %>% tidy() %>% mutate(total = sum(x), comp = round(x/total,3) ) %>% arrange(desc(comp)))
f_edition_breakdown =suppressWarnings(summary(as.factor((ck_buylist_controlled_tbl %>% filter(is_foil == 'FOIL'))$edition) ) %>% tidy() %>% mutate(total = sum(x), comp = round(x/total,3) ) %>% arrange(desc(comp)))

### ----
slim_ck_buylist = ck_buylist %>% 
    select(name,key_name,edition,blended_edition,number,rarity,
           is_foil,price_retail,qty_retail,price_buy,qty_buying)     %>%
    left_join(exclusion, by=c('blended_edition'='set_excl'),relationship = "many-to-many")         %>%
    mutate(excl_excl = ifelse(is.na(excl_excl),'Unclear',excl_excl)) %>%
    # Room for further Exploration at this Juncture
    filter(excl_excl != 'Exclude')                                   %>%
    select(-excl_excl)                                               %>%
    mutate(qty_diff  = round((qty_buying - qty_retail)/qty_buying,3),
           # Controll -Inf
           qty_diff   = ifelse(is.infinite(qty_diff),min(qty_diff),qty_diff),
           # Adjust New NaN's to lowest possible value
           qty_diff  = ifelse(is.na(qty_diff),min(qty_diff,na.rm=T),qty_diff),
           
           price_diff = round(price_buy/price_retail,3))             %>%
    # Room for further Exploration at this Juncture
    filter(price_retail > .25)                                       %>%
    mutate(qty_buying = abs(qty_buying) )                            %>%
    # Room for further Exploration at this Juncture
    filter(qty_retail > 0)                                           %>%
    arrange(desc(qty_diff))

max_ntile_price = round(max(slim_ck_buylist$price_diff) * 100,0)
max_ntile_qty   = round(max(slim_ck_buylist$qty_diff) * 100,0)

dollar_slim_ck_buylist <- slim_ck_buylist                         %>% 
    mutate(price_tiles = ntile(-price_diff,max_ntile_price),
           qty_tiles   = ntile(-qty_diff,max_ntile_qty),
           tiers       = round( (price_tiles + qty_tiles)/2 ,4),
           tiers       = ifelse(qty_buying >= 100, tiers -1, tiers)) %>%
    arrange(tiers)


#### ----
CK_Prices_df <- NULL
gc()
Limit <- data.frame(raw_elements = read_html("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=1") %>% html_nodes("a") %>% html_attr("href")) %>%  filter(grepl("page=",raw_elements)) %>%as.data.frame() %>% lapply(as.character) %>% as.data.frame() %>% mutate(pages = parse_number(str_sub(raw_elements,-5,-1))) %>% mutate(pages = as.numeric(pages)) %>% as.data.frame() %>% select(pages) %>% max()    
#cl <- makeCluster(2, type = "FORK")

#registerDoParallel(cl)
Sys.sleep(3)
Start_Time <- Sys.time()
CK_Prices_df <- NULL
for(i in 1:Limit){
    tryCatch({
        tryCatch({
            CK_Results <- GET(paste("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep=""))#, body = body)
            Card <- gsub("\\s\\(.*\\)$","",content(CK_Results,"text") %>% read_html %>% html_nodes(".productDetailTitle") %>% html_text())
            Set <- gsub("\n.*","",gsub(" \\([A-Z]\\)$","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())) )
            Rarity <- gsub('\n.*','',gsub("\\)","",gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text()))))
            Number <- as.numeric(gsub('^0*','',gsub('.*\\#\\:\\s*','',gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())))))
            Price <- as.numeric(gsub("(\\$|\\,)","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))[seq(1, length(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))),4)])
            key <- paste(Card, Set, Rarity,sep="")
            Results <- data.frame(key,Card,Set,Rarity,Number,Price,i)
            CK_Prices_df <- rbind(CK_Prices_df,Results)
            Sys.sleep(3)}, error = function(e){
                Sys.sleep(20)
                CK_Results <- GET(paste("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep=""))#, body = body)
                Card <- gsub('\\s*\\(.*','',content(CK_Results,"text") %>% read_html %>% html_nodes(".productDetailTitle") %>% html_text())
                Set <- str_extract(gsub(" \\([A-Z]\\)$","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())),'.*?(?=\\n)')
                Rarity <- str_extract(gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())),"^[A-Z]{1}")
                Number = gsub('^0','',str_extract(gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())),"[a-zA-Z]*[0-9]+$"))
                Price <- as.numeric(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))[seq(1, length(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))),4)])
                key <- paste(Card, Set, Rarity,sep="")
                Results <- data.frame(key,Card,Set,Rarity,Number,Price,i)
                CK_Prices_df <- rbind(CK_Prices_df,Results)
                Sys.sleep(3)
            })}, error = function(e){
                print(str_glue("Error on page:{i}"))}
    )
}

ck_sales_data <- CK_Prices_df

anchor_price <- ck_sales_data %>% 
    head(50)                    %>% 
    clean_names()               %>% 
    select(price)               %>% 
    max()

cleansed_ck_sales_data = ck_sales_data %>% 
    as_tibble()                                                        %>%
    clean_names()                                                      %>%
    select(-key)                                                       %>%
    mutate(ck_rank          = seq(nrow(.)),
           prices           = as.numeric(gsub('\\,','',price)),
           ck_adjusted_rank = round((prices/anchor_price)*ck_rank,5) ) %>%
    select(-price)                                                     %>%
    arrange((ck_adjusted_rank))                                        %>%
    mutate(ck_adjusted_rank = seq(nrow(.)),
           semi_key         = paste0(card,set))


end_time <- Sys.time()
end_time - Start_Time

##### ----
public_output = dollar_slim_ck_buylist                                                 %>%
    mutate(semi_key = paste0(key_name,edition))                                          %>%
    left_join(cleansed_ck_sales_data %>% select(semi_key,ck_adjusted_rank),relationship = "many-to-many")              %>%
    left_join(printings,by=c("name"="name","is_foil"="foil"),relationship = "many-to-many")                            %>%
    distinct() %>%
    left_join(exclusion, by=c("blended_edition"="set_excl"),relationship = "many-to-many")                             %>%
    rename(set_bucket = excl_excl)                                                       %>%
    mutate(set_bucket = ifelse(is.na(set_bucket),'Unclear',set_bucket),
           printing_status = ifelse(set_bucket == 'Standard','In Print','Out of Print')) %>%
    distinct()                                                                           %>%
    select(-semi_key)                                                                    %>%
    rename(sell_rank=ck_adjusted_rank)                                                   %>%
    mutate(sell_rank         = ifelse(is.na(sell_rank),max(sell_rank,na.rm=T)+1,sell_rank),
           velocity          = round( exp((log(price_tiles)+log(qty_tiles)+log(sell_rank))/3),3),
           velocity_adjusted = velocity * printings,
           time              = acquisition_time)                                         %>%
    arrange(velocity) %>%
    mutate(rank = seq(nrow(.)))                                                          %>%
    select(time,rank, everything())


high_margin_fast_sellers = public_output          %>%
    filter(price_diff > .5)                         %>%
    mutate(margin = price_retail - price_buy,
           margin_perc = round(margin/price_buy,3)) %>%
    filter(price_buy >= 3)                          %>%
    filter(margin_perc >= .70)                      %>%
    filter(sell_rank <= 1000)                        %>%
    arrange(velocity_adjusted)


wolfs_output = public_output                                 %>%
    filter(price_diff > .5)                                    %>%
    mutate(margin      = price_retail - price_buy,
           margin_perc = round(margin/price_buy,3))            %>%
    filter(printing_status == "Out of Print")                  %>%
    filter(price_buy >= 3)                                     %>%
    filter(printings <=5)                                      %>%
    filter(margin_perc >= .80)                                 %>%
    mutate(semi = paste0(name,blended_edition,number,is_foil)) %>%
    left_join(roster                                        %>%
                  mutate(semi = paste0(name,set,number,foil)) %>%
                  select(semi,param),relationship = "many-to-many")%>% 
    select(-semi)                                              %>%
    select(time, param, everything())                          %>%
    mutate(rank = seq(nrow(.)))

###### ----

wolfs_sheet_output = wolfs_output %>% 
    select(time, rank, key_name, edition, number, is_foil, price_retail, qty_retail, price_buy, qty_buying, sell_rank, printings, velocity_adjusted, margin) %>%
    filter(!is.na(time)) %>%
    mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA)))


wolfs_db_output = wolfs_output %>%
    mutate(ck_key = trimws(paste(name, blended_edition, rarity," ",is_foil, sep="")))%>%
    distinct()


ss <- drive_get("Wolfs_Buylist_Review")
sheet_write(
    wolfs_sheet_output,
    ss    = ss,
    sheet = "Current_BuyList"
)

ss <- drive_get("ck_margin_winners")
sheet_write(
    high_margin_fast_sellers,
    ss    = ss,
    sheet = "margin_winners"
)

bq_table_data = public_output %>% 
    mutate(semi = paste0(name,blended_edition,number,is_foil)) %>%
    mutate(ck_key = trimws(paste(name, blended_edition, rarity," ",is_foil, sep="")),
           Unique_Keys=ck_key,
           Date= Sys.Date())%>%
    arrange(desc(price_diff)) %>%            # Arrange the data in descending order of price_diff
    mutate(ratio_rank = dense_rank(desc(price_diff))) %>%
    arrange(desc(qty_diff)) %>%            # Arrange the data in descending order of price_diff
    mutate(qty_rank = dense_rank(desc(qty_diff))) %>%
    mutate(margin      = price_retail - price_buy,
           margin_perc = round(margin/price_buy,3))            %>%
    rename(meta_created_at = time,
           data_name=name,
           data_edition=edition,
           data_is_foil=is_foil,
           data_price_retail=price_retail,
           data_qty_retail=qty_retail,
           data_price_buy=price_buy,
           data_qty_buying=qty_buying) %>%
    left_join(roster                                        %>%
                  mutate(semi = paste0(name,set,number,foil)) %>%
                  select(semi,param),relationship = "many-to-many")%>%
    select(rank,meta_created_at,data_name,data_edition,data_is_foil,data_price_retail,
           data_qty_retail, data_price_buy,data_qty_buying,qty_diff,price_diff,
           tiers,ratio_rank,qty_rank,sell_rank,velocity,printings,set_bucket,printing_status,
           velocity_adjusted,margin,margin_perc,semi,rarity,number,ck_key,param,Unique_Keys,Date) %>%
    arrange(ratio_rank)



con <- gaeas_cradle()
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_velocity", table = paste(gsub("-","_",Sys.Date()),"_CK_VELOCITY",sep=""))
bq_table_upload(x=mybq, values = bq_table_data, fields=as_bq_fields(bq_table_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

# bq_table_data %>% 
#     select(rank,data_name,data_edition,data_is_foil,data_price_retail,data_price_buy,price_diff,ratio_rank) %>%
#     view()
    
    
    