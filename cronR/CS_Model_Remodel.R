library(pacman)
pacman::p_load(devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest,gmailr,googledrive,janitor)
'%!in%' <- function(x,y)!('%in%'(x,y))
right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
    substr(text, 1, num_char)
} #Recreating the left function from Excel 
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
invisible(pokemon_ebay <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "pokemon-ebay",
        dataset = "kanto",
        billing = "pokemon-ebay"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
})

options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

# You will only need these comments below if things go tits up.
#
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")
tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
        rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
        #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
        mutate(Semi = paste(name, Set,sep=""))})

Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                        Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                        Semi = paste(name,Set,sep="")) 

# Cardsphere Purchases  --------------------------------------------
refund_keys = read_csv("/home/cujo253/Essential_Referential_CSVS/cs_ledger.csv") %>% 
    filter(Operation  == "Refund") %>% 
    mutate(refund_operation = paste(`Trade #`,`Card Name`)) %>% select(refund_operation)


cs_ledger = read_csv("/home/cujo253/Essential_Referential_CSVS/cs_ledger.csv") %>% filter(Operation == "Purchase") %>%
    mutate(refund_operation = paste(`Trade #`,`Card Name`)) %>%
    filter(.$refund_operation %!in% refund_keys$refund_operation) %>%
    mutate(Qty = 1, Finish = ifelse(Finish == "Nonfoil","","FOIL"), `Net Amount` = (`Net Amount` * -1) )


cs_ledger = cs_ledger %>% select(Date,`Card Name`,`Set Name`,Finish,`Net Amount`,`Peer User`,Qty) %>% 
    mutate(Date = ymd(gsub("\\s+.*","",Date)), Platform = "CS", Date = as.Date(Date, "%b %d, %Y"),
           `Card Name` = gsub(" // .*","",`Card Name`))


colnames(cs_ledger) <- c("Date","Card_Name", "Set","hasFoil","Cost","Sender","Qty","Platform")


CS_Ledger_Cleaned = cs_ledger %>% filter(Cost > 0) %>%
    mutate(Set = gsub("Secret Lair Drop Series","Secret Lair",Set),
           Set = gsub("Tenth","10th",Set),
           Set = gsub("Ninth","9th",Set),
           Set = gsub("Eighth","8th",Set),
           Set = gsub("Seventh","7th",Set),
           Set = gsub("Sixth","6th",Set),
           Set = gsub("Fifth","5th",Set),
           Set = gsub("Fourth","4th",Set),
           Set = gsub("Three","3rd",Set),
           Set = gsub("^Core ","Core Set ",Set),
           Set = gsub(" Anthology:",":",Set),
           Set = gsub("5th Dawn","Fifth Dawn",Set),
           Set = gsub("the Coalition","The Coalition",Set),
           Set = gsub("Commander 2011","Commander",Set),
           Set = gsub("Magic 2010","2010 Core Set",Set),
           Set = gsub("Magic 2011","2011 Core Set",Set),
           Set = gsub("Magic 2012","2012 Core Set",Set),
           Set = gsub("Magic 2013","2013 Core Set",Set),
           Set = gsub("Magic 2014","2014 Core Set",Set),
           Set = gsub("Magic 2015","2015 Core Set",Set),
           Set = gsub("Shadows over Innistrad","Shadows Over Innistrad",Set),
           Set = gsub("Theros: Beyond Death","Theros Beyond Death",Set),
           Set = gsub("Strixhaven Mystical Archive","Strixhaven: Mystical Archive",Set),
           Set = gsub("Duel Decks: Zendikar vs. Eldrazi","Duel Decks: Zendikar Vs. Eldrazi",Set),
           Set = gsub("Modern Masters 2013","Modern Masters",Set),
           Set = gsub("Conspiracy: Take the Crown","Conspiracy - Take the Crown",Set),
           Set = gsub("Prerelease Promos","Promo Pack",Set),
           Set = gsub("Mythic Edition","Masterpiece Series: Mythic Edition",Set),
           Set = gsub("League Promos","Promo Pack",Set),
           Set = gsub("Commander Anthology Volume II","Commander Anthology Vol. II",Set),
           Set = gsub("Kaladesh Inventions","Masterpiece Series: Inventions",Set),
           Set = gsub("Store Championship Promos","Promo Pack",Set),
           Set = gsub("Buy-a-Box Promos","Promo Pack",Set)
    )%>%
    left_join(Sets %>% select(CK_BL_Scrape_Sets,mtgjson) %>% distinct(),by=c("Set"="CK_BL_Scrape_Sets")) %>%
    mutate(Set = mtgjson, Semi = paste(Card_Name,Set,sep="" )) %>% select(-mtgjson) %>% 
    left_join(Updated_Tracking_Keys %>% select(Semi,Rarity) %>% distinct() ,by=c("Semi"="Semi")) %>% 
    mutate(Key = paste(Card_Name,Set,Rarity," ",hasFoil,sep="")) %>% 
    select(Key,Date,Card_Name,Set,Rarity,hasFoil,Cost,Sender,Qty,Platform) %>%
    filter(Date >= "2021-01-01")

# Card Kingdom Purchases --------------------------------------------------

# ip = "64.225.20.203"
ck_purchases <- function(ip = "64.225.20.203"){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    
    remDr$navigate('https://cardkingdom.com/customer_login')
    username <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[1]/input')
    username$clickElement()
    username$sendKeysToElement(list("cjpach@mac.com"))
    Sys.sleep(.5)
    password <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[2]/input')
    password$clickElement()
    password$sendKeysToElement(list("dxICMH3XZU5X"))
    Sys.sleep(.5)
    remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[3]/button')$clickElement()
    Sys.sleep(.5)
    
    remDr$navigate('https://cardkingdom.com/myaccount/order_history')
    
    invoice_page = remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    page_numbers = invoice_page %>% html_nodes(xpath = '//*[@id="appWrapper"]/div[2]/div[3]/div/nav/ul/li') %>% html_text() %>% str_extract("[0-9]+") 
    
    page_numbers = page_numbers[!is.na(page_numbers)]
    
    ck_orders = NULL
    i = 1
    for(i in 1:length(page_numbers)){
    
        remDr$navigate(paste('https://www.cardkingdom.com/myaccount/order_history?page=',i,sep=""))
        
        invoice_page = remDr$getPageSource() %>% .[[1]] %>% read_html()
        
        status <- trimws( invoice_page %>% html_nodes(xpath = '//*[@id="appWrapper"]/div[2]/div[3]/table/tbody/tr/td') %>% html_text())
        invoice_numbers = status %>% as_tibble() %>% .[7:nrow(.),] %>% dplyr::slice(seq(1,n(),by = 6)) %>% unlist()
        addr = status %>% as_tibble() %>% .[7:nrow(.),] %>% dplyr::slice(seq(2,n(),by = 6)) %>% unlist()
        purchase_dates_ck = status %>% as_tibble() %>% .[7:nrow(.),] %>% dplyr::slice(seq(3,n(),by = 6)) %>% unlist()
        pay_meth = status %>% as_tibble() %>% .[7:nrow(.),] %>% dplyr::slice(seq(4,n(),by = 6)) %>% unlist()
        stat = status %>% as_tibble() %>% .[7:nrow(.),] %>% dplyr::slice(seq(5,n(),by = 6)) %>% unlist()
        amt = status %>% as_tibble() %>% .[7:nrow(.),] %>% dplyr::slice(seq(6,n(),by = 6)) %>% unlist()
        
        status = cbind(invoice_numbers,addr,purchase_dates_ck,pay_meth,stat,amt) %>% 
            as_tibble() %>% 
            filter(grepl("CANCELED",stat)==F)
        
        ck_orders = rbind(ck_orders,status)
    
    }
    
    write_csv(ck_orders,"/home/cujo253/model_csvs/ck_invoices.csv")
    
    already_obtained_invoices = read_csv("/home/cujo253/model_csvs/ck_invoices.csv") %>% select(invoice_numbers)
    already_obtained_granular = read_csv("/home/cujo253/model_csvs/ck_buys.csv") %>% select(order_id) %>% distinct()
    
    new_invoices = already_obtained_invoices %>% filter(invoice_numbers %!in% already_obtained_granular$order_id)
    
    if(nrow(new_invoices)==0){
        ck_order = read_csv("/home/cujo253/model_csvs/ck_buys.csv")
    } else {
        ck_orders = ck_orders %>% filter(invoice_numbers %in% new_invoices$invoice_numbers)
        
        ck_order <- NULL
        for(i in 1:nrow(ck_orders)){
            remDr$navigate(paste("https://cardkingdom.com/myaccount/invoice/",ck_orders$invoice_numbers[i],sep=""))
            order_contents <- data.frame(raw = trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('tr') %>% html_text()))
            order_contents <- order_contents$raw[!grepl('Description Style Qty Price Total',order_contents$raw)]
            order_contents <- data.frame(raw = order_contents[!grepl('SINGLES|PRODUCT|Subtotal|Shipping|Sales Tax|Tax',order_contents)])
            final_amt <- as.numeric(gsub("Total USD \\$","",order_contents[nrow(order_contents),]))
            order_contents <- order_contents[-nrow(order_contents),]
            products <- gsub(":.*","",order_contents)
            products <- gsub(" \\(.*","",products)
            sets <- trimws(gsub("\\d+\\s\\$.*","",gsub(" Foil","",gsub("\\s[A-Z]{2}\\s.*","",gsub(".*:","",order_contents)))))
            
            condition = right(gsub("\\(500ct\\)","",gsub("\\s\\d+\\s\\$.*","",gsub("\\s\\d{4}\\s"," ",order_contents))),2)
            
            language <- "English"
            qty <- str_match(gsub("\\s\\$.*","",order_contents),"\\d+$") 
            ind_amt <- as.numeric(gsub(".*\\$","",order_contents)) %>% as.data.frame() %>% replace(is.na(.),0)
            order_comp <- round(ind_amt/final_amt,2)
            add_fees <- final_amt-sum(ind_amt)
            order_comp <- round(order_comp * add_fees,2)
            ind_amt <- round(ind_amt + order_comp,2)
            DOP <- suppressWarnings(mdy(gsub("\\s\\d+\\:.*","",gsub(".*(S|s)hipped on ","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('xpath' = '//*[@id="appWrapper"]/div[2]/div[2]/div/div[4]') %>% html_text())))))
            raw_order <- data.frame(order_id = ck_orders$invoice_numbers[i],name = products,sets = sets, condition = condition, language = language,qty = qty, price = ind_amt, DOP = DOP,Purchased_Via = "CardKindom")
            
            raw_order$hasFoil = as.integer(grepl(" Foil",order_contents))
            raw_order$hasFoil = ifelse(as.numeric(raw_order$hasFoil) == 1, "FOIL","")
            raw_order$sets <- gsub(" Foil","",raw_order$sets)
            raw_order$sets <- gsub(" Draft Booster Box","",gsub(" Bundle","",ifelse(grepl(" Bundle| Draft Booster Box",raw_order$sets)==T,paste(raw_order$name,": ",raw_order$sets,sep=""),raw_order$sets)))
            
            raw_order = raw_order %>% left_join(Sets %>% select(CK_BL_Scrape_Sets,mtgjson), by = c("sets"="CK_BL_Scrape_Sets")) %>% filter(is.na(qty) == F) %>%
                mutate(sets = ifelse(is.na(mtgjson),as.character(sets),as.character(mtgjson))) %>% select(-mtgjson) %>% select(order_id, name,sets,everything())
            
            raw_order$language <- ifelse(grepl("JPN",raw_order$name)==T,"Japanese","English")
            raw_order$sets <- gsub(" JPN.*","",raw_order$sets)
            raw_order$abbrev <- Updated_Tracking_Keys$abbr[match(raw_order$sets,Updated_Tracking_Keys$Set)]
            raw_order$abbrev <- ifelse(is.na(raw_order$abbrev),gsub("\\)","",gsub("\\(","",str_extract(order_contents,"\\([A-Z]+\\)"))),raw_order$abbrev)
            raw_order$rarity <- Updated_Tracking_Keys$Rarity[match(paste(raw_order$name,raw_order$sets,sep=""),paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep=""))]
            raw_order$rarity[is.na(raw_order$rarity)] <- "S"
            raw_order$Key <- trimws(paste(raw_order$name,raw_order$sets,raw_order$rarity," ",raw_order$hasFoil,sep=""))
            order <- raw_order[c(1,13,2,3,11,12,10,4,5,6,7,8,9)] #%>% filter(. != 0.00)
            colnames(order)[11] <- "price"
            order$price = ifelse(grepl("\\$\\d+\\.\\d+\\s\\$",order$name),as.numeric(gsub("^\\$","",gsub("\\s\\$$","",str_extract(order$name,"\\$\\d+\\.\\d+\\s\\$")))),order$price )
            order$condition = ifelse(nchar(trimws(order$condition))<=1,"",order$condition)
            ck_order <- rbind(ck_order,order)
            ck_order <- ck_order[!grepl("^S$",ck_order$Key),]
        }
        
        ck_order <- ck_order %>% filter(is.na(DOP)==F) %>% select(Key,name,sets,abbrev,rarity,hasFoil,condition,language,qty,price,DOP,order_id,Purchased_Via)
        
        already_obtained_granular = read_csv("/home/cujo253/model_csvs/ck_buys.csv")
        
        ck_order = ck_order %>% rbind(already_obtained_granular)
        
    }
    
    
    write_csv(ck_order,"/home/cujo253/model_csvs/ck_buys.csv")
    
    return(ck_order)
}

ck_purchases_raw = ck_purchases("64.225.20.203") %>% mutate(qty = as.numeric(qty)) %>% distinct() %>% mutate(sets = gsub("^Lair of Behemoths","Ikoria: Lair of Behemoths",sets))

ck_purchases_filtered = ck_purchases_raw %>% mutate(name = gsub(" Booster Box","",ifelse(grepl("\\$",name),sets,name)),
                                 sets = ifelse(grepl("\\(",sets),gsub("\\)","",gsub(".*\\(","",sets)),sets),
                                 hasFoil = ifelse(is.na(hasFoil)==T,"",hasFoil),
                                 Key = paste(name,sets,rarity," ",hasFoil,sep="")) #%>% filter(DOP >= "2021-03-01")
ck_purchases_cleaned  = data.frame(
    Key       = ck_purchases_filtered$Key,
    Date      = ck_purchases_filtered$DOP,
    Card_Name = ck_purchases_filtered$name,
    Set       = ck_purchases_filtered$sets,
    Rarity    = ck_purchases_filtered$rarity,
    hasFoil   = ck_purchases_filtered$hasFoil,
    Cost      = round(ck_purchases_filtered$price/ck_purchases_filtered$qty,2),
    Sender    = ck_purchases_filtered$order_id,
    Qty       = ck_purchases_filtered$qty,
    Platform  = "CK")

CS_Ledger_Cleaned = rbind(CS_Ledger_Cleaned,ck_purchases_cleaned)


# Card Trader Purchases ---------------------------------------------------

ct_purchases = function(ip = "64.225.20.203"){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    
    remDr$navigate('https://www.cardtrader.com/users/sign_in?locale=en')
    Sys.sleep(.5)
    
    username = remDr$findElement('xpath','//*[@id="login-modal-static"]/div/form/div/div[2]/div[2]/input')
    username$clickElement()
    username$sendKeysToElement(list("cjpach@mac.com"))
    
    password = remDr$findElement('xpath','//*[@id="login-modal-static"]/div/form/div/div[2]/div[3]/input')
    password$clickElement()
    password$sendKeysToElement(list("Tasigur95$"))
    
    remDr$findElement('xpath','//*[@id="login-modal-static"]/div/form/div/div[3]/button')$clickElement()
    Sys.sleep(3)
    
    remDr$navigate("https://www.cardtrader.com/en/orders/purchased")
    remDr$findElement('xpath','/html/body/div[2]/div[2]/div/div[1]/div/div/div/div[2]/div[7]')$clickElement()
    Sys.sleep(.5)
    page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    orders = page_html %>% html_nodes('tr') %>% html_attr('id') %>% as_tibble() %>% drop_na() %>% 
        mutate(links = paste("https://www.cardtrader.com/",gsub("order","orders",gsub("-","/",value)),sep="")) %>%
        mutate(orders = as.numeric(gsub(".*\\-","",value)))
    
    prior_orders = read_csv("/home/cujo253/model_csvs/ct_purchases_granular.csv") %>% select(Sender,Date) %>% distinct()
    
    dated_orders = orders %>% left_join(prior_orders,by=c("orders"="Sender")) %>% drop_na()
    
    write_csv(dated_orders,"/home/cujo253/model_csvs/ct_purchase_orders.csv")
    
    new_orders = orders %>% left_join(prior_orders,by=c("orders"="Sender")) %>% filter(is.na(Date)==T)
    
    if(nrow(new_orders) > 0){
        
        ct_purchased_cards = NULL
        
        for(z in 1:nrow(new_orders)){
            remDr$navigate(new_orders$links[z])
            Sys.sleep(2)
            page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
            order_date = page_html %>% html_node('xpath' = '/html/body/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/div/div[2]/ul/li[1]') %>% 
                html_text() %>% gsub("Paid at ","",.) %>% gsub("\\s\\d{2}\\:.*","",.) %>% anytime()
            
            card_tbl = page_html %>% html_node('xpath' = '/html/body/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/div/div[5]') %>% html_table() %>%
                select(X3,X5,X8) %>% rename(card = X3, number = X5, cost = X8) %>%
                mutate(card = gsub("\\s\\/\\/.*","",card),number = as.numeric(gsub("#","",number)), cost = as.numeric(gsub("\\$","",cost))) %>% drop_na()
            
            raw_extras = NULL
            for(p in 1:nrow(card_tbl)){
            set_rarity = page_html %>% html_nodes('xpath' = paste('/html/body/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/div/div[5]/div/table/tbody/tr[',p,']/td[4]/div/span[1]/i',sep="")) %>% html_attr("class") %>% as_tibble() 
            
            foil_status = if( length(page_html %>% html_nodes('xpath' = paste('/html/body/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/div/div[5]/div/table/tbody/tr[',p,']/td[4]/div/span[4]/span/span/i',sep="") )) > 0) {
                page_html %>% html_nodes('xpath' = paste('/html/body/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/div/div[5]/div/table/tbody/tr[',p,']/td[4]/div/span[4]/span/span/i',sep="")) %>% html_attr("class")
            } else {""}
            
            package = cbind(set_rarity,foil_status)
            raw_extras = rbind(raw_extras,package)
            
            }
            
            extras = raw_extras %>% mutate(value = gsub("ss-1-5x ss-","",value)) %>%
                separate(value,c("rarity","abbr"),sep = " ss-mtg ss-") %>%
                mutate(abbr = toupper(gsub(" ss-fw","",abbr)),
                       abbr = ifelse(substring(abbr,2,2)=="M",right(abbr,3),abbr),
                       rarity = left(rarity,1),
                       foil_status = gsub("fas fa-fw fa-","",foil_status),
                       foil_status = ifelse(foil_status == "gem","Foil","")) 
            
            shipping = round((page_html %>% 
                                  html_node('xpath' = '/html/body/div[2]/div[2]/div/div[2]/div/div/div/div/div[2]/div/div[5]/div/div/table/tbody/tr[2]/td[2]') %>% 
                                  html_text() %>%
                                  gsub("\\$","",.) %>% as.numeric()) / nrow(card_extra_tbl) , 2)
            
            card_extra_tbl = cbind(card_tbl,extras) %>% 
                left_join(Updated_Tracking_Keys %>% select(Key,abbr,name,Set,Rarity,number,Foil) %>%
                              mutate(Foil = ifelse(Foil == "FOIL","Foil",Foil),
                                     Key = paste(name,Set,Rarity," ",Foil,sep="")) %>%
                                         select(-Rarity),
                          by=c("card"="name","number"="number","abbr"="abbr","foil_status"="Foil")) %>%
                mutate(Date = order_date, Sender = new_orders$orders[z],Qty = 1, Platform = "CT", cost = cost + shipping) %>%
                rename(Card_Name = card, Rarity = rarity, hasFoil = foil_status, Cost = cost) %>%
                select(Key,Date,Card_Name,Set,Rarity,hasFoil,Cost,Sender,Qty,Platform)
            
            ct_purchased_cards = rbind(ct_purchased_cards,card_extra_tbl)
            
        }
        
        write_csv(ct_purchased_cards,"/home/cujo253/model_csvs/ct_purchases_granular.csv")
        
        return(ct_purchased_cards)
    } else{
        
        ct_purchased_cards = read_csv("/home/cujo253/model_csvs/ct_purchases_granular.csv")
        
        return(ct_purchased_cards)
    }
}

card_traders_buys = ct_purchases()

card_traders_buys = card_traders_buys %>% mutate(hasFoil = ifelse(is.na(hasFoil),"",hasFoil)) %>% drop_na() %>% filter(Date >= "2021-03-01")

# card_traders_buys

CS_Ledger_Cleaned = rbind(CS_Ledger_Cleaned,card_traders_buys)

# TCG Purchases -----------------------------------------------------------

tcg_purchases <- function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr$navigate("https://tcgplayer.com")
    Sys.sleep(sample(5:9, 1))
    remDr$findElement("xpath",'//*[@id="app"]/div/header/div/div[3]/div[1]/div[2]/div[1]')$clickElement()
    Sys.sleep(sample(5:9, 1))
    remDr$findElement("css",'.account-actions-menu__title a')$clickElement()
    
    username <- remDr$findElement("xpath",'//*[@id="signInForm"]/div[1]/input')
    Sys.sleep(sample(1:3, 1))
    username$clickElement()
    Sys.sleep(sample(1:3, 1))
    username$sendKeysToElement(list("cjp"))
    Sys.sleep(.5)
    username$sendKeysToElement(list("ach"))
    Sys.sleep(.5)
    username$sendKeysToElement(list("@iclou"))
    Sys.sleep(.5)
    username$sendKeysToElement(list("d.com"))
    Sys.sleep(.5)
    
    Sys.sleep(.5)
    Password <- remDr$findElement("xpath",'//*[@id="signInForm"]/div[2]/section/input')
    Sys.sleep(sample(1:3, 1))
    Password$clickElement()
    Sys.sleep(sample(1:3, 1))
    Password$sendKeysToElement(list("Ta"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("si"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("gu"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("r9"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("5$"))
    Sys.sleep(.5)
    
    remDr$findElement('xpath','//*[@id="signInForm"]/label/span[1]')$clickElement()
    
    Sys.sleep(3)
    remDr$findElement("xpath",'//*[@id="signInForm"]/button')$clickElement()  
    
    Sys.sleep(3)
    remDr$navigate("https://store.tcgplayer.com/myaccount/orderhistory")
    
    
    remDr$findElement("id","DateRange")$clickElement()
    Sys.sleep(1)
    order_count_func = function(n = 4){
        remDr$navigate("https://store.tcgplayer.com/myaccount/orderhistory")
        
        remDr$findElement("id","DateRange")$clickElement()
        Sys.sleep(1)
        remDr$findElement("xpath",paste('//*[@id="DateRange"]/option[',n,']',sep=""))$clickElement()
        #option[0] = 30 days
        #option[1] = 90 days
        #options[2] = 120 days
        #options[3] = All Time
        if(n == 4){
        Sys.sleep(10)
        }
        else if(n == 3){
            Sys.sleep(7)
        } else if (n == 2){
            Sys.sleep(4)
        } else {
            Sys.sleep(2)
        }
        
        if(n == 4){
            order_count <- round(as.numeric(gsub("\\s.*","",gsub(".*of\\s","",gsub("\n.*","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))),-1) - 10
        } else { 
            order_count <- as.numeric(gsub("\\s.*","",gsub(".*of\\s","",gsub("\n.*","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text())))))
        }
        order_count = if(order_count < 0){order_count = 0}else{order_count = order_count}
        return(order_count)
    }
    remDr$findElement("xpath",paste('//*[@id="DateRange"]/option[4]',sep=""))$clickElement()
    #option[0] = 30 days
    #option[1] = 90 days
    #options[2] = 120 days
    #options[3] = All Time
    Sys.sleep(10)
    order_count <- round(as.numeric(gsub("\\s.*","",gsub(".*of\\s","",gsub("\n.*","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))),-1) - 10
    order_count = if(order_count < 0){order_count = 0}else{order_count = order_count}
    
    order_count = order_count_func(4)
    order_count_120 = order_count_func(3)
    order_count_90 = order_count_func(2)
    order_count_30 = order_count_func(1)
    
    historical_order_count = read_csv("/home/cujo253/model_csvs/tcg_purchase_count.csv")$value %>% unlist()
    
    if(order_count > historical_order_count){
    
        write_csv(order_count %>% as_tibble(),"/home/cujo253/model_csvs/tcg_purchase_count.csv")
        
        new_orders = order_count - historical_order_count
        
        if(new_orders <= order_count_30){
            order_count = round(order_count_30,-1)
        } else if (new_orders <= order_count_90){
            order_count = round(order_count_90,-1)
        } else {
            order_count = round(order_count_120,-1)
        }
        
        All_Pages <- NULL
        for(i in 1:round(order_count/10,1) ){
            page_html = remDr$getPageSource() %>% .[[1]] %>% read_html() 
            Order_Content_Raw <- data.frame(content = trimws(gsub("Sold by.*","",gsub("ITEMS","",trimws(page_html %>% html_nodes(".orderHistoryItems") %>% html_text())))))
            Order_Content_Raw <- data.frame(do.call('rbind', strsplit(as.character(Order_Content_Raw$content),'\n\n',fixed=TRUE)))
            colnames(Order_Content_Raw) <- c("name","set")
            Order_Content_Raw$abbrev <- Updated_Tracking_Keys$abbr[match(Order_Content_Raw$set,Updated_Tracking_Keys$Set)]
            
            Sys.sleep(.2)
            
            Order_Details_Raw <- data.frame(very_raw = trimws(gsub(" - ",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(".orderHistoryDetail") %>% html_text()))))
            Order_Details_Raw <- data.frame(do.call('rbind', strsplit(as.character(Order_Details_Raw$very_raw),'\n',fixed=TRUE)))
            Order_Details_Raw <- suppressWarnings(Order_Details_Raw %>% separate(X1,c("R","rarity"),sep=": ") %>% separate(X2,c("Con","condition","language"),sep=":"))
            Order_Details_Raw$language[is.na(Order_Details_Raw$language)] <- "English"
            Order_Details_Raw$hasFoil <- ifelse(grepl("foil",tolower(Order_Details_Raw$condition)) == F,"","Foil")
            Sys.sleep(.2)
            Order_Details <- Order_Details_Raw[c(2,6,4,5)]
            
            
            Order_Content <- data.frame(Order_Content_Raw,Order_Details)
            Order_Content$Key <- paste(Order_Content$name,Order_Content$set,Order_Content$rarity," ",Order_Content$hasFoil,sep="")
            Order_Content <- Order_Content[c(8,1,2,3,4,5,6,7)]
            
            Sys.sleep(1)
            Order_Purchase_Info <- NULL
            
            
            order_amount <- as.numeric(gsub(" order\\(.*","",gsub(".* of ","",trimws(page_html %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))
            if(order_amount < 10){order_amount <- order_amount}else{order_amount <- 10}  
            for(j in 1:order_amount){
                tryCatch({
                    Check <- str_count(trimws(gsub("\n\\s\\s+",":",gsub("DETAILS","",page_html %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table') ) %>% html_text()))),"\\$")
                    
                    Summary <- data.frame( summary = gsub("\\(.*\\)","",gsub("\\$","",gsub(" ","",gsub("\n","",trimws(page_html%>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/span[1]/table/tbody/tr',sep="")) %>% html_text()))))))
                    Summary_Content_Raw <- t(data.frame(do.call('rbind', strsplit(as.character(Summary$summary),':',fixed=TRUE))))
                    colnames(Summary_Content_Raw) <- Summary_Content_Raw[1,]
                    Summary_Content_Raw <- Summary_Content_Raw[-1,]
                    
                    if(Check == 1){Summary_Content_Raw <- as.data.frame(t(Summary_Content_Raw))}else{
                        supplamental_pricing <- data.frame(very_raw = trimws(gsub("\\,","",gsub(":","",gsub("\n","",gsub("[A-Za-z]*","",page_html%>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table/tbody/tr',sep="")) %>% html_text()))))))
                        supplamental_pricing$very_raw <- gsub("^-","",gsub(" ","-",gsub("'","",gsub("\\$","",gsub("   ","",supplamental_pricing$very_raw)))))
                        supplamental_pricing <- data.frame(do.call('rbind', strsplit(as.character(supplamental_pricing$very_raw),'-',fixed=TRUE)))
                        colnames(supplamental_pricing) <- c("X1","X2")
                        supplamental_pricing$X1 <- trimws(supplamental_pricing$X1)
                        Summary_Content_Raw <- as.data.frame(t(Summary_Content_Raw))
                        for(k in 1:Check){
                            Summary_Content_Raw <- rbind(Summary_Content_Raw,Summary_Content_Raw[1,])
                        }
                        Summary_Content_Raw <- Summary_Content_Raw[-1,]
                        Summary_Content_Raw[2] <- supplamental_pricing$X1
                        Summary_Content_Raw[1] <- supplamental_pricing$X2
                    }
                    
                    
                    Order_Purchase_Info <- rbind(Order_Purchase_Info,mutate_all(as.data.frame(Summary_Content_Raw),function(x) as.character(x)))
                }, error = function(e){break})
            }
            rownames(Order_Purchase_Info) <- seq(nrow(Order_Purchase_Info))
            Order_Purchase_Info <- mutate_all(as.data.frame(Order_Purchase_Info),function(x) as.numeric(as.character(x)))
            
            Sys.sleep(1)
            
            Compiled_Dates <- NULL
            Compiled_order_Ids <- NULL
            for(j in 1:order_amount){
                tryCatch({
                    Check <- str_count(trimws(gsub("\n\\s\\s+",":",gsub("DETAILS","",page_html%>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table') ) %>% html_text()))),"\\$")
                    Dates_Raw <- trimws(page_html %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[1]/span[1]/span[2]',sep="")) %>% html_text())
                    Order_id_Raw <- trimws(page_html %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[1]/span[3]/text()',sep="")) %>% html_text())[2]
                    
                    if(Check == 1){}else{
                        for(k in 1:Check){
                            Dates_Raw <- rbind(Dates_Raw,Dates_Raw[1])
                            Order_id_Raw <- rbind(Order_id_Raw,Order_id_Raw[1])
                        }
                        Dates_Raw <- as.matrix(Dates_Raw[-1,1])
                        Order_id_Raw <- as.matrix(Order_id_Raw[-1,1])
                    }
                    Compiled_Dates <- rbind(Compiled_Dates,Dates_Raw)
                    Compiled_order_Ids <- rbind(Compiled_order_Ids,Order_id_Raw)
                }, error = function(e){print("Loop Error")})
            }
            colnames(Compiled_Dates) <- "Purchase_Dates"
            rownames(Compiled_Dates) <- seq(nrow(Compiled_Dates))
            Compiled_Dates <- as.data.frame(Compiled_Dates)
            Compiled_Dates$Purchase_Dates <- format(mdy(Compiled_Dates$Purchase_Dates),"%m/%d/%Y")
            
            Compiled_order_Ids <- as.data.frame(Compiled_order_Ids)
            colnames(Compiled_order_Ids) <- "TCG_Order_Num"
            rownames(Compiled_order_Ids) <- seq(nrow(Compiled_order_Ids))
            
            Sys.sleep(1)
            
            Page_Contents <- data.frame(Order_Content,Order_Purchase_Info,Compiled_Dates,Compiled_order_Ids)
            
            Page_Contents <- Page_Contents %>% mutate(set = ifelse(grepl("Extended Art",name),paste(set,"Variants"),set))
            
            All_Pages <- rbind(All_Pages,Page_Contents)
            remDr$findElement("class","nextPage")$clickElement()
            Sys.sleep(1)
            
        }
        
        All_Pages = All_Pages %>% left_join(Sets %>% select(mtgjson,TCG_Key,GF_Abbr), by = c("set"="TCG_Key")) %>% distinct() %>%
            mutate(mtgjson = ifelse(is.na(mtgjson),gsub(" Variants","",set),as.character(mtgjson) )) %>%
            select(-set,-abbrev) %>% mutate(set = mtgjson, abbrev = GF_Abbr) %>% select(-mtgjson, -GF_Abbr) %>%
            mutate(name = gsub("\\s\\(.*\\)","",name), Key = paste(name,set,rarity," ",hasFoil,sep="")) %>% select(Key, name, set, abbrev, everything()) %>%
            distinct()
        
        TCG_Full_History <- All_Pages
        TCG_Full_History$condition <- gsub(" FOIL","", TCG_Full_History$condition)
        for(i in 1:nrow(TCG_Full_History)){
            if(TCG_Full_History$Shipping[i] != 0.00){TCG_Full_History$Shipping[i] <- round(TCG_Full_History$Shipping[i] / TCG_Full_History$Quantity[i],2)}
        }
        for(i in 1:nrow(TCG_Full_History)){
            if(TCG_Full_History$SalesTax[i] != 0.00){TCG_Full_History$SalesTax[i] <- round(TCG_Full_History$SalesTax[i] / TCG_Full_History$Quantity[i],2)}
        }
        TCG_Full_History$Subtotal <- round(TCG_Full_History$Subtotal + TCG_Full_History$Shipping + TCG_Full_History$SalesTax,2)
        TCG_Full_History$Platform <- "TCG"
        TCG_Full_History$Scryfall <- Updated_Tracking_Keys$scryfall[match(trimws(TCG_Full_History$Key),Updated_Tracking_Keys$Key)]
        
        Historical_TCG_Full_History = read_csv("/home/cujo253/model_csvs/tcg_purchases_granular.csv")
        
        TCG_Full_History = TCG_Full_History %>% rbind(Historical_TCG_Full_History) %>% distinct()
        
        write_csv(TCG_Full_History,"/home/cujo253/model_csvs/tcg_purchases_granular.csv")
        
        return(TCG_Full_History) 
        
    }else{
        TCG_Full_History = read_csv("/home/cujo253/model_csvs/tcg_purchases_granular.csv") %>% distinct()
        return(TCG_Full_History)
    }
    
    
}

tcg_purchases_raw = tcg_purchases("64.225.20.203")

tcg_purchases_for_bind = tcg_purchases_raw %>% select(Key,Purchase_Dates,name,set,rarity,hasFoil,Total,TCG_Order_Num,Quantity,Platform) %>%
    rename(Date = Purchase_Dates,Card_Name = name, Set = set, Rarity = rarity, Cost = Total,Sender = TCG_Order_Num,Qty = Quantity) %>%
    mutate(Date = anytime(Date), hasFoil = ifelse(is.na(hasFoil),"","Foil")) %>% filter(Date >= "2020-10-01")

CS_Ledger_Cleaned = rbind(CS_Ledger_Cleaned,tcg_purchases_for_bind)

# Card Kingdom Sales ------------------------------------------------------


# ip = "64.225.20.203"
ck_login_sales <- function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr$navigate('https://cardkingdom.com/customer_login')
    username <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[1]/input')
    username$clickElement()
    username$sendKeysToElement(list("cjpach@mac.com"))
    Sys.sleep(.5)
    password <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[2]/input')
    password$clickElement()
    password$sendKeysToElement(list("dxICMH3XZU5X"))
    Sys.sleep(.5)
    remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[3]/button')$clickElement()
    Sys.sleep(.5)
    
    remDr$navigate('https://cardkingdom.com/myaccount/selling_history')
    
    page_source = remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    order_links = page_source %>% html_nodes('a') %>% html_attr('href') %>% as_tibble() %>% filter(grepl("page=",value)) %>% distinct()
    
    status <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('table') %>% html_table(fill=T) %>% as.data.frame()
    colnames(status) <- status[1,]
    
    order_links = rbind('https://cardkingdom.com/myaccount/selling_history',order_links)
    
    ck_orders_tbl = NULL
    for(i in 1:nrow(order_links)){
        remDr$navigate(order_links$value[i])
        status <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('table') %>% html_table(fill=T) %>% as.data.frame()
        colnames(status) <- status[1,]
        
        ck_sell_orders <- status[-1,] %>% filter(Status != "CANCELED") %>% 
            mutate(Status = gsub("[A-Za-z]+\n\\s*\\-*\\s*[A-Za-z]+\\s","",Status),
                   `Order Date` = mdy(gsub("\\s\\d{2}\\:.*$","",`Order Date`))) %>%
            mutate(Status = suppressWarnings(mdy(Status))) %>%
            mutate(Status = as.Date(ifelse(is.na(Status), `Order Date` + 14, Status),origin = "1970-01-01" ),
                   `Payment Method` = ifelse( grepl("STORECREDIT.*",`Payment Method`) == T,"CK-B","CK")) %>% 
            select(-`Mailing Address`) 
        
        ck_orders_tbl = rbind(ck_orders_tbl,ck_sell_orders)
        
    }
    
    write_csv(ck_orders_tbl,"/home/cujo253/model_csvs/ck_sales.csv")
    
    already_obtained_sales = read_csv("/home/cujo253/model_csvs/ck_sales.csv") %>% select(`Order ID`) %>% clean_names() 
    already_obtained_granular = read_csv("/home/cujo253/model_csvs/ck_sales_granular.csv") %>% select(sell_id) %>% distinct()
    
    new_sales = already_obtained_sales %>% filter(order_id %!in% already_obtained_granular$sell_id)
    
    canceled_sales = already_obtained_granular %>% filter(sell_id %!in% already_obtained_sales$order_id)
    
    if(nrow(new_sales)==0){
        ck_sell_order = read_csv("/home/cujo253/model_csvs/ck_sales_granular.csv")
    } else {
        ck_sell_order <- NULL
        for(i in 1:length(new_sales$order_id)){
            remDr$navigate(paste("https://cardkingdom.com/myaccount/po_invoice/",new_sales$order_id[i],sep=""))
            order_contents <- data.frame(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('table') %>% html_table(fill = T)) %>% slice(-c(1:2)) %>%
                mutate(edition = ifelse((X1 == X2) & (X1 == X3),X1,NA)) %>% fill(edition) %>%
                filter(is.na(X2) != T) %>% filter(grepl("@",X2)==T) %>%
                separate(X2,c("X4","ind_value"),sep="\\s\\@\\s\\$") %>%
                separate(X4,c("qty","condition"),sep="\\s") %>%
                mutate(edition = gsub("\\s*\\/.*","",edition),
                       total_value = gsub("^\\$","",X3),
                       card_name = gsub("\\s*\\(.*\\)\\s*","",X1),
                       sell_id = ck_orders_tbl$`Order ID`[i],
                       dos = ck_orders_tbl$Status[i],
                       `Platform Sold` = ck_orders_tbl$`Payment Method`[i],
                       hasFoil = ifelse(grepl("Foil",edition)==T,"FOIL",""),
                       edition = gsub(" Foil","",edition),
                       semi = paste(card_name,edition,sep="")) %>%
                select(semi,card_name,edition,hasFoil,qty,ind_value,total_value,sell_id,dos,`Platform Sold`)
            
            
            
            ck_sell_order <- rbind(ck_sell_order,order_contents)
            
        }
        ck_sell_order = ck_sell_order %>% rbind(read_csv("/home/cujo253/model_csvs/ck_sales_granular.csv"))
        
        ck_sell_order = ck_sell_order %>% filter(sell_id %!in% canceled_sales$sell_id)
        
        write_csv(ck_sell_order,"/home/cujo253/model_csvs/ck_sales_granular.csv")
        
        return(ck_sell_order)
    }
}
ck_sales = ck_login_sales("64.225.20.203")


ck_sold_tbl = ck_sales %>% 
    mutate(edition = gsub("","",gsub(" Variants","",as.character(edition) ))) %>%
    left_join(Sets %>% select(CK_BL_Scrape_Sets,mtgjson) %>% mutate(mtgjson = as.character(mtgjson)),by=c("edition"="CK_BL_Scrape_Sets")) %>%
    mutate(mtgjson = ifelse(is.na(mtgjson),as.character(edition),mtgjson),
        semi = paste(card_name,mtgjson,sep="")) %>% 
    left_join(Updated_Tracking_Keys %>% select(param,Semi,Rarity) %>% group_by(Semi) %>% filter(param == min(param)) %>% ungroup() ,by=c("semi"="Semi")) %>%
    distinct() %>%
    mutate(Key = paste(card_name,mtgjson,Rarity," ",hasFoil,sep="")) %>%
    select(param,Key,card_name,mtgjson,Rarity,hasFoil,qty,ind_value,dos,sell_id,`Platform Sold`) %>% distinct() %>%
    rename(name = card_name,Set=mtgjson,Ind_Qty=qty,Ind_Amt = ind_value,DOS=dos) %>% arrange(desc(DOS)) %>% 
    filter(DOS >= "2020-10-01") %>%
    mutate(hasFoil = ifelse(is.na(hasFoil),"",hasFoil))



# TCG Sales ---------------------------------------------------------------

ip = "64.225.20.203"
tcg_login_sales <-function(ip = "159.65.219.70"){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr$navigate("https://tcgplayer.com")
    Sys.sleep(sample(5:9, 1))
    remDr$findElement("xpath",'//*[@id="app"]/div/header/div/div[3]/div[1]/div[2]/div[1]')$clickElement()
    Sys.sleep(sample(5:9, 1))
    remDr$findElement("css",'.account-actions-menu__title a')$clickElement()
    
    username <- remDr$findElement("xpath",'//*[@id="signInForm"]/div[1]/input')
    Sys.sleep(sample(1:3, 1))
    username$clickElement()
    Sys.sleep(sample(1:3, 1))
    username$sendKeysToElement(list("cjp"))
    Sys.sleep(.5)
    username$sendKeysToElement(list("ach"))
    Sys.sleep(.5)
    username$sendKeysToElement(list("@iclou"))
    Sys.sleep(.5)
    username$sendKeysToElement(list("d.com"))
    Sys.sleep(.5)
    
    Sys.sleep(.5)
    Password <- remDr$findElement("xpath",'//*[@id="signInForm"]/div[2]/section/input')
    Sys.sleep(sample(1:3, 1))
    Password$clickElement()
    Sys.sleep(sample(1:3, 1))
    Password$sendKeysToElement(list("Ta"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("si"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("gu"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("r9"))
    Sys.sleep(.5)
    Password$sendKeysToElement(list("5$"))
    Sys.sleep(.5)
    
    remDr$findElement('xpath','//*[@id="signInForm"]/label/span[1]')$clickElement()
    
    Sys.sleep(3)
    remDr$findElement("xpath",'//*[@id="signInForm"]/button')$clickElement() 
    Sys.sleep(3)
    remDr$navigate("https://store.tcgplayer.com/admin/payment/sellerpayment")
    Sys.sleep(2)
    remDr$findElement("xpath",'//*[@id="PageSize"]/option[6]')$clickElement()
    Sys.sleep(5)
    Payment_Links <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href") %>% as_tibble() %>% 
        filter( (grepl("admin\\/payment\\/SellerPaymentOrders",value)) | (grepl("admin\\/payment\\/PendingPaymentOrders",value))) %>%
        mutate(value = paste("https://store.tcgplayer.com/",value,sep=""))
    
    Payment_tbl = Payment_Links %>% mutate(unique_payment_id = gsub("https://store.tcgplayer.com//admin/payment/SellerPaymentOrders/","",value),
                                           number_of_payments = nrow(.),
                                           order_of_payments = seq(nrow(.))) 
    
    existing_payments = read_csv("/home/cujo253/model_csvs/tcg_payment_count.csv") %>% .[2:nrow(.),] #%>% rename(order_of_payments = rder_of_payments)
    
    new_sales = Payment_tbl %>% filter(unique_payment_id %!in% existing_payments$unique_payment_id)
    
    #new_sales = Payment_tbl %>% .[1:5,]
    
    if(nrow(new_sales) > 0) {
    
    write_csv(Payment_Links %>% mutate(unique_payment_id = gsub("https://store.tcgplayer.com//admin/payment/SellerPaymentOrders/","",value),
                                       number_of_payments = nrow(.),
                                       order_of_payments = seq(nrow(.))) %>%
                  rename(links = value),"/home/cujo253/model_csvs/tcg_payment_count.csv")
    
    
    Order_Links = NULL
    sales_order = NULL
    refund_orders = NULL
    for(i in 1:nrow(new_sales)){
        remDr$navigate(new_sales$value[i])
        Links <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href") %>% as_tibble() %>% filter(grepl("admin\\/Orders",value)) %>% 
            mutate(value = paste("https://store.tcgplayer.com/",value,sep=""))
        
        sales = remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_table(fill = T) %>% .[[1]] %>% as_tibble()
        
        refunds = remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_table(fill = T) %>% .[[2]]
        refunds = if(refunds[1] == "No Adjustments in this batch."){NA}else{refunds %>% as_tibble()}
        
        if(is.na(refunds[1])==F){refund_orders = rbind(refund_orders,refunds %>% as_tibble())}
        
        sales_order = rbind(sales_order, sales)
        Order_Links = rbind(Order_Links,Links)
        
        Sys.sleep(.5)
    }
    
    old_sales = read_csv("/home/cujo253/model_csvs/tcg_payment_info_on_buyers.csv")
    
    sales_order = rbind(old_sales,sales_order%>%clean_names()) %>% distinct()
    
    write_csv(sales_order %>% clean_names(),"/home/cujo253/model_csvs/tcg_payment_info_on_buyers.csv")
    
    old_orders = read_csv("/home/cujo253/model_csvs/tcg_payment_order_links.csv")
    
    old_orders = old_orders %>% distinct() %>% mutate(order_of_payments = seq(nrow(.)))
    
    New_Order_Links = Order_Links
    
    Order_Links = rbind(old_orders,Order_Links%>% mutate(unique_payment_id = gsub("https://store.tcgplayer.com//admin/Orders/ManageOrder/","",links),
                                                        number_of_payments = nrow(.),
                                                        order_of_payments = seq(nrow(.))) %>% #rename(links = value)) %>% 
        distinct())
    
    write_csv(Order_Links%>% mutate(unique_payment_id = gsub("https://store.tcgplayer.com//admin/Orders/ManageOrder/","",links),
                                    number_of_payments = nrow(.),
                                    order_of_payments = seq(nrow(.))),"/home/cujo253/model_csvs/tcg_payment_order_links.csv")
    
    # sales_order %>% mutate(`Total Sale (+)` = gsub("\\$","",`Total Sale (+)`)) %>% group_by(Buyer) %>% summarize(sales = sum(as.numeric(`Total Sale (+)`)),
    #                                                                                                              orders = n()) %>% arrange(desc(orders))
    # 
    
    
    Recent_Sales <- NULL
    
    for(i in 1:nrow(New_Order_Links)){
        remDr$navigate(New_Order_Links$value[i])
        page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
        if(length(page_html %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()) == 0){
            Test_Number = length(page_html %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
        }else(
            Test_Number = length(page_html %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
        )
        if (Test_Number == 1){
            page_source = remDr$getPageSource() %>% .[[1]] %>% read_html()
            Card_Info <- page_source %>% html_nodes('a') %>% html_attr("href")
            Card_Info <- data.frame(Card_Info[grepl("productSearch",Card_Info)])
            colnames(Card_Info) <- "link"
            Card_Link <- gsub("../../..","https://store.tcgplayer.com",Card_Info$link)
            
            
            Param <- as.data.frame(Card_Link) %>% separate(Card_Link[1],c("1","2"),"/productSearch/")
            Param <- as.data.frame(Param$`2`)
            colnames(Param) <- "param"
            Param$Trans_ID <- Order_Links$Trans_ID[i]
            Param$Key <- Updated_Tracking_Keys$Key[match(Param$param,Updated_Tracking_Keys$param)]
            Param$name <- Updated_Tracking_Keys$name[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Set <- Updated_Tracking_Keys$Set[match(Param$param,Updated_Tracking_Keys$param)]
            Param$abbr <- Updated_Tracking_Keys$abbr[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Rarity <- Updated_Tracking_Keys$Rarity[match(Param$param,Updated_Tracking_Keys$param)]
            
            card_info = page_source %>% html_nodes('xpath' = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[1]/a/text()') %>% html_text()
            
            card_info = if(length(card_info)==0){page_source %>% html_nodes('xpath' = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[1]/a/text()') %>% html_text()}else{card_info}
            
            card_info = if(length(card_info)==0){page_source %>% html_nodes('xpath' = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[1]/a/text()') %>% html_text()}else{card_info}
            
            Param$Foil <- if(grepl("Foil",card_info)){"FOIL"}else{""}
            
            Param$Foil[is.na(Param$Foil)]<-""
            
            Character_Check <- character(0)
            Param$Ind_Qty <-  if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text(),Character_Check) == T){
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }else{
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }
            
            Param$Order_Qty <- Param$Ind_Qty
            
            Param$Order_Amt <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check)){gsub("Order Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text())}else{gsub("Order Amount\\: \\$","",page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text())}
            
            Param$Base_Fee <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text(),Character_Check)){gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text()))}else{gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text()))}
            
            Test_For_Direct <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}else{page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}
            
            if(any(grepl("Direct",Test_For_Direct))==T){
                Param$Direct_Fee <- if(identical(page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}else{as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}
            } else{
                Param$Direct_Fee <- 0.00025
            }
            
            suppressWarnings(if(Param$Direct_Fee == 0.00025){
                Param$Net_Rev <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}
            } else {
                Param$Net_Rev <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}
            })
            Param$Ind_Amt <- Param$Order_Amt
            
            Param$DOS <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text(),Character_Check)){page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}else{page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}
            
            Param$Ind_Rev <- Param$Net_Rev
            
            Param$sell_id= gsub(".*\\/","",New_Order_Links$value[i])
            
            Recent_Sales <- rbind(Recent_Sales,Param)
            Sys.sleep(3)
        }else{
            page_source = remDr$getPageSource() %>% .[[1]] %>% read_html()
            Card_Info <- page_source %>% html_nodes('a') %>% html_attr("href")
            Card_Info <- data.frame(Card_Info = Card_Info[grepl("productSearch",Card_Info)])
            Card_Info <- data.frame(do.call('rbind', strsplit(as.character(Card_Info$Card_Info),'/productSearch/',fixed=TRUE)))
            Param <-data.frame(param = Card_Info$X2)
            Param$Trans_ID <- Order_Links$Trans_ID[i]
            Param$Key <- Updated_Tracking_Keys$Key[match(Param$param,Updated_Tracking_Keys$param)]
            Param$name <- Updated_Tracking_Keys$name[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Set <- Updated_Tracking_Keys$Set[match(Param$param,Updated_Tracking_Keys$param)]
            Param$abbr <- Updated_Tracking_Keys$abbr[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Rarity <- Updated_Tracking_Keys$Rarity[match(Param$param,Updated_Tracking_Keys$param)]
            
            card_info = page_source %>% html_nodes('xpath' = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[1]/a/text()') %>% html_text()
            
            card_info = if(length(card_info)==0){page_source %>% html_nodes('xpath' = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[1]/a/text()') %>% html_text()}else{card_info}
            
            card_info = if(length(card_info)==0){page_source %>% html_nodes('xpath' = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[1]/a/text()') %>% html_text()}else{card_info}
            
            Param$Foil <- if(grepl("Foil",card_info)){"FOIL"}else{""}
            
            Param$Foil[is.na(Param$Foil)]<-""
            Character_Check <- character(0)
            test_qty <-  if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text(),Character_Check) == T){
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }else{
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }
            
            Param$Ind_Qty = if(identical(test_qty,Character_Check)){
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            } else{
                test_qty
            }
            
            Param$Order_Qty <- sum(as.numeric(Param$Ind_Qty))
            
            test_amt <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check) == T){
                as.numeric(gsub("Order Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
            }else{
                as.numeric(gsub("Order Amount\\: \\$","",page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
            }
            
            Param$Order_Amt = if(identical(test_amt,numeric(0))){as.numeric(gsub("Order Amount\\: \\$","",page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))} else{test_amt}
            
            
            Test_Base_Fee <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text(),Character_Check) == T){
                as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
            }else{
                as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
            }
            
            Param$Base_Fee = if(identical(Test_Base_Fee,numeric(0))){
                as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
            } else {
                Test_Base_Fee
            }
            
            Test_For_Direct <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}else{page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}
            
            if(any(grepl("Direct",Test_For_Direct))==T){
                Param$Direct_Fee <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}else{as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}
            } else{
                Param$Direct_Fee <- 0.00025
            }
            
            value = NULL
            suppressWarnings(if(Param$Direct_Fee == 0.00025){
                value = if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}
            }) 
            if(identical(value,numeric(0))) {
                value = if(identical(page_source%>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}
            }
            
            new_value = if(is.null(value)){as.numeric(gsub("\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]/td[2]') %>% html_text()))}else{value}
            
            new_value = if(identical(new_value,numeric(0))){as.numeric(gsub("\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]/td[2]') %>% html_text()))}else{new_value}
            
            new_value = if(identical(new_value,numeric(0))){as.numeric(gsub("\\$","",page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]/td[2]') %>% html_text()))}else{new_value}
            
            Param$Net_Rev = new_value
            
            Param$Ind_Amt <- as.numeric(gsub(" .*","",gsub(".*  \\$","",page_source %>% html_nodes('.gradeA') %>% html_text()))%>% as.data.frame() %>% filter(!grepl(".*\\/.*",.)) %>% unlist())* as.numeric(Param$Ind_Qty)
            test_dos <- if(identical(page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text(),Character_Check)){
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()
            }else{
                page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()
            }
            
            Param$DOS = if(identical(test_dos,character(0))){page_source %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[6]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}else{test_dos}
            
            Param$Base_Fee <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Base_Fee,2)
            Param$Direct_Fee <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Direct_Fee,2)
            Param$Ind_Rev <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Net_Rev,2)
            
            Param$sell_id= gsub(".*\\/","",New_Order_Links$value[i])
            
            Recent_Sales <- rbind(Recent_Sales,Param)
        }
    }
    Safety_Sales <- Recent_Sales %>% distinct()
    Recent_Sales <- Recent_Sales %>% distinct() %>%
        mutate(Base_Fee = as.numeric(gsub("Fee Amount: \\$","",Base_Fee)))
    
    NA_Saviours <- Recent_Sales[which(is.na(Recent_Sales$name)==T ),]
    
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    statement = "SELECT Product_ID param,Name name,a.Set,Version abbr,Rarity FROM `gaeas-cradle.roster.fabroster` a "
    fab_roster = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1) %>% mutate(Rarity = left(Rarity,1))
    
    fab_nas = NA_Saviours %>% 
        filter(param %in% fab_roster$param) %>% 
        select(-Key,-name,-Set,-abbr,-Rarity) %>%
        left_join(fab_roster, by=c("param"="param")) %>%
        mutate(Key = paste(name,Set,abbr,Rarity," ",Foil,sep="")) %>%
        select(param,Key,name,Set,abbr,Rarity,Foil,everything()) %>% 
        distinct()
    
    con <- pokemon_ebay("wolfoftinstreet@gmail.com")
    statement = "SELECT Product_ID param, Key, Card_name name,a.Set,number abbr, Rarity FROM `pokemon-ebay.tcg_pokedex.2021_06_22_tcg_collection` a "
    poke_roster = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1) %>% mutate(Rarity = left(Rarity,1), param = as.character(param))
    
    poke_nas = NA_Saviours %>% 
        filter(param %in% poke_roster$param) %>% 
        select(-Key,-name,-Set,-abbr,-Rarity) %>%
        left_join(poke_roster, by=c("param"="param")) %>%
        mutate(Key = paste(name,Set,abbr,Rarity," ",Foil,sep="")) %>%
        select(param,Key,name,Set,abbr,Rarity,Foil,everything()) %>%
        distinct()
    
    NA_Saviours = NA_Saviours %>% 
        filter(param %!in% fab_roster$param) %>%
        filter(param %!in% poke_roster$param)
        
    if(nrow(NA_Saviours) >0){
    Replacements <- NA_Saviours
    Saviours <- NULL
    for (i in 1:nrow(NA_Saviours)){
        Saviour_Links <- paste("https://store.tcgplayer.com/productCatalog/product/productSearch/",NA_Saviours$param[i],sep="")
        
        remDr$navigate(Saviour_Links)
        Sys.sleep(1)
        page_source = remDr$getPageSource() %>% .[[1]] %>% read_html()
        
        Replacements$name[i] <- if(identical(if(identical(page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text(),Character_Check)){
            page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()
        } else {page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()},Character_Check)){
            page_source %>% html_nodes(xpath = '//*[@id="app"]/div/section[2]/div[1]/div/h1') %>% html_text() %>% trimws()} else {if(identical(page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text(),Character_Check)){
                page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()
            } else {page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()}
                
            }
        Sys.sleep(1)
        
        Replacements$Set[i] <- if(identical(if(identical(page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text(),Character_Check)){
            page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()
        } else {page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()},Character_Check)){
            page_source %>% html_nodes(xpath = '//*[@id="app"]/div/section[2]/div[1]/div/span/a/h2') %>% html_text() %>% trimws()
        } else {
            if(identical(page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text(),Character_Check)){
                page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()
            } else {page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()}
        }
        Sys.sleep(1)
        
        Replacements$abbr[i] <- if(is.na(Updated_Tracking_Keys$abbr[match(Replacements$Set[i],Updated_Tracking_Keys$Set)])){""}else{Updated_Tracking_Keys$abbr[match(Replacements$Set[i],Updated_Tracking_Keys$Set)]}
        
        Replacements$Rarity[i] <- if(identical(if(identical(gsub("\\,.*","",page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text()), Character_Check)){
            page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text()
        } else {
            gsub("\\,.*","",page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text())
        },Character_Check)){"S"} else {
            if(identical(gsub("\\,.*","",page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text()), Character_Check)){
                page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text()
            } else {
                gsub("\\,.*","",page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text())
            } 
        }
        Sys.sleep(.5)
        Replacements$Key[i] <- paste(Replacements$name[i],Replacements$Set[i],Replacements$Rarity[i],sep="")
        
    }
    }
    
    Final_Sales <- rbind(Recent_Sales,Replacements,fab_nas,poke_nas)
    
    #Recent_Sales <- Recent_Sales %>% filter(grepl("node = ",Key)==F)
    
    Final_Sales <- na.omit(Final_Sales)
    
    
    Final_Sales$Direct_Fee <- round(Final_Sales$Direct_Fee,2)
    #devtools::install_github("eddelbuettel/anytime",force = TRUE)
    Final_Sales$DOS <- anydate(Final_Sales$DOS)
    Final_Sales = Final_Sales %>% mutate(Foil = ifelse(Foil == "FOIL","Foil",""))
    
    old_final_sales = read_csv("/home/cujo253/model_csvs/tcg_payment_all_orders.csv")
    
    Final_Sales = rbind(old_final_sales,Final_Sales%>% clean_names()) %>% distinct() %>% arrange(desc(dos)) 
    
    write_csv(Final_Sales %>% clean_names(),"/home/cujo253/model_csvs/tcg_payment_all_orders.csv")
    
    
    return(Final_Sales)
    } else {
        Final_Sales = read_csv("/home/cujo253/model_csvs/tcg_payment_all_orders.csv")
        return(Final_Sales)
    }
}

tcg_sales = tcg_login_sales()

#tcg_sales = Final_Sales

tcg_sales = tcg_sales %>% mutate(foil = ifelse(is.na(foil),"",foil))

tcg_sales_bind = tcg_sales %>% rename(Key=key,Set = set, Rarity = rarity, hasFoil = foil, Ind_Qty = ind_qty,Ind_Amt = ind_amt, DOS = dos) %>%
    mutate(`Platform Sold` = "TCG") %>%
    select(param,Key,name,Set,Rarity,hasFoil,Ind_Qty,Ind_Amt,DOS,sell_id,`Platform Sold`) %>%
    filter(DOS >= "2020-10-01")

# Fifo --------------------------------------------------------------------

purchases_singles_tbl = CS_Ledger_Cleaned %>%
    mutate(Semi = paste(Card_Name,Set,sep=""),Qty = ifelse(Card_Name == "Ambition's Cost",1,Qty),
           #hasFoil = ifelse(is.na(hasFoil),"","Foil"),
           hasFoil = toupper(hasFoil),
           Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,sep=""))) %>%
    mutate(Key = paste(Card_Name,Set,Rarity,hasFoil,sep="")) %>% select(-Semi) %>% 
    filter(is.na(Qty)==F) %>%
    filter(Date >= "2020-10-01")


pivot_purch_tbl = purchases_singles_tbl %>% mutate(Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,sep="")),
                                                   Date = round_date(Date,unit="day")) %>% 
    group_by(Key,Date) %>% summarize(Cost = round(sum(Cost)/sum(Qty),2), Buy_Qty = sum(Qty)) %>% ungroup()


multiple_purchases = purchases_singles_tbl %>% mutate(Date = round_date(Date,unit="day")) %>% arrange(Key,Date) %>% filter(Qty > 1)

#multiple_purchases %>% filter(grepl("^Fang",Card_Name))

replacement_lines = NULL
if(nrow(multiple_purchases) != 0){
    for (i in 1:nrow(multiple_purchases)){
        new_lines = NULL
        for (j in 1:(multiple_purchases[i,]$Qty)){
            new_lines = rbind(new_lines,multiple_purchases[i,])
        }
        new_lines$Qty = 1
        new_lines$Cost = round(as.numeric(unique(new_lines$Cost)) / as.numeric(multiple_purchases[i,]$Qty),2)
        replacement_lines = rbind(replacement_lines,new_lines)
    }
}

unnested_purchases = purchases_singles_tbl                        %>% 
    mutate(Date = round_date(Date,unit="day")) %>% 
    arrange(Key,Date)                          %>% 
    filter(Qty <= 1)                           %>% 
    rbind(replacement_lines)                   %>%
    arrange(Date,Key)                          %>%
    filter(Date >= "2020-10-01")

#unnested_purchases %>% filter(grepl("Kaalia",Card_Name))
unique_keys = unnested_purchases %>% select(Key) %>% distinct()

regrouped_purchases = NULL
for(i in 1:nrow(unique_keys)){
    data_subbed = unnested_purchases %>% filter(Key == unique_keys$Key[i]) %>% mutate(ct = seq(nrow(.)))
    regrouped_purchases = rbind(regrouped_purchases,data_subbed)
}


#regrouped_purchases %>% filter(Card_Name == "Bitterblossom")
all_sales = ck_sold_tbl %>% mutate(DOS = round_date(DOS,unit="day"), 
                                   hasFoil = ifelse(hasFoil == "FOIL","Foil",hasFoil),
                                   Rarity = ifelse((grepl("San Diego",Set)==T) & (is.na(Rarity)==T), "M",Rarity),
                                   Key = paste(name,Set,Rarity," ",hasFoil,sep="")) %>% 
    rbind(tcg_sales_bind %>% 
              mutate(hasFoil = ifelse(hasFoil == "FOIL","Foil",hasFoil),
                     Key = paste(name,Set,Rarity," ",hasFoil,sep=""))) %>% distinct()

multiple_sales = all_sales %>% arrange(Key,DOS) %>% filter(Ind_Qty > 1)

#multiple_purchases %>% view()
replacement_lines = NULL
for (i in 1:nrow(multiple_sales)){
    new_lines = NULL
    for (j in 1:as.numeric((multiple_sales[i,]$Ind_Qty))){
        new_lines = rbind(new_lines,multiple_sales[i,])
    }
    new_lines$Ind_Qty = 1
    new_lines$Ind_Amt = round(as.numeric(unique(new_lines$Ind_Amt)) / as.numeric(multiple_sales[i,]$Ind_Qty),2)
    replacement_lines = rbind(replacement_lines,new_lines)
}


unnested_sales = all_sales          %>%
    #rbind(custom_sales)                      %>%
    mutate(DOS = round_date(DOS,unit="day")) %>% 
    arrange(Key,DOS)                         %>% 
    filter(Ind_Qty <= 1)                     %>% 
    rbind(replacement_lines)                 %>%
    arrange(DOS,Key)                         %>%
    filter(DOS >= "2020-10-01")              %>%
    mutate(Key = trimws(Key))


unique_keys = unnested_sales %>% select(Key) %>% distinct()


regrouped_sales = NULL
for(i in 1:nrow(unique_keys)){
    data_subbed = unnested_sales %>% filter(Key == unique_keys$Key[i]) %>% mutate(ct = seq(nrow(.)))
    regrouped_sales = rbind(regrouped_sales,data_subbed)
}

manual_sales_addition_1 = data.frame(param = "27765", 
                                     Key =  "Rafiq of the ManyShards of AlaraM FOIL", 
                                     name = "Rafiq of the Many",
                                     Set = "Shards of Alara",
                                     Rarity = "M",
                                     hasFoil = "FOIL",
                                     Ind_Qty = "1",
                                     Ind_Amt = "93.31",
                                     DOS = anytime("2021-06-11"),
                                     sell_id = "BL20210611-E1F1BDBB",
                                     `Platform Sold` = "TCG",
                                     ct = 1) %>% rename(`Platform Sold`=Platform.Sold)

regrouped_sales = rbind(regrouped_sales,manual_sales_addition_1) 

month_sales_review = regrouped_sales %>% filter(DOS > "2021-03-01") %>% mutate(Ind_Amt = as.numeric(Ind_Amt), month = month(DOS)) %>% group_by(month) %>% summarize(revenue = sum(Ind_Amt) )
month_purchases_review = regrouped_purchases  %>% filter(Date > "2021-03-01") %>% mutate(month = month(Date)) %>% group_by(month) %>% summarize(Cost = sum(Cost))

# regrouped_sales %>% filter(grepl("Grand Abol",name))
# regrouped_purchases %>% filter(grepl("Grand Abol",Card_Name))
# month_sales_review %>% 
#     left_join(month_purchases_review,by=c("month"="month")) %>%
#     mutate(Cost = ifelse(is.na(Cost),0,Cost),profit = revenue - Cost) %>%
#     summarize(sum(profit))

# regrouped_sales %>% 
#     mutate(Key = trimws(paste(name,Set,Rarity," ",hasFoil,ct,sep=""))) %>%
#     select(Key,Ind_Amt,DOS,sell_id,`Platform Sold`) %>%
#     filter(grepl("Rafiq",Key))
#     
# regrouped_purchases %>% mutate(Set = as.character(Set)) %>%
#     mutate(Set = ifelse( (Card_Name == "Ulamog, the Ceaseless Hunger") & (grepl("San Diego",Set) & (Date == "2021-04-11") ), "Battle for Zendikar Promos",Set )) %>%
#     mutate(Set = ifelse( (Card_Name == "Marrow-Gnawer") & (Date == "2021-05-02"), "Mystery Booster Retail Edition Foils",Set )) %>%
#     mutate(Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,ct,sep=""))) %>%
#     filter(grepl("Leyline",Key))

final_ledger = regrouped_purchases %>% mutate(Set = as.character(Set)) %>%
    
    mutate(Set = ifelse( (Card_Name == "Ulamog, the Ceaseless Hunger") & (grepl("San Diego",Set) & (Date == "2021-04-11") ), "Battle for Zendikar Promos",Set )) %>%
    mutate(Rarity = ifelse( (Card_Name == "Ulamog, the Ceaseless Hunger") & (Date == "2021-04-11"), "M",Rarity )) %>%
    
    mutate(Set = ifelse( (Card_Name == "Marrow-Gnawer") & (Date == "2021-05-02"), "Mystery Booster Retail Edition Foils",Set )) %>%
    mutate(Rarity = ifelse( (Card_Name == "Marrow-Gnawer") & (Date == "2021-05-02"), "R",Rarity )) %>%
    
    mutate(hasFoil = ifelse(hasFoil=="FOIL","Foil",hasFoil)) %>%
    mutate(Card_Name =  gsub("Leonin WarleaderLeonin Warleader","Leonin Warleader",Card_Name),
       Card_Name =  gsub("Elvish ArchersElvish Archers","Elvish Archers",Card_Name)) %>%
    mutate(Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,ct,sep=""))) %>%
    left_join(regrouped_sales %>% 
                  mutate(Key = trimws(paste(name,Set,Rarity," ",hasFoil,ct,sep=""))) %>%
                  select(Key,Ind_Amt,DOS,sell_id,`Platform Sold`),
              by = c("Key"="Key")) %>%
    mutate(Ind_Amt = as.numeric(Ind_Amt),Ind_Amt = ifelse(`Platform Sold`=="CK-B",round(Ind_Amt *1.3,2),Ind_Amt )) %>%
    rename(Sold=Ind_Amt)


resting_inv = final_ledger %>% 
    filter(is.na(DOS)) %>% 
    filter(Key != "Karn LiberatedModern Masters 2015M 1") %>%
    filter(Key != "Nicol Bolas, God-PharaohSan Diego Comic-Con 2013S Foil1") %>%
    filter(grepl("Booster Box",Key)==F) %>%
    filter(Card_Name != "Card Kingdom Blue Box") %>%
    filter(Card_Name != "Fool's Gold") %>%
    filter(grepl("Boon of Erebos",Card_Name)==F) %>%
    group_by(Card_Name, Set, Rarity, hasFoil) %>%
    summarize(Date = mean(Date),Qty = sum(Qty), Cost = round(mean(Cost),2)) %>%
    ungroup() %>%
    mutate(hold_time = round(as.numeric(Sys.Date()-Date),0),
           hasFoil = ifelse(is.na(hasFoil),"",hasFoil)) %>%
    arrange(desc(hold_time)) %>%
    mutate(Key = trimws(paste(Card_Name,Set,Rarity,' ',hasFoil,sep="")) ) %>%
    left_join(Updated_Tracking_Keys %>% select(Key,uuid,ckid),by=c("Key"="Key")) %>%
    distinct() %>%
    filter( (ckid != 239153 | Date != "2021-03-22") ) %>%
    filter( (ckid != 235961 | Date != "2021-04-22") ) %>%
    filter( (ckid != 233921 | Date != "2021-05-27") ) %>%
    filter( (ckid != 232397 | Date != "2021-06-06") ) %>%
    filter( (is.na(ckid) == T | Date != "2021-05-18") )


resting_inv_nas = resting_inv %>% filter(is.na(uuid)) %>% filter(is.na(Set)==F) %>% filter(is.na(Cost)==F)

ck_bl = fromJSON("https://api.cardkingdom.com/api/pricelist")                                                       %>% 
    as.data.frame() %>% clean_names() %>% 
    mutate(data_edition = ifelse(data_edition == "Promotional",data_variation,data_edition))                %>%
    mutate(data_is_foil = ifelse(data_is_foil=="false","","Foil"))                                          %>%
    separate(data_sku,c("abbr","number"),sep="-")                                                           %>%
    mutate(number = as.numeric(gsub("^0*","",number)))                                                      %>%
    mutate(Key = paste(data_name,number,data_is_foil,sep=""))                                               %>%
    select(data_id,Key,data_name,data_edition,data_variation,
           data_is_foil,number,abbr,data_price_buy,data_qty_buying)                                         %>%
    left_join(Updated_Tracking_Keys %>% 
                  mutate(Key = paste(name,number,Foil,sep="")) %>%
                  select(Key,uuid,param,ckid,ckid_f,Rarity),by=c("data_id"="ckid"))



bl_check = resting_inv_nas %>% mutate(Set = as.character(Set)) %>%
    mutate(uuid = ifelse( (Card_Name == "Mana Drain") & (Date == "2021-04-20"), "06fa2550-a64d-5ae1-bcc2-abc2c7a2eaf9",uuid )) %>%
    
    mutate(hasFoil = ifelse( (Card_Name == "Mana Vault") & (grepl("Box Topper",Set) & (Date == "2021-05-02") ), "Foil",hasFoil )) %>%
    mutate(uuid = ifelse( (Card_Name == "Mana Vault") & (grepl("Box Topper",Set) & (Date == "2021-05-02") ), "20efbb70-db8a-5618-a8ba-d1814a747239",uuid )) %>%
    
    mutate(hasFoil = ifelse( (Card_Name == "Marrow-Gnawer") & (grepl("Mystery Booster",Set) & (Date == "2021-05-02") ), "Foil",hasFoil )) %>%
    mutate(Set = ifelse( (Card_Name == "Marrow-Gnawer") & (grepl("Mystery Booster",Set) & (Date == "2021-05-02") ), "Secret Lair",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Marrow-Gnawer") & (grepl("Secret Lair",Set) & (Date == "2021-05-02") ), "88339ccb-b4f2-53df-97a9-0d8b2e8bca9d",uuid )) %>%
    
    mutate(hasFoil = ifelse( (Card_Name == "Rick, Steadfast Leader") & (grepl("Secret",Set) & (Date == "2021-05-02") ), "Foil",hasFoil )) %>%
    mutate(uuid = ifelse( (Card_Name == "Rick, Steadfast Leader") & (grepl("Secret",Set) & (Date == "2021-05-02") ), "e95969b4-1fab-599b-a712-f96a17335ce3",uuid )) %>%
    
    
    mutate(Set = ifelse( (Card_Name == "Nicol Bolas, God-Pharaoh") & (grepl("San Diego",Set) & (Date == "2021-04-22") ), "SDCC 2017 Promo Foil",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Nicol Bolas, God-Pharaoh") & (Date == "2021-03-19") , "c6ac4a37-8936-5026-a3cd-6bf4b7255835",uuid )) %>%
    
    mutate(Set = ifelse( (Card_Name == "Nicol Bolas, God-Pharaoh") & (grepl("San Diego",Set) & (Date == "2021-05-21") ), "SDCC 2017 Promo Foil",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Nicol Bolas, God-Pharaoh") & (Date == "2021-05-21") , "c6ac4a37-8936-5026-a3cd-6bf4b7255835",uuid )) %>%
    
    
    mutate(Set = ifelse( (Card_Name == "Ulamog, the Ceaseless Hunger") & (grepl("San Diego",Set) & (Date == "2021-04-11") ), "Prerelease Foil",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Ulamog, the Ceaseless Hunger") & (Date == "2021-04-11"), "4fd9c985-3fd8-53a9-9ea8-e366ea81b1ad",uuid )) %>%
    
    mutate(Set = ifelse( (Card_Name == "Chandra, Torch of Defiance") & (grepl("San Diego",Set) & (Date == "2021-06-08") ), "SDCC 2018 Promo Foil",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Chandra, Torch of Defiance") & (Date == "2021-06-08"), "64793505-00a2-5e24-8667-e6c7f58c91af",uuid )) %>%
    
    mutate(Set = ifelse( (Card_Name == "High Tide") & (grepl("San Diego",Set) & (Date == "2021-06-08") ), "IDW Comic Promo",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "High Tide")  & (Date == "2021-06-08"), "0488f4a8-483b-51d5-a607-55ea291b0dd0",uuid )) %>%
    
    mutate(Set = ifelse( (Card_Name == "Wurmcoil Engine") & (grepl("Kaladesh Invent",Set) & (Date == "2021-06-20") ), "Masterpiece Series: Inventions",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Wurmcoil Engine") & (Date == "2021-06-20"), "e243323f-3f57-50af-b8df-c78802094186",uuid )) %>%
    
    mutate(Set = ifelse( (Card_Name == "Sword of Body and Mind") & (grepl("Kaladesh Invent",Set) & (Date == "2021-06-20") ), "Masterpiece Series: Inventions",Set )) %>%
    mutate(uuid = ifelse( (Card_Name == "Sword of Body and Mind") & (Date == "2021-06-20"), "20b8a2d2-e9c1-5366-a31d-f5951ffbcd6c",uuid )) %>%
    
    
    mutate(Set = ifelse( (grepl("Secret Lair",Set) ), "Secret Lair",Set )) %>%
    
    mutate(Semi = paste(Card_Name,Set,hasFoil, sep="")) %>%
    
    left_join(ck_bl %>% mutate(data_edition = ifelse(data_edition == "Promotional",data_variation,data_edition)) %>% 
                  mutate(Semi = paste(data_name,data_edition,data_is_foil,sep="")) %>% 
                  select(Semi,data_id),by=c("Semi"="Semi") ) %>% 
    distinct() %>% 
    
    mutate(ckid = data_id) %>% select(-data_id) %>%
    
    left_join(Updated_Tracking_Keys %>% select(uuid,ckid),by=c("ckid"="ckid")) %>%
    
    mutate(uuid.x = ifelse(is.na(uuid.x),uuid.y,uuid.x)) %>% distinct() %>%
    rename(uuid = uuid.x) %>% select(-uuid.y,-Semi)


resting_inv = resting_inv %>% drop_na() %>% rbind(bl_check) %>% arrange(desc(hold_time))

#final_ledger %>% drop_na() %>% summarize(Inv_Cost = resting_inv %>% mutate(Ovr_Cost = Qty * Cost) %>% summarize(sum(Ovr_Cost)) %>% unlist(), Sold_Cost = sum(Cost), Sold = sum(Sold), Profit = Sold - Sold_Cost, Margin = scales::percent(Profit/(Sold_Cost+ resting_inv %>% mutate(Ovr_Cost = Qty * Cost) %>% summarize(sum(Ovr_Cost)) %>% unlist()) ))

# BAN API Pull ------------------------------------------------------------

inventory_review_tbl = resting_inv %>% 
    left_join(ck_bl %>% select(data_id,number,data_price_buy , data_qty_buying) %>% 
                  rename(BL_Value = data_price_buy, Qty_Des = data_qty_buying), 
              by = c("ckid"="data_id")) %>%
    select(-Key) %>% select(ckid, Card_Name, Set, Rarity, hasFoil, number, everything()) %>%
    mutate(BL_Value = as.numeric(BL_Value)) %>%
    mutate(dollar_change = BL_Value - Cost, percent_change = round(dollar_change / Cost,2),
           credit_change = (BL_Value*1.3) - Cost, credit_perc_change = round(credit_change/Cost,2),
           sell_choice = ifelse(hold_time >= 90, 1,0),
           sell_choice = ifelse(percent_change >= .3,1,sell_choice),
           sell_choice = ifelse(credit_perc_change >= .3,1,sell_choice),
           sell_choice = ifelse( (Qty_Des <= 0) & (sell_choice == 1),0,sell_choice)
           ) %>% distinct() %>%
    mutate(ck_profit = dollar_change * Qty,
           composite_key = paste(Card_Name,number,hasFoil,sep="")) %>%
    select(-contains("change")) %>% filter(grepl("Kozilek, Butcher of Truth",Card_Name)==F & (Date != "2021-01-10")) %>%
    filter(grepl("Oona, Queen of the Fae",Card_Name)==F & (Date != "2021-01-15")) %>%
    filter(grepl("Crucible of Worlds",Card_Name)==F & (Date != "2021-01-29")) %>% 
    filter(grepl("Elvish Archers Near Mint",Card_Name)==F & (Date != "2021-02-02")) %>% 
    filter(grepl("S",Rarity)==F ) %>%
    filter(is.na(number)==F)


con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement = paste("SELECT DISTINCT c.uuid,c.card, c.set edition, c.rarity,c.number, a.hasFoil foil,b.vendor,a.offer,a.Date date 
                  FROM `gaeas-cradle.ban_buylist.",gsub("-","_",Sys.Date()),"_buylist` a 
                  LEFT JOIN `gaeas-cradle.roster.ban_buylist_id` b on b.id = a.vendor 
                  LEFT JOIN `gaeas-cradle.roster.mtgjson_ban` c on c.uuid = a.uuid" ,sep="")
all_bls = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

all_offers = all_bls %>% mutate(foil = ifelse(foil == 0,"","Foil"),
                                card = gsub("\\s\\/\\/.*","",card),
                   composite_key = paste(card,number,foil,sep="")) %>%
    filter( uuid %in% inventory_review_tbl$uuid | composite_key %in% inventory_review_tbl$composite_key) %>%
    #drop_na() %>% 
    group_by(uuid,composite_key,card,foil) %>%
    summarize(best_offer = round(max(offer),2),
              vendor = ifelse(offer == max(offer),vendor,NA)) %>%
    drop_na() %>% ungroup() %>%
    group_by(uuid,composite_key,foil,card,best_offer) %>%
    summarize(vendor = paste(vendor, collapse = " & ")) %>%
    ungroup()


inv_all_review = inventory_review_tbl %>%
    left_join(all_offers,by=c("uuid"="uuid","composite_key"="composite_key")) 

inv_all_review_nas = inv_all_review %>% filter(is.na(vendor)) %>% distinct()


comp_check = inv_all_review_nas[1:16] %>%
    left_join(all_offers,by=c("composite_key"="composite_key"))

comp_check = comp_check %>% filter(is.na(best_offer)==F) %>%
    rename(uuid=uuid.x) %>% select(-uuid.y)

last_check = inv_all_review_nas[1:16] %>%
    left_join(all_offers,by=c("composite_key"="composite_key")) %>% filter(is.na(best_offer)!=F) %>%
    rename(uuid=uuid.x) %>% select(-uuid.y) %>% .[1:17] %>% #mutate(foil.x = foil) %>%
    left_join(all_offers,by=c("uuid"="uuid","hasFoil"="foil")) %>%
    select(-composite_key.x) %>%
    rename(composite_key = composite_key.y) %>% filter(is.na(card)!=T)

inv_all_review = inv_all_review %>% 
    filter(is.na(card)!=T) %>% 
    rbind(comp_check) %>% 
    rbind(last_check) %>% 
    arrange(desc(hold_time)) %>%
    select(-foil,-composite_key,-card, -uuid) %>%
    mutate(over_profit = round((best_offer - Cost) * Qty,2) ) 

inv_all_review = inv_all_review %>%
    filter(grepl("Kozilek, Butcher of Truth",Card_Name)==F & (Date != "2021-01-10")) %>%
    filter(grepl("Oona, Queen of the Fae",Card_Name)==F & (Date != "2021-01-15")) %>%
    filter(grepl("Crucible of Worlds",Card_Name)==F & (Date != "2021-01-29")) %>% 
    filter(grepl("Elvish Archers Near Mint",Card_Name)==F & (Date != "2021-02-02")) %>% 
    filter(grepl("S",Rarity)==F ) %>%
    filter(is.na(number)==F) %>%
    group_by(Card_Name,Set,Rarity,hasFoil,Date) %>%
    summarize(number = min(number),
              Qty = Qty,
              Cost = Cost,
              hold_time = hold_time,
              BL_Value = BL_Value,
              Qty_Des = Qty_Des,
              sell_choice = sell_choice,
              ck_profit = ck_profit,
              best_offer = best_offer,
              vendor = vendor,
              over_profit = over_profit) %>%
    ungroup() %>% view()



gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

#drive_create("TCG_Review")
ss <- drive_get("CS_Model")

sheet_write(
    inv_all_review %>% mutate_if(is.difftime,as.numeric),
    ss = ss,
    sheet = "CS_Model"
)

status_update_tbl = inv_all_review %>% 
    select(-contains("change")) %>% 
    mutate(bl_credit = round(BL_Value * 1.3,2)) %>%
    select(Card_Name,Set,Rarity,hasFoil,number,Qty,Cost,hold_time,BL_Value,bl_credit,everything()) %>%
    select(-ckid)

Total_Quant = status_update_tbl %>% summarize(sell_quant = sum(Qty), 
                                            Total_Cost = sum(Cost * Qty), 
                                            Cash_Profit = (sum(BL_Value * Qty,na.rm=T))-Total_Cost, 
                                            Credit_Profit = (sum(bl_credit * Qty,na.rm=T))-Total_Cost)

Total_Keep_Quant = status_update_tbl %>% filter(sell_choice == 0) %>% summarize(sell_quant = sum(Qty), 
                                                                                Total_Cost = sum(Cost * Qty), 
                                                                                Cash_Profit = (sum(BL_Value * Qty))-Total_Cost, 
                                                                                Credit_Profit = (sum(bl_credit * Qty))-Total_Cost)

Total_Sell_Quant = status_update_tbl %>% filter(sell_choice == 1) %>% summarize(sell_quant = sum(Qty), 
                                                                                Total_Cost = sum(Cost * Qty), 
                                                                                Cash_Profit = (sum(BL_Value * Qty))-Total_Cost, 
                                                                                Credit_Profit = (sum(bl_credit * Qty))-Total_Cost)

recommendation = if(Total_Sell_Quant$Cash_Profit >= 100){"Create and Submit a Buylist for Card Kingdom Based off google sheet below."}else{"No Need for a Buylist for Card Kingdom Based off google sheet below. "}


gm_auth_configure(path = "/home/cujo253/cronR/gmail_api.json")

options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gm_auth(email="pachun95@gmail.com")

test_email <-
    gm_mime() %>%
    gm_to("wolfoftinstreet@gmail.com") %>%
    gm_from("pachun95@gmail.com") %>%
    gm_subject("Daily Inventory Review") %>%
    gm_text_body(
        paste(
            recommendation,
            'For more granular details check the google sheet: https://docs.google.com/spreadsheets/d/13shGD0kXz7785K_-dDshJG8ZZy78j6qwrJ8DL2bC-LM/edit#gid=0'
            
        )
    )

# Verify it looks correct
gm_send_message(test_email)

# If all is good with your dr

