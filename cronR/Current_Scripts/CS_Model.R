pacman::p_load(devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest,gmailr,googledrive,stringr)
'%!in%' <- function(x,y)!('%in%'(x,y))
right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
    substr(text, 1, num_char)
} #Recreating the left function from Excel 

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

# Cardsphere Purchasing Ledger --------------------------------------------
refund_keys = read_csv("/home/cujo253/Essential_Referential_CSVS/cs_ledger.csv") %>% 
    filter(Operation  == "Refund") %>% 
    mutate(refund_operation = paste(`Trade #`,`Card Name`)) %>% select(refund_operation)


cs_ledger = read_csv("/home/cujo253/Essential_Referential_CSVS/cs_ledger.csv") %>% filter(Operation == "Purchase") %>%
    mutate(refund_operation = paste(`Trade #`,`Card Name`)) %>%
    filter(.$refund_operation %!in% refund_keys$refund_operation) %>%
    mutate(Qty = 1, Finish = ifelse(Finish == "Nonfoil","","FOIL"), `Net Amount` = (`Net Amount` * -1) )


cs_ledger = cs_ledger %>% select(Date,`Card Name`,`Set Name`,Finish,`Net Amount`,`Peer User`,Qty) %>% 
    mutate(Date = ymd(gsub("\\s+.*","",Date)), Platform = "CS", Date = as.Date(Date, "%b %d, %Y"))


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
    filter(Date >= "2021-03-01")


ip = "64.225.20.203"
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
    
    i = 1
    ck_sell_order <- NULL
    for(i in 1:length(ck_orders_tbl$`Order ID`)){
        remDr$navigate(paste("https://cardkingdom.com/myaccount/po_invoice/",ck_orders_tbl$`Order ID`[i],sep=""))
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
    #Updated_Tracking_Keys$semi = paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep="")
    ck_sell_order$Scryfall <- Updated_Tracking_Keys$scryfall[match(trimws(ck_sell_order$semi),Updated_Tracking_Keys$semi)]
    return(ck_sell_order)
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
    filter(DOS >= "2021-03-01")



# Fifo --------------------------------------------------------------------

purchases_singles_tbl = CS_Ledger_Cleaned %>%
    mutate(Semi = paste(Card_Name,Set,sep=""),Qty = ifelse(Card_Name == "Ambition's Cost",1,Qty),
           hasFoil = toupper(hasFoil),
           Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,sep=""))) %>%
    #mutate(Rarity = ifelse(is.na(Rarity),rarity,Rarity)) %>%
    mutate(Key = paste(Card_Name,Set,Rarity,hasFoil,sep="")) %>% select(-Semi) %>% filter(is.na(Qty)==F) 


pivot_purch_tbl = purchases_singles_tbl %>% mutate(Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,sep="")),
                                                   Date = round_date(Date,unit="day")) %>% 
    group_by(Key,Date) %>% summarize(Cost = round(sum(Cost)/sum(Qty),2), Buy_Qty = sum(Qty)) %>% ungroup()


multiple_purchases = purchases_singles_tbl %>% mutate(Date = round_date(Date,unit="day")) %>% arrange(Key,Date) %>% filter(Qty > 1)

replacement_lines = NULL
if(nrow(multiple_purchases) != 0){
    for (i in 1:nrow(multiple_purchases)){
        new_lines = NULL
        for (j in 1:(multiple_purchases[i,]$Qty)){
            new_lines = rbind(new_lines,multiple_purchases[i,])
        }
        new_lines$Qty = 1
        replacement_lines = rbind(replacement_lines,new_lines)
    }
}

unnested_purchases = purchases_singles_tbl                        %>% 
    mutate(Date = round_date(Date,unit="day")) %>% 
    arrange(Key,Date)                          %>% 
    filter(Qty <= 1)                           %>% 
    rbind(replacement_lines)                   %>%
    arrange(Date,Key)                          %>%
    filter(Date >= "2020-01-01")


unique_keys = unnested_purchases %>% select(Key) %>% distinct()

regrouped_purchases = NULL
for(i in 1:nrow(unique_keys)){
    data_subbed = unnested_purchases %>% filter(Key == unique_keys$Key[i]) %>% mutate(ct = seq(nrow(.)))
    regrouped_purchases = rbind(regrouped_purchases,data_subbed)
}

#regrouped_purchases %>% filter(Card_Name == "Bitterblossom")

multiple_sales = ck_sold_tbl %>% mutate(DOS = round_date(DOS,unit="day"), Key = paste(name,Set,Rarity," ",hasFoil,sep="")) %>% arrange(Key,DOS) %>% filter(Ind_Qty > 1)

#multiple_purchases %>% view()
replacement_lines = NULL
for (i in 1:nrow(multiple_sales)){
    new_lines = NULL
    for (j in 1:(multiple_sales[i,]$Ind_Qty)){
        new_lines = rbind(new_lines,multiple_sales[i,])
    }
    new_lines$Ind_Qty = 1
    replacement_lines = rbind(replacement_lines,new_lines)
}


unnested_sales = ck_sold_tbl          %>%
    #rbind(custom_sales)                      %>%
    mutate(DOS = round_date(DOS,unit="day")) %>% 
    arrange(Key,DOS)                         %>% 
    filter(Ind_Qty <= 1)                     %>% 
    rbind(replacement_lines)                 %>%
    arrange(DOS,Key)                         %>%
    filter(DOS >= "2020-01-01")              %>%
    mutate(Key = trimws(Key))


unique_keys = unnested_sales %>% select(Key) %>% distinct()


regrouped_sales = NULL
for(i in 1:nrow(unique_keys)){
    data_subbed = unnested_sales %>% filter(Key == unique_keys$Key[i]) %>% mutate(ct = seq(nrow(.)))
    regrouped_sales = rbind(regrouped_sales,data_subbed)
}

final_ledger = regrouped_purchases %>% 
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
    filter(grepl("Boon of Erebos",Card_Name)==F) %>%
    group_by(Card_Name, Set, Rarity, hasFoil) %>%
    summarize(Date = min(Date),Qty = sum(Qty), Cost = round(mean(Cost),2)) %>%
    ungroup() %>%
    mutate(hold_time = Sys.Date()-Date) %>%
    arrange(desc(hold_time))

#final_ledger %>% drop_na() %>% summarize(Inv_Cost = resting_inv %>% mutate(Ovr_Cost = Qty * Cost) %>% summarize(sum(Ovr_Cost)) %>% unlist(), Sold_Cost = sum(Cost), Sold = sum(Sold), Profit = Sold - Sold_Cost, Margin = scales::percent(Profit/(Sold_Cost+ resting_inv %>% mutate(Ovr_Cost = Qty * Cost) %>% summarize(sum(Ovr_Cost)) %>% unlist()) ))

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

inventory_review_tbl = resting_inv %>% mutate(Key = paste0(Card_Name,Set,Rarity,hasFoil)) %>% 
    left_join(CK_Smaller_List %>% select(ckid,CK_Key,number,BL_Value, Qty_Des), by = c("Key"="CK_Key")) %>%
    select(-Key) %>% select(ckid, Card_Name, Set, Rarity, hasFoil, number, everything()) %>%
    mutate(dollar_change = BL_Value - Cost, percent_change = round(dollar_change / Cost,2),
           credit_change = (BL_Value*1.3) - Cost, credit_perc_change = round(credit_change/Cost,2),
           sell_choice = ifelse(hold_time >= 90, 1,0),
           sell_choice = ifelse(percent_change >= .3,1,sell_choice),
           sell_choice = ifelse(credit_perc_change >= .3,1,sell_choice),
           sell_choice = ifelse( (Qty_Des <= 0) & (sell_choice == 1),0,sell_choice)
           )

gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

#drive_create("TCG_Review")
ss <- drive_get("CS_Model")

sheet_write(
    inventory_review_tbl %>% mutate_if(is.difftime,as.numeric),
    ss = ss,
    sheet = "CS_Model"
)

status_update_tbl = inventory_review_tbl %>% 
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
