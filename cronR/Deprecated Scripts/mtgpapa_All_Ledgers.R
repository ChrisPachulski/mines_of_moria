source("config.R")
#Don't Look at the foundation, it's hideous (& pointless)####
pacman::p_load(devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest)

right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
    substr(text, 1, num_char)
} #Recreating the left function from Excel 
moveme <- function (invec, movecommand) {
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
}

options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

# You will only need these comments below if things go tits up.
#
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")


# Cardsphere Purchasing Ledger --------------------------------------------

remDr = remoteDriver(remoteServerAddr = "159.65.219.70", port = 4445L, browser = "chrome")
remDr$open()
remDr$navigate("https://www.cardsphere.com/login")
Sys.sleep(5)

username <- remDr$findElement(using = "id", value = "email")
username$clearElement()
username$sendKeysToElement(list("mtgpapa@yahoo.com"))

passwd <- remDr$findElement(using = "id", value = "password")
passwd$clearElement()
passwd$sendKeysToElement(list("Password"))

Post_Credential_Login <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/div/div/form/button")
Post_Credential_Login$submitElement()
Sys.sleep(2)

remDr$navigate("https://www.cardsphere.com/ledger")
webElem <- remDr$findElement("css","body")
for(i in 1:500){
    webElem$sendKeysToElement(list(key="end"))
}
Page_Contents <- remDr$getPageSource() %>% .[[1]] %>% read_html()
Ledger_Contents <- Page_Contents %>% html_nodes(".cs-row") %>% html_text()
Ledger_Contents <- gsub("\n ","", Ledger_Contents)
Ledger_Contents <- Ledger_Contents[!grepl("MasterCard", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Sale", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Buy-in", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("                                              Back to Top                                                                                                  Contact Us                                                                                    Reddit                                                                                    Facebook                                                                                    Twitter                                                                                    Discord                                                                                    Blog                                                                               Terms & Conditions                   Privacy Policy                   Changelog                                                            Draft & Sealed Simulator                   Explore Cards                   Trade Guide                   Condition Guide                   Tutorials & FAQ                                                             Back to Top                                 ", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Refund", Ledger_Contents)]

Ledger_Contents <- Ledger_Contents[!grepl("Membership", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Cash-out", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Transfer", Ledger_Contents)]

Ledger_Contents <- gsub("  \n","", Ledger_Contents)
The_Ledger <- as.data.frame(Ledger_Contents)

# The_Ledger_Date <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger$Ledger_Contents),'             Purchase',fixed=TRUE)))
# The_Ledger_Date <- The_Ledger_Date[-1,]
# The_Ledger_Date <- The_Ledger_Date$X1
The_Ledger_All_Other_Info <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger$Ledger_Contents),",           ",fixed=TRUE)))
The_Ledger_Card <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger_All_Other_Info$X1),"Purchase ",fixed=TRUE)))

The_Ledger_Buyins <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger_All_Other_Info$X5),"$",fixed=TRUE)))
The_Ledger_Users <- gsub("English ","",The_Ledger_Buyins$X1 )
The_Ledger_Users <- gsub("                           -","",The_Ledger_Users)
The_Ledger_Users <- gsub("                                                                                ","",The_Ledger_Users)
Ledger_Qty <- 1
The_Ledger_Cleaned <- data.frame(The_Ledger_Card$X1,The_Ledger_Card$X2, The_Ledger_All_Other_Info$X2,The_Ledger_All_Other_Info$X3,The_Ledger_All_Other_Info$X4,The_Ledger_Buyins$X2,The_Ledger_Buyins$X3,The_Ledger_Users,Ledger_Qty)
The_Ledger_Cleaned <- The_Ledger_Cleaned[-1,]
colnames(The_Ledger_Cleaned) <- c("Date","Card_Name", "Set","Rarity","F/NF","Cost","Fee","Sender","Qty")
The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] <- lapply(The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] , trimws)

The_Ledger_Cleaned$Date <- gsub("\\s\\d{6}\\s*","",The_Ledger_Cleaned$Date)

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

Updated_Tracking_Keys$Semi <- paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep="")
New_Name <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger_Cleaned$Card_Name),' (',fixed=TRUE)))
The_Ledger_Cleaned$Card_Name <- New_Name$X1
The_Ledger_Cleaned$Semi <- paste(The_Ledger_Cleaned$Card_Name,The_Ledger_Cleaned$Set,sep="")
The_Ledger_Cleaned$Rarity <- Updated_Tracking_Keys$Rarity[match(The_Ledger_Cleaned$Semi,Updated_Tracking_Keys$Semi)]
The_Ledger_Cleaned$Rarity <- ifelse(is.na(The_Ledger_Cleaned$Rarity == T), Updated_Tracking_Keys$Rarity[match(The_Ledger_Cleaned$Card_Name,Updated_Tracking_Keys$name)], The_Ledger_Cleaned$Rarity)
The_Ledger_Cleaned <- The_Ledger_Cleaned[,-ncol(The_Ledger_Cleaned)]
The_Ledger_Cleaned$`F/NF` <- gsub("Nonfoil","",The_Ledger_Cleaned$`F/NF`)
The_Ledger_Cleaned$`F/NF` <- gsub("Foil","FOIL",The_Ledger_Cleaned$`F/NF`)
The_Ledger_Cleaned$Key <- paste(The_Ledger_Cleaned$Card_Name,The_Ledger_Cleaned$Set,The_Ledger_Cleaned$Rarity,sep="")
The_Ledger_Cleaned$Key <- paste(The_Ledger_Cleaned$Key, The_Ledger_Cleaned$`F/NF`, sep=" ")
The_Ledger_Cleaned <- The_Ledger_Cleaned[moveme(names(The_Ledger_Cleaned), "Key first")]
The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] <- lapply(The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] , trimws)
The_Ledger_Cleaned$Date <- ifelse(nchar(The_Ledger_Cleaned$Date) < 7, paste(The_Ledger_Cleaned$Date,", 2020",sep = ""), The_Ledger_Cleaned$Date)
The_Ledger_Cleaned$Cost <- as.numeric(The_Ledger_Cleaned$Cost)
#The_Ledger_Cleaned$Date <- as.Date(The_Ledger_Cleaned$Date, format = "%b %d,%Y")
The_Ledger_Cleaned$Qty <- as.numeric(The_Ledger_Cleaned$Qty)
The_Ledger_Cleaned$Platform <- "CS"


CS_Ledger_Cleaned = The_Ledger_Cleaned %>% filter(grepl("Back",Date) == F) %>%
                    mutate(Date = as.Date(Date, "%b %d, %Y")) %>% select(-Fee) %>%
                    rename("hasFoil" = `F/NF`)



# TCG Purchasing Ledger ---------------------------------------------------
n = 4
tcg_purchases <- function(ip, n = 4){
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
        Password$sendKeysToElement(list(""))
        Sys.sleep(.5)
        Password$sendKeysToElement(list(""))
        Sys.sleep(.5)
        Password$sendKeysToElement(list(""))
        Sys.sleep(.5)
        Password$sendKeysToElement(list(""))
        Sys.sleep(.5)
        Password$sendKeysToElement(list(""))
        Sys.sleep(.5)
        
        remDr$findElement('xpath','//*[@id="signInForm"]/label/span[1]')$clickElement()
        
        Sys.sleep(3)
        remDr$findElement("xpath",'//*[@id="signInForm"]/button')$clickElement()  
    
    Sys.sleep(3)
    remDr$navigate("https://store.tcgplayer.com/myaccount/orderhistory")
    
    
    remDr$findElement("id","DateRange")$clickElement()
    Sys.sleep(1)
    remDr$findElement("xpath",paste('//*[@id="DateRange"]/option[',n,']',sep=""))$clickElement()
    #option[0] = 30 days
    #option[1] = 90 days
    #options[2] = 120 days
    #options[3] = All Time
    Sys.sleep(10)
    order_count <- round(as.numeric(gsub("\\s.*","",gsub(".*of\\s","",gsub("\n.*","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))),-2)
    
    All_Pages <- NULL
    for(i in 1:order_count){
        Order_Content_Raw <- data.frame(content = trimws(gsub("Sold by.*","",gsub("ITEMS","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(".orderHistoryItems") %>% html_text())))))
        Order_Content_Raw <- data.frame(do.call('rbind', strsplit(as.character(Order_Content_Raw$content),'\n\n',fixed=TRUE)))
        colnames(Order_Content_Raw) <- c("name","set")
        Order_Content_Raw$abbrev <- Updated_Tracking_Keys$abbr[match(Order_Content_Raw$set,Updated_Tracking_Keys$Set)]
        
        Sys.sleep(1)
        
        Order_Details_Raw <- data.frame(very_raw = trimws(gsub(" - ",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(".orderHistoryDetail") %>% html_text()))))
        Order_Details_Raw <- data.frame(do.call('rbind', strsplit(as.character(Order_Details_Raw$very_raw),'\n',fixed=TRUE)))
        Order_Details_Raw <- suppressWarnings(Order_Details_Raw %>% separate(X1,c("R","rarity"),sep=": ") %>% separate(X2,c("Con","condition","language"),sep=":"))
        Order_Details_Raw$language[is.na(Order_Details_Raw$language)] <- "English"
        Order_Details_Raw$hasFoil <- ifelse(grepl("foil",tolower(Order_Details_Raw$condition)) == F,"","Foil")
        Sys.sleep(1)
        Order_Details <- Order_Details_Raw[c(2,6,4,5)]
        
        
        Order_Content <- data.frame(Order_Content_Raw,Order_Details)
        Order_Content$Key <- paste(Order_Content$name,Order_Content$set,Order_Content$rarity," ",Order_Content$hasFoil,sep="")
        Order_Content <- Order_Content[c(8,1,2,3,4,5,6,7)]
        
        Sys.sleep(1)
        Order_Purchase_Info <- NULL
        
        
        order_amount <- as.numeric(gsub(" order\\(.*","",gsub(".* of ","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))
        if(order_amount < 10){order_amount <- order_amount}else{order_amount <- 10}  
        for(j in 1:order_amount){
          tryCatch({
            Check <- str_count(trimws(gsub("\n\\s\\s+",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table') ) %>% html_text()))),"\\$")
            
            Summary <- data.frame( summary = gsub("\\(.*\\)","",gsub("\\$","",gsub(" ","",gsub("\n","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/span[1]/table/tbody/tr',sep="")) %>% html_text()))))))
            Summary_Content_Raw <- t(data.frame(do.call('rbind', strsplit(as.character(Summary$summary),':',fixed=TRUE))))
            colnames(Summary_Content_Raw) <- Summary_Content_Raw[1,]
            Summary_Content_Raw <- Summary_Content_Raw[-1,]
            
            if(Check == 1){Summary_Content_Raw <- as.data.frame(t(Summary_Content_Raw))}else{
                supplamental_pricing <- data.frame(very_raw = trimws(gsub("\\,","",gsub(":","",gsub("\n","",gsub("[A-Za-z]*","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table/tbody/tr',sep="")) %>% html_text()))))))
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
        }, error = function(e){print("Loop Error")})
        }
        rownames(Order_Purchase_Info) <- seq(nrow(Order_Purchase_Info))
        Order_Purchase_Info <- mutate_all(as.data.frame(Order_Purchase_Info),function(x) as.numeric(as.character(x)))
        
        Sys.sleep(1)
        
        Compiled_Dates <- NULL
        Compiled_order_Ids <- NULL
        for(j in 1:order_amount){
          tryCatch({
            Check <- str_count(trimws(gsub("\n\\s\\s+",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table') ) %>% html_text()))),"\\$")
            Dates_Raw <- trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[1]/span[1]/span[2]',sep="")) %>% html_text())
            Order_id_Raw <- trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[1]/span[3]/text()',sep="")) %>% html_text())[2]
            
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
        
        All_Pages <- rbind(All_Pages,Page_Contents)
        remDr$findElement("class","nextPage")$clickElement()
        Sys.sleep(1)
        
    }
    
    All_Pages = All_Pages %>% left_join(Sets %>% select(mtgjson,TCG_Key,GF_Abbr), by = c("set"="TCG_Key")) %>% 
        select(-set,-abbrev) %>% mutate(set = mtgjson, abbrev = GF_Abbr) %>% select(-mtgjson, -GF_Abbr) %>%
        mutate(name = gsub("\\s\\(.*\\)","",name), Key = paste(name,set,rarity," ",hasFoil,sep="")) %>% select(Key, name, set, abbrev, everything()) %>%
        drop_na()
    
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
    return(TCG_Full_History)
}
TCG_Full_History <- tcg_purchases("64.225.20.203",n=4)

TCG_Full_History = TCG_Full_History %>% mutate(Purchase_Dates = as.Date(Purchase_Dates, "%m/%d/%Y"))

TCG_Ledger_Cleaned = data.frame(
                     Key       = TCG_Full_History$Key,
                     Date      = TCG_Full_History$Purchase_Dates,
                     Card_Name = TCG_Full_History$name,
                     Set       = TCG_Full_History$set,
                     Rarity    = TCG_Full_History$rarity,
                     hasFoil   = TCG_Full_History$hasFoil,
                     Cost      = round(TCG_Full_History$Total/TCG_Full_History$Quantity,2),
                     Sender    = TCG_Full_History$TCG_Order_Num,
                     Qty       = TCG_Full_History$Quantity,
                     Platform  = TCG_Full_History$Platform)

Combined_Ledger = rbind(CS_Ledger_Cleaned,TCG_Ledger_Cleaned) %>% arrange(desc(Date))


# CK Purchases ------------------------------------------------------------

ck_purchases <- function(ip){
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
    #raw_invoices <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr('href')
    #invoice_numbers <- gsub('/myaccount/invoice/','',raw_invoices[grepl('/myaccount/invoice/',raw_invoices)])
    status <- trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="appWrapper"]/div[2]/div[3]/table/tbody/tr/td') %>% html_text())
    status <- gsub("\n.*","",status[!grepl('COMPLETED|CANCELED',status )])
    
    invoice_numbers <- status[grepl('^\\d+$',status)]
    purchase_dates_ck <- as.Date(mdy(gsub("\\s+\\d+\\:\\d+.*","",status[grepl('^[A-Za-z]{3}\\s',status)])))
    #last_purchase_date <- r_inv %>% select(`Purchased Via`,DOP) %>% mutate(CK_Check = (grepl(".*CK.*",`Purchased Via`))) %>% filter(CK_Check == T) %>% select(DOP) %>%arrange(desc(DOP)) %>% slice(1)
    
    
    ck_orders <- data.frame(invoice = invoice_numbers, status = "SHIPPED", dates = purchase_dates_ck) #%>% filter(dates >= last_purchase_date$DOP)
    i = 20
    ck_order <- NULL
    for(i in 1:length(ck_orders$invoice)){
        remDr$navigate(paste("https://cardkingdom.com/myaccount/invoice/",ck_orders$invoice[i],sep=""))
        order_contents <- data.frame(raw = trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('tr') %>% html_text()))
        order_contents <- order_contents$raw[!grepl('Description Style Qty Price Total',order_contents$raw)]
        order_contents <- data.frame(raw = order_contents[!grepl('SINGLES|PRODUCT|Subtotal|Shipping|Sales Tax|Tax',order_contents)])
        final_amt <- as.numeric(gsub("Total USD \\$","",order_contents[nrow(order_contents),]))
        order_contents <- order_contents[-nrow(order_contents),]
        products <- gsub(":.*","",order_contents)
        products <- gsub(" \\(.*","",products)
        sets <- trimws(gsub("\\d+\\s\\$.*","",gsub(" Foil","",gsub("\\s[A-Z]{2}\\s.*","",gsub(".*:","",order_contents)))))
        
        condition = right(gsub("\\s\\d+\\s\\$.*","",gsub("\\s\\d{4}\\s"," ",order_contents)),2)
        
        # condition <- suppressWarnings(if(grepl("\\s\\d{4}\\s",order_contents)==F){right(gsub("[a-z]+:*\\s","",gsub("\\s\\d+.*","",order_contents)),2)}else{
        #     left(gsub(".*\\d{4}\\s","",gsub("Foil ","",order_contents)),2)
        # })
        
        language <- "English"
        qty <- str_match(gsub("\\s\\$.*","",order_contents),"\\d+$") 
        ind_amt <- as.numeric(gsub(".*\\$","",order_contents)) %>% as.data.frame() %>% replace(is.na(.),0)
        order_comp <- round(ind_amt/final_amt,2)
        add_fees <- final_amt-sum(ind_amt)
        order_comp <- round(order_comp * add_fees,2)
        ind_amt <- round(ind_amt + order_comp,2)
        DOP <- suppressWarnings(mdy(gsub("\\s\\d+\\:.*","",gsub(".*(S|s)hipped on ","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('xpath' = '//*[@id="appWrapper"]/div[2]/div[2]/div/div[4]') %>% html_text())))))
        raw_order <- data.frame(order_id = ck_orders$invoice[i],name = products,sets = sets, condition = condition, language = language,qty = qty, price = ind_amt, DOP = DOP,Purchased_Via = "CardKindom")
        
        raw_order$hasFoil = as.integer(grepl(" Foil",order_contents))
        raw_order$hasFoil = ifelse(as.numeric(raw_order$hasFoil) == 1, "FOIL","")
        raw_order$sets <- gsub(" Foil","",raw_order$sets)
        raw_order$sets <- gsub(" Draft Booster Box","",gsub(" Bundle","",ifelse(grepl(" Bundle| Draft Booster Box",raw_order$sets)==T,paste(raw_order$name,": ",raw_order$sets,sep=""),raw_order$sets)))
        
        raw_order = raw_order %>% left_join(Sets %>% select(CK_BL_Scrape_Sets,mtgjson), by = c("sets"="CK_BL_Scrape_Sets")) %>% filter(is.na(qty) == F) %>%
            select(-sets) %>% mutate(sets = mtgjson) %>% select(-mtgjson) %>% select(order_id, name,sets,everything())
        
        raw_order$language <- ifelse(grepl("JPN",raw_order$name)==T,"Japanese","English")
        raw_order$sets <- gsub(" JPN.*","",raw_order$sets)
        raw_order$abbrev <- Updated_Tracking_Keys$abbr[match(raw_order$sets,Updated_Tracking_Keys$Set)]
        raw_order$rarity <- Updated_Tracking_Keys$Rarity[match(paste(raw_order$name,raw_order$sets,sep=""),paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep=""))]
        raw_order$rarity[is.na(raw_order$rarity)] <- "S"
        raw_order$Key <- trimws(paste(raw_order$name,raw_order$sets,raw_order$rarity," ",raw_order$hasFoil,sep=""))
        order <- raw_order[c(1,13,2,3,11,12,10,4,5,6,7,8,9)] %>% filter(. != 0.00)
        colnames(order)[11] <- "price"
        ck_order <- rbind(ck_order,order)
        ck_order <- ck_order[!grepl("^S$",ck_order$Key),]
    }
    
    ck_order <- ck_order %>% filter(is.na(DOP)==F) %>% select(Key,name,sets,abbrev,rarity,hasFoil,condition,language,qty,price,DOP,order_id,Purchased_Via)
    
    return(ck_order)
}

ck_ledger = ck_purchases("64.225.20.203") %>% mutate(qty = as.numeric(qty)) %>% drop_na() %>% distinct()


ck_ledger_cleaned  = data.frame(
  Key       = ck_ledger$Key,
  Date      = ck_ledger$DOP,
  Card_Name = ck_ledger$name,
  Set       = ck_ledger$sets,
  Rarity    = ck_ledger$rarity,
  hasFoil   = ck_ledger$hasFoil,
  Cost      = round(ck_ledger$price/ck_ledger$qty,2),
  Sender    = ck_ledger$order_id,
  Qty       = ck_ledger$qty,
  Platform  = "CK")


Combined_Ledger = rbind(Combined_Ledger,ck_ledger_cleaned) %>% arrange(desc(Date))


# TCG Sales  --------------------------------------------------------------

tcg_login_sales <-function(){
  remDr$navigate("https://tcgplayer.com")
  Sys.sleep(sample(5:9, 1))
  remDr$findElement("xpath",'//*[@id="app"]/div/header/div/div[3]/div[1]/div[2]/div[1]')$clickElement()
  Sys.sleep(sample(5:9, 1))
  remDr$findElement("css",'.account-actions-menu__title a')$clickElement()
  username <- remDr$findElement("id","Email")
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
  Password <- remDr$findElement("id","Password")
  Sys.sleep(sample(1:3, 1))
  Password$clickElement()
  Sys.sleep(sample(1:3, 1))
  Password$sendKeysToElement(list(""))
  Sys.sleep(.5)
  Password$sendKeysToElement(list(""))
  Sys.sleep(.5)
  Password$sendKeysToElement(list(""))
  Sys.sleep(.5)
  Password$sendKeysToElement(list(""))
  Sys.sleep(.5)
  Password$sendKeysToElement(list(""))
  Sys.sleep(.5)
  
  
  Sys.sleep(3)
  remDr$findElement("id",'loginButton')$clickElement()
  
  remDr$navigate("https://store.tcgplayer.com/admin/orders/orderlist")
  
  Sys.sleep(3)
  Latest_Dates <- format(Sold_Ledger$DOS + 1,"%m/%d/%Y")
  Latest_Date <- Latest_Dates[length(Latest_Dates)]
  Date_Input <- remDr$findElement("xpath", '//*[@id="rightSide"]/div/div[4]/div/div[2]/div[1]/div[1]/div[2]/div[1]/div[3]/div/div/div[1]/div[1]/div/div[1]/div/input')
  Date_Input$clickElement()
  Date_Input$sendKeysToElement(list(Latest_Date))
  Sys.sleep(.5)
  currentDate <- format((Sys.Date()),"%m/%d/%Y")
  Date_Endpoint <- remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/div[2]/div[1]/div[1]/div[2]/div[1]/div[3]/div/div/div[1]/div[2]/div/div[1]/div/input')
  Date_Endpoint$clickElement()
  Date_Endpoint$sendKeysToElement(list(currentDate))
  Sys.sleep(2)
  remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/div[2]/div[1]/div[1]/div[2]/div[4]/div[2]/div/div[2]/button')$clickElement()
  Sys.sleep(.5)
  remDr$findElement("xpath",'//*[@id="table-page-counts"]/span[2]/select/option[5]')$clickElement()
  Sys.sleep(2)
  remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/span/div/div[3]/div/div[2]/table/thead/tr/th[4]')$clickElement()
  Sys.sleep(2)
  #remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/span/div/div[3]/div/div[2]/table/thead/tr/th[4]')$clickElement()
  
  #remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/span/div/div[3]/div/div[2]/table/thead/tr/th[4]')$clickElement()
  Orders <- NULL
  Orders <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href")
  Orders <- data.frame(Orders[grepl("manageorder",Orders)])
  colnames(Orders) <- "Order_ID"
  Orders <- Orders %>% separate(Order_ID,c("A","B","C","D","Trans_ID"),"/")
  Orders <- as.data.frame(Orders[5])
  Order_Links <- Orders
  Order_Links$Links <- paste("https://store.tcgplayer.com/admin/orders/manageorder/",Order_Links$Trans_ID,sep="")
  
  Order_Links <- unique(Order_Links)
  Recent_Sales <- NULL
  for(i in 1:nrow(Order_Links)){
    remDr$navigate(Order_Links$Links[i])
    if(length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()) == 0){
      Test_Number = length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
    }else(
      Test_Number = length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
    )
    if (Test_Number == 1){
      Card_Info <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href")
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
      Param$Foil <- Updated_Tracking_Keys$Foil[match(Param$param,Updated_Tracking_Keys$param)]
      Param$Foil[is.na(Param$Foil)]<-""
      Character_Check <- character(0)
      Param$Ind_Qty <-  if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text(),Character_Check) == T){
        remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
      }else{
        remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
      }
      
      Param$Order_Qty <- Param$Ind_Qty
      
      Param$Order_Amt <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check)){gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text())}else{gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text())}
      
      Param$Base_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text(),Character_Check)){gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text()))}else{gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text()))}
      
      Test_For_Direct <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}else{remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}
      
      if(any(grepl("Direct",Test_For_Direct))==T){
        Param$Direct_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}else{as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}
      } else{
        Param$Direct_Fee <- 0.00025
      }
      
      suppressWarnings(if(Param$Direct_Fee == 0.00025){
        Param$Net_Rev <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}
      } else {
        Param$Net_Rev <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}
      })
      Param$Ind_Amt <- Param$Order_Amt
      
      Param$DOS <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text(),Character_Check)){remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}else{remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}
      
      Param$Ind_Rev <- Param$Net_Rev
      
      Recent_Sales <- rbind(Recent_Sales,Param)
      Sys.sleep(3)
    }else{
      #length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
      Card_Info <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href")
      Card_Info <- data.frame(Card_Info = Card_Info[grepl("productSearch",Card_Info)])
      Card_Info <- data.frame(do.call('rbind', strsplit(as.character(Card_Info$Card_Info),'/productSearch/',fixed=TRUE)))
      Param <-data.frame(param = Card_Info$X2)
      Param$Trans_ID <- Order_Links$Trans_ID[i]
      Param$Key <- Updated_Tracking_Keys$Key[match(Param$param,Updated_Tracking_Keys$param)]
      Param$name <- Updated_Tracking_Keys$name[match(Param$param,Updated_Tracking_Keys$param)]
      Param$Set <- Updated_Tracking_Keys$Set[match(Param$param,Updated_Tracking_Keys$param)]
      Param$abbr <- Updated_Tracking_Keys$abbr[match(Param$param,Updated_Tracking_Keys$param)]
      Param$Rarity <- Updated_Tracking_Keys$Rarity[match(Param$param,Updated_Tracking_Keys$param)]
      Param$Foil <- Updated_Tracking_Keys$Foil[match(Param$param,Updated_Tracking_Keys$param)]
      Param$Foil[is.na(Param$Foil)]<-""
      Character_Check <- character(0)
      Param$Ind_Qty <-  if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text(),Character_Check) == T){
        remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
      }else{
        remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
      }
      Param$Order_Qty <- sum(as.numeric(Param$Ind_Qty))
      
      Param$Order_Amt <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check) == T){
        as.numeric(gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
      }else{
        as.numeric(gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
      }
      
      Param$Order_Amt <-  if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check) == T){
        as.numeric(gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]/td[2]/b') %>% html_text()))
      }else{
        as.numeric(gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
      }
      
      Param$Base_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text(),Character_Check) == T){
        as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
      }else{
        as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
      }
      
      
      
      Test_For_Direct <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}else{remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}
      
      if(any(grepl("Direct",Test_For_Direct))==T){
        Param$Direct_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}else{as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}
      } else{
        Param$Direct_Fee <- 0.00025
      }
      
      value = NULL
      suppressWarnings(if(Param$Direct_Fee == 0.00025){
        value = if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}
      }) 
      if(identical(value,numeric(0))) {
        value = if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}
      }
      
      new_value = if(is.null(value)){as.numeric(gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]/td[2]') %>% html_text()))}else{value}
      
      new_value = if(identical(new_value,numeric(0))){as.numeric(gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]/td[2]') %>% html_text()))}else{new_value}
      
      Param$Net_Rev = new_value
      
      Param$Ind_Amt <- as.numeric(gsub(" .*","",gsub(".*  \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.gradeA') %>% html_text()))%>% as.data.frame() %>% filter(!grepl(".*\\/.*",.)) %>% unlist())* as.numeric(Param$Ind_Qty)
      Param$DOS <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text(),Character_Check)){
        remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()
      }else{
        remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()
      }
      
      Param$Base_Fee <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Base_Fee,2)
      Param$Direct_Fee <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Direct_Fee,2)
      Param$Ind_Rev <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Net_Rev,2)
      Recent_Sales <- rbind(Recent_Sales,Param)
    }
  }
  Safety_Sales <- Recent_Sales %>% distinct()
  Recent_Sales <- Recent_Sales %>% distinct()
  #Recent_Sales <- Safety_Sales
  NA_Saviours <- Recent_Sales[which(is.na(Recent_Sales$name)==T ),]
  Replacements <- NA_Saviours
  Saviours <- NULL
  for (i in 1:nrow(NA_Saviours)){
    Saviour_Links <- paste("https://store.tcgplayer.com/productCatalog/product/productSearch/",NA_Saviours$param[i],sep="")
    Sys.sleep(3)
    remDr$navigate(Saviour_Links)
    Replacements$name[i] <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()
    Sys.sleep(3)
    Replacements$Set[i] <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()
    Sys.sleep(3)
    Replacements$abbr[i] <- Updated_Tracking_Keys$abbr[match(Replacements$Set,Updated_Tracking_Keys$Set)]
    Replacements$Rarity[i] <- gsub("\\,.*","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text())
    Sys.sleep(3)
    Replacements$Key[i] <- paste(Replacements$name[i],Replacements$Set[i],Replacements$Rarity[i],sep="")
    
  }
  Recent_Sales <- rbind(Recent_Sales,Replacements)
  
  
  Recent_Sales <- na.omit(Recent_Sales)
  
  
  Recent_Sales$Direct_Fee <- round(Recent_Sales$Direct_Fee,2)
  #devtools::install_github("eddelbuettel/anytime",force = TRUE)
  Recent_Sales$DOS <- anydate(Recent_Sales$DOS)
  Recent_Sales$abbr<- ifelse(Recent_Sales$Foil=="FOIL",paste(Recent_Sales$abbr,"_F",sep=""), Recent_Sales$abbr)
  Recent_Sales$Key <- gsub(" Promos","",Recent_Sales$Key)
  Recent_Sales$DOP <- anytime(r_inv$DOP)[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
  Sets_for_NA_DOP <- as.data.frame(read_html("https://mtg.gamepedia.com/Set") %>% html_nodes(xpath ='//*[@id="mw-content-text"]/div/table[4]') %>% html_table() )
  Sets_for_NA_DOP <- Sets_for_NA_DOP[c(1:2)]
  Sets_for_NA_DOP$Released <- as.Date(anydate(paste(Sets_for_NA_DOP$Released,"-01",sep="")))
  Recent_Sales$DOP <- ifelse(is.na(Recent_Sales$DOP)==T, as.character(Sets_for_NA_DOP$Released)[match(Recent_Sales$Set,Sets_for_NA_DOP$Set)],as.character(Recent_Sales$DOP))
  Recent_Sales$DOP <- format(anydate(Recent_Sales$DOP),"%Y-%m-%d")
  Recent_Sales$DOP[is.na(Recent_Sales$DOP)] <- format(anydate("1900-01-01"),"%Y-%m-%d")
  Recent_Sales$COGS <- r_inv$`Actual COG`[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
  Recent_Sales$COGS[is.na(Recent_Sales$COGS)] <- 0
  Recent_Sales$Platform_Buy <- r_inv$`Purchased Via`[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
  Recent_Sales$Platform_Buy[is.na(Recent_Sales$Platform_Buy)] <- "Bulk/Sealed"
  Recent_Sales$Scryfall <- Updated_Tracking_Keys$scryfall[match(Recent_Sales$param,Updated_Tracking_Keys$param)]
  
  return(Recent_Sales)
}




