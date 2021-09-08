#Don't Look at the foundation, it's hideous (& pointless)####
pacman::p_load(devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest)
'%!in%' <- function(x,y)!('%in%'(x,y))
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
  select(Key,Date,Card_Name,Set,Rarity,hasFoil,Cost,Sender,Qty,Platform)

Updated_Tracking_Keys %>% select(Set) %>% distinct() %>% filter(grepl("Judge",Set)) %>% view()
Sets %>% select(CK_BL_Scrape_Sets) %>% filter(grepl("Ultimate",CK_BL_Scrape_Sets))

cs_sales = read_csv("/home/cujo253/Essential_Referential_CSVS/cs_ledger.csv")                                    %>%
  mutate(`Platform Sold` = "CS",Finish = ifelse(Finish == "Nonfoil","","FOIL"), `Net Amount` = (`Net Amount` * -1) )%>% 
  filter(Operation  == "Sale")                                                                        %>% 
  select(Date,`Card Name`,`Set Name`,Finish,`Net Amount`,`Peer User`,`Platform Sold`,`Trade #`)                   %>%
  `colnames<-` (c("Date","Card_Name", "Set","hasFoil","Ind_Amt","Sender","Platform Sold","sell_id"))              %>%
  mutate(Date = ymd(gsub("\\s+.*","",Date)), Platform = "CS", Date = as.Date(Date, "%b %d, %Y"))      %>%
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
         Set = gsub("Buy-a-Box Promos","Promo Pack",Set),
         Set = gsub("Ultimate Masters: Box Toppers","Ultimate Box Topper",Set),
  )%>%
  left_join(Sets %>% select(CK_BL_Scrape_Sets,mtgjson) %>% distinct(),by=c("Set"="CK_BL_Scrape_Sets")) %>%
  mutate(Set = mtgjson, 
         Set = ifelse( (Card_Name == "Sliver Legion") & (is.na(Set)) ,"Judge Gift Cards 2019",as.character(Set)),
         Semi = paste(Card_Name,Set,sep="" )) %>% select(-mtgjson) %>% 
  left_join(Updated_Tracking_Keys %>% select(param,Semi,Rarity) %>% distinct() ,by=c("Semi"="Semi")) %>% 
  mutate(Key = paste(Card_Name,Set,Rarity," ",hasFoil,sep=""),
         Ind_Qty = 1,
         Ind_Amt = Ind_Amt * -1) %>% 
  select(param,Key,Card_Name,Set,Rarity,hasFoil,Ind_Qty, Ind_Amt,Date,sell_id,`Platform Sold`) %>%
  rename(name = Card_Name,DOS = Date)


# TCG Purchasing Ledger ---------------------------------------------------
n = 2
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
    remDr$findElement("xpath",paste('//*[@id="DateRange"]/option[',n,']',sep=""))$clickElement()
    #option[0] = 30 days
    #option[1] = 90 days
    #options[2] = 120 days
    #options[3] = All Time
    Sys.sleep(10)
    order_count <- round(as.numeric(gsub("\\s.*","",gsub(".*of\\s","",gsub("\n.*","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))),-1) - 10
    order_count = if(order_count < 0){order_count = 0}else{order_count = order_count}
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
        }, error = function(e){break})
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
        
        Page_Contents <- Page_Contents %>% mutate(set = ifelse(grepl("Extended Art",name),paste(set,"Variants"),set))
        
        All_Pages <- rbind(All_Pages,Page_Contents)
        remDr$findElement("class","nextPage")$clickElement()
        Sys.sleep(1)
        
    }
    
    All_Pages = All_Pages %>% left_join(Sets %>% select(mtgjson,TCG_Key,GF_Abbr), by = c("set"="TCG_Key")) %>% distinct() %>%
        mutate(mtgjson = ifelse(is.na(mtgjson),gsub(" Variants","",set),as.character(mtgjson) )) %>%
        select(-set,-abbrev) %>% mutate(set = mtgjson, abbrev = GF_Abbr) %>% select(-mtgjson, -GF_Abbr) %>%
        mutate(name = gsub("\\s\\(.*\\)","",name), Key = paste(name,set,rarity," ",hasFoil,sep="")) %>% select(Key, name, set, abbrev, everything())
    
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
TCG_Full_History <- tcg_purchases("64.225.20.203",n=1)

TCG_Full_History = TCG_Full_History %>% mutate(Purchase_Dates = as.Date(Purchase_Dates, "%m/%d/%Y")) %>% distinct()

TCG_Full_History %>% mutate(new_sub = round(Total/Quantity,2)) %>% select(name,set,rarity,Total,Quantity,new_sub)

Adjusted_Order_Quantity = TCG_Full_History %>% group_by(TCG_Order_Num) %>% summarize(Order_Quantity = sum(Quantity))

TCG_Full_History = TCG_Full_History %>% left_join(Adjusted_Order_Quantity,by=c("TCG_Order_Num"="TCG_Order_Num"))

TCG_Ledger_Cleaned = data.frame(
                     Key       = TCG_Full_History$Key,
                     Date      = mdy(TCG_Full_History$Purchase_Dates),
                     Card_Name = TCG_Full_History$name,
                     Set       = TCG_Full_History$set,
                     Rarity    = TCG_Full_History$rarity,
                     hasFoil   = TCG_Full_History$hasFoil,
                     Cost      = round(TCG_Full_History$Total/TCG_Full_History$Order_Quantity,2),
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
    
    ck_order <- NULL
    i = 4
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
        
        condition = right(gsub("\\(500ct\\)","",gsub("\\s\\d+\\s\\$.*","",gsub("\\s\\d{4}\\s"," ",order_contents))),2)
        
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
    
    return(ck_order)
}

ck_ledger = ck_purchases("64.225.20.203") %>% mutate(qty = as.numeric(qty)) %>% distinct() %>% mutate(sets = gsub("^Lair of Behemoths","Ikoria: Lair of Behemoths",sets))

ck_ledger = ck_ledger %>% mutate(name = gsub(" Booster Box","",ifelse(grepl("\\$",name),sets,name)),
                     sets = ifelse(grepl("\\(",sets),gsub("\\)","",gsub(".*\\(","",sets)),sets),
                     Key = paste(name,sets,rarity,hasFoil,sep=""))

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
ip = "64.225.20.203"
tcg_login_sales <-function(ip = "64.225.20.203"){
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
  
  remDr$navigate("https://store.tcgplayer.com/admin/payment/sellerpayment")
  Sys.sleep(2)
  remDr$findElement("xpath",'//*[@id="PageSize"]/option[6]')$clickElement()
  Sys.sleep(5)
  Payment_Links <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href") %>% as_tibble() %>% filter(grepl("admin\\/payment\\/SellerPaymentOrders",value)) %>%
    mutate(value = paste("https://store.tcgplayer.com/",value,sep=""))
  
  Order_Links = NULL
  sales_order = NULL
  refund_orders = NULL
  for(i in 1:nrow(Payment_Links)){
    remDr$navigate(Payment_Links$value[i])
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
  
  
  sales_order %>% mutate(`Total Sale (+)` = gsub("\\$","",`Total Sale (+)`)) %>% group_by(Buyer) %>% summarize(sales = sum(as.numeric(`Total Sale (+)`)),
                                                orders = n()) %>% arrange(desc(orders))

  
  
  Recent_Sales <- NULL
  i = 266
  for(i in 1:nrow(Order_Links)){
    remDr$navigate(Order_Links$value[i])
    if(length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()) == 0){
      Test_Number = length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
    }else(
      Test_Number = length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
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
      
      Param$sell_id= gsub(".*\\/","",Order_Links$value[i])
      
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
      
      Param$sell_id= gsub(".*\\/","",Order_Links$value[i])
      
      Recent_Sales <- rbind(Recent_Sales,Param)
    }
  }
  Safety_Sales <- Recent_Sales %>% distinct()
  Recent_Sales <- Recent_Sales %>% distinct()
  #Recent_Sales <- Safety_Sales
  NA_Saviours <- Recent_Sales[which(is.na(Recent_Sales$name)==T ),]
  Replacements <- NA_Saviours
  Saviours <- NULL
  i = 26
  for (i in 1:nrow(NA_Saviours)){
    Saviour_Links <- paste("https://store.tcgplayer.com/productCatalog/product/productSearch/",NA_Saviours$param[i],sep="")
    
    remDr$navigate(Saviour_Links)
    Sys.sleep(1)
    page_source = remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    Replacements$name[i] <- if(identical(page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text(),Character_Check)){
      page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()
      } else {page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()}
    Sys.sleep(1)
    Replacements$Set[i] <- if(identical(page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text(),Character_Check)){
      page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()
    } else {page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()}
    Sys.sleep(1)
    Replacements$abbr[i] <- if(is.na(Updated_Tracking_Keys$abbr[match(Replacements$Set[i],Updated_Tracking_Keys$Set)])){""}else{Updated_Tracking_Keys$abbr[match(Replacements$Set[i],Updated_Tracking_Keys$Set)]}
    Replacements$Rarity[i] <- if(identical(gsub("\\,.*","",page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text()), Character_Check)){
      page_source %>% html_nodes(xpath = '/html/body/div[5]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text()
    } else {
      gsub("\\,.*","",page_source %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text())
    }
    Sys.sleep(3)
    Replacements$Key[i] <- paste(Replacements$name[i],Replacements$Set[i],Replacements$Rarity[i],sep="")
    
  }
  Recent_Sales <- rbind(Recent_Sales,Replacements)
  
  #Recent_Sales <- Recent_Sales %>% filter(grepl("node = ",Key)==F)
  
  Recent_Sales <- na.omit(Recent_Sales)
  
  
  Recent_Sales$Direct_Fee <- round(Recent_Sales$Direct_Fee,2)
  #devtools::install_github("eddelbuettel/anytime",force = TRUE)
  Recent_Sales$DOS <- anydate(Recent_Sales$DOS)
  Recent_Sales$abbr<- ifelse(Recent_Sales$Foil=="FOIL",paste(Recent_Sales$abbr,"_F",sep=""), Recent_Sales$abbr)
  Recent_Sales$Key <- gsub(" Promos","",Recent_Sales$Key)
  #Recent_Sales$DOP <- anytime(r_inv$DOP)[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
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

tcg_sales = Recent_Sales
tcg_refunds = refund_orders

#tcg_refunds %>% view()

# CK Sales ----------------------------------------------------------------

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
#ck_sales = ck_sell_order
#ck_tcg_sales_tbl %>% filter(name == "Bitterblossom")
colnames(Updated_Tracking_Keys)
colnames(Sets)
ck_sold_tbl = ck_sales %>% 
  mutate(edition = gsub("","",gsub(" Variants","",edition))) %>%
  left_join(Sets %>% select(CK_BL_Scrape_Sets,mtgjson),by=c("edition"="CK_BL_Scrape_Sets")) %>%
  mutate(semi = paste(card_name,mtgjson,sep="")) %>% 
  left_join(Updated_Tracking_Keys %>% select(param,Semi,Rarity) %>% group_by(Semi) %>% filter(param == min(param)) %>% ungroup() ,by=c("semi"="Semi")) %>%
  left_join(Combined_Ledger %>% rename(rarity = Rarity) %>% mutate(Semi = paste(Card_Name,Set,sep="")) %>% select(Semi,rarity), by = c("semi"="Semi")) %>%
  distinct() %>%
  mutate(Rarity = ifelse(is.na(Rarity),rarity,Rarity)) %>%
  mutate(Key = paste(card_name,mtgjson,Rarity," ",hasFoil,sep="")) %>%
  select(param,Key,card_name,mtgjson,Rarity,hasFoil,qty,ind_value,dos,sell_id,`Platform Sold`) %>% distinct() %>%
  rename(name = card_name,Set=mtgjson,Ind_Qty=qty,Ind_Amt = ind_value,DOS=dos) %>% arrange(desc(DOS))

colnames(ck_sold_tbl)
colnames(tcg_sales)

ck_tcg_sales_tbl = rbind(
  ck_sold_tbl,
  tcg_sales %>% select(-Ind_Amt) %>%
    mutate(Rarity = unlist(Rarity)) %>%
    rename(hasFoil=Foil,Ind_Amt = Ind_Rev) %>% 
    mutate(`Platform Sold` = "TCG") %>%
    select(param,Key,name,Set,Rarity,hasFoil,Ind_Qty,Ind_Amt,DOS,sell_id,`Platform Sold`)
  ) %>% mutate(Ind_Qty = as.numeric(Ind_Qty), Ind_Amt = as.numeric(Ind_Amt)) %>%
  rbind(cs_sales)

Combined_Ledger %>% colnames()
# Purchases ---------------------------------------------------------------
custom_sales = readxl::read_xlsx("/home/cujo253/Essential_Referential_CSVS/Ledger_Needed.xlsx",sheet = "Sales") %>%
  select(-param) %>%
  mutate(hasFoil = ifelse(is.na(hasFoil),"",hasFoil),
         sell_id = `Platform Sold`,
         DOS = format(DOS,"%Y-%m-%d")) %>%
  left_join(Updated_Tracking_Keys %>% select(param,Key),by=c("Key"="Key")) %>%
  group_by(Key,name,Set,Rarity,hasFoil,Ind_Qty,Ind_Amt,DOS,sell_id,`Platform Sold`) %>%
  summarize(param = min(param)) %>% ungroup() %>%
  select(param,everything())

custom_buys = readxl::read_xlsx("/home/cujo253/Essential_Referential_CSVS/Ledger_Needed.xlsx",sheet = "Purchases") %>%
  mutate(hasFoil = ifelse(is.na(hasFoil),"",hasFoil),
         Date = format(Date,"%Y-%m-%d")) %>%
  left_join(Updated_Tracking_Keys %>% select(param,Key),by=c("Key"="Key")) %>%
  group_by(Key,Date,Card_Name,Set,Rarity,hasFoil,Cost,Sender,Qty,Platform) %>%
  summarize(param = min(param)) %>% ungroup() %>%
  select(param,everything())


purchases_agg_tbl = Combined_Ledger %>% 
  rbind(custom_buys %>% select(-param)) %>%
  mutate(Qty = ifelse(Card_Name == "Ambition's Cost",1,Qty),
         Date = format(Date,"%Y-%m-%d")) %>% 
  group_by(Key,Card_Name,Set,Rarity,hasFoil) %>% 
  summarize(copies_purchased = sum(Qty),avg_cost = round(mean(Cost),2), avg_dop = mean(Date)) %>% 
  arrange(desc(copies_purchased))

purchases_singles_tbl = Combined_Ledger %>%
  mutate(Semi = paste(Card_Name,Set,sep=""),Qty = ifelse(Card_Name == "Ambition's Cost",1,Qty),
                           hasFoil = toupper(hasFoil),
                           Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,sep=""))) %>%
  left_join(ck_tcg_sales_tbl %>% rename(rarity = Rarity) %>% mutate(Semi = paste(name,Set,sep="")) %>% select(Semi,rarity) %>% distinct(), by = c("Semi"="Semi")) %>%
  mutate(Rarity = ifelse(is.na(Rarity),rarity,Rarity)) %>%
  mutate(Key = paste(Card_Name,Set,Rarity,hasFoil,sep="")) %>% select(-rarity,-Semi) %>% filter(is.na(Qty)==F) %>% rbind(custom_buys %>% select(-param))


pivot_purch_tbl = purchases_singles_tbl %>% mutate(Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,sep="")),
                                                   Date = round_date(Date,unit="day")) %>% 
  group_by(Key,Date) %>% summarize(Cost = round(sum(Cost)/sum(Qty),2), Buy_Qty = sum(Qty)) %>% ungroup()

pivot_sale_tbl = ck_tcg_sales_tbl %>% rbind(custom_sales) %>% mutate(Key = trimws(paste(name,Set,Rarity," ",hasFoil,sep="")),
                                             DOS = round_date(DOS,unit="day")) %>% 
  group_by(Key,DOS) %>% summarize(Rev = round(sum(Ind_Amt)/sum(Ind_Qty),2), Sale_Qty = sum(Ind_Qty)) %>% ungroup()


multiple_purchases = purchases_singles_tbl %>% mutate(Date = round_date(Date,unit="day")) %>% arrange(Key,Date) %>% filter(Qty > 1)

#multiple_purchases %>% view()
replacement_lines = NULL
for (i in 1:nrow(multiple_purchases)){
  new_lines = NULL
  for (j in 1:(multiple_purchases[i,]$Qty)){
    new_lines = rbind(new_lines,multiple_purchases[i,])
  }
  new_lines$Qty = 1
  replacement_lines = rbind(replacement_lines,new_lines)
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

multiple_sales = ck_tcg_sales_tbl %>% rbind(custom_sales) %>% mutate(DOS = round_date(DOS,unit="day"), Key = paste(name,Set,Rarity," ",hasFoil,sep="")) %>% arrange(Key,DOS) %>% filter(Ind_Qty > 1)

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


unnested_sales = ck_tcg_sales_tbl          %>%
  rbind(custom_sales)                      %>%
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


pivot_sale_tbl %>% filter(grepl("Awakening Zone",Key))
regrouped_purchases %>% filter(Card_Name == "Awakening Zone")
regrouped_sales %>% filter(name == "Awakening Zone")

final_ledger = regrouped_purchases %>% 
  mutate(Key = trimws(paste(Card_Name,Set,Rarity," ",hasFoil,ct,sep=""))) %>%
  left_join(regrouped_sales %>% 
              mutate(Key = trimws(paste(name,Set,Rarity," ",hasFoil,ct,sep=""))) %>%
              select(Key,Ind_Amt,DOS,sell_id,`Platform Sold`),
            by = c("Key"="Key")) %>%
  mutate(Ind_Amt = ifelse(`Platform Sold`=="CK-B",round(Ind_Amt *1.3,2),Ind_Amt )) %>%
  rename(Sold=Ind_Amt) 

# final_ledger %>% filter(is.na(DOS)) %>% view()

final_ledger %>% filter(Date > "2020-12-01" & is.na(DOS)) %>% view()


# Monthly Overview --------------------------------------------------------

pivot_purch_tbl = purchases_singles_tbl %>% mutate(Date = round_date(Date,unit="month")) %>% group_by(Key,Date) %>% summarize(Cost = round(sum(Cost)/sum(Qty),2), Buy_Qty = sum(Qty))

pivot_sale_tbl = ck_tcg_sales_tbl %>% mutate(DOS = round_date(DOS,unit="month")) %>% group_by(Key,DOS) %>% summarize(Rev = round(sum(Ind_Amt)/sum(Ind_Qty),2), Sale_Qty = sum(Ind_Qty))

purchase_Date_tbl = pivot_purch_tbl %>% left_join(pivot_sale_tbl,by=c("Key"="Key")) %>%
  mutate(total_cost = Cost * Buy_Qty, total_rev = Rev * Sale_Qty) %>%
  group_by(Date) %>% summarize(Ovr_Cost = sum(total_cost))

sold_Date_tbl = pivot_purch_tbl %>% left_join(pivot_sale_tbl,by=c("Key"="Key")) %>%
  mutate(total_cost = Cost * Buy_Qty, total_rev = Rev * Sale_Qty) %>%
  group_by(DOS) %>% summarize(Ovr_Proft = sum(total_rev))

joined_Date_tbl = purchase_Date_tbl %>% left_join(sold_Date_tbl, by = c("Date"="DOS"))



sum(joined_Date_tbl$Ovr_Cost,na.rm=T)
sum(joined_Date_tbl$Ovr_Proft,na.rm=T)


pivot_purch_tbl %>% left_join(pivot_sale_tbl,by=c("Key"="Key")) %>%
  mutate(total_cost = Cost * Buy_Qty, total_rev = Rev * Sale_Qty) %>%
  filter(DOS > Date) %>%
  arrange(desc(Date)) 

