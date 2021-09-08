install.packages("pacman")
pacman::p_load(devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate,rvest,bigrquery)
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

Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE) %>% select(mtgjson,TCG_Key,Set_Excl,Excl_Excl)
#View(Sets)
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



remDr = remoteDriver(remoteServerAddr = "167.172.233.212", port = 4445, browser = "chrome")
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

remDr$navigate("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList")

remDr$findElement("xpath",'//*[@id="categoryId"]/option[2]')$clickElement()
Sys.sleep(.5)
remDr$findElement("xpath",'//*[@id="conditionId"]/option[2]')$clickElement()


remDr$findElement("xpath",'//*[@id="PageSize"]/option[6]')$clickElement()

page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()

last_update_date = page_html %>% html_node("xpath"='//*[@id="categoryListLastUpdated"]') %>% html_text() %>% mdy_hm() %>% round_date(unit="day")

mini_roster = Updated_Tracking_Keys %>% select(uuid,Set,number) %>% mutate(set_number = paste(Set,number,sep="")) %>% select(uuid,set_number)

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement <- paste("SELECT DISTINCT max(Date) as Date
FROM `gaeas-cradle.mtg_syp.*`  
WHERE  _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY)) AND 
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ",sep = "")
last_date <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)  


if(last_date$Date < last_update_date){
    

    page_contents = page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
        
    
    pagination_number = round(as.numeric(str_extract(page_html %>% html_node(xpath='//*[@id="rightSide"]/div/div[9]/div[2]/div[1]/div/span') %>% html_text(),"\\d{3}\\d+")),-3)/500
    
    for(i in 2:pagination_number){
        remDr$navigate(paste("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList?categoryid=1&setNameID=All&conditionId=1&page=",i,sep=""))
        
        page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
        
        added_page_contents = page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
        
        page_contents = rbind(page_contents,added_page_contents)
    }
    
    
    remDr$navigate("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList")
    
    remDr$findElement("xpath",'//*[@id="categoryId"]/option[2]')$clickElement()
    Sys.sleep(.5)
    remDr$findElement("xpath",'//*[@id="conditionId"]/option[5]')$clickElement()
    
    
    remDr$findElement("xpath",'//*[@id="PageSize"]/option[6]')$clickElement()
    
    foil_page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    foil_page_contents = page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
    
    pagination_number = round(as.numeric(str_extract(foil_page_html %>% html_node(xpath='//*[@id="rightSide"]/div/div[9]/div[2]/div[1]/div/span') %>% html_text(),"\\d{3}\\d+")),-3)/500
    
    for(i in 2:pagination_number){
        remDr$navigate(paste("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList?categoryid=1&setNameID=All&conditionId=3&page=",i,sep=""))
        
        foil_page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
        
        added_page_contents = foil_page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
        
        foil_page_contents = rbind(foil_page_contents,added_page_contents)
    }
    
    page_contents = page_contents %>% mutate(hasFoil = "")
    foil_page_contents = foil_page_contents %>% mutate(hasFoil = "FOIL")
    
    page_contents = rbind(page_contents,foil_page_contents)
    
    names(page_contents) = str_replace_all(names(page_contents),c(" "="_"))
    
    page_contents = page_contents %>% 
        left_join(Sets %>% select(mtgjson,TCG_Key), by = c("Set"="TCG_Key")) %>% 
        left_join(Sets %>% select(Set_Excl,Excl_Excl),by = c("mtgjson"="Set_Excl")) %>%
        mutate(Set = mtgjson) %>%
        select(-mtgjson) %>% mutate(set_number = paste(Set,Number,sep="")) %>%
        left_join(mini_roster,by = c("set_number"="set_number")) %>%
        select(-set_number) %>% select(uuid, everything()) %>%
        mutate(uuid = ifelse(hasFoil != "",paste(uuid,"_f",sep=""),uuid)) %>%
        rename(Legality = Excl_Excl)
    
    page_contents$Date = last_update_date %>% ymd()
    
    page_contents = page_contents %>% distinct()
    
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    
    mybq <- bq_table(project = "gaeas-cradle", dataset = "mtg_syp", table = paste(gsub("-","_",last_update_date %>% ymd()),"_MTG_SYP",sep=""))
    bq_table_upload(x=mybq, values = page_contents, fields=as_bq_fields(page_contents),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
    print("BQ SYP Upload Successful!")
} else {print("No MTG SYP Update Today")}

Sys.sleep(3)

remDr$navigate("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList")

remDr$findElement("xpath",'//*[@id="categoryId"]/option[3]')$clickElement()
Sys.sleep(.5)
remDr$findElement("xpath",'//*[@id="conditionId"]/option[2]')$clickElement()


remDr$findElement("xpath",'//*[@id="PageSize"]/option[6]')$clickElement()

page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()

last_update_date = page_html %>% html_node("xpath"='//*[@id="categoryListLastUpdated"]') %>% html_text() %>% mdy_hm() %>% round_date(unit="day")

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement <- paste("SELECT DISTINCT max(Date) as Date
FROM `gaeas-cradle.poke_syp.*`  
WHERE  _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY)) AND 
  FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ",sep = "")
last_date <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)  

if(last_date$Date < last_update_date){
    
    
    page_contents = page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
    
    
    pagination_number = round(as.numeric(str_extract(page_html %>% html_node(xpath='//*[@id="rightSide"]/div/div[9]/div[2]/div[1]/div/span') %>% html_text(),"\\d{3}\\d+")),-3)/500
    
    for(i in 2:pagination_number){
        remDr$navigate(paste("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList?categoryid=3&setNameID=All&conditionId=1&page=",i,sep=""))
        
        page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
        
        added_page_contents = page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
        
        page_contents = rbind(page_contents,added_page_contents)
    }
    
    remDr$navigate("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList")
    
    remDr$findElement("xpath",'//*[@id="categoryId"]/option[2]')$clickElement()
    Sys.sleep(.5)
    remDr$findElement("xpath",'//*[@id="conditionId"]/option[5]')$clickElement()
    
    
    remDr$findElement("xpath",'//*[@id="PageSize"]/option[6]')$clickElement()
    
    foil_page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
    
    foil_page_contents = page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
    
    pagination_number = round(as.numeric(str_extract(foil_page_html %>% html_node(xpath='//*[@id="rightSide"]/div/div[9]/div[2]/div[1]/div/span') %>% html_text(),"\\d{3}\\d+")),-3)/500
    
    for(i in 2:pagination_number){
        remDr$navigate(paste("https://store.tcgplayer.com/admin/direct/SYPDesiredInventoryList?categoryid=3&setNameID=All&conditionId=85&page=",i,sep=""))
        
        foil_page_html = remDr$getPageSource() %>% .[[1]] %>% read_html()
        
        added_page_contents = foil_page_html %>% html_node("xpath"='//*[@id="rightSide"]/div/div[9]/div[2]/table') %>% html_table()
        
        foil_page_contents = rbind(foil_page_contents,added_page_contents)
    }
    
    page_contents = page_contents %>% mutate(hasFoil = "")
    foil_page_contents = foil_page_contents %>% mutate(hasFoil = "FOIL")
    
    page_contents = rbind(page_contents,foil_page_contents)
    
    names(page_contents) = str_replace_all(names(page_contents),c(" "="_"))
    
    page_contents$Date = last_update_date %>% ymd()
    
    page_contents = page_contents %>% distinct()
    
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    
    mybq <- bq_table(project = "gaeas-cradle", dataset = "poke_syp", table = paste(gsub("-","_",last_update_date %>% ymd()),"_POKE_SYP",sep=""))
    bq_table_upload(x=mybq, values = page_contents, fields=as_bq_fields(page_contents),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
    print("Pokemon SYP Upload Successful!")
} else {print("No Pokemon SYP Update Today")}


