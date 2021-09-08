pacman::p_load(rvest,tidyverse,RSelenium,bigrquery)
invisible(chrome <-function(ip){
    eCaps = list(chromeOptions = list (
        args = c('--disable-gpu','--disable-blink-features=AutomationControlled','--disable-extensions','--start-maximized')
    ))
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome", extraCapabilities = eCaps)
    remDr$open()
    remDr
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


sets <- read_html("https://cardmavin.com/pokemon/pokemon-card-set-symbols") %>% 
    html_node("table") %>% 
    html_table(fill = TRUE) %>% 
    select(X1,X2) %>% 
    mutate(X1 = tolower(gsub(" Series","",X1)),
           X2 = trimws((X2 %>% 
                            gsub("© ","",.) %>%
                            gsub("[A-Za-z]+","",.) %>%
                            gsub("&","",.) %>%
                            gsub("\\s*\\-\\s*\\d*","",.))) ) %>%
    distinct() %>%
    `colnames<-` (c("editions","year")) 

for(i in 1:nrow(sets)){
    if(sets$year[i]==""){sets$year[i] = sets$year[i+1]}
}

sets <- sets %>% arrange(year)

edited_sets = gsub("^wizards ","",gsub("'","",gsub("-","",gsub("trainer kit.*","trainer kit",gsub("pop \\d*","pop",gsub("\\&","and",gsub(" promos$","",gsub(" set$","",sets$editions))))))))
edited_sets = unique(trimws(edited_sets))

remDr = chrome("64.225.20.203")
remDr$navigate("https://www.beckett.com/login?utm_content=bkthp&utm_term=login")
Sys.sleep(1)
username <- remDr$findElement("id","loginEmail")
Sys.sleep(sample(1:3, 1))
username$clickElement()
Sys.sleep(sample(1:3, 1))
username$sendKeysToElement(list("wolfof"))
Sys.sleep(.5)
username$sendKeysToElement(list("tinstreet"))
Sys.sleep(.5)
username$sendKeysToElement(list("@gmail"))
Sys.sleep(.5)
username$sendKeysToElement(list(".com"))
Sys.sleep(.5)
Password <- remDr$findElement("id","loginPassword")
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

remDr$findElement("xpath",'//*[@id="loginFrm"]/div[3]/input')$clickElement()
Sys.sleep(1)

remDr$navigate("https://www.beckett.com/grading/pop-report")
remDr$findElement("xpath",'//*[@id="sport_id"]/option[31]')$clickElement()

store_becket_links = NULL

for(i in 1:length(edited_sets)){
set_box = remDr$findElement("id","set_name")
set_box$clearElement()
set_box$clickElement()
set_box$sendKeysToElement(list(edited_sets[i]))

remDr$findElement('xpath','//*[@id="pop_report_search"]/div[4]/input')$clickElement()
Sys.sleep(2)

all_links = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("a") %>% html_attr('href')
all_links_text = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("a") %>% html_text()

mini_df = cbind(all_links,all_links_text) %>% as.data.frame() %>% filter(grepl("\\/set\\_match\\/.*",all_links)) %>%
    filter(!grepl("(German|Japanese|Italian|Cantonese|Mandarin|French|Dutch|Korean|Spanish|Portuguese|Russian)",all_links_text))

pop_links_only = data.frame(newcol = c(t(mini_df$all_links)), stringsAsFactors=FALSE)

store_becket_links = rbind(store_becket_links,pop_links_only) %>% distinct()
}

psa_sets_converted = psa_db_tbls[7] %>% as.data.frame() %>% 
    distinct() %>% filter(!grepl("Japanese",PSA_Bucket)) %>% 
    filter(V1 == "Grade") %>% 
    select(PSA_Bucket) %>% 
    distinct() %>%
    mutate(PSA_Bucket = gsub("\\&","and",gsub(".*Starter Set","Starter Set",gsub("^Coin$","",gsub(" Promo$","",gsub("\\d{4} Pokemon ","",PSA_Bucket)))))) %>%
    filter(!grepl(" Battle Deck",PSA_Bucket)) %>%
    filter(!grepl("(Pop Series|Tin Topper|^Promo$|^\\d{4}.*|Portuguese|Spanish|German|Dutch|French|^Action)",PSA_Bucket)) %>%
    distinct()

for(i in 1:nrow(psa_sets_converted)){
    set_box = remDr$findElement("id","set_name")
    set_box$clearElement()
    set_box$clickElement()
    set_box$sendKeysToElement(list(psa_sets_converted$PSA_Bucket[i]))
    
    remDr$findElement('xpath','//*[@id="pop_report_search"]/div[4]/input')$clickElement()
    Sys.sleep(2)
    
    all_links = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("a") %>% html_attr('href')
    all_links_text = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("a") %>% html_text()
    
    mini_df = cbind(all_links,all_links_text) %>% as.data.frame() %>% filter(grepl("\\/set\\_match\\/.*",all_links)) %>%
        filter(!grepl("(German|Japanese|Italian|Cantonese|Mandarin|French|Dutch|Korean|Spanish|Portuguese|Russian)",all_links_text))
    
    pop_links_only = data.frame(newcol = c(t(mini_df$all_links)), stringsAsFactors=FALSE)
    
    store_becket_links = rbind(store_becket_links,pop_links_only) %>% distinct()
}

store_becket_links$newcol = trimws(store_becket_links$newcol)

Beckett_Compilation = NULL

for(i in 1:nrow(store_becket_links)){
    remDr$navigate(store_becket_links[i,])
    Sys.sleep(2)
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(.5)
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(.5)
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(.5)
    webElem$sendKeysToElement(list(key = "end"))
    Sys.sleep(.5)
    page_data = 
        remDr$getPageSource()[[1]]%>% 
        read_html() %>% 
        html_nodes("table") %>% .[2] %>% 
        html_table(fill = TRUE) %>% 
        as.data.frame() %>% 
        filter(Player != "Player")
    
    webElem$sendKeysToElement(list(key = "home"))
    
    page_data %>% tail()
    
    header = remDr$getPageSource()[[1]]%>% 
        read_html() %>% 
        html_node("h2") %>% 
        html_text()
    
    page_data$Becket_Bucket = header
    
    Beckett_Compilation = rbind(Beckett_Compilation,page_data)
}




