source("config.R")
pacman::p_load(rvest,tidyverse,RSelenium,bigrquery)
invisible(chrome <-function(ip){
    eCaps = list(chromeOptions = list (
        args = c('--disable-gpu','--disable-blink-features=AutomationControlled','--disable-extensions','--start-maximized')
    ))
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome", extraCapabilities = eCaps)
    remDr$open()
    remDr
})
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
psa_breakdown = function(Singles_Review){
    magic_psa_tbl   = Singles_Review %>% filter(grepl("Magic the Gathering",Grouping)) #%>% mutate(Edition = ifelse(is.na(Edition),"TOTAL POPULATION",Edition))
    
    mtg_foil_group = magic_psa_tbl %>% filter(grepl("^foil$",tolower(Edition))) %>% 
        mutate(hasFoil = "FOIL", Edition = gsub("^\\d{4} Magic the Gathering ","",Grouping)) %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    mtg_nonfoil_group = magic_psa_tbl %>% filter(!grepl("^foil$",tolower(Edition))) %>% 
        mutate(hasFoil = "") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    mtg_all_cleaned_group = rbind(mtg_foil_group,mtg_nonfoil_group) %>% as.data.frame() 
    
    
    # Pokemon Breakdown -------------------------------------------------------
    pokemon_psa_tbl = Singles_Review %>% filter(grepl("Pokemon",Grouping) | grepl("Pocket Monsters",Grouping)) %>% mutate(Edition = gsub("^\\d{4}\\-*\\d* Pocket Monsters","",gsub("^\\d{4}\\-*\\d* Pokemon ","",Edition)))
    
    poke_holo_group = pokemon_psa_tbl %>% filter(grepl("-holo$",tolower(Edition))) %>% 
        mutate(hasFoil = "HOLO") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_gx_group = pokemon_psa_tbl %>% filter(grepl("\\sgx$",tolower(Edition))) %>% 
        mutate(hasFoil = "GX") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_v_group = pokemon_psa_tbl %>% filter(grepl("\\sv$",tolower(Name))) %>% 
        mutate(hasFoil = "V") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_vmax_group = pokemon_psa_tbl %>% filter(grepl("\\svmax$",tolower(Name))) %>% 
        mutate(hasFoil = "VMAX") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_nonfoil_group = pokemon_psa_tbl %>% filter( (!grepl("\\svmax$",tolower(Name))) &(!grepl("-holo$",tolower(Edition))) &(!grepl("\\sgx$",tolower(Edition))) &(!grepl("\\sv$",tolower(Name))) ) %>% 
        mutate(hasFoil = "") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_all_cleaned_group = rbind(poke_holo_group,poke_gx_group,poke_v_group,poke_vmax_group,poke_nonfoil_group) %>% as.data.frame() %>% arrange(desc(Total))
    
    
    # Yugioh Breakdown --------------------------------------------------------
    
    yugioh_psa_tbl  = Singles_Review %>% filter(grepl("YU\\-GI",Grouping)) %>%
        mutate(Edition = gsub("^\\d{4}\\-*\\d* YU\\-GI\\-Oh\\! ","",Edition)) %>%
        select(Grouping,Name,Edition,everything())
    
    # World of Warcraft Breakdown ---------------------------------------------
    
    wow_psa_tbl  = Singles_Review %>% filter(grepl("World of Warcraft",Grouping)) %>%
        mutate(Edition = gsub("^\\d{4}\\-*\\d* World of Warcraft ","",Edition)) %>%
        select(Grouping,Name,Edition,everything())
    
    
    # Flesh and Blood Breakdown ---------------------------------------------
    
    fab_psa_tbl  = Singles_Review %>% filter(grepl("Flesh and Blood",Grouping)) %>%
        mutate(Edition = gsub("^\\d{4}\\-*\\d* Flesh and Blood ","",Edition)) %>%
        select(Grouping,Name,Edition,everything())
    
    # Miscellaneous Breakdown -------------------------------------------------
    
    misc_psa_tbl = Singles_Review %>% filter(
        !grepl("YU\\-GI",Grouping) & 
            !grepl("Pokemon",Grouping) & 
            !grepl("Magic the Gathering",Grouping) & 
            !grepl("Pocket Monsters",Grouping) & 
            !grepl("World of Warcraft",Grouping) & 
            !grepl("Flesh and Blood",Grouping)) %>% as.data.frame() %>%
        select(Grouping,Name,Edition,everything())
    
    
    output = list(mtg_all_cleaned_group, poke_all_cleaned_group,yugioh_psa_tbl,wow_psa_tbl,fab_psa_tbl,misc_psa_tbl)
    
    return(output)
    
}
Grade_Type = function(start_seq,table_contents_condensed){
    #1 = Normal Graded
    #2 = `+` Grades
    #3 = Q Grades
    if(page == 1){
    full_grade_totals <- table_contents_total[seq(start_seq, length(table_contents_total), 3)]
    FG_Totaled = data.frame(matrix(unlist(full_grade_totals[1:13]), ncol=length(full_grade_totals[1:13]), byrow=T))
    
    Name = Grouping
    Edition = Grouping
    FG_Totaled = cbind(Name,Edition,FG_Totaled,Grouping)} else {
        
        Name = Grouping
        Edition = Grouping
        FG_Totaled = NA
        FG_Totaled = cbind(Name,Edition,FG_Totaled,Grouping)
        
    }
    
    FG_One_10 <- table_contents_condensed[seq(start_seq, length(table_contents_condensed), 3)]
    FG_One_10<- FG_One_10[!grepl("\\s\\s+",FG_One_10)]
    FG_One_10<- FG_One_10[!grepl("[A-Za-z]+",FG_One_10)]
    FG_One_10 = FG_One_10[FG_One_10 != ""] 
    
    a = 1
    b = 13
    FG_Total = NULL
    #increment = 1
    for(increment in 1:(length(FG_One_10)/13)){
        
        Name = Cards[increment]
        
        Edition = editions[increment]
        
        FG_Grades = data.frame(matrix(unlist(gsub("–","0",gsub("\\,","",FG_One_10[a:b]))), ncol=length(FG_One_10[a:b]), byrow=F))
        
        FG_card_insert = cbind(Name,Edition,FG_Grades,Grouping)
        
        FG_Total = rbind(FG_Total,FG_card_insert)
        
        if(nrow(na.omit(FG_Totaled))!=0){FG_Total = rbind(FG_Total,FG_Totaled)}
        
        a = b + 1
        b = b + 13
        if(b > (length(FG_One_10))){b = (length(FG_One_10))}
        if(a > b){a = b -1}
        
    }
    
    Grouping_Result = FG_Total %>% as_tibble() %>% rename(Auth = X1,
                                                          `1` = X2,
                                                          `1.5` = X3,
                                                          `2` = X4,
                                                          `3` = X5,
                                                          `4` = X6,
                                                          `5` = X7,
                                                          `6` = X8,
                                                          `7` = X9,
                                                          `8` = X10,
                                                          `9` = X11,
                                                          `10` = X12,
                                                          Total = X13)
    
    
    
    aggregated_buckets = rbind(aggregated_buckets,Grouping_Result) %>% distinct()
    
    return(aggregated_buckets)
}


# Setup -------------------------------------------------------------------


remDr = chrome("64.225.20.203")
remDr$navigate("https://www.psacard.com/pop/tcg-cards/156940")
Sys.sleep(1.5)
all_links = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("a") %>% html_attr('href')
pop_links_only = all_links[grepl("^\\/pop\\/.*",all_links)]

aggregated_buckets = NULL
fg_aggregated_buckets = NULL
plus_aggregated_buckets = NULL
q_aggregated_buckets = NULL

Start_Time = Sys.time()

# PSA Pillaging -----------------------------------------------------------

for(as_foretold in 1:(length(pop_links_only)) ){
    remDr$navigate(paste("https://www.psacard.com",pop_links_only[as_foretold],sep=""))
    Sys.sleep(1.5)
    raw_links = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("a") %>% html_attr('href')
    clean_links_only = raw_links[grepl("^\\/pop\\/.*",raw_links)]

    for(grand_abolisher in 2:length(clean_links_only)){
        remDr$navigate(paste("https://www.psacard.com",clean_links_only[grand_abolisher],sep=""))
        Sys.sleep(1.5)
        
        card_total = as.numeric(str_extract(gsub(",","",remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="mainContent"]/div[2]/div/div/div[1]/div/div/div[2]/div/div[2]/div[1]/div') %>% html_text()),"\\d+"))
        
        if(identical(card_total,numeric(0))){card_total = as.numeric(str_extract(gsub(",","",remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="mainContent"]/div[2]/div/div/div[1]/div/div/div/div/div[1]/div[1]/div') %>% html_text()),"\\d+"))}
        
        cards_in_pages = gsub("\\,","",remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="tablePSA_info"]') %>% html_text()) %>% str_extract(.,"\\d+$") %>% as.numeric()
        
        if(is.na(cards_in_pages)){cards_in_pages = gsub("\\,","",remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="mainContent"]/div[2]/div/div/div[1]/div/div/div/div/div[1]/div[2]/div') %>% html_text()) %>% str_extract(.,"\\d+$") %>% as.numeric()}
        
        pagination = as.numeric(str_extract(remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="tablePSA_paginate"]/span/span') %>% html_text(),"\\d+"))
        
        if(cards_in_pages < 300){pagination = 1}
            
        for(page in 1:pagination){
            
        Sys.sleep(1.5)
        hold_time = 1
        if(nrow(remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("table") %>% .[1] %>% html_table(fill = T) %>% as.data.frame()) == 0 ){hold_time = hold_time + 1}
        
        if((nrow(remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("table") %>% .[1] %>% html_table(fill = T) %>% as.data.frame()) == 0) & (hold_time <= 5) ){Sys.sleep(2)}
        
        if(nrow(remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("table") %>% .[1] %>% html_table(fill = T) %>% as.data.frame()) != 0){
                
            Grouping = remDr$getPageSource()[[1]]%>% read_html() %>% html_node("xpath" = '//*[@id="mainContent"]/div[2]/div/div/div[1]/div/div/div[2]/div/div[1]') %>% html_text()
            
            if(is.na(Grouping)){Grouping = remDr$getPageSource()[[1]]%>% read_html() %>% html_node("xpath" = '//*[@id="mainContent"]/div[2]/div/h1') %>% html_text()}
            
            
            Cards = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("strong") %>% html_text() 
            
            Cards = Cards[!grepl("\\d+\\s+(\\-|\\–)",Cards)]
            
            set_extraction = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("td") %>%
                as.character() %>%
                str_sub(start=23, end=-39) %>%
                str_replace_all("<br>", "\n") %>%
                str_replace_all("</strong>", " ")
            
            set_extraction = set_extraction[grepl("[A-Za-z]+",set_extraction)]
            set_extraction = set_extraction[!grepl("ne\\;",set_extraction)]
            set_extraction = set_extraction[!grepl("^abi$",set_extraction)]
            set_extraction = gsub("<a href.*","",gsub(">\n<strong>.*\\s+\n","",set_extraction))
            #set_extraction = set_extraction[20:length(set_extraction)]
            #editions = gsub(".*\\s\\\n","",gsub("<a.*","",set_extraction[seq(1,length(set_extraction),17)]))
            editions = ifelse(grepl(">\n<",set_extraction)==T,Grouping,set_extraction)
            
            table_contents = remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("div") %>% html_text() %>% as.list()
            
            if(page == 1){
            table_contents_total = table_contents[1:268]
            table_contents_total = table_contents_total[!grepl("[A-Za-z]+",table_contents_total)]
            table_contents_total = table_contents_total[!grepl("\\s\\s+",table_contents_total)]
            table_contents_total = table_contents_total[!grepl("\\+",table_contents_total)]
            table_contents_total = gsub("–","0",gsub("\\,","",table_contents_total))
            table_contents_total = table_contents_total[table_contents_total != ""]
            table_contents_total = table_contents_total[3:length(table_contents_total)]
            
            tct = table_contents
            tct = tct[!grepl("[A-Za-z]+",tct)]
            tct = tct[!grepl("\\s\\s+",tct)]
            tct = tct[!grepl("\\+",tct)]
            tct = tct[!grepl("«",tct)]
            tct = gsub("–","0",gsub("\\,","",tct))
            tct = tct[tct != ""]
            
            tct %>% head()
            
            cards_in_this_page = gsub("\\sof\\s\\d+$","",gsub("\\,","",remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="tablePSA_info"]') %>% html_text())) %>% str_extract(.,"\\d+$") %>% as.numeric()
            cards_in_this_page = cards_in_this_page / page
            
            
            tct = if( (length(tct[42:length(tct)]) / (cards_in_this_page - 1)) == 39){tct = tct[42:length(tct)]} else if ((length(tct[42:length(tct)]) / (cards_in_this_page)) == 39){tct = tct[42:length(tct)]} else {tct = print("Shit")}
            
            table_contents_condensed = tct
            table_contents_condensed = table_contents_condensed[!grepl("Grade",table_contents_condensed)]
            table_contents_condensed = table_contents_condensed[!grepl("\\+",table_contents_condensed)]
            table_contents_condensed = table_contents_condensed[!grepl("^Q$",table_contents_condensed)]
            
            #start_seq = 1
            #Grade_Type = function(start_seq,table_contents_condensed){}
            
            
            fg_aggregated_buckets = rbind(fg_aggregated_buckets,Grade_Type(1,table_contents_condensed)) %>% as.data.frame() %>% distinct()
            plus_aggregated_buckets = rbind(plus_aggregated_buckets,Grade_Type(2,table_contents_condensed)) %>% as.data.frame() %>% distinct()
            q_aggregated_buckets = rbind(q_aggregated_buckets,Grade_Type(3,table_contents_condensed)) %>% as.data.frame() %>% distinct()
            
            } else {
                
                table_contents_total = NA
                
                tct = table_contents
                tct = tct[!grepl("[A-Za-z]+",tct)]
                tct = tct[!grepl("\\s\\s+",tct)]
                tct = tct[!grepl("\\+",tct)]
                tct = tct[!grepl("«",tct)]
                tct = gsub("–","0",gsub("\\,","",tct))
                tct = tct[tct != ""]
                
                # cards_in_this_page = gsub("\\sof\\s\\d+$","",gsub("\\,","",remDr$getPageSource()[[1]]%>% read_html() %>% html_nodes("xpath"='//*[@id="tablePSA_info"]') %>% html_text())) %>% str_extract(.,"\\d+$") %>% as.numeric()
                # cards_in_this_page = cards_in_this_page / page
                # 
                # 
                # tct = if( (length(tct[3:length(tct)]) / (cards_in_this_page )) == 39){tct = tct[42:length(tct)]}
                # 
                tct = tct[3:length(tct)]
                
                table_contents_condensed = tct
                table_contents_condensed = table_contents_condensed[!grepl("Grade",table_contents_condensed)]
                table_contents_condensed = table_contents_condensed[!grepl("\\+",table_contents_condensed)]
                table_contents_condensed = table_contents_condensed[!grepl("^Q$",table_contents_condensed)]
                
                
                fg_aggregated_buckets = rbind(fg_aggregated_buckets,Grade_Type(1,table_contents_condensed)) %>% as.data.frame() %>% distinct()
                plus_aggregated_buckets = rbind(plus_aggregated_buckets,Grade_Type(2,table_contents_condensed)) %>% as.data.frame() %>% distinct()
                q_aggregated_buckets = rbind(q_aggregated_buckets,Grade_Type(3,table_contents_condensed)) %>% as.data.frame() %>% distinct()
                
                
            }
            
            
            remDr$findElement("xpath",'//*[@id="tablePSA_next"]')$clickElement()
            
        } else (print(paste("PSA Group",
                            if(is.na(remDr$getPageSource()[[1]]%>% read_html() %>% html_node("xpath" = '//*[@id="mainContent"]/div[2]/div/div/div[1]/div/div/div[2]/div/div[1]') %>% html_text())){remDr$getPageSource()[[1]]%>% read_html() %>% html_node("xpath" = '//*[@id="mainContent"]/div[2]/div/h1') %>% html_text()}else{remDr$getPageSource()[[1]]%>% read_html() %>% html_node("xpath" = '//*[@id="mainContent"]/div[2]/div/div/div[1]/div/div/div[2]/div/div[1]')},
                            "is Currently Empty")))
        
        }
    }
}

Final_Time = Sys.time()
print(Final_Time - Start_Time)

g = fg_aggregated_buckets %>% mutate(across(Auth:Total,as.numeric)) %>% group_by(Name,Edition,Grouping) %>% summarize(Auth = sum(Auth),  
                                                                        `1` = sum(`1`), 
                                                                        `1.5` = sum(`1.5`), 
                                                                        `2` = sum(`2`),   
                                                                        `3` = sum(`3`),   
                                                                        `4` = sum(`4`), 
                                                                        `5` = sum(`5`),   
                                                                        `6` = sum(`6`),   
                                                                        `7` = sum(`7`),   
                                                                        `8` = sum(`8`),   
                                                                        `9` = sum(`9`),   
                                                                        `10` = sum(`10`),
                                                                        Total = sum(Total),
                                                                        Grade = "Grade") 

plus = plus_aggregated_buckets %>% mutate(across(Auth:Total,as.numeric)) %>% group_by(Name,Edition,Grouping) %>% summarize(Auth = sum(Auth),  
                                                                                                                                   `1` = sum(`1`), 
                                                                                                                                   `1.5` = sum(`1.5`), 
                                                                                                                                   `2` = sum(`2`),   
                                                                                                                                   `3` = sum(`3`),   
                                                                                                                                   `4` = sum(`4`), 
                                                                                                                                   `5` = sum(`5`),   
                                                                                                                                   `6` = sum(`6`),   
                                                                                                                                   `7` = sum(`7`),   
                                                                                                                                   `8` = sum(`8`),   
                                                                                                                                   `9` = sum(`9`),   
                                                                                                                                   `10` = sum(`10`),
                                                                                                                                   Total = sum(Total),
                                                                                                                                   Grade = "+")

q = q_aggregated_buckets %>% mutate(across(Auth:Total,as.numeric)) %>% group_by(Name,Edition,Grouping) %>% summarize(Auth = sum(Auth),  
                                                                                                                                   `1` = sum(`1`), 
                                                                                                                                   `1.5` = sum(`1.5`), 
                                                                                                                                   `2` = sum(`2`),   
                                                                                                                                   `3` = sum(`3`),   
                                                                                                                                   `4` = sum(`4`), 
                                                                                                                                   `5` = sum(`5`),   
                                                                                                                                   `6` = sum(`6`),   
                                                                                                                                   `7` = sum(`7`),   
                                                                                                                                   `8` = sum(`8`),   
                                                                                                                                   `9` = sum(`9`),   
                                                                                                                                   `10` = sum(`10`),
                                                                                                                                   Total = sum(Total),
                                                                                                                                   Grade = "Q")


summed_buckets = rbind(g,plus,q)%>% arrange(desc(Total))


Edition_Review = summed_buckets %>% ungroup() %>% filter((Name == Edition) & (Name == Grouping) & (Grouping == Edition)) %>% select(-Name,-Edition) %>% rename(`PSA_1` = `1`, 
                                                                                                                                                               `PSA1_5` = `1.5`, 
                                                                                                                                                               `PSA_2` = `2`,   
                                                                                                                                                               `PSA_3` = `3`,   
                                                                                                                                                               `PSA_4` = `4`, 
                                                                                                                                                               `PSA_5` = `5`,   
                                                                                                                                                               `PSA_6` = `6`,   
                                                                                                                                                               `PSA_7` = `7`,   
                                                                                                                                                               `PSA_8` = `8`,   
                                                                                                                                                               `PSA_9` = `9`,   
                                                                                                                                                               `PSA_10` = `10`)

Singles_Review = summed_buckets %>% filter((Name != Grouping))



# PSA Breakdown -----------------------------------------------------------
psa_breakdown = function(Singles_Review){
    magic_psa_tbl   = Singles_Review %>% filter(grepl("Magic the Gathering",Grouping)) #%>% mutate(Edition = ifelse(is.na(Edition),"TOTAL POPULATION",Edition))
    
    mtg_foil_group = magic_psa_tbl %>% filter(grepl("^foil$",tolower(Edition))) %>% 
        mutate(hasFoil = "FOIL", Edition = gsub("^\\d{4} Magic the Gathering ","",Grouping)) %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    mtg_nonfoil_group = magic_psa_tbl %>% filter(!grepl("^foil$",tolower(Edition))) %>% 
        mutate(hasFoil = "") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    mtg_all_cleaned_group = rbind(mtg_foil_group,mtg_nonfoil_group) %>% as.data.frame() 
    
    
    # Pokemon Breakdown -------------------------------------------------------
    pokemon_psa_tbl = Singles_Review %>% filter(grepl("Pokemon",Grouping) | grepl("Pocket Monsters",Grouping)) %>% mutate(Edition = gsub("^\\d{4}\\-*\\d* Pocket Monsters","",gsub("^\\d{4}\\-*\\d* Pokemon ","",Edition)))
    
    poke_holo_group = pokemon_psa_tbl %>% filter(grepl("-holo$",tolower(Edition))) %>% 
        mutate(hasFoil = "HOLO") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_gx_group = pokemon_psa_tbl %>% filter(grepl("\\sgx$",tolower(Edition))) %>% 
        mutate(hasFoil = "GX") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_v_group = pokemon_psa_tbl %>% filter(grepl("\\sv$",tolower(Name))) %>% 
        mutate(hasFoil = "V") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_vmax_group = pokemon_psa_tbl %>% filter(grepl("\\svmax$",tolower(Name))) %>% 
        mutate(hasFoil = "VMAX") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_nonfoil_group = pokemon_psa_tbl %>% filter( (!grepl("\\svmax$",tolower(Name))) &(!grepl("-holo$",tolower(Edition))) &(!grepl("\\sgx$",tolower(Edition))) &(!grepl("\\sv$",tolower(Name))) ) %>% 
        mutate(hasFoil = "") %>%
        select(Grouping,Name,Edition,hasFoil,everything())
    
    poke_all_cleaned_group = rbind(poke_holo_group,poke_gx_group,poke_v_group,poke_vmax_group,poke_nonfoil_group) %>% as.data.frame() %>% arrange(desc(Total))
    
    
    # Yugioh Breakdown --------------------------------------------------------
    
    yugioh_psa_tbl  = Singles_Review %>% filter(grepl("YU\\-GI",Grouping)) %>%
        mutate(Edition = gsub("^\\d{4}\\-*\\d* YU\\-GI\\-Oh\\! ","",Edition)) %>%
        select(Grouping,Name,Edition,everything())
    
    # World of Warcraft Breakdown ---------------------------------------------
    
    wow_psa_tbl  = Singles_Review %>% filter(grepl("World of Warcraft",Grouping)) %>%
        mutate(Edition = gsub("^\\d{4}\\-*\\d* World of Warcraft ","",Edition)) %>%
        select(Grouping,Name,Edition,everything())
    
    
    # Flesh and Blood Breakdown ---------------------------------------------
    
    fab_psa_tbl  = Singles_Review %>% filter(grepl("Flesh and Blood",Grouping)) %>%
        mutate(Edition = gsub("^\\d{4}\\-*\\d* Flesh and Blood ","",Edition)) %>%
        select(Grouping,Name,Edition,everything())
    
    # Miscellaneous Breakdown -------------------------------------------------
    
    misc_psa_tbl = Singles_Review %>% filter(
        !grepl("YU\\-GI",Grouping) & 
            !grepl("Pokemon",Grouping) & 
            !grepl("Magic the Gathering",Grouping) & 
            !grepl("Pocket Monsters",Grouping) & 
            !grepl("World of Warcraft",Grouping) & 
            !grepl("Flesh and Blood",Grouping)) %>% as.data.frame() %>%
        select(Grouping,Name,Edition,everything())
    
    
    output = list(mtg_all_cleaned_group, poke_all_cleaned_group,yugioh_psa_tbl,wow_psa_tbl,fab_psa_tbl,misc_psa_tbl)
    
    return(output)
    
}

psa_db_tbls = psa_breakdown(Singles_Review)


# PSA DB Upload -----------------------------------------------------------

con <- gaeas_cradle("wolfoftinstreet@gmail.com")

for(i in 1:6){
    export = psa_db_tbls[[i]] %>% as.data.frame() %>% rename(`PSA_1` = `1`, 
                                                             `PSA1_5` = `1.5`, 
                                                             `PSA_2` = `2`,   
                                                             `PSA_3` = `3`,   
                                                             `PSA_4` = `4`, 
                                                             `PSA_5` = `5`,   
                                                             `PSA_6` = `6`,   
                                                             `PSA_7` = `7`,   
                                                             `PSA_8` = `8`,   
                                                             `PSA_9` = `9`,   
                                                             `PSA_10` = `10`)
    if(i == 1){dataset_name  = "psa_singles_magic"}
    if(i == 2){dataset_name  = "psa_singles_pokemon"}
    if(i == 3){dataset_name  = "psa_singles_yugioh"}
    if(i == 4){dataset_name  = "psa_singles_wow"}
    if(i == 5){dataset_name  = "psa_singles_fab"}
    if(i == 6){dataset_name  = "psa_singles_misc"}
    
    mybq <- bq_table(project = "gaeas-cradle", dataset = dataset_name, table = paste(gsub("-","_",Sys.Date()),"_PSA",sep=""))
    bq_table_upload(x=mybq, values = export, fields=as_bq_fields(export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
}
print("BQ PSA Totals Upload Successful!")

mybq <- bq_table(project = "gaeas-cradle", dataset = "psa_totals", table = paste(gsub("-","_",Sys.Date()),"_PSA",sep=""))
bq_table_upload(x=mybq, values = Edition_Review, fields=as_bq_fields(Edition_Review),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
