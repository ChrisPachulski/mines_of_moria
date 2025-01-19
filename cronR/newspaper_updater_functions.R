source("config.R")
pacman::p_load(chatgpt,jsonlite)
patches = fromJSON(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "personal_data.json"))


three_day_newspaper <- function(){
    #' Connect to three_day_newspaper database
    #' 
    #' @param user The username to use when connecting to the database.
    #' @param password The password to use when connecting to the database.
    #' @param dbname The name of the database to connect to.
    #' @param host The IP address of the database host.
    #'
    #' @return A connection to the three_day_newspaper database.
    #'
    #' @export
    con = dbConnect(MySQL(), user='remote', password=patches$mysql, dbname ='three_day_newspaper',host=patches$mysql_ip)
    con
}
one_day_newspaper <- function(){
    #' Connect to one_day_newspaper database
    #' 
    #' @param user The username to use when connecting to the database.
    #' @param password The password to use when connecting to the database.
    #' @param dbname The name of the database to connect to.
    #' @param host The IP address of the database host.
    #'
    #' @return A connection to the three_day_newspaper database.
    #'
    #' @export
    con = dbConnect(MySQL(), user='remote', password=patches$mysql, dbname ='newspaper',host=patches$mysql_ip)
    con
}

gaeas_cradle <- function(){
    #' Connect to Gaeas Cradle BigQuery Database
    #' 
    #' @param patches A vector containing the email patches. 
    #' 
    #' @return A BigQuery connection object. 
    #' 
    #' @export 
    #'
    #'
    #'
    service_account_file = file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json')
    gar_auth_service(service_account_file)
    
    bq_auth(path = service_account_file)
    
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    
    options(scipen = 20)
    con
}

google_auths = function(){
    options(googleAuthR.json_path = file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json'))
    
    drive_auth(path=file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json'),cache=TRUE,use_oob = TRUE)
    # Don't be a moron, save yourself 4 hours, and share the spreadsheet with the service account email address
    gs4_auth(path=file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json'),cache=TRUE,use_oob = TRUE)
    gc()
    suppressMessages(gc())
}

set_list = function(){
    
    options(googleAuthR.json_path = file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json'))
    
    drive_auth(path=file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json'),cache=TRUE,use_oob = TRUE)
    # Don't be a moron, save yourself 4 hours, and share the spreadsheet with the service account email address
    gs4_auth(path=file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json'),cache=TRUE,use_oob = TRUE)
    gc()
    
    ss <- drive_get("Sets")
    
    Sets <-  suppressMessages(read_sheet(ss,"Sets") %>% mutate_if(is.character,as.factor))
    
    ck_conversion <-  suppressMessages(read_sheet(ss,"mtgjson_ck_sets"))
    
    tryCatch({Updated_Tracking_Keys <- read_csv(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "C20_Addition.csv"), col_types = cols(hasFoil = col_character())) %>%
        #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
        rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
        mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "C20_Addition.csv"), col_types = cols(hasFoil = col_character())) %>%
            rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
            #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
            mutate(Semi = paste(name, Set,sep=""))})
    
    Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                            Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                            Semi = paste(name,Set,sep="")) 
    return(list(Sets,ck_conversion,Updated_Tracking_Keys))
}


dataset_extraction_and_replacement = function(dataset,email_body = list()){
    if(dataset == "newspaper_updated"){
        updated_at_tbl = as_tibble(data.frame(data_value = Sys.Date()))
        return(updated_at_tbl)
    }
    
    if(dataset == "premiums"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        table_limitations = table_limitations %>% mutate(most_recent_table = most_recent_table-1)
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date) ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
            
            
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a ",sep = "")
        delayed_premium <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() %>% select(!date) 
        
        statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a ",sep = "")
        current_premium <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names()  %>% select(!date)
        
        return(list(email_body,delayed_premium,current_premium))
    }
    if(dataset == "ck_funny_money"){
        bq_con = gaeas_cradle()
        dataset = "ck_funny_money"
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
            
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Foil_Status as `F_NF`, a.BL_QTY,a.BL,a.TCG_MKT,a.CK_MKT,a.MKT_Diff,a.Sellers,a.CK_Backing,a.TCG_Backing,p.TCG_Rank,p.CK_ADJ_Rank ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                           " LEFT JOIN `gaeas-cradle.premiums.",gsub("-","_",table_limitations$most_recent_table - 3),"*` p  on r.Key = p.Key",sep = "")
        
        delayed_ck_funny_money <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names()  
        
        statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Foil_Status as `F_NF`, a.BL_QTY,a.BL,a.TCG_MKT,a.CK_MKT,a.MKT_Diff,a.Sellers,a.CK_Backing,a.TCG_Backing,p.TCG_Rank,p.CK_ADJ_Rank ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                           " LEFT JOIN `gaeas-cradle.premiums.",gsub("-","_",table_limitations$most_recent_table),"*` p  on r.Key = p.Key",sep = "")
        
        current_ck_funny_money <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() 
        
        return(list(email_body,delayed_ck_funny_money,current_ck_funny_money))
    }
    if(dataset == "buylist_growth"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as Foil, a.Todays_BL,a.Yesterday_BL,a.Week_Ago_BL,a.Month_Ago_BL,a.Yesterday_BL_Chg,a.Week_Ago_BL_Chg,a.Month_Ago_BL_Chg,a.Buylist_Backing ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                           " WHERE Todays_BL is not NULL and Yesterday_BL is not NULL and Week_Ago_BL is not NULL and Month_Ago_BL is not NULL and Yesterday_BL_Chg is not NULL and Week_Ago_BL_Chg is not NULL and Month_Ago_BL_Chg is not NULL", sep = "")
        
        delayed_buylist_growth <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() %>% 
            filter(!is.na(foil)) %>% 
            mutate(uuid = all_editions$uuid[match(unique_keys,all_editions$Key)]) %>%
            select(!c("unique_keys","name","set","rarity","foil","buylist_backing")) %>% 
            select(uuid,everything()) %>% 
            filter(todays_bl >= 3, week_ago_bl_chg <= 3) 
        
        statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as Foil, a.Todays_BL,a.Yesterday_BL,a.Week_Ago_BL,a.Month_Ago_BL,a.Yesterday_BL_Chg,a.Week_Ago_BL_Chg,a.Month_Ago_BL_Chg,a.Buylist_Backing ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                           " WHERE Todays_BL is not NULL and Yesterday_BL is not NULL and Week_Ago_BL is not NULL and Month_Ago_BL is not NULL and Yesterday_BL_Chg is not NULL and Week_Ago_BL_Chg is not NULL and Month_Ago_BL_Chg is not NULL", sep = "")
        
        current_buylist_growth <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() %>% 
            filter(!is.na(foil)) %>% 
            mutate(uuid = all_editions$uuid[match(unique_keys,all_editions$Key)]) %>%
            select(!c("unique_keys","name","set","rarity","foil","buylist_backing")) %>% 
            select(uuid,everything()) %>% 
            filter(todays_bl >= 3, week_ago_bl_chg <= 3) 
        
        return(list(email_body,delayed_buylist_growth,current_buylist_growth))
    }
    if(dataset == "demand_growth"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_TCG,a.Yesterday_TCG,a.Week_Ago_TCG,a.Month_Ago_TCG,a.Yesterday_TCG_Chg,a.Week_Ago_TCG_Chg,a.Month_Ago_TCG_Chg ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Key", sep = "")
        
        delayed_demand_growth <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names()  
        
        statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_TCG,a.Yesterday_TCG,a.Week_Ago_TCG,a.Month_Ago_TCG,a.Yesterday_TCG_Chg,a.Week_Ago_TCG_Chg,a.Month_Ago_TCG_Chg ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Key", sep = "")
        
        current_demand_growth <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() 
        
        return(list(email_body,delayed_demand_growth,current_demand_growth))
    }
    if(dataset == "vendor_growth"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_Sellers,a.Yesterday_Sellers,a.Week_Ago_Sellers,a.Month_Ago_Sellers,a.Yesterday_Sellers_Chg,a.Week_Ago_Sellers_Chg,a.Month_Ago_Sellers_Chg ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys", sep = "")
        
        delayed_vendor_growth <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() %>% 
            mutate(uuid = all_editions$uuid[match(unique_keys,all_editions$Key)]) %>%
            select(uuid,todays_sellers,yesterday_sellers,week_ago_sellers, month_ago_sellers,yesterday_sellers_chg,week_ago_sellers_chg,month_ago_sellers_chg) 
        
        statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_Sellers,a.Yesterday_Sellers,a.Week_Ago_Sellers,a.Month_Ago_Sellers,a.Yesterday_Sellers_Chg,a.Week_Ago_Sellers_Chg,a.Month_Ago_Sellers_Chg ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys", sep = "")
        
        current_vendor_growth <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() %>% 
            mutate(uuid = all_editions$uuid[match(unique_keys,all_editions$Key)]) %>%
            select(uuid,todays_sellers,yesterday_sellers,week_ago_sellers, month_ago_sellers,yesterday_sellers_chg,week_ago_sellers_chg,month_ago_sellers_chg)
        
        return(list(email_body,delayed_vendor_growth,current_vendor_growth))
    }
    if(dataset == "kpi"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT DISTINCT rtrim(a.Key) as Key, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as `F_NF`,a.Ranking, p.MKT as Retail,p.BL as Buylist, Sellers ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = rtrim(a.Key)",
                           " LEFT JOIN `gaeas-cradle.premiums.",gsub("-","_",table_limitations$most_recent_table - 3),"*` p  on r.Key = rtrim(p.Key)",
                           " ORDER BY Ranking", sep = "")
        
        delayed_kpi <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names()
        colnames(delayed_kpi)[5] <- "F/NF"
        delayed_kpi <- delayed_kpi %>% mutate(ranking = seq(nrow(.)))  %>% filter(`F/NF` == "") %>% 
            mutate(uuid = all_editions$uuid[match(key,all_editions$Key)]) %>% 
            mutate(ranking = seq(nrow(.)))  %>%
            select(uuid,ranking,retail,buylist,sellers) %>% 
            `colnames<-`(c("uuid","Ranking","Retail","Buylist","Vendors"))
        
        
        uuid_check = delayed_kpi %>% group_by(uuid) %>% summarize(ct = n()) 
        
        kpi_replacements = delayed_kpi %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
            filter(ct >= 2) %>% select(-ct) %>% group_by(uuid) %>%
            summarize(Ranking = min(Ranking),
                      Retail = mean(Retail),
                      Buylist = min(Buylist),
                      Vendors = max(Vendors)) %>%
            ungroup()
        
        delayed_kpi = delayed_kpi %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
            filter(ct < 2) %>% select(-ct) %>%
            rbind(kpi_replacements) %>% 
            arrange(Ranking) %>%
            mutate(Ranking = seq(nrow(.)))
        
        statement <- paste("SELECT DISTINCT rtrim(a.Key) as Key, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as `F_NF`,a.Ranking, p.MKT as Retail,p.BL as Buylist, Sellers ",
                           "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a ",
                           " LEFT JOIN roster.mtgjson r on r.Key = rtrim(a.Key)",
                           " LEFT JOIN `gaeas-cradle.premiums.",gsub("-","_",table_limitations$most_recent_table),"*` p  on r.Key = rtrim(p.Key)",
                           " ORDER BY Ranking", sep = "")
        
        current_kpi <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names()
        colnames(current_kpi)[5] <- "F/NF"
        current_kpi <- current_kpi %>% mutate(ranking = seq(nrow(.)))  %>% filter(`F/NF` == "") %>% 
            mutate(uuid = all_editions$uuid[match(key,all_editions$Key)]) %>% 
            mutate(ranking = seq(nrow(.)))  %>%
            select(uuid,ranking,retail,buylist,sellers) %>% 
            `colnames<-`(c("uuid","Ranking","Retail","Buylist","Vendors"))
        
        
        
        uuid_check = current_kpi %>% group_by(uuid) %>% summarize(ct = n()) 
        
        kpi_replacements = current_kpi %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
            filter(ct >= 2) %>% select(-ct) %>% group_by(uuid) %>%
            summarize(Ranking = min(Ranking),
                      Retail = mean(Retail),
                      Buylist = min(Buylist),
                      Vendors = max(Vendors)) %>%
            ungroup()
        
        current_kpi = current_kpi %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
            filter(ct < 2) %>% select(-ct) %>%
            rbind(kpi_replacements) %>% 
            arrange(Ranking) %>%
            mutate(Ranking = seq(nrow(.)))
        
        return(list(email_body,delayed_kpi,current_kpi))
    }
    if(dataset == "ck_velocity"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 1){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            } 
        }
        else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste("SELECT * ", "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 3),"*` a Limit 5000 ",sep = "")
        delayed_ck_velocity <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() %>% select(!meta_created_at) 
        
        statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),"*` a Limit 5000 ",sep = "")
        current_ck_velocity <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names()  %>% select(!meta_created_at)
        
        return(list(email_body,delayed_ck_velocity,current_ck_velocity))
    }
    if(dataset == "ensemble_forecast_results"){
        bq_con = gaeas_cradle()
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date))-1 most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
                            ORDER BY most_recent_table desc
                            LIMIT 5
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        if(table_limitations$days_behind_today > 2){
            days_behind = as.numeric(Sys.Date() - table_limitations$most_recent_table) - 1
            
            for(i in 1:days_behind){
                new_table_date = gsub("","",table_limitations$most_recent_table + i)
                
                statement <- paste("SELECT * ","FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table),table_limitations$table_name,"` a ",sep = "")
                latest_accurate_data <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1)
                latest_accurate_data$Date = table_limitations$most_recent_table + i
                
                bq_con <- gaeas_cradle()
                mybq <- bq_table(project = "gaeas-cradle", dataset = dataset, table = paste0(gsub("-","_",new_table_date)  ,table_limitations$table_name))
                bq_table_upload(x=mybq, values = latest_accurate_data, fields=as_bq_fields(latest_accurate_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
                
                email_body = rbind(email_body,list(paste0("Dataset: ",dataset," from Table: ",table_limitations$table_name," on Date: ",gsub("_","-",new_table_date)," was created via ",table_limitations$most_recent_table,".")) )
                
            }
        }else{
            print("No Replacement Effects Were Required!")
        }
        
        statement <- paste('SELECT table_name, most_recent_table, DATE_DIFF( CURRENT_DATE() , most_recent_table ,DAY) days_behind_today
                            FROM (
                                SELECT max(CAST(regexp_replace(regexp_extract(table_name,"\\\\d{4}_\\\\d{2}_\\\\d{2}"),"_","-") as Date)) most_recent_table, regexp_extract(table_name,"\\\\_[A-Za-z]+") as table_name
                                FROM `gaeas-cradle`.',dataset,'.INFORMATION_SCHEMA.TABLES
                                GROUP BY table_name
                            )
         ',sep = "")
        table_limitations <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% as_tibble() %>% drop_na()
        
        statement <- paste('SELECT *, CASE WHEN Classification = "S" then "A" 
                            WHEN Classification = "A" then "B" 
                            WHEN Classification = "B" then "C" 
                            WHEN Classification = "C" then "D" 
                            WHEN Classification = "D" then "E" 
                            WHEN Classification = "E" then "F" 
                            WHEN Classification = "F" then "G" 
                            WHEN Classification = "Ignore" then "H"
                            END as custom_sort ', "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 5),table_limitations$table_name,"` a ", "ORDER BY custom_sort ",sep = "")
        delayed_ensemble <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() 
        delayed_ensemble <- delayed_ensemble %>% rename(Card = name, 
                                                        Recent_BL = current_val, 
                                                        Historical_plus_minus = iqr,
                                                        Historical_Median = median_val, 
                                                        Historical_Max = outer_lim,
                                                        Forecasted_BL = max_forecast_value,
                                                        Forecast_plus_minus = plus_minus,
                                                        Target_Date = date,
                                                        Tier = classification,
                                                        Behavior = safety) %>%
            select(-sd) %>% mutate(uuid = all_editions$uuid[match(key,all_editions$Key)],
                                   number = all_editions$number[match(uuid,all_editions$uuid)]) %>%
            clean_names() %>%
            select(uuid,key,card,set,number, everything()) %>% distinct() %>% select(-rarity)
        
        statement <- paste('SELECT *, CASE WHEN Classification = "S" then "A" 
                            WHEN Classification = "A" then "B" 
                            WHEN Classification = "B" then "C" 
                            WHEN Classification = "C" then "D" 
                            WHEN Classification = "D" then "E" 
                            WHEN Classification = "E" then "F" 
                            WHEN Classification = "F" then "G" 
                            WHEN Classification = "Ignore" then "H"
                            END as custom_sort ', "FROM `gaeas-cradle.",dataset,".",gsub("-","_",table_limitations$most_recent_table - 2),table_limitations$table_name,"` a ", "ORDER BY custom_sort ",sep = "")
        current_ensemble <- dbSendQuery(bq_con, statement = statement) %>% dbFetch( n = -1) %>% clean_names() 
        current_ensemble <- current_ensemble %>% rename(Card = name, 
                                                        Recent_BL = current_val, 
                                                        Historical_plus_minus = iqr,
                                                        Historical_Median = median_val, 
                                                        Historical_Max = outer_lim,
                                                        Forecasted_BL = max_forecast_value,
                                                        Forecast_plus_minus = plus_minus,
                                                        Target_Date = date,
                                                        Tier = classification,
                                                        Behavior = safety) %>%
            select(-sd) %>% mutate(uuid = all_editions$uuid[match(key,all_editions$Key)],
                                   number = all_editions$number[match(uuid,all_editions$uuid)]) %>%
            clean_names() %>%
            select(uuid,key,card,set,number, everything()) %>% distinct() %>% select(-rarity)
        
        return(list(email_body,delayed_ensemble,current_ensemble))
    }
    
}

email_content_creation = function(email_body){
    message_content = NULL
    for(i in 1:(email_body %>% length())){
        message_content = rbind(message_content,email_body[[i]]) 
        message_content = suppressWarnings(message_content%>% as_tibble())
    }
    
    body_message_tbl = message_content %>% 
        separate(V1,c("Ignore","dataset","table","missing_dates"),sep=": ") %>%
        separate(missing_dates,c("missing_dates","origin_date"),sep=" was created via ") %>%
        select(-Ignore)
    
    core_error_check = str_extract(grepl("(premiums|ck_funny_money)",body_message_tbl$dataset),"TRUE") %>% as_tibble() %>% drop_na() %>% group_by(value) %>% summarize(ct = n()) %>% ungroup() %>% distinct()
    
    sentence = ""
    try(if(core_error_check$value == "TRUE"){
        sentence = paste0(sentence,core_error_check$ct," Core Error(s) Detected: Premiums &(/Or) CK Funny Money Failed (/are Failing) to update: Review CK, Goldfish, & TCG core elements. ")
    }else{
        sentence = sentence
    },silent = T)
    
    
    growth_error_check = str_extract(grepl("(growth)",body_message_tbl$dataset),"TRUE") %>% as_tibble() %>% drop_na() %>% group_by(value) %>% summarize(ct = n()) %>% ungroup() %>% distinct()
    try(if(growth_error_check$value == "TRUE"){
        sentence = paste0(sentence,growth_error_check$ct," Growth Error(s) Detected: Demand &(/Or) Vendor &(/Or) Demand Growth Failed (/are Failing) to update: Review historical replacement logic in BQ. ")
    }else{
        sentence = sentence
    },silent = T)
    
    kpi_error_check = str_extract(grepl("(kpi)",body_message_tbl$dataset),"TRUE") %>% as_tibble() %>% drop_na() %>% group_by(value) %>% summarize(ct = n()) %>% ungroup() %>% distinct()
    
    try(if(kpi_error_check$value == "TRUE"){
        sentence = paste0(sentence,kpi_error_check$ct," KPI Error Detected: Final Aggregation Failed (/is Failing) to update: Review final join in R. ")
    }else{
        sentence = sentence
    },silent = T)
    
    ck_velocity_error_check = str_extract(grepl("(ck_velocity)",body_message_tbl$dataset),"TRUE") %>% as_tibble() %>% drop_na() %>% group_by(value) %>% summarize(ct = n()) %>% ungroup() %>% distinct()
    try(if(ck_velocity_error_check$value == "TRUE"){
        sentence = paste0(sentence,ck_velocity_error_check$ct," CK Velocity Error Detected: CK_CLosed_System Script Failed (/is Failing) to update: Review Independent Script. ")
    }else{
        sentence = sentence
    },silent = T)
    
    ensemble_error_check = str_extract(grepl("(ensemble)",body_message_tbl$dataset),"TRUE") %>% as_tibble() %>% drop_na() %>% group_by(value) %>% summarize(ct = n()) %>% ungroup() %>% distinct()
    try(if(ensemble_error_check$value == "TRUE"){
        sentence = paste0(sentence,ensemble_error_check$ct," Forecasting Error Detected: H2O Script Failed (/is Failing) to update: Review Independent Script. ")
    }else{
        sentence = sentence
    },silent = T)
    
    return(sentence)
}

send_email = function(to,from,email_content){
    
    gm_auth("wolfoftinstreet@gmail.com")
    
    gm_token_write(path = file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "ban_newspaper_web.rds"), key = "GMAILR_KEY")
    
    gm_auth(token = gm_token_read(
        file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "ban_newspaper_web.rds"),
        key = "GMAILR_KEY"
    ))
    
    
    my_email = gm_mime() %>%
        gm_to(to) %>%
        gm_from(from) %>%
        gm_subject(paste0(Sys.Date()," Newspaper Error Report")) %>%
        gm_text_body(email_content)
    
    tryCatch({
        gm_send_message(my_email)
    }, error = function(e) {
        print(e$message)
    })
}

error_csv = function(email_body){
    message_content = NULL
    for(i in 1:(email_body %>% length())){
        message_content = rbind(message_content,email_body[[i]]) 
        message_content = suppressWarnings(message_content%>% as_tibble())
    }
    
    body_message_tbl = message_content %>% 
        separate(V1,c("Ignore","dataset","table","missing_dates"),sep=": ") %>%
        separate(missing_dates,c("missing_dates","origin_date"),sep=" was created via ") %>%
        select(-Ignore)
    
    return(body_message_tbl)
}


ban_delayed_newspaper = function(table, data){
    ban_con = three_day_newspaper()
    if(table == "newspaper_updated"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='newspaper_updated')
        dbDisconnect(ban_con)
    }
    if(table == "kpi"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='top_25')
        dbDisconnect(ban_con)
    }
    if(table == "vendor_growth"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='vendor_levels')
        dbDisconnect(ban_con)
    }
    if(table == "buylist_growth"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='buylist_levels')
        dbDisconnect(ban_con)
    }
    if(table == "ensemble_forecast_results"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='ensemble_forecast')
        dbDisconnect(ban_con)
    }
    if(table == "all_editions"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='mtgjson_portable')
        dbDisconnect(ban_con)
    }
    
}

ban_current_newspaper = function(table, data){
    ban_con = one_day_newspaper() 
    if(table == "newspaper_updated"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='newspaper_updated')
        dbDisconnect(ban_con)
    }
    if(table == "kpi"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='top_25')
        dbDisconnect(ban_con)
    }
    if(table == "vendor_growth"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='vendor_levels')
        dbDisconnect(ban_con)
    }
    if(table == "buylist_growth"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='buylist_levels')
        dbDisconnect(ban_con)
    }
    if(table == "ensemble_forecast_results"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='ensemble_forecast')
        dbDisconnect(ban_con)
    }
    if(table == "all_editions"){
        data = data
        dbWriteTable(conn=ban_con, overwrite = TRUE, append= FALSE, value = data, name ='mtgjson_portable')
        dbDisconnect(ban_con)
    }
    
}


