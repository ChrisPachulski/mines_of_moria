rm(list = ls())
options(java.parameters = "-Xmx32g")
`%!in%` = Negate(`%in%`)
pacman::p_load(httr,tidyverse,lubridate,bigrquery,googledrive,googlesheets4,stats,xlsx,readxl,janitor,rvest,coinmarketcapr,jsonlite)

path = "/home/cujo253/mines_of_moria/zed_run/"
custom_stable = read_xlsx(path = paste0(path,"zed_stable_list.xlsx"),sheet="horses") %>% clean_names()
#Default is set to Excel - Any change will result in GCS (Google Cloud Storage) option
Default = "Excel"
#Export Style set to Excel as default, otherwise Googlesheets
Export = "Excel"

# Set Up ------------------------------------------------------------------
# https://coinmarketcap.com/api/
# Get Yo Token. Fool.
# I saved mine in same path as a json file

setup(api_key = fromJSON(paste0(path,"coinmarket.json"))$token)
ETH_Value = get_crypto_quotes(symbol="ETH")
ETH_USD = ETH_Value$price

gaeas_cradle <- function(email,project,dataset){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = project,
        dataset = dataset,
        billing = project
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 999)
    return(con)
}
big_query_connection = function(email,project,dataset){tryCatch({
    con <- gaeas_cradle(email,project,dataset)
    statement = paste0("SELECT * FROM `",project,".",dataset,".all`")
    racing_data = dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
    
    existing_date = racing_data %>% 
        select(start_time) %>% 
        filter(start_time == max(start_time)) %>% 
        distinct() %>%
        mutate(start_time = floor_date(start_time,"day")) %>%
        .[[1]]
    
    days_since_update = as.numeric(Sys.Date() - as.Date(existing_date))
    
    if(days_since_update > 1){
        for(i in days_since_update:2){
            gc()
            date_to_grab = Sys.Date() - days_since_update
            
            horse_data = read_csv(paste("https://zed-odds.netlify.app/horse_db.csv",sep="") )
            
            horse_race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_horse_race_data.csv",sep="") ) ) %>% 
                mutate(odds = ifelse(is.na(odds),0,odds))
            
            race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_race.csv",sep="") ))
            
            gc()
            
            racing_data_bucket = race_data %>% 
                left_join(horse_race_data,  by=c("race_id"="race_id")) %>%
                left_join(horse_data,by=c("horse_id"="horse_id")) %>% distinct() %>%
                rename(race_name = name.x, horse_name = name.y)
            
            racing_data = rbind(racing_data,racing_data_bucket) %>% distinct()
            
            gc()
            
            con <- gaeas_cradle(email)
            mybq <- bq_table(project = project, dataset = dataset, table = "all")
            bq_table_upload(x=mybq, values = racing_data, fields=as_bq_fields(racing_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
            
        }
    }
    
    
    return(racing_data)
}, error = function(e){
    
    horse_race_all <- rbind(read_csv(paste0(path,"horse_race_data_db.csv") ),read_csv(paste0(path,"horse_race_data_db_before_08_01_21.csv"))) %>% distinct() 
    race_all <- read_csv(paste0(path,"race_db.csv")) 
    horse_all <- read_csv(paste0(path,"horse_db.csv") )
    
    gc()
    
    racing_data = race_all %>% 
        left_join(horse_race_all,  by=c("race_id"="race_id")) %>%
        left_join(horse_all,by=c("horse_id"="horse_id")) %>% distinct() %>%
        rename(race_name = name.x, horse_name = name.y)
    
    gc()
    
    existing_date = racing_data %>% 
        select(start_time) %>% 
        filter(start_time == max(start_time)) %>% 
        distinct() %>%
        mutate(start_time = floor_date(start_time,"day")) %>%
        .[[1]]
    
    days_since_update = as.numeric(Sys.Date() - as.Date(existing_date))
    
    if(days_since_update > 1){
        for(i in days_since_update:2){
            gc()
            date_to_grab = Sys.Date() - days_since_update
            
            horse_data = read_csv(paste("https://zed-odds.netlify.app/horse_db.csv",sep="") )
            
            horse_race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_horse_race_data.csv",sep="") ) ) %>% 
                mutate(odds = ifelse(is.na(odds),0,odds))
            
            race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_race.csv",sep="") ))
            
            gc()
            
            racing_data_bucket = race_data %>% 
                left_join(horse_race_data,  by=c("race_id"="race_id")) %>%
                left_join(horse_data,by=c("horse_id"="horse_id")) %>% distinct() %>%
                rename(race_name = name.x, horse_name = name.y)
            
            racing_data = rbind(racing_data,racing_data_bucket) %>% distinct()
            
            gc()
            
            con <- gaeas_cradle("wolfoftinstreet@gmail.com")
            mybq <- bq_table(project = project, dataset = dataset, table = "all")
            bq_table_upload(x=mybq, values = racing_data, fields=as_bq_fields(racing_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
            
        }
    }
    else{
        gc()
        
        con <- gaeas_cradle("wolfoftinstreet@gmail.com")
        mybq <- bq_table(project = "gaeas-cradle", dataset = "zed_run", table = "all")
        bq_table_upload(x=mybq, values = racing_data, fields=as_bq_fields(racing_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
        
        gc()
    }
    return(racing_data)
})}
excel_csv_connection = function(){tryCatch({
    racing_data = read_csv(paste0(path,"zed_run_massive_history.csv"))
    
    existing_date = racing_data %>% 
        select(start_time) %>% 
        filter(start_time == max(start_time)) %>% 
        distinct() %>%
        mutate(start_time = floor_date(start_time,"day")) %>%
        .[[1]]
    
    days_since_update = as.numeric(Sys.Date() - as.Date(existing_date))
    
    if(days_since_update > 1){
        for(i in days_since_update:2){
            gc()
            date_to_grab = Sys.Date() - days_since_update
            
            horse_data = read_csv(paste("https://zed-odds.netlify.app/horse_db.csv",sep="") )
            
            horse_race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_horse_race_data.csv",sep="") ) ) %>% 
                mutate(odds = ifelse(is.na(odds),0,odds))
            
            race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_race.csv",sep="") ))
            
            gc()
            
            racing_data_bucket = race_data %>% 
                left_join(horse_race_data,  by=c("race_id"="race_id")) %>%
                left_join(horse_data,by=c("horse_id"="horse_id")) %>% distinct() %>%
                rename(race_name = name.x, horse_name = name.y)
            
            racing_data = rbind(racing_data,racing_data_bucket) %>% distinct()
            
        }
        write_csv(racing_data,paste0(path,"zed_run_massive_history.csv"))
    }
    
    return(racing_data)
},error = function(e){
    horse_race_all <- rbind(read_csv(paste0(path,"horse_race_data_db.csv") ),read_csv(paste0(path,"horse_race_data_db_before_08_01_21.csv"))) %>% distinct() 
    race_all <- read_csv(paste0(path,"race_db.csv")) 
    horse_all <- read_csv(paste0(path,"horse_db.csv") )
    
    gc()
    
    racing_data = race_all %>% 
        left_join(horse_race_all,  by=c("race_id"="race_id")) %>%
        left_join(horse_all,by=c("horse_id"="horse_id")) %>% distinct() %>%
        rename(race_name = name.x, horse_name = name.y)
    
    gc()
    
    existing_date = racing_data %>% 
        select(start_time) %>% 
        filter(start_time == max(start_time)) %>% 
        distinct() %>%
        mutate(start_time = floor_date(start_time,"day")) %>%
        .[[1]]
    
    days_since_update = as.numeric(Sys.Date() - as.Date(existing_date))
    
    if(days_since_update > 1){
        for(i in days_since_update:2){
            gc()
            date_to_grab = Sys.Date() - days_since_update
            
            horse_data = read_csv(paste("https://zed-odds.netlify.app/horse_db.csv",sep="") )
            
            horse_race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_horse_race_data.csv",sep="") ) ) %>% 
                mutate(odds = ifelse(is.na(odds),0,odds))
            
            race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",date_to_grab,"_race.csv",sep="") ))
            
            gc()
            
            racing_data_bucket = race_data %>% 
                left_join(horse_race_data,  by=c("race_id"="race_id")) %>%
                left_join(horse_data,by=c("horse_id"="horse_id")) %>% distinct() %>%
                rename(race_name = name.x, horse_name = name.y)
            
            racing_data = rbind(racing_data,racing_data_bucket) %>% distinct()
            
        }
        write_csv(racing_data,paste0(path,"zed_run_massive_history.csv"))
    }
    else{
        write_csv(racing_data,paste0(path,"zed_run_massive_history.csv"))
    }
    
    return(racing_data)
})}
# horse_list = c("horse1","horse2",...)
i_wanna_know_now_damn_it = function(horse_list){
    stats = NULL
    for(i in 1:length(horse_list)){
        horse_id_info = racing_data %>%
            filter(horse_name %in% horse_list) %>%
            select(horse_id,horse_name) %>%
            distinct()
        Sys.sleep(.02)
        stat = GET(paste0("https://knowyourhorses.com/horses/",horse_id_info$horse_id[i],"/speed_statistics")) %>% 
            content("text") %>% 
            read_html() %>% 
            html_table() %>%
            .[[1]] %>%
            as_tibble() %>% 
            rename(SD = `Standard Deviation`,
                   distance = Distance) %>%
            mutate(distance = as.numeric(gsub("m","",distance))) %>%
            mutate(horse_name = horse_id_info$horse_name[i]) %>%
            select(horse_name, everything())
        
        stats = rbind(stats, stat)
    }
    
    selection_overview_tble = stats %>%
        drop_na() %>%
        left_join(ovr_baseline_stats,by=c("distance"="Base_distance")) %>%
        group_by(horse_name,distance) %>%
        summarize(
            Count = Count,
            Base_Speed = scales::percent((((Mean - Base_Mean)/Base_SD)*-1)),
            SD_Delta = scales::percent(((SD-Base_SD) / Base_SD)),
            Max_Speed = scales::percent((((Mean-(2*SD))-(Base_Mean-(2*Base_SD)))/Base_SD)*-1),
            Z_Top = (`Base_25%`-`25%`)/Base_SD,
            Z_Bottom = (`Base_75%`-`75%`)/Base_SD,
            Skew = Z_Top - Z_Bottom
            
        ) %>% 
        ungroup() %>%
        arrange(horse_name,distance)
    
    return(selection_overview_tble)
}
i_wanna_race = function(horse_list,distance,class){
    racing_info = class_overview_tbl%>% filter(horse_name %in% horse_list ) %>% 
        filter(distance == distance) %>% 
        filter(class == class)
    
    return(racing_info)
}


if(Default == "Excel"){
    racing_data = excel_csv_connection()
} else {
    big_query_connection(email = "abc@gmail.com",
                         project = "gaeas-cradle",
                         dataset = "premiums")
}

# Overall Race Times ------------------------------------------------------
ovr_baseline_stats = racing_data %>%
    filter(place >= 1 & place <= 3) %>%
    group_by(distance) %>%
    summarize(
        Count = n(),
        Fire_Rate = round(sum(fire)/Count,3),
        Min = min(horse_time, na.rm =T),
        Mean = mean(horse_time, na.rm =T),
        Max = max(horse_time, na.rm =T),
        Range = Max - Min,
        SD = sd(horse_time),
        `25%` =  quantile(horse_time,c(.25)),,
        Median = round(median(horse_time,na.rm=T),2),
        `75%` = quantile(horse_time,c(.75))
    ) %>%
    ungroup() 
colnames(ovr_baseline_stats) = paste("Base",colnames(ovr_baseline_stats),sep = "_")

horsea_tbl = racing_data %>%
    filter(place >= 1 & place <= 3) %>%
    group_by(horse_name,distance) %>%
    summarize(
        Count = n(),
        Fire_Rate = round(sum(fire)/Count,3),
        Min = min(horse_time, na.rm =T),
        Mean = mean(horse_time, na.rm =T),
        Max = max(horse_time, na.rm =T),
        Range = Max - Min,
        SD = sd(horse_time),
        `25%` = quantile(horse_time,c(.25)),
        Median = round(median(horse_time,na.rm=T),2),
        `75%` = quantile(horse_time,c(.75))
    ) %>%
    replace(is.na(.),0) %>%
    ungroup() %>% 
    arrange(distance)

selection_overview_tble = horsea_tbl %>%
    left_join(ovr_baseline_stats,by=c("distance"="Base_distance")) %>%
    group_by(horse_name,distance) %>%
    summarize(
        Count = Count,
        Base_Fire_Rate = scales::percent((Fire_Rate - Base_Fire_Rate)/Base_Fire_Rate),
        Base_Speed = scales::percent((((Mean - Base_Mean)/Base_SD)*-1)),
        SD_Delta = scales::percent(((SD-Base_SD) / Base_SD)),
        Max_Speed = scales::percent((((Mean-(2*SD))-(Base_Mean-(2*Base_SD)))/Base_SD)*-1),
        Z_Top = (`Base_25%`-`25%`)/Base_SD,
        Z_Bottom = (`Base_75%`-`75%`)/Base_SD,
        Skew = Z_Top - Z_Bottom
        
    ) %>% 
    ungroup() %>%
    arrange(horse_name,distance)


# By Class ----------------------------------------------------------------

class_baseline_stats = racing_data %>%
    group_by(class,distance) %>%
    summarize(
        Count = n(),
        Fire_Rate = round(sum(fire)/Count,3),
        Min = min(horse_time, na.rm =T),
        Mean = mean(horse_time, na.rm =T),
        Max = max(horse_time, na.rm =T),
        Range = Max - Min,
        SD = sd(horse_time),
        `25%` =  quantile(horse_time,c(.25)),,
        Median = round(median(horse_time,na.rm=T),2),
        `75%` = quantile(horse_time,c(.75))
    ) %>%
    ungroup()
colnames(class_baseline_stats) = paste("Base",colnames(class_baseline_stats),sep = "_")

horsea_classes_tbl = racing_data %>%
    #filter(horse_name %in% custom_stable$horse_names) %>%
    group_by(horse_name,class,distance) %>%
    summarize(
        Count = n(),
        Fire_Rate = round(sum(fire)/Count,3),
        Min = min(horse_time, na.rm =T),
        Mean = mean(horse_time, na.rm =T),
        Max = max(horse_time, na.rm =T),
        Range = Max - Min,
        SD = sd(horse_time),
        `25%` = quantile(horse_time,c(.25)),
        Median = round(median(horse_time,na.rm=T),2),
        `75%` = quantile(horse_time,c(.75))
    ) %>%
    replace(is.na(.),0) %>%
    ungroup() %>% 
    arrange(horse_name,desc(class),distance)

class_overview_tbl = horsea_classes_tbl %>%
    left_join(class_baseline_stats,by=c("class"="Base_class","distance"="Base_distance")) %>%
    group_by(horse_name,class,distance) %>%
    summarize(
        Count = Count,
        Base_Fire_Rate = round((Fire_Rate - Base_Fire_Rate)/Base_Fire_Rate,2),
        Base_Speed = (((Mean - Base_Mean)/Base_SD)*-1),
        SD_Delta = ((SD-Base_SD) / Base_SD),
        Max_Speed = (((Mean-(2*SD))-(Base_Mean-(2*Base_SD)))/Base_SD)*-1,
        Z_Top = round((`Base_25%`-`25%`)/Base_SD,2),
        Z_Bottom = round((`Base_75%`-`75%`)/Base_SD,2),
        Skew = Z_Top - Z_Bottom
        
    ) %>% 
    ungroup() %>%
    arrange(horse_name,desc(class),distance)



excel_exporter =  suppressWarnings(function(df){
    write.xlsx(ovr_baseline_stats %>% as.data.frame(), file=paste0(path,"overall_horse_stats.xlsx"), sheetName="Overall", row.names=FALSE)
    wb <- loadWorkbook(paste0(path,"overall_horse_stats.xlsx"))
    new_sheet <- createSheet(wb,sheetName = "Class_Reviews")
    addDataFrame(class_baseline_stats %>% as.data.frame(), new_sheet, row.names=FALSE)
    
    new_sheet <- createSheet(wb,sheetName = "My_Stable_Overall")
    addDataFrame(selection_overview_tble %>% as.data.frame(), new_sheet, row.names=FALSE)
    
    new_sheet <- createSheet(wb,sheetName = "My_Stable_By_Class")
    addDataFrame(class_overview_tbl %>% as.data.frame(), new_sheet, row.names=FALSE)
    
    saveWorkbook(wb, paste0(path,"overall_horse_stats.xlsx"))
    
    print(paste("Exported All Tabs For Your Stable!!! We are SUPER cool.",sep=""))
})

excel_exporter()



# Function for Races! -----------------------------------------------------
# This will make Hawku upset with me.
# Please don't make Hawku upset with me.

# Since Hawku's CSV update daily, for newer horses
# or for general curiosity, we can scrape
# know your horse for real time updates
# This is only for Overall stats right now.
# I haven't written a class version yet.
# This function has thus been named, with affection,
# as `i_wanna_know_now_damn_it`
horse_list = c("Peggy")
i_wanna_know_now_damn_it(horse_list)


# Let's Go Shopping! ------------------------------------------------------
# Choose your desired filters
filter_horses = class_overview_tbl %>%
    filter(Count >= 15) %>%
    filter(Count <= 150) %>%
    filter(Base_Fire_Rate >= .40) %>%
    filter(SD_Delta >= .05) %>% 
    #filter(class == 5) %>%
    filter(Skew >= -.01 & Skew <= .01) %>%
    arrange(horse_name,desc(distance)) 

# Preps a list of horse names and id's for 
# url grabs and human eyes at the end.
horses_of_interest = filter_horses %>%
    select(horse_name) %>%
    left_join(racing_data %>% select(horse_name,horse_id),
              by = c("horse_name"="horse_name")) %>%
    distinct() %>% select(horse_id) %>%
    as.list()

# Once again, we will see which of the horses
# meet our requirements above and if they're for sale
# If so, we compile them into a short list :)
cost = NULL
for(i in 1:length(horses_of_interest$horse_id)){
    Sys.sleep(.1)
    buy_now = tryCatch({(GET(paste0("https://www.hawku.com/horse/",horses_of_interest$horse_id[i])) %>% 
                             content("text") %>% 
                             read_html() %>% 
                             html_nodes('a') %>% 
                             html_attr("href") %>% 
                             as_tibble() %>%
                             filter(grepl("market_click",value)) %>% 
                             unlist() %>% 
                             str_extract(.,"price.*") %>%
                             str_extract(.,"[0-9]+\\.[0-9]+") %>% 
                             as.numeric()) * ETH_USD },
                       error = function(e){"Not For Sale"})
    
    if(identical(numeric(0),buy_now)){buy_now = "Not For Sale"}
    
    buys = data.frame(horse_id = c(horses_of_interest$horse_id[i]),cost = c(buy_now))
    cost = rbind(cost,buys)
}

cost %>% 
    filter(cost != "Not For Sale") %>%
    mutate(cost = round(as.numeric(cost),2)) %>%
    left_join(racing_data %>% select(horse_name,horse_id),
              by = c("horse_id"="horse_id")) %>%
    distinct() %>%
    select(horse_id,horse_name,cost) %>%
    left_join(filter_horses, by = c("horse_name"="horse_name")) %>%
    arrange(cost)
