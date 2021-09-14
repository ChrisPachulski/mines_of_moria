pacman::p_load(httr,tidyverse,bigrquery,googledrive,googlesheets4,stats)

gaeas_cradle <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
}

# Retrieve racing data, BQ or base zip files
tryCatch({
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    statement = "SELECT * FROM `gaeas-cradle.zed_run.all`"
    racing_data = dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)
}, 
error = function(e){
    
    horse_race_all <- rbind(read_csv("/home/cujo253/mines_of_moria/zed_run/horse_race_data_db.csv"),read_csv("/home/cujo253/mines_of_moria/zed_run/horse_race_data_db_before_08_01_21.csv")) %>% distinct() 
    race_all <- read_csv("/home/cujo253/mines_of_moria/zed_run/race_db.csv") 
    horse_all <- read_csv("/home/cujo253/mines_of_moria/zed_run/horse_db.csv") 
    
    gc()
    
    racing_data = race_all %>% 
        left_join(horse_race_all,  by=c("race_id"="race_id")) %>%
        left_join(horse_all,by=c("horse_id"="horse_id")) %>% distinct() %>%
        rename(race_name = name.x, horse_name = name.y)
    
    gc()
    
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    mybq <- bq_table(project = "gaeas-cradle", dataset = "zed_run", table = "all")
    bq_table_upload(x=mybq, values = racing_data, fields=as_bq_fields(racing_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
    
    gc()
})

existing_date = racing_data %>% 
    select(start_time) %>% 
    filter(start_time == max(start_time)) %>% 
    distinct() %>%
    mutate(start_time = floor_date(start_time,"day")) %>%
    .[[1]]

days_since_update = as.numeric(Sys.Date() - as.Date(existing_date))
days_since_update = 5
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
}


baseline_stats = racing_data %>%
    group_by(distance) %>%
    summarize(
        count = n(),
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
colnames(baseline_stats) = paste("base",colnames(baseline_stats),sep = "_")

Peggy = racing_data %>%
    filter(horse_name == "Summer Solstice") %>%
    group_by(horse_name,distance) %>%
    summarize(
        count = n(),
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

Peggy %>%
    left_join(baseline_stats,by=c("distance"="base_distance")) %>%
    group_by(horse_name,distance) %>%
    summarize(
        count = count,
        Base_Speed = scales::percent((((Mean - base_Mean)/base_SD)*-1)),
        SD_Delta = scales::percent(((SD-base_SD) / base_SD)),
        Max_Speed = scales::percent((((Mean-(2*SD))-(base_Mean-(2*base_SD)))/base_SD)*-1),
        Z_Top = round((`base_25%`-`25%`)/`base_25%`,2),
        Z_Bottom = round((`base_75%`-`75%`)/`base_75%`,2),
        Skew = Z_Top - Z_Bottom
        
    ) %>% 
    arrange(distance)


# By Class ----------------------------------------------------------------


baseline_stats = racing_data %>%
    group_by(class,distance) %>%
    summarize(
        count = n(),
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
colnames(baseline_stats) = paste("base",colnames(baseline_stats),sep = "_")

Peggy = racing_data %>%
    filter(horse_name == "Peggy") %>%
    group_by(horse_name,class,distance) %>%
    summarize(
        count = n(),
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

Peggy %>%
    left_join(baseline_stats,by=c("class"="base_class","distance"="base_distance")) %>%
    filter(class == 5) %>%
    group_by(horse_name,class,distance) %>%
    summarize(
        count = count,
        Base_Speed = (((Mean - base_Mean)/base_SD)*-1),
        SD_Delta = ((SD-base_SD) / base_SD),
        Max_Speed = (((Mean-(2*SD))-(base_Mean-(2*base_SD)))/base_SD)*-1,
        Z_Top = round((`base_25%`-`25%`)/`base_25%`,2),
        Z_Bottom = round((`base_75%`-`75%`)/`base_75%`,2),
        Skew = Z_Top - Z_Bottom
        
    ) %>% 
    ungroup() %>%
    arrange(distance)
