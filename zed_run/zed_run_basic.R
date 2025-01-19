source("config.R")
pacman::p_load(httr,tidyverse,bigrquery,googledrive,googlesheets4,stats)
horse_data = read_csv(paste("https://zed-odds.netlify.app/horse_db.csv",sep="") )
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

horse_race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",Sys.Date()-1,"_horse_race_data.csv",sep="") ) ) %>% 
    mutate(odds = ifelse(is.na(odds),0,odds))
race_data = suppressMessages(read_csv(paste("https://zed-odds.netlify.app/daily/",Sys.Date()-1,"_race.csv",sep="") ))

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "gaeas-cradle", dataset = "zed_run", table = "horse_race")
bq_table_upload(x=mybq, values = horse_race_data, fields=as_bq_fields(horse_race_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

mybq <- bq_table(project = "gaeas-cradle", dataset = "zed_run", table = "race")
bq_table_upload(x=mybq, values = race_data, fields=as_bq_fields(race_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

mybq <- bq_table(project = "gaeas-cradle", dataset = "zed_run", table = "horse")
bq_table_upload(x=mybq, values = horse_data, fields=as_bq_fields(horse_data),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")

statement <- paste("SELECT DISTINCT * 
                   FROM `gaeas-cradle.zed_run.horse_race` zrhr  ",sep = "")
horse_race_all <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()

statement <- paste("SELECT DISTINCT * 
                   FROM `gaeas-cradle.zed_run.race` zrr  ",sep = "")
race_all <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()

statement <- paste("SELECT DISTINCT * 
                   FROM `gaeas-cradle.zed_run.horse` zrh  ",sep = "")
horse_all <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% distinct()

#race_all %>% arrange(start_time)

# csv upload --------------------------------------------------------------
horse_race_all <- rbind(read_csv("/home/cujo253/zed_run/horse_race_data_db.csv"),read_csv("/home/cujo253/zed_run/horse_race_data_db_before_08_01_21.csv")) %>% distinct() 
race_all <- read_csv("/home/cujo253/zed_run/race_db.csv") 
horse_all <- read_csv("/home/cujo253/zed_run/horse_db.csv") 

gc()

hra = race_all %>% 
    left_join(horse_race_all,  by=c("race_id"="race_id")) %>%
    left_join(horse_all,by=c("horse_id"="horse_id")) %>% distinct() %>%
    rename(race_name = name.x, horse_name = name.y)


con <- gaeas_cradle("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "gaeas-cradle", dataset = "zed_run", table = "all")
bq_table_upload(x=mybq, values = hra, fields=as_bq_fields(hra),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")


my_peggy = hra %>% filter(grepl("(^On the rails$|^Chandra$|^Hands and Heels$|^Granite Street Porter$|^Wooksie$|^Mystic Missile$|^Peggy$|^Luna Nueva$|^Over My Head$|^Royal Racer$|^Estelle$|^Tempting$|^Ace Lightning$|^Stolen Angel$|^Achille$|^Forever Fourth$|^FeFe Monza$|^Alfa Romeo$|^Ad Astra$|^on the rails$|^Vroum Vroum Mag$|^Comes and Goes$|^Dutch Mist$|^First Rule of Fight Club$|^Second Rule of Fight Club$|^Waning Euphoria$|^Plea$|^Adhira$| ^Granite Street Porter$|^Rampers$)",horse_name)) %>%
    group_by(horse_name,distance,class) %>% 
    mutate(first = ifelse(place == 1,1,0),
           First_3rd = ifelse(place <=3,1,0),
           Fourth_8th = ifelse((place >3 & place <9),1,0 ),
           Eighth_12th = ifelse(place>8,1,0),
           Twelfth = ifelse(place == 12,1,0)
           ) %>% 
    summarize(odds = round(1/mean(1/mean(odds,na.rm=T)),2),
              place = round(mean(place),2),
              races_run = n(),
              iqr = round(IQR(horse_time),2),
              sd = round(sd(horse_time,na.rm=T),2),
              med = round(median(horse_time),2),
              horse_time = round(mean(horse_time),2),
              first = sum(first),
              First_3rd = sum(First_3rd),
              Fourth_8th = sum(Fourth_8th),
              Eighth_12th = sum(Eighth_12th), 
              Twelfth = sum(Twelfth)
              ) %>%
    ungroup() %>%
    filter(odds != 0) 


# hra %>% filter(grepl("(^Peggy$|^Luna Nueva$|^Over My Head$|^Royal Racer$|^Estelle$|^Tempting$|^Ace Lightning$|^Stolen Angel$|^Achille$|^Forever Fourth$|^FeFe Monza$|^Alfa Romeo$|^Ad Astra$|^on the rails$|^Vroum Vroum Mag$|^Comes and Goes$|^Dutch Mist$|^First Rule of Fight Club$|^Second Rule of Fight Club$|^Waning Euphoria$|^Plea$|^Adhira$| ^Granite Street Porter$|^Rampers$)",horse_name)) %>%
#     group_by(horse_name,distance,class) %>%
#     summarize(iqr = IQR(horse_time))

all_values = hra %>% 
    group_by(distance,class) %>%
    summarize(ovr_odds = round(1/mean(1/mean(odds,na.rm=T)),2),
              ovr_races_run = n(),
              ovr_iqr = round(IQR(horse_time,na.rm=T),2),
              ovr_sd = round(sd(horse_time,na.rm=T),2),
              ovr_med = round(median(horse_time,na.rm=T),2),
              ovr_horse_time = round(mean(horse_time,na.rm=T),2)
    ) %>%
    ungroup() %>%
    filter(ovr_odds != 0) %>%
    mutate(q1 =  ovr_med - (1.5 *ovr_iqr),
           q3 = ovr_med + (1.5 *ovr_iqr)) 
# 
# my_peggy %>% left_join(all_values,by=c("distance"="distance","class"="class")) %>% arrange(horse_name,class,distance) %>%
#     filter(horse_name == "Peggy" ) 

our_horses = hra %>% filter(grepl("(^And On the Rails$|^Chandra$|^Hands and Heels$|^Granite Street Porter$|^Wooksie$|^Mystic Missile$|^Peggy$|^Luna Nueva$|^Over My Head$|^Royal Racer$|^Estelle$|^Tempting$|^Ace Lightning$|^Stolen Angel$|^Achille$|^Forever Fourth$|^FeFe Monza$|^Alfa Romeo$|^Ad Astra$|^on the rails$|^Vroum Vroum Mag$|^Comes and Goes$|^Dutch Mist$|^First Rule of Fight Club$|^Second Rule of Fight Club$|^Waning Euphoria$|^Plea$|^Adhira$| ^Granite Street Porter$|^Rampers$)",horse_name)) %>%
    mutate(prize = ifelse(place == 1,prize_pool_first,ifelse(place == 2,prize_pool_second,ifelse(place==3,prize_pool_third,0)))) %>%
    filter(is.na(gate)==F,is.na(race_name)==F) %>%
    select(horse_id,horse_name,race_id,race_name,start_time,class,distance,fee,prize,horse_time,place,odds) %>%
    arrange(desc(start_time))

stats = my_peggy %>% left_join(all_values,by=c("distance"="distance","class"="class")) %>% 
    mutate(avg_time = horse_time,
           sd = sd,
           min_exp = med - iqr,
           med_exp = med,
           max_exp = med + iqr,
           ovr_avg_time = ovr_horse_time,
           ovr_min_exp = q1,
           ovr_max_exp = q3) %>%
    select(horse_name,distance,class,races_run,odds,ovr_odds,
           avg_time, ovr_avg_time, sd,ovr_sd,min_exp,med_exp,
           max_exp,ovr_min_exp,ovr_med,ovr_max_exp,iqr,ovr_iqr,
           first, First_3rd, Fourth_8th,Eighth_12th,Twelfth) %>% 
    mutate(ratio_first = round(first/races_run,3),
           ratio_1st_3rd = round(First_3rd/races_run,3),
           ratio_4th_8th = round(Fourth_8th/races_run,3),
           ratio_9th_12th = round(Eighth_12th/races_run,3),
           ratio_12th = round(Twelfth/races_run,3),
           
           ) %>%
    arrange(horse_name,class,distance) %>% replace(is.na(.),0)


drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("zed")
sheet_write(our_horses, ss = ss,sheet = "Phil")
sheet_write(our_horses, ss = ss,sheet = "Greeno")
sheet_write(our_horses, ss = ss,sheet = "Sturm")
sheet_write(our_horses, ss = ss,sheet = "Chris")
sheet_write(stats, ss = ss, sheet = "stats")



# Class Breakdowns --------------------------------------------------------
classes = function(class = 5){
    class_grp = enquo(class)
    class_5 = hra %>% filter(class == !!class_grp) %>%
        group_by(horse_name,distance,class) %>% 
        mutate(first = ifelse(place == 1,1,0),
               First_3rd = ifelse(place <=3,1,0),
               Fourth_8th = ifelse((place >3 & place <9),1,0 ),
               Eighth_12th = ifelse(place>8,1,0),
               Twelfth = ifelse(place == 12,1,0)
        ) %>% 
        summarize(odds = round(1/mean(1/mean(odds,na.rm=T)),2),
                  place = round(mean(place),2),
                  races_run = n(),
                  iqr = round(IQR(horse_time,na.rm=T),2),
                  sd = round(sd(horse_time,na.rm=T),2),
                  med = round(median(horse_time),2),
                  horse_time = round(mean(horse_time),2),
                  first = sum(first),
                  First_3rd = sum(First_3rd),
                  Fourth_8th = sum(Fourth_8th),
                  Eighth_12th = sum(Eighth_12th), 
                  Twelfth = sum(Twelfth)
        ) %>%
        ungroup() %>%
        filter(odds != 0) 
    
    all_values = hra %>% 
        group_by(distance,class) %>%
        summarize(ovr_odds = round(1/mean(1/mean(odds,na.rm=T)),2),
                  ovr_races_run = n(),
                  ovr_iqr = round(IQR(horse_time,na.rm=T),2),
                  ovr_sd = round(sd(horse_time,na.rm=T),2),
                  ovr_med = round(median(horse_time,na.rm=T),2),
                  ovr_horse_time = round(mean(horse_time,na.rm=T),2)
        ) %>%
        ungroup() %>%
        filter(ovr_odds != 0) %>%
        mutate(q1 =  ovr_med - (1.5 *ovr_iqr),
               q3 = ovr_med + (1.5 *ovr_iqr)) 
    
    class_5_ovr = class_5 %>% left_join(all_values,by=c("distance"="distance","class"="class")) %>% 
        mutate(avg_time = horse_time,
               sd = sd,
               min_exp = med - iqr,
               med_exp = med,
               max_exp = med + iqr,
               ovr_avg_time = ovr_horse_time,
               ovr_min_exp = q1,
               ovr_max_exp = q3) %>%
        select(horse_name,distance,class,races_run,odds,ovr_odds,
               avg_time, ovr_avg_time, sd,ovr_sd,min_exp,med_exp,
               max_exp,ovr_min_exp,ovr_med,ovr_max_exp,iqr,ovr_iqr,
               first, First_3rd, Fourth_8th,Eighth_12th,Twelfth) %>% 
        mutate(ratio_first = round(first/races_run,3),
               ratio_1st_3rd = round(First_3rd/races_run,3),
               ratio_4th_8th = round(Fourth_8th/races_run,3),
               ratio_9th_12th = round(Eighth_12th/races_run,3),
               ratio_12th = round(Twelfth/races_run,3),
               
        ) %>%
        mutate(sd_diff = sd - ovr_sd,
              time_diff = avg_time - ovr_avg_time,
              sd_tier = ntile(sd_diff,100),
              time_tier = ntile(desc(time_diff),100)) 
    
    bonafide_c5_bail = class_5_ovr %>%
        mutate(sd_diff = sd - ovr_sd,
               time_diff = avg_time - ovr_avg_time,
               sd_tier = ntile(sd_diff,100),
               time_tier = ntile(desc(time_diff),100))  %>% 
        filter(races_run >= 100) %>%
        group_by(horse_name,class,distance) %>%
        summarize(races_run = races_run,
                  final_rank = round(mean(sd_tier,time_tier),4),
                  avg_time = avg_time,
                  ovr_avg_time = ovr_avg_time,
                  sd = sd,
                  ovr_sd = ovr_sd,
                  ratio_first = ratio_first,
                  ratio_1st_3rd = ratio_1st_3rd,
                  ratio_4th_8th = ratio_4th_8th,
                  ratio_9th_12th = ratio_9th_12th,
                  ratio_12th = ratio_12th) %>%
        ungroup() %>%
        arrange(distance,desc(final_rank),avg_time) 
    
    final_known = NULL
    for(i in unique(bonafide_c5_bail$distance)){
        temp = bonafide_c5_bail %>% filter(distance == i) %>%
            mutate(final_rank = seq(nrow(.)))
        
        final_known = rbind(final_known,temp)
    }
    
    up_and_comers_c5_bail = class_5_ovr  %>%
        mutate(sd_diff = sd - ovr_sd,
               time_diff = avg_time - ovr_avg_time,
               sd_tier = ntile(sd_diff,100),
               time_tier = ntile(desc(time_diff),100)) %>% 
        filter(races_run >= 30 & races_run <100) %>%
        group_by(horse_name,class,distance) %>%
        summarize(races_run = races_run,
                  final_rank = round(mean(sd_tier,time_tier),4),
                  avg_time = avg_time,
                  ovr_avg_time = ovr_avg_time,
                  sd = sd,
                  ovr_sd = ovr_sd,
                  ratio_first = ratio_first,
                  ratio_1st_3rd = ratio_1st_3rd,
                  ratio_4th_8th = ratio_4th_8th,
                  ratio_9th_12th = ratio_9th_12th,
                  ratio_12th = ratio_12th) %>%
        ungroup() %>%
        arrange(distance,desc(final_rank),avg_time) 
    
    final_middlemen = NULL
    for(i in unique(up_and_comers_c5_bail$distance)){
        temp = up_and_comers_c5_bail %>% filter(distance == i) %>%
            mutate(final_rank = seq(nrow(.)))
        
        final_middlemen = rbind(final_middlemen,temp)
    }
    
    rookies_c5_bail = class_5_ovr  %>%
        mutate(sd_diff = sd - ovr_sd,
               time_diff = avg_time - ovr_avg_time,
               sd_tier = ntile(sd_diff,100),
               time_tier = ntile(desc(time_diff),100)) %>% 
        filter(races_run >= 10 & races_run <30) %>%
        group_by(horse_name,class,distance) %>%
        summarize(races_run = races_run,
                  final_rank = round(mean(sd_tier,time_tier),4),
                  avg_time = avg_time,
                  ovr_avg_time = ovr_avg_time,
                  sd = sd,
                  ovr_sd = ovr_sd,
                  ratio_first = ratio_first,
                  ratio_1st_3rd = ratio_1st_3rd,
                  ratio_4th_8th = ratio_4th_8th,
                  ratio_9th_12th = ratio_9th_12th,
                  ratio_12th = ratio_12th) %>%
        ungroup() %>%
        arrange(distance,desc(final_rank),avg_time) 
    
    final_rookies = NULL
    for(i in unique(rookies_c5_bail$distance)){
        temp = rookies_c5_bail %>% filter(distance == i) %>%
            mutate(final_rank = seq(nrow(.)))
        
        final_rookies = rbind(final_rookies,temp)
    }
    
    all_c5_bail = class_5_ovr  %>%
        mutate(sd_diff = sd - ovr_sd,
               time_diff = avg_time - ovr_avg_time,
               sd_tier = ntile(sd_diff,100),
               time_tier = ntile(desc(time_diff),100)) %>% 
        #filter(races_run >= 10 & races_run <30) %>%
        group_by(horse_name,class,distance) %>%
        summarize(races_run = races_run,
                  final_rank = round(mean(sd_tier,time_tier),4),
                  avg_time = avg_time,
                  ovr_avg_time = ovr_avg_time,
                  sd = sd,
                  ovr_sd = ovr_sd,
                  ratio_first = ratio_first,
                  ratio_1st_3rd = ratio_1st_3rd,
                  ratio_4th_8th = ratio_4th_8th,
                  ratio_9th_12th = ratio_9th_12th,
                  ratio_12th = ratio_12th) %>%
        ungroup() %>%
        arrange(distance,desc(final_rank),avg_time) 
    
    final_all = NULL
    for(i in unique(all_c5_bail$distance)){
        temp = all_c5_bail %>% filter(distance == i) %>%
            mutate(final_rank = seq(nrow(.)))
        
        final_all = rbind(final_all,temp)
    }
    
    drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
    gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
    ss <- drive_get(paste("zed_class_",class,sep=""))
    sheet_write(final_known, ss = ss,sheet = paste("C",class," 100+ Races",sep=""))
    sheet_write(final_middlemen, ss = ss,sheet = paste("C",class," 30-99 Races",sep=""))
    sheet_write(final_rookies, ss = ss,sheet = paste("C",class," 10 Races",sep=""))
    sheet_write(final_all, ss = ss,sheet = paste("C",class," All Horses",sep=""))
    
    }

classes(5)
classes(4)
classes(3)
classes(2)
classes(1)

specific_class = function(class =5){
    class_grp = enquo(class)
    class_5 = hra %>% filter(class == !!class_grp) %>%
        group_by(horse_name,distance,class) %>% 
        mutate(first = ifelse(place == 1,1,0),
               First_3rd = ifelse(place <=3,1,0),
               Fourth_8th = ifelse((place >3 & place <9),1,0 ),
               Eighth_12th = ifelse(place>8,1,0),
               Twelfth = ifelse(place == 12,1,0)
        ) %>% 
        summarize(odds = round(1/mean(1/mean(odds,na.rm=T)),2),
                  place = round(mean(place),2),
                  races_run = n(),
                  iqr = round(IQR(horse_time,na.rm=T),2),
                  sd = round(sd(horse_time,na.rm=T),2),
                  med = round(median(horse_time),2),
                  horse_time = round(mean(horse_time),2),
                  first = sum(first),
                  First_3rd = sum(First_3rd),
                  Fourth_8th = sum(Fourth_8th),
                  Eighth_12th = sum(Eighth_12th), 
                  Twelfth = sum(Twelfth)
        ) %>%
        ungroup() %>%
        filter(odds != 0) 
    
    all_values = hra %>% 
        group_by(distance,class) %>%
        summarize(ovr_odds = round(1/mean(1/mean(odds,na.rm=T)),2),
                  ovr_races_run = n(),
                  ovr_iqr = round(IQR(horse_time,na.rm=T),2),
                  ovr_sd = round(sd(horse_time,na.rm=T),2),
                  ovr_med = round(median(horse_time,na.rm=T),2),
                  ovr_horse_time = round(mean(horse_time,na.rm=T),2)
        ) %>%
        ungroup() %>%
        filter(ovr_odds != 0) %>%
        mutate(q1 =  ovr_med - (1.5 *ovr_iqr),
               q3 = ovr_med + (1.5 *ovr_iqr)) 
    
    class_5_ovr = class_5 %>% left_join(all_values,by=c("distance"="distance","class"="class")) %>% 
        mutate(avg_time = horse_time,
               sd = sd,
               min_exp = med - iqr,
               med_exp = med,
               max_exp = med + iqr,
               ovr_avg_time = ovr_horse_time,
               ovr_min_exp = q1,
               ovr_max_exp = q3) %>%
        select(horse_name,distance,class,races_run,odds,ovr_odds,
               avg_time, ovr_avg_time, sd,ovr_sd,min_exp,med_exp,
               max_exp,ovr_min_exp,ovr_med,ovr_max_exp,iqr,ovr_iqr,
               first, First_3rd, Fourth_8th,Eighth_12th,Twelfth) %>% 
        mutate(ratio_first = round(first/races_run,3),
               ratio_1st_3rd = round(First_3rd/races_run,3),
               ratio_4th_8th = round(Fourth_8th/races_run,3),
               ratio_9th_12th = round(Eighth_12th/races_run,3),
               ratio_12th = round(Twelfth/races_run,3),
               
        ) %>%
        mutate(sd_diff = sd - ovr_sd,
               time_diff = avg_time - ovr_avg_time,
               sd_tier = ntile(sd_diff,100),
               time_tier = ntile(desc(time_diff),100)) 
    
    bonafide_c5_bail = class_5_ovr %>%
        mutate(sd_diff = sd - ovr_sd,
               time_diff = avg_time - ovr_avg_time,
               sd_tier = ntile(sd_diff,100),
               time_tier = ntile(desc(time_diff),100))  %>% 
        filter(races_run >= 15) %>%
        group_by(horse_name,class,distance) %>%
        summarize(races_run = races_run,
                  final_rank = round(mean(sd_tier,time_tier),4),
                  avg_time = avg_time,
                  ovr_avg_time = ovr_avg_time,
                  sd = sd,
                  ovr_sd = ovr_sd,
                  ratio_first = ratio_first,
                  ratio_1st_3rd = ratio_1st_3rd,
                  ratio_4th_8th = ratio_4th_8th,
                  ratio_9th_12th = ratio_9th_12th,
                  ratio_12th = ratio_12th) %>%
        ungroup() %>%
        arrange(distance,desc(final_rank),avg_time) 
    
    final_known = NULL
    for(i in unique(bonafide_c5_bail$distance)){
        temp = bonafide_c5_bail %>% filter(distance == i) %>%
            mutate(final_rank = seq(nrow(.)))
        
        final_known = rbind(final_known,temp)
    }
    
    return(final_known)
}

class_values = rbind(specific_class(5),
                     specific_class(4),
                     specific_class(3),
                     specific_class(2),
                     specific_class(1))

