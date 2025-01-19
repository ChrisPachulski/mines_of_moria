source("config.R")
#Packages & Functions####
library(tidyverse)
library(devtools)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(gargle)
library(httr)
library(RMySQL)
library(bigrquery)
library(jsonlite)

delayed_newspaper <- function(ip){
    con = dbConnect(MySQL(), user='remote', password='zachIsTheBest404!!', dbname ='three_day_newspaper',host=ip)
    con
}
current_newspaper <- function(ip){
    con = dbConnect(MySQL(), user='remote', password='zachIsTheBest404!!', dbname ='newspaper',host=ip)
    con
}
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
#3 Day lag####
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gc()
#drive_create("TCG_Review")
ss <- drive_get("Sets")

Sets <- read_sheet(ss,"Sets") %>% mutate_if(is.character,as.factor)
#View(Sets)
ck_conversion <- read_sheet(ss,"mtgjson_ck_sets")
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


Three_Day_Lag <- gsub("-","_",Sys.Date()-2)

statement <- paste("SELECT * ","FROM `gaeas-cradle.premiums.",Three_Day_Lag,"*` a ",sep = "")
Premium <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!Date)

statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Foil_Status as `F_NF`, a.BL_QTY,a.BL,a.TCG_MKT,a.CK_MKT,a.MKT_Diff,a.Sellers,a.CK_Backing,a.TCG_Backing,p.TCG_Rank,p.CK_ADJ_Rank ",
                   "FROM `gaeas-cradle.ck_funny_money.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   " LEFT JOIN `gaeas-cradle.premiums.",Three_Day_Lag,"*` p  on r.Key = p.Key",
                   sep = "")
Funny_Money <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as Foil, a.Todays_BL,a.Yesterday_BL,a.Week_Ago_BL,a.Month_Ago_BL,a.Yesterday_BL_Chg,a.Week_Ago_BL_Chg,a.Month_Ago_BL_Chg,a.Buylist_Backing", 
                   " FROM `gaeas-cradle.buylist_growth.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   " WHERE Todays_BL is not NULL and Yesterday_BL is not NULL and Week_Ago_BL is not NULL and Month_Ago_BL is not NULL and Yesterday_BL_Chg is not NULL and Week_Ago_BL_Chg is not NULL and Month_Ago_BL_Chg is not NULL",
                   sep = "")
Buylist <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_TCG,a.Yesterday_TCG,a.Week_Ago_TCG,a.Month_Ago_TCG,a.Yesterday_TCG_Chg,a.Week_Ago_TCG_Chg,a.Month_Ago_TCG_Chg ", 
                   "FROM `gaeas-cradle.demand_growth.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   sep = "")
Demand <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_Sellers,a.Yesterday_Sellers,a.Week_Ago_Sellers,a.Month_Ago_Sellers,a.Yesterday_Sellers_Chg,a.Week_Ago_Sellers_Chg,a.Month_Ago_Sellers_Chg ", 
                   "FROM `gaeas-cradle.vendor_growth.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   sep = "")
Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT rtrim(a.Key) as Key, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as `F_NF`,a.Ranking, p.MKT as Retail,p.BL as Buylist, Sellers ", 
                   "FROM `gaeas-cradle.kpi.",Three_Day_Lag,"*` a ", 
                   " LEFT JOIN roster.mtgjson r on r.Key = rtrim(a.Key)",
                   " LEFT JOIN `gaeas-cradle.premiums.",Three_Day_Lag,"*` p  on r.Key = rtrim(p.Key)",
                   " ORDER BY Ranking", sep = "")
KPI_Master <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!Ranking) %>% distinct() 
colnames(KPI_Master)[5] <- "F/NF"
KPI_Master <- KPI_Master %>% mutate(Ranking = seq(nrow(KPI_Master)))

statement <- paste("SELECT * ", "FROM `gaeas-cradle.ck_velocity.",Three_Day_Lag,"*` a ","Limit 5000 ", sep = "")
CK_Velocity <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!meta_created_at)

Four_Day_Lag <- gsub("-","_",Sys.Date()-4)

statement <- paste('SELECT *, CASE WHEN Classification = "S" then "A" 
WHEN Classification = "A" then "B" 
WHEN Classification = "B" then "C" 
WHEN Classification = "C" then "D" 
WHEN Classification = "D" then "E" 
WHEN Classification = "E" then "F" 
WHEN Classification = "F" then "G" 
WHEN Classification = "Ignore" then "H"
END as custom_sort ', "FROM `gaeas-cradle.ensemble_forecast_results.",Four_Day_Lag,"_ENSEMBLE` a ", "ORDER BY custom_sort ",sep = "")

Ensemble_Forecast <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) 

Ensemble_Forecast_upload = Ensemble_Forecast %>% rename(Card = name, 
                                                        Recent_BL = current_val, 
                                                        Historical_plus_minus = iqr,
                                                        Historical_Median = median_val, 
                                                        Historical_Max = outer_lim,
                                                        Forecasted_BL = max_forecast_value,
                                                        Forecast_plus_minus = plus_minus,
                                                        Target_Date = Date,
                                                        Tier = Classification,
                                                        Behavior = Safety) %>%
    select(-sd) %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)],
                           number = Updated_Tracking_Keys$number[match(uuid,Updated_Tracking_Keys$uuid)]) %>%
    #left_join(Updated_Tracking_Keys %>% select(Key,uuid),by = c("Key"="Key")) %>%
    select(uuid,Key,Card,Set,number, everything()) %>% distinct() %>% select(-Rarity)


statement <- paste(
    'With t1 as (
SELECT DISTINCT Key, name, a.Set, Rarity, current_val,max_forecast_value, Classification, 
CASE WHEN Classification = "S" then "A" 
WHEN Classification = "A" then "B" 
WHEN Classification = "B" then "C" 
WHEN Classification = "C" then "D" 
WHEN Classification = "D" then "E" 
WHEN Classification = "E" then "F" 
WHEN Classification = "F" then "G" 
WHEN Classification = "Ignore" then "H" 
END as custom_sort 
FROM `gaeas-cradle.ensemble_forecast_results.*` a 
WHERE Date = CURRENT_DATE() AND 
 _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 33 DAY)) AND 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -3 DAY)) 
ORDER BY custom_sort), 
t2 as 
(SELECT Key, BL as current_val, Date  
FROM `gaeas-cradle.ck_funny_money.*` a  
WHERE  _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 3 DAY)) AND 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -2 DAY)) AND 
  Date = "2022-02-07"
), 
t3 as ( 
SELECT t1.Key, count(*) as key_count 
FROM t1 
LEFT JOIN t2 on t1.Key = t2.Key 
WHERE t2.Date = CURRENT_DATE() - 3
GROUP BY 1 
) 
SELECT t1.Key,t1.name,t1.Set,t1.Rarity,t1.current_val original_bl,t1.max_forecast_value,t2.current_val,t1.Classification, t1.custom_sort, round(abs((max_forecast_value - t2.current_val)/max_forecast_value),2) accuracy_metric, key_count 
FROM t1 
LEFT JOIN t2 on t1.Key = t2.Key 
LEFT JOIN t3 on t1.Key = t3.Key 
WHERE t2.current_val is not null AND 
max_forecast_value != t1.current_val
ORDER BY custom_sort, accuracy_metric '
    ,sep=" ")

Ensemble_Forecast_Performance <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

ensemble_performance_export = Ensemble_Forecast_Performance %>% 
  mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)],
         number = Updated_Tracking_Keys$number[match(uuid,Updated_Tracking_Keys$uuid)]) %>%
  select(uuid,Key,name,Set,number, everything()) %>% distinct() %>% select(-Rarity) %>% group_by(uuid,Key) %>% 
  summarize(
    uuid = uuid,
    Key = Key,
    Card = name,
    Set = Set,
    number = number,
    original_bl = min(original_bl),
    max_forecast_value = min(max_forecast_value),
    current_val = current_val,
    classification = min(Classification),
    custom_sort = min(custom_sort),
    accuracy_metric = round(abs((max_forecast_value - current_val)/max_forecast_value),2),
    ct = n()) %>%
  ungroup() %>% distinct() %>% #filter(accuracy_metric <= .10) %>%
  arrange(custom_sort,accuracy_metric) %>%
  filter(ct >= 2)

# drive_auth(email = "pachun95@gmail.com", use_oob = T)
# gs4_auth(email = "pachun95@gmail.com", use_oob = T)
# ss <- drive_get("Three_Day_Lag")
# sheet_write(Premium,
#             ss = ss,
#             sheet = "Premium")
# Sys.sleep(5)
# sheet_write(Funny_Money,
#             ss = ss,
#             sheet = "Funny_Money")
# Sys.sleep(5)
# sheet_write(Buylist,
#             ss = ss,
#             sheet = "Buylist")
# Sys.sleep(5)
# sheet_write(Demand,
#             ss = ss,
#             sheet = "Demand")
# Sys.sleep(5)
# sheet_write(Vendor,
#             ss = ss,
#             sheet = "Vendor")
# Sys.sleep(5)
# sheet_write(KPI_Master,
#             ss = ss,
#             sheet = "KPI_Master")
# Sys.sleep(5)
# sheet_write(data.frame(CK_Velocity),
#             ss = ss,
#             sheet = "CK_Velocity")

#BAN Server Upload for Lagged Data####
con <- delayed_newspaper("157.245.255.185")


dbListTables(con)
dbGetInfo(con)

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("Three_Day_Lag")
KPI_Table <- KPI_Master %>% filter(`F/NF` == "") %>% 
    mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)])

KPI_Table <- KPI_Table %>% mutate(Ranking = seq(nrow(KPI_Table)))  %>%
    select(uuid,Ranking,Retail,Buylist,Sellers)
colnames(KPI_Table) <- c("uuid","Ranking","Retail","Buylist","Vendors")


uuid_check = KPI_Table %>% group_by(uuid) %>% summarize(ct = n()) 

kpi_replacements = KPI_Table %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
  filter(ct >= 2) %>% select(-ct) %>% group_by(uuid) %>%
  summarize(Ranking = min(Ranking),
            Retail = mean(Retail),
            Buylist = min(Buylist),
            Vendors = max(Vendors)) %>%
  ungroup()

KPI_Table = KPI_Table %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
  filter(ct < 2) %>% select(-ct) %>%
  rbind(kpi_replacements) %>% 
  arrange(Ranking) %>%
  mutate(Ranking = seq(nrow(.)))

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = KPI_Table, name ='top_25')


dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = KPI_Table, name ='top_25')

#vendor_levels %>% filter(is.na(Week_Ago_Sellers)==F)

vendor_levels <- Vendor %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(uuid,Todays_Sellers,Yesterday_Sellers,Week_Ago_Sellers, Month_Ago_Sellers,Yesterday_Sellers_Chg,Week_Ago_Sellers_Chg,Month_Ago_Sellers_Chg)
#vendor_levels$uuid <- Updated_Tracking_Keys$uuid[match(vendor_levels$Unique_Keys,Updated_Tracking_Keys$Key)]
#vendor_levels %>% filter(is.na(Week_Ago_Sellers)==F)
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_levels, name ='vendor_levels')


BL_levels <- Buylist %>% filter(!is.na(Foil)) %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(!c("Unique_Keys","Name","Set","Rarity","Foil","Buylist_Backing")) %>% select(uuid,everything()) %>% 
  filter(Todays_BL >= 1, Week_Ago_BL_Chg <= 3)

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = BL_levels, name ='buylist_levels')

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = Ensemble_Forecast_upload, name ='ensemble_forecast')
#dbCreateTable(conn=con,"ensemble_forecast",Ensemble_Forecast_upload, temporary = F)
#dbRemoveTable(conn=con, "ensemble_forecast")

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = ensemble_performance_export, name ='ensemble_performance')
#dbCreateTable(conn=con,"ensemble_performance",ensemble_performance_export, temporary = F)
#dbRemoveTable(conn=con, "ensemble_performance")

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = Updated_Tracking_Keys, name ='mtgjson_portable')

#Current Data Upload####
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
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

currentDate <- gsub("-","_",Sys.Date()-1)

statement <- paste("SELECT * ","FROM `gaeas-cradle.premiums.",currentDate,"*` a ",sep = "")
Premium <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!Date)

statement <- paste("SELECT DISTINCT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Foil_Status as `F_NF`, a.BL_QTY,a.BL,a.TCG_MKT,a.CK_MKT,a.MKT_Diff,a.Sellers,a.CK_Backing,a.TCG_Backing,p.TCG_Rank,p.CK_ADJ_Rank ",
                   "FROM `gaeas-cradle.ck_funny_money.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   " LEFT JOIN `gaeas-cradle.premiums.",currentDate,"*` p  on r.Key = p.Key",
                   sep = "")
Funny_Money <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as Foil, a.Todays_BL,a.Yesterday_BL,a.Week_Ago_BL,a.Month_Ago_BL,a.Yesterday_BL_Chg,a.Week_Ago_BL_Chg,a.Month_Ago_BL_Chg,a.Buylist_Backing", 
                   " FROM `gaeas-cradle.buylist_growth.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = rtrim(a.Unique_Keys)",
                   " WHERE Todays_BL is not NULL and Yesterday_BL is not NULL and Week_Ago_BL is not NULL and Month_Ago_BL is not NULL and Yesterday_BL_Chg is not NULL and Week_Ago_BL_Chg is not NULL and Month_Ago_BL_Chg is not NULL",
                   sep = "")
Buylist <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT a.Key, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_TCG,a.Yesterday_TCG,a.Week_Ago_TCG,a.Month_Ago_TCG,a.Yesterday_TCG_Chg,a.Week_Ago_TCG_Chg,a.Month_Ago_TCG_Chg ", 
                   "FROM `gaeas-cradle.demand_growth.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   sep = "")
Demand <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_Sellers,a.Yesterday_Sellers,a.Week_Ago_Sellers,a.Month_Ago_Sellers,a.Yesterday_Sellers_Chg,a.Week_Ago_Sellers_Chg,a.Month_Ago_Sellers_Chg ", 
                   "FROM `gaeas-cradle.vendor_growth.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   sep = "")
Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT DISTINCT rtrim(a.Key) as Key, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as `F_NF`,a.Ranking, p.MKT as Retail,p.BL as Buylist, p.Sellers ", 
                   "FROM `gaeas-cradle.kpi.",currentDate,"*` a ", 
                   " LEFT JOIN roster.mtgjson r on r.Key = rtrim(a.Key)",
                   " LEFT JOIN `gaeas-cradle.premiums.",currentDate,"*` p  on r.Key = rtrim(p.Key)",
                   " WHERE p.MKT is not null",
                   " ORDER BY Ranking", sep = "")
KPI_Master <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!Ranking) %>% distinct() 
colnames(KPI_Master)[5] <- "F/NF"
KPI_Master <- KPI_Master %>% mutate(Ranking = seq(nrow(KPI_Master)))

statement <- paste("SELECT * ", "FROM `gaeas-cradle.ck_velocity.",currentDate,"*` a ","Limit 5000 " ,sep = "")
CK_Velocity <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!meta_created_at)

Second_Day_Lag <- gsub("-","_",(Sys.Date()-1))

statement <- paste('SELECT *, CASE WHEN Classification = "S" then "A" 
WHEN Classification = "A" then "B" 
WHEN Classification = "B" then "C" 
WHEN Classification = "C" then "D" 
WHEN Classification = "D" then "E" 
WHEN Classification = "E" then "F" 
WHEN Classification = "F" then "G" 
WHEN Classification = "Ignore" then "H"
END as custom_sort ', "FROM `gaeas-cradle.ensemble_forecast_results.",Second_Day_Lag,"_ENSEMBLE` a ", "ORDER BY custom_sort ",sep = "")
Ensemble_Forecast <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) #%>% select(-custom_sort)

Ensemble_Forecast_upload = Ensemble_Forecast %>% rename(Card = name, 
                                                        Recent_BL = current_val, 
                                                        Historical_plus_minus = iqr,
                                                        Historical_Median = median_val, 
                                                        Historical_Max = outer_lim,
                                                        Forecasted_BL = max_forecast_value,
                                                        Forecast_plus_minus = plus_minus,
                                                        Target_Date = Date,
                                                        Tier = Classification,
                                                        Behavior = Safety) %>%
    select(-sd) %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)],
                           number = Updated_Tracking_Keys$number[match(uuid,Updated_Tracking_Keys$uuid)]) %>%
    select(uuid,Key,Card,Set,number, everything()) %>% distinct() %>% select(-Rarity)

#Ensemble_Forecast_upload %>% view()

statement <- paste(
    'With t1 as (
SELECT DISTINCT Key, name, a.Set, Rarity, current_val,max_forecast_value, Classification, 
CASE WHEN Classification = "S" then "A" 
WHEN Classification = "A" then "B" 
WHEN Classification = "B" then "C" 
WHEN Classification = "C" then "D" 
WHEN Classification = "D" then "E" 
WHEN Classification = "E" then "F" 
WHEN Classification = "F" then "G" 
WHEN Classification = "Ignore" then "H" 
END as custom_sort 
FROM `gaeas-cradle.ensemble_forecast_results.*` a 
WHERE Date = CURRENT_DATE() AND 
 _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY)) AND 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 0 DAY)) 
ORDER BY custom_sort), 
t2 as 
(SELECT Key, BL as current_val  
FROM `gaeas-cradle.ck_funny_money.*` a  
WHERE Date = CURRENT_DATE()-1 AND 
 _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) AND 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 0 DAY)) 
), 
t3 as ( 
SELECT t1.Key, count(*) as key_count 
FROM t1 
LEFT JOIN t2 on t1.Key = t2.Key 
GROUP BY 1 
) 
SELECT t1.Key,t1.name,t1.Set,t1.Rarity,t1.current_val original_bl,t1.max_forecast_value,t2.current_val,t1.Classification, t1.custom_sort, round(abs((max_forecast_value - t2.current_val)/max_forecast_value),2) accuracy_metric, key_count 
FROM t1 
LEFT JOIN t2 on t1.Key = t2.Key 
LEFT JOIN t3 on t1.Key = t3.Key 
WHERE t2.current_val is not null and 
max_forecast_value != t1.current_val 
ORDER BY custom_sort, accuracy_metric '
    ,sep=" ")

Ensemble_Forecast_Performance <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) 

ensemble_performance_export = Ensemble_Forecast_Performance %>% 
    mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)],
           number = Updated_Tracking_Keys$number[match(uuid,Updated_Tracking_Keys$uuid)]) %>%
    select(uuid,Key,name,Set,number, everything()) %>% distinct() %>% select(-Rarity) %>% group_by(uuid,Key) %>% 
    summarize(
        uuid = uuid,
        Key = Key,
        Card = name,
        Set = Set,
        number = number,
        original_bl = min(original_bl),
        max_forecast_value = min(max_forecast_value),
        current_val = current_val,
        classification = min(Classification),
        custom_sort = min(custom_sort),
        accuracy_metric = round(abs((max_forecast_value - current_val)/max_forecast_value),2),
        ct = n()) %>%
    ungroup() %>% distinct() %>% #filter(accuracy_metric <= .10) %>%
    arrange(custom_sort,accuracy_metric) %>% 
  filter(ct >= 2)


#Ensemble_Forecast_upload %>% view()

# drive_auth(email = "pachun95@gmail.com", use_oob = T)
# gs4_auth(email = "pachun95@gmail.com", use_oob = T)
# ss <- drive_get("currentDate")
# sheet_write(Premium,
#             ss = ss,
#             sheet = "Premium")
# Sys.sleep(5)
# sheet_write(Funny_Money,
#             ss = ss,
#             sheet = "Funny_Money")
# Sys.sleep(5)
# sheet_write(Buylist,
#             ss = ss,
#             sheet = "Buylist")
# Sys.sleep(5)
# sheet_write(Demand,
#             ss = ss,
#             sheet = "Demand")
# Sys.sleep(5)
# sheet_write(Vendor,
#             ss = ss,
#             sheet = "Vendor")
# Sys.sleep(5)
# sheet_write(KPI_Master,
#             ss = ss,
#             sheet = "KPI_Master")
# Sys.sleep(5)
# sheet_write(data.frame(CK_Velocity),
#             ss = ss,
#             sheet = "CK_Velocity")

#BAN Server Upload for Current Data####
library(RMySQL)

con <- current_newspaper("157.245.255.185")

dbListTables(con)
dbGetInfo(con)

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("currentDate")
KPI_Table <- KPI_Master %>% filter(`F/NF` == "") %>% 
    mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)])
KPI_Table <- KPI_Table %>% mutate(Ranking = seq(nrow(KPI_Table)))  %>%
    select(uuid,Ranking,Retail,Buylist,Sellers)
colnames(KPI_Table) <- c("uuid","Ranking","Retail","Buylist","Vendors")

uuid_check = KPI_Table %>% group_by(uuid) %>% summarize(ct = n()) 

kpi_replacements = KPI_Table %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
  filter(ct >= 2) %>% select(-ct) %>% group_by(uuid) %>%
  summarize(Ranking = min(Ranking),
            Retail = mean(Retail),
            Buylist = min(Buylist),
            Vendors = max(Vendors)) %>%
  ungroup()

KPI_Table = KPI_Table %>% left_join(uuid_check, by=c("uuid"="uuid")) %>% 
  filter(ct < 2) %>% select(-ct) %>%
  rbind(kpi_replacements) %>% 
  arrange(Ranking) %>%
  mutate(Ranking = seq(nrow(.)))

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = KPI_Table, name ='top_25')


vendor_levels <- Vendor %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(uuid,Todays_Sellers,Yesterday_Sellers,Week_Ago_Sellers, Month_Ago_Sellers,Yesterday_Sellers_Chg,Week_Ago_Sellers_Chg,Month_Ago_Sellers_Chg) %>% filter(Todays_Sellers >= 0)
#vendor_levels$uuid <- Updated_Tracking_Keys$uuid[match(vendor_levels$Unique_Keys,Updated_Tracking_Keys$Key)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_levels, name ='vendor_levels')


BL_levels <- Buylist %>% filter(!is.na(Foil)) #%>% select(-Buylist_Backing)
BL_levels <- BL_levels %>% mutate(Unique_Keys = trimws(Unique_Keys)) %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>% filter(!is.na(uuid)) %>%
    select(!c("Unique_Keys","Name","Set","Rarity","Foil","Buylist_Backing")) %>% select(uuid,everything()) %>% 
  filter(Todays_BL >= 1, Week_Ago_BL_Chg <= 3)

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = BL_levels, name ='buylist_levels')

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = Ensemble_Forecast_upload, name ='ensemble_forecast')
#dbCreateTable(conn=con,"ensemble_forecast",Ensemble_Forecast_upload, temporary = F)
#dbRemoveTable(conn=con, "ensemble_forecast")

dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = ensemble_performance_export, name ='ensemble_performance')
#dbCreateTable(conn=con,"ensemble_performance",ensemble_performance_export, temporary = F)
#dbRemoveTable(conn=con, "ensemble_performance")


dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = Updated_Tracking_Keys, name ='mtgjson_portable')

#dbCreateTable(conn=con,"mtgjson_portable",Updated_Tracking_Keys, temporary = F)
#dbRemoveTable(conn=con, "mtgjson_portable")

# ss <- drive_get("Vendor_Time_Series_Results")
# vendor_decline_forecast <- range_read(ss, sheet = "Decline")
# vendor_decline_forecast$`F/NF`[is.na(vendor_decline_forecast$`F/NF`)] <- ""
# vendor_decline_forecast <- vendor_decline_forecast[which(vendor_decline_forecast$`F/NF` == ""),]
# vendor_decline_forecast$uuid <- Updated_Tracking_Keys$uuid[match(vendor_decline_forecast$Key,Updated_Tracking_Keys$Key)]
# 
# vendor_decline_forecast <- vendor_decline_forecast[-c(1:5)]
# vendor_decline_forecast <- vendor_decline_forecast[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
# dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_decline_forecast, name ='vendor_decline_forecast')
# 
# 
# ss <- drive_get("Time_Series_Results")
# bl_decline_forecast <- range_read(ss, sheet = "Growth")
# bl_decline_forecast$`F/NF`[is.na(bl_decline_forecast$`F/NF`)] <- ""
# bl_decline_forecast <- bl_decline_forecast[which(bl_decline_forecast$`F/NF` == ""),]
# bl_decline_forecast$uuid <- Updated_Tracking_Keys$uuid[match(bl_decline_forecast$Key,Updated_Tracking_Keys$Key)]
# bl_decline_forecast <- bl_decline_forecast[-c(1:5)]
# bl_decline_forecast <- bl_decline_forecast[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
# dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = bl_decline_forecast, name ='bl_growth_forecast')
# 
# 
# ss <- drive_get("TS_Delay")
# vendor_perform <- range_read(ss, sheet = "Vendor_List")
# vendor_perform$`F/NF`[is.na(vendor_perform$`F/NF`)] <- ""
# vendor_perform <- vendor_perform[which(vendor_perform$`F/NF` == ""),]
# vendor_perform$uuid <- Updated_Tracking_Keys$uuid[match(vendor_perform$Key,Updated_Tracking_Keys$Key)]
# vendor_perform <- vendor_perform[which(vendor_perform$Weeks_Ago <= 175),]
# vendor_perform <- vendor_perform[-c(1:5)]
# vendor_perform <- vendor_perform[c(8,1,2,3,4,5,6,7)]
# dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_perform, name ='vendor_forecast_perform')
# 
# 
# ss <- drive_get("TS_Delay")
# bl_perform <- range_read(ss, sheet = "BL_List")
# bl_perform$`F/NF`[is.na(bl_perform$`F/NF`)] <- ""
# bl_perform <- bl_perform[which(bl_perform$`F/NF` == ""),]
# bl_perform$uuid <- Updated_Tracking_Keys$uuid[match(bl_perform$Key,Updated_Tracking_Keys$Key)]
# 
# bl_perform <- bl_perform[-c(1:5)]
# bl_perform <- bl_perform[c(8,1,2,3,4,5,6,7)]
# dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = bl_perform, name ='bl_forecast_perform')
gc()
#devtools::install_github("R-CoderDotCom/cyberpunk")
library(cyberpunk)
library(ggplot2)
library(ggrepel)
#devtools::install_github("R-CoderDotCom/cyberpunk")

ensemble_table_a = ensemble_performance_export %>% mutate(Correct = ifelse( (max_forecast_value > original_bl) & (current_val > original_bl),1,0 ),
                                                          Correct = ifelse( (max_forecast_value < original_bl) & (current_val < original_bl),1,Correct),
                                                          Wrong = ifelse( (max_forecast_value > original_bl) & (current_val < original_bl),1,0 ),
                                                          Wrong = ifelse( (max_forecast_value < original_bl) & (current_val > original_bl),1,Wrong),
                                                          Static = ifelse((Correct != 1)&(Wrong !=1),1,0 )) %>% select(classification, Correct, Wrong, Static) %>%
  group_by(classification) %>%
  summarize(Correct = sum(Correct),
            Wrong = sum(Wrong),
            Static = sum(Static),
            Total = sum(Correct,Static,Wrong) ) 

ensemble_table_b = ensemble_performance_export %>% 
  mutate(Correct = ifelse( (max_forecast_value > original_bl) & (current_val > original_bl),1,0 ),
         Correct = ifelse( (max_forecast_value < original_bl) & (current_val < original_bl),1,Correct),
         Wrong = ifelse( (max_forecast_value > original_bl) & (current_val < original_bl),1,0 ),
         Wrong = ifelse( (max_forecast_value < original_bl) & (current_val > original_bl),1,Wrong),
         Static = ifelse((Correct != 1)&(Wrong !=1),1,0 ),
         Result = ifelse(Correct == 1, "Correct",ifelse(Wrong == 1, "Wrong",ifelse(Static == 1, "Static",NA)))) %>% 
  group_by(classification, Result) %>%
  summarize(
    Correct = sum(Correct),
    Wrong = sum(Wrong),
    Static = sum(Static),
    Aggreg = ifelse( (Correct == 0) & (Result == "Wrong"), Wrong, ifelse((Correct == 0) & (Result == "Static"), Static, Correct))) %>% distinct() %>%
  select(classification,Result,Aggreg) %>% left_join(ensemble_table_a %>% select(classification,Total), by = c("classification"="classification")) %>%
  mutate(Percent = scales::percent(round(Aggreg/Total,2))) %>% arrange(desc(classification))

clrs = colorRampPalette(c("#00ff9f","#00b8ff","#001eff","#bd00ff","#d600ff"))(7)

#scales::show_col(clrs)
clr_bg = "black"
clr_bg2 = "gray10"
clr_grid = "gray30"
clr_text = "#d600ff"

theme_cyberpunk <- function(){
  theme(
    plot.background = element_rect(fill = clr_bg, color = clr_bg),
    plot.margin = margin(1.5,2,1.5,1.5, "cm"),
    panel.background =  element_rect(fill = clr_bg, color = clr_bg),
    panel.grid = element_line(colour = clr_grid, size = 5),
    panel.grid.major = element_line(colour = clr_grid, size = 1),
    panel.grid.minor = element_line(colour = clr_grid, size = 1),
    axis.ticks.x = element_line(colour = clr_grid, size = 1),
    axis.line.y = element_line(colour = clr_grid, size = .5),
    axis.line.x = element_line(colour = clr_grid, size = .5),
    plot.title = element_text(colour = clr_text),
    plot.subtitle = element_text(colour = clr_text),
    axis.text = element_text(colour = clr_text),
    axis.title = element_text(colour = clr_text),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank(),
  legend.text = element_text(colour = 'grey80', size = 12, face = "bold"),
  strip.background = element_rect(fill = clr_bg2, color = clr_bg2)
  )
}


g = ggplot(data = ensemble_table_b %>% filter(classification != "F" & classification != "Ignore"), aes(x = reorder(classification,desc(classification)), y = Aggreg, fill = Result, label = Percent)) + 
  geom_bar(stat='identity')  + coord_flip() + 
  xlab("Tiers") + ylab("Total Cards by Tier") + ggtitle("Forecast Performance") +theme_minimal() +
  theme_cyberpunk() + 
  theme(legend.position="top") + theme(legend.title = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))  + 
  geom_label_repel(show.legend=FALSE, size = 3.5, position = position_stack(vjust = .5), direction = c("x")) +
  scale_fill_manual(values = c('Correct' = '#01ffc3',
                               'Wrong' = '#ef0888',
                               'Static' = '#F5D300')) +
  theme(plot.title = element_text(face = "bold", size = 26)) +
  theme(axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=10, face="bold"),
        axis.text.y = element_text(size=10, face="bold")) 

g 

setwd("/home/cujo253/pics")
ggsave(plot = g, width = 20, height = 9, dpi = 800, filename = "ensemble.png")



write_csv(ensemble_performance_export %>% filter( (current_val - original_bl) > 15) %>% arrange(desc((current_val - original_bl))), "performance.csv")

          