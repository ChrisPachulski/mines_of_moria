#remotes::install_github("rstudio/plumber")
pacman::p_load(plumber,bigrquery,tidyverse,cyberpunk,ggplot2,ggrepel)
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

con <- gaeas_cradle("wolfoftinstreet@gmail.com")

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
WHERE Date = CURRENT_DATE() AND 
 _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 0 DAY)) AND 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) 
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
        max_forecast_value = max(max_forecast_value),
        current_val = current_val,
        classification = min(Classification),
        custom_sort = min(custom_sort),
        accuracy_metric = round(abs((max_forecast_value - current_val)/max_forecast_value),2)) %>%
    ungroup() %>% distinct() %>% 
    arrange(custom_sort,accuracy_metric)

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

#* Forecast Performance PNG
#* @serializer png list(width = 1334, height = 750)
#* @get /current_plot

function(){
    plot(g)
}