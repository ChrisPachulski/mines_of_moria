source("config.R")
pacman::p_load(tidyverse,anytime,doFuture,lubridate,bigrquery,arules,arulesViz,recommenderlab,tidyquant,ggplot2,plotly)

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
data_pull_loads = function(days_back, current_day = -1){
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    statement = paste('SELECT date, tcg_id, Card_name card,
     a.set edition,rarity,number,
     version,condition,
     a.language,cmc,typing,coloring,
     description,flavor,
     listing_type, sold_quantity,dop,
     sell_price, formats
    FROM `gaeas-cradle.mtg_basket.*` a
    WHERE  _TABLE_SUFFIX BETWEEN
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',days_back,' DAY)) AND
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',current_day,' DAY)) ')
    
    mtg_baskets_tbl = dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) 
    return(mtg_baskets_tbl)
}
get_basket_data = function(){ 
    
    load_1 = tryCatch({data_pull_loads(days_back = 15, current_day = -1)}, 
                      error = function(e){print("Load 1(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 15, current_day = -1)})
    Sys.sleep(2)
    load_2 = tryCatch({data_pull_loads(days_back = 30, current_day = 15)}, 
                      error = function(e){print("Load 2(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 30, current_day = 15)})
    Sys.sleep(2)
    load_3 = tryCatch({data_pull_loads(days_back = 45, current_day = 30)}, 
                      error = function(e){print("Load 3(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 45, current_day = 30)})
    Sys.sleep(2)
    load_4 = tryCatch({data_pull_loads(days_back = 60, current_day = 45)}, 
                      error = function(e){print("Load 4(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 60, current_day = 45)})
    Sys.sleep(2)
    load_5 = tryCatch({data_pull_loads(days_back = 75, current_day = 60)}, 
                      error = function(e){print("Load 5(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 75, current_day = 60)})
    Sys.sleep(2)
    load_6 = tryCatch({data_pull_loads(days_back = 90, current_day = 75)}, 
                      error = function(e){print("Load 6(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 90, current_day = 75)})
    Sys.sleep(2)
    load_7 = tryCatch({data_pull_loads(days_back = 105, current_day = 90)}, 
                      error = function(e){print("Load 7(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 105, current_day = 90)})
    Sys.sleep(2)
    load_8 = tryCatch({data_pull_loads(days_back = 120, current_day = 105)}, 
                      error = function(e){print("Load 8(of 8) failed, trying again");Sys.sleep(5);data_pull_loads(days_back = 120, current_day = 105)})
    
    mtg_baskets_tbl = rbind(load_1,
                            load_2,
                            load_3,
                            load_4,
                            load_5,
                            load_6,
                            load_7,
                            load_8) %>% distinct()
    
    order_ids = mtg_baskets_tbl %>% arrange(dop) %>% select(dop) %>% distinct() %>% mutate(order_number = seq(nrow(.)))
    
    mtg_baskets_tbl = mtg_baskets_tbl %>% distinct() %>%
        left_join(order_ids,by=c("dop"="dop"))
    
    order_totals = mtg_baskets_tbl %>% group_by(order_number) %>% summarize(order_total = sum(sell_price))
    
    mtg_baskets_tbl = mtg_baskets_tbl %>%
        left_join(order_totals,by=c("order_number"="order_number")) 
    
    return(mtg_baskets_tbl)
}
store_basket_data_locally = function(){
    write_csv(mtg_baskets_tbl %>% arrange(desc(dop)),"/Users/cujo253/mines_of_moria/Essential_Referential_CSVS/baskets.csv") 
}

item_frequency = function(cumulative_perc = 0.5){
    item_frequency_tbl <- mtg_baskets_tbl %>%
        mutate(info = paste(card,number,version,sep="|")) %>%
        count(info) %>%
        arrange(desc(n)) %>%
        mutate(
            pct = n / sum(n),
            cumulative_pct = cumsum(pct),
            popular_product = ifelse(cumulative_pct <= cumulative_perc, "Yes", "No")
        ) %>%
        rowid_to_column(var = "rank") %>%
        mutate(label_text = str_glue("Rank: {rank}
                                     Product: {info}
                                     Count: {n}
                                     Pct: {scales::percent(pct)}
                                     Cumulative Pct: {scales::percent(cumulative_pct)}")) 
    
    
    
    g <- item_frequency_tbl %>%
        slice(1:5000) %>%
        ggplot(aes(rank, n)) +
        geom_point(aes(size = n, color = popular_product, text = label_text), alpha = 0.2) +
        theme_tq() +
        scale_color_tq() +
        theme(legend.direction = "vertical", 
              legend.position  = "right") +
        labs(title = "Item Frequency", 
             subtitle = "Top Items Account For Majority Of Purchases")
    
    top_products_vec <- item_frequency_tbl %>%
        filter(popular_product == "Yes") %>%
        pull(info)
    
    top_products_basket_tbl <- mtg_baskets_tbl %>%
        mutate(info = paste(card,number,version,sep="|")) %>%
        filter(info %in% top_products_vec)
    
    return(list(top_products_basket_tbl,g,item_frequency_tbl))
}
edition_frequency = function(cumulative_perc = 0.5){
    
    mtg_all = mtg_baskets_tbl
    
    gc()
    item_frequency_tbl <- mtg_all %>%
        count(edition) %>%
        arrange(desc(n)) %>%
        mutate(
            pct = n / sum(n),
            cumulative_pct = cumsum(pct),
            popular_product = ifelse(cumulative_pct <= cumulative_perc, "Yes", "No")
        ) %>%
        rowid_to_column(var = "rank") %>%
        mutate(label_text = str_glue("Rank: {rank}
                                     Product: {edition}
                                     Count: {n}
                                     Pct: {scales::percent(pct)}
                                     Cumulative Pct: {scales::percent(cumulative_pct)}")) 
    
    
    
    g <- item_frequency_tbl %>%
        slice(1:5000) %>%
        ggplot(aes(rank, n)) +
        geom_point(aes(size = n, color = popular_product, text = label_text), alpha = 0.2) +
        theme_tq() +
        scale_color_tq() +
        theme(legend.direction = "vertical", 
              legend.position  = "right") +
        labs(title = "Item Frequency", 
             subtitle = "Top Items Account For Majority Of Purchases")
    
    top_products_vec <- item_frequency_tbl %>%
        filter(popular_product == "Yes") %>%
        pull(edition)
    
    top_products_basket_tbl <-  mtg_all %>%
        filter(edition %in% top_products_vec)
    
    return(list(top_products_basket_tbl,g,item_frequency_tbl))
}
typing_frequency = function(cumulative_perc = 0.5){
    
    mtg_all = mtg_baskets_tbl %>% 
        mutate(coloring = ifelse((typing == "Land")&(is.na(coloring)), "Colorless",coloring )) %>%
        mutate(info = paste(typing,coloring,sep="|"))
    
    gc()
    item_frequency_tbl <- mtg_all %>%
        count(info) %>%
        arrange(desc(n)) %>%
        mutate(
            pct = n / sum(n),
            cumulative_pct = cumsum(pct),
            popular_product = ifelse(cumulative_pct <= cumulative_perc, "Yes", "No")
        ) %>%
        rowid_to_column(var = "rank") %>%
        mutate(label_text = str_glue("Rank: {rank}
                                 Product: {info}
                                 Count: {n}
                                 Pct: {scales::percent(pct)}
                                 Cumulative Pct: {scales::percent(cumulative_pct)}")) 
    
    
    
    g <- item_frequency_tbl %>%
        slice(1:5000) %>%
        ggplot(aes(rank, n)) +
        geom_point(aes(size = n, color = popular_product, text = label_text), alpha = 0.2) +
        theme_tq() +
        scale_color_tq() +
        theme(legend.direction = "vertical", 
              legend.position  = "right") +
        labs(title = "Item Frequency", 
             subtitle = "Top Items Account For Majority Of Purchases")
    
    top_products_vec <- item_frequency_tbl %>%
        filter(popular_product == "Yes") %>%
        pull(info)
    
    top_products_basket_tbl <- mtg_all %>%
        filter(info %in% top_products_vec)
    
    return(list(top_products_basket_tbl,g,item_frequency_tbl))
}
fifty_plus_frequency = function(cumulative_perc = 0.5){
    
    mtg_all = mtg_baskets_tbl 
    
    prices = mtg_all %>% group_by(tcg_id,card,edition,rarity,number,version,condition,language) %>%
        summarize(mprice = round(mean(order_total),2),
                  medprice = round(median(order_total),2)) %>%
        ungroup()
    
    mtg_all = mtg_baskets_tbl %>% 
        mutate(info = paste(card,edition,version,language,sep="|")) %>%
        left_join(prices)
    
    
    gc()
    item_frequency_tbl <- mtg_all %>%
        filter(medprice >= 50) %>%
        filter(rarity != "S") %>%
        count(info) %>%
        arrange(desc(n)) %>%
        mutate(
            pct = n / sum(n),
            cumulative_pct = cumsum(pct),
            popular_product = ifelse(cumulative_pct <= cumulative_perc, "Yes", "No")
        ) %>%
        rowid_to_column(var = "rank") %>%
        mutate(label_text = str_glue("Rank: {rank}
                                 Product: {info}
                                 Count: {n}
                                 Pct: {scales::percent(pct)}
                                 Cumulative Pct: {scales::percent(cumulative_pct)}")) 
    
    
    g <- item_frequency_tbl %>%
        slice(1:5000) %>%
        ggplot(aes(rank, n)) +
        geom_point(aes(size = n, color = popular_product, text = label_text), alpha = 0.2) +
        theme_tq() +
        scale_color_tq() +
        theme(legend.direction = "vertical", 
              legend.position  = "right") +
        labs(title = "Item Frequency", 
             subtitle = "Top Items Account For Majority Of Purchases")
    
    top_products_vec <- item_frequency_tbl %>%
        filter(popular_product == "Yes") %>%
        pull(info)
    
    top_products_basket_tbl <- mtg_all %>%
        filter(info %in% top_products_vec) 
    
    return(list(top_products_basket_tbl,g,item_frequency_tbl))
}
five_plus_frequency = function(cumulative_perc = 0.5){
    
    mtg_all = mtg_baskets_tbl 
    
    prices = mtg_all %>% group_by(tcg_id,card,edition,rarity,number,version,condition,language) %>%
        summarize(mprice = round(mean(sell_price),2),
                  medprice = round(median(sell_price),2)) %>%
        ungroup()
    
    mtg_all = mtg_baskets_tbl %>% 
        mutate(info = paste(card,edition,version,language,sep="|")) %>%
        left_join(prices)
    
    
    gc()
    item_frequency_tbl <- mtg_all %>%
        filter(medprice >= 5) %>%
        filter(rarity != "S") %>%
        count(info) %>%
        arrange(desc(n)) %>%
        mutate(
            pct = n / sum(n),
            cumulative_pct = cumsum(pct),
            popular_product = ifelse(cumulative_pct <= cumulative_perc, "Yes", "No")
        ) %>%
        rowid_to_column(var = "rank") %>%
        mutate(label_text = str_glue("Rank: {rank}
                                 Product: {info}
                                 Count: {n}
                                 Pct: {scales::percent(pct)}
                                 Cumulative Pct: {scales::percent(cumulative_pct)}")) 
    
    
    g <- item_frequency_tbl %>%
        slice(1:5000) %>%
        ggplot(aes(rank, n)) +
        geom_point(aes(size = n, color = popular_product, text = label_text), alpha = 0.2) +
        theme_tq() +
        scale_color_tq() +
        theme(legend.direction = "vertical", 
              legend.position  = "right") +
        labs(title = "Item Frequency", 
             subtitle = "Top Items Account For Majority Of Purchases")
    
    top_products_vec <- item_frequency_tbl %>%
        filter(popular_product == "Yes") %>%
        pull(info)
    
    top_products_basket_tbl <- mtg_all %>%
        filter(info %in% top_products_vec) 
    
    return(list(top_products_basket_tbl,g,item_frequency_tbl))
}

format_basket_matrix = function(pull_column = NULL){
    
    if(is.null(pull_column)){
        user_item_tbl <- top_products_basket_tbl %>%
            distinct() %>%
            select(order_number,info) %>%
            mutate(value = 1) %>%
            distinct() %>%
            spread(info, value, fill = 0)
        gc()
        user_item_rlab <- user_item_tbl %>%
            select(-order_number) %>%
            as.matrix() %>%
            as("binaryRatingMatrix")
        gc()
    }else{
        user_item_tbl <- top_products_basket_tbl %>%
            distinct() %>%
            select(order_number,{{pull_column}}) %>%
            mutate(value = 1) %>%
            distinct() %>%
            spread({{pull_column}}, value, fill = 0)
        gc()
        user_item_rlab <- user_item_tbl %>%
            select(-order_number) %>%
            as.matrix() %>%
            as("binaryRatingMatrix")
        gc()
    }
    return(user_item_rlab)
}
build_rules = function(conf = 0.5){
    eval_recipe <- user_item_rlab %>%
        evaluationScheme(method = "cross-validation", k = 5, given = -1)
    
    
    model_ar <- recommenderlab::Recommender(
        data = user_item_rlab, 
        method = "AR", 
        param = list(supp = i_frequency[[3]]%>% filter(popular_product == "Yes") %>% select(pct) %>% mutate(mean_val = mean(pct,na.rm=T)) %>% select(mean_val) %>% min(), conf = conf))
    
    
    rules <- model_ar@model$rule_base
    
    # 7.1 INTERACTIVE ----
    inspectDT(rules)
    rules_df = as(rules,"data.frame")
    rules_export = rules_df %>% mutate(rules = gsub("(\\{|\\})","",rules)) %>% separate(rules,into=c("og","resulting_buy"),sep=" => ") %>% arrange(desc(lift))
    
    graph = plot(rules, method = "scatterplot", 
                 marker = list(opacity = .7, size = ~lift), 
                 colors = c("blue", "green"),
                 engine = "plotly")
    
    return(list(rules_export,graph,rules))
}

