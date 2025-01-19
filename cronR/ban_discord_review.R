source("config.R")
pacman::p_load(tidyverse,lubridate,anytime,hms,ggrepel,stringi,ggplot2,textclean,tidymodels,textrecipes)
setwd("/home/cujo253/mines_of_moria/ban_logs/logs/")
options(scipen=999)
clean_server_data = function(server_data){
    cleanse = server_data %>% 
        separate(Date, c("Date","Time","PM"),sep = " ") %>% 
        mutate(Date = as.Date(Date, format = "%d-%b-%y"),
               Time = paste(Time,PM)) %>%
        select(-PM) %>%
        mutate(Time = as_hms((parse_date_time(Time, "%I:%M %p"))))
    return(cleanse)
}
'%!in%' <- function(x,y)!('%in%'(x,y))
tool_unique_card_search = function(server_data){
    unique_searches = server_data %>% 
        filter(grepl("server-dump",channel)) %>%
        mutate(Content = as.character(as.factor(Content))) %>% 
        separate(Content,c("card","user_details"),sep = " from ") %>% 
        arrange(desc(Date)) %>% 
        mutate(user_details = gsub(".*(search)* by ","",user_details)) %>%
        filter(grepl("\\/newspaper",user_details)==F) %>%
        filter(grepl("\\/sleepers",user_details)==F) %>%
        filter(grepl("\\/global",user_details)==F) %>%
        filter(grepl("https",user_details)==F) %>%
        filter(grepl("^by",user_details)==F) %>%
        filter(grepl("\\/admin_",user_details)==F) %>%
        filter(grepl("\\/arbit",user_details)==F) %>%
        filter(grepl("\\/r\\/",user_details)==F) %>%
        #filter(grepl("\\/search",user_details)==F) %>%
        filter(grepl("\\/cards",user_details)==F) %>%
        filter(grepl("\\ss\\:",user_details)==F) %>%
        filter(grepl("^Above\\]",user_details)==F) %>%
        filter(grepl("^autocard anywhere",user_details)==F) %>%
        filter(grepl("^banbot",user_details)==F) %>%
        filter(grepl("\\]$",user_details)==F) %>%
        filter(grepl("\\.(csv|xlsx|txt)",user_details)==F) %>%
        filter(is.na(user_details) == F) %>%
        separate(user_details, c("user","domain"),sep="@") %>%
        filter(nchar(user) > 3) %>%
        select(-Attachments,-Reactions) %>%
        filter(Date >= "2021-02-01") %>%
        filter(grepl("\\/search( by )*",user)==F) %>%
        mutate(user = tolower(user))
    
    return(unique_searches)
}
card_search_group_by = function(server_data,column_by){
    column_by = enquo(column_by)
    server_data %>% 
        mutate(month = month(Date)) %>%
        group_by(!!column_by) %>%
        summarize(cards_searched = n()) %>%
        ungroup() %>% 
        arrange(desc(cards_searched),desc(!!column_by))
}
card_search_group_users_by = function(server_data,column_by){
    value = tryCatch({
        column_by = enquo(column_by)
        server_data %>% 
            mutate(month = floor_date(Date,"month")) %>%
            group_by(user,!!column_by) %>%
            summarize(cards_searched = n()) %>%
            ungroup() %>% 
            arrange(desc(cards_searched),desc(!!column_by)) },
        error = function(e){
            server_data %>% 
                mutate(month = floor_date(Date,"month")) %>%
                group_by(user) %>%
                summarize(cards_searched = n()) %>%
                ungroup() %>% 
                arrange(desc(cards_searched)) 
        })
    
    
    return(value)
}

clean_discord_only = function(){
    setwd("/home/cujo253/mines_of_moria/ban_logs/logs/")
    server_data = NULL
    for(i in 1:length(list.files(pattern = "*.csv"))){
        server_data = rbind(server_data,
                            list.files(pattern = "*.csv")[i] %>% 
                                map_df(~read_csv(.)) %>% 
                                mutate(category = gsub("BAN - \\(*[a-zA-Z]+\\)*\\s*\\(*[a-zA-Z]+\\)*\\s*\\(*[a-zA-Z]+\\)*\\s*\\(*[a-zA-Z]+\\)* -\\s*[^\x01-\x7F]*","",gsub("\\s\\[.*","",list.files(pattern = "*.csv")[i] %>% deparse(substitute(.))))) %>%
                                mutate(channel = gsub("[^\x01-\x7F]", "",gsub("(BAN.*-.*-\\s|\\s\\[.*)","",list.files(pattern = "*.csv")[i] %>% deparse(substitute(.))))) )
    }
    
    
    
    cleanse = server_data %>% 
        separate(Date, c("Date","Time","PM"),sep = " ") %>% 
        mutate(Date = as.Date(Date, format = "%d-%b-%y"),
               Time = paste(Time,PM)) %>%
        select(-PM) %>%
        mutate(Time = as_hms((parse_date_time(Time, "%I:%M %p")))) %>% 
        mutate(month = month(Date)) %>% 
        mutate(month = day(Date)) %>% 
        mutate(month = hour(Date)) %>%
        mutate(channel = str_extract(gsub("\\\\U\\{.*\\}","",channel),"[a-z]+\\-*[a-z]*"))
    
    return(cleanse)
}

server_data = NULL
for(i in 1:length(list.files(pattern = "*.csv"))){
    server_data = rbind(server_data,
        list.files(pattern = "*.csv")[i] %>% 
        map_df(~read_csv(.)) %>% mutate(channel = gsub("(BAN.*-.*-\\s|\\s\\[.*)","",list.files(pattern = "*.csv")[i] %>% deparse(substitute(.))) ) )
}
#server_data = read_csv("/home/cujo253/mines_of_moria/ban_logs/logs/BAN Arbitrage Network - Admin - 🚨server-dump [769323295526748160].csv")


base_server_data = clean_server_data(server_data)

card_searches = tool_unique_card_search(base_server_data)

card_search_group_users_by(card_searches,year) 
card_search_group_users_by(card_searches,domain) 

card_search_group_users_by(card_searches) 
card_search_group_by(card_searches,domain)

users_per_month = card_search_group_users_by(card_searches,month)  %>% 
    #select(-cards_searched) %>%
    group_by(month) %>% 
    summarize(total_user = n(),
              total_searches = sum(cards_searched)) %>%
    ungroup()


card_searches %>%
    filter(Date >= '2022-01-01') %>%
    mutate(user = gsub("\\s*\\(.*","",user)) %>%
    mutate(domain = gsub("\\s*\\(.*","",domain)) %>%
    group_by(user,domain) %>%
    summarize(site_queries = n()) %>%
    ungroup() %>%
    arrange(desc(site_queries)) %>%
    mutate(identity = ifelse(grepl("(starcitygames|coolstuffinc)",domain),"Vendor","Individual User")) %>%
    #select(-user,-domain) %>%
    head(25) 
    


# Discord Only ------------------------------------------------------------


discord_review = clean_discord_only()

#discord_review %>%select(category) %>% distinct() %>% view()

category_posts = discord_review %>% 
    mutate(mon = month(Date),yr = year(Date)) %>%
    filter(channel != "server-dump") %>%
    group_by(category,yr,mon) %>%
    summarize(total_posts = n()) %>%
    ungroup() %>%
    arrange(desc(yr),mon,desc(total_posts))

channel_posts = discord_review %>%
    filter(channel != "server-dump") %>%
    mutate(Date = floor_date(Date,"month")) %>%
    group_by(channel,Date) %>%
    summarize(total_posts = n()) %>%
    ungroup() %>%
    arrange(desc(total_posts))
channel_posts %>% filter(grepl("auction",channel)) %>% view()

# Magic Discord Only ------------------------------------------------------
for(q in 78:length(unique(discord_review$category))){
   
    category_selection = tolower(unique(discord_review$category)[q])

    
    if(grepl("not public",category_selection)){next}
    if(grepl("evolution",category_selection)){next}
    if(grepl("rules$",category_selection)){next}
    if(grepl("pronouns$",category_selection)){next}
    if(grepl("server-dump",category_selection)){next}
    
    if(grepl("regional",category_selection)){next}
    if(grepl("affiliate",category_selection)){next}
    if(grepl("areas-of-interest",category_selection)){next}
    
    
    
    if(grepl("vent-old",category_selection)){next}
    
    
    ovr_category_view = discord_review %>%
        filter(channel != "server-dump") %>%
        filter(channel != "server-notifs") %>%
        filter(grepl(category_selection,tolower(category) ))  %>%
        mutate(Date = floor_date(Date,"month")) %>%
        filter(Date != max(Date)) %>%
        group_by(channel,Date) %>% 
        summarize(total_posts = n()) %>%
        ungroup() %>%
        arrange(desc(Date),desc(total_posts)) 
    
    
    latest_values = ovr_category_view %>%
        filter(Date == max(Date)) %>% .[1:3,]
    
    max_point = NULL
    for(i in 1:3){
        if(i == 1){
        max_point_1 = ovr_category_view %>%
            filter(channel == ovr_category_view$channel[i]) %>%
            filter(total_posts == max(total_posts))
        }
        if(i == 2){
        max_point_2 = ovr_category_view %>%
            filter(channel == ovr_category_view$channel[i])%>%
            filter(total_posts != max(total_posts)) %>%
            filter(total_posts == max(total_posts))}
        
        if(i == 3){
        max_point_3 = ovr_category_view %>%
            filter(channel == ovr_category_view$channel[i]) %>%
            filter(total_posts != max(total_posts)) %>%
            filter(total_posts != max(total_posts)) %>%
            filter(total_posts == max(total_posts))
        
        max_point = rbind(max_point_1,max_point_2,max_point_3)
        }
        
        
    }
    print(ggplot(ovr_category_view, aes(x = Date, y = total_posts, color = channel, label = total_posts)) +
        geom_line() +
        scale_x_date(date_labels="%Y-%m",date_breaks  ="1 month") +
        geom_point(data = latest_values, aes(x = Date, y = total_posts), shape = 21, fill = "white", size = 2, stroke = 1.7) +
        geom_text_repel(data = latest_values, aes(x = Date, y = total_posts, label = total_posts), size = 4, vjust = 1.25, hjust = .5 )+
        geom_point(data = max_point, aes(x = Date, y = total_posts), shape = 21, fill = "white", size = 2, stroke = 1.7) +
        geom_text_repel(data = max_point, aes(x = Date, y = total_posts, label = total_posts), color = "dark green",size = 4, vjust = 1.25, hjust = .75 ) +
        ggtitle(paste(str_extract(unique(discord_review$category)[q],"[A-Za-z]+\\s*[A-Za-z]*\\s*[A-Za-z]*\\s*[A-Za-z]*")," channel",sep="")) + ylab("Total Posts") + xlab("Month") +
        theme(plot.title = element_text(hjust = 0.5)))
}

ovr_user %>% filter(grepl("(coolstuffinc|starcitygames)",domain)==F ) #%>% view()


cards_of_interest = card_searches %>% 
    mutate(month = month(Date))%>%
    #filter(grepl("(coolstuffinc)",domain)==T ) %>%
    filter(Date >= "2021-01-01") %>%
    group_by(user, card) %>%
    summarize(ct = n(),
              min_date = min(Date),
              max_date = max(Date)) %>%
    ungroup() %>%
    arrange(desc(ct)) %>%
    filter(ct >= 2)

unique_card_searches = card_searches %>% filter(card %in% cards_of_interest$card )
pass_tbl = NULL



unique_cards = unique_card_searches %>%
    filter(grepl("\\/search",user)==F) %>%
    select(card) %>% distinct()
i = 1
for (i in 1:nrow(unique_cards)){
    pass_fail_prep = unique_card_searches %>%
        filter(grepl("\\/search",user)==F) %>%
        filter(card == unique_cards$card[i]) %>%
        group_by(user) %>%
        summarize(ct = n()) %>%
        arrange(desc(ct)) %>% 
        ungroup()
    
    
    pass_value = pass_fail_prep %>%
        mutate(total_card_searches = sum(ct),
               ratio = max(ct)/total_card_searches ) %>%
        mutate(pass_status = ifelse(ratio >= .4, "fail" , "pass"),
               pass_status = ifelse( (pass_status == "fail") & (total_card_searches <= 40), "pass" , pass_status)) %>%
        select(pass_status) %>% distinct()
    
    pass_value = cbind(unique_cards$card[i],pass_value$pass_status)
    
    pass_tbl=  rbind(pass_tbl,pass_value)
    print(i)
}

tested_card_searches = unique_card_searches %>% left_join(pass_tbl %>% as_tibble(),by=c("card"="V1")) %>% rename(pass_status = V2)


searches_by_month_tbl = tested_card_searches %>%
    filter(grepl("(coolstuffinc|starcitygames)",domain)==F ) %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    filter(pass_status == "pass") %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")

ggplot(aes(x = month, y= user_searches),data = searches_by_month_tbl) +
    geom_line() +
    labs(
        title = "User Card Searches Per Month",
        x = "",
        y = "Total Searches"
    ) +
    theme_minimal()
    

vendor_searches_by_month_tbl = tested_card_searches %>%
    filter(grepl("(coolstuffinc|starcitygames)",domain)==T ) %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    filter(pass_status == "pass") %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")

all_searches_tbl = searches_by_month_tbl %>%
    left_join(vendor_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(user_searches = user_searches.x, vendor_searches = user_searches.y) %>%
    select(month, user_searches, vendor_searches) %>%
    gather(key = "variable", value = "value", -month)

ggplot(aes(x = month, y = user_searches),data = vendor_searches_by_month_tbl) +
    geom_line() +
    labs(
        title = "Vendor Card Searches Per Month",
        x = "",
        y = "Total Searches"
    ) +
    theme_minimal()

ggplot(all_searches_tbl, aes(x=month,y=value)) + 
    geom_line(aes(color = variable, linetype = variable)) + 
    scale_color_manual(values = c("darkred", "steelblue")) +
    labs(
        title = "Card Searches Per Month",
        x = "",
        y = "Total Searches"
    ) +
    theme_minimal()

csi_searches_by_month_tbl = tested_card_searches %>%
    filter(grepl("(coolstuffinc)",domain)==T ) %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    filter(pass_status == "pass") %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")

scg_searches_by_month_tbl = tested_card_searches %>%
    filter(grepl("(starcity)",domain)==T ) %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    filter(pass_status == "pass") %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")

all_vendor_searches_tbl = csi_searches_by_month_tbl %>%
    left_join(scg_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(csi_searches = user_searches.x, scg_searches = user_searches.y) %>%
    select(month, csi_searches, scg_searches) %>%
    gather(key = "variable", value = "value", -month)

ggplot(all_vendor_searches_tbl, aes(x=month,y=value)) + 
    geom_line(aes(color = variable, linetype = variable)) + 
    scale_color_manual(values = c("darkred", "steelblue")) +
    labs(
        title = "Vendor Searches Per Month",
        x = "",
        y = "Total Searches"
    ) +
    theme_minimal()


# Newspaper Usage Rates --------------------------------------------------------

newspaper_unique_card_searches = discord_review %>% 
    mutate(Content = as.character(as.factor(Content))) %>% 
    separate(Content,c("card","user_details"),sep = " from ") %>% 
    arrange(desc(Date)) %>% 
    filter(grepl("\\/newspaper",user_details)==T) %>%
    mutate(user_details = gsub("\\/newspaper by ","",user_details)) %>%
    filter(is.na(user_details) == F) %>%
    separate(user_details, c("user","domain"),sep="@") %>%
    filter(nchar(user) > 3) %>%
    select(-Attachments,-Reactions) 



newspaper_unique_card_searches %>%  nrow()

newspaper_ovr_user = newspaper_unique_card_searches %>%
    filter(!grepl("newspaper",user)) %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(user,month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na()

newspaper_ovr_month = newspaper_unique_card_searches %>%
    filter(!grepl("newspaper",user)) %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na() %>%
    arrange(month)

newspaper_ovr_month %>% 
    left_join(users_per_month) %>% 
    rename(newspaper_searches = cards_searched) %>%
    mutate(newspaper_traffic =scales::percent( (newspaper_searches/total_searches), accuracy = 0.01)) %>%
    filter(month != max(month))

newspaper_searches_by_month_tbl = newspaper_unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) 


# Sleeper Usage Rates -----------------------------------------------------

sleepers_unique_card_searches = discord_review %>% 
    mutate(Content = as.character(as.factor(Content))) %>% 
    separate(Content,c("card","user_details"),sep = " from ") %>% 
    arrange(desc(Date)) %>% 
    filter(grepl("\\/sleepers ",user_details)==T) %>%
    mutate(user_details = gsub("\\/sleepers by ","",user_details)) %>%
    filter(is.na(user_details) == F) %>%
    separate(user_details, c("user","domain"),sep="@") %>%
    filter(nchar(user) > 3) %>%
    select(-Attachments,-Reactions) %>%
    filter(Date >= "2021-02-01")



sleepers_unique_card_searches %>% nrow() 

sleepers_ovr_user = sleepers_unique_card_searches %>%
    #mutate(month = month(Date)) %>%
    group_by(user,domain) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na()

sleepers_searches_by_month_tbl = sleepers_unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) 

# Global Usage Rates ------------------------------------------------------

global_unique_card_searches = discord_review %>% 
    mutate(Content = as.character(as.factor(Content))) %>% 
    separate(Content,c("card","user_details"),sep = " from ") %>% 
    arrange(desc(Date)) %>% 
    filter(grepl("\\/global ",user_details)==T) %>%
    mutate(user_details = gsub("\\/global by ","",user_details)) %>%
    filter(is.na(user_details) == F) %>%
    separate(user_details, c("user","domain"),sep="@") %>%
    filter(nchar(user) > 3) %>%
    select(-Attachments,-Reactions) %>%
    filter(Date >= "2021-02-01")


unique_card_searches %>% nrow()
global_unique_card_searches %>% nrow() 

global_ovr_user = global_unique_card_searches %>%
    #mutate(month = month(Date)) %>%
    group_by(user,domain) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na()

global_searches_by_month_tbl = global_unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")

all_searches_by_month_tbl = unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month >= "2022-01-01")

all_page_searches_tbl = all_searches_by_month_tbl %>%
    left_join(newspaper_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(all_searches = user_searches.x, newspaper_searches = user_searches.y) %>%
    left_join(sleepers_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(sleepers_searches = user_searches) %>%
    left_join(global_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(global_searches = user_searches) %>%
    select(month, all_searches, newspaper_searches, sleepers_searches, global_searches) %>%
    gather(key = "variable", value = "value", -month)

ggplot(all_page_searches_tbl, aes(x=month,y=value)) + 
    geom_line(aes(color = variable, linetype = variable)) + 
    #scale_color_manual(values = c("darkred", "steelblue")) +
    labs(
        title = "Page Hits Per Month",
        x = "",
        y = "Total Clicks"
    ) +
    theme_minimal()

aux_page_searches_tbl = global_searches_by_month_tbl %>%
    left_join(newspaper_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(global_searches = user_searches.x, newspaper_searches = user_searches.y) %>%
    left_join(sleepers_searches_by_month_tbl,by=c("month"="month")) %>%
    rename(sleepers_searches = user_searches) %>%
    select(month, newspaper_searches, sleepers_searches, global_searches) %>%
    gather(key = "variable", value = "value", -month)

ggplot(aux_page_searches_tbl, aes(x=month,y=value)) + 
    geom_line(aes(color = variable, linetype = variable)) + 
    #scale_color_manual(values = c("darkred", "steelblue")) +
    labs(
        title = "Page Hits Per Month",
        x = "",
        y = "Total Clicks"
    ) +
    theme_minimal()


# Discord Sentiment -------------------------------------------------------
pacman::p_load(janitor)
author_content_all = discord_review %>% 
    filter(!grepl("(auction|autobot|watch|neverland|super|mods|cast)",channel)) %>%
    clean_names() %>% 
    filter(date >= '2020-01-01') %>% 
    select(author,date,content) %>%
    mutate(date = floor_date(date,"month"))

authors_from_2020 = author_content_all %>% 
    group_by(author,date) %>% 
    summarize(ct = n() ) %>% 
    ungroup() %>% 
    arrange((ct)) %>%
    filter(!grepl(("\\#0000"),author)) 

user_post_review = function(authors_from_2020){
    left_join(
        authors_from_2020 %>%
            group_by(date) %>%
            summarize(all_user_who_posted_at_least_once = n()) %>%
            ungroup(),
        
        authors_from_2020 %>%
            filter(ct >= 35) %>%
            group_by(date) %>%
            summarize(users_post_one_per_day = n()) %>%
            ungroup()) %>%
        left_join(.,
                  
                  authors_from_2020 %>%
                      filter(ct >= 70) %>%
                      group_by(date) %>%
                      summarize(users_post_two_per_day = n()) %>%
                      ungroup()) %>%
        left_join(.,
                  authors_from_2020 %>%
                      filter(ct >= 100) %>%
                      group_by(date) %>%
                      summarize(users_post_three_per_day = n()) %>%
                      ungroup()
        ) %>% 
        left_join(
            authors_from_2020 %>%
                filter(ct >= 300) %>%
                group_by(date) %>%
                summarize(users_post_ten_plus_per_day = n()) %>%
                ungroup()
        ) %>% 
        mutate(
            core_users_comp = round(users_post_ten_plus_per_day/all_user_who_posted_at_least_once,3)
        ) 
}

group_logic = function(group_5){
    group_5_patterns = group_5 %>%
        arrange(date, author) %>%
        group_by(author) %>%
        summarize(date,
                  lagged_date = as.Date(ifelse(is.na(lag(date)), format(date - months(1),"%Y-%m-%d"), format(lag(date),"%Y-%m-%d") )) ,
                  sequential_months = ifelse( (date - lagged_date) < 32, 1,0 ) + 1 ) %>%
        ungroup() %>% 
        select(-lagged_date) %>%
        group_by(author) %>% 
        summarize(
            date,
            streak_lengths = ceiling(ave(sequential_months, cumsum(sequential_months == 1), FUN = cumsum)/2),
            
            sequential_months = floor(sequential_months/2)) %>%
        ungroup() 
    
    user_trends = group_5_patterns %>%
        mutate(rate = "users_post_ten_plus_per_day") %>%
        left_join(
            group_5_patterns %>% filter(sequential_months == 0) %>% group_by(author) %>% summarize(number_of_streaks = n()+1) %>% ungroup() 
        ) %>%
        left_join(
            group_5_patterns %>% group_by(author) %>% summarize(total_months = n()) %>% ungroup() 
        ) %>%
        left_join(
            group_5_patterns %>%  group_by(author) %>% filter(streak_lengths == max(streak_lengths)) %>% summarize(longest_streak_end_date = max(date)) %>% ungroup() 
        ) %>%
        left_join(
            group_5_patterns %>%  group_by(author) %>% filter(date == max(date)) %>% ungroup() %>% rename(most_recent_month = date) %>% select(-streak_lengths,-sequential_months)
        ) %>%
        mutate(number_of_streaks = ifelse(is.na(number_of_streaks),1,number_of_streaks))
    
    
    distinct_group_5_dates = group_5 %>% select(date) %>% distinct()
    
    new_core_users = NULL
    for(i in 1:nrow(distinct_group_5_dates)){
        tbl_one = group_5 %>% filter(date == distinct_group_5_dates$date[i])
        tbl_two = group_5 %>% filter(date == distinct_group_5_dates$date[i+1]) %>% filter(author %!in% tbl_one$author)
        
        if(i == 1){
            new_core_users = rbind(length(tbl_one$author),length(tbl_two$author))
        }
        
        new_core_users = rbind(new_core_users,length(tbl_two$author))
    }
    
    remove_existing_core_members = group_5 %>% 
        nest(contains('author'), .key = 'author') %>% 
        mutate(author = map(author, simplify)) %>% 
        mutate(
            author_list = sapply(author, toString)
        ) %>%
        mutate(rate = "users_post_ten_plus_per_day")
    ovr_core_tbl = cbind(distinct_group_5_dates,new_core_users %>% as_tibble() %>% .[-c((nrow(new_core_users)-1):nrow(new_core_users)),] %>% rename(new_core_users = V1)) %>%
        cbind(.,remove_existing_core_members %>% select(rate,author_list) )
    
    
    gain_loss_tbl = NULL
    for(i in 1:(nrow(remove_existing_core_members)-1) ){
        
        
        new_authors = as.data.frame(remove_existing_core_members$author[i+1])
        colnames(new_authors) = c("V1")
        
        old_authors = as.data.frame(remove_existing_core_members$author[i])
        colnames(old_authors) = c("V1")
        
        if(i == 1){
            new_core_usernames = paste(old_authors %>% unlist(), collapse = ", ")
            lost_core_users = paste("", collapse=", ")
            gain_loss_line_item = cbind(new_core_usernames,lost_core_users)
            gain_loss_tbl = rbind(gain_loss_tbl,gain_loss_line_item)
        }
        
        new_core_users = anti_join(new_authors,old_authors)[[1]]
        lost_core_users = anti_join(old_authors,new_authors)[[1]]
        if(identical(character(0),new_core_users)){
            new_core_users = ""
        }
        if(identical(character(0),lost_core_users)){
            lost_core_users = ""
        }
        
        new_core_usernames = paste(new_core_users, collapse = ", ")
        lost_core_users = paste(lost_core_users, collapse = ", ")
        
        
        gain_loss_line_item = cbind(new_core_usernames,lost_core_users)
        gain_loss_tbl = rbind(gain_loss_tbl,gain_loss_line_item)
    }
    
    ovr_tbl = cbind(ovr_core_tbl,gain_loss_tbl %>% as_tibble() ) 
    
    return(list(ovr_tbl,user_trends))
}

exclusive_user_post_review = function(authors_from_2020){
    
    group_5 = authors_from_2020 %>%
        filter(ct >= 300) %>%
        filter(!grepl("(Scryfall#0970|Pricefall (beta)#0367)",author)) %>%
        group_by(date,author) %>%
        summarize(users_post_ten_plus_per_day = n()) %>%
        ungroup()
    
    group_5_results = group_logic(group_5)
    
    group_5_ovr = group_5_results[[1]]
    
    group_5_usrs = group_5_results[[2]]
    
    group_4 = authors_from_2020 %>%
        filter(ct >= 150) %>%
        filter(ct < 300) %>%
        filter(!grepl("(Scryfall#0970|Pricefall (beta)#0367)",author)) %>%
        group_by(date,author) %>%
        summarize(users_post_three_per_day = n()) %>%
        ungroup()
    
    group_4_results = group_logic(group_4)
    
    group_4_ovr = group_4_results[[1]] %>% as_tibble() %>% mutate(rate="users_post_5-10_per_day")
    
    group_4_usrs = group_4_results[[2]]%>% as_tibble() %>% mutate(rate="users_post_5-10_per_day")
    
    
    group_3 = authors_from_2020 %>%
        filter(ct >= 70) %>%
        filter(ct < 150) %>%
        filter(!grepl("(Scryfall#0970|Pricefall (beta)#0367)",author)) %>%
        group_by(date,author) %>%
        summarize(users_post_two_per_day = n()) %>%
        ungroup()
    
    group_3_results = group_logic(group_3)
    
    group_3_ovr = group_3_results[[1]] %>% as_tibble() %>% mutate(rate="users_post_2-5_per_day")
    
    group_3_usrs = group_3_results[[2]]%>% as_tibble() %>% mutate(rate="users_post_2-5_per_day")
    
    
    group_2 = authors_from_2020 %>%
        filter(ct >= 35) %>%
        filter(ct < 70) %>%
        filter(!grepl("(Scryfall#0970|Pricefall (beta)#0367)",author)) %>%
        group_by(date,author) %>%
        summarize(users_post_one_per_day = n()) %>%
        ungroup()
    
    group_2_results = group_logic(group_2)
    
    group_2_ovr = group_2_results[[1]] %>% as_tibble() %>% mutate(rate="users_post_three_per_day")
    
    group_2_usrs = group_2_results[[2]]%>% as_tibble() %>% mutate(rate="users_post_three_per_day")
    
    group_1 = authors_from_2020 %>%
        filter(ct < 35) %>%
        filter(!grepl("(Scryfall#0970|Pricefall (beta)#0367)",author)) %>%
        group_by(date,author) %>%
        summarize(all_user_who_posted_at_least_once = n()) %>%
        ungroup()
    
    group_1_results = group_logic(group_1)
    
    group_1_ovr = group_1_results[[1]] %>% as_tibble() %>% mutate(rate="users_post_three_per_day")
    
    group_1_usrs = group_1_results[[2]]%>% as_tibble() %>% mutate(rate="users_post_three_per_day")
    
    ovr_groups = rbind(group_5_ovr,group_4_ovr,group_3_ovr,group_2_ovr,group_1_ovr)
    
    usr_groups = rbind(group_5_usrs,group_4_usrs,group_3_usrs,group_2_usrs,group_1_usrs)
    
    return(list(ovr_groups,usr_groups))
}


all_user_overview = user_post_review(authors_from_2020)
exclusive_all_user_overview = exclusive_user_post_review(authors_from_2020)


# Users - Remove Admins & Mods --------------------------------------------
remove_admins_mods = discord_review %>%
    clean_names() %>%
    filter(!grepl(("\\#0000"),author)) %>% 
    filter(grepl("(super|mod)",channel)) %>%
    select(author) %>%
    distinct()

cleansed_users = discord_review %>% 
    clean_names() %>% 
    filter(!grepl("(auction|autobot|watch|neverland|super|mods|cast)",channel)) %>%
    filter(author %!in% remove_admins_mods$author ) %>%
    filter(date >= '2020-01-01') %>% 
    select(author,date,content) %>%
    mutate(date = floor_date(date,"month"))

cleansed_authors_from_2020 = cleansed_users %>% 
    group_by(author,date) %>% 
    summarize(ct = n() ) %>% 
    ungroup() %>% 
    arrange((ct)) %>%
    filter(!grepl(("\\#0000"),author)) 

cleansed_user_overview = user_post_review(cleansed_authors_from_2020)


exclusive_cleansed_user_overview = exclusive_user_post_review(cleansed_authors_from_2020)



# Remove - Admins, Mods, Founding Members ---------------------------------
remove_admins_mods_fm = discord_review %>%
    clean_names() %>%
    filter(!grepl(("\\#0000"),author)) %>% 
    filter(grepl("(super|mod|neverland)",channel)) %>%
    select(author) %>%
    distinct()

cleansed_fm_users = discord_review %>% 
    clean_names() %>% 
    filter(!grepl("(auction|autobot|watch|neverland|super|mods|cast)",channel)) %>%
    filter(author %!in% remove_admins_mods_fm$author ) %>%
    filter(date >= '2020-01-01') %>% 
    select(author,date,content) %>%
    mutate(date = floor_date(date,"month"))

cleansed_fm_authors_from_2020 = cleansed_fm_users %>% 
    group_by(author,date) %>% 
    summarize(ct = n() ) %>% 
    ungroup() %>% 
    arrange((ct)) %>%
    filter(!grepl(("\\#0000"),author)) 

cleansed_fm_user_overview = user_post_review(cleansed_fm_authors_from_2020)
exclusive_fm_cleansed_user_overview = exclusive_user_post_review(cleansed_fm_authors_from_2020)

pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium)

patches = read_json(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "personal_data.json"))

google_auths = function(){
    options(httr_oob_default=TRUE) 
    options(gargle_oauth_email = patches$og_patches, use_oob=TRUE)
    drive_auth(email = patches$og_patches,use_oob=TRUE)
    gs4_auth(email = patches$og_patches,use_oob=TRUE)
    suppressMessages(gc())
}
google_auths()
ss <- drive_get("Discord Logs")

sheet_write(
    exclusive_all_user_overview[[1]],
    ss = ss,
    sheet = "all_users"
)

sheet_write(
    exclusive_cleansed_user_overview[[1]],
    ss = ss,
    sheet = "admin/mod_removed_users"
)

sheet_write(
    exclusive_fm_cleansed_user_overview[[1]],
    ss = ss,
    sheet = "true_users"
)

sheet_write(
    exclusive_all_user_overview[[2]],
    ss = ss,
    sheet = "all_users_trends"
)

sheet_write(
    exclusive_cleansed_user_overview[[2]],
    ss = ss,
    sheet = "admin/mod_removed_users_trends"
)

sheet_write(
    exclusive_fm_cleansed_user_overview[[2]],
    ss = ss,
    sheet = "true_users_trends"
)



# ** Putting it all together ----

text_recipe_5B <- recipe(content ~ ., data = author_content_all) %>%
    step_tokenize(content) %>%
    step_stopwords(content) %>%
    step_ngram(
        content,
        num_tokens     = 3, 
        min_num_tokens = 1
    ) %>%
    step_tokenfilter(
        content,
        max_tokens = 100
    ) %>%
    step_tfidf(content)


text_recipe_5B %>% prep() %>% juice()


