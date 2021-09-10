pacman::p_load(tidyverse,lubridate,anytime,hms,ggrepel,stringi,ggplot2)
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
        mutate(user = lower(user))
    
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
            mutate(month = month(Date)) %>%
            group_by(user,!!column_by) %>%
            summarize(cards_searched = n()) %>%
            ungroup() %>% 
            arrange(desc(cards_searched),desc(!!column_by)) },
        error = function(e){
            server_data %>% 
                mutate(month = month(Date)) %>%
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
                                mutate(category = gsub(" -.*","",gsub("(BAN Arbitrage Network - )","",list.files(pattern = "*.csv")[i] %>% deparse(substitute(.))))) %>%
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

card_search_group_users_by(card_searches,month) 
card_search_group_users_by(card_searches,domain) 

card_search_group_users_by(card_searches) 
card_search_group_by(card_searches,domain)

users_per_month = card_search_group_users_by(card_searches,month)  %>% 
    #select(-cards_searched) %>%
    group_by(month) %>% 
    summarize(total_user = n(),
              total_searches = sum(cards_searched)) %>%
    ungroup()




# Discord Only ------------------------------------------------------------


discord_review = clean_discord_only()

category_posts = discord_review %>% 
    group_by(category) %>%
    summarize(total_posts = n()) %>%
    ungroup() %>%
    arrange(desc(total_posts))

channel_posts = discord_review %>%
    filter(channel != "server-dump") %>%
    mutate(Date = floor_date(Date,"month")) %>%
    group_by(channel,Date) %>%
    summarize(total_posts = n()) %>%
    ungroup() %>%
    arrange(desc(total_posts))


# Magic Discord Only ------------------------------------------------------
for(q in 1:length(unique(discord_review$category))){

    category_selection = tolower(unique(discord_review$category)[q])
    
    if(grepl("not public",category_selection)){next}
    
    ovr_category_view = discord_review %>%
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
            filter(channel == ovr_category_view$channel[i]) %>%
            filter(total_posts == max(total_posts))}
        
        if(i == 3){
        max_point_3 = ovr_category_view %>%
            filter(channel == ovr_category_view$channel[i]) %>%
            filter(total_posts == max(total_posts))
        }
        
        max_point = rbind(max_point_1,max_point_2,max_point_3)
    }
    print(ggplot(ovr_category_view, aes(x = Date, y = total_posts, color = channel, label = total_posts)) +
        geom_line() +
        scale_x_date(date_labels="%m",date_breaks  ="1 month") +
        geom_point(data = latest_values, aes(x = Date, y = total_posts), shape = 21, fill = "white", size = 2, stroke = 1.7) +
        geom_text_repel(data = latest_values, aes(x = Date, y = total_posts, label = total_posts), size = 4, vjust = 1.25, hjust = .5 )+
        geom_point(data = max_point, aes(x = Date, y = total_posts), shape = 21, fill = "white", size = 2, stroke = 1.7) +
        geom_text_repel(data = max_point, aes(x = Date, y = total_posts, label = total_posts), color = "dark green",size = 4, vjust = 1.25, hjust = .75 ) +
        ggtitle(paste(str_extract(unique(discord_review$category)[q],"[A-Za-z]+")," Channels",sep="")) + ylab("Total Posts") + xlab("Month") +
        theme(plot.title = element_text(hjust = 0.5)))
}









ovr_user %>% filter(grepl("(coolstuffinc|starcitygames)",domain)==F ) #%>% view()


card_searches %>% 
    mutate(month = month(Date))%>%
    filter(grepl("(coolstuffinc)",domain)==T ) %>%
    filter(Date >= "2021-02-01") %>%
    group_by(month,card) %>%
    summarize(ct = n()) %>%
    ungroup() %>%
    arrange(desc(ct)) %>%
    filter(month == 8)


ovr_avg_user_card_tbl %>%
    left_join(ovr_vendors_card_tbl,by=c("card"="card"))

pass_tbl = NULL

unique_cards = unique_card_searches %>%
    filter(grepl("\\/search",user)==F) %>%
    select(card) %>% distinct()

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


tested_card_searches %>% filter(grepl("jacob",user)) %>% select(user) %>% distinct()

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

newspaper_unique_card_searches = server_data_date_cleansed_tbl %>% 
    mutate(Content = as.character(as.factor(Content))) %>% 
    separate(Content,c("card","user_details"),sep = " from ") %>% 
    arrange(desc(Date)) %>% 
    filter(grepl("\\/newspaper",user_details)==T) %>%
    mutate(user_details = gsub("\\/newspaper by ","",user_details)) %>%
    filter(is.na(user_details) == F) %>%
    separate(user_details, c("user","domain"),sep="@") %>%
    filter(nchar(user) > 3) %>%
    select(-Attachments,-Reactions) 


unique_card_searches %>% nrow()
newspaper_unique_card_searches %>% nrow() 

newspaper_ovr_user = newspaper_unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(user,month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na()

newspaper_ovr_month = newspaper_unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na() %>%
    arrange(month)

newspaper_searches_by_month_tbl = newspaper_unique_card_searches %>%
    mutate(month = floor_date(Date,unit="month")) %>%
    group_by(month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(month) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")


# Sleeper Usage Rates -----------------------------------------------------

sleepers_unique_card_searches = server_data_date_cleansed_tbl %>% 
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


unique_card_searches %>% nrow()
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
    rename(user_searches = cards_searched) %>% 
    filter(month <= "2021-06-25")

# Global Usage Rates ------------------------------------------------------

global_unique_card_searches = server_data_date_cleansed_tbl %>% 
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
    filter(month <= "2021-06-25")

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
