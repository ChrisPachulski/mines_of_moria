pacman::p_load(tidyverse,lubridate,anytime,hms)
server_data = read_csv("/home/cujo253/mines_of_moria/ban_logs/logs/BAN Arbitrage Network - Admin - 🚨server-dump [769323295526748160].csv")


server_data %>% mutate_if(is.character,as.factor) %>% summary()

server_data_date_cleansed_tbl = server_data %>% 
    separate(Date, c("Date","Time","PM"),sep = " ") %>% 
    mutate(Date = as.Date(Date, format = "%d-%b-%y"),
           Time = paste(Time,PM)) %>%
    select(-PM) %>%
    mutate(Time = as_hms(ymd_hms(parse_date_time(Time, "%I:%M %p"))))

unique_card_searches = server_data_date_cleansed_tbl %>% 
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
    filter(is.na(user_details) == F) %>%
    separate(user_details, c("user","domain"),sep="@") %>%
    filter(nchar(user) > 3) %>%
    select(-Attachments,-Reactions) %>%
    filter(Date >= "2021-02-01") %>%
    filter(grepl("\\/search( by )*",user)==F)


unique_card_searches %>% 
    mutate(month = month(Date)) %>%
    group_by(user,month) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>% 
    arrange(desc(cards_searched))

ovr_user = unique_card_searches %>%
    #mutate(month = month(Date)) %>%
    group_by(user,domain) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>% drop_na()

ovr_user %>% filter(grepl("(coolstuffinc|starcitygames)",domain)==F ) #%>% view()

ovr_domain = unique_card_searches %>%
    #mutate(month = month(Date)) %>%
    group_by(domain) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched))

ovr_avg_user_card_tbl = tested_card_searches %>%
    filter(grepl("(coolstuffinc|starcitygames)",domain)==F ) %>%
    filter(Date >= "2021-05-01") %>%
    filter(Date <= "2021-06-15") %>%
    filter(pass_status == "pass") %>%
    group_by(card) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched)) %>%
    mutate(users_total_searches = sum(cards_searched),
           users_distribution = round(cards_searched/users_total_searches,3)) %>%
    rename(user_searches = cards_searched)


ovr_vendors_card_tbl = unique_card_searches %>%
    filter( (grepl("(coolstuffinc|starcitygames)",domain)==T) & grepl("",user) ) %>%
    filter(Date >= "2021-05-01") %>%
    group_by(card) %>%
    summarize(cards_searched = n()) %>%
    ungroup() %>%
    arrange(desc(cards_searched))  %>%
    mutate(vendor_total_searches = sum(cards_searched),
           vendor_distribution = round(cards_searched/vendor_total_searches,3))%>%
    rename(vendor_searches = cards_searched)

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
