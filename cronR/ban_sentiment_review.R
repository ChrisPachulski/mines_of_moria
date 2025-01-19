pacman::p_load(tidyverse,lubridate,anytime,hms,ggrepel,stringi,ggplot2,textclean,tidymodels,textrecipes,tidytext,textdata,wordcloud)
setwd("/home/cujo253/mines_of_moria/ban_logs/logs/")
options(scipen=999)
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

base_server_data = read_csv('/home/cujo253/mines_of_moria/Essential_Referential_CSVS/base_discord_review_11_28_22.csv')

channel_count = base_server_data %>% clean_names() %>%
    mutate(year_lab = year(date),month_lab = month(date)) %>%
    group_by(year_lab,channel) %>% 
    tally() %>% 
    rename(post = n)

thing = channel_count %>%
    filter(year_lab == 2022) %>%
    arrange(desc(post)) %>% 
    mutate(channel = gsub('MTG','',channel)) %>% 
    mutate(channel = str_replace_all(string = channel,
                                     pattern = '[^[:alnum:][:space:]]',
                                     replacement = ""))%>% write_csv('/home/cujo253/channels.csv')

#card_searches = tool_unique_card_search(base_server_data)
my_stop_words = stop_words %>% 
    bind_rows(tibble(data.frame(word = 'yeah', lexicon = 'cjp'))) %>% 
    bind_rows(tibble(data.frame(word = 'ill', lexicon = 'cjp'))) %>% 
    bind_rows(tibble(data.frame(word = 'pretty', lexicon = 'cjp'))) %>% 
    bind_rows(tibble(data.frame(word = 'discord', lexicon = 'cjp'))) %>% 
    bind_rows(tibble(data.frame(word = 'ban', lexicon = 'cjp'))) %>% 
    bind_rows(tibble(data.frame(word = 'auction', lexicon = 'cjp')))

discover_sentiment = function(data = base_server_data, lexicon_choice = 'nrc', author = 'Charly', from = '2018-01-01', to = '2100-01-01'){
    
    tokenized_comments = data %>%
        filter(grepl(author,Author)) %>%
        filter(Date >= from) %>%
        filter(Date <= to) %>%
        select(Content) %>%
        rowid_to_column() %>%
        unnest_tokens(output = word,
                      input = Content,
                      to_lower = T,
                      token = 'ngrams',
                      n = 1,
                      n_min = 1,
                      stopwords = my_stop_words %>% pull(word)) %>%
        drop_na()
    
    if( lexicon_choice == 'nrc'){
        
        emotional_comments_tbl = tokenized_comments %>%
            inner_join(get_sentiments(lexicon = lexicon_choice))
        
        if(emotional_comments_tbl %>%filter(sentiment == 'anticipation') %>% nrow() == 0){
            emotional_comments_tbl =  emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'anticipation') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'joy') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'joy') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'positive') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'positive') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'surprise') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'surprise') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'trust') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'trust') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'negative') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'negative') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'anger') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'anger') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'disgust') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'disgust') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'fear') %>% nrow() == 0){
            emotional_comments_tbl =  emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'fear') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'sadness') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'sadness') ))
        }
        
        
        
        aggregated_emotions = emotional_comments_tbl %>%
            select(-word) %>%
            count(rowid, sentiment) %>%
            pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0)) %>%
            mutate(sentiment = positive - negative) %>%
            left_join(
                data %>%
                    filter(grepl(author,Author)) %>%
                    filter(Date >= from) %>%
                    filter(Date <= to) %>%
                    select(Content,Date) %>%
                    rowid_to_column() 
            ) %>% drop_na()
        
        daily_review = aggregated_emotions %>%
            group_by(Date) %>%
            summarize(
                anger  = sum(anger),
                disgust  = sum(disgust),
                trust  = sum(trust),
                fear  = sum(fear),
                anticipation  = sum(anticipation),
                sadness    = sum(sadness),
                joy  = sum(joy),
                surprise  = sum(surprise),
                positive = sum(positive),
                negative = sum(negative)
            ) %>%
            left_join(
                aggregated_emotions %>%
                    group_by(Date) %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        ovr_review = aggregated_emotions %>%
            summarize(
                anger  = sum(anger),
                disgust  = sum(disgust),
                trust  = sum(trust),
                fear  = sum(fear),
                anticipation  = sum(anticipation),
                sadness    = sum(sadness),
                joy  = sum(joy),
                surprise  = sum(surprise),
                positive = sum(positive),
                negative = sum(negative)
            ) %>%
            cbind(
                aggregated_emotions %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        return(list(ovr_review,daily_review,emotional_comments_tbl))
    }

    if( lexicon_choice == 'bing'){
        
        emotional_comments_tbl = tokenized_comments %>%
            inner_join(get_sentiments(lexicon = lexicon_choice))
        
        
        if(emotional_comments_tbl %>%filter(sentiment == 'positive') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'positive') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'negative') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'negative') ))
        }
        
        
        aggregated_emotions = emotional_comments_tbl %>%
            select(-word) %>%
            count(rowid, sentiment) %>%
            pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0)) %>%
            mutate(sentiment = positive - negative) %>%
            left_join(
                data %>%
                    filter(grepl(author,Author)) %>%
                    filter(Date >= from) %>%
                    filter(Date <= to) %>%
                    select(Content,Date) %>%
                    rowid_to_column() 
            ) %>% drop_na()
        
        daily_review = aggregated_emotions %>%
            group_by(Date) %>%
            summarize(
                positive = sum(positive),
                negative = sum(negative)
            ) %>%
            left_join(
                aggregated_emotions %>%
                    group_by(Date) %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        ovr_review = aggregated_emotions %>%
            summarize(
                positive = sum(positive),
                negative = sum(negative)
            ) %>%
            cbind(
                aggregated_emotions %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        return(list(ovr_review,daily_review,emotional_comments_tbl))
    }
    
    if( lexicon_choice == 'afinn'){
        
        emotional_comments_tbl = tokenized_comments %>%
            inner_join(get_sentiments(lexicon = lexicon_choice)) %>%
            rename(sentiment = value) %>% distinct()
        
        aggregated_emotions = emotional_comments_tbl %>%
            select(-word) %>%
            left_join(
                data %>%
                    filter(grepl(author,Author)) %>%
                    filter(Date >= from) %>%
                    filter(Date <= to) %>%
                    select(Content,Date) %>%
                    rowid_to_column() 
            ) %>% drop_na()
        
        daily_review = aggregated_emotions %>%
            group_by(Date) %>%
            summarize(
                sentiment = sum(sentiment)
            ) %>%
            left_join(
                aggregated_emotions %>%
                    group_by(Date) %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        ovr_review = aggregated_emotions %>%
            summarize(
                sentiment = sum(sentiment)
            ) %>%
            cbind(
                aggregated_emotions %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        return(list(ovr_review,daily_review,emotional_comments_tbl))
    }
    
    if( lexicon_choice == 'loughran'){
        
        emotional_comments_tbl = tokenized_comments %>%
            inner_join(get_sentiments(lexicon = lexicon_choice))
        
        
        if(emotional_comments_tbl %>%filter(sentiment == 'uncertainty') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'uncertainty') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'constraining') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'constraining') ))
        }        
        if(emotional_comments_tbl %>%filter(sentiment == 'litigious') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'litigious') ))
        }
        if(emotional_comments_tbl %>%filter(sentiment == 'superfluous') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'superfluous') ))
        }  
        if(emotional_comments_tbl %>%filter(sentiment == 'positive') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'positive') ))
        }  
        if(emotional_comments_tbl %>%filter(sentiment == 'negative') %>% nrow() == 0){
            emotional_comments_tbl = emotional_comments_tbl %>% rbind(tibble(data.frame(rowid = NA, word = NA, sentiment = 'negative') ))
        }  
        
        
        aggregated_emotions = emotional_comments_tbl %>%
            select(-word) %>%
            count(rowid, sentiment) %>%
            pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n=0)) %>%
            mutate(sentiment = positive - negative) %>%
            left_join(
                data %>%
                    filter(grepl(author,Author)) %>%
                    filter(Date >= from) %>%
                    filter(Date <= to) %>%
                    select(Content,Date) %>%
                    rowid_to_column() 
            ) %>% drop_na()
        
        daily_review = aggregated_emotions %>%
            group_by(Date) %>%
            summarize(
                uncertainty = sum(uncertainty),
                constraining = sum(constraining),
                litigious = sum(litigious),
                superfluous = sum(superfluous),
                positive = sum(positive),
                negative = sum(negative)
            ) %>%
            left_join(
                aggregated_emotions %>%
                    group_by(Date) %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        ovr_review = aggregated_emotions %>%
            summarize(
                uncertainty = sum(uncertainty),
                constraining = sum(constraining),
                litigious = sum(litigious),
                superfluous = sum(superfluous),
                positive = sum(positive),
                negative = sum(negative)
            ) %>%
            cbind(
                aggregated_emotions %>%
                    tally() %>%
                    rename(posts = n)
            ) %>%
            mutate(Author = author) %>%
            select(Author, everything())
        
        return(list(ovr_review,daily_review,emotional_comments_tbl))
    }
    
}


active_authors = base_server_data %>%
    filter(Date >= '2018-01-01') %>%
    filter(Date <= '2100-01-01') %>% 
    select(Author) %>%
    filter(!grepl('.*\\#0000',Author)) %>% 
    distinct() 

# nrc ---------------------------------------------------------------------

n_iter <- active_authors %>% nrow()

pb <- txtProgressBar(min = 0,
                     max = n_iter,
                     style = 3,
                     width = n_iter, # Needed to avoid multiple printings
                     char = "=") 

init <- numeric(n_iter)
end <- numeric(n_iter)


nrc_ovr_tbl = NULL
nrc_daily_tbl = NULL
nrc_raw_ngrams = NULL
for(i in 1:nrow(active_authors)){
    
    init[i] <- Sys.time()
    
    results = suppressMessages(discover_sentiment(data = base_server_data, lexicon_choice = 'nrc', author = active_authors[i,] %>% unlist(), from = '2018-01-01', to = '2100-01-01'))
    nrc_ovr_tbl = bind_rows(nrc_ovr_tbl,results[[1]]) %>% distinct()
    nrc_daily_tbl = bind_rows(nrc_daily_tbl,results[[2]]) %>% distinct()
    nrc_raw_ngrams = bind_rows(nrc_raw_ngrams,results[[3]]) %>% distinct()
    
    end[i] <- Sys.time()
    
    setTxtProgressBar(pb, i)
    time <- round(seconds_to_period(sum(end - init)), 0)
    
    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining), "")
    }

close(pb)


nrc_ovr_tbl %>%
    filter(!grepl('.*\\#0000',Author)) %>%
    arrange(desc(posts)) %>%
    group_by(Author) %>%
    summarize(anger,disgust,trust,fear,anticipation,sadness,joy,surprise,
              emotions = sum(anger,disgust,trust,fear,anticipation,sadness,joy,surprise),
              positive, negative, sentiment = positive - negative,posts,
              anger_perc = scales::percent(anger/emotions),
              disgust_perc = scales::percent(disgust/emotions),
              trust_perc = scales::percent(trust/emotions),
              fear_perc = scales::percent(fear/emotions),
              anticipation_perc = scales::percent(anticipation/emotions),
              sadness_perc = scales::percent(sadness/emotions),
              joy_perc = scales::percent(joy/emotions),
              surprise_perc = scales::percent(surprise/emotions)) %>%
    arrange(desc(posts)) %>% view()

# afinn ---------------------------------------------------------------------

n_iter <- active_authors %>% nrow()

pb <- txtProgressBar(min = 0,
                     max = n_iter,
                     style = 3,
                     width = n_iter, # Needed to avoid multiple printings
                     char = "=") 

init <- numeric(n_iter)
end <- numeric(n_iter)

i = 1
afinn_ovr_tbl = NULL
afinn_daily_tbl = NULL
afinn_raw_ngrams = NULL
for(i in 1:nrow(active_authors)){
    
    init[i] <- Sys.time()
    
    results = suppressMessages(discover_sentiment(data = base_server_data, lexicon_choice = 'afinn', author = active_authors[i,] %>% unlist(), from = '2018-01-01', to = '2100-01-01'))
    afinn_ovr_tbl = bind_rows(afinn_ovr_tbl,results[[1]]) %>% distinct()
    afinn_daily_tbl = bind_rows(afinn_daily_tbl,results[[2]]) %>% distinct()
    afinn_raw_ngrams = bind_rows(afinn_raw_ngrams,results[[3]]) %>% distinct()
    
    end[i] <- Sys.time()
    
    setTxtProgressBar(pb, i)
    time <- round(seconds_to_period(sum(end - init)), 0)
    
    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining), "")
}

close(pb)

# bing ---------------------------------------------------------------------

n_iter <- active_authors %>% nrow()

pb <- txtProgressBar(min = 0,
                     max = n_iter,
                     style = 3,
                     width = n_iter, # Needed to avoid multiple printings
                     char = "=") 

init <- numeric(n_iter)
end <- numeric(n_iter)


bing_ovr_tbl = NULL
bing_daily_tbl = NULL
bing_raw_ngrams = NULL

for(i in 1:nrow(active_authors)){
    
    init[i] <- Sys.time()
    
    results = suppressMessages(discover_sentiment(data = base_server_data, lexicon_choice = 'bing', author = active_authors[i,] %>% unlist(), from = '2018-01-01', to = '2100-01-01'))
    bing_ovr_tbl = bind_rows(bing_ovr_tbl,results[[1]]) %>% distinct()
    bing_daily_tbl = bind_rows(bing_daily_tbl,results[[2]]) %>% distinct()
    bing_raw_ngrams = bind_rows(bing_raw_ngrams,results[[3]]) %>% distinct()
    
    end[i] <- Sys.time()
    
    setTxtProgressBar(pb, i)
    time <- round(seconds_to_period(sum(end - init)), 0)
    
    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining), "")
}

close(pb)

# loughran ---------------------------------------------------------------------

n_iter <- active_authors %>% nrow()

pb <- txtProgressBar(min = 0,
                     max = n_iter,
                     style = 3,
                     width = n_iter, # Needed to avoid multiple printings
                     char = "=") 

init <- numeric(n_iter)
end <- numeric(n_iter)


loughran_ovr_tbl = NULL
loughran_daily_tbl = NULL
loughran_raw_ngrams = NULL
i = 1
for(i in 1:nrow(active_authors)){
    
    init[i] <- Sys.time()
    
    results = suppressMessages(discover_sentiment(data = base_server_data, lexicon_choice = 'loughran', author = active_authors[i,] %>% unlist(), from = '2018-01-01', to = '2100-01-01'))
    loughran_ovr_tbl = bind_rows(loughran_ovr_tbl,results[[1]]) %>% distinct()
    loughran_daily_tbl = bind_rows(loughran_daily_tbl,results[[2]]) %>% distinct()
    loughran_raw_ngrams = bind_rows(loughran_raw_ngrams,results[[3]]) %>% distinct()
    
    end[i] <- Sys.time()
    
    setTxtProgressBar(pb, i)
    time <- round(seconds_to_period(sum(end - init)), 0)
    
    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    est <- n_iter * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining), "")
}

close(pb)


# Review ------------------------------------------------------------------

afinn_ovr_tbl

loughran_ovr_tbl %>%
    mutate(perc = round(negative/posts,2)) %>%
    filter(posts >= 500) %>%
    arrange(desc(perc))

bing_ovr_tbl %>%
    mutate(neg_perc = round(negative/posts,2)) %>%
    filter(posts >= 500) %>%
    arrange(desc(neg_perc))

afinn_ovr_tbl %>%
    filter(posts >= 500) %>%
    arrange(desc(sentiment))

afinn_ovr_tbl %>%
    arrange((sentiment))

bing_ovr_tbl %>%
    arrange((sentiment))

# Wordclouds --------------------------------------------------------------

base_server_data %>%
    filter(grepl('Wolf.*',Author)) %>%
    anti_join(my_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))

base_server_data %>%
    filter(grepl('.*elmo*',Author)) %>%
    mutate(Content = gsub('https.*($|\\s)',' ',Content)) %>% 
    mutate(Content = gsub('\\d+(\\.\\d+)*','',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(LP|lp)(\\s|$)',' lightly played ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(NM|nm)(\\s|$)',' near mint ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(Sec|sec)(\\s|$)',' give me a second ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(Min|min)(\\s|$)',' give me a minute ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(LOL|lol)(\\s|$)',' laugh out loud ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(CK|ck)(\\s|$)',' cardkingdom ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(BC|bc)(\\s|$)',' because ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(TCG|tcg)(\\s|$)',' tcgplayer ',Content)) %>%
    mutate(Content = gsub('(^|\\s)(MTG|mtg|Mtg)(\\s)*price(\\s|$)',' chillcot ',Content)) %>%  
    mutate(Content = gsub('(^|\\s)(MTG|mtg)(\\s|$)',' magic the gathering ',Content)) %>%   
    mutate(Content = gsub('(^|\\s)(BL|bl)(\\s|$)',' buylist ',Content)) %>%  
    mutate(Content = gsub('(^|\\s)(EV|ev)(\\s|$)',' estimated value ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(MKT|mkt)(\\s|$)',' market ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(XD|xd)(\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('\\@[A-Za-z]+(\\\\n)*(\\s)*',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z]{3}\\W[A-Za-z](\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z]{2}\\W[A-Za-z](\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z]{1}\\W[A-Za-z](\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(dont|Dont)(\\s|$)',' do not ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z][A-Za-z]*[A-Za-z]*[A-Za-z]*(\\s|$)','',Content)) %>% 
    select(Content) %>%
    rowid_to_column() %>%
    unnest_tokens(output = word,
                  input = Content,
                  to_lower = T,
                  token = 'ngrams',
                  n = 1,
                  n_min = 1,
                  stopwords = my_stop_words %>% pull(word)) %>%
    drop_na() %>%
    anti_join(my_stop_words) %>%
    count(word) %>%
    arrange(desc(n)) %>%
    filter(word != 'na') %>%
    filter(word != 'xd') %>%
    with(wordcloud(word, n, max.words = 20))


library(reshape2)

tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("gray20", "gray80"),
                     max.words = 100)


base_server_data %>%
    filter(grepl('.*Bacon*',Author)) %>%
    mutate(Content = gsub('https.*($|\\s)',' ',Content)) %>% 
    mutate(Content = gsub('\\d+(\\.\\d+)*','',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(LP|lp)(\\s|$)',' lightly played ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(NM|nm)(\\s|$)',' near mint ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(Sec|sec)(\\s|$)',' give me a second ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(Min|min)(\\s|$)',' give me a minute ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(LOL|lol)(\\s|$)',' laugh out loud ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(CK|ck)(\\s|$)',' cardkingdom ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(BC|bc)(\\s|$)',' because ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(TCG|tcg)(\\s|$)',' tcgplayer ',Content)) %>%
    mutate(Content = gsub('(^|\\s)(MTG|mtg|Mtg)(\\s)*price(\\s|$)',' chillcot ',Content)) %>%  
    mutate(Content = gsub('(^|\\s)(MTG|mtg)(\\s|$)',' magic the gathering ',Content)) %>%   
    mutate(Content = gsub('(^|\\s)(BL|bl)(\\s|$)',' buylist ',Content)) %>%  
    mutate(Content = gsub('(^|\\s)(EV|ev)(\\s|$)',' estimated value ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(MKT|mkt)(\\s|$)',' market ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(XD|xd)(\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('\\@[A-Za-z]+(\\\\n)*(\\s)*',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z]{3}\\W[A-Za-z](\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z]{2}\\W[A-Za-z](\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z]{1}\\W[A-Za-z](\\s|$)',' ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)(dont|Dont)(\\s|$)',' do not ',Content)) %>% 
    mutate(Content = gsub('(^|\\s)[A-Za-z][A-Za-z]*[A-Za-z]*[A-Za-z]*(\\s|$)','',Content)) %>% 
    select(Content) %>%
    rowid_to_column() %>%
    unnest_tokens(output = word,
                  input = Content,
                  to_lower = T,
                  token = 'ngrams',
                  n = 1,
                  n_min = 1,
                  stopwords = my_stop_words %>% pull(word)) %>%
    drop_na()%>%
    inner_join(get_sentiments("nrc")) %>%
    filter(grepl('(positive|negative)',sentiment)) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("red", "green"),
                     max.words = 25)
    
    
