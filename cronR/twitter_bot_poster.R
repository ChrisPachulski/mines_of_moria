source("config.R")
pacman::p_load(tidyverse,ggplot2,ggrepel,bigrquery,googlesheets4,googledrive,jsonlite,janitor,tidyRSS,lubridate,anytime,rtweet,magick,gmailr)
patches = read_json(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "personal_data.json"))

gm_auth_configure(path = file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "gmail_ids.json"),use_oob=T)
gm_auth(email = patches$patches, use_oob = T)

already_posted_logic = read_csv(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "twitter_bot_logic_check.csv")) %>%
    filter(date == max(date))

if(already_posted_logic$date < Sys.Date()){
    tryCatch({
        my_threads <- gm_threads(search = paste0('Re: ',Sys.Date(),": Twitter"), num_results = 10)
        
        # retrieve the latest thread by retrieving the first ID
        latest_thread <- gm_thread(gm_id(my_threads)[[1]])
        Subject = latest_thread$messages[[1]]$payload$headers[[6]]$value
        # Overwrite existing media for content in email being manually approved.
        setwd(file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', ''))
        attachment = gm_save_attachments(latest_thread$messages[[1]])
        
        # Check my reply against acceptable answers to post content to twitter  
        # By ripping thr body out of the email and looking for the go/no go command
        user_response = gm_body(latest_thread$messages[[2]]) %>% .[[1]] %>% gsub("\\r.*","",.) %>% tolower()
        answers = list("yes","please","make it so","dew it","do it","good","yea(h)*","fire","sure","ok(ay)*","alright","why not","great","post","send")
        
        if(user_response %in% answers){
            
            twitter_api_keys = read_json(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "twitter_api_wolfoftinstreet.json"))
            
            # Create a token containing your Twitter keys
            token_for_wolf= rtweet::create_token(
                app = "automatic_poster_app",  # the name of the Twitter app
                consumer_key = twitter_api_keys$consumer_key,
                consumer_secret = twitter_api_keys$consumer_key_secret,
                access_token = twitter_api_keys$access_token,
                access_secret = twitter_api_keys$access_token_secret
            )
            
            tweet_status = gm_body(latest_thread$messages[[1]]) %>% .[[1]] %>% gsub("\\s*\\n\\nShall I post this for you.*","",.) 
            
            rtweet::post_tweet(
                status = tweet_status,
                media = c(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "twitter_media.png")),
                media_alt_text = 'Plot',
                token = token_for_wolf
            )
            
            write_csv(tibble(date = Sys.Date(), status = "logic completed"),file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "twitter_bot_logic_check.csv"))
            
        }else{
            
            gm_auth_configure(path = file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "gmail_ids.json"),use_oob=T)
            
            gm_auth(email = patches$patches, use_oob = T)
            
            tweet_status = gm_body(latest_thread$messages[[1]]) %>% .[[1]] %>% gsub("\\s*\\n\\nShall I post this for you.*","",.) 
            
            my_email = gm_mime() %>%
                gm_to(patches$patches) %>%
                gm_from(patches$patches) %>%
                gm_subject(paste0(Sys.Date(),": Declined Tweet Content")) %>%
                gm_text_body(paste0(tweet_status," 
        
        Was not Published. Please publish manually if still desired." )) %>% 
                gm_attach_file(file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "twitter_media.png"))
            
            gm_send_message(my_email)
            
            write_csv(tibble(date = Sys.Date(), status = "logic completed"),file.path(path_prefix, "mines_of_moria", "Essential_Referential_CSVS", "twitter_bot_logic_check.csv"))
            
            
        }
    }, 
        error = function(e){print("Still Awaiting Confirmation Email.")})
}
