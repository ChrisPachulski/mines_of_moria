pacman::p_load(tidyverse,rvest,assertthat,devtools,googlesheets4,googledrive,janitor,broom)

options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv") %>% rename(name=card,Rarity=rarity)
currentDate <- Sys.Date() - 161


preliminary_tbl = data.frame(style = "preliminary", format = c("vintage","pauper","standard","pioneer","modern","legacy"))
league_tbl = data.frame(style = "league", format = c("vintage","pauper","standard","pioneer","modern","legacy"))

formats_tbl = rbind(league_tbl,preliminary_tbl)


retrieve_decklists = function(formats_tbl,days = 30){
    all_decklists = NULL
    for(d in 1:days){
        desired_date = (Sys.Date() + 1) - d
        for(i in 1:nrow(formats_tbl)){
            url = paste("https://magic.wizards.com/en/articles/archive/mtgo-standings/",formats_tbl$format[i],"-",formats_tbl$style[i],"-",desired_date,sep="")
            tryCatch(
                data <- url %>% 
                    read_html() %>% 
                    html_nodes(".sorted-by-overview-container") %>% 
                    html_text(), 
                error = function(e){next}    # a function that returns NA regardless of what it's passed
            )
            Deck_Number <- as.data.frame(data)
            Deck_number <- nrow(Deck_Number)
            #
            
            tryCatch(
                Card_Totals <- url %>% 
                    read_html() %>% 
                    html_nodes(".card-count") %>% 
                    html_text(), 
                error = function(e){next}    # a function that returns NA regardless of what it's passed
            )
            Card_Totals <- as.data.frame(Card_Totals)
            
            tryCatch(
                Card_Name <- url %>% 
                    read_html() %>% 
                    html_nodes(".card-name") %>% 
                    html_text(), 
                error = function(e){next}    # a function that returns NA regardless of what it's passed
            )
            Card_Name <- as.data.frame(Card_Name)
            
            Combined_Decklists <- tibble(Card_Totals,
                                         Card_Name)
            Combined_Decklists <- as.data.frame(Combined_Decklists)
            names(Combined_Decklists) <- c("QTY","CARD")
            
            Combined_Decklists <- as.data.frame(Combined_Decklists)
            Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
            Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
            Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
            Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
            Combined_Decklists <- as.data.frame(Combined_Decklists)
            
            if(dim(Combined_Decklists)[1] != 0 ){
                Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
            } else {
                Final_Decklist_Results <- 0
                Reduced_Decklists <- as.data.frame(0)
                print("No Results Were Found")
            }
            if(dim(Reduced_Decklists)[1] > 1){
                List <- Reduced_Decklists$CARD
                ABC <- NULL
                for(p in List){
                    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == p)
                    df_sub<- as.data.frame(df_sub)
                    A  <- round(mean(df_sub$QTY),2)
                    ABC <- rbind(ABC,A)
                }
                Reduced_Decklists$Avg_Copies <- ABC
                
                Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
                Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
                Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
                Reduced_Decklists <- as.data.frame(Reduced_Decklists)
                Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
                #View(Reduced_Decklists)
                Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
                Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
                Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
                Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
                Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
                Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
                Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
                Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
                Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
                #View(Reduced_Decklists)
                Final_Decklist_Results <- Reduced_Decklists[,1:4]
                Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
                Final_Decklist_Results$Date <- desired_date
                Final_Decklist_Results$Sample_Size <- Deck_number
                Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
                Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
                Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
                Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
                Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
                Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
                Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
                Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
                Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
                Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
                Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
                Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
                Final_Decklist_Results$Format = formats_tbl$format[i]
                Final_Decklist_Results$Style = formats_tbl$style[i]
                
                all_decklists = rbind(all_decklists,Final_Decklist_Results)
            }
        }
    }
    
    return(all_decklists)
}

decklists = retrieve_decklists(formats_tbl,14)

decklist_cleaned = decklists %>% rename(decks_in_tournament=Sample_Size) %>% clean_names() %>%
    mutate(format = as.factor(as.character(format)), style = as.factor(as.character(style)), rarity = as.factor(as.character(rarity)))

ss <- drive_get("Decklists For Ban")
gs4_auth(email="pachun95@gmail.com", use_oob = T)
sheet_write(
    decklist_cleaned,
    ss = ss,
    sheet = "all"
)
