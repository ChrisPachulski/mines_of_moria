url <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/modern-league-2020-01-03"
html <- read_html(url)
Deck_Number <- html %>% html_nodes(".sorted-by-overview-container") %>% html_text()
Deck_Number <- as.data.frame(Deck_Number)
Deck_number <- nrow(Deck_Number)

Card_Totals <- html %>% html_nodes(".card-count") %>% html_text()
Card_Totals <- as.data.frame(Card_Totals)

Card_Name <- html %>% html_nodes(".card-name") %>% html_text()
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

Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
List <- Reduced_Decklists$CARD
ABC <- NULL
for(i in List){
  df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == print(i))
  df_sub<- as.data.frame(df_sub)
  A  <- round(mean(df_sub$QTY),2)
  ABC <- rbind(ABC,A)
}
Reduced_Decklists$Avg_Copies <- ABC

Reduced_Decklists$Rarity <- Final_Export$Rarity[match(Reduced_Decklists$CARD,Final_Export$Card)]
Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
Reduced_Decklists <- as.data.frame(Reduced_Decklists)
Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
Reduced_Decklists$QTY_Cards <- round(Reduced_Decklists$QTY / 4,0)
Reduced_Decklists$QTY_Lands <- round(Reduced_Decklists$QTY / 3,0)
Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
View(Reduced_Decklists)
Final_Decklist_Results <- Reduced_Decklists[,1:4]

View(Final_Decklist_Results)

setwd("/cloud/project")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Deck_Lists",".csv",sep="")
write.csv(Final_Decklist_Results, file=csvFileName)
