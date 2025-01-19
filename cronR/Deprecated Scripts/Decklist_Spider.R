source("config.R")
#Packages & Needed Functions####
library(rvest)     # HTML Hacking & Web Scraping
library(jsonlite)  # JSON manipulation
library(dplyr)     # Data Manipulation
library(tidyr)     # The Janitor is this guy
library(readr)

url <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/modern-league-2020-02-14"
html <- read_html(url)
Deck_Number <- html %>% html_nodes(".sorted-by-overview-container") %>% html_text()
Deck_Number <- as.data.frame(Deck_Number)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv("/cloud/project/Reports/High Confidence Reps/2020-02-10_Premium.csv")

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
Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
View(Final_Decklist_Results)

setwd("/cloud/project/Decklists/Modern Decklists")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Modern_Deck_Lists",".csv",sep="")
write.csv(Final_Decklist_Results, file=csvFileName, row.names = FALSE)
Deck_number
