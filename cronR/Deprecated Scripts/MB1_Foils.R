library(easypackages)
libraries("tidyverse","bigrquery","googledrive","devtools","googlesheets4","RSelenium","rvest")
getwd()
Original_MB1_Foils <- read_csv("/home/cujo253/Essential_Referential_CSVS/Retail_Mystery_Booster_Data_Analysis.csv")
Original_MB1_Foils <- Original_MB1_Foils[-1,]
Sets <- suppressWarnings(read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE))
Sets <- as.data.frame(Sets)
suppressWarnings(Sets <- Sets[,c(1:3,5)])
names(Sets) <- c("CK_Modif_Set","Goldfish_Full","Goldfish_Abbrev","TCG_Key")

Original_MB1_Foils$TCG_Sets <- Sets$TCG_Key[match(Original_MB1_Foils$`Similar Art Set`,Sets$Goldfish_Abbrev)]
Original_MB1_Foils$TCG_Sets[is.na(Original_MB1_Foils$TCG_Sets)] <- "10th Edition"
Original_MB1_Foils$TCG_Sets <- ifelse(Original_MB1_Foils$`Card Name` == "Undead Warchief", "timeshifted", as.character(Original_MB1_Foils$TCG_Sets))
MB1_Designation <- "mystery-booster-retail-exclusives"

#Using Erebor's Container
remDr = remoteDriver(remoteServerAddr = "167.99.63.23", port = 4445L, browser = "chrome")
remDr$open()
remDr$maxWindowSize()
Research_Reward <- NULL
remDr$navigate("https://shop.tcgplayer.com/magic/alara-reborn/karrthus-tyrant-of-jund")
Sys.sleep(5)
remDr$findElement("xpath",'//*[@id="detailsFilters"]/div/div/ul[4]/li[3]/a')$clickElement()
Sys.sleep(2)
Original_MB1_Foils <- Original_MB1_Foils[-79,]
for(i in 1:nrow(Original_MB1_Foils)){
  url <- paste("https://shop.tcgplayer.com/magic/",Original_MB1_Foils$TCG_Sets[i],"/",Original_MB1_Foils$`Card Name`[i],sep = "")
  remDr$navigate(url)
  Sys.sleep(5)
  #remDr$navigate("https://shop.tcgplayer.com/magic/alara-reborn/karrthus-tyrant-of-jund")
  Old_Set_Retail <- unlist(remDr$findElement("class", "product-listing__price")$getElementText())
  url <- paste("https://shop.tcgplayer.com/magic/",MB1_Designation,"/",Original_MB1_Foils$`Card Name`[i],sep = "")
  remDr$navigate(url)
  Sys.sleep(5)
  #remDr$navigate("https://shop.tcgplayer.com/magic/mystery-booster-retail-exclusives/karrthus-tyrant-of-jund")
  New_Set_Retail <- unlist(remDr$findElement("class", "product-listing__price")$getElementText())
  Combined_Research <- data.frame(Original_MB1_Foils$`Card Name`[i],Original_MB1_Foils$`Similar Art Set`[i],New_Set_Retail,Old_Set_Retail)
  Combined_Research$New_Set_Retail <- gsub("\\$","", Combined_Research$New_Set_Retail)
  Combined_Research$Old_Set_Retail <- gsub("\\$","", Combined_Research$Old_Set_Retail)
  Combined_Research$New_Set_Retail <- as.numeric(as.character(Combined_Research$New_Set_Retail))
  Combined_Research$Old_Set_Retail <- as.numeric(as.character(Combined_Research$Old_Set_Retail))
  Combined_Research$`Price Difference` <- round(Combined_Research$New_Set_Retail - Combined_Research$Old_Set_Retail,2)
  Combined_Research$`% Difference` <- round(Combined_Research$New_Set_Retail/Combined_Research$Old_Set_Retail,2) - 1
  Final_Card_Evaluation <- Combined_Research
  Research_Reward <- rbind(Research_Reward,Final_Card_Evaluation)
}

Original_MB1_Foils <- read_csv("/home/cujo253/Essential_Referential_CSVS/Retail_Mystery_Booster_Data_Analysis.csv")
Original_MB1_Foils <- Original_MB1_Foils[-1,]
Sets <- suppressWarnings(read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE))
Sets <- as.data.frame(Sets)
suppressWarnings(Sets <- Sets[,c(1:3,5)])
names(Sets) <- c("CK_Modif_Set","Goldfish_Full","Goldfish_Abbrev","TCG_Key")

Original_MB1_Foils$TCG_Sets <- Sets$TCG_Key[match(Original_MB1_Foils$`Similar Art Set`,Sets$Goldfish_Abbrev)]
Original_MB1_Foils$TCG_Sets[is.na(Original_MB1_Foils$TCG_Sets)] <- "10th Edition"
Original_MB1_Foils$TCG_Sets <- ifelse(Original_MB1_Foils$`Card Name` == "Undead Warchief", "timeshifted", as.character(Original_MB1_Foils$TCG_Sets))

Jerk_Card <- Original_MB1_Foils[79,]
url <- paste("https://shop.tcgplayer.com/magic/",Jerk_Card$TCG_Sets[1],"/",Jerk_Card$`Card Name`[1],sep = "")
remDr$navigate(url)
Sys.sleep(5)
Old_Set_Retail <- unlist(remDr$findElement("class", "product-listing__price")$getElementText())
Jerk_Card$`Card Name` <- ifelse(Jerk_Card$`Card Name` == "nezumi-shortfang-stabwhisker-the-odious", "nezumi-shortfang-nezumi-shortfang", as.character(Jerk_Card$`Card Name`))
url <- paste("https://shop.tcgplayer.com/magic/",MB1_Designation,"/",Jerk_Card$`Card Name`[1],sep = "")
remDr$navigate(url)
Sys.sleep(5)
New_Set_Retail <- unlist(remDr$findElement("class", "product-listing__price")$getElementText())
Combined_Research <- data.frame(Jerk_Card$`Card Name`[1],Jerk_Card$`Similar Art Set`[1],New_Set_Retail,Old_Set_Retail)
Combined_Research$New_Set_Retail <- gsub("\\$","", Combined_Research$New_Set_Retail)
Combined_Research$Old_Set_Retail <- gsub("\\$","", Combined_Research$Old_Set_Retail)
Combined_Research$New_Set_Retail <- as.numeric(as.character(Combined_Research$New_Set_Retail))
Combined_Research$Old_Set_Retail <- as.numeric(as.character(Combined_Research$Old_Set_Retail))
Combined_Research$`Price Difference` <- round(Combined_Research$New_Set_Retail - Combined_Research$Old_Set_Retail,2)
Combined_Research$`% Difference` <- round(Combined_Research$New_Set_Retail/Combined_Research$Old_Set_Retail,2) - 1
Final_Card_Evaluation <- Combined_Research
colnames(Research_Reward) <- c("Card","Old_Set","MB1_Price","Old_Printing_Price","Price_Difference","Perc_Diff")
colnames(Final_Card_Evaluation) <- c("Card","Old_Set","MB1_Price","Old_Printing_Price","Price_Difference","Perc_Diff")
Research_Reward <- rbind(Research_Reward,Final_Card_Evaluation)
Research_Reward$Card <- ifelse(Research_Reward$Card == "nezumi-shortfang-nezumi-shortfang", "Nezumi Shortfang",as.character(Research_Reward$Card))


View(Research_Reward)
url <- paste("https://edhrec.com/sets/fmb1",sep = "")
remDr$navigate(url)
Sys.sleep(10)
#remDr$navigate("https://edhrec.com/sets/ARB")
EDH_Goodies <- remDr$getPageSource()[[1]] %>% read_html()
EDH_Card <- EDH_Goodies %>% html_nodes(".card__name") %>% html_text()
EDH_Info <- EDH_Goodies %>% html_nodes(".card__label") %>% html_text()
EDH_Info <- gsub("In ","",EDH_Info)
EDH_Info <- data.frame(do.call('rbind', strsplit(as.character(EDH_Info)," decks\n",fixed=TRUE)))
EDH_Info$X2 <- gsub(" decks","",EDH_Info$X2)
EDH_Combo <- data.frame(EDH_Card, EDH_Info$X1, EDH_Info$X2)
EDH_Combo$EDH_Info.X1 <- as.character(EDH_Combo$EDH_Info.X1)
EDH_Combo <- EDH_Combo[!grepl(" decks",EDH_Combo$EDH_Info.X1),]
Place_Holder <- data.frame(do.call('rbind', strsplit(as.character(EDH_Combo$EDH_Info.X2)," of ",fixed=TRUE)))
EDH_Combo <- data.frame(EDH_Combo$EDH_Card,EDH_Combo$EDH_Info.X1,Place_Holder$X1,Place_Holder$X2)
colnames(EDH_Combo) <- c("Card","# of Decks In","Perc","Possible Decks")
EDH_Combo$Rank <- seq(nrow(EDH_Combo))

Research_Reward$Card <- ifelse(Research_Reward$Card == "Wear Tear", "Wear // Tear", as.character(Research_Reward$Card) )

Research_Reward$`# deck/EDHrec`<- EDH_Combo$`# of Decks In`[match(Research_Reward$Card,EDH_Combo$Card)]
Research_Reward$EDHREC_Rank_MB1 <- EDH_Combo$Rank[match(Research_Reward$Card,EDH_Combo$Card)]
#Research_Reward <- Research_Reward[,-ncol(Research_Reward)]
TCG_Stuff <- NULL
for (i in 1:5){
url <- paste("https://www.tcgplayer.com/search/magic/mystery-booster-retail-exclusives?productLineName=magic&setName=mystery-booster-retail-exclusives&page=",i,"&productTypeName=Cards",sep ="")
remDr$navigate(url)
Sys.sleep(7)
remDr$findElement("xpath",'//*[@id="app"]/div/section[2]/div/div[2]/div/div/div[2]/div/select')$clickElement()
remDr$findElement("xpath",'//*[@id="app"]/div/section[2]/div/div[2]/div/div/div[2]/div/select/option[2]')$clickElement()

TCG_Goodies <- remDr$getPageSource()[[1]] %>% read_html()
TCG_Info <- TCG_Goodies %>% html_nodes(".search-result") %>% html_text()
TCG_Info <- gsub("  Magic: The Gathering  Mystery Booster: Retail Exclusives  ","", TCG_Info)
TCG_Info <- gsub("  Magic: The Gathering  Mystery Booster: Retail Exclusives ","", TCG_Info)
TCG_Info <- gsub("Listings As low as","", TCG_Info)
TCG_Info <- gsub("Market Price:"," ", TCG_Info)
TCG_Info <- gsub(" Product Details","", TCG_Info)
TCG_Info <- gsub("\\$","", TCG_Info)
TCG_Info <- gsub("  ","--", TCG_Info)
TCG_Info <- gsub(" Magic: The Gathering--Mystery Booster: Retail Exclusives--","", TCG_Info)
TCG_Info <- gsub(" Magic: The Gathering--Mystery Booster: Retail Exclusives ","",TCG_Info)
TCG_Info <- gsub("[A-Za-z]+ · #\\d+ ","",TCG_Info)
TCG_Info <- data.frame(do.call('rbind', strsplit(as.character(TCG_Info),"--",fixed=TRUE)))
colnames(TCG_Info) <- c("Card","Listings","Low_Price","Market_Price")
TCG_Stuff <- rbind(TCG_Stuff,TCG_Info)
}
TCG_Info <- TCG_Stuff
TCG_Info$Rank <- seq(nrow(TCG_Info))
TCG_Info

Research_Reward$Card <- ifelse(Research_Reward$Card == "Nezumi Shortfang", "Nezumi Shortfang // Nezumi Shortfang", as.character(Research_Reward$Card) )
Research_Reward$TCG_Rank <- TCG_Info$Rank[match(Research_Reward$Card,TCG_Info$Card)]
Research_Reward$Combined_Ranking <- round((Research_Reward$EDHREC_Rank_MB1 + Research_Reward$TCG_Rank)/ 2,2)
Research_Reward <- Research_Reward[order(Research_Reward$Combined_Ranking),]
Research_Reward$Combined_Ranking <- seq(nrow(Research_Reward))
#sheets_deauth()
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("MB1_Foils")
sheet_write(
  Final,
  ss = "/d/16G2uhRMyI8vUdqfJRsTkPzOKH_FF4nd9zPyLIiNTd-4",
  sheet = "New"
)

Final <- Research_Reward
