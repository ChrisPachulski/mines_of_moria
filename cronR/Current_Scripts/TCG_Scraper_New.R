library(RSelenium)
library(jsonlite)
library(tidyverse)
library(devtools)
library(bigrquery)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)

#Selenium Server####
remDr = remoteDriver(remoteServerAddr = "64.225.17.196", port = 4445L, browser = "chrome")
remDr$open()
#Let's Surf the Web####

remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=magic&productTypeName=Cards&rarityName=Mythic%7CUncommon%7CRare")
Sys.sleep(2)
Best_Selling <- remDr$findElement(using = "css", value = "select")$clickElement()
Best_Selling <- remDr$findElement("xpath",'//*[@id="app"]/div/section[2]/div/div/div[3]/div/div/div[2]/div/select/option[2]')$clickElement()
Sys.sleep(3)
total = 25 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
for(i in 1:25){
Load_More <- remDr$findElement(using = "class", value = "load-more-button")$clickElement()
setTxtProgressBar(pb,i)
Sys.sleep(sample(1:3, 1))
}
Sys.sleep(5)
library(rvest)
library(stringr)
Page_Contents <- remDr$getPageSource() %>% .[[1]] %>% read_html()
Results <- Page_Contents %>% html_nodes(".search-result__list-wrap") %>% html_text()
Results <- Results[!grepl("Foreign", Results)]
Results <- Results[!grepl("Currently out of stock", Results)]
Results <- gsub("  Magic: The Gathering  ","", Results)
Results <- gsub(" Magic: The Gathering","", Results)
Results <- gsub("Listings As low as ","", Results)
Results <- gsub(" Listings","", Results)
Results <- gsub("Market Price: ","", Results)
Results <- gsub("  ","---", Results)
Results <- gsub(" \\$","---", Results)
Results <- gsub("^---","", Results)
Results <- as.data.frame(Results)
Results <- data.frame(do.call('rbind', strsplit(as.character(Results$Results),'---',fixed=TRUE)))
colnames(Results) <- c("Set", "Card_Name", "Vendor Listings","MKT_EST", "MKT")
Results$Rarity <- Updated_Tracking_Keys$Rarity[match(paste(Results$Card_Name,Results$Set,sep=""),Updated_Tracking_Keys$Semi)]
Results$Primary_Key <- paste(Results$Card_Name,Results$Set,Results$Rarity,sep="")
TCG <- Results[c(7,2,1,6,4,3,5)]

