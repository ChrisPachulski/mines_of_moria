library(rvest)     # HTML Hacking & Web Scraping
library(jsonlite)  # JSON manipulation
library(tidyverse) # Data Manipulation
library(tidyquant) # ggplot2 theme
library(xopen)     # Opens URL in Browser
library(dplyr)
library(knitr)     # Pretty HTML Tables
library(purrr)     # Allows for the replacement of loops and suite of apply functions
library(tibble)    # Breakdown further elements
library(dplyr)     # Data Manipulation
library(tidyr)     # The Janitor is this guy
library(tictoc)
Vendor = NULL
total = 1000 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
tic()
for(i in 1:1000){
  url <- paste0("https://shop.tcgplayer.com/magic/product/show?Rarity=Mythic&Rarity=Rare&Rarity=Uncommon&Rarity=Common&Condition=LightlyPlayed&Condition=NearMint&Language=English&MinQuantity=4&PageNumber=",i)
  html <- read_html(url)
  bit <- html %>% html_nodes(".product__offers-more-count") %>% html_text()
  bit <-data.frame(bit)
  #view(bit)
  bitp <- bit[- (grep("photos", bit$bit)),]
  bitp <- as.data.frame(bitp)
  bits <- NULL
  bits <- ifelse(nrow(bit) == 10, as.list(bit), as.list(bitp))
  bits <- as.data.frame(bits)
  colnames(bits) <- ("bit")
  bit <- bits
  #bit <- as.factor(bit)
  bit <- as.data.frame(bit)
  #bit$bit <- as.character(bit$bit)
  bit_1 <- data.frame(do.call('rbind', strsplit(as.character(bit$bit),' price',fixed=TRUE)))
  bit <- bit_1$X1
  bit <- data.frame(bit)
  bit_Ex <- ifelse(nrow(bit) != 10, (bit %>% slice(rep(1:n(), each = 2))), bit )
  bit_Ex <- data.frame(bit_Ex) 
  bit_Ex = bit_Ex[c(2,2,4,6,8,10,12,14,16,18),]
  bit_Ex <- data.frame(bit_Ex) 
  bit = ifelse(nrow(bit) == 10, bit, bit_Ex)
  bit <- data.frame(bit)
  colnames(bit) <- ("bit")
  bit_info <- html %>% html_nodes(".product__name") %>% html_text()
  bit_info <- data.frame(bit_info)
  bit_info <- as.data.frame(bit_info)
  bit <- as.data.frame(bit)
  bit_set <- html %>% html_nodes(".product__group") %>% html_text()
  bit_set <- data.frame(do.call('rbind', strsplit(as.character(bit_set),' (',fixed=TRUE)))
  bit_set <- bit_set$X1
  bit_rare <-  html %>% html_nodes(".product__extended-field") %>% html_text()
  bit_rare <- as.data.frame(bit_rare)
  bit_rare <- bit_rare[- grep("Number", bit_rare$bit_rare),]
  bit_rare <- as.data.frame(bit_rare)
  bit_rare <- data.frame(do.call('rbind', strsplit(as.character(bit_rare$bit_rare),'Rarity ',fixed=TRUE)))
  bit_rare <- bit_rare$X2
  bit_price <- html %>% html_nodes(".product__prices") %>% html_text()
  bit_price <- as.data.frame(bit_price)
  bit_price <- data.frame(do.call('rbind', strsplit(as.character(bit_price$bit_price),'\r\n',fixed=TRUE)))
  bit_price <- data.frame(do.call('rbind', strsplit(as.character(bit_price$X3),'$',fixed=TRUE)))
  bit_price <- bit_price$X2
  United <- data.frame(X1 = bit_info,
                       X2 = bit_set,
                       X3 = bit_rare,
                       x4 = bit_price,
                       X5 = bit)
  Vendor <-rbind(Vendor,United)
  
  setTxtProgressBar(pb,i)
  
}
toc()

TCG_Vendor <- as.data.frame(Vendor)
TCG_Vendor$Primary_Key <- paste(TCG_Vendor$bit_info,TCG_Vendor$X2,TCG_Vendor$X3,sep="")
TCG_Vendor <- TCG_Vendor[c(6,1,2,3,4,5)]
names(TCG_Vendor) <- c("Primary_Key","Card_Name","Set","Rarity","MKT_EST","Vendor Listings")
TCG_Vendor <- as.data.frame(TCG_Vendor)
TCG_Vendor$Rank <- seq.int(nrow(TCG_Vendor))
head(TCG_Vendor)
#setwd("/cloud/project/Reports/TCG Vendor")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_TCG",".csv",sep="")
write.csv(TCG_Vendor, file=csvFileName, row.names = FALSE) 