library("tidyverse")
library("rvest")
library("jsonlite")

New_Set <- NULL
total = 2 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
#Start_Time <- Sys.time()
for(i in 1:2){
  url <- paste0("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=3162&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep="")
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  html <- read_html(url)
  card_name <- html %>% html_nodes(".productDetailTitle") %>% html_text()
  card_info <- html %>% html_nodes(".productDetailSet") %>% html_text()
  card_set <- "Ikoria: Lair of Behemoths Variants"
  card_rarity <- data.frame(do.call('rbind',strsplit(as.character(card_info),'(',fixed=TRUE)))
  card_rarity <- data.frame(do.call('rbind',strsplit(as.character(card_rarity$X2),')',fixed=TRUE)))
  card_rarity <- card_rarity$X1
  card_key <- paste(card_name,card_set,card_rarity,sep="")
  card_foil <- ""
  New_Stuff <- data.frame(
    X1 <- card_key,
    X2 <- card_name,
    X3 <- card_set,
    X4 <- card_rarity,
    X5 <- card_foil)
  New_Set <- rbind(New_Set, New_Stuff)
  
  setTxtProgressBar(pb,i)
  
}
End_Time <- Sys.time()

New_Set
summary(New_Set)
New_Set_Foils <- New_Set
New_Set_Foils$X5....card_foil <- "FOIL"
New_Set_Foils$X1....card_key <- paste(New_Set_Foils$X1....card_key,New_Set_Foils$X5....card_foil, sep = "")

New_Set_All <- rbind(New_Set,New_Set_Foils)
New_Set_All <- New_Set_All[!grepl("L", New_Set_All$X4....card_rarity),]

setwd("/home/cujo253/Essential_Referential_CSVS")
write_csv(New_Set_All,"IKA_VAR_Addition.csv")
