library(jsonlite)
library(httr)
library("tidyverse")
library("rvest")
library("jsonlite")
library(devtools)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(readr)
#install.packages("readr", INSTALL_opts = '--no-lock',force = TRUE)
library(gargle)
library(httr)
library(anytime)
tinytest::test_package("anytime")
install.packages("anytime")
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
funk <- function(t){
  ifelse(nchar(t) <= 10, right(t,1),ifelse(nchar(t)<=190, right(t,2),ifelse(nchar(t)>=191, right((t),3),0)))
} #Character Count utilization of 'left'&'right' functions for quantity breakdown
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
}

ss <- drive_get("Bills & MTG 2020")
Inventory <- as.data.frame(range_read(ss,'Raw Inventory'))
Inventory$...7 <- as.character(Inventory$...7)
Inventory$...8 <- as.numeric(as.character(Inventory$...8))
Inventory$...9 <- as.numeric(as.character(Inventory$...9))
Inventory$...9[is.na(Inventory$...9)] <- 0
Inventory$...10 <- as.numeric(as.character(Inventory$...10))
Inventory$...11 <- as.character(Inventory$...11)
Inventory$...11[is.na(Inventory$...11)] <- ""
Inventory$...12 <- as.character(Inventory$...12)
Inventory$...13 <- as.character(Inventory$...13)
Inventory$...14 <- as.character(Inventory$...14)
Inventory$...15 <- as.character(Inventory$...15)
Inventory$...16 <- as.character(Inventory$...16)
Inventory$...17 <- round(as.numeric(as.character(Inventory$...17)),2)
Inventory$...18 <- round(as.numeric(as.character(Inventory$...18)),2)
Inventory$...19 <- round(as.numeric(as.character(Inventory$...19)),2)
Inventory$...20 <- round(as.numeric(as.character(Inventory$...20)),2)
Inventory$...21 <- round(as.numeric(as.character(Inventory$...21)),2)
Inventory$...22 <- round(as.numeric(as.character(Inventory$...22)),2)
Inventory$...23 <- round(as.numeric(as.character(Inventory$...23)),2)
Inventory$...24 <- round(as.numeric(as.character(Inventory$...24)),2)
Inventory$...25 <- as.character(Inventory$...25)
Inventory$...26 <- as.character(Inventory$...26)
Inventory$...27 <- as.character(Inventory$...27)
Inventory$...28 <- as.character(Inventory$...28)
Inventory$...29 <- as.character(Inventory$...29)
Inventory$...30 <- round(as.numeric(as.character(Inventory$...30)),0)
Inventory <- as.data.frame(Inventory[-1,-c(1:6)])
colnames(Inventory) <- c("MTG Card","Buy Qty","Sold QTY","Curr. QTY","Foil","Long_Set","Key","Set","Purchased_Via","DOP",
                         "Credit_Diff_$","Diff_$","Des_ROI","Des_TCG","Market","Orig_COG","Actual_COG","Tot_COG","Live_CK_BL",
                         "Color","Type","Format","Rare","Days_Held")
Inventory$DOP <- anydate(as.numeric(Inventory$DOP))
Inventory <- na.omit(Inventory)

Sold <- as.data.frame(range_read(ss,'Inventory Sold'))
Sold <- Sold[c(1:36)]
Sold$Qty <- as.numeric(as.character(Sold$Qty))
Sold$Foil[is.na(Sold$Foil)] <- ""
Sold$`Indivi. Amt. Paid` <- round(Sold$`Indivi. Amt. Paid`,2)
Sold$`Ind. Amt. Sold`<-round(Sold$`Ind. Amt. Sold`,2)
Sold$`V Fees`<-round(Sold$`V Fees`,2)
Sold$Rev <-round(Sold$Rev,2)
Sold$Profit<-round(Sold$Profit,2)
Sold$ROI<-round(Sold$ROI,2)


Updated_Tracking_Keys <- as.data.frame(read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())))



total = 50598 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) 
card_dictionary <- NULL
for(i in 1:nrow(Scryfall_IDS)){
  scryfall_link <- paste("https://api.scryfall.com/cards/",Scryfall_IDS$scryfallID[i],sep="")
  scryfall <- GET(scryfall_link)
  
  oracle_id <- (content(scryfall,"parsed")$oracle_id)
  tcgplayer_id <- (content(scryfall,"parsed")$tcgplayer_id)
  if(is.null(tcgplayer_id) == T){tcgplayer_id = NA}
  card <- (content(scryfall,"parsed")$name)
  if(is.null(card) == T){card = NA}
  release_date <- (content(scryfall,"parsed")$released_at)
  if(is.null(release_date) == T){release_date = NA}
  cmc <- (content(scryfall,"parsed")$cmc)
  if(is.null(cmc) == T){cmc = NA}
  type_line <- (content(scryfall,"parsed")$type_line)
  if(is.null(type_line) == T){type_line = NA}
  standard <- (content(scryfall,"parsed")$legalities$standard)
  if(is.null(standard) == T){standard = NA}
  pioneer <- (content(scryfall,"parsed")$legalities$pioneer)
  if(is.null(pioneer) == T){pioneer = NA}
  modern <- (content(scryfall,"parsed")$legalities$modern)
  if(is.null(modern) == T){modern = NA}
  legacy <- (content(scryfall,"parsed")$legalities$legacy)
  if(is.null(legacy) == T){legacy = NA}
  commander <- (content(scryfall,"parsed")$legalities$commander)
  if(is.null(commander) == T){commander = NA}
  foil <- (content(scryfall,"parsed")$foil)
  if(is.null(foil) == T){foil = NA}
  nonfoil <- (content(scryfall,"parsed")$nonfoil)
  if(is.null(nonfoil) == T){nonfoil = NA}
  promo <- (content(scryfall,"parsed")$promo)
  if(is.null(promo) == T){promo = NA}
  reprint <- (content(scryfall,"parsed")$reprint)
  if(is.null(reprint) == T){reprint = NA}
  set_abbr <- (content(scryfall,"parsed")$set)
  if(is.null(set_abbr) == T){set_abbr = NA}
  edition <- (content(scryfall,"parsed")$set_name)
  if(is.null(edition) == T){edition = NA}
  collector_number <- (content(scryfall,"parsed")$collector_number)
  if(is.null(collector_number) == T){collector_number = NA}
  rarity <- (content(scryfall,"parsed")$rarity)
  if(is.null(rarity) == T){rarity = NA}
  edhrec <- content(scryfall,"parsed")$edhrec_rank
  if(is.null(edhrec) == T){edhrec = NA}
  color_1 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[1]]}, error = function(e){color_1 = "NA"})
  color_2 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[2]]}, error = function(e){color_2 = "NA"})
  color_3 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[3]]}, error = function(e){color_3 = "NA"})
  color_4 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[4]]}, error = function(e){color_4 = "NA"})
  color_5 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[5]]}, error = function(e){color_5 = "NA"})
  color_6 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[6]]}, error = function(e){color_6 = "NA"})
  if(is.null(color_1) == T){color_1 = NA}
  if(is.null(color_2) == T){color_2 = NA}
  if(is.null(color_3) == T){color_3 = NA}
  if(is.null(color_4) == T){color_4 = NA}
  if(is.null(color_5) == T){color_5 = NA}
  if(is.null(color_6) == T){color_6 = NA}
  purchase_tcg <- (content(scryfall,"parsed")$purchase_uris$tcgplayer)
  
  row_info <- cbind(oracle_id,tcgplayer_id,card,release_date,cmc,type_line,
                    standard,pioneer,modern,legacy,commander,foil,nonfoil,
                    promo,reprint,set_abbr,edition,collector_number,rarity,
                    edhrec,purchase_tcg)
  card_dictionary <- rbind(card_dictionary, row_info)
  colnames(row_info)
  colnames(card_dictionary)
  setTxtProgressBar(pb,i)
  Sys.sleep(.12)
}

card_dictionary_backup <- card_dictionary

