source("config.R")
install.packages(c("rvest","tidyverse","RSelenium"))
library(tidyverse)
library(rvest)
library(RSelenium)

Card_Info <- read_csv("~/Essential_Referential_CSVS/2020-05-13_Scryfall.csv", 
                      col_types = cols(foil = col_character(), 
                                       nonfoil = col_character(), promo = col_character(), 
                                       reprint = col_character()))
url_part_1 <- "https://shop.tcgplayer.com/productcatalog/product/getpricetable?captureFeaturedSellerData=True&pageSize=50&productId="
tcg_id <- Card_Info$tcgplayer_id[2]
nonfoil_url_part_2 <- "&gameName=magic&useV2Listings=true&_=1589378718034"
foil_url_part_2 <- "&gameName=magic&useV2Listings=true&_=1589379502769"
nonfoil_url <- paste(url_part_1,tcg_id,nonfoil_url_part_2,sep="")
foil_url <- paste(url_part_1,tcg_id,foil_url_part_2,sep="")
