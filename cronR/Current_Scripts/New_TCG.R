library(devtools)
#devtools::install_github("tidyverse/googlesheets4", force = TRUE)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(gargle)
library(httr)
Start_Time <- Sys.time()
A <- 0
B <- 1000
C <- 1000
TCG__Best_Sellers <- NULL
body <- paste('{
    "algorithm": "salesrel",
    "from": "',A,'",
    "size": "',B,'",
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
              sep="")
A <- B + 1
B <- 999
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(30);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
for(i in 1:C){
  Name <- TCG_Results_1[[1]]$results[[i]]$productName
  Set <- TCG_Results_1[[1]]$results[[i]]$setName
  Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
  MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
  Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
  MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
  Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
  Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID)
  TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}
repeat{
body <- paste('{
    "algorithm": "salesrel",
    "from": "',A,'",
    "size": "',B,'",
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
sep="")
A <- A + 1000
B <- 999
C <- 999
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
repeat{
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(30);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
if((length(TCG_Results_1[[1]]$results) != 0)) break
  }
for(i in 1:C){
Name <- TCG_Results_1[[1]]$results[[i]]$productName
Set <- TCG_Results_1[[1]]$results[[i]]$setName
Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID)
TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}
if(A >= 11001) break
Sys.sleep(10)
}
End_Time <- Sys.time()
print(paste("TCG Best Sellers Lasted:",round(End_Time - Start_Time,2)))
TCG__Best_Sellers <- unique(TCG__Best_Sellers)
TCG__Best_Sellers <- as.data.frame(TCG__Best_Sellers)
TCG__Best_Sellers$Rank <- seq(nrow(TCG__Best_Sellers))
TCG__Best_Sellers$Rarity <- ifelse(TCG__Best_Sellers$Rarity == "Mythic","M", ifelse(TCG__Best_Sellers$Rarity == "Rare", "R", ifelse(TCG__Best_Sellers$Rarity == "Uncommon", "U", ifelse(TCG__Best_Sellers$Rarity == "Common", "C", TCG__Best_Sellers$Rarity))))
TCG__Best_Sellers$Key <- paste(TCG__Best_Sellers$Name,TCG__Best_Sellers$Set,TCG__Best_Sellers$Rarity,sep="")
TCG__Best_Sellers <- TCG__Best_Sellers[moveme(names(TCG__Best_Sellers), "Key first")]
End_Time <- Sys.time()
print(paste("TCG Best Sellers Lasted:",round(End_Time - Start_Time,2)))

setwd("/home/cujo253/Reports")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_New_TCG",".csv",sep="")
write.csv(TCG__Best_Sellers, file=csvFileName, row.names = FALSE) 

options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

drive_auth(email = "pachun95@gmail.com")
drive_auth(use_oob=TRUE)

# my_dfs <- list(Final_Export_1, Funny_Money_Analysis )
# sheets_auth()
# sheets_create(
#   paste(currentDate,"_Market_Review",sep=""),
#   sheets = my_dfs
# )

#Old_View <- read_csv("/home/cujo253/Reports/TCG Vendor/2020-05-19_TCG.csv")
ss <- drive_get("New_TCG")
#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com")
sheet_write(
  TCG__Best_Sellers,
  ss = "/d/1Ef2FgpR-bOg28a8JetHTTIXFH4FeR3eSEj5wpIAVjlU",
  sheet = "TCG_Real_View"
)
# sheet_write(
#   Old_View,
#   ss = "/d/1Ef2FgpR-bOg28a8JetHTTIXFH4FeR3eSEj5wpIAVjlU",
#   sheet = "What_Others_See"
# )

