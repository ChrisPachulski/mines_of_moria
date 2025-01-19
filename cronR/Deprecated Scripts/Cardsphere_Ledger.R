source("config.R")
#Don't Look at the foundation, it's hideous (& pointless)####
library(RSelenium)
library(rvest)
library(jsonlite)
library(tidyverse)
library(devtools)
library(bigrquery)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
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

options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

# You will only need these comments below if things go tits up.
#
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)

remDr = remoteDriver(remoteServerAddr = "159.65.219.70", port = 4445L, browser = "chrome")
remDr$open()
remDr$navigate("https://www.cardsphere.com/login")
Sys.sleep(5)

username <- remDr$findElement(using = "id", value = "email")
username$clearElement()
username$sendKeysToElement(list("cjpach@mac.com"))

passwd <- remDr$findElement(using = "id", value = "password")
passwd$clearElement()
passwd$sendKeysToElement(list("Tasigur95$"))

Post_Credential_Login <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/div/div/form/button")
Post_Credential_Login$submitElement()
Sys.sleep(2)
RSelenium::selKeys %>% names()

remDr$navigate("https://www.cardsphere.com/ledger")
webElem <- remDr$findElement("css","body")
for(i in 1:500){
webElem$sendKeysToElement(list(key="end"))
}
Page_Contents <- remDr$getPageSource() %>% .[[1]] %>% read_html()
Ledger_Contents <- Page_Contents %>% html_nodes(".cs-row") %>% html_text()
Ledger_Contents <- gsub("\n ","", Ledger_Contents)
Ledger_Contents <- Ledger_Contents[!grepl("MasterCard", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Sale", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Buy-in", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("                                              Back to Top                                                                                                  Contact Us                                                                                    Reddit                                                                                    Facebook                                                                                    Twitter                                                                                    Discord                                                                                    Blog                                                                               Terms & Conditions                   Privacy Policy                   Changelog                                                            Draft & Sealed Simulator                   Explore Cards                   Trade Guide                   Condition Guide                   Tutorials & FAQ                                                             Back to Top                                 ", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Refund", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Membership", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Cash-out", Ledger_Contents)]
Ledger_Contents <- Ledger_Contents[!grepl("Transfer", Ledger_Contents)]

Ledger_Contents <- gsub("  \n","", Ledger_Contents)
The_Ledger <- as.data.frame(Ledger_Contents)

# The_Ledger_Date <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger$Ledger_Contents),'             Purchase',fixed=TRUE)))
# The_Ledger_Date <- The_Ledger_Date[-1,]
# The_Ledger_Date <- The_Ledger_Date$X1
The_Ledger_All_Other_Info <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger$Ledger_Contents),",           ",fixed=TRUE)))
The_Ledger_Card <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger_All_Other_Info$X1),"Purchase ",fixed=TRUE)))

The_Ledger_Buyins <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger_All_Other_Info$X5),"$",fixed=TRUE)))
The_Ledger_Users <- gsub("English ","",The_Ledger_Buyins$X1 )
The_Ledger_Users <- gsub("                           -","",The_Ledger_Users)
The_Ledger_Users <- gsub("                                                                                ","",The_Ledger_Users)
Ledger_Qty <- 1
The_Ledger_Cleaned <- data.frame(The_Ledger_Card$X1,The_Ledger_Card$X2, The_Ledger_All_Other_Info$X2,The_Ledger_All_Other_Info$X3,The_Ledger_All_Other_Info$X4,The_Ledger_Buyins$X2,The_Ledger_Buyins$X3,The_Ledger_Users,Ledger_Qty)
The_Ledger_Cleaned <- The_Ledger_Cleaned[-1,]
colnames(The_Ledger_Cleaned) <- c("Date","Card_Name", "Set","Rarity","F/NF","Cost","Fee","Sender","Qty")
The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] <- lapply(The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] , trimws)

The_Ledger_Cleaned$Date <- gsub("\\s\\d{6}\\s*","",The_Ledger_Cleaned$Date)

tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
  #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
  rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
  mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))})

Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                        Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                        Semi = paste(name,Set,sep="")) 

Updated_Tracking_Keys$Semi <- paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep="")
New_Name <- data.frame(do.call('rbind', strsplit(as.character(The_Ledger_Cleaned$Card_Name),' (',fixed=TRUE)))
The_Ledger_Cleaned$Card_Name <- New_Name$X1
The_Ledger_Cleaned$Semi <- paste(The_Ledger_Cleaned$Card_Name,The_Ledger_Cleaned$Set,sep="")
The_Ledger_Cleaned$Rarity <- Updated_Tracking_Keys$Rarity[match(The_Ledger_Cleaned$Semi,Updated_Tracking_Keys$Semi)]
The_Ledger_Cleaned$Rarity <- ifelse(is.na(The_Ledger_Cleaned$Rarity == T), Updated_Tracking_Keys$Rarity[match(The_Ledger_Cleaned$Card_Name,Updated_Tracking_Keys$name)], The_Ledger_Cleaned$Rarity)
The_Ledger_Cleaned <- The_Ledger_Cleaned[,-ncol(The_Ledger_Cleaned)]
The_Ledger_Cleaned$`F/NF` <- gsub("Nonfoil","",The_Ledger_Cleaned$`F/NF`)
The_Ledger_Cleaned$`F/NF` <- gsub("Foil","FOIL",The_Ledger_Cleaned$`F/NF`)
The_Ledger_Cleaned$Key <- paste(The_Ledger_Cleaned$Card_Name,The_Ledger_Cleaned$Set,The_Ledger_Cleaned$Rarity,sep="")
The_Ledger_Cleaned$Key <- paste(The_Ledger_Cleaned$Key, The_Ledger_Cleaned$`F/NF`, sep=" ")
The_Ledger_Cleaned <- The_Ledger_Cleaned[moveme(names(The_Ledger_Cleaned), "Key first")]
The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] <- lapply(The_Ledger_Cleaned[,c(1:ncol(The_Ledger_Cleaned))] , trimws)
The_Ledger_Cleaned$Date <- ifelse(nchar(The_Ledger_Cleaned$Date) < 7, paste(The_Ledger_Cleaned$Date,", 2020",sep = ""), The_Ledger_Cleaned$Date)
The_Ledger_Cleaned$Cost <- as.numeric(The_Ledger_Cleaned$Cost)
Avg_Cost <- aggregate(Cost~Key, data=The_Ledger_Cleaned, mean)
The_Ledger_Cleaned$Date <- as.Date(The_Ledger_Cleaned$Date, format = "%b %d,%Y")
Avg_Date <- aggregate(Date~Key, data=The_Ledger_Cleaned, mean)
The_Ledger_Cleaned$Qty <- as.numeric(The_Ledger_Cleaned$Qty)
Sum_Qty <- aggregate(Qty~Key, data=The_Ledger_Cleaned, sum)

Calculated_Ledger <- NULL
Calculated_Ledger$Key <- Sum_Qty$Key
Calculated_Ledger <- as.data.frame(Calculated_Ledger)
Calculated_Ledger$Name <- The_Ledger_Cleaned$Card_Name[match(Calculated_Ledger$Key,The_Ledger_Cleaned$Key)]
Calculated_Ledger$Set <- The_Ledger_Cleaned$Set[match(Calculated_Ledger$Key,The_Ledger_Cleaned$Key)]
Calculated_Ledger$Rarity <- The_Ledger_Cleaned$Rarity[match(Calculated_Ledger$Key,The_Ledger_Cleaned$Key)]
Calculated_Ledger$`F/NF` <- The_Ledger_Cleaned$`F/NF`[match(Calculated_Ledger$Key,The_Ledger_Cleaned$Key)]
Calculated_Ledger$Avg_Date <- Avg_Date$Date[match(Calculated_Ledger$Key,Avg_Date$Key)]
Calculated_Ledger$Avg_Cost <- round(Avg_Cost$Cost[match(Calculated_Ledger$Key,Avg_Cost$Key)],2)
Calculated_Ledger$Qty_Sum <- Sum_Qty$Qty[match(Calculated_Ledger$Key,Sum_Qty$Key)]
Calculated_Ledger$Hold_Time <- round(Sys.Date() - Calculated_Ledger$Avg_Date,0)



Calculated_Users <- NULL
Calculated_Users$Key <- paste(The_Ledger_Cleaned$Date,The_Ledger_Cleaned$Sender)
Calculated_Users <- as.data.frame(Calculated_Users)
Calculated_Users$Date <- The_Ledger_Cleaned$Date
Calculated_Users$Sender <- The_Ledger_Cleaned$Sender
Calculated_Users$Qty <- The_Ledger_Cleaned$Qty
Calculated_Users$Cost <- The_Ledger_Cleaned$Cost
Order_Sums <- aggregate(Qty~Sender, data=Calculated_Users, sum)
Order_Dates <- aggregate(Date~Sender, data=Calculated_Users, mean)
Order_Costs <- aggregate(Cost~Sender, data=Calculated_Users, mean)

Order_Dates$QTY <- Order_Sums$Qty[match(Order_Dates$Sender,Order_Sums$Sender)]
Order_Dates$Costs <- round(Order_Costs$Cost[match(Order_Dates$Sender,Order_Costs$Sender)],2)
Order_Dates <- Order_Dates[order(-Order_Dates$QTY),]


ss <- drive_get("Bills & MTG 2020")
Inventory_Sold <- range_read(ss,"Inventory Sold")
Inventory_Sold <- Inventory_Sold[1:36]
Inventory_Sold$Set <- gsub("_f","", Inventory_Sold$Set)
Inventory_Sold$Set <- gsub("_F","", Inventory_Sold$Set)
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
CS_Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/2020-04-21_CS_Sets.csv")

Inventory_Sold$`Unique Card Tag` <- paste(Inventory_Sold$`Card Name`,Inventory_Sold$Set,sep=" ")
Inventory_Sold$`Unique Card Tag` <- trimws(Inventory_Sold$`Unique Card Tag`)
Inventory_Sold$Set <- CS_Sets$CS_All_Sets[match(Inventory_Sold$Set,CS_Sets$GF_Abbr)]
Inventory_Sold <- Inventory_Sold[with(Inventory_Sold,  grepl("CS", Inventory_Sold$`Platform Buy`)),]

Calculated_Ledger$Semi <- paste(Calculated_Ledger$Name,Calculated_Ledger$Set,sep="")
Calculated_Ledger$Semi <- paste(Calculated_Ledger$Semi,Calculated_Ledger$`F/NF`,sep=" ")

Inventory_Sold$Semi <- paste(Inventory_Sold$`Card Name`,Inventory_Sold$Set,sep="")
Inventory_Sold$Foil[is.na(Inventory_Sold$Foil)==T] <- ""
Inventory_Sold$Semi <- paste(Inventory_Sold$Semi,Inventory_Sold$Foil,sep=" ")

Sales_Qty <- aggregate(as.numeric(as.character(Inventory_Sold$`# Sold`))~Semi, data=Inventory_Sold, sum)
colnames(Sales_Qty) <- c("Semi","QTY")
Calculated_Ledger$Sold_Qty <- Sales_Qty$QTY[match(Calculated_Ledger$Semi,Sales_Qty$Semi)]
Calculated_Ledger$Sold_Qty[is.na(Calculated_Ledger$Sold_Qty)==T] <- 0
Calculated_Ledger$Qty_Sum <- Calculated_Ledger$Qty_Sum - Calculated_Ledger$Sold_Qty

Calculated_Ledger <- Calculated_Ledger[which(Calculated_Ledger$Qty_Sum > 0),]
ncol(Calculated_Ledger)
Calculated_Ledger <- Calculated_Ledger[,-c(10:11)]
Calculated_Ledger$Hold_Time <- as.numeric(as.character(Calculated_Ledger$Hold_Time))
Calculated_Ledger_by_Cost <- Calculated_Ledger[order(-Calculated_Ledger$Avg_Cost),]
Calculated_Ledger_by_Date <- Calculated_Ledger[order(Calculated_Ledger$Hold_Time),]
Calculated_Ledger_by_QTY <- Calculated_Ledger[order(-Calculated_Ledger$Qty_Sum),]


#gs4_create(name = "Cardsphere_Ledger", sheets = list(Calculated_Ledger))
drive_auth(email = "pachun95@gmail.com", use_oob = T)
ss <- drive_get("Cardsphere_Ledger")
gs4_auth(email = "pachun95@gmail.com", use_oob = T)

sheet_write(
  Calculated_Ledger_by_Cost,
  ss = ss,
  sheet = "By_Cost"
)
sheet_write(
  Calculated_Ledger_by_Date,
  ss = ss,
  sheet = "By_Date"
)
sheet_write(
  Calculated_Ledger_by_QTY,
  ss = ss,
  sheet = "By_Qty"
)

sheet_write(
  The_Ledger_Cleaned,
  ss=ss,
  sheet="Pure_Ledger"
)


View(The_Recent_Ledger)
The_Ledger_Cleaned <- The_Ledger_Cleaned[order(-as.numeric(The_Ledger_Cleaned$Date)),]
The_Recent_Ledger <- The_Ledger_Cleaned[which(The_Ledger_Cleaned$Date > (currentDate-21)),]
ss <- drive_get("Bills & MTG 2020")
Raw_Inventory <- range_read(ss,"Raw Inventory", col_types = "c")
Raw_Inventory <- Raw_Inventory[-c(1:5)]
Raw_Inventory <- Raw_Inventory[-1,]
colnames(Raw_Inventory) <- c("Mini_Key","Name","OG_QTY","QTY_Sold","Qty",
                             "Foil","Long_Edition","Key","Short_Edition","Vendor_Buy",
                             "DOP","Credit_Diff","Dollar_Diff","Desired_ROI","Desired_TCG_Sales",
                             "MKT","COG","Inflated_COG","Total_COG","BL",
                             "Color","Type","Format","Rarity","Days_Held")
Raw_Inventory$DOP <- as.Date(Raw_Inventory$DOP, "%m/%d/%y")
Raw_Inventory$QTY_Sold[is.na(unlist(Raw_Inventory$QTY_Sold)) == T] <- 0
Raw_Inventory$Foil[is.na(Raw_Inventory$Foil) == T] <- ""

Raw_Inventory$COG <- gsub("\\$","", Raw_Inventory$COG)
Raw_Inventory <- data.frame(Raw_Inventory$Key,Raw_Inventory$Name,Raw_Inventory$Long_Edition,Raw_Inventory$Rarity,Raw_Inventory$Foil,as.numeric(Raw_Inventory$Qty),Raw_Inventory$DOP,as.numeric(Raw_Inventory$COG))
colnames(Raw_Inventory) <- c("Key","Name","Set","Rarity","Foil","Qty","DOP","COG")
Raw_Inventory <- as.data.frame(na.omit(Raw_Inventory))

The_Recent_Ledger$Inventory_Check <- Raw_Inventory$QTY[match(The_Recent_Ledger$Key,Raw_Inventory$Key)]
Additions <- as.data.frame(The_Recent_Ledger[which( (The_Recent_Ledger$Inventory_Check == 0)| (is.na(The_Recent_Ledger$Inventory_Check)==T) ),])
Additions$Inventory_Check[is.na(Additions$Inventory_Check)==T] <- 0
New_Cards_Added <- rbind(Raw_Inventory[1],Additions[1])
New_Qty <- rbind(Raw_Inventory[6],Additions[10])

  