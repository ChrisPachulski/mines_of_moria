source("config.R")
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
library(googlesheets4)
ss <- drive_get("Wolfs_Buylist_Review")
Wolfs_Buylist <- sheets_read(ss)
Wolfs_Buylist <- as.data.frame(Wolfs_Buylist)
Buylist <- Wolfs_Buylist
Buylist$Semi_Key <- paste(Buylist$data.name,Buylist$data.edition, sep="")
Wolfs_Buylist$data.is_foil[is.na(Wolfs_Buylist$data.is_foil) == T] <- ""
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.is_foil == ""),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy <= 20),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy >= 1),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$Printings <= 5),]
Wolfs_Buylist <- Wolfs_Buylist[order(Wolfs_Buylist$Velocity_Adjusted),]
Wolfs_Targets <- data.frame(Wolfs_Buylist$data.name,Wolfs_Buylist$data.edition,Wolfs_Buylist$data.price_retail,Wolfs_Buylist$data.price_buy,Wolfs_Buylist$Printings)
Wolfs_Targets$Semi_Key <- paste(Wolfs_Targets$Wolfs_Buylist.data.name,Wolfs_Targets$Wolfs_Buylist.data.edition,sep="")


SS <- drive_get("Cardsphere_Review")
Cardsphere_Retrieval <- sheets_read(SS, sheet = "All_CS_NF")
Cardsphere_Retrieval <- as.data.frame(Cardsphere_Retrieval)
CS <- Cardsphere_Retrieval

Cardsphere_Retrieval$isfoil[is.na(Cardsphere_Retrieval$isfoil) == T] <- ""
Cardsphere_Retrieval <- Cardsphere_Retrieval[which(Cardsphere_Retrieval$Opportunities >= -1.50),]
Cardsphere_Retrieval <- Cardsphere_Retrieval[which(Cardsphere_Retrieval$CK_BL_Offer >= 1),]

Target_List <- append(Cardsphere_Retrieval$Key,Wolfs_Targets$Semi_Key)
Target_List <- as.data.frame(Target_List)
nrow(Target_List)
Target_List <- unique(Target_List$Target_List)
Target_List <- as.data.frame(Target_List)

Target_List$Name <- Buylist$data.name[match(Target_List$Target_List,Buylist$Semi_Key)]
Target_List$Edition <- Buylist$data.edition[match(Target_List$Target_List,Buylist$Semi_Key)]
All_Cards <- read_csv("/home/cujo253/Reports/All_Cards_MB1.csv",col_types = cols(`F/NF` = col_character()))
All_Cards$Semi <- paste(All_Cards$name,All_Cards$Set,sep="")
Target_List$Rarity <-All_Cards$Rarity[match(Target_List$Target_List,All_Cards$Semi)]
Target_List$CK_Buylist <- CS$CK_BL_Offer[match(Target_List$Target_List,CS$Key)]
Target_List$CS_Retail <- CS$retail[match(Target_List$Target_List,CS$Key)]
Target_List$CS_Offer <- round((Target_List$CS_Retail * .72),1)
Target_List$My_Offer <- Target_List$CS_Offer - as.numeric(as.character(Target_List$CK_Buylist))
Target_List <- na.omit(Target_List)
Target_List <- Target_List[order(Target_List$My_Offer),]
Target_List <- Target_List[which(Target_List$My_Offer <= 0),]

CS_Import_List <- NULL
CS_Import_List$Name <- Target_List$Name
CS_Import_List <- as.data.frame(CS_Import_List)
CS_Import_List$Edition <- Target_List$Edition
CS_Import_List$Condition <- "Near Mint"
CS_Import_List$Language <- "English"
CS_Import_List$Finish <- ""
CS_Import_List$Tags <- ""
CS_Import_List$Quantity <- 12
CS_Import_List$Tradelist_Count <- 0

CS_Import_List <- CS_Import_List[moveme(names(CS_Import_List), "Tradelist_Count first")]
CS_Import_List <- CS_Import_List[moveme(names(CS_Import_List), "Quantity first")]
colnames(CS_Import_List)[2] <- c("Tradelist Count")

ss <- drive_get("Cardsphere_Import_List")
#sheets_deauth()
sheets_auth()
sheets_write(
  CS_Import_List,
  ss = ss,
  sheet = "CS_Import_List"
)
