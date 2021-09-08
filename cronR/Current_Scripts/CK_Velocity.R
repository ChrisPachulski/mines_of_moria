library(readr)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)

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
currentDate <- Sys.Date()
#Pioneer####
setwd("/home/cujo253/Metrics/Daily_Velocity_Trackers")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)

Velocity_Tracker <- NULL

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  date_recorded <- as.Date("2020-03-13")
  tmp$date <- date_recorded
  tmp$data.name <- as.character(tmp$data.name)
  tmp$data.edition <- as.character(tmp$data.edition)
  tmp$data.is_foil <- as.character(tmp$data.is_foil)
  tmp$Velocity_Adjusted <- as.numeric(tmp$Velocity_Adjusted)
  tmp$data.price_retail <- as.numeric(tmp$data.price_retail)
  tmp$data.price_buy <- as.numeric(tmp$data.price_buy)
  date_recorded <- date_recorded + 1
  Velociraptors <- cbind(tmp$date, tmp$data.name, tmp$data.edition, tmp$data.is_foil, tmp$Velocity_Adjusted, tmp$data.price_retail, tmp$data.price_buy)
  Velocity_Tracker <- rbind(Velocity_Tracker, Velociraptors)
}

Velocity_Tracker <- as.data.frame(Velocity_Tracker)
colnames(Velocity_Tracker) <- c("Date","Card","Edition","Foil","Velocity","Retail","Buylist")
Velocity_Tracker$Key <- paste(Velocity_Tracker$Card,Velocity_Tracker$Edition, sep = "")
Velociraptor <- paste(Velocity_Tracker$Card,Velocity_Tracker$Edition, sep = "")
Velociraptor <- unique(Velociraptor)

meaned_value <- NULL
for (velociraptor in Velociraptor){
  baby_velociraptor <- Velocity_Tracker[which(Velocity_Tracker$Key == velociraptor),]
  avg_velociraptor <- round(mean(as.numeric(as.character(baby_velociraptor$Velocity)), na.rm = T),4)
  meaned_value <- rbind(meaned_value, avg_velociraptor)
}

meaned_retail <- NULL
for (velociraptor in Velociraptor){
  baby_velociraptor <- Velocity_Tracker[which(Velocity_Tracker$Key == velociraptor),]
  avg_velociraptor <- round(mean(as.numeric(as.character(baby_velociraptor$Retail)), na.rm = T),4)
  meaned_retail <- rbind(meaned_retail, avg_velociraptor)
}

meaned_buylist <- NULL
for (velociraptor in Velociraptor){
  baby_velociraptor <- Velocity_Tracker[which(Velocity_Tracker$Key == velociraptor),]
  avg_velociraptor <- round(mean(as.numeric(as.character(baby_velociraptor$Buylist)), na.rm = T),4)
  meaned_buylist <- rbind(meaned_buylist, avg_velociraptor)
}


Avg_Velocity <- data.frame(Velociraptor)
Avg_Velocity$card <- Velocity_Tracker$Card[match(Avg_Velocity$Velociraptor,Velocity_Tracker$Key)]
Avg_Velocity$edition <- Velocity_Tracker$Edition[match(Avg_Velocity$Velociraptor,Velocity_Tracker$Key)]
#Avg_Velocity$foil <- Velocity_Tracker$Foil[match(Avg_Velocity$Velociraptor,Velocity_Tracker$Key)]
Avg_Velocity <- data.frame(Avg_Velocity,meaned_value,meaned_retail,meaned_buylist)
Avg_Velocity <- Avg_Velocity[order(Avg_Velocity$meaned_value),]
Avg_Velocity$meaned_value <- seq(nrow(Avg_Velocity))
Avg_Velocity$CK_Margin <- round(Avg_Velocity$meaned_retail - Avg_Velocity$meaned_buylist,2)
Avg_Velocity$Revenue_Perc <- round(Avg_Velocity$CK_Margin/Avg_Velocity$meaned_buylist,4)
colnames(Avg_Velocity) <- c("Key","Card","Edition","Sales_Velocity_Ranking","Avg_Retail","Avg_Buylist","Retail_Diff","Revenue_Perc")
Avg_Velocity$Avg_Retail <- round(Avg_Velocity$Avg_Retail,2)
Avg_Velocity$Avg_Buylist <- round(Avg_Velocity$Avg_Buylist,2)
Avg_Velocity$Credit_Diff <- round(Avg_Velocity$Retail_Diff * .70,2)
Avg_Velocity$Credit_Perc <- round(Avg_Velocity$Revenue_Perc * .70,4)
Clean_Df <- as.data.frame(Avg_Velocity)
rownames(Clean_Df) <- seq(nrow(Clean_Df))


 
drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
ss <- drive_get("CK_Velocity")
sheet_write(Clean_Df,
            ss = ss,
            sheet = "CK_Velocity")

setwd("/home/cujo253/Metrics/Daily_Velocity_Trackers")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)

Velocity_Tracker <- NULL

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  date_recorded <- as.Date("2020-03-13")
  tmp$date <- date_recorded
  tmp$data.name <- as.character(tmp$data.name)
  tmp$data.edition <- as.character(tmp$data.edition)
  tmp$data.is_foil <- as.character(tmp$data.is_foil)
  tmp$Velocity_Adjusted <- as.numeric(tmp$Velocity_Adjusted)
  tmp$data.price_retail <- as.numeric(tmp$data.price_retail)
  tmp$data.price_buy <- as.numeric(tmp$data.price_buy)
  date_recorded <- date_recorded + 1
  Velociraptors <- cbind(tmp$date, tmp$data.name, tmp$data.edition, tmp$data.is_foil, tmp$Velocity_Adjusted, tmp$data.price_retail, tmp$data.price_buy)
  Velocity_Tracker <- rbind(Velocity_Tracker, Velociraptors)
}

Velocity_Tracker <- as.data.frame(Velocity_Tracker)
colnames(Velocity_Tracker) <- c("Date","Card","Edition","Foil","Velocity","Retail","Buylist")
Velocity_Tracker$Key <- paste(Velocity_Tracker$Card,Velocity_Tracker$Edition, sep = "")
Velociraptor <- paste(Velocity_Tracker$Card,Velocity_Tracker$Edition, sep = "")
Velociraptor <- unique(Velociraptor)

medianed_value <- NULL
for (velociraptor in Velociraptor){
  baby_velociraptor <- Velocity_Tracker[which(Velocity_Tracker$Key == velociraptor),]
  avg_velociraptor <- round(median(as.numeric(as.character(baby_velociraptor$Velocity)), na.rm = T),4)
  medianed_value <- rbind(medianed_value, avg_velociraptor)
}

medianed_retail <- NULL
for (velociraptor in Velociraptor){
  baby_velociraptor <- Velocity_Tracker[which(Velocity_Tracker$Key == velociraptor),]
  avg_velociraptor <- round(median(as.numeric(as.character(baby_velociraptor$Retail)), na.rm = T),4)
  medianed_retail <- rbind(medianed_retail, avg_velociraptor)
}

medianed_buylist <- NULL
for (velociraptor in Velociraptor){
  baby_velociraptor <- Velocity_Tracker[which(Velocity_Tracker$Key == velociraptor),]
  avg_velociraptor <- round(median(as.numeric(as.character(baby_velociraptor$Buylist)), na.rm = T),4)
  medianed_buylist <- rbind(medianed_buylist, avg_velociraptor)
}


Med_Velocity <- data.frame(Velociraptor)
Med_Velocity$card <- Velocity_Tracker$Card[match(Med_Velocity$Velociraptor,Velocity_Tracker$Key)]
Med_Velocity$edition <- Velocity_Tracker$Edition[match(Med_Velocity$Velociraptor,Velocity_Tracker$Key)]
#Med_Velocity$foil <- Velocity_Tracker$Foil[match(Med_Velocity$Velociraptor,Velocity_Tracker$Key)]
Med_Velocity <- data.frame(Med_Velocity,medianed_value,medianed_retail,medianed_buylist)
Med_Velocity <- Med_Velocity[order(Med_Velocity$medianed_value),]
Med_Velocity$medianed_value <- seq(nrow(Med_Velocity))
Med_Velocity$CK_Margin <- round(Med_Velocity$medianed_retail - Med_Velocity$medianed_buylist,2)
Med_Velocity$Revenue_Perc <- round(Med_Velocity$CK_Margin/Med_Velocity$medianed_buylist,4)
colnames(Med_Velocity) <- c("Key","Card","Edition","Sales_Velocity_Ranking","Med_Retail","Med_Buylist","Retail_Diff","Revenue_Perc")
Med_Velocity$Med_Retail <- round(Med_Velocity$Med_Retail,2)
Med_Velocity$Med_Buylist <- round(Med_Velocity$Med_Buylist,2)
Med_Velocity$Credit_Diff <- round(Med_Velocity$Retail_Diff * .70,2)
Med_Velocity$Credit_Perc <- round(Med_Velocity$Revenue_Perc * .70,4)
Clean_Df <- as.data.frame(Med_Velocity)
rownames(Clean_Df) <- seq(nrow(Clean_Df))



drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
ss <- drive_get("CK_Velocity_(March-April)")
sheet_write(Clean_Df,
            ss = ss,
            sheet = "Median_CK_Velocity")
