source("config.R")
#Packages####
setwd("/home/cujo253/cronR/")
#install.packages("doParallel")
library(foreach)
library(doParallel)
library(reshape2)
library(zoo)
library(plyr)
library(rlist)
library(fpp2)
library(forecast)
library(stats)
library(tseries)
library(data.table)
library(gtools)
library(tidyverse) # Data Manipulation
library(reshape2)
#install.packages("readr",INSTALL_opts = '--no-lock',force = TRUE)
library(readr)
library(rvest)
library(taRifx)
library(knitr)     # Pretty HTML Tables
library(purrr)     # Allows for the replacement of loops and suite of apply functions
library(tibble)    # Breakdown further elements
library(tictoc)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
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

#Time Series Analysis for BL####
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
currentDate <- Sys.Date()
#Set Today's Date

setwd("/home/cujo253/Reports/High Confidence Reps")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)-1
currentDate <- Sys.Date()

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(8:12)]
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Updated_Tracking_Keys$Key <- trimws(Updated_Tracking_Keys$Key)
#View(Updated_Tracking_Keys)
New_Roster <- Updated_Tracking_Keys

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default =  "c"))
  tmp$Sellers <- gsub("[^0-9.-]", NA,as.character(tmp$Sellers))
  Updated_Tracking_Keys$New <- tmp$BL[match(Updated_Tracking_Keys$Key,tmp$Key)]
  Updated_Tracking_Keys$New <- as.numeric(as.character(Updated_Tracking_Keys$New))
  New_Roster <- cbind(New_Roster,Updated_Tracking_Keys$New) 
}

C20_Updated_Roster <- New_Roster

colnames(New_Roster) <- c("Key","Name","Set","Rarity","Foil",format(seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day'),format = "%Y-%m-%d") )
New_Roster$Foil <- as.character(New_Roster$Foil)

New_Roster$Foil[is.na(New_Roster$Foil)==T] <- ""
New_Roster[is.na(New_Roster)==T] <- 0

setwd("/home/cujo253/Metrics/TBD Updated Roster")
csvFileName <- paste("3_BuyList_History",".csv",sep="")
write.csv(New_Roster, file=csvFileName, row.names = FALSE) 

Current_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/3_BuyList_History.csv", sep ="")
#Make it automate-able
Current_Buylist_Tracker <- as.data.frame(read_csv(Current_Buylist_Tracker, 
                                                  col_types = cols(Foil = col_character())))



#Three_Week_Movers <- read_csv(Three_Week_Movers)
#Keys_Of_Interest <- Three_Week_Movers$Key
Keys_Of_Interest <- Current_Buylist_Tracker$Key
Keys_Of_Interest <- as.data.frame(Keys_Of_Interest)
colnames(Keys_Of_Interest) <- c("Key")
Keys_Of_Interest <- merge(Keys_Of_Interest,Current_Buylist_Tracker, by="Key")
#Pull it into a variable (ensure foil is as character)
data <- Keys_Of_Interest
data$Exclude <- Exclusion$Excl_Excl[match(data$Set, Exclusion$Set_Excl)]
data <- data[which(data$Exclude != "Exclude"),]
data <- data[-ncol(data)]
#Convert to "data" for simplicity
nrow(data)
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)

data <- data[which(data$Foil == "NF"),]
#data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
#Remove the Foils bc foils are gross
data <- data[which(data$Rarity != "U"),] #<- Rarity Selection Tool
data <- data[which(data$Rarity != "C"),] 
trans_data <- t(data)
#Flip the data so the keys are on top
removed_data <- as.data.frame(trans_data)

headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
#Store the headers away to be repulled back in after transformation is complete
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))

#Ensure all Buy List values are numerical for calculation
meaned_data <- round(na.aggregate(meaned_data),2)
#If* cards have NA's, if there was no offer, the NA's will be filled in with the mean offer it has had in the time period selected
meaned_data <- sapply(meaned_data, as.factor)

Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)

#Rejoined headers with numeric Values
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
#Removed any lingering NaN's (potential for Inf & -Inf that would also need to be addressed should they arise)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]


#Once again, ax those headers for future ts conversion
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
length(values)
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))

Buy_List_ts <- as.data.frame(Buy_List_ts)

values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Buy_List_ts <- Buy_List_ts[-nrow(Buy_List_ts),]
training_rows <- round(nrow(Buy_List_ts)*.70,0)
test_rows <- round(training_rows:nrow(Buy_List_ts),0)

Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

for (i in 1:1000) {
  Frequent_Value <- round((nrow(Buy_List_ts)/i),0)
  if(Frequent_Value == 7){ts_frequency_value <- i}#else{ts_frequency_value <- print("Increase Range")}
}


Time_Series_Training_Dates <- read_csv("/home/cujo253/cronR/Time_Series_Training_Dates.csv")
Training_Week <- Time_Series_Training_Dates$Training_Week[match(currentDate,Time_Series_Training_Dates$Date)]
Training_Day <- Time_Series_Training_Dates$Training_Day[match(currentDate,Time_Series_Training_Dates$Date)]
Test_Week <- Time_Series_Training_Dates$Test_Week[match(currentDate,Time_Series_Training_Dates$Date)]
Test_Day <- Time_Series_Training_Dates$Test_Day[match(currentDate,Time_Series_Training_Dates$Date)]


ts_frequency_value <- ts_frequency_value-1
Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = 7)

#Holts-Winter Analysis#
#All unique Keys to cycle through for time Series Analysis

Forecast_DF <- (1:(nrow(Buy_List_ts)+9))
Forecast_DF <- as.data.frame(Forecast_DF)

total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
#for(Key in Reformatted_Keys[c(1:nrow(Reformatted_Keys)),])
Start_Time <- Sys.time() 
cl <- makeCluster(3, type = "FORK")
registerDoParallel(cl)

Forecast_DF_Par <- foreach(Key = Reformatted_Keys[c(1:total),], .packages = c("forecast"),.combine=cbind)%dopar% {
  Sys.sleep(.25)
  desired_Column <- colnames(Buy_List_ts[Key])
  #Buy_List_ts_plot <- plot(Buy_List_ts_final[,desired_Column], xlab="Days",ylab="Card Offers",main= Key)
  bl_train_data <- window(Buy_List_ts_final[,desired_Column], start=c(1), end=c(Training_Week,Training_Day), freq=7)
  bl_test_data <- window(Buy_List_ts_final[,desired_Column], start=c(Test_Week,Test_Day),freq=7)
  bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=7)
  bl_train_data[bl_train_data == 0] <- .01
  bl_train_data_hw <- hw(bl_train_data, seasonal = "multiplicative", h = 7)
  df <- as.data.frame(bl_train_data_hw)
  
  HW_Vec2 <- cbind(bl_test_data,df[,1])
  #ts.plot(HW_Vec2, col=c("blue","red"), main = "Actual (Blue) vs Forecast (Red)")
  RMSE2 <- round(sqrt(sum(((HW_Vec2[,1]-HW_Vec2[,2])^2)/length(HW_Vec2[,1]))),4)
  MAPE2 <- round(mean(abs((HW_Vec2[,1]-HW_Vec2[,2])/HW_Vec2[,1])),4)
  
  #HW_CYA <- paste("When this prediction is incorrect, it will be within $",RMSE2," 68% of the time, or, within ",MAPE2*100,"% of what the true Buylist offer will be",sep="")
  bl_data_all[bl_data_all == 0] <- .01
  bl_data_all_hw <- hw(bl_data_all, seasonal = "multiplicative", h = 7)
  df <- as.data.frame(bl_data_all_hw)
  
  Future_Forecast_Values <- rbind(as.data.frame(df[,1]),RMSE2)
  Future_Forecast_Values <- rbind(Future_Forecast_Values, MAPE2)
  Future_Forecast_Values <- as.data.frame(Future_Forecast_Values)
  
  Seperated_Column <- as.list(Buy_List_ts_final[,desired_Column])
  Future_Forecast_Values <- as.list(Future_Forecast_Values)
  Present_Future <- append(Seperated_Column, Future_Forecast_Values)
  New_Forecast_Column <-round(data.frame(unlist(Present_Future)),2)
  colnames(New_Forecast_Column) <- colnames(Buy_List_ts[Key])
  Forecast_DF <- cbind(Forecast_DF, New_Forecast_Column)
  
}

End_Time <- Sys.time()
End_Time - Start_Time
stopCluster(cl)
#CK####
Limit <- as.numeric(trimws(unlist(as.list(read_html("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any") %>% html_nodes("li") %>% html_text())[853])))
Start_Time <- Sys.time() 
cl <- makeCluster(3, type = "FORK")
registerDoParallel(cl)

CK_Result <- foreach(i = 1:Limit, .packages = c("rvest","httr"),.combine=rbind)%dopar% {
  Sys.sleep(.25)
  CK_Results <- GET(paste("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep=""))#, body = body)
  Card <- content(CK_Results,"text") %>% read_html %>% html_nodes(".productDetailTitle") %>% html_text()
  Set <- gsub(" \\([A-Z]\\)$","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())) 
  Rarity <- gsub("\\)","",gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())))
  Price <- as.numeric(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))[seq(1, length(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))),4)])
  key <- paste(Card, Set, Rarity)
  Results <- data.frame(key,Card,Set,Rarity,Price,i)
}
End_Time <- Sys.time()
End_Time-Start_Time
stopCluster(cl)
CK_Result <- CK_Result[order(CK_Result$i),]
CK_Result$i <- seq(nrow(CK_Result))
colnames(CK_Result)[6] <- "Rank"