#Packages####
install.packages("ggplot2")
install.packages("readr")
install.packages("reshape2")
install.packages("zoo")
install.packages("plyr")
install.packages("rlist")
install.packages("tidyr")
install.packages("fpp2")
install.packages("forecast")
install.packages("stats")
install.packages("tseries")
install.packages("data.table")
install.packages("gtools")
library(readr)
library(reshape2)
library(zoo)
library(plyr)
library(rlist)
library(dplyr)
library(tidyr)
library(fpp2)
library(forecast)
library(stats)
library(tseries)
library(data.table)
library(gtools)
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
#Foreplay - TS Formation from Tracker####
currentDate <- Sys.Date()
#Set Today's Date
Three_Week_Movers <- paste("/home/cujo253/Reports/KPI/Master/",currentDate,"_Master_KPI.csv",sep ="")
Current_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",currentDate,"BuyList_History.csv", sep ="")
#Make it automate-able
Current_Buylist_Tracker <- read_csv(Current_Buylist_Tracker, col_types = cols(Foil = col_character()))
Three_Week_Movers <- read_csv(Three_Week_Movers)
Keys_Of_Interest <- Three_Week_Movers$Key
Keys_Of_Interest <- as.data.frame(Keys_Of_Interest)
colnames(Keys_Of_Interest) <- c("Key")
Keys_Of_Interest <- merge(Keys_Of_Interest,Current_Buylist_Tracker, by="Key")
#Pull it into a variable (ensure foil is as character)
data <- Keys_Of_Interest
#Convert to "data" for simplicity
#data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
#Remove the Foils bc foils are gross
#data <- data[which(data$Rarity == "M"),] #<- Rarity Selection Tool
trans_data <- t(data)
#Flip the data so the keys are on top
removed_data <- as.data.frame(trans_data)
#View(removed_data)
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
ncol(Cleaned_Recombined_data)
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
ncol(Cleaned_Recombined_data)
ncol(Titless_Recombined_data)
#Once again, ax those headers for future ts conversion
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
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
training_rows <- round(nrow(Buy_List_ts)*.70,0)
test_rows <- round(training_rows:nrow(Buy_List_ts),0)

Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

for (i in 1:1000) {
  Frequent_Value <- (nrow(Buy_List_ts)/i)
  if(Frequent_Value == 7){ts_frequency_value <- i}#else{ts_frequency_value <- print("Increase Range")}
}

ts_frequency_value
Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = ts_frequency_value)

#Random Walk W/ Drift Analysis####
#All Unique Keys to cycle through for time Series Analysis

Forecast_DF <- (1:(nrow(Buy_List_ts)+9))
Forecast_DF <- as.data.frame(Forecast_DF)

total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
Start_Time <- Sys.time()
#for(Key in Reformatted_Keys[c(1:nrow(Reformatted_Keys)),])
for(Key in Reformatted_Keys[c(1:total),]){
desired_Column <- which( colnames(Buy_List_ts)== Key)
#Buy_List_ts_plot <- plot(Buy_List_ts_final[,desired_Column], xlab="Days",ylab="Card Offers",main= Key)
bl_train_data <- window(Buy_List_ts_final[,desired_Column], start=c(1), end=c(7,4), freq=ts_frequency_value)
bl_test_data <- window(Buy_List_ts_final[,desired_Column], start=c(7,5),freq=ts_frequency_value)
bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=ts_frequency_value)
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
colnames(New_Forecast_Column) <- c(Key)
Forecast_DF <- cbind(Forecast_DF, New_Forecast_Column)

setTxtProgressBar(pb, Key)
}

End_Time <- Sys.time()
End_Time - Start_Time

#View(Forecast_DF)
FC_DF <- Forecast_DF[,-1]
FC_DF <- t(FC_DF)
FC_DF <- as.data.frame(FC_DF)
All_Cards <- read_csv("/home/cujo253/Reports/All_Cards_TBD.csv", col_types = cols(`F/NF` = col_character()))
Reformatted_Keys$Name <- All_Cards$name[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Set <- All_Cards$Set[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Rarity <- All_Cards$Rarity[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$'F/NF' <- All_Cards$`F/NF`[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
ncol(FC_DF)

format(currentDate, format = "%Y-%m-%d")

Final_Touches <- data.frame(Reformatted_Keys, FC_DF[c((ncol(FC_DF)-9):ncol(FC_DF))])
colnames(Final_Touches) <- c("Key","Name","Set","Rarity","F/NF",format(currentDate,format = "%Y-%m-%d"),format(currentDate+1,format = "%Y-%m-%d"),format(currentDate+2,format = "%Y-%m-%d"),format(currentDate+3,format = "%Y-%m-%d"),format(currentDate+4,format = "%Y-%m-%d"),format(currentDate+5,format = "%Y-%m-%d"),format(currentDate+6,format = "%Y-%m-%d"),format(currentDate+7,format = "%Y-%m-%d"),"RMSE","MAPES")
Final_Touches$`F/NF`[is.na(Final_Touches$`F/NF`)==TRUE]<- ""
Filtered_Angle <- Final_Touches[which(Final_Touches$MAPES<.10),]
Filtered_Angle$Diff <- Filtered_Angle[,12]-Filtered_Angle[,6]
Filtered_Angle <- Filtered_Angle[order(-Filtered_Angle$Diff),]
rownames(Filtered_Angle) <- seq(nrow(Filtered_Angle))

#Neural Network Time Series####
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

for (i in 1:1000) {
  Frequent_Value <- (nrow(Buy_List_ts)/i)
  if(Frequent_Value == 7){ts_frequency_value <- i}#else{ts_frequency_value <- print("Increase Range")}
}

ts_frequency_value
Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = ts_frequency_value)
NNETAR_DF <- (1:(nrow(Buy_List_ts)+9))
NNETAR_DF <- as.data.frame(NNETAR_DF)

total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
Start_Time <- Sys.time()
#(Key in Reformatted_Keys[c(1:total),])
for(Key in Reformatted_Keys[c(1:total),]){
  desired_Column <- which( colnames(Buy_List_ts)== Key)
  Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = ts_frequency_value)

  #Buy_List_ts_plot <- plot(Buy_List_ts_final[,desired_Column], xlab="Days",ylab="Card Offers",main= Key)
  bl_train_data <- window(Buy_List_ts_final[,desired_Column], start=c(1), end=c(7,4), freq=ts_frequency_value)
  bl_test_data <- window(Buy_List_ts_final[,desired_Column], start=c(7,5),freq=ts_frequency_value)
  bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=ts_frequency_value)
  
  set.seed(253)
  fit = nnetar(bl_train_data)
  nnetforecast <- forecast(fit, h=7, PI = F)
  #autoplot(bl_test_data,series="Test Set")+autolayer(nnetforecast, series = "Forecast")+ggtitle("Test vs Forecast")+xlab("Weeks")+ylab("Offer Amount")+guides(colour=guide_legend(title="Forecast"))
  
  Vec2<- (cbind(bl_test_data, as.data.frame(forecast(fit, h=7, PI = F))[,1]))
  #ts.plot(Vec2, col=c("blue","red"), main = "Actual vs Forecast")
  RMSE2 <- round(sqrt(sum(((Vec2[,1]-Vec2[,2])^2)/length(Vec2[,1]))),4)
  MAPE2 <- round(mean(abs((Vec2[,1]-Vec2[,2])/Vec2[,1])),4)
  
  fit_all <- nnetar(bl_data_all)
  nnetforecast_all <- forecast(fit, h=7, PI = F)
  
  df <- as.data.frame(nnetforecast_all)
  Future_Forecast_Values <- rbind(as.data.frame(df[,1]),RMSE2)
  Future_Forecast_Values <- rbind(Future_Forecast_Values, MAPE2)
  Future_Forecast_Values <- as.data.frame(Future_Forecast_Values)
  
  Seperated_Column <- as.list(Buy_List_ts_final[,desired_Column])
  Future_Forecast_Values <- as.list(Future_Forecast_Values)
  Present_Future <- append(Seperated_Column, Future_Forecast_Values)
  NN_New_Forecast_Column <-round(data.frame(unlist(Present_Future)),2)
  colnames(NN_New_Forecast_Column) <- c(Key)
  NNETAR_DF <- cbind(NNETAR_DF, NN_New_Forecast_Column)
  
  setTxtProgressBar(pb, Key)
}

End_Time <- Sys.time()
End_Time - Start_Time

FC_DF <- NNETAR_DF[,-1]
FC_DF <- t(FC_DF)
FC_DF <- as.data.frame(FC_DF)
Reformatted_Keys$Name <- All_Cards$name[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Set <- All_Cards$Set[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Rarity <- All_Cards$Rarity[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$'F/NF' <- All_Cards$`F/NF`[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]

NN_Final_Touches <- data.frame(Reformatted_Keys, FC_DF[c((ncol(FC_DF)-9):ncol(FC_DF))])
colnames(NN_Final_Touches) <- c("Key","Name","Set","Rarity","F/NF",format(currentDate,format = "%Y-%m-%d"),format(currentDate+1,format = "%Y-%m-%d"),format(currentDate+2,format = "%Y-%m-%d"),format(currentDate+3,format = "%Y-%m-%d"),format(currentDate+4,format = "%Y-%m-%d"),format(currentDate+5,format = "%Y-%m-%d"),format(currentDate+6,format = "%Y-%m-%d"),format(currentDate+7,format = "%Y-%m-%d"),"RMSE","MAPES")
NN_Final_Touches$`F/NF`[is.na(NN_Final_Touches$`F/NF`)==TRUE]<- ""
NN_Filtered_Angle <- NN_Final_Touches[which(NN_Final_Touches$MAPES<.10),]
NN_Filtered_Angle$Diff <- NN_Filtered_Angle[,12]-NN_Filtered_Angle[,6]
NN_Filtered_Angle <- NN_Filtered_Angle[order(-NN_Filtered_Angle$Diff),]
rownames(NN_Filtered_Angle) <- seq(nrow(NN_Filtered_Angle))
#View(NN_Filtered_Angle)

Time_Series_keys <- as.list(NN_Filtered_Angle$Key,Filtered_Angle$Key)
Time_Series_keys <- as.data.frame((Time_Series_keys))
Time_Series_keys <- t(Time_Series_keys)
Time_Series_keys <- as.data.frame(Time_Series_keys)
#rownames(Time_Series_keys)<- seq(nrow(Time_Series_keys))
colnames(Time_Series_keys)<-"Key"
Time_Series_keys <- as.data.frame(Time_Series_keys)
colnames(Time_Series_keys)<-"Key"
rownames(Time_Series_keys) <- seq(nrow(Time_Series_keys))
Time_Series_keys$name <- All_Cards$name[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$Set <- All_Cards$Set[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$Rarity <- All_Cards$Rarity[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$'F/NF' <- All_Cards$`F/NF`[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$Todays_BL <- Filtered_Angle[,6][match(Time_Series_keys$Key,Filtered_Angle$Key)]
Time_Series_keys$`F/NF`[is.na(Time_Series_keys$`F/NF`)==TRUE]<-""
Final_Compilation <- na.omit(Time_Series_keys)
Final_Compilation$HW_Prediction <- Filtered_Angle[,13][match(Final_Compilation$Key,Filtered_Angle$Key)]
Final_Compilation$NN_Prediction <- NN_Filtered_Angle[,13][match(Final_Compilation$Key,NN_Filtered_Angle$Key)]
Final_Compilation$HW_Prediction <- Final_Compilation[,7] - Final_Compilation[,6]
Final_Compilation$NN_Prediction <- Final_Compilation[,8] - Final_Compilation[,6]
Joint_Increase <- Final_Compilation[which(Final_Compilation[,7]>0),]
Joint_Increase <- Joint_Increase[which(Joint_Increase[,8]>0),]
rownames(Joint_Increase)<- seq(nrow(Joint_Increase))
#EMA - Exponential Moving Average Class####
library(TTR)
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

for (i in 1:1000) {
  Frequent_Value <- (nrow(Buy_List_ts)/i)
  if(Frequent_Value == 7){ts_frequency_value <- i}#else{ts_frequency_value <- print("Increase Range")}
}

ts_frequency_value
Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = ts_frequency_value)
NNETAR_DF <- (1:(nrow(Buy_List_ts)+9))
NNETAR_DF <- as.data.frame(NNETAR_DF)

total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
Start_Time <- Sys.time()
#(Key in Reformatted_Keys[c(1:total),])
for(Key in Reformatted_Keys[c(1:total),]){
  desired_Column <- which( colnames(Buy_List_ts)== Key)
  #desired_Column <- 50
  Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = ts_frequency_value)
  
  bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=ts_frequency_value)

  EMA_Model <- round(EMA(bl_data_all, n=ts_frequency_value),1)
  
  Picture_Name <- paste("/home/cujo253/Plots/Flat_Histories/",print(Key),".png", sep="")
  png(file=Picture_Name, width=2000, height=600)
  par(mfrow=c(1,2))
  plot(round(bl_data_all,1), xlab = print("Time (/Weeks)"), ylab = print(Key), main = "No Smoothing")
  plot(EMA_Model, type = "l", xlab = print("Time (/Day)"), ylab = print(Key), main = "EMA Smoothing")
  dev.off()
  
  #Picture_Name <- paste("/home/cujo253/Plots/",print(Key),"_Smoothed.png", sep="")
  #png(file=Picture_Name, width=1000, height=600)
  #plot(EMA_Model, type = "l", xlab = print("Time (/Day)"), ylab = print(Key), main = "EMA Smoothing")
  #dev.off()
  
}





#Google SHeets Export####
library(devtools)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

my_dfs <- list(Joint_Increase,Final_Compilation, Filtered_Angle, NN_Filtered_Angle)
sheets_auth()
sheets_create(
  paste(currentDate,"_HW_TS_Results",sep=""),
  sheets = my_dfs
)



#Google SHeets Export####
library(devtools)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

my_dfs <- list(Filtered_Angle, NN_Filtered_Angle,Final_Compilation)
sheets_auth()
sheets_create(
  paste(currentDate,"_HW_TS_Results",sep=""),
  sheets = my_dfs
)
