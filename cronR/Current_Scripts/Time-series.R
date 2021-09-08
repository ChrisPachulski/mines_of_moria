library(readr)
library(reshape2)
library(zoo)
library(plyr)
library(rlist)
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
#Foreplay####
currentDate <- Sys.Date()-1
Current_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",currentDate,"BuyList_History.csv", sep ="")
Current_Buylist_Tracker <- read_csv(Current_Buylist_Tracker, col_types = cols(Foil = col_character()))
data <- Current_Buylist_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
#data <- data[which(data$Rarity == "M"),]
#View(data)
trans_data <- t(data)
#removed_data <- trans_data[-c(1:5),]
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
#meaned_data <- meaned_data[colSums(!is.na(meaned_data)) > 0]
meaned_data <- round(na.aggregate(meaned_data),2)
#View(meaned_data)
meaned_data <- sapply(meaned_data, as.factor)
x <- rbind(headers, meaned_data)
x <- as.data.frame(x)
x2 <- Filter(function(x) !any(x=="NaN"), x)
names(x2) <- as.matrix(x2[1,])
x2 <- x2[-c(1:5),]
#View(x2)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
x2$Dates <- values
x2 <- x2[moveme(names(x2), "Dates first")]
data_ts <- x2[,-1]
rownames(data_ts) <- x2[,1]
#Time Series Formatting Done####
#data_ts <- as.ts(data_ts)
data_ts[] <- lapply(data_ts, function(x) as.numeric(as.character(x)))
#view(data_ts)
#data_ts <- sapply(data_ts, as.character)
#data_ts <- sapply(data_ts, as.numeric)
#data_ts <- as.data.frame(data_ts)
#View(data_ts)
library(MASS)
#install.packages("tseries")
library(tseries)
library(forecast)
library(ggplot2)
head(data_ts)
COI <- data_ts$`Sphinx Ambassador2010 Core SetM`
plot(COI,type="l", xlab = "Days", ylab = "Buy List Offer", main = "Card of Interest")


lnstonks <- log(data_ts)
lnstonks[is.na(lnstonks)] <- 0
lnstonks[is.nan(lnstonks)] <- 0
lnstonks[lnstonks == -Inf] <- 0
lnstonks[lnstonks == Inf] <- 0
#View(lnstonks)
trn_rows <- round(nrow(data_ts) * .7,0)
tst_rows <- (nrow(data_ts)-(trn_rows))
tst_rows <- as.numeric(tst_rows)
trn_data <- lnstonks[(1:trn_rows),]                  
tst_data <- lnstonks[((trn_rows+1):nrow(lnstonks)),]
trn_data <- as.data.frame(trn_data)

Arima_Results <- NULL
3+tst_rows
Arima_Results <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
Arima_Results <- as.data.frame(Arima_Results)
total = ncol(lnstonks) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
#View(lnstonks)
#ncol(lnstonks)
#lnstonks[13]
for (i in 1:ncol(lnstonks)){
measures <- NULL
#acf(trn_data[,i], lag.max=20)
#pacf(trn_data[,i], lag.max=20)
#adf.test(trn_data[,i])
diff <- diff(as.matrix(trn_data),1)
p1_test <- as.data.frame(t(as.matrix(unlist(adf.test(diff[,13])))))
p1_test$p.value <- as.numeric(as.character(p1_test$p.value))
p1_test <- ifelse(p1_test$p.value <= .05, "Pass","Fail")
p1_test <- ifelse(is.na(p1_test) == TRUE, "Fail", p1_test)
blarima <- ts(trn_data[,i], frequency = 1)
fittrn <- auto.arima(blarima)
fut_vals_ln <- forecast(fittrn, h = tst_rows)
fut_vals_ln <- as.numeric(fut_vals_ln$mean)
fut_vals <- round(exp(fut_vals_ln),1)
fut_vals
df <- data.frame(exp(tst_data[,i]), fut_vals)
col_headings <- c("Actual","Future")
names(df) <- col_headings
perc_err <- mean(((df$Actual-df$Future)/df$Actual))
resid_check <- as.data.frame(t(as.matrix(unlist(Box.test(fittrn$residuals, lag=5, type="Ljung-Box")))))
resid_check$p.value <- as.numeric(as.character(resid_check$p.value))
pass_fail <- ifelse(resid_check$p.value >= .05, "Pass", "Fail")
blarima <- ts(lnstonks[,i], frequency = 1)
fittrn <- auto.arima(blarima)
forec_vals <- forecast(fittrn, h = tst_rows)
forec_vals <- as.numeric(forec_vals$mean)
forec_vals <- round(exp(forec_vals),1)
forec_vals <- as.data.frame(forec_vals)
resid_check <- as.data.frame(t(as.matrix(unlist(Box.test(fittrn$residuals, lag=5, type="Ljung-Box")))))
resid_check$p.value <- as.numeric(as.character(resid_check$p.value))
pass_fail <- ifelse(resid_check$p.value >= .05, "Pass", "Fail")
pass_fail
measures <- rbind(measures,p1_test)
measures <- rbind(measures,round(perc_err,2))
measures <- rbind(measures,pass_fail)
measures <- as.data.frame(measures)
measures <- data.frame(measures = c(measures[,"V1"], forec_vals[,"forec_vals"]))
measures[1,] <- p1_test
measures[2,] <- round(perc_err,2)
measures[3,] <- pass_fail
Arima_Results <- cbind(Arima_Results, measures)
setTxtProgressBar(pb,i)
}
Forecast_Outputs <- Arima_Results[,-1]
View(Forecast_Outputs)

the_card_name_headers <- x2[1,]
the_card_name_headers <- the_card_name_headers[,-1]


data_ts_fore <- rbind.fill(data_ts, Forecast_Outputs)
#View(data_ts_fore)
data_ts_fore[nrow(data_ts)+1:nrow(Forecast_Outputs),] = Forecast_Outputs

rebuild_1 <- t(as.matrix(data_ts_fore))
rebuild_1 <- as.data.frame(rebuild_1)
library(data.table)
rebuild_1 <- setDT(rebuild_1, keep.rownames = TRUE)[]
rebuild_1 <- as.data.frame(rebuild_1)
Dates <- seq(from = as.Date("2019-12-20"), to = as.Date("2020-02-21"), by = 'day')
colnames(rebuild_1) = Dates
colnames(rebuild_1)[1] <- "Key"
View(rebuild_1)
before <- nrow(rebuild_1)
rebuild_1 <- rebuild_1[which(rebuild_1$`2020-02-05` != "Fail"),]
after_fail_1 <- nrow(rebuild_1)
rebuild_1 <- rebuild_1[which(rebuild_1$`2020-02-07` != "Fail"),]
after_fail_2 <- nrow(rebuild_1)
rebuild_1$name <- data$name[match(rebuild_1$Key,data$Key)]
rebuild_1$Set <- data$Set[match(rebuild_1$Key,data$Key)]
rebuild_1$Rarity <- data$Rarity[match(rebuild_1$Key,data$Key)]
rebuild_1$Foil <- data$Foil[match(rebuild_1$Key,data$Key)]
rebuild_1 <- as.data.frame(rebuild_1)
rebuild_1 <- rebuild_1[moveme(names(rebuild_1), "name after Key; Set after name; Rarity after Set; Foil after Rarity")]
Arima_Accuracy <- data_frame(rebuild_1$`2020-02-05`,rebuild_1$`2020-02-06`, rebuild_1$`2020-02-07`)
colnames(Arima_Accuracy) <- c("test_1","Error %","test_2")
summary(rebuild_1)
ncol(rebuild_1)
rebuild_1 <- rebuild_1[,-c(52:54)]
rebuild_1$Error_Pct <- Arima_Accuracy$`Error %`
rebuild_1$Days_Forecasted  <- rep(tst_rows,nrow(rebuild_1))
Final_Df <- rebuild_1
getwd()
setwd("/cloud/project/Forecasting")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Arima_Mythic",".csv",sep="")
write.csv(Final_Df, file=csvFileName, row.names = FALSE)
