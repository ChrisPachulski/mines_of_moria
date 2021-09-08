library(readr)
library(reshape2)
library(zoo)
library(plyr)
library(rlist)
library(ggplot2)
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

#Analysis Stuff####
data_ts <- as.data.frame(data_ts)
data_ts$Date <- seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
#data_ts$Date <- format(data_ts$Date, format = "%Y-%m-%d")
data_ts_try <- data_ts[moveme(names(data_ts),"Date first")]
data_ts_try$`Underworld BreachTheros Beyond DeathR`
#View(data_ts_try)
desired_Column <- which( colnames(data_ts_try)=="Underworld BreachTheros Beyond DeathR")
data_ts_1 <- ts(data_ts_try, start=(1),frequency = 7)
card_1 <- plot(data_ts_1[,desired_Column], xlab="Days",ylab="Card Offers",main="BL/Time")
card_1_ts <- data_ts_1[,1]
plot(data_ts_1[,desired_Column],xlab="7 Day Periods",ylab="Offers")
ggseasonplot(data_ts_1[,desired_Column]) + ylab("degree") + ggtitle("Weekly Plot: Card offer Data")
ggseasonplot(data_ts_1[,desired_Column], polar = TRUE) + ylab("degree") + ggtitle("Weekly Plot: Card offer Data")
monthplot(data_ts_1[,desired_Column],xlab="7 Day Periods",ylab="Offers")
DCPSE <- decompose(data_ts_1[,desired_Column], type = "multiplicative")
plot(DCPSE)
DCPSE
randomness <- as.list(DCPSE$random)
randomness <- sapply(randomness, as.numeric)
#randomness[is.na(randomness)==TRUE] <- "?"
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)
cmax <- my.max(randomness)-1
cmin <- 1-my.min(randomness)
cmax_min <- c(cmax,cmin)
cavg_random <- mean(cmax_min)
cavg_random

# MAD <- Mean Absolute Deviation
#       Up/Down guage
# MAPE <- Mean Absolute Percentage Error
#       Used extensively in ts, acts as a unit free criterea allowing it to be very comparable to other ts
# MSE <- Mean Squared error
#       
# RMSE <- Root mean squared error
#       Same scale as original value now
nrow(data_ts_1)
ts_train_data <- window(data_ts_1[,desired_Column], start=c(1), end=c(10,2), freq=7)
ts_test_data <- window(data_ts_1[,desired_Column], start=c(10,3),freq=7)
Ts_data_all <- window(data_ts_1[,desired_Column], start=c(1),freq=7)
autoplot(ts_train_data, series="Train")+autolayer(ts_test_data, series="Test")+ggtitle("Training and Test Data")+xlab("Weeks")+ylab("Offer Amount")+guides(colour=guide_legend(title="Forecast"))
#Random Walk w/ Drift
# Forecasts the next period value as per the amount of change over time 
# (aka the drift, which is the evaluated average change seen in past data)
#This method can be applied along with decomposition
Ts_Train_dcpse <- stl(log10(ts_train_data), s.window='p')
TS_Train_Stl <- forecast(Ts_Train_dcpse, method ="rwdrift",h=7)
summary(TS_Train_Stl)
price_conversion <- as.data.frame(TS_Train_Stl)
10^price_conversion
plot(TS_Train_Stl, main = "Hanweir Battlements")
#CYA
(ts_test_data)
Vec2<- 10^(cbind(log10(ts_test_data), as.data.frame(forecast(Ts_Train_dcpse, method ="rwdrift",h=7))[,1]))
ts.plot(Vec2, col=c("blue","red"), main = "Hanweir Battlements: Actual vs Forecast")
RMSE2 <- round(sqrt(sum(((Vec2[,1]-Vec2[,2])^2)/length(Vec2[,1]))),4)
MAPE2 <- round(mean(abs((Vec2[,1]-Vec2[,2])/Vec2[,1])),4)
paste("Accuracy Measures: RMSE =", RMSE2,"and MAPE:",MAPE2)
CYA <- paste("When this prediction is incorrect, it will be within $",RMSE2," 68% of the time, or, within ",MAPE2*100,"% of what the true Buylist offer will be",sep="")

ts_historical_data <- window(data_ts_1[,desired_Column], start=c(1), freq=7)
thd_dcpse <- stl(log10(ts_historical_data), s.window='p')
thd_dcpse_stl <- forecast(thd_dcpse, method ="rwdrift",h=4)

Vec_Future <- 10^(as.data.frame(forecast(thd_dcpse, method ="rwdrift",h=4)))[,1]
Vec_Future

Ts_data_all[Ts_data_all == 0] <- .01
ts_train_data_HW <- hw(Ts_data_all, seasonal = "multiplicative", h = 7)
plot(ts_train_data_HW)
summary(ts_train_data_HW)
# Beta demonstrates the variance in the trend, closer to 0 is better
# Gamma demonstrates how strong the seasonal affect on the data is, a higher value indicates a greater effect
df <- as.data.frame(ts_train_data_HW)
df
HW_Vec2 <- cbind(ts_test_data,df[,1])
ts.plot(HW_Vec2, col=c("blue","red"), main = "PROSSH: Actual vs Forecast")
RMSE2 <- round(sqrt(sum(((HW_Vec2[,1]-HW_Vec2[,2])^2)/length(HW_Vec2[,1]))),4)
MAPE2 <- round(mean(abs((HW_Vec2[,1]-HW_Vec2[,2])/HW_Vec2[,1])),4)
HW_CYA <- paste("When this prediction is incorrect, it will be within $",RMSE2," 68% of the time, or, within ",MAPE2*100,"% of what the true Buylist offer will be",sep="")
HW_CYA
CYA
plot(hw(Ts_data_all, seasonal = "multiplicative", h = 7))
