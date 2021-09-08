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
#Exclusions####
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
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
#Exclusions Entered - Proceed####
#Foreplay - TS Formation from Tracker####
currentDate <- Sys.Date()
#Set Today's Date
#Three_Week_Movers <- paste("/home/cujo253/Reports/KPI/Master/",currentDate,"_Master_KPI.csv",sep ="")
Current_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/3_BuyList_History.csv", sep ="")
#Make it automate-able
Current_Buylist_Tracker <- read_csv(Current_Buylist_Tracker, col_types = cols(Foil = col_character()))
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
data <- data[-84]

#Convert to "data" for simplicity
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
data <- data[which(data$Foil == "NF"),]
#data <- data[which(data$name == "Azusa, Lost but Seeking"),]
#data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
#Remove the Foils bc foils are gross
#data <- data[which(data$name == "Volcanic Island"),] #<- Rarity Selection Tool
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
Buy_List_ts <- Buy_List_ts[-nrow(Buy_List_ts),]
training_rows <- round(nrow(Buy_List_ts)*.70,0)
test_rows <- round(training_rows:nrow(Buy_List_ts),0)

Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)


GUI_Dataframe <- as.data.frame(Buy_List_ts)
#GUI_Dataframe <- GUI_Dataframe[c((nrow(GUI_Dataframe)-40):nrow(GUI_Dataframe) ),]
GUI_Dataframe$"Argothian EnchantressUrza's SagaR"

library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= `Argothian EnchantressUrza's SagaR`, label = `Argothian EnchantressUrza's SagaR`)) + 
  geom_line(aes(y= `Argothian EnchantressUrza's SagaR`), colour="green", size = 1.25) + 
  geom_text(colour = "white",vjust = 0.00, nudge_y = 0.08, size = 3,check_overlap = TRUE)+
  labs(title="Argothian EnchantressUrza's SagaR", 
       subtitle="CardKingdom Buy List Offers", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "1 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))

desired_Graph
# 
# names <- colnames(GUI_Dataframe[c(1:nrow(GUI_Dataframe)),])
# names <- as.data.frame(names)
# GUI_Dataframe$`Abbot of Keral KeepMagic OriginsR FOIL`
#for(i in 2:ncol(GUI_Dataframe))

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= `Argothian EnchantressUrza's SagaR`, label = `Argothian EnchantressUrza's SagaR`)) + 
  geom_line(aes(y=`Argothian EnchantressUrza's SagaR`), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Argothian EnchantressUrza's SagaR", 
       subtitle="Via CK Buylist", 
       caption="Source: WolfofTinStreet", 
       y="$ Offer") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Argothian Enchantress",".png",sep="")
ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/", width = 15,height = 5 , limitsize = T)
desired_Graph

