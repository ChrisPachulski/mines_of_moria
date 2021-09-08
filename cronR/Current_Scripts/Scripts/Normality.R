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
data <- read_csv("/cloud/project/Metrics/TBD Updated Roster/2020-02-03BuyList_History.csv", 
                 col_types = cols(Foil = col_character()))
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
data <- data[which(data$Rarity != "C"),]
data <- data[which(data$Rarity != "U"),]
data <- data[which(data$Rarity != "R"),]
data <- data[which(data$Foil == "NF"),]
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
values = seq(from = as.Date("2019-12-20"), to = as.Date("2020-02-03"), by = 'day')
x2$Dates <- values
x2 <- x2[moveme(names(x2), "Dates first")]
data_ts <- x2[,-1]
rownames(data_ts) <- x2[,1]
#Time Series Formatting Done####
#data_ts <- as.ts(data_ts)
data_ts[] <- lapply(data_ts, function(x) as.numeric(as.character(x)))
shapiro.test(data_ts$`Sneak AttackEternal MastersM`)
Mythic_Dev <- round(sapply(data_ts,sd),2)
Mythic_Dev <-as.data.frame(Mythic_Dev)
Mythic_Dev <- setDT(Mythic_Dev, keep.rownames = TRUE)[]
Mythic_Dev <- (t(Mythic_Dev))
Mythic_Dev <-as.data.frame(list(Mythic_Dev))
names <- as.character(unlist(Mythic_Dev[1,]))
colnames(Mythic_Dev) <- as.character(unlist(Mythic_Dev[1,]))
Mythic_Dev <- Mythic_Dev[-1,]
Mythic_Dev <- as.data.frame(Mythic_Dev)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
val <- t(as.numeric.factor(unlist(Mythic_Dev)))
Mythic_Deviation <- NULL
Mythic_Deviation <- val
colnames(Mythic_Deviation) <- names
Mythic_Deviation <- as.data.frame(Mythic_Deviation)
nrow(data_ts)
ncol(data_ts)
x <- cor(data_ts)
zdf <- as.data.frame(as.table(x))
ABC <- zdf
zdf$Freq <- round(zdf$Freq,4)
zdf <- zdf[which(zdf$Freq != 1.000),]
Strong_Cor <- zdf[which(zdf$Freq < -.90),]
#Strong_Cor$Card <- data$name[match(Strong_Cor$Var1,data$Key)] 
Strong_Cor$Set <- data$Set[match(Strong_Cor$Var1,data$Key)]
#Strong_Cor$Card_2 <- data$name[match(Strong_Cor$Var2,data$Key)] 
Strong_Cor$Set_2 <- data$Set[match(Strong_Cor$Var2,data$Key)]
Strong_Cor <- Strong_Cor[order(-Strong_Cor$Freq),]
#Strong_Cor <- Strong_Cor[which(Strong_Cor$Set != "Theros Beyond Death"),]
#Strong_Cor <- Strong_Cor[which(Strong_Cor$Set_2 != "Theros Beyond Death"),]
Strong_Cor <- Strong_Cor[!duplicated(Strong_Cor$Freq),]
Strong_Cor <- Strong_Cor[moveme(names(Strong_Cor), "Freq first")]
Strong_Cor$Grouping <- Exclusion$Excl_Excl[match(Strong_Cor$Set,Exclusion$Set_Excl)]
Strong_Cor$Grouping_2 <- Exclusion$Excl_Excl[match(Strong_Cor$Set_2,Exclusion$Set_Excl)]
Strong_Cor <- Strong_Cor[Strong_Cor$Grouping == Strong_Cor$Grouping_2, ]
Strong_Cor <- Strong_Cor[which(Strong_Cor$Grouping != "Exclude"),]
Strong_Cor <- Strong_Cor[which(Strong_Cor$Set != "Theros Beyond Death"),]
Strong_Cor <- Strong_Cor[which(Strong_Cor$Set_2 != "Theros Beyond Death"),]
Final_review <- data.frame(Strong_Cor$Freq,Strong_Cor$Var1,Strong_Cor$Var2)
colnames(Final_review) <- c("Correlation", "Card_1", "Card_2")
setwd("/cloud/project/Metrics")
csvFileName <- paste(currentDate,"_Mythic_NF_Correlations",".csv",sep="")
write.csv(Final_review, file=csvFileName, row.names = FALSE)

View(Mythic_Deviation)
#StdDev(data_ts[,24])
#ncol(data_ts)
#ptg <- c(1)
#ptg <- as.data.frame(ptg)
#for (i in 1:ncol(data_ts)){
#  Final_Check <- StdDev(data_ts[,i])
#  #Normality_Check <- try(as.data.frame(t(as.matrix(unlist(shapiro.test(data_ts[,i]))))), silent = TRUE)
#  Normality_Check <- if(Final_Check == 0){
#    Normality_Check = 1
#  } else {
#    Normality_Check <- as.data.frame(t(as.matrix(unlist(shapiro.test(data_ts[,i])))))
#  }
#  P_Check <- try(as.numeric(as.character(Normality_Check$p.value)), silent = TRUE)
#  My_P_Value <- ifelse(class(P_Check) == "try-error", 0, as.numeric(as.character(Normality_Check$p.value)))
#  Normal <- ifelse(My_P_Value > .05, "NML","NN")
#  Normal <- as.data.frame(Normal)
#  ptg <- cbind(ptg,Normal$Normal)
#}

#output.norm <- ptg[,-1]
#trans <- t(output.norm)
#View(trans)
#summary(trans)


#View(Mythic_Dev)
