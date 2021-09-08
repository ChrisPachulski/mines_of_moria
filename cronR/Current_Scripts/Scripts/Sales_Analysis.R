#Packages####
library("rpart")
library("rpart.plot") 
library("readr")
library(C50)
library(party)
library(partykit)
library(Hmisc)
library(neuralnet)
library(cluster)
library(fpc)
library(psych)
library(plyr)
library(plotrix)
library(randomForest)
library(ggplot2)
#Basic Formattting####
mtg = read_csv("Bills & MTG - R Data (mtg).csv")
`Card Analysis` <- read_csv("Bills & MTG - R Data - Card Analysis.csv")
Card_Colour = read_csv("Bills & MTG - Card_Colour.csv")

`Card Analysis` =as.data.frame(`Card Analysis`)
`Card Analysis`$Qty = as.numeric(`Card Analysis`$Qty)
`Card Analysis`$Qty =  ifelse(`Card Analysis`$Qty <= 1, 1, ifelse(`Card Analysis`$Qty <= 2, 2, ifelse(`Card Analysis`$Qty <=5, 4, ifelse(`Card Analysis`$Qty <=9, 8, ifelse(`Card Analysis`$Qty <=13, 12, 15)))))
`Card Analysis`$Days.Held = as.numeric(`Card Analysis`$Days.Held)
`Card Analysis`$Days.Held = ifelse(`Card Analysis`$Days.Held <= 15, 15 ,ifelse(`Card Analysis`$Days.Held <= 30, 30, ifelse(`Card Analysis`$Days.Held <= 60, 60, ifelse(`Card Analysis`$Days.Held <= 90,90,ifelse(`Card Analysis`$Days.Held <= 120, 120, ifelse(`Card Analysis`$Days.Held <= 180,180,ifelse(`Card Analysis`$Days.Held <= 240,240, ifelse(`Card Analysis`$Days.Held <= 365, 365, ifelse(`Card Analysis`$Days.Held >= 366, 366, 0)))))))))

`Card Analysis` =as.data.frame(`Card Analysis`)

Sold_Card = subset(mtg, Sold=="Yes")
Sold_Card = Sold_Card[,-1]
summary(Sold_Card)
Sold_Card$Day.Sold = as.factor(Sold_Card$Day.Sold)
Sold_Card$Day.Sold = ifelse(Sold_Card$Day.Sold %in% c('Monday'),1,ifelse(Sold_Card$Day.Sold %in% c('Tuesday'),2,ifelse(Sold_Card$Day.Sold %in% c('Wednesday'),3,ifelse(Sold_Card$Day.Sold %in% c("Thursday"),4,ifelse(Sold_Card$Day.Sold %in% c("Friday"),5,ifelse(Sold_Card$Day.Sold %in% c("Saturday"),6,ifelse(Sold_Card$Day.Sold %in% c("Sunday"),7,0)))))))
Sold_Card$Day.Sold = as.numeric(Sold_Card$Day.Sold)
Sold_Card = as.data.frame(Sold_Card)
Sold_Card$Profit = ifelse(Sold_Card$Profit <= 0, 1 ,ifelse(Sold_Card$Profit <= 5, 5, ifelse(Sold_Card$Profit <= 10, 10, ifelse(Sold_Card$Profit <= 20,20, ifelse(Sold_Card$Profit <= 30, 30, ifelse(Sold_Card$Profit <= 40,40,ifelse(Sold_Card$Profit <= 50,50, ifelse(Sold_Card$Profit <= 75, 75, ifelse(Sold_Card$Profit >= 100, 100, 101)))))))))

#Boxplot & Hist for Day Sold####
summary(Sold_Card)
Sold_Card$Numeric.Day.Sold = as.numeric(Sold_Card$Day.Sold)
h1 =hist(Sold_Card$Numeric.Day.Sold)
Day_Table = table(Sold_Card$Day.Sold, Sold_Card$Numeric.Day.Sold)
Day_Table
summary(Sold_Card)
ncol(Sold_Card)
Sold_Card$Day.Sold = as.numeric(Sold_Card$Day.Sold)
bp1 = boxplot(Sold_Card$Day.Sold, main="Most likely Day of Week Sales")
summary(Sold_Card$Day.Sold)
Days.Sold.Breakdown = count(Sold_Card, vars = "Day.Sold")
# Mon = 1, Tuesday = 2, etc
Days.Sold.Breakdown
pie(Days.Sold.Breakdown$freq, labels = Days.Sold.Breakdown$Profit, main = "Day Sold",col = rainbow(length(Days.Sold.Breakdown$freq)))
legend("topright", c("70","161","51","46","44","37","46"), cex = .65,
       fill = rainbow(length(Days.Sold.Breakdown$freq)))
summary(Sold_Card)
Profit.Breakdown = count(Sold_Card, vars = "Profit")
Profit.Breakdown
pie(Profit.Breakdown$freq, labels = Profit.Breakdown$Profit, main = "Profit Pie",col = rainbow(length(Profit.Breakdown$freq)))
legend("topright", c("9","123","87","94","21","4","4","2","8","3"), cex = .55,
       fill = rainbow(length(Profit.Breakdown$freq)))

# Busiest time of the week for sales are Mon-Tues-Wed, Least active are Fri-Sat-Sun
#
#Boxplot & Hist for Week of Month####
bp2 = boxplot(Sold_Card$Week.of.Month)
h2 = hist(Sold_Card$Week.of.Month)
Sold_Card$F.Week.of.Month = as.factor(Sold_Card$Week.of.Month)
summary(Sold_Card$F.Week.of.Month)
Sold_Card = Sold_Card[,-14]

Week.Sold.Breakdown = count(Sold_Card, vars = "Week.of.Month")
#Account for the fact that there are 4 months with a 5th week.
Week.Sold.Breakdown$freq[5] = Week.Sold.Breakdown$freq[5]*4
Week.Sold.Breakdown
pie(Week.Sold.Breakdown$freq, labels = Week.Sold.Breakdown$Week.of.Month, main = "Week of Month Breakdown Pie",col = rainbow(length(Week.Sold.Breakdown$freq)))
legend("topright", c("94","88","63","92","72"), cex = .65,
       fill = rainbow(length(Profit.Breakdown$freq)))
#28% drop in the third week of the month, with a return to consistency in week 4 to match weeks 1 & 2. Will have to equivalize week 5 somehow (Check).

#
#Boxplot & Hist for Colour####
Sold_Card$Color_Number = ifelse(Sold_Card$Color %in% c('Black'),1,ifelse(Sold_Card$Color %in% c('Green'),2,ifelse(Sold_Card$Color %in% c('Red'),3,ifelse(Sold_Card$Color %in% c("Blue"),4,ifelse(Sold_Card$Color %in% c("White"),5,ifelse(Sold_Card$Color %in% c("Brown"),6,ifelse(Sold_Card$Color %in% c("Multi"),7,ifelse(Sold_Card$Color %in% c("Land"),8,9))))))))
#1 = Black | 2 = Green | 3 = Red | Blue = 4 | White = 5 | Brown = 6 |Multi = 7 | Land = 8 | 9 = What the shitsicles
h3 = hist(Sold_Card$Color_Number)
bp3 = boxplot(Sold_Card$Color_Number)
Sold_Card$Color_Number = as.factor(Sold_Card$Color_Number)
summary(Sold_Card$Color_Number)
`Card Analysis`$Color_Number = ifelse(`Card Analysis`$Color %in% c('Black'),1,ifelse(`Card Analysis`$Color %in% c('Green'),2,ifelse(`Card Analysis`$Color %in% c('Red'),3,ifelse(`Card Analysis`$Color %in% c("Blue"),4,ifelse(`Card Analysis`$Color %in% c("White"),5,ifelse(`Card Analysis`$Color %in% c("Brown"),6,ifelse(`Card Analysis`$Color %in% c("Multi"),7,ifelse(`Card Analysis`$Color %in% c("Land"),8,9))))))))
`Card Analysis`$Color_Number = as.factor(`Card Analysis`$Color_Number)
summary(`Card Analysis`$Color_Number)

Color_Table = as.data.frame(`Card Analysis`$Color_Number)
Color.Breakdown = count(`Card Analysis`, vars = "Color_Number")
Color.Breakdown = as.data.frame(Color.Breakdown)
Color.Breakdown.1 = count(Sold_Card, vars = "Color_Number")
Color.Breakdown.1 = as.data.frame(Color.Breakdown.1)

Color.Breakdown$Sold_Freq = Color.Breakdown.1$freq
Color.Breakdown$Total_Count = (Color.Breakdown$Sold_Freq + Color.Breakdown$freq)
Color.Breakdown$Ratio = round(Color.Breakdown$Sold_Freq/Color.Breakdown$Total_Count,2)*100
Color.Breakdown

#Viewing Individual Sales####
Resting_Inven_by_Color = c("Black","Green","Red","Blue","White","Brown","Multi","Land","Sealed")
Resting_Inven_by_Color = as.data.frame(Resting_Inven_by_Color)
Resting_Inven_by_Color$Current_Inventory = (Color.Breakdown$Total_Count - Color.Breakdown$Sold_Freq)
Resting_Inven_by_Color$Resting_Percent = round((Resting_Inven_by_Color$Current_Inventory/Color.Breakdown$Total_Count),2)*100
Resting_Inven_by_Color$Sold_Inventory = Color.Breakdown$Sold_Freq
Resting_Inven_by_Color$Sold_Percent = ((Resting_Inven_by_Color$Resting_Percent-100)*-1)
Resting_Inven_by_Color$Overall_Sold_Percent = round((Resting_Inven_by_Color$Sold_Inventory/sum(Resting_Inven_by_Color$Sold_Inventory)),2)*100
Resting_Inven_by_Color
#Breakdown of Sales by Colour Table####
sum(Resting_Inven_by_Color$Overall_Sold_Percent)
#1 Black, 2 Green, 3 Red,4 Blue, 5 White, 6 Brown, 7 Multi, 8 Land, 9 Sealed

#Viewing Unique Cards ####
summary(Sold_Card)
Card_Colour
Card_Colour$Color_Number = ifelse(Card_Colour$Color %in% c('Black'),1,ifelse(Card_Colour$Color %in% c('Green'),2,ifelse(Card_Colour$Color %in% c('Red'),3,ifelse(Card_Colour$Color %in% c("Blue"),4,ifelse(Card_Colour$Color %in% c("White"),5,ifelse(Card_Colour$Color %in% c("Brown"),6,ifelse(Card_Colour$Color %in% c("Multi"),7,ifelse(Card_Colour$Color %in% c("Land"),8,9))))))))
summary(Card_Colour)
Card_Colour_1 = sapply(Card_Colour, as.factor)
Card_Colour_1 = as.data.frame(Card_Colour_1)
summary(Card_Colour_1)
Card_Colour_1 = sapply(Card_Colour_1, as.numeric)
Card_Colour_1 = as.data.frame(Card_Colour_1)
summary(Card_Colour_1)
Card_Colour_1 = Card_Colour_1[order(Card_Colour_1$Sold),]
Card_Colour_1$row = 1:nrow(Card_Colour_1)
nrow(Card_Colour_1)
Card_Colour_2 = Card_Colour_1[c(177:315),]
cc_agg1 = count(Card_Colour_2, vars = "Color") #Sold Unique Cards
cc_agg = count(Card_Colour_1, vars = "Color")#Total Unique Cards
cc_agg
cc_agg1

Unique_Inven_by_Color = c("Black","Green","Red","Blue","White","Brown","Multi","Land","Sealed")
Unique_Inven_by_Color = as.data.frame(Unique_Inven_by_Color)
Unique_Inven_by_Color$Current_Inventory = (cc_agg$freq - cc_agg1$freq)
Unique_Inven_by_Color$Unique_Percent = round(((cc_agg$freq - cc_agg1$freq)/cc_agg$freq),2)*100
Unique_Inven_by_Color$Sold_Inventory = cc_agg1$freq
Unique_Inven_by_Color$Sold_Percent = ((Unique_Inven_by_Color$Unique_Percent-100)*-1)
Unique_Inven_by_Color$Overall_Sold_Percent = round((Unique_Inven_by_Color$Sold_Inventory/sum(Unique_Inven_by_Color$Sold_Inventory)),2)*100
Unique_Inven_by_Color
#
# 

#Next Step#### Aggregated by profit and days held
summary(Sold_Card)
Sold_Card$Profit = as.numeric(Sold_Card$Profit)
Sold_Card = as.data.frame(Sold_Card)
Sold_Card$`Days Held`
Sold_Card$`Days Held` = ifelse(Sold_Card$`Days Held` <= 15, 15 ,ifelse(Sold_Card$`Days Held` <= 30, 30, ifelse(Sold_Card$`Days Held` <= 60, 60, ifelse(Sold_Card$`Days Held` <= 90,90,ifelse(Sold_Card$`Days Held` <= 120, 120, ifelse(Sold_Card$`Days Held` <= 180,180,ifelse(Sold_Card$`Days Held` <= 240,240, ifelse(Sold_Card$`Days Held` <= 365, 365, ifelse(Sold_Card$`Days Held` >= 366, 366, 0)))))))))
Profit_Agg = round(aggregate(Sold_Card[,-c(1,2,3,4,5,6,7,8,10,11,12,13)],by=list(Sold_Card$Profit),FUN=sum),0)

Prof_per_dow = count(Sold_Card, vars = "Day.Sold") #Sold Unique Cards
Prof_per_dow
Sales_Per_DOW = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
Sales_Per_DOW = as.data.frame(Sales_Per_DOW)
Sales_Per_DOW$Number.Per.Day.Sold = Prof_per_dow$freq
Sales_Per_DOW$Day.Sold.Percent = round((Sales_Per_DOW$Number.Per.Day.Sold/sum(Sales_Per_DOW$Number.Per.Day.Sold)),2)*100
Sales_Per_DOW

#Profit by Day####
summary(Sold_Card)
Sold_Card = sapply(Sold_Card, as.numeric)
Sold_Card = as.data.frame((Sold_Card))
Day_of_Week_Agg = round(aggregate(Sold_Card[,-c(1,2,3,4,5,6,7,8,9,10,11,12,14)],by=list(Sold_Card$`Days Held`),FUN=sum),0)
Day_of_Week_Agg
names(Day_of_Week_Agg)[1]<-"Days.Held"
names(Day_of_Week_Agg)[2]<-"Profit"
Day_of_Week_Agg
dowa_profit = as.data.frame(Day_of_Week_Agg$Profit)
dowa_profit
sum(dowa_profit[1,])/sum(Day_of_Week_Agg$Profit)#21% of my profit occurs from cards held between/to 1-15 days
sum(dowa_profit[2,])/sum(Day_of_Week_Agg$Profit)#34% of my profit occurs from cards held between 15-30 days
sum(dowa_profit[3,])/sum(Day_of_Week_Agg$Profit)#22% of my profit occurs from cards held between 30-60 days
sum(dowa_profit[c(1,2,3),])/sum(Day_of_Week_Agg$Profit)#76% of my profitable specs move within or equal to 60 days.
sum(dowa_profit[c(4,5,6,7,8,9),])#1370
sum(dowa_profit[c(4,5,6,7,8,9),])/sum(Day_of_Week_Agg$Profit) #24% of profitable sale after 90 days
# 52% of my resting inventory is over 90 days, as of May 30, 2019. Thus, half my inventory has less than a 25% chance of being moved profitably moving forward####
#Profit by Mon-Tues-Wednes####
summary(Sold_Card)
Sold_Card$Day.Sold = ifelse(Sold_Card$Day.Sold %in% c('Monday'),1,ifelse(Sold_Card$Day.Sold %in% c('Tuesday'),2,ifelse(Sold_Card$Day.Sold %in% c('Wednesday'),3,ifelse(Sold_Card$Day.Sold %in% c("Thursday"),4,ifelse(Sold_Card$Day.Sold %in% c("Friday"),5,ifelse(Sold_Card$Day.Sold %in% c("Saturday"),6,ifelse(Sold_Card$Day.Sold %in% c("Sunday"),7,8)))))))
Sold_Card$Day.Sold
Day.Sold.Agg = round(aggregate(Sold_Card[,-c(1,2,3,4,5,6,7,8,9,10,11,12)],by=list(Sold_Card$Day.Sold),FUN=sum),0)
Day.Sold.Agg
names(Day.Sold.Agg)[1]<-"Day.Sold"
names(Day.Sold.Agg)[2]<-"Profit"
Day.Sold.Agg
plot(Day.Sold.Agg$Day.Sold,Day.Sold.Agg$Profit)
sum(Day.Sold.Agg$Profit)

#Rpart####
mtg$Qty = as.numeric(mtg$Qty)
mtg$Qty =  ifelse(mtg$Qty <= 1, 1, ifelse(mtg$Qty <= 2, 2, ifelse(mtg$Qty <=5, 4, ifelse(mtg$Qty <=9, 8, ifelse(mtg$Qty <=13, 12, 15)))))
mtg$`Days Held` = as.numeric(mtg$`Days Held`)
mtg$`Days Held` = ifelse(mtg$`Days Held` <= 15, 15 ,ifelse(mtg$`Days Held` <= 30, 30, ifelse(mtg$`Days Held` <= 60, 60, ifelse(mtg$`Days Held` <= 90,90,ifelse(mtg$`Days Held` <= 120, 120, ifelse(mtg$`Days Held` <= 180,180,ifelse(mtg$`Days Held` <= 240,240, ifelse(mtg$`Days Held` <= 365, 365, ifelse(mtg$`Days Held` >= 366, 366, 0)))))))))
mtg$Profit = as.numeric(mtg$Profit)
mtg$Profit = ifelse(mtg$Profit <= 0, 1 ,ifelse(mtg$Profit <= 5, 5, ifelse(mtg$Profit <= 10, 10, ifelse(mtg$Profit <= 20,20, ifelse(mtg$Profit <= 30, 30, ifelse(mtg$Profit <= 40,40,ifelse(mtg$Profit <= 50,50, ifelse(mtg$Profit <= 75, 75, ifelse(mtg$Profit >= 100, 100, 101)))))))))


summary(mtg)
SaleModel = 'Sold ~ Foil + Color + Format + Rare + `Days Held` + Profit + Day.Sold + Week.of.Month'

Salefit = rpart(SaleModel, data = mtg, method = "class",control= rpart.control(cp = .025))
print(Salefit)
summary(Salefit)
rpart.plot(Salefit, main ="Sold (Y/N) - Classification Tree (CP = .025)",
           box.palette="Blues")

mtg$rand = runif(nrow(mtg)) #assign rand num to each obs
mtg = mtg[order(mtg$rand),] #order asc by random num
nrow(mtg)
495 * .8 #396
495 * .2 #113
mtgtrain = mtg[1:396,]
mtgtest= mtg[397:495,]
#CART Accuracy
mtgtest$predict = predict(Salefit, newdata=mtgtest, type='class')

#Counts
tab = table(mtgtest$Sold, mtgtest$predict)
dimnames(tab) <- list(Actual = c("No","Yes"), "Predicted" = c("No","Yes"))
tab
#Predict Table as Pct
pcttab <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
dimnames(pcttab) <- list(Actual = c("No","Yes"), "Predicted" = c("No","Yes"))
cartaccuracy = print(round(pcttab, 2))
cartaccuracy = as.data.frame(cartaccuracy)

#Conclusion: VERY good at predicting a card will sell, but I believe that is because of our skewed data (4:1 for sales to resting inventory).
#Curious to see further.

#C5 DT####
mtg1 = sapply(mtg, as.factor)
mtg1 = as.data.frame(mtg1)
mtgtrain1 = mtg[1:396,]
mtgtest1= mtg[397:495,]

mtgtrain1 = sapply(mtgtrain1,as.factor)
mtgtest1 = sapply(mtgtest1, as.factor)
mtgtrain1 = as.data.frame(mtgtrain1)
mtgtest1 = as.data.frame(mtgtest1)

mtgc5overall = C5.0(Profit ~ Foil + Color + Format + Rare + `Days Held` + Day.Sold + Week.of.Month, data = mtg1)
summary(mtgc5overall)
plot(mtgc5overall)

MtgC5Model <- C5.0(Profit ~ Foil + Color + Format + Rare + `Days Held` + Day.Sold + Week.of.Month, data = mtgtrain1)
summary(MtgC5Model)
plot(MtgC5Model)


#Ruh-row
mtgtest1$predict = predict(MtgC5Model, newdata=mtgtest1, type='class')

#Counts
tab = table(mtgtest1$Sold, mtgtest1$predict)
dimnames(tab) <- list(Actual = c("No","Yes"), "Predicted" = c("No","Yes"))
tab
#Predict Table as Pct
pcttab <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
dimnames(pcttab) <- list(Actual = c("No","Yes"), "Predicted" = c("No","Yes"))
C5accuracy = print(round(pcttab, 2))
C5accuracy = as.data.frame(C5accuracy)

#CI Tree####
mtgtrain2 = sapply(mtgtrain,as.factor)
mtgtest2 = sapply(mtgtest, as.factor)
mtgtrain2 = as.data.frame(mtgtrain2)
mtgtest2 = as.data.frame(mtgtest2)
summary(mtgtest2)
mtgCI = ctree(Sold ~ Foil + Color + Format + Rare + `Days Held` + Profit + Day.Sold + Week.of.Month, data = mtgtrain2)
mtgCI
mtgtest2$predict = predict(mtgCI, newdata=(mtgtest2), type='response')
#Counts
tab = table(mtgtest2$Sold, mtgtest2$predict)
dimnames(tab) <- list(Actual = c("No","Yes"), "Predicted" = c("No", "Yes"))
tab
#Predict Table as Pct
pcttab <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]))
dimnames(pcttab) <- list(Actual = c("No","Yes"), "Predicted" = c("No", "Yes"))
CIAccuracy = print(round(pcttab, 2))
CIAccuracy = as.data.frame(CIAccuracy)
#DT - Resulting Accuracy############

cartaccuracy
C5accuracy 
CIAccuracy #CI may fail depending on randomization

summary(mtgc5overall)
summary(MtgC5Model)

#Next Investigation Steps####
summary(mtg)
ncol(mtg)
mtg = mtg[,-15]
mtg1 = mtg1[,-15]
#Clustering####
mtg$rand = runif(nrow(mtg)) #assign rand num to each obs
mtg = mtg[order(mtg$rand),]

mtg2 = sapply(mtg1, as.numeric)
mtg2 = as.data.frame(mtg2)
mtg2 = na.omit(mtg2)
summary(mtg2)

# mtg_agg = aggregate(mtg2[,-c(1)],by=list(mtg2$Sold),FUN=mean)
# summary(mtg_agg)
# mtg = as.data.frame(mtg[,-17])
# summary(mtg)
summary(mtg2)
mtg_Distance = dist(as.matrix(mtg2))

mtg_d1 = hclust(mtg_Distance)
summary(mtg_d1)
print(mtg_d1)
plot (mtg_d1, main="Complete Linkage Dendrogram")

mtg_d2 <- hclust(mtg_Distance, method="single")  # default is complete
plot (mtg_d2, main="Complete Linkage Dendrogram")

# Dependant upon the data and dependant upon the domain

mtg_d3<- hclust(mtg_Distance, method="ward.D2") 
plot(mtg_d2, main="Ward Method Dendrogram") # display dendogram

mtg_Kmeans <- kmeans(mtg_Distance, 5) # 5 cluster solution
mtg2$cluster = mtg_Kmeans$cluster
plotcluster(mtg2, mtg2$cluster, main="MTG Plot Clusters")                    

summary(mtg2)
Cluster1 = subset(mtg2, cluster==1)
Cluster1 = Cluster1[,-c(1,2)]
Cluster2 = subset(mtg2, cluster==2)
Cluster2 = Cluster2[,-c(1,2)]
Cluster3 = subset(mtg2, cluster==3)
Cluster3 = Cluster3[,-c(1,2)]
Cluster4 = subset(mtg2, cluster==4)
Cluster4 = Cluster4[,-c(1,2)]
Cluster5 = subset(mtg2, cluster==5)
Cluster5 = Cluster5[,-c(1,2)]
#Manova Test on Cluster Results####
mtg2 = sapply(mtg2, as.factor)
mtg2 = as.data.frame(mtg2)
mtgman = manova(cbind(mtg2$Qty, mtg2$Foil, mtg2$Set, mtg2$Buy_Plat, mtg2$Color, mtg2$Format, mtg2$Rare, mtg2$`Days Held`, mtg2$Day.Sold, mtg2$Week.of.Month, mtg2$Profit) ~ mtg2$cluster)
summary(mtgman)
summary.aov(mtgman)
mtgman2 = manova(cbind(mtg2$Foil,mtg2$Color,mtg2$Format, mtg2$Rare, mtg2$`Days Held`, mtg2$Day.Sold, mtg2$Week.of.Month) ~ mtg2$cluster)
summary(mtgman2)
summary.aov(mtgman2)
#mtgman3 = manova(cbind(mtg2$Foil,mtg2$Rare, mtg2$Format, mtg2$Set, mtg2$Week.of.Month) ~ mtg2$cluster)
#summary(mtgman3)
#summary.aov(mtgman3)
#K Means Clustering Results ####
#Foil, Rarity, Set, & Format are my statistically Significant Variables given 5 clusters using K-Means ####
#This really makes me want to subset the data and look at each cluster to see their breakdowns on these variables and take an extra peak at profitability for each####


Cluster1 = sapply(Cluster1, as.factor)
Cluster2 = sapply(Cluster2, as.factor)
Cluster3 = sapply(Cluster3, as.factor)
Cluster4 = sapply(Cluster4, as.factor)
Cluster5 = sapply(Cluster5, as.factor)
Cluster1 = as.data.frame(Cluster1)
Cluster2 = as.data.frame(Cluster2)
Cluster3 = as.data.frame(Cluster3)
Cluster4 = as.data.frame(Cluster4)
Cluster5 = as.data.frame(Cluster5)
summary(Cluster1)
summary(Cluster1[,c(2,5,6,7,8,10,11)])
summary(Cluster2[,c(2,5,6,7,8,10,11)])
summary(Cluster3[,c(2,5,6,7,8,10,11)])
summary(Cluster4[,c(2,5,6,7,8,10,11)])
summary(Cluster5[,c(2,5,6,7,8,10,11)])

#Factor Analysis####
mtg3 = na.omit(mtg[,-c(1,15)])
mtg3 = sapply(mtg3, as.factor)
mtg3 = as.data.frame(mtg3)
mtg3 = sapply(mtg3, as.numeric)
mtg3 = as.data.frame(mtg3[,-c(1,2,13)])
summary(mtg3)
mtg3 = na.omit(mtg3)
parallel <- fa.parallel(mtg3, fm = 'minres', fa = 'fa') #suggest between 4 factors

f4A <- fa(mtg3,nfactors = 4,rotate = "none",fm="minres")
print(f4A)
print(f4A$loadings,cutoff = 0.30)
f4B <- fa(mtg3,nfactors = 4,rotate = "oblimin",fm="minres")
print(f4B)
print(f4B$loadings,cutoff = 0.40)
f4C <- fa(mtg3,nfactors = 4,rotate = "varimax",fm="minres")
print(f4C)
print(f4C$loadings,cutoff = 0.40)

#Factor Analysis Results####Oblimin
#Factor 1 - Format, Buy_Plat, & Rarity --> "Set"
#Factor 2 - Sold & Profit -- > "Buy_Plat & Format"
#Factor 3 - Foil --> "Week-Of-Month-Sold"
#Factor 4 - Days.Held --> "Profitable Sale"


#So now do I subset the data with these loadings and move forward from there? Unsure
#RandomForest for Kicks and Gigs####
set.seed(45)
mtg$Day.Sold = as.factor(mtg$Day.Sold)
mtg$Day.Sold = ifelse(mtg$Day.Sold %in% c('Monday'),1,ifelse(mtg$Day.Sold %in% c('Tuesday'),2,ifelse(mtg$Day.Sold %in% c('Wednesday'),3,ifelse(mtg$Day.Sold %in% c("Thursday"),4,ifelse(mtg$Day.Sold %in% c("Friday"),5,ifelse(mtg$Day.Sold %in% c("Saturday"),6,ifelse(mtg$Day.Sold %in% c("Sunday"),7,0)))))))
mtg$Day.Sold = as.numeric(mtg$Day.Sold)
mtg = as.data.frame(mtg)
mtg$Profit = ifelse(mtg$Profit <= 0, 1 ,ifelse(mtg$Profit <= 5, 5, ifelse(mtg$Profit <= 10, 10, ifelse(mtg$Profit <= 20,20, ifelse(mtg$Profit <= 30, 30, ifelse(mtg$Profit <= 40,40,ifelse(mtg$Profit <= 50,50, ifelse(mtg$Profit <= 75, 75, ifelse(mtg$Profit >= 100, 100, 101)))))))))
mtg = as.data.frame(mtg)
summary(mtg)
mtg$`Days Held` = as.numeric(mtg$`Days Held`)
mtg$`Days Held` = ifelse(mtg$`Days Held` <= 15, 15 ,ifelse(mtg$`Days Held` <= 30, 30, ifelse(mtg$`Days Held` <= 60, 60, ifelse(mtg$`Days Held` <= 90,90,ifelse(mtg$`Days Held` <= 120, 120, ifelse(mtg$`Days Held` <= 180,180,ifelse(mtg$`Days Held` <= 240,240, ifelse(mtg$`Days Held` <= 365, 365, ifelse(mtg$`Days Held` >= 366, 366, 0)))))))))

mtg = sapply(mtg, as.factor)
mtg = as.data.frame(mtg)
data.imputed = rfImpute(UpDown~ Qty +Foil + Buy_Plat + Color + Format + Rare + `Days Held` + Day.Sold + Week.of.Month +Sold, data = mtg, iter = 10)
summary(data.imputed)
data.imputed$Days.Held = data.imputed$`Days Held`

oob.values = vector(length=8)
for(i in 1:8){
  temp.model=randomForest(UpDown~Qty +Foil+Buy_Plat+Color+Format+Rare+Days.Held+Day.Sold+Week.of.Month+Sold, data=data.imputed,mtry=i,ntree=250)
  oob.values[i]=temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values = round(as.data.frame(oob.values),4)
oob.values

rfmodel = randomForest(UpDown~ Qty +Foil+Buy_Plat+Color+Format+Rare+Days.Held+Day.Sold+Week.of.Month+Sold, data = data.imputed, mtry=2,ntree=250, Proximity = TRUE)
rfmodel
oob.error.rate = data.frame(
  Trees = rep(1:nrow(rfmodel$err.rate),times=3),
  Type=rep(c("OOB","No","Yes"), each=nrow(rfmodel$err.rate)),
  Error= c(rfmodel$err.rate[,"OOB"],
           rfmodel$err.rate[,"No"],
           rfmodel$err.rate[,"Yes"]))
ggplot(data=oob.error.rate, aes(x=Trees, y= Error))+geom_line(aes(color=Type))
