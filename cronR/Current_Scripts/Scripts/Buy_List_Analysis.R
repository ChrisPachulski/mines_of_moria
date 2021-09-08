library(readr)
BH<- read_csv("Old Trackers/Metrics/2019-12-07/BuyList_History2019-12-08.csv", 
                                             col_types = cols(Foil = col_character()))
View(BH)

BH_NF <- BH[which(is.na(BH$Foil) == TRUE ),]
BH_NF_M <- BH_NF[which(BH_NF$Rarity == "M"),]
BH_NF_R <- BH_NF[which(BH_NF$Rarity == "R"),]
BH_NF_U <- BH_NF[which(BH_NF$Rarity == "U"),]
BH_NF_C <- BH_NF[which(BH_NF$Rarity == "C"),]

nrow(BH_NF_M)
nrow(BH_NF_R)
nrow(BH_NF_U)
nrow(BH_NF_C)

BH_NF_M <- BH_NF_M[c(-1,-2,-3,-4,-5)]
library(zoo)
na.aggregate(BH_NF_M)
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
BH_NF_M <- replace(BH_NF_M, TRUE, lapply(BH_NF_M, NA2mean))
summary(BH_NF_M)
