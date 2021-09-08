install.packages(c("rvest","tidyverse","RSelenium"))
library(tidyverse)
library(rvest)
library(RSelenium)


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

#Japanese Palantir####
remDr = remoteDriver(remoteServerAddr = "64.225.20.203", port = 4445L, browser = "chrome")
remDr$open()
Fireball_Entire <- NULL
# remDr$navigate("https://store.channelfireball.com/buylist#%7B%22q%22%3A%22Search%20the%20buylist%20for...%22%7D")
# Sys.sleep(5)
# remDr$findElement('xpath','//*[@id="search-primary"]/select/option[2]')$clickElement()
# Sys.sleep(1)
# remDr$findElement('xpath','//*[@id="search-secondary"]/select/option[2]')$clickElement()
# Sys.sleep(1)
# remDr$findElement('xpath','//*[@id="search-tertiary"]/select/option[3]')$clickElement()

Sys.sleep(1)
for(i in 1){
  Fireball <- remDr$getPageSource()[[1]]%>% read_html()
  Sys.sleep(1)
  Fireball_Cards <-  as.data.frame(Fireball %>% html_nodes("h4") %>% html_text())
  Fireball_Cards <- as.data.frame(Fireball_Cards[-1,])
  Fireball_Cards <- as.data.frame(Fireball_Cards[-nrow(Fireball_Cards),])
  Fireball_Cards <- as.data.frame(Fireball_Cards[-nrow(Fireball_Cards),])
  Fireball_Quantity <- Fireball %>% html_nodes(".variant-qty") %>% html_text()
  Fireball_Quantity <- Fireball_Quantity[seq(1,length(Fireball_Quantity),3)]
  Fireball_Quantity <- gsub("Limit ","",Fireball_Quantity)
  Fireball_Offer <- Fireball %>% html_nodes(".regular.price") %>% html_text()
  Fireball_Offer <- Fireball_Offer[seq(1,length(Fireball_Offer),5)]
  Limit <- length(Fireball_Cards$`Fireball_Cards[-nrow(Fireball_Cards), ]`)
  
  Fireball_Set <- Fireball %>% html_nodes(".category") %>% html_text()
  Fireball_Set <- as.data.frame(Fireball_Set)
  Fireball_Set <- as.data.frame(Fireball_Set[c(1:Limit),1])
  Fireball_Double_Check <- cbind(Fireball_Cards,Fireball_Quantity)
  Fireball_Double_Check <- Fireball_Double_Check[!grepl("Not on buylist",Fireball_Double_Check)]
  colnames(Fireball_Double_Check) <- c("Card","Quantity")
  
  Fireball_Standard <- cbind(as.character(Fireball_Double_Check$Card),Fireball_Set,Fireball_Offer,Fireball_Double_Check$Quantity)
  colnames(Fireball_Standard) <- c("Card","Edition","Offer","Qty")
  Fireball_Entire <- rbind(Fireball_Entire,Fireball_Standard)
  tryCatch(expr = {remDr$findElement('class','next_page')$clickElement()}, error = function(e){Skip = "Yes"})
  Fireball <- remDr$getPageSource()[[1]]%>% read_html()
  Sys.sleep(1)
  Fireball_Cards <-  as.data.frame(Fireball %>% html_nodes("h4") %>% html_text())
  Fireball_Cards <- as.data.frame(Fireball_Cards[-1,])
  Fireball_Cards <- as.data.frame(Fireball_Cards[-nrow(Fireball_Cards),])
  Fireball_Cards <- as.data.frame(Fireball_Cards[-nrow(Fireball_Cards),])
  Fireball_Quantity <- Fireball %>% html_nodes(".variant-qty") %>% html_text()
  Fireball_Quantity <- Fireball_Quantity[seq(1,length(Fireball_Quantity),3)]
  Fireball_Quantity <- gsub("Limit ","",Fireball_Quantity)
  Fireball_Offer <- Fireball %>% html_nodes(".regular.price") %>% html_text()
  Fireball_Offer <- Fireball_Offer[seq(1,length(Fireball_Offer),5)]
  Limit <- length(Fireball_Cards$`Fireball_Cards[-nrow(Fireball_Cards), ]`)
  
  Fireball_Set <- Fireball %>% html_nodes(".category") %>% html_text()
  Fireball_Set <- as.data.frame(Fireball_Set)
  Fireball_Set <- as.data.frame(Fireball_Set[c(1:Limit),1])
  Fireball_Double_Check <- cbind(Fireball_Cards,Fireball_Quantity)
  Fireball_Double_Check <- Fireball_Double_Check[!grepl("Not on buylist",Fireball_Double_Check)]
  colnames(Fireball_Double_Check) <- c("Card","Quantity")
  
  Fireball_Standard <- cbind(as.character(Fireball_Double_Check$Card),Fireball_Set,Fireball_Offer,Fireball_Double_Check$Quantity)
  colnames(Fireball_Standard) <- c("Card","Edition","Offer","Qty")
  Fireball_Entire <- rbind(Fireball_Entire,Fireball_Standard)
  Fireball_Entire <- unique(Fireball_Entire)
}


Sys.sleep(1)
remDr$findElement('xpath','//*[@id="search-secondary"]/select/option[3]')$clickElement()
Sys.sleep(1)
for(i in 2:4){
  unique_xpath <- paste('//*[@id="search-tertiary"]/select/option[',i,']')
  remDr$findElement('xpath',unique_xpath)$clickElement()
  Sys.sleep(1)
  Fireball <- remDr$getPageSource()[[1]]%>% read_html()
  Sys.sleep(1)
  Fireball_Cards <-  as.data.frame(Fireball %>% html_nodes("h4") %>% html_text())
  Fireball_Cards <- as.data.frame(Fireball_Cards[-c(1:2),])
  Fireball_Cards <- as.data.frame(Fireball_Cards[-nrow(Fireball_Cards),])
  Fireball_Quantity <- Fireball %>% html_nodes(".variant-qty") %>% html_text()
  Fireball_Quantity <- Fireball_Quantity[seq(1,length(Fireball_Quantity),3)]
  Fireball_Quantity <- gsub("Limit ","",Fireball_Quantity)
  Fireball_Offer <- Fireball %>% html_nodes(".regular.price") %>% html_text()
  Fireball_Offer <- Fireball_Offer[seq(1,length(Fireball_Offer),5)]
  
  Fireball_Set <- remDr$findElement('xpath',unique_xpath)$getElementText()
  Fireball_Double_Check <- cbind(Fireball_Cards,Fireball_Quantity)
  Fireball_Double_Check <- Fireball_Double_Check[!grepl("Not on buylist",Fireball_Double_Check)]
  colnames(Fireball_Double_Check) <- c("Card","Quantity")
  
  Fireball_Standard <- cbind(as.character(Fireball_Double_Check$Card),Fireball_Set,Fireball_Offer,Fireball_Double_Check$Quantity)
  colnames(Fireball_Standard) <- c("Card","Edition","Offer","Qty")
  Fireball_Entire <- rbind(Fireball_Entire,Fireball_Standard)
  tryCatch(expr = {remDr$findElement('class','next_page')$clickElement()}, error = function(e){Skip = "Yes"})
  Sys.sleep(1)
  Fireball <- remDr$getPageSource()[[1]]%>% read_html()
  Sys.sleep(1)
  Fireball_Cards <-  as.data.frame(Fireball %>% html_nodes("h4") %>% html_text())
  Fireball_Cards <- as.data.frame(Fireball_Cards[-c(1:2),])
  Fireball_Cards <- as.data.frame(Fireball_Cards[-nrow(Fireball_Cards),])
  Fireball_Quantity <- Fireball %>% html_nodes(".variant-qty") %>% html_text()
  Fireball_Quantity <- Fireball_Quantity[seq(1,length(Fireball_Quantity),3)]
  Fireball_Quantity <- gsub("Limit ","",Fireball_Quantity)
  Fireball_Offer <- Fireball %>% html_nodes(".regular.price") %>% html_text()
  Fireball_Offer <- Fireball_Offer[seq(1,length(Fireball_Offer),5)]
  
  Fireball_Set <- remDr$findElement('xpath',unique_xpath)$getElementText()
  Fireball_Double_Check <- cbind(Fireball_Cards,Fireball_Quantity)
  Fireball_Double_Check <- Fireball_Double_Check[!grepl("Not on buylist",Fireball_Double_Check)]
  colnames(Fireball_Double_Check) <- c("Card","Quantity")
  
  Fireball_Standard <- cbind(as.character(Fireball_Double_Check$Card),Fireball_Set,Fireball_Offer,Fireball_Double_Check$Quantity)
  colnames(Fireball_Standard) <- c("Card","Edition","Offer","Qty")
  Fireball_Entire <- rbind(Fireball_Entire,Fireball_Standard)
}
