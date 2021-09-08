CK_Store_Front <-NULL #Assign NULL value
total = 683 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
tic()
for(i in 1:683){
  
  
  url <- paste0("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  html <- read_html("scrapedpage.html")
  json <- html %>% html_nodes(".itemContentWrapper") %>% html_text()
  #html_nodes(".itemContentWrapper")#
  #Sys.sleep(sample(3:17,1))
  json <-data.frame(json)
  CK_Store_Front <-rbind(CK_Store_Front,json)
  setTxtProgressBar(pb,i)
  
}
toc()#CK Market Formatting####
CK <-data.frame(do.call('rbind', strsplit(as.character(CK_Store_Front$json),'\n',fixed=TRUE))) #Delimiting
CK2<-data.frame(do.call('rbind', strsplit(as.character(CK$X5),' (',fixed=TRUE))) #Delimiting card name and sets
CK3<-data.frame(do.call('rbind', strsplit(as.character(CK2$X2),')',fixed=TRUE))) #Delimiting card name and sets to make match with prior data frames
CK$X4<-CK2$X1 #Replace superfluous columns with new delimited formats
CK$X5<- CK3$X1 #Replace superfluous columns with new delimited formats
CK$X3 = as.character(CK$X3)
CK4 <- data.frame(do.call('rbind', strsplit(as.character(CK$X3),'\r',fixed=TRUE)))
CK$X3 <- CK4$do.call..rbind...strsplit.as.character.CK.X3.....r...fixed...TRUE..
#CK$X3 = substr(CK$X3,1,nchar(CK$X3)-1) 
CK$X2 <- paste(CK$X3,CK$X4,CK$X5, sep="") #Create primary key
CK_MKT <- CK[c(1,2,3,4,5,11)] #Subset the data, there is a lot* of superfluous data in this data frame
CK_MKT$X1 <- seq.int(nrow(CK_MKT)) #Assign rankings to their Highest demand cards
(CK_MKT$X3)
test2 <- test #Create seperate data frame (df) to the og to ensure I recreate my primary key correctly again
test2$CK_Key = substr(test$CK_Key,1,nchar(test$CK_Key)-1) #account for that pesky space character again
test$CK_Rank <- CK_MKT$X1[match(test2$CK_Key,CK_MKT$X2)] #Merge the CK best selling rankings with our original CK Buylist scrape
Low_Confidence_Report <- test
