library(tidyverse)
library(bigrquery)
library(googlesheets4)
library(googledrive)
library(RSelenium)
library(rvest)
library(readr)

bq_auth(email = "wolfoftinstreet@gmail.com", use_oob = TRUE)
con <- dbConnect(
  bigrquery::bigquery(),
  project = "gaeas-cradle",
  dataset = "premiums",
  billing = "gaeas-cradle"
)
bqr_auth(email = "wolfoftinstreet@gmail.com")




setwd("/home/cujo253/Reports/Tokyo")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
currentDate <- Sys.Date()- (Number_Of_Files)
for (i in 1:(Number_Of_Files)){
  Title_Date <- gsub("\\-","\\_",currentDate)
  tmp  <- read_csv(temp[i])
  mybq <- bq_table(project = "gaeas-cradle", dataset = "tokyo", table = paste(Title_Date,"_Tokyo",sep=""))
  bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  currentDate <- currentDate + 1
  Sys.sleep(1)
}



