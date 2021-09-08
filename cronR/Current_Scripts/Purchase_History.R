install.packages('RSelenium')
install.packages("stringi")
library(stringi)
library(RSelenium)
library(rvest)
library(jsonlite)
library(tidyverse)


remDr = remoteDriver(remoteServerAddr = "64.225.17.196", port = 4445L, browser = "chrome")
remDr$open()

remDr$navigate("https://store.tcgplayer.com/login?returnUrl=https://www.tcgplayer.com/")


webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = 'home' ) )

username <- remDr$findElement(using = "id", value = "Email")
username$clearElement()
username$sendKeysToElement(list("cjpach@icloud.com"))

passwd <- remDr$findElement(using = "id", value = "Password")
passwd$clearElement()
passwd$sendKeysToElement(list("Grades95$"))

Post_Credential_Login <- remDr$findElement(using = "id", value = "loginButton")
Post_Credential_Login$submitElement()

remDr$navigate("https://store.tcgplayer.com/myaccount/orderhistory")

history <- remDr$findElement(using = "id", value = "DateRange")
history$sendKeysToElement(list("All Time"))
Sys.sleep(1)

get_html <- function(remDr){
  remDr$getPageSource() %>% .[[1]] %>% read_html()
}

Page_Contents <- get_html(remDr)
remDr$findElement("css", ".orderHeader")$getElementText()

Attempt <- remDr$findElements(using = "tag name", value = 'select')
Attempt
URL <- remDr$getCurrentUrl()
URL <- as.character(URL)
Attempt = fromJSON(URL)
