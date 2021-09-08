# Pacman's Ghosts ---------------------------------------------------------
install.packages("pacman")
pacman::p_load(tidyverse,bigrquery,XML,RCurl,skimr,jsonlite,lubridate, RSelenium,dplyr, googledrive, googlesheets4,zoo)

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
invisible(gaeas_cradle <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "gaeas-cradle",
    dataset = "premiums",
    billing = "gaeas-cradle"
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
})

IP = "159.65.219.70"
CS_Login = function(IP){
  remDr = remoteDriver(remoteServerAddr = IP, port = 4445, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr$navigate("https://www.cardsphere.com/login")
  Sys.sleep(5)
  
  username <- remDr$findElement(using = "id", value = "email")
  username$clearElement()
  username$sendKeysToElement(list("cjpach@mac.com"))
  
  passwd <- remDr$findElement(using = "id", value = "password")
  passwd$clearElement()
  passwd$sendKeysToElement(list("Tasigur95$"))
  
  Post_Credential_Login <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/div/div/form/button")
  Post_Credential_Login$submitElement()
  Sys.sleep(2)
  remDr$navigate("https://www.cardsphere.com/wants")
  Actions <- remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[3]/a')
  Actions$clickElement()
  
  Delete_Wants <- remDr$findElement(using = "class", value = 'btn-danger')
  Delete_Wants$clickElement()
  Delete_Wants_Check <- remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[2]/form/div/label/input')
  Delete_Wants_Check$clickElement()
  Delete_Final <-remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[3]/button[2]')
  Delete_Final$clickElement()
  New_Wants <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[1]/a')
  New_Wants$clickElement()
  
  
  for(i in 1:30){
    Offer_Perc <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[6]/div/div[1]')
    Offer_Perc$clickElement()
    Sys.sleep(.25)
  }
  
  #i = 4
  for(i in 1:nrow(CS_Import_List)){
    tryCatch(expr = {
      Card_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[1]/div/input')
      Card_Search$clickElement()
      Sys.sleep(.3)
      Card_Search$sendKeysToElement(list(CS_Import_List$Name[i]))
      Sys.sleep(3)
      Card_Search <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[1]')
      Sys.sleep(1)
      Card_Search$clickElement()
      Card_Search$clickElement()
      
      Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
      Set_Search$clickElement()
      Sys.sleep(1)
      remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')$clickElement()
      Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
      Set_Selection$clickElement()
      Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
      Sys.sleep(1)
      Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
      Set_Select$clickElement()
      Set_Select$click(buttonId = 2)
      if(unlist(Set_Search$getElementText()) == "Select sets"){
        Sys.sleep(1)
        Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
        Set_Search$clickElement()
        
        # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
        # Set_Search$clickElement()
        Sys.sleep(1)
        Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
        Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
        Sys.sleep(1)
        Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
        Set_Select$clickElement()
        Set_Select$click(buttonId = 2)
        Sys.sleep(2)
      }
      
      if(remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span[1]')$getElementText() %>% unlist() == "Select finish"){
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/div[2]/input')
        foil_box$sendKeysToElement(list("Nonfoil",key="enter"))
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
      }
      
      Sys.sleep(1)
      Quantity_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[9]/div/input')
      Sys.sleep(.20)
      Quantity_Selection$sendKeysToElement(list("2"))
      
      Sys.sleep(1)
      Submition <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[12]/button')
      Sys.sleep(2)
      Submition$clickElement()
      Sys.sleep(.25)
    }, error = function(e){ 
      remDr$refresh()
      for(j in 1:30){
        Offer_Perc <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[6]/div/div[1]')
        Offer_Perc$clickElement()
        Sys.sleep(.25)
      }
      Card_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[1]/div/input')
      Card_Search$clickElement()
      Sys.sleep(.3)
      Card_Search$sendKeysToElement(list(CS_Import_List$Name[i]))
      Sys.sleep(3)
      Card_Search <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[1]')
      Sys.sleep(1)
      Card_Search$clickElement()
      Card_Search$clickElement()
      
      Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
      Set_Search$clickElement()
      Sys.sleep(1)
      remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')$clickElement()
      Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
      Set_Selection$clickElement()
      Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
      Sys.sleep(1)
      Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
      Set_Select$clickElement()
      Set_Select$click(buttonId = 2)
      if(unlist(Set_Search$getElementText()) == "Select sets"){
        Sys.sleep(1)
        Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
        Set_Search$clickElement()
        
        # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
        # Set_Search$clickElement()
        Sys.sleep(1)
        Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
        Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
        Sys.sleep(1)
        Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
        Set_Select$clickElement()
        Set_Select$click(buttonId = 2)
        Sys.sleep(2)
      }
      
      if(remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span[1]')$getElementText() %>% unlist() == "Select finish"){
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/div[2]/input')
        foil_box$sendKeysToElement(list("Nonfoil",key="enter"))
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
      }
      
      Sys.sleep(1)
      Quantity_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[9]/div/input')
      Sys.sleep(.20)
      Quantity_Selection$sendKeysToElement(list("2"))
      
      Sys.sleep(1)
      Submition <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[12]/button')
      Sys.sleep(2)
      Submition$clickElement()
      Sys.sleep(.25)
    })
  }
}

CS_Additions = function(IP){
  remDr = remoteDriver(remoteServerAddr = IP, port = 4445, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr$navigate("https://www.cardsphere.com/login")
  Sys.sleep(5)
  
  username <- remDr$findElement(using = "id", value = "email")
  username$clearElement()
  username$sendKeysToElement(list("cjpach@mac.com"))
  
  passwd <- remDr$findElement(using = "id", value = "password")
  passwd$clearElement()
  passwd$sendKeysToElement(list("Tasigur95$"))
  
  Post_Credential_Login <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/div/div/form/button")
  Post_Credential_Login$submitElement()
  Sys.sleep(2)
  remDr$navigate("https://www.cardsphere.com/wants")
  # Actions <- remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[3]/a')
  # Actions$clickElement()
  # 
  # Delete_Wants <- remDr$findElement(using = "class", value = 'btn-danger')
  # Delete_Wants$clickElement()
  # Delete_Wants_Check <- remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[2]/form/div/label/input')
  # Delete_Wants_Check$clickElement()
  # Delete_Final <-remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[3]/button[2]')
  # Delete_Final$clickElement()
  # New_Wants <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[1]/a')
  # New_Wants$clickElement()
  
  #max_offer <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[7]/input')
  #max_offer$clickElement()
  #max_offer$sendKeysToElement(list())
  #i =49
  for(i in 1:nrow(additional_minned_history)){
    tryCatch(expr = {
      Card_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[1]/div/input')
      Card_Search$clickElement()
      Sys.sleep(.3)
      Card_Search$sendKeysToElement(list(additional_minned_history$Name[i]))
      Sys.sleep(3)
      Card_Search <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[1]')
      
      if(additional_minned_history$Name[i] == "Shadowspear"){
      suppressMessages(
      tryCatch(expr = {
      list_check = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
      if(!is.na(list_check$getElementText() %>% unlist())){
        Card_Options <- remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
        Card_Options$clickElement()
        Card_Options$sendKeysToElement(list(key = "down_arrow"))
      }
      },  error = function(e){Sys.sleep(.2)})
      )
      } 
      if(additional_minned_history$Name[i] == "Endurance"){
        suppressMessages(
          tryCatch(expr = {
            list_check = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
            if(!is.na(list_check$getElementText() %>% unlist())){
              Card_Options <- remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
              Card_Options$clickElement()
              
            }
          },  error = function(e){Sys.sleep(.2)})
        )
      } 
      if(additional_minned_history$Name[i] == "Solitude"){
        suppressMessages(
          tryCatch(expr = {
            list_check = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
            if(!is.na(list_check$getElementText() %>% unlist())){
              Card_Options <- remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
              Card_Options$clickElement()
              
            }
          },  error = function(e){Sys.sleep(.2)})
        )
      } 
      if(additional_minned_history$Name[i] == "Urza's Saga"){
        suppressMessages(
          tryCatch(expr = {
            list_check = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
            if(!is.na(list_check$getElementText() %>% unlist())){
              Card_Options <- remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
              Card_Options$clickElement()
              Card_Options$sendKeysToElement(list(key = "down_arrow"))
            }
          },  error = function(e){Sys.sleep(.2)})
        )
      } 
      
      
      Sys.sleep(1)

      Card_Search$clickElement()
      Card_Search$clickElement()
      
      Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
      Set_Search$clickElement()
      Sys.sleep(1)
      remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')$clickElement()
      Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
      Set_Selection$clickElement()
      Set_Selection$sendKeysToElement(list(additional_minned_history$Edition[i]))
      if(additional_minned_history$Edition[i] == "Modern Horizons 2"){Set_Selection$sendKeysToElement(list(key = "down_arrow"))}
      if(additional_minned_history$Edition[i] == "Modern Horizons 2"){Set_Selection$sendKeysToElement(list(key = "enter"))}
      
      Sys.sleep(1)
      Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
      Set_Select$clickElement()
      Set_Select$click(buttonId = 2)
      if(unlist(Set_Search$getElementText()) == "Select sets"){
        Sys.sleep(1)
        Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
        Set_Search$clickElement()
        
        # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
        # Set_Search$clickElement()
        Sys.sleep(1)
        Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
        Set_Selection$sendKeysToElement(list(additional_minned_history$Edition[i]))
        Sys.sleep(1)
        Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
        Set_Select$clickElement()
        Set_Select$click(buttonId = 2)
        Sys.sleep(2)
      }
      
      if(remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span[1]')$getElementText() %>% unlist() == "Select finish"){
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/div[2]/input')
        foil_box$sendKeysToElement(list("Nonfoil",key="enter"))
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
      }
      
      if(remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span[1]')$getElementText() %>% unlist() == "Foil"){
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
        remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/div[3]/div/div[2]/a')$clickElement()
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/div[2]/input')
        foil_box$sendKeysToElement(list("Nonfoil",key="enter"))
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
      }
      
      Sys.sleep(1)
      Quantity_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[9]/div/input')
      Sys.sleep(.20)
      Quantity_Selection$sendKeysToElement(list("2"))
      
      max_offer <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[7]/input')
      max_offer$clickElement()
      max_offer$clearElement()
      max_offer$clickElement()
      max_offer$sendKeysToElement(list(as.character(additional_minned_history$min_value[i])))
      
      Sys.sleep(1)
      Submition <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[12]/button')
      Sys.sleep(2)
      Submition$clickElement()
      Sys.sleep(.25)
    }, error = function(e){ 
      remDr$refresh()
      
      Card_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[1]/div/input')
      Card_Search$clickElement()
      Sys.sleep(.3)
      Card_Search$sendKeysToElement(list(additional_minned_history$Name[i]))
      Sys.sleep(3)
      Card_Search <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[1]')
      Sys.sleep(1)
        if(additional_minned_history$Name[i] == "Shadowspear"){
          suppressMessages(
            tryCatch(expr = {
              list_check = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
              if(!is.na(list_check$getElementText() %>% unlist())){
                Card_Options <- remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[2]')
                Card_Options$clickElement()
                Card_Options$sendKeysToElement(list(key = "down_arrow"))
              }
            },  error = function(e){Sys.sleep(.2)})
          )
        }
      Sys.sleep(1)
      Card_Search$clickElement()
      Card_Search$clickElement()
      
      Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
      Set_Search$clickElement()
      Sys.sleep(1)
      remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')$clickElement()
      Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
      Set_Selection$clickElement()
      Set_Selection$sendKeysToElement(list(additional_minned_history$Edition[i]))
      Sys.sleep(1)
      Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
      Set_Select$clickElement()
      Set_Select$click(buttonId = 2)
      if(unlist(Set_Search$getElementText()) == "Select sets"){
        Sys.sleep(1)
        Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
        Set_Search$clickElement()
        
        # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
        # Set_Search$clickElement()
        Sys.sleep(1)
        Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
        Set_Selection$sendKeysToElement(list(additional_minned_history$Edition[i]))
        Sys.sleep(1)
        Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
        Set_Select$clickElement()
        Set_Select$click(buttonId = 2)
        Sys.sleep(2)
      }
      
      if(remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span[1]')$getElementText() %>% unlist() == "Select finish"){
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/div[2]/input')
        foil_box$sendKeysToElement(list("Nonfoil",key="enter"))
        foil_box = remDr$findElement('xpath','//*[@id="wants"]/div[1]/form/div[4]/div/a/span')
        foil_box$clickElement()
      }
      
      Sys.sleep(1)
      Quantity_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[9]/div/input')
      Sys.sleep(.20)
      Quantity_Selection$sendKeysToElement(list("2"))
      
      max_offer <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[7]/input')
      max_offer$clickElement()
      max_offer$clearElement()
      max_offer$clickElement()
      max_offer$sendKeysToElement(list(as.character(additional_minned_history$min_value[i])))
      
      
      Sys.sleep(1)
      Submition <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[12]/button')
      Sys.sleep(2)
      Submition$clickElement()
      Sys.sleep(.25)
    })
  }
}

currentDate <- Sys.Date()



# Needed External Lists ---------------------------------------------------

tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
  #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
  rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
  mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))})

Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                        Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                        Semi = paste(name,Set,sep="")) 
Sets  = read.csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion = read.csv("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE) %>% select(Set_Excl, Excl_Excl)


# Card Kingdom Buy List Acquisition ---------------------------------------

CK_Buylist = fromJSON("https://api.cardkingdom.com/api/v2/pricelist") %>% as.data.frame() %>% 
  mutate(data.edition = as.factor(data.edition), 
         data.price_buy = as.numeric(as.character(data.price_buy)),
         data.price_retail = as.numeric(as.character(data.price_retail)))

No_Foils =  CK_Buylist                                     %>% 
  filter(data.variation == "" & data.is_foil == "false") %>% 
  group_by(data.edition)                                 %>% 
  count(data.edition)                                    %>%
  rename("edition"="data.edition","count"="n")


Foils =  CK_Buylist                                        %>% 
  filter(data.variation == "" & data.is_foil == "true")  %>% 
  group_by(data.edition)                                 %>% 
  count(data.edition)                                    %>%
  rename("edition"="data.edition","count"="n")      

NF_Total_Offers <- sum(No_Foils$count)
F_Total_Offers <- sum(Foils$count)

No_Foils = No_Foils %>% mutate(composition = (round(count/NF_Total_Offers,4)))
Foils = Foils %>% mutate(composition = (round(count/F_Total_Offers,4)))


# Jenny Craig Buy List ----------------------------------------------------

Slim_CK_Buylist = CK_Buylist                                %>% 
  select(meta.created_at     ,
         data.name           ,
         data.edition        ,
         data.is_foil        ,
         data.price_retail   ,
         data.qty_retail     ,
         data.price_buy      ,
         data.qty_buying)                                 %>%
  left_join(.,
            Sets %>% select(CK_BL_Scrape_Sets,mtgjson), 
            by = c("data.edition" = "CK_BL_Scrape_Sets")) %>%
  left_join(.,Exclusion, by = c("mtgjson" = "Set_Excl"))  %>% 
  #mutate(data.edition = mtgjson)                         %>% 
  replace_na(list(Excl_Excl = "Unclear"))                 %>%
  filter(Excl_Excl != "Exclude"   & 
           data.qty_buying != 0 &
           data.price_buy > 1.50)                       %>%
  select(-mtgjson, - Excl_Excl)                           %>%
  mutate(data.is_foil = ifelse(data.is_foil == "false",
                               "",
                               "FOIL"),
         #
         data.qty_retail = ifelse(data.qty_retail == 0,
                                  1, 
                                  data.qty_retail),
         #
         QTY_Diff = round((data.qty_buying
                           -
                             data.qty_retail)
                          /
                            data.qty_buying,2),
         #
         Price_Diff = round(data.price_buy
                            /
                              data.price_retail,2),
         
         Tier_QTY_Diff = ntile(QTY_Diff,10),
         Tier_Price_Diff = ntile(Price_Diff,10),
         Tier_data.qty_buying = ntile(data.qty_buying,10))%>%
  mutate(
    Tier = round(rowMeans(
      (select(.,Tier_QTY_Diff,
              Tier_Price_Diff,
              Tier_data.qty_buying))),2))          %>%
  
  arrange(desc(Tier))                                     %>%
  select(-Tier_QTY_Diff,
         -Tier_Price_Diff,
         -Tier_data.qty_buying)                           %>%
  
  `row.names<-` (seq(nrow(.)))                            %>%
  mutate(meta.created_at = paste(data.name,
                                 data.edition,
                                 data.is_foil,
                                 sep=""))


# Parlor Tricks for New Folks ---------------------------------------------

Eternal_Growers = Slim_CK_Buylist                           %>% 
  filter(Tier >= 9.0 & data.qty_retail >= 8)              %>%
  left_join(.,
            Sets %>% select(CK_BL_Scrape_Sets,mtgjson), 
            by = c("data.edition" = "CK_BL_Scrape_Sets")) %>%
  left_join(.,Updated_Tracking_Keys      %>% 
              select(rdate,Set)        %>% 
              distinct(), 
            by = c("mtgjson" = "Set"))                    %>% 
  mutate(data.edition = mtgjson)                          %>% 
  distinct()                                              %>%
  filter(is.na(rdate)==F                                   & 
           rdate <= (Sys.Date() %m-% months(13)))

Updated_Tracking_Keys      %>% 
  select(Set,name) %>% group_by(Set) %>%
  summarize(cards_in = n()) %>% 
  arrange(desc(cards_in))

Eternal_Growers %>% 
  select(-contains("created")) %>% 
  select(-contains("foil"))    %>% 
  select(-mtgjson,-rdate)      %>%
  mutate(data.edition = gsub(" Retail Edition Foils","",data.edition)) %>%
  mutate_if(is.character,as.factor) %>%
  group_by(data.edition)       %>%
  summarize(hits = n())  %>%
  arrange(desc(hits)) %>%
  left_join(Updated_Tracking_Keys      %>% 
              select(Set,name) %>% group_by(Set) %>%
              summarize(cards_in = n()) %>% 
              arrange(desc(cards_in)), by = c("data.edition"="Set")) %>%
  mutate(ratio = round(hits/cards_in,4)*100,
         ratio_tier = ntile(desc(ratio),10)) %>%
  filter(cards_in >= 50) %>%
  arrange(desc(ratio))

# Card Sphere Acquisition -------------------------------------------------

CardSphere_Printed_Sets = "https://www.cardsphere.com/sets" %>% 
  getURL()                                                %>% 
  htmlTreeParse(.,useInternalNode=TRUE)                   %>%
  xpathSApply("//li", xmlValue)                           %>%
  as_tibble()                                             %>%
  suppressWarnings(
    separate(value,c("x","x1"),sep="  ",remove = F)
  )                                                   %>% 
  mutate(value = trimws(value))                           %>% 
  select(value)                                           %>% 
  filter(value != "Log In"                    ,
         value != "Sign Up"                   ,
         value != "Explore"                   ,
         value != "Contact Us"                ,
         value != "Reddit"                    ,
         value != "Facebook"                  ,
         value != "Twitter"                   ,
         value != "Discord"                   ,
         value != "Blog"                      ,
         value != "Merchandise"               ,
         value != "Terms & Conditions"        ,
         value != "Privacy Policy"            ,
         value != "Changelog"                 ,
         value != "Draft & Sealed Simulator"  ,
         value != "Explore Cards"             ,
         value != "Trade Guide"               ,
         value != "Condition Guide"           ,
         value != "Tutorials & FAQ") 


Cardsphere_Set_Numbers = "https://www.cardsphere.com/sets" %>% 
  getURL()                                               %>% 
  htmlTreeParse(.,useInternalNode=TRUE)                  %>%
  xpathSApply("//a", xmlGetAttr, 'href')                 %>%
  as_tibble()                                            %>%
  mutate(value = gsub("/sets/","",value))                %>%
  filter(grepl("\\/",value)==F & 
           grepl("\\#",value)==F) 

Cardsphere_Outer_Shell = data.frame(Cardsphere_Editions = 
                                      CardSphere_Printed_Sets$value,
                                    CardSphere_Set_Numbers = 
                                      Cardsphere_Set_Numbers$value)
#Loop preparation and loading bar
All_CardSphere <- NULL
total = nrow(Cardsphere_Outer_Shell) 
pb <- txtProgressBar(min=0, max = total, style = 3)
Q <- 1
pacman::p_load(rvest)
for (set in Cardsphere_Outer_Shell$CardSphere_Set_Numbers) {
  Sys.sleep(.5)
  cs_attempt <- paste("https://www.cardsphere.com/sets/",set,sep="") %>%
    read_html()                                                    %>%
    html_nodes(".cs-row")                                          %>% 
    html_text()                                                    %>% 
    as_tibble()                                                    %>% 
    mutate(value = gsub(" '/(\r\n)+|\r+|\n\\s+|\t+/i'","",value))  %>%
    filter(grepl("\\$.*\\$.*\\$",value) != T                        &
             grepl("Booster Box",value) != T                          &
             grepl("Booster Pack",value) != T                         &
             grepl("Fat Pack",value) != T                             &
             grepl("Back to Top",value) != T)                        %>%
    separate(value,c("name","nf_price","f_price"),sep="\\$")       %>%
    mutate(Set_Number = set,
           nf_price = as.numeric(nf_price),
           f_price = as.numeric(f_price))                          %>%
    left_join(.,Cardsphere_Outer_Shell, 
              by = c("Set_Number"="CardSphere_Set_Numbers"))       %>%
    mutate(isfoil = "")                                            %>%
    relocate("name",
             "Cardsphere_Editions",
             "isfoil",
             "nf_price",
             "f_price")                                            %>%
    select(-Set_Number)
  #
  #
  cs_nonfoil_prices = cs_attempt                              %>% 
    select("name",
           "Cardsphere_Editions",
           "isfoil",
           "nf_price")                                      %>%
    mutate(Key = paste(name,
                       Cardsphere_Editions,
                       isfoil,sep=""))                      %>%
    relocate(Key,everything())                              %>%
    rename(c("price" = "nf_price",
             "edition" = "Cardsphere_Editions"))
  #
  #
  cs_foil_prices = cs_attempt                                 %>% 
    select("name",
           "Cardsphere_Editions",
           "isfoil",
           "f_price")                                       %>%
    mutate(isfoil = "FOIL",
           Key = paste(name,
                       Cardsphere_Editions,
                       isfoil,sep=""))                      %>%
    relocate(Key,everything())                              %>%
    rename(c("price" = "f_price", 
             "edition" = "Cardsphere_Editions"))
  
  #
  cs_prices = rbind(cs_nonfoil_prices,cs_foil_prices) %>% na.omit()
  #
  All_CardSphere <- rbind(All_CardSphere, cs_prices)
  Sys.sleep(sample(.29:1.63, 1))
  setTxtProgressBar(pb,Q)
  Q <- Q+1
} 

CardSphere_Final_Output <- All_CardSphere



# Card Sphere Conditioning Raw Output -------------------------------------

CardsphereNF = CardSphere_Final_Output                       %>%
  mutate(offer_on_retail = round(price*.70,1))             %>%
  left_join(.,
            Slim_CK_Buylist                     %>% 
              select(meta.created_at,
                     data.price_buy), 
            by = c("Key" = "meta.created_at"))             %>%
   left_join(.,
             Updated_Tracking_Keys               %>% 
               select(rdate,Semi), 
             by = c("Key"="Semi"))                          %>%
   relocate(rdate,Key,name,edition,everything())           %>%
  mutate(CK_BL_Offer = round((data.price_buy * .85),2),
         Opportunities = data.price_buy - offer_on_retail,
         Perc_Marg = round(Opportunities/CK_BL_Offer,2),
         Perc_Opp = round(Opportunities/offer_on_retail,2))%>%
  filter(isfoil == ""                                       &
           Opportunities >= 2.0                              & 
           Perc_Opp >= .15                                    &
           Perc_Marg >= .15                                   &
           CK_BL_Offer >= 2                           
         
  )                                                 %>%
  select(-data.price_buy)                                  %>%
  distinct()                                               %>%
  na.omit()                                                %>%
  arrange(desc(Opportunities))                             %>%
  filter(grepl("Urza's Tower",name)!=T)                    %>%
  filter(grepl("Urza's Mine",name)!=T)                     %>%
  filter(rdate <= (Sys.Date()-90) )

# Prepare for Google Sheet Export -----------------------------------------

# CardsphereNF %>% view()
#Additional_Line = c(12,0,"Allosaurus Shepherd","Jumpstart","Near Mint","English","","")
CS_Import_List = data.frame(Quantity        = 12                  ,
                            Tradelist_Count = 0                   ,
                            Name            = CardsphereNF$name   ,
                            Edition         = CardsphereNF$edition,
                            Condition       = "Near Mint"         ,
                            Language        = "English"           ,
                            Finish          = ""                  ,
                            Tags            = ""                  ) %>%
  filter(grepl("^Urza's",Name)==F)

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("Cardsphere_Import_List")
sheet_write(CS_Import_List, ss = ss,sheet = "CS_Import_List")


# R Selenium - Implementation on Site --------------------------------------
CS_Login("159.65.219.70")

# Additional Addins From DB-------------------------------------------------------

con <- gaeas_cradle("wolfoftinstreet@gmail.com")

statement <- paste(
  'SELECT DISTINCT Key,name,a.set,Rarity, count(*) as occur FROM `gaeas-cradle.ensemble_forecast_results.*` a WHERE  _TABLE_SUFFIX BETWEEN 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 10 DAY)) AND 
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND 
  (classification not like "Ignore" AND classification not like "F" AND classification not like "E" AND classification not like "D")  
  GROUP BY 1,2,3,4 
  order by occur desc  ',
  sep = ""
)

ensemble_occur <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() 

Eternal_Growers = Eternal_Growers %>% rename(name=data.name,set=data.edition) %>% select(name,set)

query_needs = ensemble_occur %>% filter(occur >= 5) %>% select(name,set) %>% distinct() %>% rbind(Eternal_Growers) %>% distinct() %>% drop_na() %>%
  filter((grepl("Foil",set)==F))

target_list = NULL
for(i in 1:nrow(query_needs)){
  card_valuation = NA
  tryCatch({
    statement <- paste(
      '
    WITH t1 as 
    (SELECT *, concat(Card_name,a.Set,Rarity,isFoil) Key   
    FROM `tcg-sr-rr.',unique(tolower(gsub("-","_",gsub("&","and",gsub("’","",gsub("\\.","",gsub(":","",gsub("'","",gsub(" ","_",query_needs$set[i]))))))))),'.*`a 
    WHERE  _TABLE_SUFFIX BETWEEN 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 150 DAY)) AND 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND 
    Methodology like "SR" AND 
    Card_name like "',query_needs$name[i],'" AND 
    isFoil not like "FOIL" 
    Order By Date), 
    t2 as 
    (SELECT Key, BL, CK_Backing, Sellers, Date 
    FROM `gaeas-cradle.ck_funny_money.*` 
    WHERE  _TABLE_SUFFIX BETWEEN 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 150 DAY)) AND 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND 
     Card like "',query_needs$name[i],'") 
    SELECT DISTINCT t1.Key, t1.Card_name, t1.Set, t1.Rarity, t1.number, t1.MKT_EST, CASE WHEN t2.BL is NULL THEN 0 ELSE t2.BL END BL, t2.Sellers, 
    CASE WHEN t2.CK_Backing IS NULL THEN 0 ELSE t2.CK_Backing END CK_Backing_Perc, t1.Date 
    FROM t1  
    LEFT join t2 on CONCAT(t1.Key, t1.Date) = CONCAT(t2.Key, t2.Date) 
    WHERE number = (SELECT DISTINCT min(number) as number FROM t1) 
    ORDER BY Date; 
    
    ',
      sep = ""
    )
    tryCatch({card_sample <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()},error = function(e){print("Couldn't query this card")})
    value = length(unique(card_sample$Sellers))
    if(value == 1){next}
    card_sample
    temp_1 = card_sample %>% mutate(avg_value_1 = round(rollmean(BL,90,na.pad = T,align = c("right")),1),
                                    avg_value_2 = round(rollmean(BL,75,na.pad = T,align = c("right")),1),
                                    avg_value_3 = round(rollmean(BL,60,na.pad = T,align = c("right")),1),
                                    avg_value_4 = round(rollmean(BL,45,na.pad = T,align = c("right")),1),
                                    avg_value_5 = round(rollmean(BL,30,na.pad = T,align = c("right")),1)) %>% drop_na()
    
    
    temp_2 = card_sample %>% mutate(avg_value_1l = round(rollmean(BL,90,na.pad = T,align = c("right")),1) %>% lag(1),
                                    avg_value_2l = round(rollmean(BL,75,na.pad = T,align = c("right")),1) %>% lag(1),
                                    avg_value_3l = round(rollmean(BL,60,na.pad = T,align = c("right")),1) %>% lag(1),
                                    avg_value_4l = round(rollmean(BL,45,na.pad = T,align = c("right")),1) %>% lag(1),
                                    avg_value_5l = round(rollmean(BL,30,na.pad = T,align = c("right")),1) %>% lag(1)) %>% drop_na()
    
    temp_3 = temp_1 %>% left_join(temp_2 %>% select(Date,avg_value_1l,avg_value_2l,avg_value_3l,avg_value_4l,avg_value_5l), by =c("Date"="Date")) %>%
      mutate(lagged_1 = ifelse(is.na(avg_value_1l)==T,0,avg_value_1l), check_col_1 = ifelse(avg_value_1 >= avg_value_1l,1,0),
             lagged_2 = ifelse(is.na(avg_value_2l)==T,0,avg_value_2l), check_col_2 = ifelse(avg_value_2 >= avg_value_2l,1,0),
             lagged_3 = ifelse(is.na(avg_value_3l)==T,0,avg_value_3l), check_col_3 = ifelse(avg_value_3 >= avg_value_3l,1,0),
             lagged_4 = ifelse(is.na(avg_value_4l)==T,0,avg_value_4l), check_col_4 = ifelse(avg_value_4 >= avg_value_4l,1,0),
             lagged_5 = ifelse(is.na(avg_value_5l)==T,0,avg_value_5l), check_col_5 = ifelse(avg_value_5 >= avg_value_5l,1,0)) %>% 
      select(-contains("_value_"),-contains("lagged_")) %>% drop_na() %>% rowwise() %>%
      mutate(check_col = mean(c(check_col_1,check_col_2,check_col_3,check_col_4,check_col_5))) %>% ungroup() %>% arrange(Date) %>%
      select(-contains("check_col_")) %>%
      mutate(growth_ratio = round(sum(check_col)/nrow(.),4))
    
    if(nrow(temp_3)==0){next}
    
    card_valuation = if( (sum(temp_3$check_col)/nrow(temp_3)>=.1)){(temp_3 %>% select(Key,Card_name,Set,Rarity,growth_ratio) %>% distinct())}else{NA}
    
    oldest_sample = card_sample %>% filter(Date==min(Date)) %>% rename(MKT_150=MKT_EST,BL_150=BL,BLB_150=CK_Backing_Perc) %>% select(MKT_150,BL_150,BLB_150) %>% dplyr::slice(1)
    
    ninety_sample = card_sample %>% filter(Date== (max(Date)-90) | Date== (max(Date)-89) | Date==(max(Date)-88) ) %>% 
      rename(MKT_90=MKT_EST,BL_90=BL,BLB_90=CK_Backing_Perc) %>% filter(Date == min(Date)) %>% select(MKT_90,BL_90,BLB_90) %>% dplyr::slice(1)
    
    thirty_sample = card_sample %>% filter(Date== (max(Date)-30) | Date== (max(Date)-29) | Date==(max(Date)-28) | Date==(max(Date)-27) | Date==(max(Date)-26) ) %>% 
      rename(MKT_30=MKT_EST,BL_30=BL,BLB_30=CK_Backing_Perc) %>% filter(Date == min(Date)) %>% select(MKT_30,BL_30,BLB_30) %>% dplyr::slice(1)
    
    card_valuation = cbind(card_valuation,oldest_sample)
    card_valuation = cbind(card_valuation,ninety_sample)
    card_valuation = cbind(card_valuation,thirty_sample)
    
    if(is.na(card_valuation$Key)==F){target_list = rbind(target_list,card_valuation)}else{print(paste(query_needs$name[i]," ",query_needs$set[i]," failed parameters"))}
    },
    
    error = function(e){print("No love for this card")})
  
}


partial_combine = target_list %>% distinct() %>% mutate(Semi = paste(Card_name,Set,sep="")) %>% left_join(CardSphere_Final_Output %>% filter(isfoil == "") %>%
                                                                                             mutate(edition = gsub("Secret Lair Drop","Secret Lair Drop Series",edition),
                                                                                                    edition = gsub("Tenth","10th",edition),
                                                                                                    edition = gsub("Ninth","9th",edition),
                                                                                                    edition = gsub("Eighth","8th",edition),
                                                                                                    edition = gsub("Seventh","7th",edition),
                                                                                                    edition = gsub("Sixth","6th",edition),
                                                                                                    edition = gsub("Fifth","5th",edition),
                                                                                                    edition = gsub("Fourth","4th",edition),
                                                                                                    edition = gsub("Three","3rd",edition),
                                                                                                    edition = gsub(" edition "," ",edition),
                                                                                                    edition = gsub(" Anthology:",":",edition),
                                                                                                    edition = gsub("5th Dawn","Fifth Dawn",edition),
                                                                                                    edition = gsub("the Coalition","The Coalition",edition),
                                                                                                    edition = gsub("Commander 2011","Commander",edition),
                                                                                                    edition = gsub("Magic 2010","2010 Core edition",edition),
                                                                                                    edition = gsub("Magic 2011","2011 Core edition",edition),
                                                                                                    edition = gsub("Magic 2012","2012 Core edition",edition),
                                                                                                    edition = gsub("Magic 2013","2013 Core edition",edition),
                                                                                                    edition = gsub("Magic 2014","2014 Core edition",edition),
                                                                                                    edition = gsub("Magic 2015","2015 Core edition",edition),
                                                                                                    Semi = paste(name,edition,sep="")) %>% select(Semi,price), 
                                                                                           by = c("Semi"="Semi")) %>% distinct() %>%
  left_join(Slim_CK_Buylist %>% 
              select(meta.created_at,data.price_retail,data.qty_retail,data.price_buy,data.qty_buying,QTY_Diff,Price_Diff,Tier) %>%
              mutate(Tier = round(Tier,0)),
            by = c("Semi"="meta.created_at")) %>% 
  rename(ck_retail = data.price_retail,ck_qty = data.qty_retail,ck_buy = data.price_buy,ck_buy_qty = data.qty_buying) %>%
  select(-Semi) %>%
  mutate(ck_buy = ifelse(is.na(ck_buy), round(MKT_30 * BLB_30,1),ck_buy),
         ck_retail = ifelse(is.na(ck_retail), round(MKT_30 * 1.1,1),ck_retail),
         ck_buy_qty = ifelse(is.na(ck_buy_qty), 1,ck_buy_qty),
         QTY_Diff = ifelse(is.na(QTY_Diff), min(QTY_Diff,na.rm=T),QTY_Diff),
         Tier = ifelse(is.na(Tier), max(Tier,na.rm=T),Tier),
         Price_Diff = ifelse(is.na(Price_Diff),round(ck_buy/price,2),Price_Diff))

Short_list_keys = NULL
for(i in unique(partial_combine$Key)){
  Short_list = paste('"',i,'",',sep="")
  Short_list_keys = paste(Short_list_keys,Short_list,sep="")
}

Short_list_keys = gsub(",$","",Short_list_keys)

con                = gaeas_cradle("wolfoftinstreet@gmail.com")

statement <- paste(
  'SELECT a.Key, number, Param param,TCG_MKT, Date  
    FROM `gaeas-cradle.ck_funny_money.*` a
    LEFT JOIN `gaeas-cradle.roster.mtgjson` b on b.tcg_id = a.Param
      WHERE a.Key in (',Short_list_keys,') and 
      _Table_Suffix between 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 1 DAY)) AND 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 0 DAY))  ',
  sep = ""
)

tcg_values <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% filter(Date == max(Date)) %>% select(-Date)

#tcg_values %>% filter(grepl("Huatli",Key))

all_combined = partial_combine %>% 
  rename(cs_price = price) %>% 
  left_join(tcg_values, by=c("Key"="Key")) %>% distinct() %>%
  select(param,Key,Card_name,Set,Rarity,number,everything()) %>% 
  mutate(ck_retail = ifelse(ck_retail >= TCG_MKT * 1.07, round(ck_retail * .85,1),ck_retail),
         cs_price = ifelse(is.na(cs_price)==T,TCG_MKT,cs_price)) %>% distinct()

i = 1
ecom_retail = NULL
for(i in 1:nrow(all_combined)){
  ecom = round(weighted.mean(x=c(all_combined$cs_price[i],all_combined$ck_retail[i],all_combined$TCG_MKT[i]), w=c(.15,.05,.75) ),1) 
  ecom_retail = rbind(ecom_retail,ecom)
}

all_combined$ecom_retail = ecom_retail %>% as.vector()

#all_combined %>% view()

personal_review = all_combined %>% 
  select(-cs_price,-ck_retail,-TCG_MKT,-ck_qty,-ck_buy_qty,-QTY_Diff) %>% 
  select(param, Key, Card_name, Set, Rarity, number, ecom_retail,everything(),Price_Diff) %>%
  #mutate(my_tier = round(growth_ratio * Tier,1)) %>%
  rename(ck_ratio = Price_Diff) %>%
  mutate(full_mkt_chg = round( (ecom_retail - MKT_150)/ecom_retail ,2),
         full_bl_chg = round( (ck_buy - BL_150)/ck_buy,2),
         full_blb_chg = round( (ck_ratio - BLB_150)/ck_ratio ,2),
         
         mid_mkt_chg = round( (ecom_retail - MKT_90)/ecom_retail ,2),
         mid_bl_chg = round( (ck_buy - BL_90)/ck_buy,2),
         mid_blb_chg = round( (ck_ratio - BLB_90)/ck_ratio ,2),
         
         short_mkt_chg = round( (ecom_retail - MKT_30)/ecom_retail ,2),
         short_bl_chg = round( (ck_buy - BL_30)/ck_buy,2),
         short_blb_chg = round( (ck_ratio - BLB_30)/ck_ratio ,2),
  ) %>% select(-contains("_150"),-contains("_90"),-contains("_30")) %>%
  rowwise(param,Key,Card_name,Set,Rarity,number, ecom_retail, growth_ratio, ck_buy, ck_ratio, Tier) %>%
  summarize(
    mkt_avg = round(sum(full_mkt_chg,mid_mkt_chg,short_mkt_chg)/3,2),
    bl_avg = round(sum(full_bl_chg,mid_bl_chg,short_bl_chg)/3,2),
    blb_avg = round(sum(full_blb_chg,mid_blb_chg,short_blb_chg)/3,2),
    averages = round(sum(mkt_avg,bl_avg,blb_avg)/3,2),
    growth_ratio = round(growth_ratio * averages,1),
    my_tier = Tier
  ) %>% distinct() %>% 
  select(-Tier) %>% arrange(desc(my_tier)) %>% distinct()

my_offer = personal_review %>% mutate(ecom_ratio = round(ck_buy/ecom_retail,2)) %>%
  mutate(ecom_ratio = ifelse(ecom_ratio>ck_ratio, round(ck_ratio*.9,2) , round(ecom_ratio*.9,2))) %>%
  mutate(exp_offer = round(ecom_ratio * ecom_retail,1)) %>%
  mutate(exp_offer = ifelse(my_tier == 10, round(exp_offer * 1.25,1),
                            ifelse(my_tier >= 9.5, round(exp_offer * 1.20,1),
                                   ifelse(my_tier >= 9.0, round(exp_offer * 1.15,1),
                                          ifelse(my_tier >= 8.5, round(exp_offer * 1.12,1),
                                                 ifelse(my_tier >= 8.0, round(exp_offer * 1.08,1),
                                                        ifelse(my_tier >= 7.5, round(exp_offer * 1.05,1),
                                                               ifelse(my_tier <= 7.0, round(exp_offer * 1.00,1),round(ck_buy * .70,1)
                                                               )))))))) %>%
  #mutate(exp_offer = ifelse( (exp_offer) >= (ecom_retail*.85) ,round(exp_offer * .85,1),exp_offer)) %>% 
  #mutate(exp_offer = ifelse(growth_ratio >= (ck_ratio * 1.5), round(ck_buy * 1.15,1), exp_offer ),
  #       exp_offer = ifelse(growth_ratio <= (ck_ratio * 1.05), exp_offer * .70, exp_offer )) %>%
  arrange(desc(my_tier)) %>%  distinct() %>% drop_na() %>%
  mutate(Set = gsub("Secret Lair Drop","Secret Lair Drop Series",Set),
         Set = gsub("Tenth","10th",Set),
         Set = gsub("Ninth","9th",Set),
         Set = gsub("Eighth","8th",Set),
         Set = gsub("Seventh","7th",Set),
         Set = gsub("Sixth","6th",Set),
         Set = gsub("Fifth","5th",Set),
         Set = gsub("Fourth","4th",Set),
         Set = gsub("Three","3rd",Set),
         Set = gsub(" Set "," ",Set),
         Set = gsub(" Anthology:",":",Set),
         Set = gsub("5th Dawn","Fifth Dawn",Set),
         Set = gsub("the Coalition","The Coalition",Set),
         Set = gsub("Commander 2011","Commander",Set),
         Set = gsub("Magic 2010","2010 Core Set",Set),
         Set = gsub("Magic 2011","2011 Core Set",Set),
         Set = gsub("Magic 2012","2012 Core Set",Set),
         Set = gsub("Magic 2013","2013 Core Set",Set),
         Set = gsub("Magic 2014","2014 Core Set",Set),
         Set = gsub("Magic 2015","2015 Core Set",Set),
         Set = gsub("Theros Beyond Death","Theros: Beyond Death",Set)) %>%
  filter( round((ck_buy - ecom_retail)/ck_buy,2 ) <= .05 ) %>%
  filter(round(exp_offer/ecom_retail,2) >= .4) %>%
  left_join(all_combined %>% select(param,cs_price),by=c("param"="param")) %>%
  mutate(my_offer_cs_ratio = round(exp_offer/cs_price,2),
         exp_offer = ifelse(my_offer_cs_ratio > .6, round(cs_price * .6,2),exp_offer),
         my_offer_cs_ratio = round(exp_offer/cs_price,2)) %>% 
  filter(my_offer_cs_ratio >= .5) %>%
  mutate(exp_offer = round(cs_price * .50,2)) %>%
  filter(exp_offer < (ck_buy - 2) )


# Best Sellers Addition ---------------------------------------------------
bs_additions_tbl = NULL
try({
con                = gaeas_cradle("wolfoftinstreet@gmail.com")

statement = paste('
  With t1 as (
SELECT DISTINCT Product_ID
  FROM `gaeas-cradle.mtg_churn.',gsub("-","_",Sys.Date()-1),'_mtg_churn` c
  LEFT JOIN (SELECT  PARSE_DATE("%Y-%m-%d",  rdate) as rdate, a.set FROM `gaeas-cradle.roster.mtgjson` a) b on b.set = c.set
  WHERE earliest_dop >= (current_date() - 13) and latest_dop >= (current_date() - 1) and price >= 5 and sales_per_day >= 5
),
t2 as (
  SELECT *
  FROM `gaeas-cradle.mtg_churn.*`
  WHERE _TABLE_SUFFIX BETWEEN
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 7 DAY)) AND
  FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND 
  Methodology like "SR"
)
SELECT *
FROM (
SELECT t1.Product_ID,t2.Card_name,t2.Set,t2.Rarity,t2.isFoil,t2.number,round(AVG(t2.price),2) as MKT, round(AVG(t2.sales_per_day),2) sales_per_day
FROM t1
LEFT JOIN t2 on t2.Product_ID = t1.Product_ID
GROUP BY 1,2,3,4,5,6
ORDER BY Card_name,isFoil) b 
WHERE Card_name is not null and sales_per_day >= 5 and MKT >= 35
ORDER BY sales_per_day desc
',sep="")

bs_values <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()

bs_extended_tbl = bs_values %>% mutate(Semi = paste(Card_name,Set,sep="")) %>% left_join(CardSphere_Final_Output %>% filter(isfoil == "") %>%
                                                                             mutate(edition = gsub("Secret Lair Drop","Secret Lair Drop Series",edition),
                                                                                    edition = gsub("Tenth","10th",edition),
                                                                                    edition = gsub("Ninth","9th",edition),
                                                                                    edition = gsub("Eighth","8th",edition),
                                                                                    edition = gsub("Seventh","7th",edition),
                                                                                    edition = gsub("Sixth","6th",edition),
                                                                                    edition = gsub("Fifth","5th",edition),
                                                                                    edition = gsub("Fourth","4th",edition),
                                                                                    edition = gsub("Three","3rd",edition),
                                                                                    edition = gsub(" edition "," ",edition),
                                                                                    edition = gsub(" Anthology:",":",edition),
                                                                                    edition = gsub("5th Dawn","Fifth Dawn",edition),
                                                                                    edition = gsub("the Coalition","The Coalition",edition),
                                                                                    edition = gsub("Commander 2011","Commander",edition),
                                                                                    edition = gsub("Magic 2010","2010 Core edition",edition),
                                                                                    edition = gsub("Magic 2011","2011 Core edition",edition),
                                                                                    edition = gsub("Magic 2012","2012 Core edition",edition),
                                                                                    edition = gsub("Magic 2013","2013 Core edition",edition),
                                                                                    edition = gsub("Magic 2014","2014 Core edition",edition),
                                                                                    edition = gsub("Magic 2015","2015 Core edition",edition),
                                                                                    Semi = paste(name,edition,sep="")) %>% select(Semi,price) %>% rename(cs_price = price), 
                                                                           by = c("Semi"="Semi")) %>% distinct() %>%
  left_join(Slim_CK_Buylist %>% 
              select(meta.created_at,data.price_retail,data.qty_retail,data.price_buy,data.qty_buying,QTY_Diff,Price_Diff,Tier) %>%
              mutate(Tier = round(Tier,0)),
            by = c("Semi"="meta.created_at")) %>% 
  rename(ck_retail = data.price_retail,ck_qty = data.qty_retail,ck_buy = data.price_buy,ck_buy_qty = data.qty_buying) %>%
  mutate(exp_offer = ck_buy + 1,
         diff = value - exp_offer) %>%
  filter(diff >= 5) %>% distinct() %>%
  filter(isFoil == "") %>%
  #lazy addendum
  mutate(exp_offer = round(cs_price * .67,2) )



bs_additions_tbl = data.frame(Quantity        = 12                  ,
                              Tradelist_Count = 0                   ,
                              Name            = bs_extended_tbl$Card_name,
                              Edition         = bs_extended_tbl$Set,
                              Condition       = "Near Mint"         ,
                              Language        = "English"           ,
                              Finish          = ""                  ,
                              Tags            = ""                  ,
                              min_value       = bs_extended_tbl$exp_offer) %>% distinct()

})
#CardSphere_Final_Output
if(nrow(my_offer) == 0 ){additional_minned_history = NULL}else{
  additional_minned_history = data.frame(Quantity        = 12                  ,
                                         Tradelist_Count = 0                   ,
                                         Name            = my_offer$Card_name,
                                         Edition         = my_offer$Set,
                                         Condition       = "Near Mint"         ,
                                         Language        = "English"           ,
                                         Finish          = ""                  ,
                                         Tags            = ""                  ,
                                         min_value       = my_offer$exp_offer) %>% distinct()
  }

additional_minned_history = rbind(additional_minned_history,bs_additions_tbl)

additional_minned_history = additional_minned_history %>% 
  mutate(Edition = gsub("Core Set 2020","Core 2020",Edition)) %>%
  mutate(Edition = gsub("Theros Beyond Death","Theros: Beyond Death",Edition)) %>%
  group_by(Quantity,Tradelist_Count,Name,Edition,Condition,Language,Finish,Tags) %>%
  summarize(min_value = min(min_value)) %>% ungroup()

# additional_minned_history %>% view()

# Selenium for additions
CS_Additions("159.65.219.70")
print("Additions Added Too!")

