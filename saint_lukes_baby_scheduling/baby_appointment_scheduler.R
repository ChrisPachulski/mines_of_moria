pacman::p_load(tidyverse,bigrquery,XML,RCurl,skimr,jsonlite,lubridate, RSelenium,dplyr, googledrive, googlesheets4,zoo,rvest)

IP = "159.203.123.73"
remDr = remoteDriver(remoteServerAddr = IP, port = 4445, browser = "chrome")
remDr$open()
remDr$maxWindowSize()
remDr$navigate("https://www.slhn.org/womens/obstetrics/childbirth-and-pregnancy-classes/new-beginnings-tour")
html_info = remDr$getPageSource() %>% .[[1]] %>% read_html()

link_names = html_info %>% html_nodes("a") %>% 
    html_text() %>%
    as_tibble() %>%
    filter(grepl('Onsite New Beginnings Tour.*Anderson',value)) %>%
    rename(text_name = value)

url_info = html_info %>% html_nodes("a") %>% 
    html_attr('href') %>% 
    trimws() %>% 
    as_tibble() %>% 
    filter(grepl('onsite-new-beginnings-tour-anderson-campus',value)) %>%
    rename(link_url = value)

link_tbl = cbind(link_names, url_info)

link_tbl %>% write_csv('/home/cujo253/mines_of_moria/saint_lukes_baby_scheduling/historical_links.csv')


successfully_scheduled = read_csv('/home/cujo253/mines_of_moria/saint_lukes_baby_scheduling/confirmation.csv')

if(successfully_scheduled$is_scheduled[1] == "No"){
    for(i in 1:nrow(link_tbl)){
        remDr$navigate(link_tbl$link_url[i])
        Sys.sleep(3)
        if(remDr$getTitle()[[1]][1] != "Leading CMMS System & Enterprise Asset Management Software"){
            sold_out_status_text = ""
            sold_out_status_list = remDr$findElement('xpath','//*[@id="registration-data"]/div/table/tbody/tr[1]/td[6]/span')
            sold_out_status_text = sold_out_status_list$getElementText() %>% unlist()
            
            if(sold_out_status_text != "Sold Out"){
                quantity_field = remDr$findElement('xpath','//*[@id="quantity_1"]')
                quantity_field$clickElement()
                quantity_field$sendKeysToElement(list('2'))
                Sys.sleep(3)
                
                form_attribute_test = remDr$findElement('xpath','//*[@id="registration-data"]/div/table/tbody/tr[1]')
                form_identifier = form_attribute_test$getElementAttribute('data-productguid')[[1]][1]
                
                first_name_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_firstname"]'))
                first_name_field$clickElement()
                first_name_field$sendKeysToElement(list('Chris'))
                
                last_name_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_lastname"]'))
                last_name_field$clickElement()
                last_name_field$sendKeysToElement(list('Pachulski'))
                
                email_address_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_emailaddress"]'))
                email_address_field$clickElement()
                email_address_field$sendKeysToElement(list('cjpach@mac.com'))
                
                phone_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_phone"]'))
                phone_field$clickElement()
                phone_field$sendKeysToElement(list('2103243649'))
                
                mom_dob_xpath = remDr$findElement('xpath',str_glue('//*[@id="questions_{form_identifier}"]/td/div[1]/div[2]/div[6]/label'))$getElementAttribute('for')[[1]][1]
                
                mom_dob_field = remDr$findElement('xpath',str_glue('//*[@id="{mom_dob_xpath}"]'))
                mom_dob_field$clickElement()
                mom_dob_field$sendKeysToElement(list('04/25/1995'))
                
                mom_due_xpath = remDr$findElement('xpath',str_glue('//*[@id="questions_{form_identifier}"]/td/div[1]/div[2]/div[7]/label'))$getElementAttribute('for')[[1]][1]
                
                mom_due_field = remDr$findElement('xpath',str_glue('//*[@id="{mom_due_xpath}"]'))
                mom_due_field$clickElement()
                mom_due_field$sendKeysToElement(list('04/25/2023'))
                
                mom_obstet_xpath = remDr$findElement('xpath',str_glue('//*[@id="questions_{form_identifier}"]/td/div[1]/div[2]/div[8]/label'))$getElementAttribute('for')[[1]][1]
                
                mom_obstet_field = remDr$findElement('xpath',str_glue('//*[@id="{mom_obstet_xpath}"]'))
                mom_obstet_field$clickElement()
                mom_obstet_field$sendKeysToElement(list('NA'))
                
                address_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_address"]'))
                address_field$clickElement()
                address_field$sendKeysToElement(list('1023 Jean Ct'))
                
                city_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_city"]'))
                city_field$clickElement()
                city_field$sendKeysToElement(list('Easton'))
                
                state_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_state"]'))
                state_field$clickElement()
                state_field$sendKeysToElement(list('PA'))
                
                zipcode_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_0_zipcode"]'))
                zipcode_field$clickElement()
                zipcode_field$sendKeysToElement(list('18045'))
                
                # copy_info_button 
                remDr$findElement('xpath',str_glue('//*[@id="questions_{form_identifier}"]/td/div[2]/button'))$clickElement()
                
                Sys.sleep(1)
                first_name_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_1_firstname"]'))
                first_name_field$clickElement()
                first_name_field$clearElement()
                first_name_field$sendKeysToElement(list('Dana'))
                
                email_address_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_1_emailaddress"]'))
                email_address_field$clickElement()
                email_address_field$clearElement()
                email_address_field$sendKeysToElement(list('dana.kim.425@gmail.com'))
                
                phone_field = remDr$findElement('xpath',str_glue('//*[@id="{form_identifier}_1_phone"]'))
                phone_field$clickElement()
                phone_field$clearElement()
                phone_field$sendKeysToElement(list('8082562239'))
                
                remDr$findElement('xpath','//*[@id="registration-data"]/div/table/tfoot/tr/td[5]/button')$clickElement()
                Sys.sleep(2)
                
                remDr$findElement('xpath','//*[@id="lbtnContinue"]')$clickElement()
                
                successfully_scheduled = as_tibble(data.frame(is_scheduled = "yes"))
                successfully_scheduled %>% write_csv('/home/cujo253/mines_of_moria/saint_lukes_baby_scheduling/confirmation.csv')
                
            }
        }
    }
}

print(str_glue('Attempted Sign Up resulted in the following status: {successfully_scheduled$is_scheduled[1]}. Attempt was made at {format(Sys.time(), tz="America/New_York",usetz=TRUE)}'))


