source("config.R")
require("pacman")
pacman::p_load(tidyverse,httr,bigrquery,googleAuthR,lubridate,jsonlite,data.table,reshape2,rvest, janitor)

gaeas_cradle <- function(){
    
    service_account_file = file.path(path_prefix, 'mines_of_moria', 'Essential_Referential_CSVS', 'gaeas-cradle.json')
    gar_auth_service(service_account_file)
    
    bq_auth(path = service_account_file)
    
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    #bq_auth(email = patches$patches, use_oob = TRUE)
    options(scipen = 20)
    con
    
}

bq_import = function(upload_tibble,dataset_name,table_name,event_date=Sys.Date()){
    mybq <- bq_table(project = "gaeas-cradle", dataset = dataset_name, table = paste(gsub("-","_",event_date),"_",table_name,sep=""))
    bq_table_upload(x=mybq, values = upload_tibble, fields=as_bq_fields(upload_tibble),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
    print("BQ TCG Upload Successful!")
}

system('rm -rf /home/cujo253/mines_of_moria/cronR/data/yugioh')
system('rm /home/cujo253/mines_of_moria/cronR/data/yugioh_pull.log')

system('docker run --rm --platform linux/amd64 --user 1000:1000 -v /home/cujo253/mines_of_moria/cronR/data:/scraped-data/ mtgelmo/tcg_scraper:latest --category yugioh')

# file.edit("~/.Rprofile")
# 
# install.packages("reticulate")
# 
# # Load the reticulate package
# library(reticulate)
# 
# # Specify the path to your virtual environment
# use_virtualenv("~/snakez", required = TRUE)
# py_config()
event_date = Sys.Date()
#event_hour = hour(Sys.time())

file_path = '/home/cujo253/mines_of_moria/cronR/data/yugioh/listings.csv'
listings_data_tibble =read_csv(file_path) %>% as_tibble()

file_path = '/home/cujo253/mines_of_moria/cronR/data/yugioh/product_data.csv'
product_data_tibble =read_csv(file_path) %>% as_tibble()

file_path = '/home/cujo253/mines_of_moria/cronR/data/yugioh/sellers.csv'
sellers_data_tibble = read_csv(file_path) %>% as_tibble()

con <- gaeas_cradle()


bq_import(upload_tibble = listings_data_tibble,
          dataset_name = 'docker_listings_yugioh',
          table_name='listings',
          event_date=Sys.Date())

bq_import(upload_tibble = product_data_tibble,
          dataset_name = 'docker_product_data_yugioh',
          table_name = 'product_data',
          event_date=Sys.Date())

bq_import(upload_tibble = sellers_data_tibble,
          dataset_name = 'docker_sellers_yugioh',
          table_name = 'sellers',
          event_date=Sys.Date())