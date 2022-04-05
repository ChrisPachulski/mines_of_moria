pacman::p_load(tidyverse,janitor,RMySQL,gmailr,bigrquery,googlesheets4,googledrive)
source("/home/cujo253/mines_of_moria/cronR/newspaper_updater_functions.R")


# Auxiliary Google Sheets  ------------------------------------------------

google_auths()
aux_set_sheets = set_list()
editions = aux_set_sheets[[1]]
ck_editions = aux_set_sheets[[2]]
all_editions = aux_set_sheets[[3]]


# BQ Premiums ----------------------------------------------------------------

premiums_data = dataset_extraction_and_replacement(dataset = "premiums")
email_body = premiums_data[[1]]


# BQ CK Funny Money ----------------------------------------------------------

ck_funny_money_data = dataset_extraction_and_replacement(dataset = "ck_funny_money",email_body = email_body)
email_body = ck_funny_money_data[[1]]


# BQ Buy List Growth ---------------------------------------------------------

buylist_growth_data = dataset_extraction_and_replacement(dataset = "buylist_growth",email_body = email_body)
email_body = buylist_growth_data[[1]]


# BQ Demand Growth -----------------------------------------------------------

demand_growth_data = dataset_extraction_and_replacement(dataset = "demand_growth",email_body = email_body)
email_body = demand_growth_data[[1]]

# BQ Vendor Growth -----------------------------------------------------------

vendor_growth_data = dataset_extraction_and_replacement(dataset = "vendor_growth",email_body = email_body)
email_body = vendor_growth_data[[1]]

# BQ KPI Growth -----------------------------------------------------------

kpi_data = dataset_extraction_and_replacement(dataset = "kpi",email_body = email_body)
email_body = kpi_data[[1]]


# BQ CK Velocity -------------------------------------------------------------

ck_velocity_data = dataset_extraction_and_replacement(dataset = "ck_velocity",email_body = email_body)
email_body = ck_velocity_data[[1]]


# BQ Ensemble Forecast Results -----------------------------------------------

ensemble_forecast_results_data = dataset_extraction_and_replacement(dataset = "ensemble_forecast_results",email_body = email_body)
email_body = ensemble_forecast_results_data[[1]]


# Failsafe Logic - Email & Re-run Commands --------------------------------

if(length(email_body) > 0){
    
    google_auths()
    
    email_body = email_body %>% unique()
    
    email_content = trimws(email_content_creation(email_body))
    
    send_email(to = "wolfoftinstreet@gmail.com", from = "wolfoftinstreet@gmail.com", email_content)
    
    safety_csv = tibble(origin_date = as.Date(min(error_csv(email_body)$origin_date)))
    
    write_csv(safety_csv,"/home/cujo253/mines_of_moria/Essential_Referential_CSVS/newspaper_safety.csv")
    

}else{
    try(file.remove("/home/cujo253/mines_of_moria/Essential_Referential_CSVS/newspaper_safety.csv"))
}


# Random Weight Time ------------------------------------------------------

Sys.sleep(sample(1:28800, 1))


# Upload to BAN -----------------------------------------------------------

ban_delayed_newspaper(table = "kpi", data = kpi_data[[2]])
ban_delayed_newspaper(table = "vendor_growth", data = vendor_growth_data[[2]])
ban_delayed_newspaper(table = "buylist_growth", data = buylist_growth_data[[2]])
ban_delayed_newspaper(table = "ensemble_forecast_results", data = ensemble_forecast_results_data[[2]])
ban_delayed_newspaper(table = "all_editions", data = all_editions)

ban_current_newspaper(table = "kpi", kpi_data[[3]])
ban_current_newspaper(table = "vendor_growth", vendor_growth_data[[3]])
ban_current_newspaper(table = "buylist_growth", buylist_growth_data[[3]])
ban_current_newspaper(table = "ensemble_forecast_results", ensemble_forecast_results_data[[3]])
ban_current_newspaper(table = "all_editions", all_editions)




