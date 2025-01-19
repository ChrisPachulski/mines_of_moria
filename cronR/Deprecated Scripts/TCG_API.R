source("config.R")
install.packages("httr")
library(httr)
library(jsonlite)
r <- POST("https://api.tcgplayer.com/token", add_headers("application/x-www-form-urlencoded"), body = "grant_type=client_credentials&client_id=B7776726-DD87-488A-A610-0258CC43739F&client_secret=67CDFA3B-EB9E-4EEB-811F-C1F452ACD6DF" )
Initial_Bearer <- content(r,"parsed")
Initial_Bearer <- (as.data.frame(Initial_Bearer)
                   Bearer_Token <- paste("bearer ",Initial_Bearer$access_token,sep="")
                   
                   r2 <- GET("http://api.tcgplayer.com/v1.37.0/catalog/categories", 
                             add_headers(.headers = c("Accept" = "application/json",
                                                      "Authorization" = Bearer_Token)))
                   Bearer_Query <- content(r2,"parsed")
                   Bearer_Query[[4]]
                   unlist(Bearer_Query[[4]])
                   #rarity
                   d <- GET("http://api.tcgplayer.com/v1.37.0/catalog/categories/1/rarities", 
                            add_headers(.headers = c("Accept" = "application/json",
                                                     "Authorization" = Bearer_Token)))
                   TCG_Rarities <- as.data.frame(fromJSON(content(d,"text"))$results)
                   #foil vs nonfoil
                   d <- GET("http://api.tcgplayer.com/v1.37.0/catalog/categories/1/printings", 
                            add_headers(.headers = c("Accept" = "application/json",
                                                     "Authorization" = Bearer_Token)))
                   fromJSON(content(d,"text"))$results
                   TCG_Foil <- as.data.frame(fromJSON(content(d,"text"))$results)
                   
                   d <- GET("http://api.tcgplayer.com/v1.37.0/catalog/products/15023", 
                            add_headers(.headers = c("Accept" = "application/json",
                                                     "Authorization" = Bearer_Token)))
                   fromJSON(content(d,"text"))$results
                   TCG_Wow <- as.data.frame(fromJSON(content(d,"text"))$results)
                   
                   
                   
                   d2 <- GET("http://api.tcgplayer.com/v1.37.0/catalog/categories/1/search/manifest", 
                             add_headers(.headers = c("Accept" = "application/json",
                                                      "Authorization" = Bearer_Token)))
                   Magic <- fromJSON(content(d2,"text"))[3]
                   attempt = purrr::flatten(Magic)
                   q <- (purrr::flatten(attempt$filters))
                   TCG_Sets <- q$items[[3]]
                   TCG_Rarities <- q$items[[4]]
                   TCG_Formats <- q$items[[5]]
                   TCG_Colors <- q$items[[6]]
                   TCG_Card_Types <- q$items[[7]]
                   
                   View(q)
                   # curl --include --request POST \
                   # --header "application/x-www-form-urlencoded" \
                   # --data-binary "grant_type=client_credentials&client_id=B7776726-DD87-488A-A610-0258CC43739F&client_secret=67CDFA3B-EB9E-4EEB-811F-C1F452ACD6DF" \
                   # 'https://api.tcgplayer.com/token'