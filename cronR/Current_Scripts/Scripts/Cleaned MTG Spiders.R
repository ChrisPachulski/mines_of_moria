#Load libraries####
install.packages("Rtools")
install.packages("rvest")
install.packages("jsonlite")
install.packages("backports")
install.packages("tidyverse")
install.packages("tidyquant")
install.packages("xopen")
install.packages("knitr")
install.packages("svMisc") #- Outsourced
#Packages & Needed Functions####
library(rvest)     # HTML Hacking & Web Scraping
library(jsonlite)  # JSON manipulation
library(tidyverse) # Data Manipulation
library(tidyquant) # ggplot2 theme
library(xopen)     # Opens URL in Browser
library(knitr)     # Pretty HTML Tables
library(purrr)     # Allows for the replacement of loops and suite of apply functions
library(tibble)    # Breakdown further elements
library(dplyr)     # Data Manipulation
library(tidyr)     # The Janitor is this guy
library(tictoc)    # Time our stuff
#library(svMisc)    # Progress bar on loops to make me feel good - Outsourced
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
funk <- function(t){
  ifelse(nchar(t) <= 10, right(t,1),ifelse(nchar(t)<=190, right(t,2),ifelse(nchar(t)>=191, right((t),3),0)))
} #Character Count utilization of 'left'&'right' functions for quantity breakdown
#CK- Buylist####
tic()
All <-NULL #Create Null value to assign our scraped values to in following steps
total = 35 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
for(i in 1:35){
  url <- paste0("https://www.cardkingdom.com/purchasing/mtg_singles?filter%5Bipp%5D=1000&filter%5Bsort%5D=edition&filter%5Bsearch%5D=mtg_advanced&page=",i)
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE) # Gets around company firewall restrictions
  html <- read_html(url)  #read_html("scrapedpage.html")
  json <- html %>% html_nodes(".itemContentWrapper") %>% html_text()
  json <-data.frame(json)
  All <-rbind(All,json)
  setTxtProgressBar(pb,i)
  
} #Cardkingdom BL scraper: URL Location - HTML Elements to 'borrow':Loop results to paste under each other page results to the prior null value: Report current status to loading bar to ensure scrape has not crashed.
toc()
#CK Buylist Formatting####
test<-data.frame(do.call('rbind', strsplit(as.character(All$json),'\n',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
test<-subset(test, select = -c(X8,X9,X10,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X27,X28,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46)) #Remove the NUMEROUS Extraneous columns, we could certainly try to refine the amount being scraped here moving forward.

#test2<-as.data.frame(sapply(test, function(x) x[x != " "])) - OLD, leave me alone, but, just n case, retain.
test2 <- test$X24 %>% dplyr::na_if("") #Remove NA values in a new column (ensures we have done it right without tampering with original output)
test$X24<-test2 #After we are positive, replace the og column with cleaned version
test[]<-t(apply(test,1,function(x){
  c(x[!is.na(x)],x[is.na(x)])})) #Function to remove all NA's from the entire doc (aka I got clever and made it cleaner than above, but fun to see both ways)
test <- as.data.frame(test) #Ensure correct structure
test$X31 = funk(test$X31) #apply our quantity function to the appropriate columns
test$X32 = funk(test$X32) #due to uneven newline escapes, we must also run the quantity function across the following column as well, what i tend to name as the "aux" or "auxillary" column
test2<-data.frame(do.call('rbind', strsplit(as.character(test$X5),' (',fixed=TRUE))) 
test3<-data.frame(do.call('rbind', strsplit(as.character(test2$X2),')',fixed=TRUE))) #59&60, we are breaking apart the cardname and set formatting to recreate in our desired format
test$X4<-test2$X1
test$X5<- test3$X1
test$X6 <- test3$X2 #having reviewed the outputs of our delimiting above, reconstruct the card names, sets, and rarity above.
test$X2 <- paste(test$X3,test$X4,test$X5,test$X6," ",sep = "") #Creating THE PRIMARY KEY for all subsequent scrape joins.
test$X1 <- paste(test$X4," ", sep ="") #Create a secondary key for mtggoldfish as they serve as the baseline, in terms of all cards we could possibly track
test <- test[c(7,2,3,1,4,5,6,9,14,15)] #Restructure & Reorder the columns into how will fit into my 'honeypot' document
Price_Trim <- function(t){
  ifelse(nchar(t) <= 4, (right(t,3)),ifelse(nchar(t)<= 5, (right(t,4)),ifelse(nchar(t)<= 6, (right(t,5)),ifelse(nchar(t)<= 7, (right(t,6)),ifelse(nchar(t)<= 8, (right(t,7)),0)))))
} #The scrape returns the Price values with a '$', which prevents R from recognizing as a numeric value. Therefor, we must remove the specal character.
test$X24<-Price_Trim(test$X24) #apply the price trim function
test$X24 <- as.numeric(test$X24) #Now convertible to numeric format
test$X24 <- (test$X24 / 100)#CK scraper, for whatever reason, drops the decimal point, so we must adjust back to real dollar amounts (eg, '006' is 6 cents, not 6 dollars)
#LOAD IN "SETS" HERE OR YOUR OUTPUT WILL BE FUGLY
names(test) <- c("MTG_Gold_Key","CK_Key","Card Name", "CK_Modif_Set","Set","Rarity","NF/F","BL_Value","Qty_Des","Qty_Aux") #rename columns to be a tad easier to track
test$Gold_Merge <- "" #This is an utterly superfluous step but it's my code so... your move...
test <- as.data.frame(test) #Ensure proper formatting
#Load in Sets####
CK_BL_Scrape_Sets <- c("  3rd Edition ",
                       "  5th Edition ",
                       "  7th Edition ",
                       "  8th Edition ",
                       "  9th Edition ",
                       " 10th Edition ",
                       " 2010 Core Set ",
                       " 2011 Core Set ",
                       " 2012 Core Set ",
                       " 2013 Core Set ",
                       " 2015 Core Set ",
                       " 3rd Edition ",
                       " 4th Edition ",
                       " 5th Edition ",
                       " 6th Edition ",
                       " 7th Edition ",
                       " 8th Edition ",
                       " 9th Edition ",
                       " Aether Revolt ",
                       " Amonkhet ",
                       " Antiquities ",
                       " Apocalypse ",
                       " Archenemy ",
                       " Avacyn Restored ",
                       " Battle for Zendikar ",
                       " Battlebond ",
                       " Beta ",
                       " Betrayers of Kamigawa ",
                       " Born of the Gods ",
                       " Champions of Kamigawa ",
                       " Coldsnap ",
                       " Collectors Ed ",
                       " Collectors Ed Intl ",
                       " Commander 2013 ",
                       " Commander 2014 ",
                       " Commander 2015 ",
                       " Commander 2016 ",
                       " Commander 2017 ",
                       " Commander 2019 ",
                       " Commander's Arsenal ",
                       " Conflux ",
                       " Conspiracy - Take the Crown ",
                       " Core Set 2019 ",
                       " Core Set 2020 ",
                       " Dissension ",
                       " Dominaria ",
                       " Dragon's Maze ",
                       " Dragons of Tarkir ",
                       " Duel Decks: Anthology ",
                       " Duel Decks: Knights Vs. Dragons ",
                       " Duel Decks: Nissa Vs. Ob Nixilis ",
                       " Eventide ",
                       " Exodus ",
                       " Fate Reforged ",
                       " Fifth Dawn ",
                       " Future Sight ",
                       " Gatecrash ",
                       " Guildpact ",
                       " Guilds of Ravnica ",
                       " Ice Age ",
                       " Iconic Masters ",
                       " Innistrad ",
                       " Ixalan ",
                       " Judgment ",
                       " Kaladesh ",
                       " Khans of Tarkir ",
                       " Legends ",
                       " Legions ",
                       " Lorwyn ",
                       " Magic Origins ",
                       " Masters 25 ",
                       " Mercadian Masques ",
                       " Mirage ",
                       " Mirrodin ",
                       " Mirrodin Besieged ",
                       " Modern Event Deck ",
                       " Modern Horizons ",
                       " Modern Masters ",
                       " Modern Masters 2015 ",
                       " Modern Masters 2017 ",
                       " Morningtide ",
                       " Nemesis ",
                       " Onslaught ",
                       " Planechase ",
                       " Planechase Anthology ",
                       " Planeshift ",
                       " Portal ",
                       " Portal 3K ",
                       " Premium Deck Series: Graveborn ",
                       " Promo Pack ",
                       " Promotional ",
                       " Ravnica ",
                       " Ravnica Allegiance ",
                       " Rise of the Eldrazi ",
                       " Rivals of Ixalan ",
                       " Scars of Mirrodin ",
                       " Scourge ",
                       " Shadowmoor ",
                       " Shadows Over Innistrad ",
                       " Shards of Alara ",
                       " Signature Spellbook: Gideon ",
                       " Starter 1999 ",
                       " The Dark ",
                       " Theros ",
                       " Throne of Eldraine ",
                       " Throne of Eldraine Variants ",
                       " Time Spiral ",
                       " Timeshifted ",
                       " Torment ",
                       " Ultimate Masters ",
                       " Unglued ",
                       " Unhinged ",
                       " Unlimited ",
                       " Urza's Legacy ",
                       " Urza's Saga ",
                       " Visions ",
                       " War of the Spark ",
                       " War of the Spark JPN Planeswalkers ",
                       " Weatherlight ",
                       " World Championships ",
                       " Worldwake ",
                       " Zendikar ",
                       "10th Edition ",
                       "2010 Core Set ",
                       "2011 Core Set ",
                       "2012 Core Set ",
                       "2013 Core Set ",
                       "2014 Core Set ",
                       "2015 Core Set ",
                       "Aether Revolt ",
                       "Alara Reborn ",
                       "Alliances ",
                       "Alpha ",
                       "Amonkhet ",
                       "Anthologies ",
                       "Antiquities ",
                       "Apocalypse ",
                       "Arabian Nights ",
                       "Archenemy ",
                       "Archenemy - Nicol Bolas ",
                       "Art Series ",
                       "Avacyn Restored ",
                       "Battle for Zendikar ",
                       "Battle Royale ",
                       "Battlebond ",
                       "Beatdown ",
                       "Beta ",
                       "Betrayers of Kamigawa ",
                       "Born of the Gods ",
                       "Champions of Kamigawa ",
                       "Chronicles ",
                       "Coldsnap ",
                       "Coldsnap Theme Decks ",
                       "Collectors Ed ",
                       "Collectors Ed Intl ",
                       "Commander ",
                       "Commander 2013 ",
                       "Commander 2014 ",
                       "Commander 2015 ",
                       "Commander 2016 ",
                       "Commander 2017 ",
                       "Commander 2018 ",
                       "Commander 2019 ",
                       "Commander Anthology ",
                       "Commander Anthology Vol. II ",
                       "Commander's Arsenal ",
                       "Conflux ",
                       "Conspiracy ",
                       "Conspiracy - Take the Crown ",
                       "Core Set 2019 ",
                       "Core Set 2020 ",
                       "Dark Ascension ",
                       "Darksteel ",
                       "Deckmaster ",
                       "Dissension ",
                       "Dominaria ",
                       "Dragon's Maze ",
                       "Dragons of Tarkir ",
                       "Duel Decks: Ajani Vs. Nicol Bolas ",
                       "Duel Decks: Anthology ",
                       "Duel Decks: Blessed Vs. Cursed ",
                       "Duel Decks: Divine Vs. Demonic ",
                       "Duel Decks: Elspeth Vs. Kiora ",
                       "Duel Decks: Elspeth Vs. Tezzeret ",
                       "Duel Decks: Elves Vs. Goblins ",
                       "Duel Decks: Elves Vs. Inventors ",
                       "Duel Decks: Garruk Vs. Liliana ",
                       "Duel Decks: Heroes Vs. Monsters ",
                       "Duel Decks: Izzet Vs. Golgari ",
                       "Duel Decks: Jace Vs. Chandra ",
                       "Duel Decks: Jace Vs. Vraska ",
                       "Duel Decks: Knights Vs. Dragons ",
                       "Duel Decks: Merfolk Vs. Goblins ",
                       "Duel Decks: Mind Vs. Might ",
                       "Duel Decks: Nissa Vs. Ob Nixilis ",
                       "Duel Decks: Phyrexia Vs. The Coalition ",
                       "Duel Decks: Sorin Vs. Tibalt ",
                       "Duel Decks: Speed Vs. Cunning ",
                       "Duel Decks: Venser Vs. Koth ",
                       "Duel Decks: Zendikar Vs. Eldrazi ",
                       "Duels of the Planeswalkers ",
                       "Eldritch Moon ",
                       "Eternal Masters ",
                       "Eventide ",
                       "Exodus ",
                       "Explorers of Ixalan ",
                       "Fallen Empires ",
                       "Fate Reforged ",
                       "Fifth Dawn ",
                       "From the Vault: Angels ",
                       "From the Vault: Annihilation ",
                       "From the Vault: Dragons ",
                       "From the Vault: Exiled ",
                       "From the Vault: Legends ",
                       "From the Vault: Lore ",
                       "From the Vault: Realms ",
                       "From the Vault: Relics ",
                       "From the Vault: Transform ",
                       "From the Vault: Twenty ",
                       "Future Sight ",
                       "Game Night ",
                       "Gatecrash ",
                       "Global Series: Jiang Yanggu & Mu Yanling ",
                       "Guildpact ",
                       "Guilds of Ravnica ",
                       "Guilds of Ravnica: Guild Kits ",
                       "Homelands ",
                       "Hour of Devastation ",
                       "Ice Age ",
                       "Iconic Masters ",
                       "Innistrad ",
                       "Invasion ",
                       "Ixalan ",
                       "Journey into Nyx ",
                       "Judgment ",
                       "Kaladesh ",
                       "Khans of Tarkir ",
                       "Legends ",
                       "Legions ",
                       "Lorwyn ",
                       "Magic Origins ",
                       "Masterpiece Series: Expeditions ",
                       "Masterpiece Series: Inventions ",
                       "Masterpiece Series: Invocations ",
                       "Masterpiece Series: Mythic Edition ",
                       "Masters 25 ",
                       "Mercadian Masques ",
                       "Mirage ",
                       "Mirrodin ",
                       "Mirrodin Besieged ",
                       "Modern Event Deck ",
                       "Modern Horizons ",
                       "Modern Masters ",
                       "Modern Masters 2015 ",
                       "Modern Masters 2017 ",
                       "Morningtide ",
                       "Nemesis ",
                       "New Phyrexia ",
                       "Oath of the Gatewatch ",
                       "Odyssey ",
                       "Onslaught ",
                       "Planar Chaos ",
                       "Planechase ",
                       "Planechase 2012 ",
                       "Planechase Anthology ",
                       "Planeshift ",
                       "Portal ",
                       "Portal 3K ",
                       "Portal II ",
                       "Premium Deck Series: Fire & Lightning ",
                       "Premium Deck Series: Graveborn ",
                       "Premium Deck Series: Slivers ",
                       "Promo Pack ",
                       "Promotional ",
                       "Prophecy ",
                       "Ravnica ",
                       "Ravnica Allegiance ",
                       "Ravnica Allegiance: Guild Kits ",
                       "Return to Ravnica ",
                       "Rise of the Eldrazi ",
                       "Rivals of Ixalan ",
                       "Saviors of Kamigawa ",
                       "Scars of Mirrodin ",
                       "Scourge ",
                       "Shadowmoor ",
                       "Shadows Over Innistrad ",
                       "Shards of Alara ",
                       "Signature Spellbook: Gideon ",
                       "Signature Spellbook: Jace ",
                       "Starter 1999 ",
                       "Starter 2000 ",
                       "Stronghold ",
                       "Tempest ",
                       "The Dark ",
                       "Theros ",
                       "Throne of Eldraine ",
                       "Throne of Eldraine Variants ",
                       "Time Spiral ",
                       "Timeshifted ",
                       "Torment ",
                       "Ultimate Box Topper ",
                       "Ultimate Masters ",
                       "Unglued ",
                       "Unhinged ",
                       "Unlimited ",
                       "Unstable ",
                       "Urza's Destiny ",
                       "Urza's Legacy ",
                       "Urza's Saga ",
                       "Vanguard ",
                       "Visions ",
                       "War of the Spark ",
                       "War of the Spark JPN Planeswalkers ",
                       "Weatherlight ",
                       "World Championships ",
                       "Worldwake ",
                       "Zendikar ")
MTG_Goldfish_Sets <- c("Revised Edition",
                       "Fifth Edition",
                       "Seventh Edition",
                       "Eighth Edition",
                       "Ninth Edition",
                       "Tenth Edition",
                       "Magic 2010",
                       "Magic 2011",
                       "Magic 2012",
                       "Magic 2013",
                       "Magic 2015 Core Set",
                       "Revised Edition",
                       "Fourth Edition",
                       "Fifth Edition",
                       "Classic Sixth Edition",
                       "Seventh Edition",
                       "Eighth Edition",
                       "Ninth Edition",
                       "Aether Revolt",
                       "Amonkhet",
                       "Antiquities",
                       "Apocalypse",
                       "Archenemy",
                       "Avacyn Restored",
                       "Battle for Zendikar",
                       "Battlebond",
                       "Limited Edition Beta",
                       "Betrayers of Kamigawa",
                       "Born of the Gods",
                       "Champions of Kamigawa",
                       "Coldsnap",
                       "",
                       "",
                       "Commander 2013 Edition",
                       "Commander 2014",
                       "Commander 2015",
                       "Commander 2016",
                       "Commander 2017",
                       "Commander 2019",
                       "Commander's Arsenal",
                       "Conflux",
                       "Conspiracy: Take the Crown",
                       "Core Set 2019",
                       "Core Set 2020",
                       "Dissension",
                       "Dominaria",
                       "Dragon's Maze",
                       "Dragons of Tarkir",
                       "Duel Decks Anthology",
                       "Duel Decks: Knights vs. Dragons",
                       "Duel Decks: Nissa vs. Ob Nixilis",
                       "Eventide",
                       "Exodus",
                       "Fate Reforged",
                       "Fifth Dawn",
                       "Future Sight",
                       "Gatecrash",
                       "Guildpact",
                       "Guilds of Ravnica",
                       "Ice Age",
                       "Iconic Masters",
                       "Innistrad",
                       "Ixalan",
                       "Judgment",
                       "Kaladesh",
                       "Khans of Tarkir",
                       "Legends",
                       "Legions",
                       "Lorwyn",
                       "Magic Origins",
                       "Masters 25",
                       "Mercadian Masques",
                       "Mirage",
                       "Mirrodin",
                       "Mirrodin Besieged",
                       "Modern Event Deck 2014",
                       "Modern Horizons",
                       "Modern Masters",
                       "Modern Masters 2015 Edition",
                       "Modern Masters 2017 Edition",
                       "Morningtide",
                       "Nemesis",
                       "Onslaught",
                       "Planechase",
                       "Planechase Anthology",
                       "Planeshift",
                       "Portal",
                       "Portal Three Kingdoms",
                       "Premium Deck Series: Graveborn",
                       "Promo Pack: Throne of Eldraine",
                       "Promotional",
                       "Ravnica: City of Guilds",
                       "Ravnica Allegiance",
                       "Rise of the Eldrazi",
                       "Rivals of Ixalan",
                       "Scars of Mirrodin",
                       "Scourge",
                       "Shadowmoor",
                       "Shadows over Innistrad",
                       "Shards of Alara",
                       "Signature Spellbook: Gideon",
                       "Starter 1999",
                       "The Dark",
                       "Theros",
                       "Throne of Eldraine",
                       "Throne of Eldraine Promos",
                       "Time Spiral",
                       'Time Spiral "Timeshifted"',
                       "Torment",
                       "Ultimate Masters",
                       "Unglued",
                       "Unhinged",
                       "Unlimited Edition",
                       "Urza's Legacy",
                       "Urza's Saga",
                       "Visions",
                       "War of the Spark",
                       "War of the Spark",
                       "Weatherlight",
                       "",
                       "Worldwake",
                       "Zendikar",
                       "Tenth Edition",
                       "Magic 2010",
                       "Magic 2011",
                       "Magic 2012",
                       "Magic 2013",
                       "Magic 2014 Core Set",
                       "Magic 2015 Core Set",
                       "Aether Revolt",
                       "Alara Reborn",
                       "Alliances",
                       "Limited Edition Alpha",
                       "Amonkhet",
                       "Anthologies",
                       "Antiquities",
                       "Apocalypse",
                       "Arabian Nights",
                       "Archenemy",
                       "Archenemy: Nicol Bolas",
                       "",
                       "Avacyn Restored",
                       "Battle for Zendikar",
                       "Battle Royale Box Set",
                       "Battlebond",
                       "Beatdown Box Set",
                       "Limited Edition Beta",
                       "Betrayers of Kamigawa",
                       "Born of the Gods",
                       "Champions of Kamigawa",
                       "Chronicles",
                       "Coldsnap",
                       "Coldsnap Theme Deck Reprints",
                       "",
                       "",
                       "Magic: The Gathering-Commander",
                       "Commander 2013 Edition",
                       "Commander 2014",
                       "Commander 2015",
                       "Commander 2016",
                       "Commander 2017",
                       "Commander 2018",
                       "Commander 2019",
                       "Commander Anthology",
                       "Commander Anthology 2018",
                       "Commander's Arsenal",
                       "Conflux",
                       "Magic: The Gathering—Conspiracy",
                       "Conspiracy: Take the Crown",
                       "Core Set 2019",
                       "Core Set 2020",
                       "Dark Ascension",
                       "Darksteel",
                       "Deckmasters: Garfield vs. Finkel",
                       "Dissension",
                       "Dominaria",
                       "Dragon's Maze",
                       "Dragons of Tarkir",
                       "Duel Decks: Ajani vs. Nicol Bolas",
                       "Commander Anthology",
                       "Duel Decks: Blessed vs. Cursed",
                       "Duel Decks: Divine vs. Demonic",
                       "Duel Decks: Elspeth vs. Kiora",
                       "Duel Decks: Elspeth vs. Tezzeret",
                       "Duel Decks: Elves vs. Goblins",
                       "Duel Decks: Elves vs. Inventors",
                       "Duel Decks: Garruk vs. Liliana",
                       "Duel Decks: Heroes vs. Monsters",
                       "Duel Decks: Izzet vs. Golgari",
                       "Duel Decks: Jace vs. Chandra",
                       "Duel Decks: Jace vs. Vraska",
                       "Duel Decks: Knights vs. Dragons",
                       "Duel Decks: Merfolk vs. Goblins",
                       "Duel Decks: Mind vs. Might",
                       "Duel Decks: Nissa vs. Ob Nixilis",
                       "Duel Decks: Phyrexia vs. the Coalition",
                       "Duel Decks: Sorin vs. Tibalt",
                       "Duel Decks: Speed vs. Cunning",
                       "Duel Decks: Venser vs. Koth",
                       "Duel Decks: Zendikar vs. Eldrazi",
                       "Duels of the Planeswalkers",
                       "Eldritch Moon",
                       "Eternal Masters",
                       "Eventide",
                       "Exodus",
                       "Explorers of Ixalan",
                       "Fallen Empires",
                       "Fate Reforged",
                       "Fifth Dawn",
                       "From the Vault: Angels",
                       "From the Vault: Annihilation",
                       "From the Vault: Dragons",
                       "From the Vault: Exiled",
                       "From the Vault: Legends",
                       "From the Vault: Lore",
                       "From the Vault: Realms",
                       "From the Vault: Relics",
                       "From the Vault: Transform",
                       "From the Vault: Twenty",
                       "Future Sight",
                       "Game Night",
                       "Gatecrash",
                       "Global Series",
                       "Guildpact",
                       "Guilds of Ravnica",
                       "Guilds of Ravnica Guild Kits",
                       "Homelands",
                       "Hour of Devastation",
                       "Ice Age",
                       "Iconic Masters",
                       "Innistrad",
                       "Invasion",
                       "Ixalan",
                       "Journey into Nyx",
                       "Judgment",
                       "Kaladesh",
                       "Khans of Tarkir",
                       "Legends",
                       "Legions",
                       "Lorwyn",
                       "Magic Origins",
                       "Zendikar Expeditions",
                       "Masterpiece Series: Kaladesh Inventions",
                       "Masterpiece Series: Amonkhet Invocations",
                       "",
                       "Masters 25",
                       "Mercadian Masques",
                       "Mirage",
                       "Mirrodin",
                       "Mirrodin Besieged",
                       "Modern Event Deck 2014",
                       "Modern Horizons",
                       "Modern Masters",
                       "Modern Masters 2015 Edition",
                       "Modern Masters 2017 Edition",
                       "Morningtide",
                       "Nemesis",
                       "New Phyrexia",
                       "Oath of the Gatewatch",
                       "Odyssey",
                       "Onslaught",
                       "Planar Chaos",
                       "Planechase",
                       "Planechase 2012 Edition",
                       "Planechase Anthology",
                       "Planeshift",
                       "Portal",
                       "Portal Three Kingdoms",
                       "Portal Second Age",
                       "Premium Deck Series: Fire and Lightning",
                       "Premium Deck Series: Graveborn",
                       "Premium Deck Series: Slivers",
                       "Promo Pack: Throne of Eldraine",
                       "Promotional",
                       "Prophecy",
                       "Ravnica: City of Guilds",
                       "Ravnica Allegiance",
                       "Ravnica Allegiance Guild Kits",
                       "Return to Ravnica",
                       "Rise of the Eldrazi",
                       "Rivals of Ixalan",
                       "Saviors of Kamigawa",
                       "Scars of Mirrodin",
                       "Scourge",
                       "Shadowmoor",
                       "Shadows over Innistrad",
                       "Shards of Alara",
                       "Signature Spellbook: Gideon",
                       "Signature Spellbook: Jace",
                       "Starter 1999",
                       "Starter 2000",
                       "Stronghold",
                       "Tempest",
                       "The Dark",
                       "Theros",
                       "Throne of Eldraine",
                       "Throne of Eldraine Promos",
                       "Time Spiral",
                       'Time Spiral "Timeshifted"',
                       "Torment",
                       "Ultimate Box Topper",
                       "Ultimate Masters",
                       "Unglued",
                       "Unhinged",
                       "Unlimited Edition",
                       "Unstable",
                       "Urza's Destiny",
                       "Urza's Legacy",
                       "Urza's Saga",
                       "Vanguard",
                       "Visions",
                       "War of the Spark",
                       "War of the Spark",
                       "Weatherlight",
                       "",
                       "Worldwake",
                       "Zendikar")
GF_Abbr <- c("3ED",
             "5ED",
             "7",
             "8ED",
             "9ED",
             "10",
             "M10",
             "M11",
             "M12",
             "M13",
             "M15",
             "3ED",
             "4ED",
             "5ED",
             "6ED",
             "7",
             "8ED",
             "9ED",
             "AER",
             "AKH",
             "ATQ",
             "AP",
             "ARC",
             "AVR",
             "BFZ",
             "BBD",
             "LEB",
             "BOK",
             "BNG",
             "CHK",
             "CSP",
             "",
             "",
             "C13",
             "C14",
             "C15",
             "C16",
             "C17",
             "C19",
             "CM1",
             "CON",
             "CN2",
             "M19",
             "M20",
             "DIS",
             "DOM",
             "DGM",
             "DTK",
             "DD3",
             "DDG",
             "DDR",
             "EVE",
             "EX",
             "FRF",
             "5DN",
             "FUT",
             "GTC",
             "GPT",
             "GRN",
             "ICE",
             "IMA",
             "ISD",
             "XLN",
             "JUD",
             "KLD",
             "KTK",
             "LEG",
             "LGN",
             "LRW",
             "ORI",
             "A25",
             "MM",
             "MI",
             "MRD",
             "MBS",
             "MD1",
             "MH1",
             "MMA",
             "MM2",
             "MM3",
             "MOR",
             "NE",
             "ONS",
             "PC1",
             "PCA",
             "PS",
             "POR",
             "PTK",
             "PD3",
             "PPELD",
             "PRM",
             "RAV",
             "RNA",
             "ROE",
             "RIX",
             "SOM",
             "SCG",
             "SHM",
             "SOI",
             "ALA",
             "SS2",
             "S99",
             "DRK",
             "THS",
             "ELD",
             "PELD",
             "TSP",
             "TSB",
             "TOR",
             "UMA",
             "UGL",
             "UNH",
             "2ED",
             "UL",
             "UZ",
             "VI",
             "WAR",
             "WAR",
             "WL",
             "",
             "WWK",
             "ZEN",
             "10",
             "M10",
             "M11",
             "M12",
             "M13",
             "M14",
             "M15",
             "AER",
             "ARB",
             "ALL",
             "LEA",
             "AKH",
             "ATH",
             "ATQ",
             "AP",
             "ARN",
             "ARC",
             "E01",
             "",
             "AVR",
             "BFZ",
             "BRB",
             "BBD",
             "BTD",
             "LEB",
             "BOK",
             "BNG",
             "CHK",
             "CHR",
             "CSP",
             "CST",
             "",
             "",
             "CMD",
             "C13",
             "C14",
             "C15",
             "C16",
             "C17",
             "C18",
             "C19",
             "CMA",
             "CM2",
             "CM1",
             "CON",
             "CNS",
             "CN2",
             "M19",
             "M20",
             "DKA",
             "DST",
             "DKM",
             "DIS",
             "DOM",
             "DGM",
             "DTK",
             "DDH",
             "CMA",
             "DDQ",
             "DDC",
             "DDO",
             "DDF",
             "EVG",
             "DDU",
             "DDD",
             "DDL",
             "DDJ",
             "DD2",
             "DDM",
             "DDG",
             "DDT",
             "DDS",
             "DDR",
             "DDE",
             "DDK",
             "DDN",
             "DDI",
             "DDP",
             "DPA",
             "EMN",
             "EMA",
             "EVE",
             "EX",
             "E02",
             "FEM",
             "FRF",
             "5DN",
             "V15",
             "V14",
             "DRB",
             "V09",
             "V11",
             "V16",
             "V12",
             "V10",
             "V17",
             "V13",
             "FUT",
             "GNT",
             "GTC",
             "GS1",
             "GPT",
             "GRN",
             "GK1",
             "HML",
             "HOU",
             "ICE",
             "IMA",
             "ISD",
             "IN",
             "XLN",
             "JOU",
             "JUD",
             "KLD",
             "KTK",
             "LEG",
             "LGN",
             "LRW",
             "ORI",
             "EXP",
             "MS2",
             "MS3",
             "",
             "A25",
             "MM",
             "MI",
             "MRD",
             "MBS",
             "MD1",
             "MH1",
             "MMA",
             "MM2",
             "MM3",
             "MOR",
             "NE",
             "NPH",
             "OGW",
             "OD",
             "ONS",
             "PLC",
             "PC1",
             "PC2",
             "PCA",
             "PS",
             "POR",
             "PTK",
             "PO2",
             "PD2",
             "PD3",
             "H09",
             "PPELD",
             "PRM",
             "PR",
             "RAV",
             "RNA",
             "GK2",
             "RTR",
             "ROE",
             "RIX",
             "SOK",
             "SOM",
             "SCG",
             "SHM",
             "SOI",
             "ALA",
             "SS2",
             "SS1",
             "S99",
             "S00",
             "ST",
             "TE",
             "DRK",
             "THS",
             "ELD",
             "PELD",
             "TSP",
             "TSB",
             "TOR",
             "PRM-UMA",
             "UMA",
             "UGL",
             "UNH",
             "2ED",
             "UST",
             "UD",
             "UL",
             "UZ",
             "VAN",
             "VI",
             "WAR",
             "WAR",
             "WL",
             "",
             "WWK",
             "ZEN")
TCG_Key <- c("Revised Edition",
             "5th Edition",
             "7th Edition",
             "8th Edition",
             "9th Edition",
             "10th Edition",
             "Magic 2010 (M10)",
             "Magic 2011 (M11)",
             "Magic 2012 (M12)",
             "Magic 2013 (M13)",
             "Magic 2015 (M15)",
             "Revised Edition",
             "4th Edition",
             "5th Edition",
             "6th Edition",
             "7th Edition",
             "8th Edition",
             "9th Edition",
             "Aether Revolt",
             "Amonkhet",
             "Antiquities",
             "Apocalypse",
             "Archenemy",
             "Avacyn Restored",
             "Battle for Zendikar",
             "Battlebond",
             "",
             "Betrayers of Kamigawa",
             "Born of the Gods",
             "Champions of Kamigawa",
             "Coldsnap",
             "",
             "",
             "Commander 2013",
             "Commander 2014",
             "Commander 2015",
             "Commander 2016",
             "Commander 2017",
             "Commander 2019",
             "",
             "Conflux",
             "Conspiracy: Take the Crown",
             "Core Set 2019",
             "Core Set 2020",
             "Dissension",
             "Dominaria",
             "Dragon's Maze",
             "Dragons of Tarkir",
             "Duel Decks: Anthology",
             "Duel Decks: Knights vs. Dragons",
             "Duel Decks: Nissa vs. Ob Nixilis",
             "Eventide",
             "Exodus",
             "Fate Reforged",
             "Fifth Dawn",
             "Future Sight",
             "Gatecrash",
             "Guildpact",
             "Guilds of Ravnica",
             "Ice Age",
             "Iconic Masters",
             "Innistrad",
             "Ixalan",
             "Judgment",
             "Kaladesh",
             "Khans of Tarkir",
             "Legends",
             "Legions",
             "Lorwyn",
             "Magic Origins",
             "Masters 25",
             "Mercadian Masques",
             "Mirage",
             "Mirrodin",
             "Mirrodin Besieged",
             "Magic Modern Event Deck",
             "Modern Horizons",
             "Modern Masters",
             "Modern Masters 2015",
             "Modern Masters 2017",
             "Morningtide",
             "Nemesis",
             "Onslaught",
             "Planechase 2012",
             "Planechase Anthology",
             "Shards of Alara",
             "Guilds of Ravnica: Guild Kits",
             "Portal Three Kingdoms",
             "Premium Deck Series: Graveborn",
             "Promo Pack: Throne of Eldraine",
             "",
             "Ravnica: City of Guilds",
             "Ravnica Allegiance",
             "Rise of the Eldrazi",
             "Rivals of Ixalan",
             "Scars of Mirrodin",
             "Scourge",
             "Shadowmoor",
             "Shadows over Innistrad",
             "Shards of Alara",
             "Signature Spellbook: Gideon",
             "",
             "The Dark",
             "Theros",
             "Throne of Eldraine",
             "",
             "Time Spiral",
             "Timeshifted",
             "Torment",
             "Ultimate Masters",
             "Unglued",
             "Unhinged",
             "Unlimited Edition",
             "Urza's Legacy",
             "Urza's Saga",
             "Visions",
             "War of the Spark",
             "War of the Spark",
             "Weatherlight",
             "",
             "Worldwake",
             "Zendikar",
             "10th Edition",
             "Magic 2010 (M10)",
             "Magic 2011 (M11)",
             "Magic 2012 (M12)",
             "Magic 2013 (M13)",
             "Magic 2014 (M14)",
             "Magic 2015 (M15)",
             "Aether Revolt",
             "Alara Reborn",
             "Alliances",
             "",
             "Amonkhet",
             "Anthologies",
             "Antiquities",
             "Apocalypse",
             "Arabian Nights",
             "Archenemy",
             "Archenemy: Nicol Bolas",
             "",
             "Avacyn Restored",
             "Battle for Zendikar",
             "",
             "Battlebond",
             "",
             "",
             "Betrayers of Kamigawa",
             "Born of the Gods",
             "Champions of Kamigawa",
             "Chronicles",
             "Coldsnap",
             "Coldsnap Theme Deck Reprints",
             "",
             "",
             "Commander",
             "Commander 2013",
             "Commander 2014",
             "Commander 2015",
             "Commander 2016",
             "Commander 2017",
             "Commander 2018",
             "Commander 2019",
             "Commander Anthology",
             "Commander Anthology Volume II",
             "",
             "Conflux",
             "Conspiracy",
             "Conspiracy: Take the Crown",
             "Core Set 2019",
             "Core Set 2020",
             "Dark Ascension",
             "Darksteel",
             "",
             "Dissension",
             "Dominaria",
             "Dragon's Maze",
             "Dragons of Tarkir",
             "Duel Decks: Ajani vs. Nicol Bolas",
             "Duel Decks: Anthology",
             "Duel Decks: Blessed vs. Cursed",
             "",
             "Duel Decks: Elspeth vs. Kiora",
             "Duel Decks: Elspeth vs. Tezzeret",
             "Duel Decks: Merfolk vs. Goblins",
             "Duel Decks: Elves vs. Inventors",
             "Duel Decks: Garruk vs. Liliana",
             "Duel Decks: Heroes vs. Monsters",
             "Duel Decks: Izzet vs. Golgari",
             "Duel Decks: Jace vs. Chandra",
             "Duel Decks: Jace vs. Vraska",
             "Duel Decks: Knights vs. Dragons",
             "Duel Decks: Merfolk vs. Goblins",
             "Duel Decks: Mind vs. Might",
             "Duel Decks: Nissa vs. Ob Nixilis",
             "Duel Decks: Phyrexia Vs. The Coalition ",
             "Duel Decks: Sorin vs. Tibalt",
             "Duel Decks: Speed vs. Cunning",
             "Duel Decks: Venser vs. Koth",
             "Duel Decks: Zendikar vs. Eldrazi",
             "",
             "Eldritch Moon",
             "Eternal Masters",
             "Eventide",
             "Exodus",
             "Explorers of Ixalan",
             "Fallen Empires",
             "Fate Reforged",
             "Fifth Dawn",
             "From the Vault: Angels ",
             "From the Vault: Annihilation",
             "From the Vault: Dragons ",
             "From the Vault: Exiled ",
             "From the Vault: Legends ",
             "From the Vault: Lore",
             "From the Vault: Realms",
             "From the Vault: Relics ",
             "From the Vault: Transform",
             "From the Vault: Twenty ",
             "Future Sight",
             "Magic Game Night",
             "Gatecrash",
             "Global Series Jiang Yanggu & Mu Yanling",
             "Guildpact",
             "Guilds of Ravnica",
             "Guilds of Ravnica: Guild Kits",
             "Homelands",
             "Hour of Devastation",
             "Ice Age",
             "Iconic Masters",
             "Innistrad",
             "Invasion",
             "Ixalan",
             "Journey Into Nyx",
             "Judgment",
             "Kaladesh",
             "Khans of Tarkir",
             "Legends",
             "Legions",
             "Lorwyn",
             "Magic Origins",
             "Masterpiece Series: Expeditions ",
             "Masterpiece Series: Kaladesh Inventions",
             "Masterpiece Series: Invocations ",
             "Masterpiece Series: Mythic Edition ",
             "Masters 25",
             "Mercadian Masques",
             "Mirage",
             "Mirrodin",
             "Mirrodin Besieged",
             "Magic Modern Event Deck",
             "Modern Horizons",
             "Modern Masters",
             "Modern Masters 2015",
             "Modern Masters 2017",
             "Morningtide",
             "Nemesis",
             "New Phyrexia",
             "Oath of the Gatewatch",
             "Odyssey",
             "Onslaught",
             "Planar Chaos",
             "Planechase",
             "Planechase 2012",
             "Planechase Anthology",
             "Planeshift",
             "Portal",
             "Portal Three Kingdoms",
             "",
             "Premium Deck Series: Fire and Lightning",
             "Premium Deck Series: Graveborn",
             "Premium Deck Series: Slivers",
             "Promo Pack: Throne of Eldraine",
             "",
             "Prophecy",
             "Ravnica: City of Guilds",
             "Ravnica Allegiance",
             "Ravnica Allegiance: Guild Kits",
             "Return to Ravnica",
             "Rise of the Eldrazi",
             "Rivals of Ixalan",
             "Saviors of Kamigawa",
             "Scars of Mirrodin",
             "Scourge",
             "Shadowmoor",
             "Shadows over Innistrad",
             "Shards of Alara",
             "Signature Spellbook: Gideon",
             "Signature Spellbook: Jace",
             "",
             "",
             "Stronghold",
             "Tempest",
             "The Dark",
             "Theros",
             "Throne of Eldraine",
             "",
             "Time Spiral",
             "Timeshifted",
             "Torment",
             "",
             "Ultimate Masters",
             "Unglued",
             "Unhinged",
             "Unlimited Edition",
             "Unstable",
             "Urza's Destiny",
             "Urza's Legacy",
             "Urza's Saga",
             "Vanguard",
             "Visions",
             "War of the Spark",
             "",
             "Weatherlight",
             "",
             "Weatherlight",
             "Zendikar")
Sets <- data.frame(CK_BL_Scrape_Sets,MTG_Goldfish_Sets,GF_Abbr,TCG_Key,stringsAsFactors = TRUE)
#Sets Added####
names(Sets) <- c("CK_Modif_Set","Goldfish_Full","Goldfish_Abbrev","TCG_Key") #rename columns to identify sets by their website of origin.
test$Gold_Merge <- Sets$Goldfish_Abbrev[match(test$CK_Modif_Set,Sets$CK_Modif_Set)] #Merge goldfish abbreviated sets against cardkingdom scrape values for sets
test$MTG_Gold_Key <- paste(test$`Card Name`,test$Gold_Merge, sep ="") #create the secondary key for goldfish
CK_BL_Output <- test
#Goldfish Market Scrape####
#Create a list of all the sites for MTG Goldfish to retrieve the market values from
sets = list("10E","1E","2ED","3ED","4ED","5DN","5ED","6ED","7E","8ED","9ED","A25","AER","AKH","ALA","ALL","AP","ARB","ARC","ARN","ATQ","AVR","BBD","BFZ","BNG","BOK","BOOSTER","BRB","BTD","C13","C14","C15","C16","C17","C18","C19","CHK","CHR","CM1","CM2","CMA","CMD","CN2","CNS","CON","CSP","DD2","DDC","DDD","DDE","DDF","DDG","DDH","DDI","DDJ","DDK","DDL","DDM","DDN","DDO","DDP","DDQ","DDR","DDS","DDT","DDU","DGM","DIS","DKA","DOM","DRB","DRK","DST","DTK","E01","E02","ELD","EMA","EMN","EVE","EVG","EX","EXP","FEM","FRF","FUT","GK1","GK2","GNT","GPT","GRN","GTC","H09","HML","HOU","ICE","IMA","ISD","JOU","JUD","KLD","KTK","LEA","LEB","LEG","LGN","LRW","M10","M11","M12","M13","M14","M15","M19","M20","MBS","MD1","ME2","ME3","ME4","MED","MED-GRN","MED-RNA","MED-WAR","MH1","MI","MM","MM2","MM3","MMA","MOR","MRD","MS2","MS3","NE","NPH","OD","OGW","ONS","ORI","PC1","PC2","PCA","PD2","PD3","PELD","PLC","PO2","POR","PR","PRM","PRM-ARN","PRM-BAB","PRM-CHP","PRM-FNM","PRM-GBP","PRM-GDP","PRM-GPP","PRM-GUR","PRM-GWP","PRM-HRO","PRM-JSS","PRM-JUD","PRM-LEP","PRM-LPC","PRM-MED","PRM-MPR","PRM-MSC","PRM-OHP","PRM-PRE","PRM-PTP","PRM-REL","PRM-SDCC13","PRM-SDCC14","PRM-SDCC15","PRM-SDCC16","PRM-SDCC17","PRM-SDCC18","PRM-SDCC19","PRM-SPO","PRM-SSP","PRM-UGF","PRM-UMA","PRM-WMCQ","PRM-WPN","PS","PTG","PTK","PZ1","PZ2","RAV","RIX","RNA","ROE","RTR","S00","S99","SCG","SHM","SOI","SOK","SOM","SS1","SS2","ST","TD0","TD2","TE","THS","TOR","TPR","TSB","TSP","UD","UGL","UL","UMA","UNH","UST","UZ","V09","V10","V11","V12","V13","V14","V15","V16","V17","VAN","VI","VMA","WAR","WL","WWK","XLN","ZEN")
total = 237 #238 sets on the list above, take my word for it ;)
pb <- txtProgressBar(min=0, max = total, style = 3) # loading bar formatting
Gold_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
tic()
for(i in sets){
  
  url <- paste0("https://www.mtggoldfish.com/index/",print(i),"#paper")
  html <- read_html(url)
  tbls_ls <- html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls <-data.frame(tbls_ls)
  Gold_Market <-rbind(Gold_Market,tbls_ls)
  setTxtProgressBar(pb,i)
}
toc()
#Goldfish Market Formatting####
Back_Up_Gold <- Gold_Market
Gold_Market <- Gold_Market[c(5,1,2,4)] #Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Gold_Market$Price <- as.numeric(Gold_Market$Price) #Convert to numeric from characters
Gold_Market$Price <- round(Gold_Market$Price * 0.845,2) #ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg goldfish but you also kinda suck here.
Gold_Market$Daily <- paste(Gold_Market$Card,Gold_Market$Set, sep="") #Create secondary goldfish key to act as merger column
test$Gold_Market = Gold_Market$Price[match(test$MTG_Gold_Key,Gold_Market$Daily)] #merge our CK buylist values with the Goldfish market value
test <- test[c(1,2,8,9,10,12,3,4,5,6,7,11)] #Rearrange the columns once more for the honey pot
test$CK_Key = substr(test$CK_Key,1,nchar(test$CK_Key)-1) #Noticed here that my CK key had an extra space at the end that was messing with final export. This line should belong up in CK formatting but since this is where the discovery is most pertinet for followup steps, I've left it here
Basic_Market_Review <- test
Basic_Market_Review <- Basic_Market_Review[c(2,12,8,9,6,5,11,3)]
Basic_Market_Review$Gold_Market[is.na(Basic_Market_Review$Gold_Market)] <- 0
names(Basic_Market_Review) = c("Key","Card","Set","Rarity","F/NF","MKT_Est","BL","BL Qty")
#CK- Bestsellers####
#https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=2
CK_Store_Front <-NULL #Assign NULL value
CK_Prices <- NULL
total = 683 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
tic()
for(i in 1:683){
  
  
  url <- paste0("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  html <- read_html("scrapedpage.html")
  json <- html %>% html_nodes(".itemContentWrapper") %>% html_text()
  prices <- html %>% html_nodes(".stylePrice") %>% html_text()
  #html_nodes(".itemContentWrapper")#
  #Sys.sleep(sample(3:17,1))
  json <-data.frame(json)
  prices <-data.frame(prices)
  CK_Store_Front <-rbind(CK_Store_Front,json)
  CK_Prices <- rbind(CK_Prices, prices)
  setTxtProgressBar(pb,i)
  
}
toc()
#CK Market Formatting####
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

#TCG- Market####
TCG <-NULL
Vendor = NULL
total = 1000 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
tic()
for(i in 1:1000){
  
  
  url <- paste0("https://shop.tcgplayer.com/magic/product/show?Type=Cards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards%2cCards&newSearch=false&orientation=list&PageNumber=",i)
  html <- read_html(url)
  json <- html %>% html_nodes(".product__details") %>% html_text()
  json <-data.frame(json)
  TCG <-rbind(TCG,json)
  bit <- html %>% html_nodes(".product__offers-more-count") %>% html_text()
  bit <-data.frame(bit)
  bit <- bit[- grep("photos", bit$bit),]
  bit_1 <- data.frame(do.call('rbind', strsplit(as.character(bit),' price',fixed=TRUE)))
  bit <- bit_1$X1
  bit <- data.frame(bit)
  bit_info <- html %>% html_nodes(".product__name") %>% html_text()
  bit_info <- data.frame(bit_info)
  bit_info <- as.data.frame(bit_info)
  bit <- as.data.frame(bit)
  bit_set <- html %>% html_nodes(".product__group") %>% html_text()
  bit_set <- data.frame(do.call('rbind', strsplit(as.character(bit_set),' (',fixed=TRUE)))
  bit_set <- bit_set$X1
  bit_rare <-  html %>% html_nodes(".product__extended-field") %>% html_text()
  bit_rare <- as.data.frame(bit_rare)
  bit_rare <- bit_rare[- grep("Number", bit_rare$bit_rare),]
  bit_rare <- as.data.frame(bit_rare)
  bit_rare <- data.frame(do.call('rbind', strsplit(as.character(bit_rare$bit_rare),'Rarity ',fixed=TRUE)))
  bit_rare <- bit_rare$X2
  bit_price <- html %>% html_nodes(".product__prices") %>% html_text()
  bit_price <- as.data.frame(bit_price)
  bit_price <- data.frame(do.call('rbind', strsplit(as.character(bit_price$bit_price),'\r\n',fixed=TRUE)))
  bit_price <- data.frame(do.call('rbind', strsplit(as.character(bit_price$X3),'$',fixed=TRUE)))
  bit_price <- bit_price$X2
  United <- data.frame(X1 = bit_info,
                       X2 = bit_set,
                       X3 = bit_rare,
                       x4 = bit_price,
                       X5 = bit)
  Vendor <-rbind(Vendor,United)
  
  setTxtProgressBar(pb,i)
  
}
toc()
#TCG Initial Formatting#### 
TCG_Final<-data.frame(do.call('rbind', strsplit(as.character(TCG$json),'\n',fixed=TRUE)))
#TCG_Levels<-data.frame(do.call('rbind', strsplit(as.character(TCG_Stock_Levels),'\n',fixed=TRUE)))
TCG_Final1 <-subset(TCG_Final, select = -c(X6))
TCG_Final1 = as.data.frame(apply(TCG_Final1,2,function(x)gsub('\\s+', '',x)))
Delimit_1<-data.frame(do.call('rbind', strsplit(as.character(TCG_Final$X6),'  ',fixed=TRUE)))
Delimit_2<-data.frame(do.call('rbind', strsplit(as.character(TCG_Final$X7),'(Magic)',fixed=TRUE)))
Delimit_2<-data.frame(do.call('rbind', strsplit(as.character(Delimit_2$X1),'  ',fixed=TRUE)))
Delimit_3<-data.frame(do.call('rbind', strsplit(as.character(TCG_Final$X9),'Rarity',fixed=TRUE)))
TCG_Final$X6 <- Delimit_1$X15
TCG_Final$X7 <- Delimit_2$X15
TCG_Final$X9 <- Delimit_3$X2
TCG_Final$X16 <- TCG_Final1$X16
TCG_Final$X6 <- as.character(TCG_Final$X6)
TCG_Final$X7 <- as.character(TCG_Final$X7)
TCG_Final$X9 <- as.character(TCG_Final$X9)
TCG_Final$X6 = substr(TCG_Final$X6,1,nchar(TCG_Final$X6)-1)
TCG_Final$X7 = substr(TCG_Final$X7,1,nchar(TCG_Final$X7)-1)
TCG_Final$X9 <- left(right(TCG_Final$X9,2),1)
TCG_Final$X9 <- paste(TCG_Final$X9," ")
TCG_Final$X9 <- left(TCG_Final$X9,2)
TCG_Final$X4 <- seq.int(nrow(TCG_Final))
Delimit_4<-data.frame(do.call('rbind', strsplit(as.character(TCG_Final$X15),'\r',fixed=TRUE)))
TCG_Final$X15 <- Delimit_4$do.call..rbind...strsplit.as.character.TCG_Final.X15.....r...
TCG_Final <- subset(TCG_Final, select = c(X4,X5,X6,X7,X9,X15,X16))
#TCG Vendor Formmatting, targeted####
TCG_Vendor <- as.data.frame(Vendor)
TCG_Vendor$Primary_Key <- paste(TCG_Vendor$bit_info,TCG_Vendor$X2,TCG_Vendor$X3," ",sep="")
TCG_Vendor <- TCG_Vendor[c(6,1,2,3,4,5)]
names(TCG_Vendor) <- c("Primary_Key","Card_Name","Set","Rarity","MKT_EST","Vendor Listings")
TCG_Vendor <- as.data.frame(TCG_Vendor)
TCG_Vendor$Rank <- seq.int(nrow(TCG_Vendor))
Middle_Confidence_Report <- TCG_Vendor
#TCG Rankings and Pricing####
TCG_Export = data.frame(TCG_Final$X4,TCG_Final$X5,TCG_Final$X6,TCG_Final$X7,TCG_Final$X9,TCG_Final$X15,TCG_Final$X16)
TCG_Export
names(TCG_Export) = c("TCG_Rank","TCG_Key","TCG Card Name","Set","Rarity","TCG_Avg_Value_Aux","TCG_Avg_Value")
Sets$CK_Modif_Set <- trimws(Sets$CK_Modif_Set)
TCG_Export$Set <- Sets$CK_Modif_Set[match(TCG_Export$Set,Sets$TCG_Key)]
TCG_Export$TCG_Key <- paste(TCG_Export$`TCG Card Name`, TCG_Export$Set, TCG_Export$Rarity, sep = "")
test$TCG_Rank <-TCG_Export$TCG_Rank[match(test$CK_Key,TCG_Export$TCG_Key)]
summary(test$TCG_Rank)
test$TCG_Price <- TCG_Export$TCG_Avg_Value[match(test$CK_Key,TCG_Export$TCG_Key)]
test$TCG_Price_Aux <- TCG_Export$TCG_Avg_Value_Aux[match(test$CK_Key,TCG_Export$TCG_Key)]
test$Gold_Market[is.na(test$Gold_Market)] <- 0
test$CK_Rank[is.na(test$CK_Rank)] <- ""
test$TCG_Rank[is.na(test$TCG_Rank)] <- ""
Delimit_5 <- data.frame(do.call('rbind', strsplit(as.character(test$TCG_Price_Aux),' ',fixed=TRUE)))
test$TCG_Price_Aux = Delimit_5$X41
test$TCG_Price <- as.character(test$TCG_Price)
test$TCG_Price_Aux <- as.character(test$TCG_Price_Aux)
test$TCG_Price[is.na(test$TCG_Price)] <- ""
test$TCG_Price_Aux[is.na(test$TCG_Price_Aux)] <- ""
test$TCG_Price <- ifelse(test$TCG_Price == "PriceGuide","",test$TCG_Price) 
test$TCG_Price_Aux <- ifelse(test$TCG_Price_Aux == "                                Market Price","",test$TCG_Price_Aux)
test$TCG_Price = ifelse(test$TCG_Price == "", test$TCG_Price_Aux, test$TCG_Price)
test$Qty_Des = ifelse(test$Qty_Des == "", test$Qty_Aux, test$Qty_Des)

Pricing = test[c(1,2,3,4,5,14,6,7,8,9,10,11,12,13,15)]
Pricing = as.data.frame(Pricing)
Pricing$TCG_Price <- Price_Trim(Pricing$TCG_Price)
Pricing$TCG_Price <- as.numeric(Pricing$TCG_Price)
Pricing$Gold_Market <- as.numeric(Pricing$Gold_Market)
Pricing$MKT_Est <- ifelse(is.na(Pricing$TCG_Price)==TRUE, Pricing$Gold_Market, Pricing$TCG_Price)
Pricing1 = Pricing
Pricing1$MKT_Est = as.numeric(Pricing1$MKT_Est)
Pricing1$BL_Value = as.numeric(Pricing1$BL_Value)
Pricing$Arbit <- ifelse(is.na(Pricing1$MKT_Est) != TRUE,(Pricing1$BL_Value - Pricing1$MKT_Est),0)
Pricing$Arbit = round(Pricing$Arbit,2)
Pricing$BL_Value[is.na(Pricing$BL_Value)] <- 0
Pricing$Arbit[is.na(Pricing$Arbit)] <- 0
Pricing_Export <- Pricing[c(2,8,9,10,11,12,14,6,16,3,17,4)]
#
Ranking <- Pricing_Export
Ranking$CK_Rank = as.numeric(Ranking$CK_Rank)
Ranking$TCG_Rank = as.numeric(Ranking$TCG_Rank)
Ranking$MKT_Est = as.numeric(Ranking$MKT_Est)
Ranking <- Ranking[order(Ranking$CK_Rank),]
Anchor_CK_price <- Ranking[c(1),(9)]
Ranking$CK_Rank <- round(((Ranking$MKT_Est/Anchor_CK_price)*Ranking$CK_Rank),5)
Ranking <- Ranking[order(-Ranking$CK_Rank),]
Worst_CK_Rank <- (Ranking[c(1),(7)])+1
Ranking$CK_Rank <- ifelse(Ranking$CK_Rank == 0,Worst_CK_Rank,Ranking$CK_Rank)
Ranking$CK_Rank[is.na(Ranking$CK_Rank)] <- Worst_CK_Rank
Ranking <- Ranking[order(Ranking$CK_Rank),]
Absolute_CK_Ranking <- Ranking$CK_Rank
Absolute_CK_Ranking <- seq.int(nrow(Ranking))
Ranking$Abs_CK_Rank <- Absolute_CK_Ranking
Ranking1 <- Ranking
Ranking1$TCG_Rank[is.na(Ranking1$TCG_Rank)] <- 10001
Ranking1$CK_Rank <- as.numeric(Ranking1$CK_Rank)
Ranking1$TCG_Rank <- as.numeric(Ranking1$TCG_Rank)
Ranking1$Weighted_Rank = round(ifelse(((is.na(Ranking1$CK_Rank) != TRUE) & (is.na(Ranking1$TCG_Rank) != TRUE)), (((Ranking1$CK_Rank*.0818)+(Ranking1$TCG_Rank*.5568)/(.5568+.0818))),ifelse(((is.na(Ranking1$CK_Rank) != TRUE) & (is.na(Ranking1$TCG_Rank) = TRUE)),Ranking1$CK_Rank,ifelse(((is.na(Ranking1$CK_Rank) = TRUE) & (is.na(Ranking1$TCG_Rank) != TRUE)),Ranking1$TCG_Rank,40001))),2)
summary(Ranking1$TCG_Rank)
Ranking1 <- Ranking1[order(Ranking1$Weighted_Rank),]
Ranking1$CK_Rank <- Ranking$CK_Rank
Ranking1$TCG_Rank <- Ranking$TCG_Rank
Ranking1 = as.data.frame(Ranking1)
Ranking2 = Ranking1
Ranking2$CK_Rank = as.numeric(Ranking1$CK_Rank)
Ranking2$TCG_Rank = as.numeric(Ranking1$TCG_Rank)
Ranking2$Demand_Pct_Conf = round(ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE) & (Ranking2$CK_Rank < Worst_CK_Rank)), 64,ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE)),56,ifelse((Ranking2$CK_Rank < Worst_CK_Rank),8,0))),0)
Ranking2$CK_Rank <- Ranking$CK_Rank
Ranking2$TCG_Rank <- Ranking$TCG_Rank
ncol(Ranking2)
Ranking_Export <- Ranking2[c(1,2,3,4,5,6,14,15,9,10,11,12,7,13,8)]
Ranking_Export$Vendor <- TCG_Vendor$`Vendor Listings`[match(Ranking_Export$CK_Key,TCG_Vendor$Primary_Key)]
Ranking_Export$TCG_Rank[is.na(Ranking_Export$TCG_Rank)] <- ""
Ranking_Export$Vendor[is.na(Ranking_Export$Vendor)] <- 500
Ranking_Export <- Ranking_Export[c(1,2,3,4,5,6,7,8,9,10,11,12,16,15,14,13)]

Final_Export <- Pricing_Export
Final_Export$Vendor <- Ranking_Export$Vendor[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Weighted_Rank <- Ranking_Export$Weighted_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Adj_CK_Ranking <- Ranking_Export$CK_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Demand_PCT_Conf <-  ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),0,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),8,ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),56,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),64,0))))
Final_Export$TCG_Rank
View(test)
ncol(Final_Export)
Final_Export <- Final_Export[c(1,2,4,5,6,14,16,9,10,11,12,13,8,7,15)]
#Final_Export$BL_Value <- test$BL_Value[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export <- Final_Export[order(-Final_Export$Demand_PCT_Conf,-Final_Export$Arbit,Final_Export$Weighted_Rank),]

#Export My Masterpiece####
currentDate <- Sys.Date()
csvFileName <- paste("High_Confidence_Report",currentDate,".csv",sep="")
write.csv(Final_Export, file=csvFileName) 

currentDate <- Sys.Date()
csvFileName <- paste("MTG_Vendors_",currentDate,".csv",sep="")
write.csv(TCG_Vendor, file=csvFileName) 

currentDate <- Sys.Date()
csvFileName <- paste("Low_Confidence_Report",currentDate,".csv",sep="")
write.csv(Basic_Market_Review, file=csvFileName) 
