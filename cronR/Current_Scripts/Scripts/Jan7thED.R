#Load libraries####
#install.packages("Rtools")
install.packages('Rtools', repos = "http://cran.us.r-project.org")
#install.packages("rvest")
install.packages('rvest', repos = "http://cran.us.r-project.org")
#install.packages("jsonlite")
install.packages('jsonlite', repos = "http://cran.us.r-project.org")
#install.packages("tidyverse")
install.packages('tidyverse', repos = "http://cran.us.r-project.org")
#install.packages("tidyquant")
install.packages('tidyquant', repos = "http://cran.us.r-project.org")
#install.packages("xopen")
install.packages('xopen', repos = "http://cran.us.r-project.org")
#install.packages("knitr")
install.packages('knitr', repos = "http://cran.us.r-project.org")
#p_install_gh("bnosac/cronR")
#Packages & Needed Functions####
library(rvest)     # HTML Hacking & Web Scraping
library(jsonlite)  # JSON manipulation
library(tidyverse) # Data Manipulation
library(tidyquant) # ggplot2 theme
library(xopen)     # Opens URL in Browser
library(dplyr)
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
total = 140 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
for(i in 1:140){
  url <- paste0("https://www.cardkingdom.com/purchasing/mtg_singles?filter%5Bipp%5D=250&filter%5Bsort%5D=edition&filter%5Bsearch%5D=mtg_advanced&page=",i)
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE) # Gets around company firewall restrictions
  html <- read_html("scrapedpage.html")  #html <- read_html(url) html <- read_html("scrapedpage.html")
  json <- html %>% html_nodes(".itemContentWrapper") %>% html_text()
  json <-data.frame(json)
  All <-rbind(All,json)
  #Sys.sleep(sample(3:7, 1))
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
test$X4<-trimws(test2$X1)
test$X5<- test3$X1
test$X6 <- test3$X2 #having reviewed the outputs of our delimiting above, reconstruct the card names, sets, and rarity above.
test$X2 <- paste(test$X3,test$X4,test$X5,test$X6,sep = "") #Creating THE PRIMARY KEY for all subsequent scrape joins.
test$X1 <- test$X4#Create a secondary key for mtggoldfish as they serve as the baseline, in terms of all cards we could possibly track
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
CK_BL_Scrape_Sets <- c("  3rd Edition ","  5th Edition ","  7th Edition ","  8th Edition ","  9th Edition "," 10th Edition "," 2010 Core Set "," 2011 Core Set ",
                       " 2012 Core Set "," 2013 Core Set "," 2015 Core Set "," 3rd Edition "," 4th Edition "," 5th Edition "," 6th Edition "," 7th Edition "," 8th Edition ",
                       " 9th Edition "," Aether Revolt "," Amonkhet "," Antiquities "," Apocalypse "," Archenemy "," Avacyn Restored "," Battle for Zendikar "," Battlebond "," Beta ",
                       " Betrayers of Kamigawa "," Born of the Gods "," Champions of Kamigawa "," Coldsnap "," Collectors Ed "," Collectors Ed Intl "," Commander 2013 "," Commander 2014 ",
                       " Commander 2015 "," Commander 2016 "," Commander 2017 "," Commander 2019 "," Commander's Arsenal "," Conflux "," Conspiracy - Take the Crown "," Core Set 2019 ",
                       " Core Set 2020 "," Dissension "," Dominaria "," Dragon's Maze "," Dragons of Tarkir "," Duel Decks: Anthology "," Duel Decks: Knights Vs. Dragons "," Duel Decks: Nissa Vs. Ob Nixilis "," Eventide ",
                       " Exodus "," Fate Reforged "," Fifth Dawn "," Future Sight "," Gatecrash "," Guildpact "," Guilds of Ravnica "," Ice Age "," Iconic Masters "," Innistrad ",
                       " Ixalan "," Judgment "," Kaladesh "," Khans of Tarkir "," Legends "," Legions "," Lorwyn "," Magic Origins "," Masters 25 "," Mercadian Masques "," Mirage ",
                       " Mirrodin "," Mirrodin Besieged "," Modern Event Deck "," Modern Horizons "," Modern Masters "," Modern Masters 2015 "," Modern Masters 2017 ",
                       " Morningtide ", "Mystery Booster"," Nemesis "," Onslaught "," Planechase "," Planechase Anthology "," Planeshift "," Portal "," Portal 3K "," Premium Deck Series: Graveborn ",
                       " Promo Pack "," Promotional "," Ravnica "," Ravnica Allegiance "," Rise of the Eldrazi "," Rivals of Ixalan "," Scars of Mirrodin "," Scourge ",
                       " Shadowmoor "," Shadows Over Innistrad "," Shards of Alara "," Signature Spellbook: Gideon "," Starter 1999 "," The Dark "," Theros "," Throne of Eldraine ",
                       " Throne of Eldraine Variants "," Time Spiral "," Timeshifted "," Torment "," Ultimate Masters "," Unglued "," Unhinged "," Unlimited "," Urza's Legacy ",
                       " Urza's Saga "," Visions "," War of the Spark "," War of the Spark JPN Planeswalkers "," Weatherlight "," World Championships "," Worldwake "," Zendikar ",
                       "10th Edition ","2010 Core Set ","2011 Core Set ","2012 Core Set ","2013 Core Set ","2014 Core Set ","2015 Core Set ","Aether Revolt ","Alara Reborn ","Alliances ",
                       "Alpha ","Amonkhet ","Anthologies ","Antiquities ","Apocalypse ","Arabian Nights ","Archenemy ","Archenemy - Nicol Bolas ","Art Series ","Avacyn Restored ","Battle for Zendikar ",
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
                       "Scourge ","Secret Lair",
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
                       "Zendikar ", "Theros Beyond Death")
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
                       "Morningtide","",
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
                       "Scourge","Secret Lair",
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
                       "Zendikar","Theros Beyond Death")
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
             "MOR","",
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
             "SCG","SLD",
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
             "ZEN",
             "THB")
GF_Abbr_Foil <- c("",
                  "", 
                  "7E_F",
                  "8ED_F",
                  "9ED_F",
                  "10E_F",
                  "M10_F",
                  "M11_F",
                  "M12_F",
                  "M13_F",
                  "M15_F",
                  "",
                  "",
                  "",
                  "",
                  "7_F",
                  "8ED_F",
                  "9ED_F",
                  "AER_F",
                  "AKH_F",
                  "",
                  "AP_F",
                  "",
                  "AVR_F",
                  "BFZ_F",
                  "BBD_F",
                  "",
                  "BOK_F",
                  "BNG_F",
                  "CHK_F",
                  "CSP_F",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "CON_F",
                  "CN2_F",
                  "M19_F",
                  "M20_F",
                  "DIS_F",
                  "DOM_F",
                  "DGM_F",
                  "DTK_F",
                  "",
                  "",
                  "",
                  "EVE_F",
                  "EX_F",
                  "FRF_F",
                  "5DN_F",
                  "FUT_F",
                  "GTC_F",
                  "GPT_F",
                  "GRN_F",
                  "ICE_F",
                  "IMA_F",
                  "ISD_F",
                  "XLN_F",
                  "JUD_F",
                  "KLD_F",
                  "KTK_F",
                  "LEG_F",
                  "LGN_F",
                  "LRW_F",
                  "ORI_F",
                  "A25_F",
                  "MM_F",
                  "MI_F",
                  "MRD_F",
                  "MBS_F",
                  "MD1_F",
                  "MH1_F",
                  "MMA_F",
                  "MM2_F",
                  "MM3_F",
                  "MOR_F",
                  "",
                  "NE_F",
                  "ONS_F",
                  "",
                  "",
                  "PS_F",
                  "POR_F",
                  "PTK_F",
                  "",
                  "",
                  "",
                  "RAV_F",
                  "RNA_F",
                  "ROE_F",
                  "RIX_F",
                  "SOM_F",
                  "SCG_F",
                  "SHM_F",
                  "SOI_F",
                  "ALA_F",
                  "SS2_F",
                  "",
                  "DRK_F",
                  "THS_F",
                  "ELD_F",
                  "PELD_F",
                  "TSP_F",
                  "TSB_F",
                  "TOR_F",
                  "UMA_F",
                  "UGL_F",
                  "UNH_F",
                  "",
                  "UL_F",
                  "UZ_F",
                  "VI_F",
                  "WAR_F",
                  "WAR_F",
                  "WL_F",
                  "",
                  "WWK_F",
                  "ZEN_F",
                  "10_F",
                  "M10_F",
                  "M11_F",
                  "M12_F",
                  "M13_F",
                  "M14_F",
                  "M15_F",
                  "AER_F",
                  "ARB_F",
                  "ALL_F",
                  "",
                  "AKH_F",
                  "ATH_F",
                  "ATQ_F",
                  "AP_F",
                  "ARN_F",
                  "ARC_F",
                  "",
                  "",
                  "AVR_F",
                  "BFZ_F",
                  "",
                  "BBD_F",
                  "",
                  "",
                  "BOK_F",
                  "BNG_F",
                  "CHK_F",
                  "CHR_F",
                  "CSP_F",
                  "CST_F",
                  "",
                  "",
                  "CMD_F",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "CON_F",
                  "CNS_F",
                  "CN2_F",
                  "M19_F",
                  "M20_F",
                  "DKA_F",
                  "DST_F",
                  "",
                  "DIS_F",
                  "DOM_F",
                  "DGM_F",
                  "DTK_F",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "EMN_F",
                  "EMA_F",
                  "EVE_F",
                  "EX_F",
                  "E02_F",
                  "FEM_F",
                  "FRF_F",
                  "5DN_F",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "FUT_F",
                  "",
                  "GTC_F",
                  "",
                  "GPT_F",
                  "GRN_F",
                  "",
                  "HML_F",
                  "HOU_F",
                  "ICE_F",
                  "IMA_F",
                  "ISD_F",
                  "IN_F",
                  "XLN_F",
                  "JOU_F",
                  "JUD_F",
                  "KLD_F",
                  "KTK_F",
                  "LEG_F",
                  "LGN_F",
                  "LRW_F",
                  "ORI_F",
                  "",
                  "",
                  "",
                  "",
                  "A25_F",
                  "MM_F",
                  "MI_F",
                  "MRD_F",
                  "MBS_F",
                  "",
                  "MH1_F",
                  "MMA_F",
                  "MM2_F",
                  "MM3_F",
                  "MOR_F",
                  "NE_F",
                  "NPH_F",
                  "OGW_F",
                  "OD_F",
                  "ONS_F",
                  "PLC_F",
                  "",
                  "",
                  "",
                  "PS_F",
                  "POR_F",
                  "PTK_F",
                  "PO2_F",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "",
                  "RAV_F",
                  "RNA_F",
                  "GK2_F",
                  "RTR_F",
                  "ROE_F",
                  "RIX_F",
                  "SOK_F",
                  "SOM_F",
                  "SCG_F",
                  "",
                  "SHM_F",
                  "SOI_F",
                  "ALA_F",
                  "SS2_F",
                  "SS1_F",
                  "",
                  "",
                  "ST_F",
                  "TE_F",
                  "DRK_F",
                  "THS_F",
                  "ELD_F",
                  "PELD_F",
                  "TSP_F",
                  "TSB_F",
                  "TOR_F",
                  "",
                  "UMA_F",
                  "UGL_F",
                  "UNH_F",
                  "",
                  "UST_F",
                  "UD_F",
                  "UL_F",
                  "UZ_F",
                  "VAN_F",
                  "VI_F",
                  "WAR_F",
                  "WAR_F",
                  "WL_F",
                  "",
                  "WWK_F",
                  "ZEN_F",
                  "THB_F")
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
             "Morningtide","Mystery Booster Cards",
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
             "Scourge","Secret Lair Drop Series",
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
             "Zendikar",
             "Theros Beyond Death")
Sets <- data.frame(CK_BL_Scrape_Sets,MTG_Goldfish_Sets,GF_Abbr,GF_Abbr_Foil,TCG_Key,stringsAsFactors = TRUE)

#Sets Added####
names(Sets) <- c("CK_Modif_Set","Goldfish_Full","Goldfish_Abbrev","TCG_Key") #rename columns to identify sets by their website of origin.
Sets$CK_Modif_Set <- trimws(Sets$CK_Modif_Set)
Sets$Goldfish_Full <- trimws(Sets$Goldfish_Full)
Sets$Goldfish_Abbrev <- trimws(Sets$Goldfish_Abbrev)
Sets$TCG_Key <- trimws(Sets$TCG_Key)
test$Gold_Merge <- Sets$Goldfish_Abbrev[match(test$CK_Modif_Set,Sets$CK_Modif_Set)] #Merge goldfish abbreviated sets against cardkingdom scrape values for sets
test$MTG_Gold_Key <- paste(test$`Card Name`,test$Gold_Merge, sep ="") #create the secondary key for goldfish
CK_BL_Output <- test
#Goldfish Market Scrape####
#Create a list of all the sites for MTG Goldfish to retrieve the market values from
sets = list("10E","1E","2ED","3ED","4ED","5DN","5ED","6ED","7E","8ED","9ED","A25","AER","AKH","ALA","ALL","AP","ARB","ARC","ARN","ATQ","AVR","BBD","BFZ","BNG","BOK","BOOSTER","BRB","BTD","C13","C14","C15","C16","C17","C18","C19","CHK","CHR","CM1","CM2","CMA","CMD","CN2","CNS","CON","CSP","DD2","DDC","DDD","DDE","DDF","DDG","DDH","DDI","DDJ","DDK","DDL","DDM","DDN","DDO","DDP","DDQ","DDR","DDS","DDT","DDU","DGM","DIS","DKA","DOM","DRB","DRK","DST","DTK","E01","E02","ELD","EMA","EMN","EVE","EVG","EX","EXP","FEM","FRF","FUT","GK1","GK2","GNT","GPT","GRN","GTC","H09","HML","HOU","ICE","IMA","ISD","JOU","JUD","KLD","KTK","LEA","LEB","LEG","LGN","LRW","M10","M11","M12","M13","M14","M15","M19","M20","MBS","MD1","ME2","ME3","ME4","MED","MED-GRN","MED-RNA","MED-WAR","MH1","MI","MM","MM2","MM3","MMA","MOR","MRD","MS2","MS3","NE","NPH","OD","OGW","ONS","ORI","PC1","PC2","PCA","PD2","PD3","PELD","PLC","PO2","POR","PR","PRM","PRM-ARN","PRM-BAB","PRM-CHP","PRM-FNM","PRM-GBP","PRM-GDP","PRM-GPP","PRM-GUR","PRM-GWP","PRM-HRO","PRM-JSS","PRM-JUD","PRM-LEP","PRM-LPC","PRM-MED","PRM-MPR","PRM-MSC","PRM-OHP","PRM-PRE","PRM-PTP","PRM-REL","PRM-SDCC13","PRM-SDCC14","PRM-SDCC15","PRM-SDCC16","PRM-SDCC17","PRM-SDCC18","PRM-SDCC19","PRM-SPO","PRM-SSP","PRM-UGF","PRM-UMA","PRM-WMCQ","PRM-WPN","PS","PTG","PTK","PZ1","PZ2","RAV","RIX","RNA","ROE","RTR","S00","S99","SCG","SLD","SHM","SOI","SOK","SOM","SS1","SS2","ST","TD0","TD2","TE","THS","TOR","TPR","TSB","TSP","UD","UGL","UL","UMA","UNH","UST","UZ","V09","V10","V11","V12","V13","V14","V15","V16","V17","VAN","VI","VMA","WAR","WL","WWK","XLN","ZEN")
total = 238 #238 sets on the list above, take my word for it ;)
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
Back_Up_Gold <- Gold_Market
#Gold_Market <- Back_Up_Gold
#
#Foil Sets####
sets_Foil = list("7E_F",
                 "8ED_F",
                 "9ED_F",
                 "10E_F",
                 "M10_F",
                 "M11_F",
                 "M12_F",
                 "M13_F",
                 "M15_F",
                 "AER_F",
                 "AKH_F",
                 "AP_F",
                 "AVR_F",
                 "BFZ_F",
                 "BBD_F",
                 "BOK_F",
                 "BNG_F",
                 "CHK_F",
                 "CSP_F",
                 "CON_F",
                 "CN2_F",
                 "M19_F",
                 "M20_F",
                 "DIS_F",
                 "DOM_F",
                 "DGM_F",
                 "DTK_F",
                 "EVE_F",
                 "EX_F",
                 "FRF_F",
                 "5DN_F",
                 "FUT_F",
                 "GTC_F",
                 "GPT_F",
                 "GRN_F",
                 "IMA_F",
                 "ISD_F",
                 "XLN_F",
                 "JUD_F",
                 "KLD_F",
                 "KTK_F",
                 "LGN_F",
                 "LRW_F",
                 "ORI_F",
                 "A25_F",
                 "MM_F",
                 "MI_F",
                 "MRD_F",
                 "MBS_F",
                 "MH1_F",
                 "MMA_F",
                 "MM2_F",
                 "MM3_F",
                 "MOR_F",
                 "NE_F",
                 "ONS_F",
                 "PS_F",
                 "RAV_F",
                 "RNA_F",
                 "ROE_F",
                 "RIX_F",
                 "SOM_F",
                 "SCG_F",
                 "SHM_F",
                 "SOI_F",
                 "ALA_F",
                 "THS_F",
                 "ELD_F",
                 "TSP_F",
                 "TSB_F",
                 "TOR_F",
                 "UMA_F",
                 "UGL_F",
                 "UNH_F",
                 "UL_F",
                 "UZ_F",
                 "VI_F",
                 "WAR_F",
                 "WL_F",
                 "WWK_F",
                 "M14_F",
                 "ARB_F",
                 "CNS_F",
                 "DKA_F",
                 "DST_F",
                 "EMN_F",
                 "EMA_F",
                 "HOU_F",
                 "IN_F",
                 "JOU_F",
                 "NPH_F",
                 "OGW_F",
                 "OD_F",
                 "PLC_F",
                 "RTR_F",
                 "SOK_F",
                 "ST_F",
                 "TE_F",
                 "UST_F",
                 "UD_F",
                 "ZEN_F")
#Foil Scrape####
total = 150 #240 sets on the list above, take my word for it ;)
pb <- txtProgressBar(min=0, max = total, style = 3) # loading bar formatting
Gold_Foil_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
tic()
for(i in sets_Foil){
  
  try(url <- paste0("https://www.mtggoldfish.com/index/",print(i),"#paper"))
  html <- read_html(url)
  tbls_fs <- html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls <-data.frame(tbls_fs)
  Gold_Foil_Market <-rbind(Gold_Foil_Market,tbls_ls)
  setTxtProgressBar(pb,i)
}
toc()
Foil_Market <- Gold_Foil_Market
#Goldfish Market Formatting####
Gold_Market <- Gold_Market[c(5,1,2,4)] #Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Gold_Market$Price <- as.numeric(Gold_Market$Price) #Convert to numeric from characters
Gold_Market$Price <- round(Gold_Market$Price * 0.845,2) #ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg goldfish but you also kinda suck here.
Gold_Market$Daily <- paste(Gold_Market$Card,Gold_Market$Set, sep="") #Create secondary goldfish key to act as merger column
Foil_Market <- Foil_Market[c(5,1,2,4)] #Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Foil_Market$Price <- as.numeric(Foil_Market$Price) #Convert to numeric from characters
Foil_Market$Price <- round(Foil_Market$Price * 0.845,2) #ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg Foilfish but you also kinda suck here.
Foil_Market$Daily <- paste(Foil_Market$Card,Foil_Market$Set, sep="")
test$Gold_Market <- ifelse(test$`NF/F` == " FOIL", Foil_Market$Price[match(test$MTG_Gold_Key,Foil_Market$Daily)] ,Gold_Market$Price[match(test$MTG_Gold_Key,Gold_Market$Daily)])
test <- test[c(1,2,8,9,10,12,3,4,5,6,7,11)] #Rearrange the columns once more for the honey pot
#test$CK_Key = substr(test$CK_Key,1,nchar(test$CK_Key)-1) #Noticed here that my CK key had an extra space at the end that was messing with final export. This line should belong up in CK formatting but since this is where the discovery is most pertinet for followup steps, I've left it here
Basic_Market_Review <- test
Basic_Market_Review <- Basic_Market_Review[c(2,7,9,10,11,6,3,4)]
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
  json <-data.frame(json)
  CK_Store_Front <-rbind(CK_Store_Front,json)
  CK_name <- html %>% html_nodes(".productDetailTitle") %>% html_text()
  CK_set <- html %>% html_nodes(".productDetailSet") %>% html_text()
  CK_set_a <- data.frame(do.call('rbind', strsplit(as.character(CK_set),'(',fixed=TRUE)))
  CK_set_b <- data.frame(do.call('rbind', strsplit(as.character(CK_set_a$X1),'\n',fixed=TRUE)))
  CK_set <- trimws(CK_set_b$X2)
  CK_rarity <- data.frame(do.call('rbind',strsplit(as.character(CK_set_a$X2),')',fixed=TRUE)))
  CK_rarity <- trimws(CK_rarity$X1)
  prices <- html %>% html_nodes(".stylePrice") %>% html_text()
  prices <- data.frame(do.call('rbind',strsplit(as.character(prices),'$',fixed=TRUE)))
  prices <- data.frame(do.call('rbind',strsplit(as.character(prices$X2),'\n',fixed=TRUE)))
  prices <- prices[,1]
  prices <- as.data.frame(prices)
  prices = prices[seq(1, nrow(prices), 4), ]
  prices <- as.data.frame(prices)
  CK_name <- as.data.frame(CK_name)
  nrow(CK_name)
  CK_set <- as.data.frame(CK_set)
  nrow(CK_set)
  CK_rarity <- as.data.frame(CK_rarity)
  nrow(CK_rarity)
  prices <- as.data.frame(prices)
  nrow(prices)
  CK_NF_MKT <- data.frame(
    X2 <- CK_name,
    X3 <- CK_set,
    X4 <- CK_rarity,
    X5 <- prices)
  CK_Prices <- rbind(CK_Prices, CK_NF_MKT)
  
  setTxtProgressBar(pb,i)
  
}
toc()
CK_Prices_df <- as.data.frame(CK_Prices)
CK_Prices_df$key <- paste(CK_Prices_df$CK_name,CK_Prices_df$CK_set,CK_Prices_df$CK_rarity," ",sep="")
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

test2 <- test #Create seperate data frame (df) to the og to ensure I recreate my primary key correctly again
#test2$CK_Key = substr(test$CK_Key,1,nchar(test$CK_Key)-1) #account for that pesky space character again
test2$CK_Key <- trimws(test2$CK_Key)
test$CK_Rank <- CK_MKT$X1[match(test2$CK_Key,CK_MKT$X2)] #Merge the CK best selling rankings with our original CK Buylist scrape
Low_Confidence_Report <- test
View (Low_Confidence_Report)
#TCG- Market####
TCG <-NULL
Vendor = NULL
total = 1000 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
tic()
for(i in 1:1000){
  url <- paste0("https://shop.tcgplayer.com/magic/product/show?Rarity=Mythic&Rarity=Rare&Rarity=Uncommon&Rarity=Common&Condition=LightlyPlayed&Condition=NearMint&Language=English&MinQuantity=4&PageNumber=",i)
  html <- read_html(url)
  json <- html %>% html_nodes(".product__details") %>% html_text()
  json <-data.frame(json)
  TCG <-rbind(TCG,json)
  bit <- html %>% html_nodes(".product__offers-more-count") %>% html_text()
  bit <-data.frame(bit)
  bitp <- bit[- (grep("photos", bit$bit)),]
  bitp <- as.data.frame(bitp)
  bits <- NULL
  bits <- ifelse(nrow(bit) == 10, as.list(bit), as.list(bitp))
  bits <- as.data.frame(bits)
  colnames(bits) <- ("bit")
  bit <- bits
  #bit <- as.factor(bit)
  bit <- as.data.frame(bit)
  #bit$bit <- as.character(bit$bit)
  bit_1 <- data.frame(do.call('rbind', strsplit(as.character(bit$bit),' price',fixed=TRUE)))
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
#View(TCG_Final)
#TCG Vendor Formatting, targeted####
TCG_Vendor <- as.data.frame(Vendor)
TCG_Vendor$Primary_Key <- paste(TCG_Vendor$bit_info,TCG_Vendor$X2,TCG_Vendor$X3,sep="")
TCG_Vendor <- TCG_Vendor[c(6,1,2,3,4,5)]
names(TCG_Vendor) <- c("Primary_Key","Card_Name","Set","Rarity","MKT_EST","Vendor Listings")
TCG_Vendor <- as.data.frame(TCG_Vendor)
TCG_Vendor$Rank <- seq.int(nrow(TCG_Vendor))
Middle_Confidence_Report <- TCG_Vendor
#View(TCG_Final)
#TCG Rankings and Pricing####
TCG_Export = data.frame(TCG_Final$X4,TCG_Final$X5,TCG_Final$X6,TCG_Final$X7,TCG_Final$X9,TCG_Final$X15,TCG_Final$X16)
names(TCG_Export) = c("TCG_Rank","TCG_Key","TCG Card Name","Set","Rarity","TCG_Avg_Value_Aux","TCG_Avg_Value")
Sets <- data.frame(CK_BL_Scrape_Sets,MTG_Goldfish_Sets,GF_Abbr,GF_Abbr_Foil,TCG_Key,stringsAsFactors = TRUE)
TCG_Export$Set <- Sets$CK_BL_Scrape_Sets[match(TCG_Export$Set,Sets$TCG_Key)]
TCG_Export$Rarity <- trimws(TCG_Export$Rarity)
TCG_Export$Set <- trimws(TCG_Export$Set)
TCG_Export$`TCG Card Name` <- trimws(TCG_Export$`TCG Card Name`)
TCG_Export$TCG_Key <- paste(TCG_Export$`TCG Card Name`, TCG_Export$Set, TCG_Export$Rarity, sep = "")
test$CK_Key <- trimws(test$CK_Key)
test$TCG_Rank <-TCG_Export$TCG_Rank[match(test$CK_Key,TCG_Export$TCG_Key)]

summary(test)
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
Pricing$MKT_Est <- ifelse(Pricing$`NF/F` == " FOIL", Pricing$Gold_Market, ifelse(is.na(Pricing$TCG_Price)==TRUE,Pricing$Gold_Market,Pricing$TCG_Price))
Pricing1 = Pricing
Pricing1$MKT_Est = as.numeric(Pricing1$MKT_Est)
Pricing1$BL_Value = as.numeric(Pricing1$BL_Value)
Pricing$Arbit <- ifelse(is.na(Pricing1$MKT_Est) != TRUE,(Pricing1$BL_Value - Pricing1$MKT_Est),0)
Pricing$Arbit = round(Pricing$Arbit,2)
Pricing$BL_Value[is.na(Pricing$BL_Value)] <- 0
Pricing$Arbit[is.na(Pricing$Arbit)] <- 0
Pricing_Export <- Pricing[c(2,8,9,10,11,12,14,6,16,3,17,4)]
#View(Pricing_Export)
#
Ranking <- Pricing_Export
Ranking$CK_Rank = as.numeric(Ranking$CK_Rank)
Ranking$TCG_Rank = as.numeric(Ranking$TCG_Rank)
Ranking$MKT_Est = as.numeric(Ranking$MKT_Est)
Ranking <- Ranking[order(Ranking$CK_Rank),]
#If Weighted and Adj are "Inf" <- change Line 1596 from c(1) to c(2)####
Anchor_CK_price <- Ranking[c(4),(9)]
View(Ranking)
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
#View(Ranking1)
Ranking1$CK_Rank <- Ranking$CK_Rank
Ranking1$TCG_Rank <- Ranking$TCG_Rank
Ranking1 = as.data.frame(Ranking1)
Ranking2 = Ranking1
Ranking2$CK_Rank = as.numeric(Ranking1$CK_Rank)
Ranking2$TCG_Rank = as.numeric(Ranking1$TCG_Rank)
Ranking2$Demand_Pct_Conf = round(ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE) & (Ranking2$CK_Rank < Worst_CK_Rank)), 64,ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE)),56,ifelse((Ranking2$CK_Rank < Worst_CK_Rank),8,0))),0)
Ranking2$CK_Rank <- Ranking$CK_Rank
Ranking2$TCG_Rank <- Ranking$TCG_Rank
Ranking2 <- Ranking2[order(Ranking2$CK_Rank),]
Ranking_Export <- Ranking2[c(1,2,3,4,5,6,14,15,9,10,11,12,7,13,8)]
Ranking_Export$Vendor <- TCG_Vendor$`Vendor Listings`[match(Ranking_Export$CK_Key,TCG_Vendor$Primary_Key)]
Ranking_Export$TCG_Rank[is.na(Ranking_Export$TCG_Rank)] <- ""
Ranking_Export$Vendor <- as.character(Ranking_Export$Vendor)
Ranking_Export$Vendor[is.na(Ranking_Export$Vendor)] <- ""
Ranking_Export <- Ranking_Export[c(1,2,3,4,5,6,7,8,9,10,11,12,16,15,14,13)]
Final_Export <- Pricing_Export
Final_Export$Vendor <- Ranking_Export$Vendor[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Weighted_Rank <- Ranking_Export$Weighted_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Adj_CK_Ranking <- Ranking_Export$CK_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Demand_PCT_Conf <-  ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),0,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),8,ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),56,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),64,0))))
Final_Export$TCG_Rank
Final_Export <- Final_Export[c(1,2,4,5,6,12,10,9,11,13,8,15,14,16)]
names(Final_Export)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT","Arb","Sellers","TCG_Rank","CK_ADJ_Rank","OVR_Rank","%_of_Market")
Final_Export$CK_ADJ_Rank <- round(Final_Export$CK_ADJ_Rank,2)
Final_Export <- Final_Export[order(Final_Export$CK_ADJ_Rank),]
Final_Export$CK_ADJ_Rank <- seq.int(nrow(Final_Export))
Final_Export <- Final_Export[order(Final_Export$OVR_Rank),]
Final_Export$OVR_Rank <- seq.int(nrow(Final_Export))
Final_Export$MKT_TS_Single <- round((Final_Export$MKT * 1.08875)+.78,2)
Final_Export$MKT_TS_Set <- round(((Final_Export$MKT * 4)* 1.08875)+.78,2)
Final_Export$`Single_Arb_%` <- round((Final_Export$BL - Final_Export$MKT_TS_Single)/Final_Export$MKT_TS_Single,2)
Final_Export$`Set_Arb_%` <- round(((Final_Export$BL*4) - Final_Export$MKT_TS_Set)/Final_Export$MKT_TS_Set,2)
Final_Export_1 <- Final_Export
Final_Export_1$MKT <- as.numeric(Final_Export_1$MKT)
Final_Export_1 <- Final_Export[c(1,2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,7)]
#seq.int(nrow(CK_MKT))
#Final_Export$BL_Value <- test$BL_Value[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export_1 <- Final_Export_1[order(-Final_Export_1$`Single_Arb_%`),]
Final_Export_1<-subset(Final_Export_1, Final_Export_1$MKT!=0)
Final_Export_1<-subset(Final_Export_1, Final_Export_1$MKT!=0)

#Export My Masterpiece####
setwd("/cloud/project/Reports/High Confidence Reps")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE) 

setwd("/cloud/project/Reports/TCG Vendor")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_TCG",".csv",sep="")
write.csv(TCG_Vendor, file=csvFileName, row.names = FALSE) 
View(TCG_Vendor)
setwd("/cloud/project/Reports/Low Confidence Reps")
Basic_Market_Review <- Basic_Market_Review[c(1,2,3,4,5,8,7,6)]
names(Basic_Market_Review)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT")
csvFileName <- paste(currentDate,"_Basic",".csv",sep="")
write.csv(Basic_Market_Review, file=csvFileName, row.names = FALSE) 


#Funny Money Report####
CK_Prices_df$key <- trimws(CK_Prices_df$key)
CK_Prices_df$TCG_Price <- Final_Export$MKT[match(CK_Prices_df$key,Final_Export$Key)]
CK_Prices_df$TCG_Price <- as.numeric(CK_Prices_df$TCG_Price)
CK_Prices_df$CK_TCG_PCT_DIFF <- as.numeric(as.character(CK_Prices_df$prices))/CK_Prices_df$TCG_Price
CK_Price_Comparison <- CK_Prices_df[c(5,1,2,3,6,4)]
names(CK_Price_Comparison) <- c("Key","Card_Name","Set","Rarity","TCG_Price","CK_Price")
CK_Price_Comparison$TCG_Price[is.na(CK_Price_Comparison$TCG_Price)] <- 1
CK_Price_Comparison$TCG_Price <- ifelse(CK_Price_Comparison$TCG_Price == 0.00,1.00, CK_Price_Comparison$TCG_Price)
CK_Price_Comparison$CK_Price <- as.numeric(as.character(CK_Price_Comparison$CK_Price))
CK_Price_Comparison$Price_Diff <- round((CK_Price_Comparison$CK_Price/CK_Price_Comparison$TCG_Price),2)
CK_Price_Comparison$Price_Diff <- ifelse(CK_Price_Comparison$Price_Diff == CK_Price_Comparison$CK_Price, "Not Captured", CK_Price_Comparison$Price_Diff)
CK_Price_Comparison <- CK_Price_Comparison[which(CK_Price_Comparison$Price_Diff != "Not Captured"),]
CK_Price_Comparison <- CK_Price_Comparison[order(-as.numeric(CK_Price_Comparison$Price_Diff)),]
#Exclusion Sets####
Set_Excl = c("Throne of Eldraine",
             "Modern Horizons",
             "War of the Spark",
             "Core Set 2020",
             "Amonkhet",
             "Ravnica Allegiance",
             "2015 Core Set",
             "Dominaria",
             "2014 Core Set",
             "Commander 2019",
             "Guilds of Ravnica",
             "Khans of Tarkir",
             "Aether Revolt",
             "Theros",
             "Ixalan",
             "Kaladesh",
             "Fate Reforged",
             "Core Set 2019",
             "Unstable",
             "Return to Ravnica",
             "Rivals of Ixalan",
             "2013 Core Set",
             "Hour of Devastation",
             "Duel Decks: Elves Vs. Inventors",
             "Shadows Over Innistrad",
             "Eldritch Moon",
             "2011 Core Set",
             "Zendikar",
             "Future Sight",
             "Born of the Gods",
             "Ultimate Masters",
             "Iconic Masters",
             "Explorers of Ixalan",
             "Magic Origins",
             "Prophecy",
             "Dragons of Tarkir",
             "Dragon's Maze",
             "Gatecrash",
             "Innistrad",
             "Battle for Zendikar",
             "Ice Age",
             "Modern Masters 2015",
             "Chronicles",
             "Fifth Dawn",
             "Oath of the Gatewatch",
             "Champions of Kamigawa",
             "Journey into Nyx",
             "Commander 2018",
             "Worldwake",
             "Avacyn Restored",
             "New Phyrexia",
             "Commander 2014",
             "Dark Ascension",
             "Tempest",
             "Dissension",
             "2012 Core Set",
             "Battlebond",
             "Guildpact",
             "Commander 2017",
             "Time Spiral",
             "Visions",
             "Ravnica",
             "Masters 25",
             "Darksteel",
             "Shadowmoor",
             "Signature Spellbook: Gideon",
             "Scars of Mirrodin",
             "Rise of the Eldrazi",
             "Coldsnap",
             "Urza's Legacy",
             "Odyssey",
             "Legions",
             "Urza's Saga",
             "Mirrodin",
             "Global Series: Jiang Yanggu & Mu Yanling",
             "2010 Core Set",
             "Planar Chaos",
             "Mirrodin Besieged",
             "Morningtide",
             "Conspiracy",
             "Invasion",
             "Modern Masters 2017",
             "Conspiracy - Take the Crown",
             "Onslaught",
             "Conflux",
             "Eternal Masters",
             "Saviors of Kamigawa",
             "Torment",
             "Stronghold",
             "Planeshift",
             "Commander 2013",
             "Weatherlight",
             "Commander 2016",
             "Scourge",
             "Mirage",
             "Modern Masters",
             "Nemesis",
             "Unlimited",
             "Lorwyn",
             "Apocalypse",
             "Mercadian Masques",
             "Exodus",
             "Alara Reborn",
             "Betrayers of Kamigawa",
             "Urza's Destiny",
             "Legends",
             "Modern Event Deck",
             "Alliances",
             "Duel Decks: Mind Vs. Might",
             "Commander Anthology Vol. II",
             "Ravnica Allegiance: Guild Kits",
             "Commander",
             "Commander 2015",
             "Signature Spellbook: Jace",
             "Duel Decks: Zendikar Vs. Eldrazi",
             "Duel Decks: Nissa Vs. Ob Nixilis",
             "Homelands",
             "Duel Decks: Izzet Vs. Golgari",
             "Eventide",
             "The Dark",
             "Duel Decks: Blessed Vs. Cursed",
             "Chronicles",
             "Judgment",
             "Weatherlight",
             "Ice Age",
             "Duel Decks: Jace Vs. Vraska",
             "Planechase Anthology",
             "Fallen Empires",
             "10th Edition",
             "Duel Decks: Sorin Vs. Tibalt",
             "Archenemy - Nicol Bolas",
             "Antiquities",
             "Time Spiral",
             "Duel Decks: Speed Vs. Cunning",
             "Duel Decks: Knights Vs. Dragons",
             "Commander Anthology",
             "Duel Decks: Venser Vs. Koth",
             "Duel Decks: Elspeth Vs. Tezzeret",
             "Game Night",
             "Duel Decks: Elspeth Vs. Kiora",
             "Tempest",
             "Unglued",
             "Scourge",
             "Duel Decks: Heroes Vs. Monsters",
             "Portal",
             "Torment",
             "Fallen Empires",
             "Legions",
             "Stronghold",
             "Unhinged",
             "Shards of Alara",
             "3rd Edition",
             "Guilds of Ravnica: Guild Kits",
             "Nemesis",
             "Portal",
             "Odyssey",
             "Coldsnap",
             "7th Edition",
             "Urza's Saga",
             "Duel Decks: Jace Vs. Chandra",
             "Planechase 2012",
             "Archenemy",
             "6th Edition",
             "Duel Decks: Merfolk Vs. Goblins",
             "Duel Decks: Garruk Vs. Liliana",
             "Urza's Legacy",
             "Duel Decks: Phyrexia Vs. The Coalition",
             "Portal II",
             "5th Edition",
             "Onslaught",
             "Timeshifted",
             "4th Edition",
             "Legends",
             "Beatdown",
             "8th Edition",
             "Antiquities",
             "Prophecy",
             "Mercadian Masques",
             "Planechase",
             "Timeshifted",
             "Duel Decks: Ajani Vs. Nicol Bolas",
             "Duel Decks: Elves Vs. Goblins",
             "Mirage",
             "Battle Royale",
             "Duel Decks: Divine Vs. Demonic",
             "The Dark",
             "Starter 1999",
             "Apocalypse",
             "Starter 1999",
             "Urza's Destiny",
             "Homelands",
             "Portal 3K",
             "Battle Royale",
             "Starter 2000",
             "Visions",
             "Portal 3K",
             "Arabian Nights",
             "Portal II",
             "Unlimited",
             "Beta",
             "Beta",
             "Alpha",
             "Alpha",
             "World Championships",
             "Throne of Eldraine Variants",
             "Promotional",
             "Duel Decks: Anthology",
             "Anthologies",
             "Promo Pack",
             "Collectors Ed",
             "Magic Origins",
             "Conflux",
             "Journey into Nyx",
             "Commander 2019",
             "Ixalan",
             "Born of the Gods",
             "Rise of the Eldrazi",
             "Commander",
             "Amonkhet",
             "Return to Ravnica",
             "Avacyn Restored",
             "Innistrad",
             "Ravnica Allegiance",
             "Shadows Over Innistrad",
             "Dark Ascension",
             "Oath of the Gatewatch",
             "2014 Core Set",
             "Core Set 2019",
             "Dragons of Tarkir",
             "Dragon's Maze",
             "Dominaria",
             "Throne of Eldraine Variants",
             "Gatecrash",
             "Modern Masters 2015",
             "Unstable",
             "2012 Core Set",
             "2010 Core Set",
             "Commander 2014",
             "Iconic Masters",
             "Alara Reborn",
             "Guilds of Ravnica: Guild Kits",
             "Hour of Devastation",
             "Shards of Alara",
             "Modern Masters 2017",
             "Battle for Zendikar",
             "Ravnica Allegiance: Guild Kits",
             "Unstable",
             "Iconic Masters",
             "Khans of Tarkir",
             "2011 Core Set",
             "Promo Pack",
             "Promo Pack",
             "Commander 2018",
             "2012 Core Set",
             "Zendikar",
             "Worldwake",
             "Commander 2013",
             "Throne of Eldraine",
             "Signature Spellbook: Gideon",
             "6th Edition",
             "3rd Edition",
             "8th Edition",
             "7th Edition",
             "5th Edition",
             "9th Edition",
             "Collectors Ed Intl",
             "4th Edition",
             "Fate Reforged",
             "Judgment",
             "Onslaught",
             "Collectors Ed",
             "Collectors Ed Intl",
             "Alliances",
             "Collectors Ed",
             "Beta",
             "Exodus",
             "Anthologies",
             "Coldsnap Theme Decks",
             "3rd Edition",
             "5th Edition",
             "Mirage",
             "Unlimited",
             "6th Edition",
             "Tempest",
             "Urza's Legacy",
             "7th Edition",
             "Anthologies",
             "Antiquities",
             "Arabian Nights",
             "Coldsnap",
             "Collectors Ed Intl",
             "Beatdown",
             "Time Spiral",
             "Prophecy",
             "Legends",
             "Mercadian Masques",
             "Urza's Saga",
             "Coldsnap Theme Decks",
             "Starter 1999",
             "Urza's Destiny",
             "Nemesis",
             "Scourge",
             "Judgment",
             "Odyssey",
             "Starter 2000",
             "Torment",
             "Vanguard",
             "Vanguard",
             "Ice Age",
             "Game Night",
             "Eldritch Moon",
             "Scars of Mirrodin",
             "Throne of Eldraine Variants",
             "2015 Core Set",
             "Gatecrash",
             "Kaladesh",
             "War of the Spark",
             "Aether Revolt",
             "Rivals of Ixalan",
             "Core Set 2020",
             "Theros",
             "Guilds of Ravnica",
             "Journey into Nyx",
             "Ravnica Allegiance: Guild Kits",
             "Explorers of Ixalan",
             "Core Set 2020",
             "2015 Core Set",
             "Battle for Zendikar",
             "Explorers of Ixalan",
             "War of the Spark",
             "Ravnica Allegiance",
             "Ravnica",
             "Future Sight",
             "World Championships",
             "9th Edition",
             "8th Edition",
             "Promotional",
             "Commander 2017",
             "Commander 2015",
             "Commander Anthology Vol. II",
             "Champions of Kamigawa",
             "Planechase",
             "Unhinged",
             "Unglued",
             "Invasion",
             "10th Edition",
             "Planeshift",
             "Mirrodin",
             "Battlebond",
             "Duel Decks: Merfolk Vs. Goblins",
             "New Phyrexia",
             "Ultimate Masters",
             "Commander Anthology",
             "Duel Decks: Elves Vs. Goblins",
             "Duel Decks: Merfolk Vs. Goblins",
             "Duel Decks: Jace Vs. Vraska",
             "Duel Decks: Elves Vs. Inventors",
             "Shadowmoor",
             "Planechase Anthology",
             "Lorwyn",
             "Modern Masters",
             "Morningtide",
             "Commander 2016",
             "Guildpact",
             "Eventide",
             "Duel Decks: Nissa Vs. Ob Nixilis",
             "Duel Decks: Garruk Vs. Liliana",
             "Duel Decks: Mind Vs. Might",
             "Fifth Dawn",
             "Saviors of Kamigawa",
             "Avacyn Restored",
             "Planar Chaos",
             "Deckmaster",
             "Duel Decks: Izzet Vs. Golgari",
             "Masters 25",
             "Champions of Kamigawa",
             "Commander 2014",
             "2013 Core Set",
             "Conflux",
             "Duel Decks: Venser Vs. Koth",
             "Planar Chaos",
             "Archenemy",
             "Signature Spellbook: Jace",
             "Conspiracy",
             "Duels of the Planeswalkers",
             "Duel Decks: Elspeth Vs. Kiora",
             "Darksteel",
             "Duel Decks: Blessed Vs. Cursed",
             "Battle Royale",
             "Betrayers of Kamigawa",
             "Modern Masters",
             "Eternal Masters",
             "Duel Decks: Nissa Vs. Ob Nixilis",
             "Conspiracy - Take the Crown",
             "Duel Decks: Ajani Vs. Nicol Bolas",
             "Duel Decks: Anthology",
             "Fallen Empires",
             "Darksteel",
             "Mirrodin Besieged",
             "Planechase",
             "Unglued",
             "Commander Anthology Vol. II",
             "2011 Core Set",
             "Duel Decks: Speed Vs. Cunning",
             "Duel Decks: Phyrexia Vs. The Coalition",
             "Legions",
             "Archenemy - Nicol Bolas",
             "Commander 2018",
             "Global Series: Jiang Yanggu & Mu Yanling",
             "Dark Ascension",
             "Unhinged",
             "Promotional",
             "World Championships",
             "Planechase 2012",
             "2010 Core Set",
             "Archenemy",
             "Duel Decks: Divine Vs. Demonic",
             "Duel Decks: Jace Vs. Chandra",
             "Duel Decks: Sorin Vs. Tibalt",
             "Duel Decks: Elspeth Vs. Tezzeret",
             "Duel Decks: Knights Vs. Dragons",
             "Commander 2015",
             "Legions",
             "Masters 25",
             "Dissension",
             "Rise of the Eldrazi",
             "Modern Event Deck",
             "Commander",
             "Commander 2013",
             "Duel Decks: Heroes Vs. Monsters",
             "Eventide",
             "Magic Origins",
             "Modern Horizons",
             "Deckmaster",
             "Duel Decks: Mind Vs. Might",
             "Duel Decks: Zendikar Vs. Eldrazi",
             "Amonkhet",
             "Ravnica",
             "Shadowmoor",
             "Shadows Over Innistrad",
             "Throne of Eldraine",
             "Masterpiece Series: Mythic Edition",
             "Ultimate Masters",
             "10th Edition",
             "Art Series",
             "Hour of Devastation",
             "Invasion",
             "Shards of Alara",
             "Duel Decks: Blessed Vs. Cursed",
             "War of the Spark JPN Planeswalkers",
             "Planechase Anthology",
             "Duel Decks: Jace Vs. Chandra",
             "Aether Revolt",
             "Duel Decks: Zendikar Vs. Eldrazi",
             "Commander Anthology",
             "Commander 2016",
             "Duel Decks: Ajani Vs. Nicol Bolas",
             "Duel Decks: Izzet Vs. Golgari",
             "Duels of the Planeswalkers",
             "8th Edition",
             "The Dark",
             "Arabian Nights",
             "Invasion",
             "Portal II",
             "Weatherlight",
             "Alpha",
             "Timeshifted",
             "World Championships",
             "Deckmaster",
             "War of the Spark JPN Planeswalkers",
             "Masterpiece Series: Mythic Edition",
             "Vanguard",
             "Apocalypse")
Excl_Excl <- c("Standard",
               "Modern",
               "Standard",
               "Standard",
               "Pioneer",
               "Standard",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "EDH",
               "Standard",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Exclude",
               "Pioneer",
               "Unclear",
               "Pioneer",
               "Pioneer",
               "Modern",
               "Pioneer",
               "Unclear",
               "Pioneer",
               "Pioneer",
               "Modern",
               "Modern",
               "Modern",
               "Pioneer",
               "Modern",
               "EDH",
               "Pioneer",
               "Pioneer",
               "Exclude",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Modern",
               "Pioneer",
               "Exclude",
               "Modern",
               "Exclude",
               "Modern",
               "Pioneer",
               "Modern",
               "Pioneer",
               "EDH",
               "Modern",
               "Modern",
               "Modern",
               "EDH",
               "Modern",
               "Exclude",
               "Modern",
               "Modern",
               "EDH",
               "Modern",
               "EDH",
               "Exclude",
               "Exclude",
               "Modern",
               "Modern",
               "Modern",
               "Modern",
               "EDH",
               "Modern",
               "Modern",
               "Exclude",
               "Exclude",
               "Exclude",
               "Unclear",
               "Exclude",
               "Modern",
               "EDH",
               "Modern",
               "Modern",
               "Modern",
               "Modern",
               "EDH",
               "Unclear",
               "Modern",
               "EDH",
               "Exclude",
               "EDH",
               "EDH",
               "Modern",
               "Exclude",
               "Exclude",
               "EDH",
               "EDH",
               "Exclude",
               "EDH",
               "Exclude",
               "Exclude",
               "Modern",
               "Exclude",
               "Exclude",
               "Modern",
               "Exclude",
               "Exclude",
               "Exclude",
               "Modern",
               "Modern",
               "Exclude",
               "Exclude",
               "Unclear",
               "Exclude",
               "Unclear",
               "EDH",
               "Pioneer",
               "EDH",
               "EDH",
               "EDH",
               "Unclear",
               "Unclear",
               "Exclude",
               "Unclear",
               "Modern",
               "Exclude",
               "Unclear",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Unclear",
               "EDH",
               "Unclear",
               "Modern",
               "Unclear",
               "EDH",
               "Exclude",
               "Exclude",
               "Unclear",
               "Unclear",
               "EDH",
               "Unclear",
               "Unclear",
               "Exclude",
               "Unclear",
               "Exclude",
               "Unclear",
               "Exclude",
               "Unclear",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Unclear",
               "Modern",
               "Exclude",
               "EDH",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Unclear",
               "EDH",
               "EDH",
               "Exclude",
               "Unclear",
               "Unclear",
               "Exclude",
               "Unclear",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "EDH",
               "Exclude",
               "Unclear",
               "Unclear",
               "Exclude",
               "Unclear",
               "Unclear",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Unclear",
               "Standard",
               "EDH",
               "Unclear",
               "Exclude",
               "EDH",
               "Exclude",
               "Pioneer",
               "EDH",
               "Pioneer",
               "EDH",
               "Pioneer",
               "Pioneer",
               "Modern",
               "EDH",
               "Pioneer",
               "Pioneer",
               "Modern",
               "Modern",
               "Standard",
               "Pioneer",
               "Modern",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Standard",
               "Pioneer",
               "Modern",
               "Unclear",
               "Modern",
               "Modern",
               "EDH",
               "EDH",
               "Modern",
               "EDH",
               "Pioneer",
               "Modern",
               "Modern",
               "Pioneer",
               "Pioneer",
               "Unclear",
               "EDH",
               "Pioneer",
               "Modern",
               "EDH",
               "EDH",
               "EDH",
               "Modern",
               "Modern",
               "Modern",
               "EDH",
               "Standard",
               "EDH",
               "Exclude",
               "Exclude",
               "Modern",
               "Exclude",
               "Exclude",
               "Modern",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Pioneer",
               "Modern",
               "Standard",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Standard",
               "Pioneer",
               "Pioneer",
               "Standard",
               "Pioneer",
               "Standard",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Standard",
               "Pioneer",
               "Pioneer",
               "Pioneer",
               "Standard",
               "Standard",
               "Modern",
               "Modern",
               "Unclear",
               "Modern",
               "Modern",
               "EDH",
               "EDH",
               "EDH",
               "EDH",
               "Modern",
               "EDH",
               "Unclear",
               "Unclear",
               "Unclear",
               "Modern",
               "EDH",
               "Modern",
               "EDH",
               "Unclear",
               "Modern",
               "Modern",
               "EDH",
               "Unclear",
               "Unclear",
               "Unclear",
               "Unclear",
               "Modern",
               "EDH",
               "Modern",
               "Modern",
               "Modern",
               "EDH",
               "Modern",
               "Modern",
               "Unclear",
               "Unclear",
               "Unclear",
               "Modern",
               "Modern",
               "Modern",
               "Modern",
               "Unclear",
               "Unclear",
               "Modern",
               "Modern",
               "EDH",
               "Modern",
               "EDH",
               "Unclear",
               "Modern",
               "EDH",
               "EDH",
               "EDH",
               "Unclear",
               "Unclear",
               "Modern",
               "Unclear",
               "Unclear",
               "Modern",
               "Modern",
               "EDH",
               "Unclear",
               "EDH",
               "Unclear",
               "Unclear",
               "Unclear",
               "Modern",
               "Modern",
               "EDH",
               "Unclear",
               "EDH",
               "Modern",
               "Unclear",
               "Unclear",
               "Unclear",
               "EDH",
               "EDH",
               "EDH",
               "Modern",
               "Unclear",
               "EDH",
               "Unclear",
               "EDH",
               "Modern",
               "EDH",
               "Unclear",
               "Unclear",
               "Unclear",
               "Unclear",
               "Unclear",
               "EDH",
               "Unclear",
               "Modern",
               "Modern",
               "Modern",
               "Unclear",
               "EDH",
               "EDH",
               "Unclear",
               "Modern",
               "Pioneer",
               "Modern",
               "Unclear",
               "Unclear",
               "Unclear",
               "Pioneer",
               "Modern",
               "Modern",
               "Pioneer",
               "Standard",
               "EDH",
               "Modern",
               "Modern",
               "Unclear",
               "Pioneer",
               "Unclear",
               "Modern",
               "Unclear",
               "Standard",
               "EDH",
               "Unclear",
               "Pioneer",
               "Unclear",
               "EDH",
               "EDH",
               "Unclear",
               "Unclear",
               "Unclear",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Exclude",
               "Standard",
               "EDH",
               "Exclude",
               "Exclude")
#Exclusions Entered - Proceed####
Exclusion <- data.frame(Set_Excl,Excl_Excl, stringsAsFactors = TRUE)
CK_Price_Comparison$Set_Group <- Exclusion$Excl_Excl[match(CK_Price_Comparison$Set,Exclusion$Set_Excl)]
CK_Price_Comparison$CK_BL <- Final_Export$BL[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$CK_BL_Backing <- round((CK_Price_Comparison$CK_BL/CK_Price_Comparison$CK_Price),2)
CK_Price_Comparison$TCG_BL_Backing <- round((CK_Price_Comparison$CK_BL/CK_Price_Comparison$TCG_Price),2)
CK_Price_Comparison$TCG_Vendors <- Final_Export$Sellers[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$BL_Desired_Amt <- Final_Export$BL_QTY[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$`F/NF` <- ""
CK_Price_Comparison <- CK_Price_Comparison[c(1,2,3,4,14,13,9,5,6,7,12,10,11,8)]
names(CK_Price_Comparison) <- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","TCG_MKT","CK_MKT","MKT_Diff","Sellers","CK_MKT_%","TCG_MKT_%","Group")
CK_Price_Excluded_Sets <- CK_Price_Comparison[which(CK_Price_Comparison$Group != "Exclude"),]
CK_Price_Excluded_Sets$TCG_Rank <- Final_Export$TCG_Rank[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Excluded_Sets$CK_Rank <- Final_Export$CK_Rank[match(CK_Price_Comparison$Key,Final_Export$Key)]

#Final Preparations For Funny Money####
#Funny_Money_Analysis_Commons <- CK_Price_Excluded_Sets[which(CK_Price_Excluded_Sets$Rarity == "C" |CK_Price_Excluded_Sets$Rarity == "L"| CK_Price_Excluded_Sets$Rarity == "S"   ),]
#Funny_Money_Analysis_Uncommons <- CK_Price_Excluded_Sets[which(CK_Price_Excluded_Sets$Rarity == "U"),]
#Funny_Money_Analysis_Rares <- CK_Price_Excluded_Sets[which(CK_Price_Excluded_Sets$Rarity == "R"),]
#Funny_Money_Analysis_Mythics <- CK_Price_Excluded_Sets[which(CK_Price_Excluded_Sets$Rarity == "M"),]
Funny_Money_Analysis <- CK_Price_Excluded_Sets
setwd("/cloud/project/Funny Money")
#csvFileName <- paste(currentDate,"CK_Credit_Commons_",".csv",sep="")
#write.csv(Funny_Money_Analysis_Commons, file=csvFileName, row.names = FALSE) 

#csvFileName <- paste(currentDate,"CK_Credit_Uncommons_",".csv",sep="")
#write.csv(Funny_Money_Analysis_Uncommons, file=csvFileName, row.names = FALSE) 

#csvFileName <- paste(currentDate,"CK_Credit_Rares_",".csv",sep="")
#write.csv(Funny_Money_Analysis_Rares, file=csvFileName, row.names = FALSE) 

#csvFileName <- paste(currentDate,"CK_Credit_Mythics_",".csv",sep="")
#write.csv(Funny_Money_Analysis_Mythics, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(currentDate,"_CK_Credit_Data",".csv",sep="")
write.csv(Funny_Money_Analysis, file=csvFileName, row.names = FALSE) 


#Export My Masterpiece####
setwd("/cloud/project/Old Trackers/High Confidence Reps")
Final_Export_1$Exclusion <- Exclusion$Excl_Excl[match(Final_Export_1$Set,Exclusion$Set_Excl)]

currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE)

#Load in Dated Reports####
Special_Rep <- Final_Export[c(1,2,3,4,7,15,17,18,10,11,12)]
Yesterday <- read_csv("/cloud/project/Reports/High Confidence Reps/2020-01-27_Premium.csv", 
                      col_types = cols(`F/NF` = col_character(), 
                                       Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Week_Ago <- read_csv("/cloud/project/Reports/High Confidence Reps/2020-01-22_Premium.csv", 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Month_Ago <-read_csv("/cloud/project/Reports/High Confidence Reps/2019-12-28_Premium.csv", 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
#View(Week_Ago)
Yesterday$Sellers <- as.numeric(Yesterday$Sellers)
Special_Rep$Sellers<- as.numeric(Special_Rep$Sellers)
Week_Ago$Sellers <- as.numeric(Week_Ago$Sellers)
Month_Ago$Sellers <- as.numeric(Month_Ago$Sellers)
Key_Amalgamation <- NULL
Key_Amalgamation <- cbind(Special_Rep$Key, Yesterday$Key)
Key_Amalgamation <- cbind(Key_Amalgamation, Week_Ago$Key)
Key_Amalgamation <- cbind(Key_Amalgamation, Month_Ago$Key)
Key_Amalgamation <- as.data.frame(Key_Amalgamation)
library(reshape2)
Key_Amalgamation <- melt(Key_Amalgamation, id.vars=c(),var='Key')
Unique_Keys <- unique(Key_Amalgamation$value)
Unique_Keys <- as.data.frame(Unique_Keys)
library(readxl)
Names <- read_excel("/cloud/project/Reports/High Confidence Reps/Names(Jan 25).xlsx")
Names <- as.data.frame(Names)
Unique_Keys$Name <- Names$Card[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Set <- Names$Set[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Rarity <- Names$Rarity[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Foil <- Names$`F/NF`[match(Unique_Keys$Unique_Keys,Names$Key)]
#Buy List growth Report####
BuyList_Growth <- Unique_Keys
BuyList_Growth$Todays_BL <- Special_Rep$BL[match(BuyList_Growth$Unique_Keys,Special_Rep$Key)]
BuyList_Growth$Yesterday_BL <- Yesterday$BL[match(BuyList_Growth$Unique_Keys,Yesterday$Key)]
BuyList_Growth$Week_Ago_BL <- Week_Ago$BL[match(BuyList_Growth$Unique_Keys,Week_Ago$Key)]
BuyList_Growth$Month_Ago_BL <- Month_Ago$BL[match(BuyList_Growth$Unique_Keys,Month_Ago$Key)]

BuyList_Growth$Yesterday_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Yesterday_BL)/BuyList_Growth$Yesterday_BL,4)
BuyList_Growth$Week_Ago_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Week_Ago_BL)/BuyList_Growth$Week_Ago_BL,4)
BuyList_Growth$Month_Ago_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Month_Ago_BL)/BuyList_Growth$Month_Ago_BL,4)

BuyList_Growth$BuyList_Backing <- Funny_Money_Analysis$CK_MKT[match(BuyList_Growth$Unique_Keys, Funny_Money_Analysis$Key)]
BuyList_Growth$BuyList_Backing <- 1 - round((BuyList_Growth$BuyList_Backing - BuyList_Growth$Todays_BL)/BuyList_Growth$BuyList_Backing,4)

BuyList_Growth[is.na(BuyList_Growth)] <- ""
BuyList_Growth[BuyList_Growth == "Inf"] <- ""

Consistent_BuyLists <- subset(BuyList_Growth, is.na(BuyList_Growth$Todays_BL) != TRUE & is.na(BuyList_Growth$Yesterday_BL) != TRUE & is.na(BuyList_Growth$Week_Ago_BL) != TRUE & is.na(BuyList_Growth$Month_Ago_BL) != TRUE & is.na(BuyList_Growth$Yesterday_BL_Chg) != TRUE & is.na(BuyList_Growth$Week_Ago_BL_Chg) != TRUE & is.na(BuyList_Growth$Month_Ago_BL_Chg) != TRUE)


#View(BuyList_Growth)
#Vendor Growth####
Vendor_Growth <- Unique_Keys
Vendor_Growth$Todays_Sellers <- Special_Rep$Sellers[match(Vendor_Growth$Unique_Keys,Special_Rep$Key)]
Vendor_Growth$Yesterday_Sellers <- Yesterday$Sellers[match(Vendor_Growth$Unique_Keys,Yesterday$Key)]
Vendor_Growth$Week_Ago_Sellers <- Week_Ago$Sellers[match(Vendor_Growth$Unique_Keys,Week_Ago$Key)]
Vendor_Growth$Month_Ago_Sellers <- Month_Ago$Sellers[match(Vendor_Growth$Unique_Keys,Month_Ago$Key)]

Vendor_Growth$Yesterday_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Yesterday_Sellers)/Vendor_Growth$Yesterday_Sellers,4)*(-1)
Vendor_Growth$Week_Ago_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Week_Ago_Sellers)/Vendor_Growth$Week_Ago_Sellers,4)*(-1)
Vendor_Growth$Month_Ago_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Month_Ago_Sellers)/Vendor_Growth$Month_Ago_Sellers,4)*(-1)
Consistent_Vendors <- subset(Vendor_Growth, is.na(Vendor_Growth$Todays_Sellers) != TRUE & is.na(Vendor_Growth$Yesterday_Sellers) != TRUE & is.na(Vendor_Growth$Week_Ago_Sellers) != TRUE & is.na(Vendor_Growth$Month_Ago_Sellers) != TRUE & is.na(Vendor_Growth$Yesterday_Sellers_Chg) != TRUE & is.na(Vendor_Growth$Week_Ago_Sellers_Chg) != TRUE & is.na(Vendor_Growth$Month_Ago_Sellers_Chg) != TRUE)

View(Consistent_Vendors)

#Demand Growth####
TCG_Growth <- Unique_Keys
TCG_Growth$Todays_TCG <- Special_Rep$TCG_Rank[match(TCG_Growth$Unique_Keys,Special_Rep$Key)]
TCG_Growth$Yesterday_TCG <- Yesterday$TCG_Rank[match(TCG_Growth$Unique_Keys,Yesterday$Key)]
TCG_Growth$Week_Ago_TCG <- Week_Ago$TCG_Rank[match(TCG_Growth$Unique_Keys,Week_Ago$Key)]
TCG_Growth$Month_Ago_TCG <- Month_Ago$TCG_Rank[match(TCG_Growth$Unique_Keys,Month_Ago$Key)]

TCG_Growth$Todays_TCG <- as.numeric(TCG_Growth$Todays_TCG)
TCG_Growth$Yesterday_TCG <- as.numeric(TCG_Growth$Yesterday_TCG)
TCG_Growth$Week_Ago_TCG <- as.numeric(TCG_Growth$Week_Ago_TCG)
TCG_Growth$Month_Ago_TCG <- as.numeric(TCG_Growth$Month_Ago_TCG)

TCG_Growth$Yesterday_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Yesterday_TCG)/TCG_Growth$Yesterday_TCG,4)*(-1)
TCG_Growth$Week_Ago_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Week_Ago_TCG)/TCG_Growth$Week_Ago_TCG,4)*(-1)
TCG_Growth$Month_Ago_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Month_Ago_TCG)/TCG_Growth$Month_Ago_TCG,4)*(-1)
Consistent_Sellers <- subset(TCG_Growth, is.na(TCG_Growth$Todays_TCG) != TRUE & is.na(TCG_Growth$Yesterday_TCG) != TRUE & is.na(TCG_Growth$Week_Ago_TCG) != TRUE & is.na(TCG_Growth$Month_Ago_TCG) != TRUE & is.na(TCG_Growth$Yesterday_TCG_Chg) != TRUE & is.na(TCG_Growth$Week_Ago_TCG_Chg) != TRUE & is.na(TCG_Growth$Month_Ago_TCG_Chg) != TRUE)

#View(TCG_Growth)
#View(Consistent_Sellers)

#Export growth Reports####
setwd("/cloud/project/Reports/Growth Reports")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_C_BuyList_Growth",".csv",sep="")
write.csv(Consistent_BuyLists, file=csvFileName, row.names = FALSE)
csvFileName <- paste(currentDate,"_C_Demand_Growth",".csv",sep="")
write.csv(Consistent_Sellers, file=csvFileName, row.names = FALSE)
csvFileName <- paste(currentDate,"_C_Vendor_Growth",".csv",sep="")
write.csv(Consistent_Vendors, file=csvFileName, row.names = FALSE)
#Under Construction####
#Top Movers - Consolidated####
View(Consistent_BuyLists)
Consistent_BuyLists$Yesterday_BL_Chg <- as.numeric(Consistent_BuyLists$Yesterday_BL_Chg)
Consistent_BuyLists$Week_Ago_BL_Chg <- as.numeric(Consistent_BuyLists$Week_Ago_BL_Chg)
Consistent_BuyLists$Month_Ago_BL_Chg <- as.numeric(Consistent_BuyLists$Month_Ago_BL_Chg)
Consistent_BuyLists$Todays_BL <- as.numeric(Consistent_BuyLists$Todays_BL)
Consistent_BuyLists$Yesterday_BL <- as.numeric(Consistent_BuyLists$Yesterday_BL )
Consistent_BuyLists$Week_Ago_BL <- as.numeric(Consistent_BuyLists$Week_Ago_BL)
Consistent_BuyLists$Month_Ago_BL <- as.numeric(Consistent_BuyLists$Month_Ago_BL)
Consistent_BuyLists$Foil <- as.factor(Consistent_BuyLists$Foil)

#Today's Movers####
CB_Short <- Consistent_BuyLists[which(Consistent_BuyLists$Todays_BL > 1.5),]
CB_Short_F <- CB_Short[which(CB_Short$Foil == "FOIL"),]
CB_Short_NF <- CB_Short[which(CB_Short$Foil != "FOIL"),]
CB_Short_NF <- CB_Short_NF[which(CB_Short_NF$BuyList_Backing != ""),]
CB_Short_F <- CB_Short_F[order(-CB_Short_F$Yesterday_BL_Chg),]
CB_Short_NF <- CB_Short_NF[order(-CB_Short_NF$Yesterday_BL_Chg),]

CB_Short <- Consistent_Sellers[which(Consistent_Sellers$Todays_BL > 1.5),]
CB_Short_F <- CB_Short[which(CB_Short$Foil == "FOIL"),]
CB_Short_NF <- CB_Short[which(CB_Short$Foil != "FOIL"),]
CB_Short_NF <- CB_Short_NF[which(CB_Short_NF$BuyList_Backing != ""),]
CB_Short_F <- CB_Short_F[order(-CB_Short_F$Yesterday_BL_Chg),]
CB_Short_NF <- CB_Short_NF[order(-CB_Short_NF$Yesterday_BL_Chg),]