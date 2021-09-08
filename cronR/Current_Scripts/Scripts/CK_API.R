#install.packages(jsonlite)
library(jsonlite)
library(readr)
library(mailR)
#API ABuse####
CK_Buylist <- fromJSON("https://api.cardkingdom.com/api/pricelist")
CK_Buylist <- as.data.frame(CK_Buylist)
CK_Buylist$Key <- paste(CK_Buylist$data.name,CK_Buylist$data.edition,sep="")

All_Cards <- read_csv("/home/cujo253//Reports/All_Cards_MB1.csv", col_types = cols(`F/NF` = col_character()))
All_Cards[is.na(All_Cards)] <- ""
All_Cards$Semi <- paste(All_Cards$name,All_Cards$Set," ",All_Cards$`F/NF`,sep="")
All_Cards$Semi <- trimws(All_Cards$Semi)
CK_Buylist$data.rarity <- All_Cards$Rarity[match(CK_Buylist$Key,All_Cards$Semi)]
#Buy List Cajiguring for MAXIMUM Confusion####
My_CK_alter <- CK_Buylist[,c(6,8,15,9,10,11,12,13)]
My_CK_alter[is.na(My_CK_alter)] <- ""
My_CK_alter$data.is_foil <- ifelse(My_CK_alter$data.is_foil == "true"," FOIL","")
My_CK_alter$data.edition <- ifelse(grepl(" Foil",My_CK_alter$data.edition), substr(My_CK_alter$data.edition,1,nchar(My_CK_alter$data.edition)-5), My_CK_alter$data.edition)
My_CK_alter$data.Key <- paste(My_CK_alter$data.name,My_CK_alter$data.edition,My_CK_alter$data.is_foil, sep="")
My_CK_alter$data.rarity <- All_Cards$Rarity[match(My_CK_alter$data.Key,All_Cards$Semi)]
My_CK_alter$data.Key <- paste(My_CK_alter$data.name,My_CK_alter$data.edition,My_CK_alter$data.rarity,My_CK_alter$data.is_foil, sep="")
My_CK_alter <- My_CK_alter[,c(9,1,2,3,4,5,6,7,8)]
names(My_CK_alter) <- c("Key", "Card_Name","Edition","Rarity","Foil","CK_Retail","CK_Inventory","CK_BL_Offer","CK_Desired_Qty")
My_CK_alter$CK_BL_Offer <- as.numeric(as.character(My_CK_alter$CK_BL_Offer))
My_CK_alter$CK_Retail <- as.numeric(as.character(My_CK_alter$CK_Retail))
My_CK_alter$BL_Ratio <- round((My_CK_alter$CK_BL_Offer/My_CK_alter$CK_Retail),2)
My_CK_alter$CK_Inventory <- as.numeric(as.character(My_CK_alter$CK_Inventory))
My_CK_alter$CK_Desired_Qty <- as.numeric(as.character(My_CK_alter$CK_Desired_Qty))
My_CK_alter$QTY_Ratio <- round((My_CK_alter$CK_Inventory/My_CK_alter$CK_Desired_Qty),2)
My_CK_alter$QTY_Ratio <- ifelse(My_CK_alter$QTY_Ratio == "Inf","",My_CK_alter$QTY_Ratio )
CK_Alter_NF <- My_CK_alter[which(My_CK_alter$Foil=="" & My_CK_alter$Rarity != ""),]
CK_Alter_F <- My_CK_alter[which(My_CK_alter$Foil!="" | My_CK_alter$Rarity == ""),]
CK_Alter_NF <- CK_Alter_NF[order(-CK_Alter_NF$BL_Ratio),]
CK_Alter_F <- CK_Alter_F[order(-CK_Alter_F$BL_Ratio),]
View(CK_Alter_NF)
#CSV Exports####
setwd("/home/cujo253/Reports/CK_JSON_EXTRACT/")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_NF_CardKingdom_Buy_List",".csv",sep="")
write.csv(CK_Alter_NF, file=csvFileName, row.names = FALSE)
csvFileName <- paste(currentDate,"_F&NF_CardKingdom_Buy_List",".csv",sep="")
write.csv(My_CK_alter, file=csvFileName, row.names = FALSE)
#Email Lists####
CK_NF_Export <- paste("/home/cujo253/Reports/CK_JSON_EXTRACT/",currentDate,"_NF_CardKingdom_Buy_List.csv", sep ="")
CK_Export <- paste("/home/cujo253/Reports/CK_JSON_EXTRACT/",currentDate,"_F&NF_CardKingdom_Buy_List.csv", sep ="")

Email_List <- c("jimwest@gmail.com", "the.inn.data.dump@gmail.com","shiznat29@hotmail.com","smosharo@gmail.com","chicagostylegaming@gmail.com","hargismb@yahoo.com",
                "Wyang830@aol.com", "smallwcn@gmail.com", "goneaway64@hotmail.com", "ericknsy@gmail.com", "cjpach@mac.com","vittorio.giovara@gmail.com","c.traarbach@live.com","pastorkenandrs@gmail.com", "hiroshik22@gmail.com","kyle.brecht@gmail.com",
                "Kikkyoseraph@gmail.com","andreiklepatch@gmail.com","Phil.maise@gmail.com","kyleac117@gmail.com","J.medina.mtg@gmail.com","brendanmoro@gmail.com","ericwowacc@live.com","irishmtgbuyer@gmail.com","Lucalo4444@gmail.com","Emmmzyne@gmail.com",
                "stefank.1996@live.at","martin.hughes.au@gmail.com","A88kim@gmail.com","midnightgamesmtg@gmail.com","foophil@yahoo.com","adam@chaostibet.com","Jobenour3@gmail.com","GraveTitan19@gmail.com","jhardaway8887@yahoo.com","Njenkinsneary@gmail.com",
                "Blake_miller@hotmail.com","jayteeaxejay@gmail.com","heliashigh@gmail.com","thatoreoguy@gmail.com")
Email_List <- as.data.frame(Email_List)
Foil_Inclusion <- c("teoh.kev@hotmail.com")
Foil_Inclusion <- as.data.frame(Foil_Inclusion)
Practice_List <- c("adam@chaostibet.com")
Practice_List <- as.data.frame(Practice_List)

for (recipient in Email_List$Email_List){
  sender <- "wolfoftinstreet@gmail.com"
  send.mail(from = sender,
            to = print(recipient),
            subject = paste("CardKingdom Buylist (NF) For",currentDate,sep=" "),
            body = "Hello All, 
            
Well, turns out, when your work is tightly tied to the stock market, and the stock market pulls an 'Old Spice' Swan Dive, things get hectic.

The Links provided below for growth and CK vs TCG Pricing will update daily, around 9am EST. These links will auto-refresh with new data so there will not be a need for daily emails, so long as you keep ahold of these links.

In case you haven't caught on, CardKingdom's Buy List has plummeted. It is not the baseline it was even a week ago. This indicates from the outset that speculation just became the wild west in terms of risk minimization.

As you take into account everything that's going on in, please just keep this in mind, and I hope you all stay safe.


All google sheet links are view only to ensure no accidental edits/deletes.
            
PLEASE NOTE, if you go to 'File -> Make a Copy' you will have access for yourself to play with filters as you wish and dig around as you please.
            
There is a data guide above my signature near the end of the email which, hopefully, may clarify some things for folks.
            
            
Growth Reports -> https://docs.google.com/spreadsheets/d/1d3EIrYG7rif8G7hqKvwuOThbI-trIJQwcUmVlsfSCmM/edit?usp=sharing

CK Pricing vs TCG Pricing <- https://docs.google.com/spreadsheets/d/15bKPEFkO0sRaa68tof9VhLgvptWLJH68S92zbN-9f8c/edit?usp=sharing

CK vs TCG Explanation: As CK's sales begin to slow, they're slowly lowering the retail cost on a number of cards. This report attempts to identify and present those ooportunities for you.
                      Ideally, This can help you get out of credit, or better yet, allow you a little extra leverage in investing should you have that ability during this time.
            
My Personal Pick's of the Day -> There haven't been any for two weeks


            
Please see the attached CSV for Cardkingdom's Buy List for all Non-Foil Cards (if you would like foils as well, contact me and we can work something out). 
            
            
If this is your first time reviewing this, please see the link below which currently acts as the data guide for all reports.
            
Data Guide: https://docs.google.com/spreadsheets/d/1aE8S7_-cjlFExlX_W0wxXxB5B6r-kisEgbGU4AJwd4w/edit?usp=sharing
            
            
Cheers,
Wolf of Tin Street
            
            
If you enjoy receiving content, please consider donating at: https://www.patreon.com/ban_community/posts , everything I send out is  free for use, but if you feel like giving back it is much appreciated. 
Should you have any questions, concerns, political statements, or want to receive further additional content, please feel free to email me at wolfoftinstreet@gmail.com and I'll get back to you as soon as I can... or I'll ban you. 
            
Coin toss, really.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "wolfoftinstreet@gmail.com",            
                        passwd = "BAN2020!", ssl = TRUE),
            attach.files = c(CK_NF_Export),
            authenticate = TRUE,
            send = TRUE)
}

for (recipient in Foil_Inclusion$Foil_Inclusion){
  sender <- "wolfoftinstreet@gmail.com"
  send.mail(from = sender,
            to = print(recipient),
            subject = paste("CardKingdom Buylist (NF) For",currentDate,sep=" "),
            body = "Hello All, 
            
            Well, turns out, when your work is tightly tied to the stock market, and the stock market pulls an 'Old Spice' Swan Dive, things get hectic.
            
            The Links provided below for growth and CK vs TCG Pricing will update daily, around 9am EST. These links will auto-refresh with new data so there will not be a need for daily emails, so long as you keep ahold of these links.
            
            In case you haven't caught on, CardKingdom's Buy List has plummeted. It is not the baseline it was even a week ago. This indicates from the outset that speculation just became the wild west in terms of risk minimization.
            
            As you take into account everything that's going on in, please just keep this in mind, and I hope you all stay safe.
            
            
            All google sheet links are view only to ensure no accidental edits/deletes.
            
            PLEASE NOTE, if you go to 'File -> Make a Copy' you will have access for yourself to play with filters as you wish and dig around as you please.
            
            There is a data guide above my signature near the end of the email which, hopefully, may clarify some things for folks.
            
            
            Growth Reports -> https://docs.google.com/spreadsheets/d/1d3EIrYG7rif8G7hqKvwuOThbI-trIJQwcUmVlsfSCmM/edit?usp=sharing
            
            CK Pricing vs TCG Pricing <- https://docs.google.com/spreadsheets/d/15bKPEFkO0sRaa68tof9VhLgvptWLJH68S92zbN-9f8c/edit?usp=sharing
            
            CK vs TCG Explanation: As CK's sales begin to slow, they're slowly lowering the retail cost on a number of cards. This report attempts to identify and present those ooportunities for you.
            Ideally, This can help you get out of credit, or better yet, allow you a little extra leverage in investing should you have that ability during this time.
            
            My Personal Pick's of the Day -> There haven't been any for two weeks
            
            
            
            Please see the attached CSV for Cardkingdom's Buy List for all Non-Foil Cards (if you would like foils as well, contact me and we can work something out). 
            
            
If this is your first time reviewing this, please see the link below which currently acts as the data guide for all reports.
            
Data Guide: https://docs.google.com/spreadsheets/d/1aE8S7_-cjlFExlX_W0wxXxB5B6r-kisEgbGU4AJwd4w/edit?usp=sharing
            
            
Cheers,
Wolf of Tin Street
            
            
If you enjoy receiving content, please consider donating at: https://www.patreon.com/ban_community/posts , everything I send out is  free for use, but if you feel like giving back it is much appreciated. 
Should you have any questions, concerns, political statements, or want to receive further additional content, please feel free to email me at wolfoftinstreet@gmail.com and I'll get back to you as soon as I can... or I'll ban you. 
            
Coin toss, really.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "wolfoftinstreet@gmail.com",            
                        passwd = "BAN2020!", ssl = TRUE),
            attach.files = c(CK_Export),
            authenticate = TRUE,
            send = TRUE)
}
