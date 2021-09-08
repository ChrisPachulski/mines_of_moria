install.packages("mailR")
library(mailR)
currentDate <- Sys.Date()
send.mail(
  from="pachun95@gmail.com", to = "cjpach@mac.com", 
  subject = paste("Buy List Reports for ",currentDate,sep =""),
  body = "The message you want to send",
  smtp = list(host.name = "smtp.gmail.com",
              port = 465,
              user.name = "pachun95@gmail.com",
              passwd = "Gibson95", 
              #ssl = T
              #tls = T), 
              authenticate = T,  send = T
  ))
