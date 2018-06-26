library(data.table)
library(reshape2)
library(tidyr)
library(dplyr)
library(plyr)
library(ctv)
library(SnowballC)
library(tm)
library(wordcloud)
library(qdap)
require(xlsx)
# d1 <- read.csv("break1.csv", stringsAsFactors = F, na.strings = c("", "NA"))
# d2 <- read.csv("break2.csv", stringsAsFactors = F, na.strings = c("", "NA"))
# d3 <- read.csv("break3.csv",stringsAsFactors = F, na.strings = c("", "NA"))
# d4 <- read.csv("report1529825809499.csv",stringsAsFactors = F, na.strings = c("", "NA"))
# d5 <- read.csv("report1529826232716.csv",stringsAsFactors = F, na.strings = c("", "NA"))
# d6 <- read.csv("report1529827258705.csv",stringsAsFactors = F, na.strings = c("", "NA"))
# d7 <- read.csv("report1529827258706.csv", stringsAsFactors = F, na.strings = c("", "NA"))
# data_fin <- rbind(d1,d2,d3,d4,d5,d6,d7)
# write.csv(data_fin, "all_list.csv", row.names = F)
data_fin <- read.csv("all_list.csv",stringsAsFactors = F, na.strings = c("", "NA"))
paid_data <- read.csv("All Paid data since June 2016.csv", stringsAsFactors = F, na.strings = c("", "NA"))
#tt <- apply(paid_data,1, function (x) {ifelse ((is.na(x) == TRUE), "NA1", x)})
paid_data$Company.ID[is.na(paid_data$Company.ID) == T] <- "NA1"
paid_data$Phone[is.na(paid_data$Phone) == T] <- "NA1"
paid_data$EMAIL[is.na(paid_data$EMAIL) == T] <- "NA1"
paid_data$Company.ID <- trimws(paid_data$Company.ID, which = "both")
paid_data$Phone <- trimws(paid_data$Phone, which = "both")
paid_data$EMAIL <- trimws(paid_data$EMAIL, which = "both")
#########
# data_fin$Phone <- data_fin$Primary.Contact.Phone
# data_fin$EMAIL <- data_fin$Primary.Contact.Email
# data_fin$Primary.Contact.Email <- NULL
# data_fin$Primary.Contact.Phone <- NULL
data_fin$Company.ID[is.na(data_fin$Company.ID) == T] <- "NA2"
data_fin$Phone[is.na(data_fin$Phone) == T] <- "NA2"
data_fin$EMAIL[is.na(data_fin$EMAIL) == T] <- "NA2"

data_fin$Company.ID <- trimws(data_fin$Company.ID, which = "both")
data_fin$Phone <- trimws(data_fin$Phone, which = "both")
data_fin$EMAIL <- trimws(data_fin$EMAIL, which = "both")
####remove matching customers
data1 <- anti_join(data_fin, paid_data, by = "Company.ID")
data2 <- anti_join(data1, paid_data, by = "Phone")
data3 <- anti_join(data2, paid_data, by = "EMAIL")
write.csv(data3, "unpaid_list.csv", row.names = F)
### word cloud
docs <- Corpus(VectorSource(paid_data$Company.Name))
docs <- tm_map(docs,tolower)
docs <- tm_map(docs,stripWhitespace)
#docs <- tm_map(docs,removeWords, stopwords("english"))
#docs <- tm_map(docs,removeNumbers)
#docs <- tm_map(docs,removePunctuation)
require(quanteda)
paid_data$Company.Name <- removeNumbers(paid_data$Company.Name)
paid_data$Company.Name <- removePunctuation(paid_data$Company.Name)
paid_data$Company.Name <- stripWhitespace(paid_data$Company.Name)
mydata.corpus <- corpus(paid_data$Company.Name, 
                        dovcars = paid_data[-which(names(paid_data)=="Company.Name")])
mydata.dtm <- dfm(mydata.corpus)
#mydata.dtm
top_words <- data.frame(topfeatures(mydata.dtm, 1000))
top_words$word <- row.names(top_words)
row.names(top_words) <- NULL
colnames(top_words) <- c("Freq", "Word")
write.csv(top_words, "top_words.csv", row.names = F)
###########
unpaid <- read.csv("unpaid_list.csv", stringsAsFactors = F)
unpaid$Account.Name <- removeNumbers(unpaid$Account.Name)
unpaid$Account.Name <- tolower(unpaid$Account.Name)
unpaid$Account.Name <- removePunctuation(unpaid$Account.Name)
unpaid$Account.Name <- stripWhitespace(unpaid$Account.Name)
var2 <- unpaid$Account.Name
var1 <- top_words$Word
var11 <- var1[nchar(var1) >= 3]
var12 <- var11[1:500]
#tt <- var2[order(-sapply(var2, function(x) sum(var1 %in% x)))]
#t1 <- unpaid[var1 %in% unpaid$Account.Name,]

##### 
t1_web <- t1[is.na(t1$Website..3rd.Party.)== F,]
t1_web <- t1_web[order(t1_web$No.of.Logins, decreasing = T),]
write.csv(t1_web, "shortlist.csv", row.names = F)
####

pp <- unpaid[is.na(unpaid$Website..3rd.Party.)== F,]
#pp1 <- pp[var1 %in% pp$Account.Name,]
pp1 <- pp[unique(grep(paste(var12,collapse="|"), pp$Account.Name, value = FALSE)),]
#vv <- var1[1:50]
#ms <- unique (grep(paste(vv,collapse="|"),  pp$Account.Name, value=FALSE))

pp1<- pp1[order(pp1$No.of.Logins, decreasing = T),]
write.csv(pp1, "shortlist1.csv", row.names = F)

####
pp_2 <- pp1[pp1$No.of.Logins == 2,]
pp_1 <- pp1[pp1$No.of.Logins == 1,]
pp_0 <- pp1[pp1$No.of.Logins == 0,]
key <- read.csv("key_words.csv", stringsAsFactors = F)
key$keyword <- stripWhitespace(key$keyword)
key$keyword <- tolower(key$keyword)
pp_2_key <- pp_2[unique(grep(paste(key$keyword,collapse="|"), pp_2$Account.Name, value = FALSE)),]
pp_1_key <- pp_1[unique(grep(paste(key$keyword,collapse="|"), pp_1$Account.Name, value = FALSE)),]
pp_0_key <- pp_0[unique(grep(paste(key$keyword,collapse="|"), pp_0$Account.Name, value = FALSE)),]
#tomatch <- paste(key$keyword, collapse = "|")
pp_21<- anti_join(pp_2, pp_2_key)
pp_11<- anti_join(pp_1, pp_1_key)
pp_01<- anti_join(pp_0, pp_0_key)
###
pp_key <- rbind(pp_2_key, pp_1_key, pp_0_key)
pp_wo_key <- rbind(pp_21, pp_11, pp_01)
pp_login <- pp1[!(pp1$No.of.Logins %in% c(0,1,2)), ]
write.csv( pp_login, "more_login.csv", row.names = F)
write.csv( pp_key, "login2_keyword.csv", row.names = F)
write.csv( pp_wo_key, "login2_wo_key.csv", row.names = F)