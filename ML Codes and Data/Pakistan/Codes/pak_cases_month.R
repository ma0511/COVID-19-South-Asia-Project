#################################################################
#################################################################
######### Balancing Lives and Livelihoods using Contagion Risk 
######### Based COVID-19 Management for Low-Income
######### Countries: A Study on Bangladesh
######### Pakistan (Sindh) Data
######### Updated Data
#################################################################
################################################################# 

###################
# Load the packages 
###################

library("ggplot2")    # install.packages("ggplot2")
library("dplyr")      # install.packages("dplyr")
library("haven")      # install.packages("haven")
library("stargazer")  # install.packages("stargazer")   
library("labelled")   # install.packages("labelled")
library("tidyr")      # install.packages("tidyr")
library("caret")      # install.packages("caret")
library("lubridate")  # install.packages("lubridate")
library("mlbench")    # install.packages("mlbench")
library("randomForest")  
library("zoo")
library("stringr")
library("purrr")
library("MLeval")
library("scales")
library("pROC")
library("AUC")

########################
# User Defined Functions
########################

firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

Matt_Coef <- function (conf_matrix)
{
  TP <- conf_matrix$table[1,1]
  TN <- conf_matrix$table[2,2]
  FP <- conf_matrix$table[1,2]
  FN <- conf_matrix$table[2,1]
  
  mcc_num <- (TP*TN - FP*FN)
  mcc_den <- 
    as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
  
  mcc_final <- mcc_num/sqrt(mcc_den)
  return(mcc_final)
}

signature=function(x){
  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
  return(sig)
}
partialMatch=function(x,y,levDist=0.1){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))
  return(matched)
}

#########################
# Read the Pakistani data 
#########################

setwd("~/Research/Shonchoy_Covid-19/Pak_data")

covid.pak <- haven::read_dta("pakistan_index.dta")

df <- covid.pak

################
# Clean the data  
################

##... Format the date variable 

df$date2 <- as.Date(df$date, format = "%m%d%Y")

# View(df[, c("district", "daily_cases", "daily_death", "date", "date2")])

##... Create the month, week, and day variables

df$month <- lubridate::month(df$date2, label = TRUE, abbr = FALSE)
df$day <- lubridate::wday(df$date2, label = TRUE, abbr = FALSE)
df$week <- lubridate::week(df$date2)
df$year <- lubridate::year(df$date2)

df$week <- factor(df$week)
df$year <- factor(df$year)

##... Select the relevant variables 

df <- dplyr::select(df, district, daily_cases, daily_death, date2, cr_index_pakistan_1, day, week, month, year)

##... Rename variables

df <- df %>% dplyr::rename(cases = daily_cases, deaths = daily_death, date = date2, index = cr_index_pakistan_1)

##... District Names in Sindh 

# unique(df$district); length(unique(df$district))

##... Arrange the data

df <- dplyr::arrange(df, district, date)

##... Earliest date for each district 

summary(df$date)

# df %>% 
#   dplyr::group_by(district) %>% 
#   dplyr::slice(which.min(date))

##... Latest date for each district 

# df %>% 
#   dplyr::group_by(district) %>% 
#   dplyr::slice(which.max(date))

##... Analyse Missing 

sum(is.na(df$cases))
sum(is.na(df$deaths))

##... Summary Statistics for covid-19 cases and deaths 

summary(df$cases)
summary(df$deaths)

sum(df$cases)
sum(df$deaths)

##... Find the latest date for each district. 

df1 <- df %>%
  dplyr::group_by(district) %>%
  dplyr::summarise(max_date = max(date, na.rm = TRUE))

##################################
# Append the updated Covid-19 data
##################################

df <- dplyr::select(df, district, date, cases, deaths)

setwd("C:/Users/User/Documents/Research/Shonchoy_Covid-19/Updated_data")
covid.pak.1 <- read.csv("sindh_covid_updated.csv", header = TRUE, as.is = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
covid.pak.2 <- read.csv("sindh_covid_updated_errata.csv", header = TRUE, as.is = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

df <- rbind.data.frame(df, covid.pak.1, covid.pak.2)

##... Create the month, week, and day variables (again)

df$date <- as.Date(df$date)

df$month <- lubridate::month(df$date, label = TRUE, abbr = FALSE)
df$day <- lubridate::wday(df$date, label = TRUE, abbr = FALSE)
df$week <- lubridate::week(df$date)
df$year <- lubridate::year(df$date)

df$week <- factor(df$week)
df$year <- factor(df$year)

#################################
# Merge the revised CR-index data
#################################

##... Read the data 

setwd("C:/Users/User/Documents/Research/Shonchoy_Covid-19/Revised_CR_Index_Jun2022")
cr.index.pk <- haven::read_dta("Pakistan.dta")

unique(cr.index.pk$district)
unique(df$district)[!(unique(df$district) %in%  unique(cr.index.pk$district))]

##... Remove data from Korangi and Sujawal

df <- dplyr::filter(df, !(district %in% c("Korangi", "Sujawal")))

##... Merge the data

# cr.index.pk.district <- data.frame(district = unique(cr.index.pk$district), index = unique(cr.index.pk$cr_index_pakistan_1))
cr.index.pk.district <- cr.index.pk %>% dplyr::group_by(district) %>% dplyr::summarise(index = mean(cr_index_pakistan_1))

unique(df$district)
unique(cr.index.pk.district$district)
unique(df$district)[!(unique(df$district) %in%  unique(cr.index.pk.district$district))]
unique(cr.index.pk.district$district)[!(unique(cr.index.pk.district$district) %in%  unique(df$district))]

cr.index.pk.district[cr.index.pk.district$district == "Jccobabad", c("district")] <- "Jacobabad"
cr.index.pk.district[cr.index.pk.district$district == "Malir", c("district")] <- "Karachi Malir"
cr.index.pk.district[cr.index.pk.district$district == "East", c("district")] <- "Karachi East"
cr.index.pk.district[cr.index.pk.district$district == "West", c("district")] <- "Karachi West"
cr.index.pk.district[cr.index.pk.district$district == "Central", c("district")] <- "Karachi Central"
cr.index.pk.district[cr.index.pk.district$district == "South", c("district")] <- "Karachi South"
cr.index.pk.district[cr.index.pk.district$district == "T.M Khan", c("district")] <- "Tando Muhammad Khan"
cr.index.pk.district[cr.index.pk.district$district == "N. Feroze", c("district")] <- "Nowshero Feroze"
cr.index.pk.district[cr.index.pk.district$district == "S.B.A", c("district")] <- "Shaheed Banazir Abad"
cr.index.pk.district[cr.index.pk.district$district == "Kambar", c("district")] <- "Shahdadkot" # https://en.wikipedia.org/wiki/Qambar_Shahdadkot_District

df <- merge(df, cr.index.pk.district, by = "district", all.x = TRUE)

##... Arrange the data

df <- dplyr::arrange(df, district, cases, deaths)

summary(df$cases)
summary(df$deaths)
summary(df$date)

########################################################################
##... Predictive Performance: Validation Set Approach Using Monthly Data
########################################################################

df.pak <- df
set.seed(10)

##... Data Preparation 

df1 <- df.pak %>%
  dplyr::select(cases, district, month, year, index) %>%
  dplyr::group_by(district, month, year) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(month, year) %>% dplyr::mutate(total_cases = sum(cases))
df1 <- transform(df1, cases_pct = (cases/total_cases) * 100)

df1$case_cat <- ifelse(df1$cases_pct >= quantile(df1$cases_pct, probs = 0.75), "High_Risk", "Low_Risk")
df1$case_cat <- factor(df1$case_cat)

df1$month <- factor(df1$month)
df1$district <- factor(df1$district)

##... Model Performance: Model 1

control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random")

df1.train <- df1[df1$year == 2020, ]
df1.test <- df1[df1$year == 2021, ]

# rf.cv.1 <- train(case_cat ~ month,
#                  data = df1.train,
#                  method = 'rf',
#                  trControl = control)
# 
# rf.pred.1 <- predict(rf.cv.1 , df1.test)
# 
# rf.cm.1 <- caret::confusionMatrix(rf.pred.1, df1.test$case_cat)
# rf.cm.1
# 
# pred.1 <- predict(rf.cv.1 , df1.test, type = "prob")
# test.1 <- evalm(data.frame(pred.1, df1.test$case_cat))
# test.1

##... Model Performance: Model 2

# control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random")

# df1.train <- df1[df1$year == 2020, ]
# df1.test <- df1[df1$year == 2021, ]

rf.cv.2 <- train(case_cat ~ index,
                 data = df1.train,
                 method = 'rf',
                 trControl = control)

rf.pred.2 <- predict(rf.cv.2 , df1.test)

rf.cm.2 <- caret::confusionMatrix(rf.pred.2, df1.test$case_cat)
rf.cm.2

# df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred.2))
# all(rf.pred.2 == df1.train[df1.train$month %in% unique(df1.test$month), ]$case_cat)


pred.2 <- predict(rf.cv.2 , df1.test, type = "prob")
test.2 <- evalm(data.frame(pred.2, df1.test$case_cat))
test.2

########################################################################
##... Predictive Performance: Validation Set Approach Using Monthly Data
##... Using Median cut-off
########################################################################

set.seed(10)

##... Data Preparation 

df1 <- df.pak %>%
  dplyr::select(cases, district, month, year, index) %>%
  dplyr::group_by(district, month, year) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(month, year) %>% dplyr::mutate(total_cases = sum(cases))
df1 <- transform(df1, cases_pct = (cases/total_cases) * 100)

df1$case_cat <- ifelse(df1$cases_pct >= quantile(df1$cases_pct, probs = 0.50), "High_Risk", "Low_Risk")
df1$case_cat <- factor(df1$case_cat)

df1$month <- factor(df1$month)
df1$district <- factor(df1$district)

##... Model Performance: Model 1

# control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random")
# 
# df1.train <- df1[df1$year == 2020, ]
# df1.test <- df1[df1$year == 2021, ]
# 
# rf.cv.1 <- train(case_cat ~ month,
#                  data = df1.train,
#                  method = 'rf',
#                  trControl = control,
#                  ntree = 100)
# 
# rf.pred.1 <- predict(rf.cv.1 , df1.test)
# 
# rf.cm.1 <- caret::confusionMatrix(rf.pred.1, df1.test$case_cat)
# rf.cm.1
# 
# pred.1 <- predict(rf.cv.1 , df1.test, type = "prob")
# test.1 <- evalm(data.frame(pred.1, df1.test$case_cat))
# test.1

##... Model Performance: Model 2

control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random")

df1.train <- df1[df1$year == 2020, ]
df1.test <- df1[df1$year == 2021, ]

rf.cv.2 <- train(case_cat ~ index,
                 data = df1.train,
                 method = 'rf',
                 trControl = control)

rf.pred.2 <- predict(rf.cv.2 , df1.test)

rf.cm.2 <- caret::confusionMatrix(rf.pred.2, df1.test$case_cat)
rf.cm.2

pred.2 <- predict(rf.cv.2 , df1.test, type = "prob")
test.2 <- evalm(data.frame(pred.2, df1.test$case_cat))
test.2

########################################################################
##... Predictive Performance: Validation Set Approach Using Monthly Data
##... Multiclass Classification 
########################################################################

set.seed(10)

##... Data Preparation 

df1 <- df.pak %>%
  dplyr::select(cases, district, month, year, index) %>%
  dplyr::group_by(district, month, year) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(month, year) %>% dplyr::mutate(total_cases = sum(cases))
df1 <- transform(df1, cases_pct = (cases/total_cases) * 100)

df1$case_cat <- with(df1, ave(cases_pct, FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                                  "V.High", ifelse(x >= quantile(x = cases_pct, probs = 0.50) & x < quantile(x = cases_pct, probs = 0.75), "High",
                                                                                   ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.50), "Average",
                                                                                          "Low")))))
df1$case_cat <- factor(df1$case_cat)

df1$month <- factor(df1$month)
df1$district <- factor(df1$district)

##... Model Performance: Model 1

control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random")

df1.train <- df1[df1$year == 2020, ]
df1.test <- df1[df1$year == 2021, ]

rf.cv.1 <- train(case_cat ~ index,
                 data = df1.train,
                 method = 'rf',
                 trControl = control)

rf.pred.1 <- predict(rf.cv.1 , df1.test)

rf.cm.1 <- caret::confusionMatrix(rf.pred.1, df1.test$case_cat)
rf.cm.1

mean(rf.cm.1$byClass[,"Balanced Accuracy"])
mean(rf.cm.1$byClass[,"Sensitivity"])
mean(rf.cm.1$byClass[,"Specificity"])

pred.1 <- predict(rf.cv.1, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred.1)

