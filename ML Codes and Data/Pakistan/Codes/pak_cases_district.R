#################################################################
#################################################################
######### Balancing Lives and Livelihoods using Contagion Risk 
######### Based COVID-19 Management for Low-Income Countries
######### Pakistan Data
######### Updated Data
#################################################################
#################################################################

###################
# Load the packages 
###################

##... Notes 

# For non-R Users 

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

##################################################
# Exploratory Analysis: Time Series of Daily Cases
##################################################

df.time <- df %>%
  dplyr::select(cases, date) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(daily_cases = sum(cases, na.rm = TRUE))

df.time$month <- lubridate::month(df.time$date, label = TRUE, abbr = FALSE)
df.time$year <- lubridate::year(df.time$date)

ggplot(data = df.time, aes(x = as.Date(date), y = daily_cases)) +
  geom_line(size = 1, color = "blue") + 
  labs(x = "Date", y = "Daily Cases") + 
  theme_minimal() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45)) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "months") + 
  scale_y_continuous(labels = comma)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  January 2021
#############################################################

df.pak <- df
set.seed(10)

##... Data Preparation: Using January 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-01-31")

df$test <- ifelse(df$date >= "2021-01-01" & df$date <= "2021-01-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("test", "district"),
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using January 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  February 2021
#############################################################

set.seed(10)

##... Data Preparation: Using February 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-02-28")
df <- dplyr::filter(df, !(month %in% c("January") & year == "2021"))

df$test <- ifelse(df$date >= "2021-02-01" & df$date <= "2021-02-28", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("test", "district"),
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using February 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  March 2021
#############################################################

##... Data Preparation: Using March 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-03-31")
df <- dplyr::filter(df, !(month %in% c("January", "February") & year == 2021))

df$test <- ifelse(df$date >= "2021-03-01" & df$date <= "2021-03-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using March 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  April 2021
#############################################################

##... Data Preparation: Using April 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-04-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March") & year == 2021))

df$test <- ifelse(df$date >= "2021-04-01" & df$date <= "2021-04-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using April 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  May 2021
#############################################################

##... Data Preparation: Using May 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-05-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April") & year == 2021))

df$test <- ifelse(df$date >= "2021-05-01" & df$date <= "2021-05-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using May 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  June 2021
#############################################################

##... Data Preparation: Using June 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-06-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May") & year == 2021))

df$test <- ifelse(df$date >= "2021-06-01" & df$date <= "2021-06-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using June 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  July 2021
#############################################################

##... Data Preparation: Using July 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-07-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June") & year == 2021))

df$test <- ifelse(df$date >= "2021-07-01" & df$date <= "2021-07-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using July 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  August 2021
#############################################################

##... Data Preparation: Using August 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-08-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July") & year == 2021))

df$test <- ifelse(df$date >= "2021-08-01" & df$date <= "2021-08-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using August 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  September 2021
#############################################################

##... Data Preparation: Using September 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-09-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August") & year == 2021))

df$test <- ifelse(df$date >= "2021-09-01" & df$date <= "2021-09-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using September 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  October 2021
#############################################################

##... Data Preparation: Using October 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-10-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September") & year == 2021))

df$test <- ifelse(df$date >= "2021-10-01" & df$date <= "2021-10-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using October 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  November 2021
#############################################################

##... Data Preparation: Using November 2021 as the test

# df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-11-30")
# df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September",
#                                        "October") & year == 2021))
# 
# df$test <- ifelse(df$date >= "2021-11-01" & df$date <= "2021-11-30", 1, 0)
# 
# df1 <- df %>%
#   dplyr::group_by(district, test) %>%
#   dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
#                    index = unique(index))
# 
# df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
# df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)
# 
# df1$percentile_25 <- quantile(df1$index, probs = 0.25)
# df1$percentile_90 <- quantile(df1$index, probs = 0.90) 
# 
# df1$case_cat <- with(df1, ave(cases_pct, test,
#                               FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
#                                                        "High_Risk", "Low_Risk")))
# df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using November 2021 as the test

# control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")
# 
# df1.train <- df1[df1$test == 0, ]
# df1.test <- df1[df1$test == 1, ]
# 
# rf.cv <- train(case_cat ~ index,
#                data = df1.train,
#                method = 'rf',
#                trControl = control)
# 
# rf.pred <- predict(rf.cv , df1.test)
# 
# rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
# rf.cm
# 
# Matt_Coef(rf.cm)

##... ROC - AUC

# pred <- predict(rf.cv , df1.test, type = "prob")
# test <- evalm(data.frame(pred, df1.test$case_cat))
# test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  All 2021
#############################################################

set.seed(10)

##... Data Preparation: Using year 2021 as the test

df <- df.pak

df <- dplyr::filter(df, date >= "2020-09-01")

df$test <- ifelse(df$year == "2021", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("test", "district"),
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.75), 
                                                       "High", "Low")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using year 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

# pROC (AUC)

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::auc(pROC::roc(df1.test$case_cat, pred[,2]))
pROC::auc(pROC::roc(df1.test$case_cat, pred[,1]))

# pROC::roc(df1.test$case_cat, pred[,2], smoothed = TRUE,
#           # arguments for ci
#           # ci=TRUE, ci.alpha=0.9, stratified=FALSE,
#           # arguments for plot
#           plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
#           print.auc=TRUE, show.thres=TRUE)


pROC::plot.roc(df1.test$case_cat, pred[,2], percent = FALSE, 
               main = "ROC curves for the CR-Index using Covid-19 cases in Pakistan (Sindh)", 
               add =  FALSE, asp = NA, print.auc = TRUE,
               col = "red", lwd = 4, legacy.axes = TRUE,
               print.auc.cex=par(2),
               # print.auc.pattern = "%.2f",
               auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE)
# ci = TRUE, ci.alpha = 0.95, stratified = FALSE)

##... Confusion Matrix Plot

table <- data.frame(caret::confusionMatrix(rf.pred, df1.test$case_cat)$table)

plotTable <- table %>%
  mutate(Accuracy = ifelse(table$Prediction == table$Reference, "Correct", "Incorrect")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

p1 <- ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = Accuracy, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(Correct = "green", Incorrect = "red")) +
  theme_bw() +
  # theme(legend.position = "top") +
  labs(title = "Confusion Matrix for the CR-Index using Covid-19 cases in Pakistan (Sindh)") + 
  xlim(rev(levels(table$Reference)))
p1

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  January 2021
# Using Median cut-off
#############################################################

set.seed(10)

##... Data Preparation: Using January 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-01-31")

df$test <- ifelse(df$date >= "2021-01-01" & df$date <= "2021-01-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("test", "district"),
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using January 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  February 2021
# Using Median cut-off
#############################################################

set.seed(10)

##... Data Preparation: Using February 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-02-28")
df <- dplyr::filter(df, !(month %in% c("January") & year == 2021))

df$test <- ifelse(df$date >= "2021-02-01" & df$date <= "2021-02-28", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("test", "district"),
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using February 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  March 2021
# Using Median cut-off
#############################################################

##... Data Preparation: Using March 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-03-31")
df <- dplyr::filter(df, !(month %in% c("January", "February") & year == 2021))

df$test <- ifelse(df$date >= "2021-03-01" & df$date <= "2021-03-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using March 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  April 2021
# Using Median cut-off
#############################################################

set.seed(10)

##... Data Preparation: Using April 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-04-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March") & year == 2021))

df$test <- ifelse(df$date >= "2021-04-01" & df$date <= "2021-04-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using April 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  May 2021
# Using Median cut-off
#############################################################

set.seed(10)

##... Data Preparation: Using May 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-05-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April") & year == 2021))

df$test <- ifelse(df$date >= "2021-05-01" & df$date <= "2021-05-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using May 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  June 2021
# Using Median cut-off
#############################################################

set.seed(10)

##... Data Preparation: Using June 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-06-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May") & year == 2021))

df$test <- ifelse(df$date >= "2021-06-01" & df$date <= "2021-06-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using June 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  July 2021
# Using Median cut-off
#############################################################

##... Data Preparation: Using July 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-07-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June") & year == 2021))

df$test <- ifelse(df$date >= "2021-07-01" & df$date <= "2021-07-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using July 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  August 2021
# Using Median cut-off
#############################################################

##... Data Preparation: Using August 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-08-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July") & year == 2021))

df$test <- ifelse(df$date >= "2021-08-01" & df$date <= "2021-08-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using August 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  September 2021
# Using Median cut-off
#############################################################

##... Data Preparation: Using September 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-09-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August") & year == 2021))

df$test <- ifelse(df$date >= "2021-09-01" & df$date <= "2021-09-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using September 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  October 2021
# Using Median cut-off
#############################################################

##... Data Preparation: Using October 2021 as the test

df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-10-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September") & year == 2021))

df$test <- ifelse(df$date >= "2021-10-01" & df$date <= "2021-10-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, test,
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High_Risk", "Low_Risk")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using October 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  November 2021
# Using Median cut-off
#############################################################

##... Data Preparation: Using November 2021 as the test

# df <- dplyr::filter(df.pak, date >= "2020-09-01" & date <= "2021-11-30")
# df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September",
#                                        "October") & year == 2021))
# 
# df$test <- ifelse(df$date >= "2021-11-01" & df$date <= "2021-11-30", 1, 0)
# 
# df1 <- df %>%
#   dplyr::group_by(district, test) %>%
#   dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
#                    index = unique(index))
# 
# df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
# df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)
# 
# df1$percentile_25 <- quantile(df1$index, probs = 0.25)
# df1$percentile_90 <- quantile(df1$index, probs = 0.90) 
# 
# df1$case_cat <- with(df1, ave(cases_pct, test,
#                               FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
#                                                        "High_Risk", "Low_Risk")))
# df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using November 2021 as the test

# control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")
# 
# df1.train <- df1[df1$test == 0, ]
# df1.test <- df1[df1$test == 1, ]
# 
# rf.cv <- train(case_cat ~ index,
#                data = df1.train,
#                method = 'rf',
#                trControl = control)
# 
# rf.pred <- predict(rf.cv , df1.test)
# 
# rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
# rf.cm
# 
# Matt_Coef(rf.cm)

##... ROC - AUC

# pred <- predict(rf.cv , df1.test, type = "prob")
# test <- evalm(data.frame(pred, df1.test$case_cat))
# test

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  All 2021
# Using Median cut-off
#############################################################

set.seed(10)

##... Data Preparation: Using year 2021 as the test

df <- df.pak

df <- dplyr::filter(df, date >= "2020-09-01")

df$test <- ifelse(df$year == 2021, 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("test", "district"),
                              FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.50), 
                                                       "High", "Low")))
df1$case_cat <- factor(df1$case_cat)

##... Model Performance : Using year 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

Matt_Coef(rf.cm)

##... ROC - AUC

pred <- predict(rf.cv , df1.test, type = "prob")
test <- evalm(data.frame(pred, df1.test$case_cat))
test


