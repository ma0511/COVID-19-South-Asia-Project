#################################################################
#################################################################
######### Balancing Lives and Livelihoods using Contagion Risk 
######### Based COVID-19 Management for Low-Income
######### Countries: A Study on Bangladesh
######### Indian Data (Cases)
######### Arranging the data by state, district, and date
######### Multiclass
######### Red (>= 90), Orange (< 90 and >= 25), Green (< 25)
######### Updated Data (2021-2022) 
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

###############################
# Read the Indian covid-19 data 
###############################

setwd("~/Research/Shonchoy_Covid-19")
covid.ind.old <- haven::read_dta("ML_crindex_India.dta")

setwd("~/Research/Shonchoy_Covid-19/Updated_data")
covid.ind <- haven::read_dta("covid_infected_deaths.dta")

###############################
# Select the relevant variables
###############################

df <- dplyr::select(covid.ind, date, lgd_state_name, lgd_state_id, lgd_district_name, lgd_district_id, total_cases, total_deaths)

##################
# Data Preparation
##################

##... Rename variables 

df <- df %>% dplyr::rename(cum_cases = total_cases, district = lgd_district_name, state = lgd_state_name, deaths = total_deaths)

##... State Names

length(unique(df$state)); unique(df$state)
nrow(df[df$state == "", ])
nrow(df[df$state == "", "state"])
df[df$state == "", ]
df[df$state == "", ] <- NA

##... Remove missing data 

nrow(df[complete.cases(df), ])
df <- df[complete.cases(df), ]

##... Format the date variable 

df$date <- as.Date(df$date, format = "%Y-%m-%d")

##... Create the month, week, and day variables

df$month <- lubridate::month(df$date, label = TRUE, abbr = FALSE)
df$day <- lubridate::wday(df$date, label = TRUE, abbr = FALSE)
df$week <- lubridate::week(df$date)
df$year <- lubridate::year(df$date)

df$week <- factor(df$week)
df$year <- factor(df$year)

##... Arrange the data

df <- dplyr::arrange(df, state, district, date)

##... Daily Cases

df <- df %>% dplyr::group_by(district) %>%
  dplyr::mutate(daily_case = c(cum_cases[1], diff(cum_cases)))

summary(df$daily_case)

df <- dplyr::filter(df, !(daily_case < 0))

summary(df$daily_case)

##... Clean the district variable 

df$district <- str_trim(df$district)

##... Rename the daily case variable

df <- df %>% dplyr::rename(cases = daily_case)

##... Filter the data (select from May 2020)

# df <- df %>% dplyr::filter(date >= "2020-05-01")

###################################################
# Merge the covid-19 data with the revised CR-index
###################################################

##... Read the revised CR-index data

setwd("~/Research/Shonchoy_Covid-19/Revised_CR_Index_Jun2022")
cr.index <- haven::read_dta("India.dta") 

##... See the district names

length(unique(cr.index$lgd_district_id))

##... See the district names from covid-19 data

unique(df$district)
length(unique(df$lgd_district_id))

##... Remove unwanted district names and cleaning district variable

df <- dplyr::filter(df, district != "not reported")
df$district <- gsub(pattern = "\\s+", replacement = " ", df$district)
df$district <- stringr::str_trim(df$district)

##... Merge the data

df <- merge(df, cr.index[, c("lgd_state_id", "lgd_district_id", "cr_index_india_2")], by = c("lgd_state_id", "lgd_district_id"),
            all.x = TRUE)

##... Rename the variables

colnames(df)[colnames(df) == "cr_index_india_2"] <- "index"

##... Arrange the data

df <- dplyr::arrange(df, state, district, date)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  January 2021
#############################################################

df.ind <- df

set.seed(10)

# ##... Filter the data (select from July 2020)
# 
# df <- df.ind %>% dplyr::filter(date >= "2020-07-01")

##... Data Preparation: Using January 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-01-31")

df$test <- ifelse(df$date >= "2021-01-01" & df$date <= "2021-01-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using January 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  February 2021
#############################################################

set.seed(10)

##... Data Preparation: Using February 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-02-28")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using February 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  March 2021
#############################################################

set.seed(10)

##... Data Preparation: Using March 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-03-31")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using March 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  April 2021
#############################################################

set.seed(10)

##... Data Preparation: Using April 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-04-30")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using April 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)


#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  May 2021
#############################################################

set.seed(10)

##... Data Preparation: Using May 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-05-31")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using May 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  June 2021
#############################################################

set.seed(10)

##... Data Preparation: Using June 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-06-30")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using June 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  July 2021
#############################################################

set.seed(10)

##... Data Preparation: Using July 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-07-31")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using July 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  August 2021
#############################################################

set.seed(10)

##... Data Preparation: Using August 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-08-31")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using August 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  September 2021
#############################################################

set.seed(10)

##... Data Preparation: Using September 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-09-30")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using September 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  October 2021
#############################################################

set.seed(10)

##... Data Preparation: Using October 2021 as the test

df <- dplyr::filter(df.ind, date >= "2020-05-01" & date <= "2021-10-31")
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

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using October 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  All 2021
#############################################################

set.seed(10)

##... Data Preparation: Using year 2021 as the test

df <- df.ind

df <- dplyr::filter(df, date >= "2020-03-01")

df$test <- ifelse(df$year == 2021, 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using year 2021 as the test

control <- trainControl(method = "repeatedcv", number = 5, repeats = 50, search = "random", sampling = "up")

df1.train <- df1[df1$test == 0, ]
df1.test <- df1[df1$test == 1, ]

rf.cv <- train(case_cat ~ index,
               data = df1.train,
               method = 'rf',
               trControl = control)

rf.pred <- predict(rf.cv , df1.test)

rf.cm <- caret::confusionMatrix(rf.pred, df1.test$case_cat)
rf.cm

mean(rf.cm$byClass[,"Balanced Accuracy"])
mean(rf.cm$byClass[,"Sensitivity"])
mean(rf.cm$byClass[,"Specificity"])

##... ROC - AUC

library("pROC")
# library("tinytex")

pred <- predict(rf.cv, df1.test, type = "prob")
result <- pROC::multiclass.roc(df1.test$case_cat, pred)
result

# multiclass.roc(df1.test$case_cat, pred[,1],
#                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
#                print.auc=TRUE, show.thres=TRUE,
#                main = "ROC curves for the CR-Index using Covid-19 cases in India",
#                levels = c("Red", "Green"))
# 
# pre <- predict(rf.cv, df1.test,type='prob')
# roc(df1.test$case_cat, pre[,3], levels = c("Orange", "Red"), plot = TRUE)
# roc(df1.test$case_cat, pre[,3], levels = c("Green", "Red"), plot = TRUE, add = TRUE)

# https://stackoverflow.com/questions/73164367/plot-of-average-auc-from-multiclass-roc-in-r

# pred <- predict(rf.cv, df1.test)
# roc <- multiclass.roc(df1.test$case_cat, pred)
# rsA <- roc[["rocs"]]
# rsA
# 
# par(pty="s")
# plot(rsA[[1]],cex.axis=1.8,lwd = 4,grid = FALSE, xlab="",ylab="",legacy.axes = F,colorize=FALSE, col="gray80", print.auc=F,print.auc.x=0.8,print.auc.y=0.33) # plot ROC curve
# plot(rsA[[2]],cex.axis=1.8,cex.lab=2,lwd = 4,colorize=FALSE, xlab="",ylab="",grid = FALSE, legacy.axes = F,col="gray50", add=TRUE, print.auc=F,print.auc.x=0.8, print.auc.y=0.33)
# plot(rsA[[3]],cex.axis=1.8,cex.lab=2,lwd = 4,colorize=FALSE, xlab="",ylab="",grid = FALSE, legacy.axes = F,col="black", add=TRUE, print.auc=F,print.auc.x=0.8, print.auc.y=0.33)
# title(xlab = "1-Specificity", line = 4,cex.lab=2.5)            # Add x-axis text
# title(ylab = "Sensitivity", line = 5,cex.lab=2.5)            # Add y-axis text
# legend(0.7, 0.35, legend=c(TeX("Claims 0 vs 1: 0.664"), TeX("Claims 0 vs >=2: 0.770"), TeX("Claims 1 vs >=2: 0.637")),
#        col=c("black", "gray50", "gray80","red"), lty=c(1, 1, 1, 3),lwd = 4, cex=1.7,
#        box.lty=0)
# 

# # https://stackoverflow.com/questions/72179298/how-do-i-make-and-plot-roc-curves-in-r-for-multiclass-classification
# 
# 
# pred <- predict(rf.cv, df1.test)
# result <- pROC::multiclass.roc(df1.test$case_cat, as.numeric(pred))
# result
# 
# plot.roc(result$rocs[[1]],
#          print.auc=T,
#          legacy.axes = T)
# plot.roc(result$rocs[[2]],
#          add=T, col = 'red',
#          print.auc = T,
#          legacy.axes = T,
#          print.auc.adj = c(0,3))
# plot.roc(result$rocs[[3]],add=T, col = 'blue',
#          print.auc=T,
#          legacy.axes = T,
#          print.auc.adj = c(0,5))
# 
# legend('bottomright',
#        legend = c('Red-Green',
#                   'Red-Orange',
#                   'Green-Orange'),
#        col=c('black','red','blue'),lwd=2)

