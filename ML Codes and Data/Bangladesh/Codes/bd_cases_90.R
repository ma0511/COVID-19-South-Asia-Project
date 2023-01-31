#################################################################
#################################################################
######### Balancing Lives and Livelihoods using Contagion Risk 
######### Based COVID-19 Management for Low-Income
######### Countries: A Study on Bangladesh
######### Updated Data (2021-2022)
######### Multiclass
######### Red (>= 90), Orange (< 90 and >= 25), Green (< 25)
#################################################################
#################################################################

###################
# Load the packages 
###################

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
library("yardstick")

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

################################
# Read the revised CR index data 
################################

setwd("C:/Users/User/Documents/Research/Shonchoy_Covid-19/Revised_CR_Index_Jun2022")
bd.index <- haven::read_dta("Bangladesh.dta")

# Number of unique districts 

unique(bd.index$district)

# Select the relevant variables

bd.index <- dplyr::select(bd.index, district, index_a)

# Clean the district variable

bd.index$district <- str_trim(bd.index$district)

####################################
# Read the updated the covid-19 data 
####################################

setwd("C:/Users/User/Documents/Research/Shonchoy_Covid-19/Updated_Data/BD_Data")

file.list <- list.files(pattern = '*.csv')
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names
df <- purrr::map_df(file.list, read.csv, .id = "id")

#########################
# Clean the covid-19 data 
#########################

colnames(df)[c(1, 3)] <- c("district", "cases")

df$district <- gsub(pattern = "\\.csv", "", df$district)
df$district <- str_trim(df$district)

df <- df %>% tidyr::separate(Category, c("month_", "day_"), sep = "-")
df$day_ <- as.numeric(df$day_)

df$Year <- as.integer(df$Year)
colnames(df)[colnames(df) == "Year"] <- "year"

##... Date Variable

df$date_str <- paste(df$month_, df$day_, df$year, sep = "-")
df$date <- as.Date(df$date_str, format = "%B-%d-%Y")

##... Fill in the missing date variable. 

df <- df %>%
  dplyr::group_by(district) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by = "day"))

##... Arrange the data by district and date

df <- dplyr::arrange(df, district, date)

##... Convert missing cases to 0

df[is.na(df$cases), "cases"] <- 0

##... Drop unwanted variables

df <- dplyr::select(df, -day_, -month_, -year, -date_str)

##... Add month, day, and week variables

df$day <- lubridate::wday(df$date, label = TRUE, abbr = FALSE)
df$week <- lubridate::week(df$date)
df$month <- lubridate::month(df$date, label = TRUE, abbr = FALSE)
df$year <- lubridate::year(df$date)

##... District Names 

unique(bd.index$district)
unique(df$district)

unique(bd.index$district)[!(unique(bd.index$district) %in% unique(df$district))] 

df[which(df$district == "Nowabganj"), "district"] <- "Chapainawabganj"
df[which(df$district == "Jhalakati"), "district"] <- "Jhalokati"
df[which(df$district == "Noagoan"), "district"] <- "Naogaon"
df[which(df$district == "Narsingdhi"), "district"] <- "Narsingdi"
df[which(df$district == "Panchagharh"), "district"] <- "Panchagarh"
df[which(df$district == "Perojpur"), "district"] <- "Pirojpur"
df[which(df$district == "Sariatpur"), "district"] <- "Shariatpur"
df[which(df$district == "Thakurgoan"), "district"] <- "Thakurgaon"

bd.index[which(bd.index$district == "Maulvibazar"), "district"] <- "Moulvi Bazar"
bd.index[which(bd.index$district == "Gaibandah"), "district"] <- "Gaibandha"
bd.index[which(bd.index$district == "CHAPAI NABABGANJ"), "district"] <- "Chapainawabganj"
bd.index[which(bd.index$district == "SIRAJGANJ"), "district"] <- "Sirajganj"
bd.index[which(bd.index$district == "Khagrachhari"), "district"] <- "Khagrachari"

df <- dplyr::arrange(df, district, date)

##... Merge the variables

df.21 <- merge(df, bd.index[, c("district", "index_a")], by = "district", all.x = TRUE) 

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  January 2021
#############################################################

set.seed(10)

##... Data Preparation: Using January 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-01-31")

df$test <- ifelse(df$date >= "2021-01-01" & df$date <= "2021-01-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)

relevel(df1$case_cat, ref = "Red")

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)


#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  February 2021
#############################################################

set.seed(10)

##... Data Preparation: Using February 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-02-28")
df <- dplyr::filter(df, !(month %in% c("January") & year == 2021))

df$test <- ifelse(df$date >= "2021-02-01" & df$date <= "2021-02-28", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)


#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  March 2021
#############################################################

set.seed(10)

##... Data Preparation: Using March 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-03-31")
df <- dplyr::filter(df, !(month %in% c("January", "February") & year == 2021))

df$test <- ifelse(df$date >= "2021-03-01" & df$date <= "2021-03-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  April 2021
#############################################################

set.seed(10)

##... Data Preparation: Using April 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-04-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March") & year == 2021))

df$test <- ifelse(df$date >= "2021-04-01" & df$date <= "2021-04-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)


#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  May 2021
#############################################################

set.seed(10)

##... Data Preparation: Using May 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-05-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April") & year == 2021))

df$test <- ifelse(df$date >= "2021-05-01" & df$date <= "2021-05-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  June 2021
#############################################################

set.seed(10)

##... Data Preparation: Using June 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-06-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May") & year == 2021))

df$test <- ifelse(df$date >= "2021-06-01" & df$date <= "2021-06-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  July 2021
#############################################################

set.seed(10)

##... Data Preparation: Using July 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-07-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June") & year == 2021))

df$test <- ifelse(df$date >= "2021-07-01" & df$date <= "2021-07-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  August 2021
#############################################################

set.seed(10)

##... Data Preparation: Using August 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-08-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July") & year == 2021))

df$test <- ifelse(df$date >= "2021-08-01" & df$date <= "2021-08-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  September 2021
#############################################################

set.seed(10)

##... Data Preparation: Using September 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-09-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August") & year == 2021))

df$test <- ifelse(df$date >= "2021-09-01" & df$date <= "2021-09-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  October 2021
#############################################################

set.seed(10)

##... Data Preparation: Using October 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-10-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August",
                                       "September") & year == 2021))

df$test <- ifelse(df$date >= "2021-10-01" & df$date <= "2021-10-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  November 2021
#############################################################

set.seed(10)

##... Data Preparation: Using November 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-11-30")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August",
                                       "September", "October") & year == 2021))

df$test <- ifelse(df$date >= "2021-11-01" & df$date <= "2021-11-30", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)

df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using November 2021 as the test

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  December 2021
#############################################################

set.seed(10)

##... Data Preparation: Using December 2021 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2021-12-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August",
                                       "September", "October", "November") & year == 2021))

df$test <- ifelse(df$date >= "2021-12-01" & df$date <= "2021-12-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)

df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using December 2021 as the test

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  January 2022
#############################################################

set.seed(10)

##... Data Preparation: Using January 2022 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2022-01-31")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                                       "October", "November", "December") & year == 2021))

df$test <- ifelse(df$date >= "2022-01-01" & df$date <= "2022-01-31", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)

df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using January 2022 as the test

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  February 2022
#############################################################

set.seed(10)

##... Data Preparation: Using February 2022 as the test

df <- dplyr::filter(df.21, date >= "2020-01-01" & date <= "2022-02-28")
df <- dplyr::filter(df, !(month %in% c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                                       "October", "November", "December") & year == 2021))
df <- dplyr::filter(df, !(month %in% c("January") & year == 2022))

df$test <- ifelse(df$date >= "2022-02-01" & df$date <= "2022-02-28", 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)

df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using February 2022 as the test

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  All 2021
#############################################################

set.seed(10)

##... Data Preparation: Using year 2021 as the test

df <- df.21
df <- dplyr::filter(df, year %in% c(2020, 2021))

df$test <- ifelse(df$year == 2021, 1, 0)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

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

pred <- predict(rf.cv, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred)

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)

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


#############################################################
# Predictive Performance and Model Comparison: District Level  
# Training data: All 2020
# Testing data:  All 2022
#############################################################

set.seed(10)

##... Data Preparation: Using year 2022 as the test

df <- df.21
df <- dplyr::filter(df, year %in% c(2020, 2022))

df$test <- ifelse(df$year == 2022, 1, 0)
df$march_2022 <- ifelse(df$month == "March" & df$year == 2022, 1, 0)
df <- dplyr::filter(df, df$march_2022 != 1)

df1 <- df %>%
  dplyr::group_by(district, test) %>%
  dplyr::summarise(cases_dist = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(test) %>% dplyr::mutate(total_cases = sum(cases_dist))
df1 <- df1 %>% dplyr::mutate(cases_pct = (cases_dist/total_cases) * 100)

df1$percentile_25 <- quantile(df1$index, probs = 0.25)
df1$percentile_90 <- quantile(df1$index, probs = 0.90) 

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)

df1$case_cat <- relevel(df1$case_cat, ref = "Red")

##... Model Performance : Using year 2022 as the test

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

##... Multiclass 

df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred))
yardstick::mcc(df1.test, df1.test$case_cat, rf.pred)
