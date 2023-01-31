#################################################################
#################################################################
######### Balancing Lives and Livelihoods using Contagion Risk 
######### Based COVID-19 Management for Low-Income Countries
######### Indian Data (Cases)
######### Arranging the data by state, district, and date
######### Multiclassification
######### Red (>= 90), Orange (< 90 and >= 25), Green (< 25)
######### District-Monthly Level
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
library("pROC")

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

##... Filter the data (select from May 2020)

df2 <- df %>% dplyr::filter(date >= "2020-03-01")
df2 <- dplyr::arrange(df2, state, district, date)

########################################################################
##... Predictive Performance: Validation Set Approach Using Monthly Data
##... Multiclass Classification
##... Three Categories
########################################################################

##... Data Preparation 

df1 <- df2 %>%
  dplyr::select(cases, district, month, year, index) %>%
  dplyr::group_by(district, month, year) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE),
                   index = unique(index))

df1 <- df1 %>% dplyr::group_by(month, year) %>% dplyr::mutate(total_cases = sum(cases))
df1 <- transform(df1, cases_pct = (cases/total_cases) * 100)

df1$case_cat <- with(df1, ave(cases_pct, FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                  "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
df1$case_cat <- relevel(df1$case_cat, ref = "Red")

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