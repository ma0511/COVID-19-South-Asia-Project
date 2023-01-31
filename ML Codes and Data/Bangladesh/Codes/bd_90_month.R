#################################################################
#################################################################
#################################################################
######### Balancing Lives and Livelihoods using Contagion Risk 
######### Based COVID-19 Management for Low-Income Countries
######### Bangladesh, District-Monthly, Cases
######### Updated Data (2021-2022)
######### Multiclass
######### Red (>= 90), Orange (< 90 and >= 25), Green (< 25)
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

library("partykit")   
library("rpart")        
library("rattle")        
library("rpart.plot")     
library("RColorBrewer")

library("ConfusionTableR")

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

########################################################################
##... Predictive Performance: Validation Set Approach Using Monthly Data
##... Multiclass Classification 
########################################################################

set.seed(10)

df1 <- df.21 %>%
  dplyr::filter(date >= "2020-05-01") %>%
  dplyr::select(cases, district, month, year, index_a) %>%
  dplyr::group_by(district, month, year) %>%
  dplyr::summarise(cases = sum(cases, na.rm = TRUE),
                   index = unique(index_a))

df1 <- df1 %>% dplyr::group_by(month, year) %>% dplyr::mutate(total_cases = sum(cases))
df1 <- transform(df1, cases_pct = (cases/total_cases) * 100)

df1$case_cat <- with(df1, ave(cases_pct, c("district", "test"), FUN = function(x) ifelse(x >= quantile(x = cases_pct, probs = 0.90),
                                                                                         "Red", ifelse(x >= quantile(x = cases_pct, probs = 0.25) & x < quantile(x = cases_pct, probs = 0.90), "Orange","Green"))))
df1$case_cat <- factor(df1$case_cat)
relevel(df1$case_cat, ref = "Red")

##... Model Performance: Model 1

control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random")

df1.train <- df1[df1$year == 2020, ]
df1.test <- df1[df1$year == 2021 | df1$year == 2022, ]

rf.cv.1 <- train(case_cat ~ index,
                 data = df1.train,
                 method = 'rf',
                 trControl = control)

rf.cv.1
rf.pred.1 <- predict(rf.cv.1 , df1.test)

rf.cm.1 <- caret::confusionMatrix(rf.pred.1, df1.test$case_cat)
rf.cm.1

mean(rf.cm.1$byClass[,"Balanced Accuracy"])
mean(rf.cm.1$byClass[,"Sensitivity"])
mean(rf.cm.1$byClass[,"Specificity"])

# df1.test <- cbind.data.frame(df1.test, as.data.frame(rf.pred.1))

pred.1 <- predict(rf.cv.1, df1.test, type = "prob")
pROC::multiclass.roc(df1.test$case_cat, pred.1)
