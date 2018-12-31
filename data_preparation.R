##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Price Elasticiy
##### Section: Data Preperation
##### Date: 12/18/2018

### load dependent libraries
library(tidyverse)
library(lme4)
library(nlme)
library(readstata13)
library(AER)
library(ivpack)

## adjust scientific notation
options(scipen=999) 

## functions
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

## load dataframe into enviornment
df_eda <- read.csv("C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\Data\\KP_NationalAccount.csv", stringsAsFactors = TRUE)
glimpse(df_eda, list.len=200)
mean(is.na(df_eda$kp_c_rate_lowest))

## bin firm size
hist(df_eda$eligible)

df_eda$firm_size <- ifelse(df_eda$eligible <= 100, 'LTE100',
                             ifelse((df_eda$eligible > 100 & df_eda$eligible <= 500),'GT100_LTE500',
                                    ifelse((df_eda$eligible > 500 & df_eda$eligible <= 1000),'GT500_LTE1000',
                                           ifelse((df_eda$eligible > 1000 & df_eda$eligible <= 3000),'GT1000_LTE3000','GT3000'))))

prop.table(table(df_eda$firm_size))

## round penetration rate
df_eda$pen_rate <- round(df_eda$pen_rate,3)

## create dummy variables for the effective year and firm size (cleaner labels in summary table)
df_eda <- as.data.frame(df_eda)
df_eda <- cbind(df_eda,dummies::dummy(df_eda$region, sep = "_"))

for( i in colnames(df_eda)){
  colnames(df_eda)[which(colnames(df_eda)==i)] = gsub("df_eda_","",(i))
}

df_eda <- cbind(df_eda,dummies::dummy(df_eda$firm_size, sep = "_"))

for( i in colnames(df_eda)){
  colnames(df_eda)[which(colnames(df_eda)==i)] = gsub("df_eda_","firmsize_",(i))
}

df_eda <- cbind(df_eda,dummies::dummy(df_eda$effective_date_year, sep = "_"))

for( i in colnames(df_eda)){
  colnames(df_eda)[which(colnames(df_eda)==i)] = gsub("df_eda_","edy_",(i))
}

glimpse(df_eda)

## create ms and msoo 
df_eda$ms <- df_eda$n_subscribers/df_eda$eligible
df_eda$msoo <- (df_eda$eligible-df_eda$n_subscribers)/df_eda$eligible

hist(df_eda$ms)
hist(df_eda$msoo)

## create the dv
df_eda$dv <- log(df_eda$ms)-log(df_eda$msoo)
hist(df_eda$dv)

