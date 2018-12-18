#### Topic: Calculating Price Elasticities
#### Midterm Assignment
#### Course: Strategic Pricing
#### Term: Spring 2017
#### Professor: Joseph Kuehn

## load required packages
library(tidyverse)
library(readstata13)
library(AER)
library(ivpack)

## load dataset
mydata <- read.dta13("C:\\Users\\Naveed\\Desktop\\Education\\CSUEB\\MS_ECON\\MS_ECON_Spring_2017\\ECON_6999_Strategic Pricing\\Midterm\\OTC_MainData_mod(2).dta")

## create ms and msoo
mydata$ms <- mydata$sales_/mydata$allsales
mydata$msoo <- mydata$genericsales/mydata$allsales
mydata$dv <- log(mydata$ms)-log(mydata$msoo)

## model 1, Q1
## using ols with price and promotion as product characteristics
model1 <- lm(dv 
             ~ price_
             + prom_
             , data = mydata)
summary(model1)

## model 2, Q2
## using ols with price and promotion as product characteristics and brand dummies
model2 <- lm(dv
             ~ price_
             + prom_
             + as.factor(brand)
             , data = mydata)
summary(model2)

## model 3, Q3
## using ols with price and promotion as product characteristics and store brand
model3 <- lm(dv
             ~ price_
             + prom_
             + as.factor(brand)
             + as.factor(store)
             , data = mydata)
summary(model3)

## estimate model 1, 2, and 3 (from above) using wholesale cost as an instrument
# model 1, Q4
iv_model1 <-  ivreg(dv
                    ~ price_
                    + prom_ 
                    | cost_ + prom_
                    , data = mydata)
summary(iv_model1)                    

# model 2, Q4
iv_model2 <-  ivreg(dv 
                    ~ price_
                    + prom_
                    + as.factor(brand)
                    | cost_ + prom_ + as.factor(brand)
                    , data = mydata)
summary(iv_model2)                    

# model 3, Q4
iv_model3 <-  ivreg(dv 
                    ~ price_
                    + prom_
                    + as.factor(brand)
                    + as.factor(store)
                    | cost_ + prom_ + as.factor(brand) + as.factor(store)
                    , data = mydata)
summary(iv_model3)                    

## estimate model 1, 2, and 3 (from above) using average price in other markets as an instrument
# model 1, Q5
iv2_model1 <-  ivreg(dv
                     ~ price_
                     + prom_ 
                     | avoutprice + prom_
                     , data = mydata)
summary(iv2_model1)                    

# model 2, Q5
iv2_model2 <-  ivreg(dv 
                     ~ price_
                     + prom_
                     + as.factor(brand)
                     | avoutprice + prom_ + as.factor(brand)
                     , data = mydata)
summary(iv2_model2)                    

# model 3, Q5
iv2_model3 <-  ivreg(dv 
                     ~ price_
                     + prom_
                     + as.factor(brand)
                     + as.factor(store)
                     | avoutprice + prom_ + as.factor(brand) + as.factor(store)
                     , data = mydata)
summary(iv2_model3)                    

## calculating price elasticities for iv2_model3
## ahat x (Price) x (1 - ms)
summary(iv2_model3[[1]][[2]]*(mydata$price_)*(1-mydata$ms))
mydata$price_elasticity <- iv2_model3[[1]][[2]]*(mydata$price_)*(1-mydata$ms)

mydata %>%
  group_by(brand) %>%
  summarize(mean_price_elasticity = mean(price_elasticity))
