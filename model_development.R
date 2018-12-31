##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Price Elasticiy
##### Section: Model Development
##### Date: 12/18/2018

### load dependent libraries
library(tidyverse)
library(lme4)
library(nlme)
library(readstata13)
library(AER)
library(ivpack)

### model 1
model1 <- lm(dv
             ~ kp_rate_mean
             , data = df_eda)
summary(model1)

### final model
ols_final_1stStage <- lm(kp_rate_mean
                        ~ mrn_avg_riskscore
                        + zipcode_avg_hh_income
                        + prop_blackhisp
                        + fcr
                        , data = df_eda)

summary(ols_final_1stStage)

ols_final_2ndStage <- lm(dv
                        ~ ols_final_1stStage$fitted.values
                        + zipcode_avg_hh_income
                        + prop_blackhisp
                        + fcr
                        , data = df_eda)

summary(ols_final_2ndStage)

ivreg_final <-  ivreg(dv 
                  ~ kp_rate_mean
                  + zipcode_avg_hh_income
                  + prop_blackhisp
                  + fcr
                  | mrn_avg_riskscore + zipcode_avg_hh_income + prop_blackhisp + fcr
                  , data = df_eda)
summary(ivreg_final)  

### calculating price elasticities for final model
### ahat x (Price) x (1 - ms)
summary(ivreg_final[[1]][[2]]*(df_eda$kp_rate_mean)*(1-df_eda$ms))
df_eda$price_elasticity <- ivreg_final[[1]][[2]]*(df_eda$kp_rate_mean)*(1-df_eda$ms)

hist(df_eda$price_elasticity)

df_eda_2 <- df_eda %>%
  group_by(region_account_number) %>%
  summarize(mean_price_elasticity = mean(price_elasticity))

hist(df_eda_2$mean_price_elasticity)

df_eda_3 <- df_eda %>%
  group_by(region_account_number) %>%
  filter(effective_date_year == max(effective_date_year))

df_eda_3 <- df_eda_3 %>%
  left_join(df_eda_2, by = "region_account_number")

