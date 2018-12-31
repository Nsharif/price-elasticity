##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Price Elasticiy
##### Section: EDA, Charts, and Tables
##### Date: 12/18/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)
library(lattice)
library(Matrix)
library(lme4)
library(nlme)
library(gridExtra)
library(grid)
library(sjPlot)
library(sjmisc)
library(stargazer)

## table view of the dataframe
## figure 1_a
x <- df_eda %>%
  select(kp_rate_mean, n_subscribers) %>%
  summarise(kp_rate_mean=round(mean(kp_rate_mean),2),
            n_subscribers=round(mean(n_subscribers))) %>%
  rownames_to_column () %>%
  gather(Variable, value, -rowname) %>%
  spread(rowname, value) %>% 
  rename(Mean = '1')

y <- df_eda %>%
  select(kp_rate_mean, n_subscribers) %>%
  summarise(kp_rate_sd=round(sd(kp_rate_mean, na.rm = TRUE),2),
            n_subscribers_sd=round(sd(n_subscribers, na.rm = TRUE))) %>%
  rownames_to_column () %>%
  gather(Variable, value, -rowname) %>%
  spread(rowname, value) %>%
  rename(Stdv = '1')

fig_1_a <- bind_cols(x,y) %>%
  select(Variable, Mean, Stdv)

tab_df(fig_1_a
       , title = "Univariate Statistics"
       , file = "C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_1_a.doc") # CRUCIAL - save this as a word document

remove(x)
remove(y)

### Correlation 
### Figure 1_b
fig_1_b <- df_eda %>%
  select(kp_rate_mean, n_subscribers) %>%
  mutate(log_kp_rate_mean = log(kp_rate_mean)) %>%
  mutate(log_n_subscribers = log(n_subscribers))

sjt.corr(fig_1_b[,c("log_kp_rate_mean", "log_n_subscribers")]
         , title = "Sample Bivariate Correlations"
         , file = "C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_1_b.doc")

### OLS Regression Analysis 
### Figure 1_c
ols_1 <- lm(log(n_subscribers)
          ~ log(kp_rate_mean)
          , data = df_eda)

tab_model(ols_1
       , title = "OLS Regression Analysis"
       , file="C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_1_c.doc")

# Calculating the covariances
cov(log(df_eda$n_subscribers),log(df_eda$kp_rate_mean))/var(log(df_eda$kp_rate_mean))

### table view of the dataframe
### figure 3_a
x <- df_eda %>%
  select(kp_rate_mean, n_subscribers, mrn_avg_riskscore) %>%
  summarise(kp_rate_mean=round(mean(kp_rate_mean),2),
            n_subscribers=round(mean(n_subscribers)),
            mrn_avg_riskscore=round(mean(mrn_avg_riskscore),2)) %>%
  rownames_to_column () %>%
  gather(Variable, value, -rowname) %>%
  spread(rowname, value) %>% 
  rename(Mean = '1')

y <- df_eda %>%
  select(kp_rate_mean, n_subscribers, mrn_avg_riskscore) %>%
  summarise(kp_rate_sd=round(sd(kp_rate_mean, na.rm = TRUE),2),
            n_subscribers_sd=round(sd(n_subscribers, na.rm = TRUE)),
            mrn_avg_riskscore_sd=round(sd(mrn_avg_riskscore),2)) %>%
  rownames_to_column () %>%
  gather(Variable, value, -rowname) %>%
  spread(rowname, value) %>%
  rename(Stdv = '1')

fig_3_a <- bind_cols(x,y) %>%
  select(Variable, Mean, Stdv)

tab_df(fig_3_a
       , title = "Univariate Statistics"
       , file = "C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_3_a.doc") # CRUCIAL - save this as a word document

remove(x)
remove(y)

### Correlation 
### Figure 3_b
fig_3_b <- df_eda %>%
  select(kp_rate_mean, n_subscribers, mrn_avg_riskscore) %>%
  mutate(log_kp_rate_mean = log(kp_rate_mean)) %>%
  mutate(log_n_subscribers = log(n_subscribers))

sjt.corr(fig_3_b[,c("log_kp_rate_mean", "log_n_subscribers", "mrn_avg_riskscore")]
         , title = "Sample Bivariate Correlations"
         , file = "C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_3_b.doc")

### Method-of-Moments IVE Estimate 
### Figure 3_c
ols_iv_1stStage_2 <- lm(log(kp_rate_mean)
             ~ mrn_avg_riskscore
             , data = df_eda)

summary(ols_iv_1stStage_2)

ols_iv_2ndStage_2 <- lm(log(n_subscribers)
                      ~ ols_iv_1stStage$fitted.values
                      , data = df_eda)

summary(ols_iv_2ndStage_2)

ivreg_2 <-  ivreg(log(n_subscribers) 
                    ~ log(kp_rate_mean)
                    | mrn_avg_riskscore
                    , data = df_eda)
summary(ivreg_2)   

tab_model(ols_iv_1stStage_1,ols_iv_2ndStage_2 # corrected standard errors are in the ivreg object
          , title = "Method-of-Moments IVE Estimate"
          , file="C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_3_c.doc")

# Calculating the covariances
cov(log(df_eda$n_subscribers),df_eda$mrn_avg_riskscore)/cov(log(df_eda$kp_rate_mean),df_eda$mrn_avg_riskscore)

### table view of the dataframe
### figure 4_a
x <- df_eda %>%
  select(kp_rate_mean, n_subscribers, mrn_avg_riskscore, zipcode_avg_hh_income, prop_blackhisp, fcr) %>%
  summarise(kp_rate_mean=round(mean(kp_rate_mean),2),
            n_subscribers=round(mean(n_subscribers)),
            mrn_avg_riskscore=round(mean(mrn_avg_riskscore),2),
            zipcode_avg_hh_income=round(mean(zipcode_avg_hh_income),2),
            prop_blackhisp=mean(prop_blackhisp),
            fcr=round(mean(fcr),2)) %>%
  rownames_to_column () %>%
  gather(Variable, value, -rowname) %>%
  spread(rowname, value) %>% 
  rename(Mean = '1')

y <- df_eda %>%
  select(kp_rate_mean, n_subscribers, mrn_avg_riskscore, zipcode_avg_hh_income, prop_blackhisp, fcr) %>%
  summarise(kp_rate_sd=round(sd(kp_rate_mean),2),
            n_subscribers_sd=round(sd(n_subscribers)),
            mrn_avg_riskscore_sd=round(sd(mrn_avg_riskscore),2),
            zipcode_avg_hh_income_sd=round(sd(zipcode_avg_hh_income),2),
            prop_blackhisp_sd=sd(prop_blackhisp),
            fcr_sd=round(sd(fcr),2)) %>%
  rownames_to_column () %>%
  gather(Variable, value, -rowname) %>%
  spread(rowname, value) %>%
  rename(Stdv = '1')

fig_4_a <- bind_cols(x,y) %>%
  select(Variable, Mean, Stdv)

tab_df(fig_4_a
       , title = "Univariate Statistics"
       , file = "C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_4_a.doc") # CRUCIAL - save this as a word document

remove(x)
remove(y)

### Correlation 
### Figure 4_b
fig_4_b <- df_eda %>%
  select(kp_rate_mean, n_subscribers, mrn_avg_riskscore, zipcode_avg_hh_income, prop_blackhisp, fcr) %>%
  mutate(log_kp_rate_mean = log(kp_rate_mean)) %>%
  mutate(log_n_subscribers = log(n_subscribers))

sjt.corr(fig_4_b[,c("log_kp_rate_mean", "log_n_subscribers", "mrn_avg_riskscore", "zipcode_avg_hh_income", "prop_blackhisp", "fcr")]
         , title = "Sample Bivariate Correlations"
         , file = "C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_4_b.doc")

### Method-of-Moments IVE Estimate with control variables 
### Figure 4_c
ols_iv_1stStage_3 <- lm(log(kp_rate_mean)
                      ~ mrn_avg_riskscore
                      + zipcode_avg_hh_income
                      + prop_blackhisp
                      + fcr
                      , data = df_eda)

summary(ols_iv_1stStage_3)

ols_iv_2ndStage_3 <- lm(log(n_subscribers)
                      ~ ols_iv_1stStage$fitted.values
                      + zipcode_avg_hh_income
                      + prop_blackhisp
                      + fcr
                      , data = df_eda)

summary(ols_iv_2ndStage_3)

ivreg_3 <-  ivreg(log(n_subscribers) 
                ~ log(kp_rate_mean)
                + zipcode_avg_hh_income
                + prop_blackhisp
                + fcr
                | mrn_avg_riskscore + zipcode_avg_hh_income + prop_blackhisp + fcr
                , data = df_eda)
summary(ivreg_3)   

tab_model(ols_iv_1stStage_3,ols_iv_2ndStage_3 # corrected standard errors are in the ivreg object
          , title = "Method-of-Moments IVE Estimate"
          , file="C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_4_c.doc")

### Final Model
### Figure 5
tab_model(ols_final_1stStage,ols_final_2ndStage # corrected standard errors are in the ivreg object
          , title = "Method-of-Moments IVE Estimate"
          , file="C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\R Projects\\Price Elasticity\\supporting docs\\fig_5.doc")
