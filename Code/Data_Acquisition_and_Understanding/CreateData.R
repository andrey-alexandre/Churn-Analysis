library(readr)
library(dplyr)
library(stringr)
library(lubridate)

data <- 
  read_csv('./Data/Raw/Telco_Costumer_Churn.csv')%>% 
  filter(tenure != 0) %>%
  mutate(PaymentMethod=ifelse(str_detect(PaymentMethod, 'automatic'),
                              'Automatic', 
                              PaymentMethod),
         SeniorCitizen=factor(SeniorCitizen, levels = c(0, 1), 
                              labels = c('No', 'Yes')),
         Churn=ifelse(Churn == 'Yes', 1 ,0))  %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(Partner=relevel(Partner,'Yes'),
         Dependents=relevel(Dependents,'Yes'),
         InternetService=relevel(InternetService,'No'),
         Contract=relevel(Contract,'Two year')) %>%
  mutate(Date = Sys.Date()-30*tenure)

data %>%
  write_csv2('./Data/Preprocessed/Telco_Costumer_Churn.csv')

data %>% 
  select(-contains('ID'), 
         -MultipleLines, -OnlineBackup, -OnlineSecurity, 
         -DeviceProtection, -TechSupport, -StreamingTV, 
         -StreamingMovies, -gender, -SeniorCitizen) %>% 
  mutate(Treatment=rbinom(nrow(.), 1, .5)) %>% 
  write_csv2('./Data/Modelling/Telco_Costumer_Churn.csv')
