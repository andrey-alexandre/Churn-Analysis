library(readr)
library(stringr)
library(tidymodels)
library(censored)
library(Hmisc)


cindex <- function(truth, estimate){
  rcorr.cens(x=estimate, S=truth)
}


cindex <- new_numeric_metric(cindex, direction = "maximize")


extract <- function(){
  data <- 
    read_csv('./Data/Raw/Telco_Costumer_Churn.csv')
  
  return(data)
}


treat <- function(data){
  treated_data <- data %>% 
    filter(tenure != 0) %>%
    mutate(
      PaymentMethod=ifelse(str_detect(PaymentMethod, 'automatic'), 'Automatic', PaymentMethod), 
      Churn=ifelse(Churn == 'Yes', 1 ,0)
    )  %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(Partner=relevel(Partner,'Yes'),
           Dependents=relevel(Dependents,'Yes'),
           InternetService=relevel(InternetService,'No'),
           Contract=relevel(Contract,'Two year')) %>% select(-contains('ID'),
                              -MultipleLines, -OnlineBackup, -OnlineSecurity, 
                              -DeviceProtection, -TechSupport, -StreamingTV, 
                              -StreamingMovies, -gender, -SeniorCitizen)
  
  return(treated_data)
}


train <- function(ml_spec, data){
  set.seed(1)
  
  wf <- 
    workflow() %>%
    add_variables(outcomes = c(tenure, Churn),
                  predictors = everything()) %>% 
    add_model(ml_spec, formula = Surv(tenure, Churn) ~ .)
  
  fit_rs <- 
    wf %>%
    fit(data)
  
  return(fit_rs)
}


df_treated <- extract() %>% treat() 
data_split <- initial_split(df_treated, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

train_obs_surv <- Surv(train_data$tenure, train_data$Churn)
obs_surv <- Surv(test_data$tenure, test_data$Churn)

bagt_spec <- 
  bag_tree(cost_complexity = 0) %>%
  set_engine("rpart") %>% 
  set_mode("censored regression") 
bagt_fitted <- train(bagt_spec, train_data)
bagt_y_pred <- predict(bagt_fitted, test_data, type = "time")
bagt_ci <- rcorr.cens(x=bagt_y_pred$.pred_time, S=obs_surv)
bagt_p <-
  data.frame('y_pred'=bagt_y_pred$.pred_time, 'y'=test_data$tenure, 'color'=factor(test_data$Churn)) %>% 
  ggplot(aes(x=y_pred, y=y, col=color)) + 
  geom_point() +
  geom_abline() + 
  ggtitle("Bag Model")

boostt_spec <- 
  boost_tree(trees = 30) %>%
  set_engine("mboost") %>% 
  set_mode("censored regression") 
boostt_fitted <- train(boostt_spec, train_data)
boostt_y_pred <- predict(boostt_fitted, test_data, type = "time")
boostt_ci <- rcorr.cens(x=boostt_y_pred$.pred_time, S=obs_surv)
boostt_p <-
  data.frame('y_pred'=boostt_y_pred$.pred_time, 'y'=test_data$tenure, 'color'=factor(test_data$Churn)) %>% 
  ggplot(aes(x=y_pred, y=y, col=color)) + 
  geom_point() +
  geom_abline() + 
  ggtitle("Boost Model")

sr_spec <- 
  survival_reg(dist = "logistic") %>%
  set_engine("survival") %>% 
  set_mode("censored regression") 
sr_fitted <- train(sr_spec, train_data)
sr_y_pred <- predict(sr_fitted, test_data, type = "time")
sr_ci <- rcorr.cens(x=sr_y_pred$.pred_time, S=obs_surv)
sr_p <- 
  data.frame('y_pred'=sr_y_pred$.pred_time, 'y'=test_data$tenure, 'color'=factor(test_data$Churn)) %>% 
  ggplot(aes(x=y_pred, y=y, col=color)) + 
  geom_point() +
  geom_abline() + 
  ggtitle("Regression Model")

ph_spec <- 
  proportional_hazards() %>%
  set_engine("survival") %>% 
  set_mode("censored regression")
ph_fitted <- train(ph_spec, train_data)
ph_y_pred <- predict(ph_fitted, test_data, type = "time")
ph_ci <- rcorr.cens(x=ph_y_pred$.pred_time, S=obs_surv)
ph_p <- 
  data.frame('y_pred'=ph_y_pred$.pred_time, 'y'=test_data$tenure, 'color'=factor(test_data$Churn)) %>% 
  ggplot(aes(x=y_pred, y=y, col=color)) + 
  geom_point() +
  geom_abline() + 
  ggtitle("PH Model")

cat('Bag Tree: ', round(bagt_ci[1], 3), '\n',
    'Boost Tree: ', round(boostt_ci[1], 3), '\n',
    'Logistic Regression: ', round(sr_ci[1], 3), '\n',
    'PH: ', round(ph_ci[1], 3), '\n', sep = '')
gridExtra::grid.arrange(bagt_p, boostt_p, sr_p, ph_p, ncol=2)
