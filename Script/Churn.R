library(readr)
library(dplyr)
library(survival)
library(ggplot2)
library(muhaz)
library(survsim)
se <- function(y, y_hat){
  e <- y - y_hat
  se <- e^2
  
  return(se)
}
pe <- function(y, y_hat){
  e <- y - y_hat
  pe <- e/y
  
  return(pe)
}

data <- 
  read_csv('Data/Telco_Costumer_Churn.csv')%>% 
  filter(tenure != 0) %>%
  mutate(PaymentMethod = ifelse(stringr::str_detect(PaymentMethod, 'automatic'), 'Automatic', PaymentMethod),
         SeniorCitizen = factor(SeniorCitizen, levels = c(0, 1), labels = c('No', 'Yes')),
         Churn = ifelse(Churn == 'Yes', 1 ,0))  %>% 
  mutate_if(is.character, as.factor)

# Define variables 
time <- data$tenure
event <- data$Churn
X <- data %>% select(-contains('ID'), -Churn, -tenure)

# Descriptive statistics
summary(time)
lohist(time)

summary(event)
table(event) %>% prop.table() %>% round(2)

summary(X)

ggplot(aes(x = TotalCharges, y = tenure, col = InternetService), data = data) +
  geom_point()

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary_kmsurvival <- summary(kmsurvival)
data.frame(time = summary_kmsurvival$time, prob=summary_kmsurvival$surv) %>% 
  ggplot(aes(x = time, y = prob)) +
  geom_line(color = 'red') +
  labs(x = 'Tempo', y = 'Probabilidade de sobrevivência') +
  scale_y_continuous(limits = c(0, 1))

# Kaplan-Meier non-parametric analysis by group
nomes <- colnames(select_if(X, is.factor))
p <- list()

# This codes creates multiple plots to analyse the survival probabilities by every group
for(i in nomes){
  group <- X[, i, T]
  kmsurvival <- survfit(Surv(time, event) ~ group)
  summary_kmsurvival <- summary(kmsurvival)
  
  p[[i]] <- 
    data.frame(time = kmsurvival$time, group = rep(levels(group), kmsurvival$strata), prob = kmsurvival$surv) %>% 
    ggplot(aes(x = time, y = prob, col = group)) +
    geom_line() +
    labs(x = 'Tempo', y = 'Probabilidade de sobrevivência', title = paste('Sobrevivência por', i)) +
    scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))
}
pdf(file = 'Survival.pdf')
p
dev.off()

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time, event) ~ .,
               data = select(X, -MultipleLines, -OnlineBackup, -OnlineSecurity, 
                             -DeviceProtection, -TechSupport, -StreamingTV, 
                             -StreamingMovies, -gender, -SeniorCitizen), 
               method="breslow")
summary_coxph <- summary(coxph)

# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS
logistic <-  survreg(Surv(time,event) ~ ., 
                     data = select(X, -MultipleLines, -OnlineBackup, -OnlineSecurity, 
                                   -DeviceProtection, -TechSupport, -StreamingTV, 
                                   -StreamingMovies, -gender, -SeniorCitizen),
                     dist="logistic")
summary(logistic)

se(time, predict(logistic, type ='response')) %>% mean %>% sqrt
pe(time, predict(logistic, type ='response')) %>% abs %>% median

# Treatment simulation
set.seed(145)
sim.data <- simple.surv.sim(n=1000, foltime=36, dist.ev=c('llogistic'),
                            anc.ev= .5, beta0.ev=5, anc.cens=.3,
                            beta0.cens=2, beta=list(c(-0.4)), x=list(c("bern", 0.5)))

kmsurvival_t <- survfit(Surv(stop, status) ~ x, data = sim.data)
summary_kmsurvival_t <- summary(kmsurvival_t)
data.frame(time = summary_kmsurvival_t$time, group = summary_kmsurvival_t$strata, prob = summary_kmsurvival_t$surv) %>% 
  ggplot(aes(x = time, y = prob, col = group)) +
  geom_line() +
  labs(x = 'Tempo', y = 'Probabilidade de sobrevivência', title = paste('Sobrevivência por tratamento')) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))

weibull_t <- survreg(Surv(stop, status) ~ x, data = sim.data, dist="weibull")
se(sim.data$stop, predict(weibull_t, type ='response')) %>% mean %>% sqrt
pe(sim.data$stop, predict(weibull_t, type ='response')) %>% abs %>% median
