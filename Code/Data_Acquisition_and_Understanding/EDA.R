library(readr)
library(dplyr)
library(stringr)
library(survival)
library(ggplot2)
library(survsim)
source('./Code/Functions/metrics.R')

# Extraction ----
data <- 
  read_csv('./Data/Raw/Telco_Costumer_Churn.csv')%>% 
  filter(tenure != 0) %>%
  mutate(PaymentMethod=ifelse(str_detect(PaymentMethod, 'automatic'),
                              'Automatic', 
                              PaymentMethod),
         Churn=ifelse(Churn == 'Yes', 1 ,0))  %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(Partner=relevel(Partner,'Yes'),
         Dependents=relevel(Dependents,'Yes'),
         InternetService=relevel(InternetService,'No'),
         Contract=relevel(Contract,'Two year'))

# Define variables ----
time <- data$tenure
event <- data$Churn
X <- data %>% select(Partner, Dependents, PhoneService, InternetService, Contract, PaperlessBilling, PaymentMethod, 
                     MonthlyCharges, TotalCharges)


# Descriptive statistics ----
summary(time)
density(time) %>% plot

summary(event)
table(event) %>% prop.table() %>% round(2)

summary(X)

ggplot(aes(x=TotalCharges, y=tenure, col=InternetService), data=data) +
  geom_point()

# General EDA ----

## Payment Method ----
data %>% 
  group_by(PaymentMethod, Churn) %>% 
  summarise(N=n()) %>% 
  mutate(P=str_c(round(100*N/sum(N),2), '%'),
         Churn = factor(Churn)) %>% 
  ggplot(aes(x=PaymentMethod, y=N, fill=Churn)) +
  geom_col(position="dodge")+
  geom_text(aes(label=P), colour = "white", size = 5,
            vjust = 1.5, position = position_dodge(.9))

## Partner ----
data %>%
  mutate(Churn = factor(Churn)) %>% 
  ggplot(aes(x=Partner, fill=Churn)) +
  geom_bar(position="dodge")

## Monthly Charges----
data %>%
  mutate(Churn = factor(Churn)) %>% 
  ggplot(aes(x=MonthlyCharges, y=Churn, fill=Churn)) +
  geom_boxplot()


# Kaplan-Meier non-parametric analysis ----
## General KM ----
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary_kmsurvival <- summary(kmsurvival)
data.frame(time=summary_kmsurvival$time, prob=summary_kmsurvival$surv) %>% 
  ggplot(aes(x=time, y=prob)) +
  geom_line(color='red') +
  labs(x='Tempo', y='Probabilidade de sobrevivência') +
  scale_y_continuous(limits = c(0, 1))

## Group KM ----
nomes <- colnames(select_if(X, is.factor))
p <- list()

### Grouped KM ---- 
#This codes creates multiple plots to analyse the survival probabilities by every group
for(i in nomes){
  group <- X[, i, T]
  kmsurvival <- survfit(Surv(time, event) ~ group)
  summary_kmsurvival <- summary(kmsurvival)
  
  p[[i]] <- 
    data.frame(time = kmsurvival$time, group = rep(levels(group), kmsurvival$strata), prob = kmsurvival$surv) %>% 
    ggplot(aes(x = time, y = prob, col = group)) +
    geom_line() +
    labs(x = 'Time', y = 'Survival rate', title = paste('Survival by', i)) +
    scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))
}
pdf(file = 'Survival.pdf')
p
dev.off()

# Cox proportional hazard model - coefficients and hazard rates ----
coxph <- coxph(Surv(time, event) ~ .,
               data=X)
summary_coxph <- summary(coxph)
print(summary_coxph)
