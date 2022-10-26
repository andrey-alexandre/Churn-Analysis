library(readr)
library(dplyr)
library(stringr)
library(survival)
library(ggplot2)
library(survsim)
source('./Code/Functions/metrics.R')

# Treatment simulation
set.seed(145)
sim.data <- simple.surv.sim(n=1000, foltime=36, dist.ev=c('llogistic'),
                            anc.ev= .5, beta0.ev=5, anc.cens=.3,
                            beta0.cens=2, beta=list(c(-0.4)), 
                            x=list(c("bern", 0.5)))

kmsurvival_t <- survfit(Surv(stop, status) ~ x, data = sim.data)
summary_kmsurvival_t <- summary(kmsurvival_t)
data.frame(time = summary_kmsurvival_t$time, group = summary_kmsurvival_t$strata, 
           prob = summary_kmsurvival_t$surv) %>% 
  ggplot(aes(x = time, y = prob, col = group)) +
  geom_line() +
  labs(x = 'Tempo', y = 'Probabilidade de sobrevivência', title = paste('Sobrevivência por tratamento')) +
  scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))

weibull_t <- survreg(Surv(stop, status) ~ x, data = sim.data, dist="weibull")
se(sim.data$stop, predict(weibull_t, type ='response')) %>% mean %>% sqrt
pe(sim.data$stop, predict(weibull_t, type ='response')) %>% abs %>% median