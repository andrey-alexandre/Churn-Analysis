library(ggplot2)
library(dplyr)
library(muhaz)

# Tempo de sobrevivência mediano
sum(survfit(Surv(time,abs(event-1)) ~ 1)$surv > .50)+1

result.simple <- muhaz(time, event, subset=group, max.time=72,
                       bw.grid=2.25, bw.method="global", b.cor="none")
plot(result.simple)

group <- X[, 'MonthlyCharges', T]
data.frame(group, time, event=factor(event)) %>%  
  ggplot(aes(x=group, y=time, col=event))+
  geom_point()
  