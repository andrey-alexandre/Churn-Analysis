library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(survival)
library(coin)
library(muhaz)
# source('./Code/Modelling/Churn.R')


# Define UI for application that draws a histogram
ui <- 
  navbarPage("Churn Analysis",
             tabPanel('EDA',
                      fluidPage(
                        fluidRow(
                          column(width=6,
                                 selectInput("EDAGroupA",
                                             label=h4("Select variable 1 to be analyzed"),
                                             choices=
                                               list('Partner',
                                                    'Dependents', 'PhoneService',
                                                    'InternetService', 'Contract',
                                                    'PaperlessBilling', 'PaymentMethod'),
                                             selected = 'Partner')
                          ),
                          column(width=6,
                                 selectInput("EDAGroupB",
                                             label=h4("Select variable 2 to be analyzed"),
                                             choices=
                                               list('Total', 'Partner',
                                                    'Dependents', 'PhoneService',
                                                    'InternetService', 'Contract',
                                                    'PaperlessBilling', 'PaymentMethod'),
                                             selected = 'Total')
                          )
                        ),
                        fluidRow(
                          column(
                            width=6,
                            plotOutput("EDAHeatMap")
                          ),
                          column(
                            width=6,
                            plotOutput("EDAChurnHeatMap")
                          ),
                        ),
                        fluidRow(
                          plotOutput("EDATime")
                        )
                      )
             ),
             tabPanel('Primary',
                      fluidPage(
                        fluidRow(
                          column(
                            width=2,
                            selectInput("PrimaryGroup",
                                        label=h4("Select variable to be analyzed"),
                                        choices=
                                          list('Total', 'Partner',
                                               'Dependents', 'PhoneService',
                                               'InternetService', 'Contract',
                                               'PaperlessBilling', 'PaymentMethod'),
                                        selected = 'Total')
                          ),
                          column(
                            width=10,
                            h4("In the first analysis it was used a Kaplan Meyer model to estimate the survival probabilities of the groups. In the following plot it's possible to see the survival probability as the time passes by for each group analyzed")
                          )
                        ),
                        fluidRow(
                          column(width=2,
                                 tableOutput('PrimaryTable')),
                          column(width=10,
                                 plotOutput("PrimarySurvivalPlot"),
                                 plotOutput("TimePlot")
                          )
                        )
                      )
             ),
             tabPanel('Treatment',
                      fluidRow(
                        width=12,
                        selectInput("TreatmentGroup", 
                                    label=h4("Select variable to be analyzed"),
                                    choices=
                                      list('Total', 'Partner',
                                           'Dependents', 'PhoneService',
                                           'InternetService', 'Contract',
                                           'PaperlessBilling', 'PaymentMethod'),
                                    selected = 'Total'),
                      ),
                      fluidRow(
                        column(width=3, 
                               tableOutput('TreatmentTable'),
                               textOutput('p_value')),
                        column(width=9,
                               plotOutput("TreatmentSurvivalPlot")
                        )
                      )
             )
             
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  con <- DBI::dbConnect(RMariaDB::MariaDB(), 
                        host = "churn-mysql-container",
                        user = "myuser",
                        password = "password",
                        dbname='churnApp')
  
  X <- 
    tbl(con, 'CostumerChurn') %>% collect() %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate(Treatment = as.factor(Treatment))
  time <- X$tenure
  event <- X$Churn
  
  output$EDAHeatMap <- renderPlot({
    if(input$EDAGroupB == 'Total' | input$EDAGroupA == input$EDAGroupB){
      X %>%
        select(X = input$EDAGroupA) %>%
        group_by(X) %>%
        summarise(N = n()) %>% group_by(X) %>% mutate(Percentual = N/sum(N)) %>%
        ggplot(aes(x=X, y=Percentual))+
        geom_col()+
        geom_text(aes(y=Percentual+.01,
                      label=paste0(N, " (", round(100*Percentual, 2), "%)")))
    }else{
      X %>%
        select(X = input$EDAGroupA, Y = input$EDAGroupB) %>%
        group_by(X, Y) %>%
        summarise(N=n()) %>%
        mutate(Percentual = N/sum(N)) %>%
        ggplot(aes(x=X, y=Y, fill=Percentual)) +
        geom_raster() +
        scale_fill_gradient(low = "#33CCFF", high = "#003366", limits=0:1, guide=F) +
        xlab(input$EDAGroupA) +
        ylab(input$EDAGroupB)
    }
  })
  output$EDAChurnHeatMap <- renderPlot({
    if(input$EDAGroupB == 'Total'){
      X %>%
        select("Churn", Xgroup = input$EDAGroupA) %>%
        mutate(Churn = ifelse(Churn == 1, "Yes", "No")) %>%
        group_by(Churn, Xgroup) %>%
        summarise(N = n()) %>%
        mutate(Percentual = N/sum(N)) %>%
        ggplot(aes(x=Xgroup, y=Churn, fill=Percentual)) +
        geom_raster() +
        scale_fill_gradient(low = "#33CCFF", high = "#003366", limits=0:1) +
        xlab(input$EDAGroupA) +
        ylab('Is churn?')
    }else{
      X %>%
        select("Churn", Xgroup = input$EDAGroupA, Ygroup = input$EDAGroupB) %>%
        tidyr::gather("Variables", "Values", -Churn) %>%
        mutate(Churn = ifelse(Churn == 1, "Yes", "No"),
               Variables = factor(Variables, levels=c("Xgroup", "Ygroup"), labels=c(input$EDAGroupA, input$EDAGroupB))) %>%
        group_by(Churn, Variables, Values) %>%
        summarise(N = n()) %>% ungroup %>%
        group_by(Variables, Values) %>% mutate(Percentual = N/sum(N)) %>%
        ggplot(aes(x=Values, y=Churn, fill=Percentual)) +
        geom_raster() +
        scale_fill_gradient(low = "#33CCFF", high = "#003366", limits=0:1) +
        facet_grid(~Variables, scales='free_x')
    }
  })
  output$EDATime <- renderPlot({
    if(input$EDAGroupB == 'Total' | input$EDAGroupA == input$EDAGroupB){
      X %>%
        select(Date, group = input$EDAGroupA, Churn) %>%
        group_by(Date, group) %>% summarise(Churn = sum(Churn)) %>%
        mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
        tidyr::gather("DatePart", "DateValue", -Churn, -Date, -group) %>%
        group_by(DatePart, DateValue, group) %>% summarise(TotalChurn = mean(Churn)) %>%
        ggplot(aes(x=DateValue, y=TotalChurn, col=group)) +
        geom_line() +
        facet_grid(~DatePart, scales="free_x") +
        xlab("")
    }else{
      data %>%
        select(Date, groupA = input$EDAGroupA, groupB = input$EDAGroupB, Churn) %>%
        group_by(Date, groupA, groupB) %>% summarise(Churn = sum(Churn)) %>%
        mutate(Year = year(Date), Month = month(Date), Day = day(Date)) %>%
        tidyr::gather("DatePart", "DateValue", -Churn, -Date, -groupA, -groupB) %>%
        tidyr::gather('Group', 'GroupValue', -DatePart, -DateValue, -Churn, -Date) %>%
        mutate(Group = factor(Group, levels=c('groupA', 'groupB'), labels=c(input$EDAGroupA, input$EDAGroupB))) %>%
        group_by(DatePart, DateValue, Group, GroupValue) %>% summarise(TotalChurn = mean(Churn)) %>%
        ggplot(aes(x=DateValue, y=TotalChurn, col=GroupValue)) +
        geom_line() +
        facet_grid(Group~DatePart, scales="free_x") +
        xlab("")
    }
    
  })
  
  PrimaryTable_ <- reactive({
    if(input$PrimaryGroup == 'Total'){
      churn_percentual <- data.frame(event=event) %>% summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n())
      
      survival_matrix <- survfit(Surv(time,abs(event-1)) ~ 1)$surv %>% matrix(nrow = 72)
      median_time <- colSums(survival_matrix > .50)+1; names(median_time) <- 'Total'
      
      table_ <- bind_cols(churn_percentual, data.frame(median_time, group='Total')) %>% select(group, churn_percentual, median_time, N)
    }else{
      group <- X[, input$PrimaryGroup, T]
      churn_percentual <- data.frame(group=group, event=event) %>% group_by(group) %>% summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n())
      
      survival_matrix <- survfit(Surv(time,abs(event-1)) ~ group)$surv %>% matrix(nrow = 72)
      median_time <- colSums(survival_matrix > .50)+1; names(median_time) <- levels(group)
      
      table_ <- bind_cols(churn_percentual, data.frame(median_time)) %>% select(group, churn_percentual, median_time, N)
    }
    
    return(table_)
  })
  output$PrimaryTable <- renderTable(PrimaryTable_())
  output$PrimarySurvivalPlot <- renderPlot({
    
    if(input$PrimaryGroup == 'Total'){
      kmsurvival <- survfit(Surv(time, event) ~ 1)
      summary_kmsurvival <- summary(kmsurvival)
      
      data.frame(time=kmsurvival$time, prob=kmsurvival$surv) %>%
        ggplot(aes(x = time, y = prob)) +
        geom_line(color='red') +
        labs(x = 'Time', y = 'Survival probability', title = paste('Survival probability for', input$PrimaryGroup)) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))
    }else{
      group <- X[, input$PrimaryGroup, T]
      kmsurvival <- survfit(Surv(time, event) ~ group)
      summary_kmsurvival <- summary(kmsurvival)
      
      group <- rep(levels(group), kmsurvival$strata)
      group <- ifelse(is.na(group), 1, group)
      
      
      data.frame(time=kmsurvival$time, group=group, prob=kmsurvival$surv) %>%
        ggplot(aes(x = time, y = prob, col = group)) +
        geom_line() +
        labs(x = 'Time', y = 'Survival probability', title = paste('Survival probability for', input$PrimaryGroup)) +
        scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))
    }
    
    
  })
  output$TimePlot <- renderPlot({
    if(input$PrimaryGroup == 'Total'){
      data.frame(time, event) %>%
        select(time, event) %>%
        group_by(time) %>%
        summarize(N = sum(event)) %>%
        ggplot(aes(x=time, y=N)) +
        geom_col(position='dodge', fill='grey') +
        labs(x = 'Time', y = 'Number of churn', title = paste('Number of churn for', input$PrimaryGroup))
    }else{
      group <- X[, input$PrimaryGroup, T]
      data.frame(group, time, event) %>%
        select(group, time, event) %>%
        group_by(group, time) %>%
        summarize(N = sum(event)) %>%
        ggplot(aes(x=time, y=N, fill=group)) +
        geom_col(position='dodge') +
        labs(x = 'Time', y = 'Number of churn', title = paste('Number of churn for', input$PrimaryGroup))
      
    }
  })
  
  TreatmentTable_ <- reactive({
    if(input$TreatmentGroup == 'Total'){
      survival_data <- 
        X %>% 
        select(time=tenure, event=Churn, Treatment)
      churn_percentual <- 
        survival_data %>% 
        group_by(Treatment) %>% 
        summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n())
      
      kmsurvival <- survfit(Surv(time, event) ~ Treatment, 
                            data=survival_data) 
      Treatment <- rep(levels(survival_data$Treatment), 
                       kmsurvival$strata)
      survival_list <- split(kmsurvival$surv, Treatment)
      median_time <- sapply(survival_list, function(x) sum(x>.5)+1); names(median_time) <- levels(survival_data$Treatment)
      
      TreatmentTable_ <- bind_cols(churn_percentual, data.frame(median_time)) %>% select(Treatment, churn_percentual, median_time, N)
    }else{
      survival_data <- 
        X %>% 
        select(time=tenure, event=Churn, Treatment, strata=input$TreatmentGroup) %>% 
        mutate(Treatment=paste(Treatment, strata, sep='|'), 
               Treatment=factor(Treatment))
      churn_percentual <- 
        survival_data %>% 
        group_by(Treatment) %>% 
        summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n()) %>% 
        tidyr::separate(Treatment, c('Treatment', 'strata'), '\\|')
      
      kmsurvival <- survfit(Surv(time, event) ~ Treatment, 
                            data=survival_data) 
      Treatment <- rep(levels(survival_data$Treatment), 
                       kmsurvival$strata)
      survival_list <- split(kmsurvival$surv, Treatment)
      median_time <- sapply(survival_list, function(x) sum(x>.5)+1); names(median_time) <- levels(survival_data$Treatment)
      
      TreatmentTable_ <- bind_cols(churn_percentual, data.frame(median_time)) %>% select(Treatment, strata, churn_percentual, median_time, N)
    }
    
    return(TreatmentTable_)
  })
  output$TreatmentTable <- renderTable(TreatmentTable_())
  output$p_value <- 
    renderText('Há diferença significativa entre os tratamentos')

  output$TreatmentSurvivalPlot <- renderPlot({
    
    if(input$TreatmentGroup == 'Total'){
      survival_data <- 
        X %>% 
        select(time=tenure, event=Churn, Treatment)
      kmsurvival <- survfit(Surv(time, event) ~ Treatment, 
                            data=survival_data) 
      Treatment <- rep(levels(survival_data$Treatment), 
                       kmsurvival$strata)
      
      data.frame(time=kmsurvival$time, Treatment=Treatment, prob=kmsurvival$surv) %>% 
        ggplot(aes(x = time, y = prob, col = Treatment)) +
        geom_line() +
        labs(x = 'Time', y = 'Survival probability', title='Survival probability') +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1))
    }else{
      survival_data <- 
        X %>% 
        select(time=tenure, event=Churn, Treatment, strata=input$TreatmentGroup) %>% 
        mutate(Treatment=paste(Treatment, strata, sep='|'), 
               Treatment=factor(Treatment))
      kmsurvival <- survfit(Surv(time, event) ~ Treatment, 
                            data=survival_data) 
      Treatment <- rep(levels(survival_data$Treatment), 
                       kmsurvival$strata)
      
      data.frame(time=kmsurvival$time, Treatment=Treatment, prob=kmsurvival$surv) %>% 
        tidyr::separate(Treatment, c('Treatment', 'strata'), '\\|') %>% 
        ggplot(aes(x = time, y = prob, col = Treatment)) +
        geom_line() +
        labs(x = 'Time', y = 'Survival probability', title='Survival probability') +
        scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
        facet_grid(cols=vars(strata))
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
