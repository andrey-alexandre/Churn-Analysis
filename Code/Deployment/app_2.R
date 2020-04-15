library(dplyr)
library(ggplot2)
library(survival)
library(coin)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Churn analysis for a Telcom enterprise"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(width=12,
             selectInput("group", label = h4("Select variable to be analyzed"), 
                         choices = list('Total', 'sex', 'histology'
                                        #, 'MonthlyCharges', 'TotalCharges'
                         ), 
                         selected = 'Total'),
    ),
    
    # Show a plot of the generated distribution
    fluidRow(
        column(width=4, 
               tableOutput('table'),
               textOutput('p_value')),
        column(width=8,
               plotOutput("SurvivalPlot"),
               plotOutput("TimePlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    table_ <- reactive({
        if(input$group == 'Total'){
            survival_data <- 
                glioma %>% 
                select(time, event, group)
            churn_percentual <- 
                survival_data %>% 
                group_by(group) %>% 
                summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n())
            
            kmsurvival <- survfit(Surv(time, event) ~ group, 
                                  data=survival_data) 
            group <- rep(levels(survival_data$group), 
                         kmsurvival$strata)
            survival_list <- split(kmsurvival$surv, group)
            median_time <- sapply(survival_list, function(x) sum(x>.5)+1); names(median_time) <- levels(survival_data$group)
            
            table_ <- bind_cols(churn_percentual, data.frame(median_time)) %>% select(group, churn_percentual, median_time, N)
        }else{
            survival_data <- 
                glioma %>% 
                select(time, event, group, strata=input$group) %>% 
                mutate(group=paste(group, strata, sep='|'), 
                       group=factor(group))
            churn_percentual <- 
                survival_data %>% 
                group_by(group) %>% 
                summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n()) %>% 
                tidyr::separate(group, c('group', 'strata'), '\\|')
            
            kmsurvival <- survfit(Surv(time, event) ~ group, 
                                  data=survival_data) 
            group <- rep(levels(survival_data$group), 
                         kmsurvival$strata)
            survival_list <- split(kmsurvival$surv, group)
            median_time <- sapply(survival_list, function(x) sum(x>.5)+1); names(median_time) <- levels(survival_data$group)
            
            table_ <- bind_cols(churn_percentual, data.frame(median_time)) %>% select(group, strata, churn_percentual, median_time, N)
        }
        
        return(table_)
    })
    output$table <- renderTable(table_())
    
    p_value <- reactive({
        if(input$group == 'Total'){
            survival_data <- 
                glioma %>% 
                select(time, event, group)
            
            test <- logrank_test(Surv(time, event) ~ group, data = survival_data,
                                 distribution = "exact")
            
            p_value <- test@distribution@pvalue(test@statistic@teststatistic)
            text_ <- ifelse(p_value < .05, 'Há diferença significativa entre os tratamentos', 'Não há diferença significativa entre os tratamentos')
            
        }else{
            survival_data <- 
                glioma %>% 
                select(time, event, group, strata=input$group)
            
            test <- logrank_test(Surv(time, event) ~ group|strata, data = survival_data,
                                 distribution = approximate(nresample = 10000))
            
            p_value <- test@distribution@pvalue(test@statistic@teststatistic)
            text_ <- ifelse(p_value < .05, 'Há diferença significativa do tratamento para ao menos um dos grupos', 'Não há diferença significativa entre os grupos')
            
        }
        
        
        return(text_)
    })
    output$p_value <- renderText(p_value())
    
    output$SurvivalPlot <- renderPlot({
        
        if(input$group == 'Total'){
            survival_data <- 
                glioma %>% 
                select(time, event, group)
            kmsurvival <- survfit(Surv(time, event) ~ group, 
                                  data=survival_data) 
            group <- rep(levels(survival_data$group), 
                         kmsurvival$strata)
            
            data.frame(time=kmsurvival$time, group=group, prob=kmsurvival$surv) %>% 
                ggplot(aes(x = time, y = prob, col = group)) +
                geom_line() +
                labs(x = 'Time', y = 'Survival probability', title='Survival probability') +
                scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1))
        }else{
            survival_data <- 
                glioma %>% 
                select(time, event, group, strata=input$group) %>% 
                mutate(group=paste(group, strata, sep='|'), 
                       group=factor(group))
            kmsurvival <- survfit(Surv(time, event) ~ group, 
                                  data=survival_data) 
            group <- rep(levels(survival_data$group), 
                         kmsurvival$strata)
            
            data.frame(time=kmsurvival$time, group=group, prob=kmsurvival$surv) %>% 
                tidyr::separate(group, c('group', 'strata'), '\\|') %>% 
                ggplot(aes(x = time, y = prob, col = group)) +
                geom_line() +
                labs(x = 'Time', y = 'Survival probability', title='Survival probability') +
                scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
                facet_grid(cols=vars(strata))
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
