library(shiny)
library(ggplot2)
library(dplyr)
library(muhaz)
source('/home/andrey/Projetos/Churn-Analysis/Code/Modelling/Churn.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Churn analysis for a Telcom enterprise"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(width=12,
             selectInput("group", label = h4("Select variable to be analyzed"), 
                         choices = list('Total','Partner', 'Dependents', 'PhoneService', 'InternetService', 'Contract', 'PaperlessBilling', 'PaymentMethod'
                                        #, 'MonthlyCharges', 'TotalCharges'
                         ), 
                         selected = 'Total'),
    ),
    
    # Show a plot of the generated distribution
    fluidRow(
        column(width=2, 
               tableOutput('table')),
        column(width=10,
               plotOutput("SurvivalPlot"),
               plotOutput("TimePlot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    table_ <- reactive({
        if(input$group == 'Total'){
            churn_percentual <- data.frame(event=event) %>% summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n())
            
            survival_matrix <- survfit(Surv(time,abs(event-1)) ~ 1)$surv %>% matrix(nrow = 72)
            median_time <- colSums(survival_matrix > .50)+1; names(median_time) <- 'Total'
            
            table_ <- bind_cols(churn_percentual, data.frame(median_time, group='Total')) %>% select(group, churn_percentual, median_time, N)
        }else{
            group <- X[, input$group, T]
            churn_percentual <- data.frame(group=group, event=event) %>% group_by(group) %>% summarize(churn_percentual = paste(round(100*mean(event), 2), "%"), N=n())
            
            survival_matrix <- survfit(Surv(time,abs(event-1)) ~ group)$surv %>% matrix(nrow = 72)
            median_time <- colSums(survival_matrix > .50)+1; names(median_time) <- levels(group)
            
            table_ <- bind_cols(churn_percentual, data.frame(median_time)) %>% select(group, churn_percentual, median_time, N)
        }
        
        return(table_)
    })
    output$table <- renderTable(table_())

    output$SurvivalPlot <- renderPlot({
        
        if(input$group == 'Total'){
            kmsurvival <- survfit(Surv(time, event) ~ 1)   
            summary_kmsurvival <- summary(kmsurvival)
            
            data.frame(time=kmsurvival$time, prob=kmsurvival$surv) %>% 
                ggplot(aes(x = time, y = prob)) +
                geom_line(color='red') +
                labs(x = 'Time', y = 'Survival probability', title = paste('Survival probability for', input$group)) +
                scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))
        }else{
            group <- X[, input$group, T]
            kmsurvival <- survfit(Surv(time, event) ~ group)
            summary_kmsurvival <- summary(kmsurvival)
            
            group <- rep(levels(group), kmsurvival$strata)
            group <- ifelse(is.na(group), 1, group)
            
            
            data.frame(time=kmsurvival$time, group=group, prob=kmsurvival$surv) %>% 
                ggplot(aes(x = time, y = prob, col = group)) +
                geom_line() +
                labs(x = 'Time', y = 'Survival probability', title = paste('Survival probability for', input$group)) +
                scale_y_continuous(limits = c(0.2, 1), breaks = seq(.2, 1, .1))
        }
        
        
    })
    output$TimePlot <- renderPlot({
        if(input$group == 'Total'){
            data.frame(time, event) %>% 
                select(time, event) %>%
                group_by(time) %>% 
                summarize(N = sum(event)) %>% 
                ggplot(aes(x=time, y=N)) +
                geom_col(position='dodge', fill='grey') +
                labs(x = 'Time', y = 'Number of failures', title = paste('Number of failures for', input$group))
        }else{
            group <- X[, input$group, T]
            data.frame(group, time, event) %>% 
                select(group, time, event) %>%
                group_by(group, time) %>% 
                summarize(N = sum(event)) %>% 
                ggplot(aes(x=time, y=N, fill=group)) +
                geom_col(position='dodge') +
                labs(x = 'Time', y = 'Number of failures', title = paste('Number of failures for', input$group))
            
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
