# Monthly expense calculator in RShiny 
RShiny- Interactive monthly expenses calculator with expenses visualized by month 
# Packages installation 
```
install.packages("shiny")
install.packages("ggplot2")
install.packages("reshape2")

library(shiny)
library(ggplot2)
library(reshape2)
```

# Define UI for the app
```
ui <- fluidPage(
    titlePanel("Monthly Expenses Calculator"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("month", "Select Month:",
                        choices = month.name),
            
            numericInput("rent", "Rent:", value = 0),
            numericInput("internet", "Internet:", value = 0),
            numericInput("groceries", "Groceries:", value = 0),
            numericInput("transport", "Transport:", value = 0),
            numericInput("entertainment", "Entertainment:", value = 0),
            
            actionButton("submit", "Submit")
        ),
        
        mainPanel(
            h3(textOutput("selected_month")),
            plotOutput("expense_plot"),
            plotOutput("comparison_plot"),
            tableOutput("expense_table")
        )
    )
)
```
# Define server logic for the app
```
server <- function(input, output) {
    
    # Reactive values to store expenses for each month
    expenses <- reactiveValues(data = data.frame(
        Month = character(),
        Rent = numeric(),
        Internet = numeric(),
        Groceries = numeric(),
        Transport = numeric(),
        Entertainment = numeric(),
        stringsAsFactors = FALSE
    ))
    
    observeEvent(input$submit, {
        new_data <- data.frame(
            Month = input$month,
            Rent = input$rent,
            Internet = input$internet,
            Groceries = input$groceries,
            Transport = input$transport,
            Entertainment = input$entertainment,
            stringsAsFactors = FALSE
        )
        
        if (input$month %in% expenses$data$Month) {
            expenses$data <- expenses$data[expenses$data$Month != input$month, ]
        }
        
        expenses$data <- rbind(expenses$data, new_data)
    })
    
    output$selected_month <- renderText({
        paste("Expenses for", input$month)
    })
    
    output$expense_plot <- renderPlot({
        if (nrow(expenses$data) > 0) {
            month_data <- subset(expenses$data, Month == input$month)
            expense_melted <- melt(month_data, id.vars = "Month")
            
            ggplot(expense_melted, aes(x = variable, y = value)) +
                geom_col(fill = "skyblue") +
                labs(x = "Expense Category", y = "Amount", title = paste("Expenses Breakdown for", input$month)) +
                theme_minimal()
        }
    })
    
    output$comparison_plot <- renderPlot({
        if (nrow(expenses$data) > 0) {
            expense_melted <- melt(expenses$data, id.vars = "Month")
            
            ggplot(expense_melted, aes(x = variable, y = value, fill = Month)) +
                geom_col(position = "dodge") +
                labs(x = "Expense Category", y = "Amount", title = "Comparison of Monthly Expenses by Category") +
                theme_minimal() +
                scale_fill_brewer(palette = "Set3")
        }
    })
    
    output$expense_table <- renderTable({
        subset(expenses$data, Month == input$month)
    })
}
```
# Run the application
```
shinyApp(ui = ui, server = server)
```
