library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(janitor)

categories <- read_csv("raw_data/categories copy.csv") %>%
  clean_names()


# Define UI for data upload app ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Edinburgh Tool Library"),
  dashboardSidebar(

    # The dynamically-generated user panel
    uiOutput("userpanel"),
    sidebarMenu(
      menuItem("Upload Files", tabName = "uploader", icon = icon("jedi-order")),
      menuItem("Data Viz", tabName = "viz", icon = icon("dashboard"))
    )
  ),

  # Sidebar panel for inputs ----
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "uploader",

        # Input: Select loans file ----
        fileInput("file1", "Choose the loans CSV File",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        
        # Input: Select usage file ----
        fileInput("file2", "Choose the usage CSV File",
                  multiple = TRUE,
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                  )
        ),

        # Input: Select number of rows to display ----
        radioButtons("disp", "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        ),
        
        # Output: Raw Data file 
        tableOutput("loans"),
        tableOutput("usage")
      ),
      
      
      tabItem(tabName = "viz",
              fluidRow(column(4,
              tableOutput("top_tools")),
              column(8,
              plotOutput("category_plot")
              )),
              fluidRow(textOutput("savings")))
    )
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
    
    raw_loans <- eventReactive(input$file1, {
        read.csv(input$file1$datapath) %>% 
            clean_names()
    })
    
    raw_usage <- eventReactive(input$file2, {
        read.csv(input$file2$datapath) %>% 
            clean_names()
    })
    
    clean_data <- reactive({
        raw_loans() %>%
            mutate(due_date = as.Date(due_date, format = "%d/%m/%Y"),
                   checked_out = as.POSIXct(checked_out, format = "%d/%m/%Y %H:%M"),
                   checked_in = as.POSIXct(checked_in, format = "%d/%m/%Y %H:%M")
            ) %>%
            left_join(categories, by = "item_id") %>%
            mutate(month = month(checked_out, label = T))
    })
    
  output$loans <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    if (input$disp == "head") {
      return(head(raw_loans()))
    }
    else {
      return(raw_loans())
    }
  })
  
  output$usage <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      if (input$disp == "head") {
          return(head(raw_usage()))
      }
      else {
          return(raw_usage())
      }
  })

output$top_tools <- renderTable({
    clean_data() %>% 
        group_by(month, item_name) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% 
        arrange(month, desc(count)) %>% 
        left_join(categories, by = c("item_name" = "name")) %>% 
        select(month, item_name, categories, count) %>% 
        slice_max(1) 
})
 

output$category_plot <- renderPlot({
    clean_data() %>%
        group_by(month, categories) %>% 
        drop_na(categories) %>% 
        summarise(count = n()) %>% 
        ggplot(aes(x = month, y = count, col = categories, group = categories)) +
        geom_line() +
        labs(x = "Month", 
             y = "Count",
             col = "Category",
             title = "2019 Loans by Category"
        ) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_brewer(palette="Paired")
})
    

savings_val <- reactive({ 
    clean_data() %>%
    filter(is.na(renewal)) %>% 
    group_by(user_id) %>% 
    summarise(savings = sum(replacement_cost)) %>% 
    drop_na(savings)
})

output$savings <- renderText({
    
        paste("Average Savings: £", mean(savings_val()$savings))
    })


}
# Run the app ----
shinyApp(ui, server)
