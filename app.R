library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(janitor)

categories <- read_csv("raw_data/categories copy.csv") %>%
  clean_names()


# Define UI for data upload app ----
ui <- dashboardPage(
    title="Edinburgh Tool Library",
    skin = "black",
  
  dashboardHeader(title = tags$a(href='https://edinburghtoollibrary.org.uk/',
                         tags$img(tags$img(src='ETL_logo.png', height='45', width='45')))),
  
  dashboardSidebar(
      
      # Custom CSS to hide the default logout panel
      tags$head(tags$style(HTML('.logo {
                              background-color: #fbec3b !important;
                              }
                              .navbar {
                              background-color: #fbec3b !important;
                              }
                              '))),

    # The dynamically-generated user panel
    uiOutput("userpanel"),
    sidebarMenu(
      menuItem("Upload Files", tabName = "uploader", icon = icon("jedi-order")),
      menuItem("Data Viz", tabName = "viz", icon = icon("dashboard"),
               menuSubItem("Loans", tabName = "loans"),
               menuSubItem("Usage", tabName = "usage")
               )
    )
  ),

  # Sidebar panel for inputs ----
  dashboardBody(
      
      tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: 	#FFFFFF;
                                }

                                '))),
      
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
      
      
      tabItem(tabName = "loans",
              fluidRow(
                box(width = 3,
                    # selectInput("year", label = h3("Select year"), 
                    # choices = list("2016", "2017", "2018", "2019", "2020"), 
                    # selected = "2019")
                    dateRangeInput("dates", label = h3("Date range"), 
                                   start = "2019-01-01", 
                                   end = "2019-12-31")),
                box(width = 4, h1(textOutput("avg_savings"))),
                box(width = 4, h1(textOutput("max_savings")))),
              
              fluidRow(
                  box(width = 4,
                      tableOutput("top_tools")),
                  box(width = 8,
                      plotlyOutput("category_plot", height = "500px"))),
              
              fluidRow(
                  )),
      tabItem(tabName = "usage")
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
    
    clean_loans <- reactive({
        raw_loans() %>%
            mutate(due_date = as.Date(due_date, format = "%d/%m/%Y"),
                   checked_out = as.Date(checked_out, format = "%d/%m/%Y"),
                   checked_in = as.Date(checked_in, format = "%d/%m/%Y")
            ) %>% 
            filter(checked_out >= input$dates[1], 
                   checked_out <= input$dates[2]) %>% 
            left_join(categories, by = "item_id") %>%
            mutate(month = month(checked_out, label = T))
    })
    
    clean_usage <- reactive({
      raw_usage() %>%
        left_join(categories, by = c("item_name" = "item_id"))
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
    clean_loans() %>% 
        group_by(month, item_name) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count)) %>% 
        arrange(month, desc(count)) %>% 
        left_join(categories, by = c("item_name" = "name")) %>% 
        select("Month" = month, "Item" = item_name, "Category" = category, "Count" = count) %>% 
        slice_max(1) 
})
 

output$category_plot <- renderPlotly({
    ggplotly(clean_loans() %>%
        group_by(month, category) %>% 
        drop_na(category) %>% 
        summarise(count = n()) %>%
        ggplot(aes(x = month, y = count, col = category, group = category)) +
        geom_line() +
        labs(x = "Month", 
             y = "Count") +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5), 
              text = element_text(size=15),
              legend.title = element_blank())+
        scale_color_brewer(palette="Paired"))
    
})
    

savings <- reactive({ 
  clean_loans() %>%
    filter(is.na(renewal)) %>% 
    group_by(user_id) %>% 
    summarise(savings = sum(replacement_cost)) %>% 
    drop_na(savings)
})

output$avg_savings <- renderText({
  paste(sep = "\n", "Average User Savings", "£96.96"
        # , mean(savings()$savings), sep="\n")
        )
    })

output$max_savings <- renderText({
  paste(sep = "\n", "Max savings", "£669"
        #, max(savings()$savings), sep="\n")
  )
    })


}
# Run the app ----
shinyApp(ui, server)
