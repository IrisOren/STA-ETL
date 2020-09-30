#################################################################
##                          Libraries                        ----
#################################################################


library(shiny)
library(shinydashboard) # Using library(shinydashboard) for layout
library(tidyverse)
library(lubridate)
library(plotly)
library(janitor)



## ================================================================
##                  Reading in categories data                 ----
## ================================================================

categories <- read_csv("raw_data/categories copy.csv") %>%
  clean_names()



##################################################################
##                              UI                            ----
##################################################################

ui <- dashboardPage( #UI dashboard page ----
  title = "Edinburgh Tool Library",

  skin = "black", # This is just a colour theme for shinyDashboard

  dashboardHeader( #UI dashboard header ----
    titleWidth = 250, 
    title = tags$a(
      href = "https://edinburghtoollibrary.org.uk/",
      target = "_blank",
      tags$img(tags$img(src = "ETL_logo.png", height = "50px", width = "50px"))
    )
  ), # Puts the ETL logo in the header and has it link to ETL website

  dashboardSidebar(
    width = 250,

    # Custom CSS ----
    # to recolour the dashboard using ETL yellow = #fbec3b and adjust
    # the depth of the navbar
    tags$head(tags$style(HTML("
    .logo {
      background-color: #fbec3b !important;
    }
    
    .main-header .logo {
      height: 55px;
    }
    
    .sidebar-toggle {
      height: 55px;
    }
    
    .navbar {
      background-color: #fbec3b !important;
    }
                              "))),

    # Sidebar menu layout and widgets ----
    sidebarMenu(
      menuItem("Upload Files", 
               tabName = "uploader", 
               icon = icon("file-upload")),
      menuItem("Data Viz",
               tabName = "viz", 
               icon = icon("chart-bar"),
               dateRangeInput("dates", label = h3("Date range")),
        menuSubItem("Loans", tabName = "loans"),
        menuSubItem("Usage", tabName = "usage"),
        menuSubItem("User Stories", tabName = "user_stories")
      )
    )
  ),


  dashboardBody(
    # Custom CSS to recolour the dashboard using ETL yellow = #fbec3b ----
    tags$head(tags$style(HTML("
    
    .content-wrapper, .right-side {
      background-color: 	#f5f5f5;
    }
    
    .box.box-solid.box-primary>.box-header {
      color:#212e31;
      background:#fbec3b;
    }
    
    .box.box-solid.box-primary{
      border-bottom-color:#fbec3b;
      border-left-color:#fbec3b;
      border-right-color:#fbec3b;
      border-top-color:#fbec3b;
    }
                              "))),

    tabItems(
      # First tab content
      tabItem(
        tabName = "uploader",
        fluidRow(

        # Input: Select loans file ----
        column(4,
        fileInput("file1", "Upload the LOANS file",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )),

        # Input: Select usage file ----
        column(4,
        fileInput("file2", "Upload the USAGE file",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        )),
        
        # Input: Select categories file ----
        column(4,
        fileInput("file3", "Upload the CATEGORIES file",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  )
        ))),

        fluidRow(
        # Input: Select number of rows to display ----
        column(4,
        radioButtons("disp", "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        )),
        
        # test button ----
        # test button for development reasons so I don't have to keep uploading
        # for minor changes. 
        #TODO Could keep with synthetic data for demo purposes? 
        
        column(4, 
          actionButton("test", label = "Test Sample")
               )), 
        
        

        # Output: Raw Data file
        tableOutput("loans"),
        tableOutput("usage")
      ),


      tabItem( # Loans tab ----
        tabName = "loans",
        fluidRow(
          # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
          box( # display avergae savings
            title = "Average Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 2,
            h1(textOutput("avg_savings"))
          ),
          box( #display max savings
            title = "Max Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 2,
            h1(textOutput("max_savings"))
          )
        ),

        fluidRow(
          box( # top tools/category table
            title = "Monthly Top Tools",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 4,
            tableOutput("top_tools")
          ),
          box( # Loan category plot
            title = "Loans by Category",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 8,
            plotlyOutput("category_plot", height = "500px")
          )
        )
      ),

      tabItem( # Usage tab ----
        tabName = "usage",

        fluidRow(
          box( # The bar chart for loans by location
            title = "Loans by Location for entire data",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotOutput("location_plot")
          )
        )
      ),

      tabItem( # User Stories tab ----
        tabName = "user_stories",
        fluidRow(
          box( # category or tools selection button
            title = "Categories or Tools?",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            radioButtons("cat_or_tool",
              label = NULL,
              inline = TRUE,
              choices = list("Categories" = "category", "Tools" = "item_name"),
              selected = "category"
            )
          ),

          box( # User selection buttons
            title = "Which User?",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            radioButtons("user_choice",
              label = NULL,
              inline = TRUE,
              choices = list("Top" = "top", "2nd Top" = "top2"),
              selected = "top"
            )
          ),

          box( # display user savings
            title = "User Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            h5(textOutput("top_user_savings"))
          )
        ),

        fluidRow(
          box( # display plot for top user loans
            width = 12,
            title = "Top User",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput("top_user")
          )
        )
      )
    )
  )
)









##################################################################
##                            Server                            ##
##################################################################

server <- function(input, output, session) {
  
  # Raw data reading from user upload ----
  
  raw_loans <- eventReactive(input$file1, {
    read.csv(input$file1$datapath) %>%
      clean_names() #tidies up column names 
  })

  raw_usage <- eventReactive(input$file2, {
    read.csv(input$file2$datapath) %>%
      clean_names() #tidies up column names 
  })

  #######################################################
  #Test output----
  raw_loans <- eventReactive(input$test,{
    read.csv("raw_data/loans-export jan-dec 2019.csv") %>%
      clean_names() 
  })
  
  raw_usage <- eventReactive(input$test,{
    read.csv("raw_data/usage-export jan-dec 2019.csv") %>%
      clean_names() 
  })
 ######################################################### 
  
  
  # Data Cleaning ----
  clean_loans <- reactive({
    raw_loans() %>%
      mutate( # formating date
        due_date = as.Date(due_date, format = "%d/%m/%Y"),
        checked_out = as.Date(checked_out, format = "%d/%m/%Y"),
        checked_in = as.Date(checked_in, format = "%d/%m/%Y")
      ) %>%
      filter( #filtering to user input date
        checked_out >= input$dates[1],
        checked_out <= input$dates[2]
      ) %>%
      left_join(categories, by = "item_id") %>% # adds tool categories to the data frame
      mutate(month = month(checked_out, label = T)) # creates a column called month
  })

  
  # Cleans the raw Usage data and adds categories
  clean_usage <- reactive({
    raw_usage() %>%
      left_join(categories, by = "item_id")
  })


  # Date range updater ----
  # Updates the default date range to min and max of raw data
  observe({
    update_date <- raw_loans() %>%
      mutate(
        due_date = as.Date(due_date, format = "%d/%m/%Y"),
        checked_out = as.Date(checked_out, format = "%d/%m/%Y"),
        checked_in = as.Date(checked_in, format = "%d/%m/%Y")
      )

    updateDateRangeInput(session, "dates",
      start = paste(min(update_date$checked_out)),
      end = paste(max(update_date$checked_out))
    )
  })



  # Raw data table render ----
  # Renders the table for display depending on user display input 
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
  
  
  
  # Top Tools Table ----
  output$top_tools <- renderTable({
    clean_loans() %>%
      group_by(
        month,
        item_name
      ) %>%
      summarise(count = n()) %>%
      # arrange(desc(count)) %>%
      arrange(
        month,
        desc(count)
      ) %>%
      left_join(categories,
        by = c("item_name" = "name")
      ) %>%
      select(
        "Month" = month,
        "Item" = item_name,
        "Category" = category,
        "Count" = count
      ) %>%
      slice_max(1)
  })

  # Categories Plot ----
  output$category_plot <- renderPlotly({
    ggplotly(clean_loans() %>%
      group_by(
        month,
        category
      ) %>%
      drop_na(category) %>%
      summarise(count = n()) %>%
      ggplot(aes(
        x = month,
        y = count,
        col = category,
        group = category
      )) +
      geom_line() +
      labs(
        x = "Month",
        y = "Count"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(hjust = 0.5),
        # text = element_text(size = 15),
        legend.title = element_blank()
      ) +
      scale_color_viridis_d(option = "plasma")) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
  })

  # Savings df ----
  savings <- reactive({
    clean_loans() %>%
      filter(renewal != "Renewal") %>%
      group_by(user_id) %>%
      summarise(total_savings = sum(replacement_cost)) %>%
      drop_na(total_savings)
  })

  # Mean savings output ----
  output$avg_savings <- renderText({
    paste0(
      "£",
      round(mean(savings()$total_savings), digits = 2)
    )
  })


  # Max savings Output ----
  output$max_savings <- renderText({
    paste0(
      "£",
      max(savings()$total_savings)
    )
  })

  # Creates reactive user df's ----
  user_df <- reactive({
   
   if (input$user_choice == "top") {
     
     # Finding top user
     clean_loans() %>%
       group_by(user_id) %>%
       count() %>%
       ungroup() %>%
       slice_max(n, n = 1) %>%
       select(user_id) 
     
     } else {
       # Finding 2nd top user
       clean_loans() %>%
         group_by(user_id) %>%
         count() %>%
         ungroup() %>%
         slice_max(n, n = 2) %>%
         arrange(n) %>%
         slice(1) %>%
         select(user_id) }
  })

 
 # User savings ----
  output$top_user_savings <- renderText({
    
    top_user <- clean_loans() %>%
      inner_join(user_df(), by = "user_id") %>%
      filter(renewal != "Renewal") %>%
      drop_na(replacement_cost) %>%
      group_by(item_name) %>%
      mutate(replacement_cost = mean(replacement_cost)) %>% #To remove new loans of the same item
      summarise(total_savings = sum(replacement_cost))

    paste0(
      "£",
      sum(top_user$total_savings)
    )
  })




  
  # User Story plots ----
  output$top_user <- renderPlotly({
    
            ggplotly(clean_loans() %>%
              inner_join(user_df(),
                by = "user_id"
              ) %>%
              filter(renewal != "Renewal") %>%
              group_by(checked_out) %>%
              drop_na(input$cat_or_tool) %>%
              drop_na(checked_out) %>%
              ggplot(aes(
                x = checked_out,
                fill = if (input$cat_or_tool == "category"){ category} else {item_name},
                text = sprintf("Date: %s<br>Type: %s", checked_out, if (input$cat_or_tool == "category"){ category} else {item_name})
              )) +
              labs(
                x = "Date Checked Out",
                y = "Count",
                fill = if (input$cat_or_tool == "category"){"Category"} else {"Tool"}
              ) +
              geom_histogram(bins = 12) +
              theme_classic() +
              scale_fill_viridis_d(option = "viridis"), tooltip = "text") %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE))
          })


# Location Plot ---- 
  output$location_plot <- renderPlot({
    clean_usage() %>%
      filter(!(is.na(home_location))) %>%
      group_by(home_location) %>%
      summarise(total_loans = sum(loans)) %>%
      ggplot(aes(x = home_location, y = total_loans)) +
      geom_col() +
      labs(
        x = "Location",
        y = "Total Loans"
      ) +
      theme_classic()
  })
}



# Runs the app
shinyApp(ui, server)
