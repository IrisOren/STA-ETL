#################################################################
##                          Libraries                        ----
#################################################################


library(shiny)
library(shinydashboard) # Using library(shinydashboard) for layout
library(tidyverse)
library(lubridate)
library(plotly)
library(janitor)
library(bsplus)



## ================================================================
##                  Reading in categories data                 ----
## ================================================================

categories <- read_csv("raw_data/inventory-with categories.csv") %>%
  clean_names()

##################################################################
##                              UI                            ----
##################################################################

ui <- dashboardPage( # UI dashboard page ----
  title = "Edinburgh Tool Library",

  skin = "black", # This is just a colour theme for shinyDashboard

  dashboardHeader( # UI dashboard header ----
    titleWidth = 250,
    title = tags$a(
      href = "https://edinburghtoollibrary.org.uk/",
      target = "_blank",
      tags$img(tags$img(src = "ETL_logo.png", height = "50px", width = "50px"))
    )
  ), # Puts the ETL logo in the header and has it link to ETL website

  dashboardSidebar(
    width = 250,

    # Sidebar menu layout and widgets ----
    sidebarMenu(
      menuItem("Upload Files",
        tabName = "uploader",
        icon = icon("file-upload")
      ),
      menuItem(#"Date",
               tabName = "date", 
               icon = icon("calendar"),
               dateRangeInput("dates", label = "Date range")),
      # menuItem("Data Viz",
      #   tabName = "viz",
      #   icon = icon("chart-bar")),
        menuItem("Loans", tabName = "loans", icon = icon("tools")),
        menuItem("Usage", tabName = "usage", icon = icon("bar-chart")),
        menuItem("Savings", tabName = "savings", icon = icon("coins")),
        menuItem("User Stories", tabName = "user_stories", icon = icon("book"))
      
    )
  ),


  dashboardBody(

    # Sourcing ustom CSS ----
    tags$head(includeCSS("styles.css")),


    tabItems(
      # First tab content
      tabItem(
        tabName = "uploader",
        
        h2("Upload files:"),
        
        fluidRow(

          # Input: Select loans file ----
          column(
            4,
            fileInput("file1", "loans-export [date].csv",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),

          # Input: Select usage file ----
          column(
            4,
            fileInput("file2", "usage-export [date].csv",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          ),

          # Input: Select categories file ----
          column(
            4,
            fileInput("file3", "inventory-with categories.csv",
              multiple = TRUE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            )
          )
        ),

        fluidRow(
          # Input: Select number of rows to display ----
          column(
            4,
            radioButtons("disp", "Display",
              choices = c(
                Head = "head",
                All = "all"
              ),
              selected = "head"
            )
          ),

          # test button ----
          # test button for development reasons so I don't have to keep uploading
          # for minor changes.
          # TODO Could keep with synthetic data for demo purposes?

          column(
            4,
            actionButton("test", label = "Demo") %>% bs_embed_tooltip("Test app with sample data", placement = "bottom")
          )
        ),



        # Output: Raw Data file
        fluidRow(box(
          width = 12,
          title = "Loans",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          tableOutput("loans")
        )),
        fluidRow(box(
          width = 12,
          title = "Usage",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          tableOutput("usage")
        )),
        fluidRow(box(
          width = 12,
          title = "Categories",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary"
        ))
      ),


      tabItem( # Loans tab ----
        tabName = "loans",

        fluidRow(
          box( # Loan category plot
            title = "Loans by Category",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 12,
            plotlyOutput("category_plot", height = "500px")
          )
        ),
        fluidRow(
          box( # top tools/category table
            title = "Monthly Top Tools",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 6,
            tableOutput("top_tools")
          )
        )
      ),

      tabItem( # Usage tab ----
        tabName = "usage",

#TODO this usage plot can probably be calculated from loans data and therefore include date

        fluidRow(
          box( # The bar chart for loans by location
            title = span("Loans by Location for entire data", icon("info-circle") %>% 
                           bs_embed_tooltip("Not adjusted by date",
                                            placement = "bottom")),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotOutput("location_plot")
          )
        )
      ),

      tabItem(
        tabName = "savings",

        fluidRow(
          box(
            title = "Membership Fee?",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            numericInput("num", label = "Membership fee £:", value = 30)
          ),
          box( # display avergae savings
            title = "Average Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            h1(textOutput("avg_savings"))
          ),
          box( # display max savings
            title = "Max Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            h1(textOutput("max_savings") %>% 
                 bs_embed_tooltip("Not including renewals or repeat loans of the same tool ",
                                  placement = "bottom"))
          )
        ),

        fluidRow(
          box(
            title = "Average Savings by Category",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 9,
            plotlyOutput("savings_plot")
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
            title = span("Which User?", icon("info-circle") %>% 
                           bs_embed_tooltip("Top by number of loans",
                                            placement = "bottom")),
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
            title = "Number of Loans",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            h5(textOutput("top_user_loans"))
          ),
          
          box( # display user savings
            title = span("User Savings", icon("info-circle") %>% 
                           bs_embed_tooltip("Not including renewals or repeat loans of the same tool ",
                                            placement = "bottom")),
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
      clean_names() # tidies up column names
  })

  raw_usage <- eventReactive(input$file2, {
    read.csv(input$file2$datapath) %>%
      clean_names() # tidies up column names
  })

  #######################################################
  # Test output----
  raw_loans <- eventReactive(input$test, {
    read.csv("raw_data/loans-export jan-dec 2019.csv") %>%
      clean_names()
  })

  raw_usage <- eventReactive(input$test, {
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
      filter( # filtering to user input date
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
    table <- clean_loans() %>%
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
        "Tool" = item_name,
        "Category" = category,
        "Count" = count
      ) %>%
      slice_max(1)

    # TODO flip this table around
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
        group = category,
        text = sprintf(
          "Month: %s<br>Count: %s<br>Category: %s",
          month,
          count,
          category
        )
      )) +
      geom_line() +
      geom_point() +
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
      scale_color_viridis_d(option = "plasma"),
    tooltip = "text"
    ) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE))
  })

  # Savings df ----
  savings <- reactive({
    clean_loans() %>%
      filter(renewal != "Renewal") %>% 
      drop_na(replacement_cost) %>% 
      group_by(user_id) %>% 
      distinct(item_name, .keep_all=TRUE) %>% 
      summarise(total_savings = sum(replacement_cost)) 
  })

  # Mean savings output ----
  output$avg_savings <- renderText({
    paste0(
      "£",
      round(mean(savings()$total_savings - input$num), digits = 2)
    )
  })


  # Max savings Output ----
  output$max_savings <- renderText({
    paste0(
      "£",
      max(savings()$total_savings - input$num)
    )
  })



  output$savings_plot <- renderPlotly({
    ggplotly(clean_loans() %>%
      filter(
        !(is.na(replacement_cost)),
        !(is.na(renewal))
      ) %>%
      group_by(category) %>%
      summarise(avg_savings = mean(replacement_cost)) %>%
      ggplot(aes(
        x = reorder(category, avg_savings),
        y = avg_savings,
        text = sprintf(
          "Category: %s<br>Average Savings: %s",
          category,
          avg_savings
        )
      )) +
      geom_col(fill = "#2c3c40") +
      labs(
        x = NULL,
        y = "£ Savings"
      ) +
      coord_flip() +
      theme_classic(),
    tooltip = "text"
    ) %>%
      # config(displayModeBar = F) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE))
  })




  # Creates reactive top_user df ----
  user_df <- reactive({
    
    df <- clean_loans() %>%
      filter(renewal != "Renewal") %>%
      group_by(user_id) %>%
      count() %>% 
      ungroup()
    
    if (input$user_choice == "top") {
      # Finding top user
       df %>% 
        slice_max(n, n = 1) %>%
        select(user_id)
    } else {
      # Finding 2nd top user
      df %>% 
        slice_max(n, n = 2) %>%
        arrange(n) %>%
        slice(1) %>%
        select(user_id)
    }
  })


  # User savings ----
  output$top_user_savings <- renderText({
    top_user <- clean_loans() %>%
      inner_join(user_df(), by = "user_id") %>%
      filter(renewal != "Renewal") %>% 
      group_by(user_id) %>% 
      drop_na(replacement_cost) %>% 
      distinct(item_name, .keep_all=TRUE) %>% 
      summarise(total_savings = sum(replacement_cost))
      
    paste0(
      "£",
      sum(top_user$total_savings) - input$num
    )
  })

  output$top_user_loans <- renderText({
    top_user_1 <- clean_loans() %>% 
      inner_join(user_df(), by = "user_id") %>%
      filter(renewal != "Renewal") %>% 
      summarise(total = n())
    
    paste0(
      top_user_1$total)
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
        fill = if (input$cat_or_tool == "category") {
          category
        } else {
          item_name
        },
        text = sprintf(
          "Date: %s<br>Type: %s<br>User ID: %s",
          checked_out,
          if (input$cat_or_tool == "category") {
            category
          } else {
            item_name
          },
          user_id
        )
      )) +
      labs(
        x = "Date Checked Out",
        y = "Count",
        fill = if (input$cat_or_tool == "category") {
          "Category"
        } else {
          "Tool"
        }
      ) +
      geom_histogram(bins = 12) +
      theme_classic() +
      scale_fill_viridis_d(option = "viridis"), tooltip = "text") %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE))
  })


  # Location Plot ----
  output$location_plot <- renderPlot({
    clean_usage() %>%
      filter(!(is.na(home_location))) %>%
      group_by(home_location) %>%
      summarise(total_loans = sum(loans)) %>%
      ggplot(aes(x = home_location, y = total_loans)) +
      geom_col(fill = "#2c3c40") +
      labs(
        x = "Location",
        y = "Total Loans"
      ) +
      theme_classic()
  })
}



# Runs the app
shinyApp(ui, server)
