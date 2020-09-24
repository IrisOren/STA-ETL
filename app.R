
#################################################################
##                          Libraries                          ##
#################################################################


library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(janitor)



## ================================================================
##                  Reading in categories data                  ==
## ================================================================


categories <- read_csv("raw_data/categories copy.csv") %>%
  clean_names()

##################################################################
##                              UI                              ##
##################################################################


ui <- dashboardPage( # Using library(shinydashboard) for layout
  title = "Edinburgh Tool Library",

  skin = "black", # This is just a colour theme for shinyDashboard

  dashboardHeader(
    titleWidth = 250,
    title = tags$a(
      href = "https://edinburghtoollibrary.org.uk/",
      target = "_blank",
      tags$img(tags$img(src = "ETL_logo.png", height = "50px", width = "50px"))
    )
  ), # TPuts the ETL logo in the header and has it link to ETL website

  dashboardSidebar(
    width = 250,

    # Custom CSS to recolour the dashboard using ETL yellow = #fbec3b
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

    # The sidebar menu layout and widgets
    sidebarMenu(
      menuItem("Upload Files", tabName = "uploader", icon = icon("jedi-order")),
      menuItem("Data Viz",
        tabName = "viz", icon = icon("dashboard"),
        dateRangeInput("dates",
          label = h3("Date range")),
        menuSubItem("Loans", tabName = "loans"),
        menuSubItem("Usage", tabName = "usage"),
        menuSubItem("User Stories", tabName = "user_stories")
      )
    )
  ),

  # Sidebar panel for inputs ----
  dashboardBody(
    # Custom CSS to recolour the dashboard using ETL yellow = #fbec3b
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


      tabItem(
        tabName = "loans",
        fluidRow(
          # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
          box(
            title = "Average Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 2,
            h1(textOutput("avg_savings"))
          ),
          box(
            title = "Max Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 2,
            h1(textOutput("max_savings"))
          )
        ),

        fluidRow(
          box(
            title = "Monthly Top Tools",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 4,
            tableOutput("top_tools")
          ),
          box(
            title = "Loans by Category",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 8,
            plotlyOutput("category_plot", height = "500px")
          )
        )
      ),

      tabItem(tabName = "usage",
            
              fluidRow(
                box(
                  title = "Loans by Location for entire data",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  status = "primary",
                  plotOutput("location_plot")))),

      tabItem(
        tabName = "user_stories",
        fluidRow(
          box(
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
          box(
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
            ),
            "Does not interact with graph yet"),
          box(
            title = "User Savings",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            width = 3,
            h3(textOutput("top_user_savings")))),
        
        fluidRow(
          box(width = 12, 
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
      mutate(
        due_date = as.Date(due_date, format = "%d/%m/%Y"),
        checked_out = as.Date(checked_out, format = "%d/%m/%Y"),
        checked_in = as.Date(checked_in, format = "%d/%m/%Y")
      ) %>%
      filter(
        checked_out >= input$dates[1],
        checked_out <= input$dates[2]
      ) %>%
      left_join(categories, by = "item_id") %>%
      mutate(month = month(checked_out, label = T))
  })

  clean_usage <- reactive({
    raw_usage() %>%
      left_join(categories, by = "item_id")
  })


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
        scale_color_viridis_d(option ="plasma"))
  })


  savings <- reactive({
    clean_loans() %>%
      filter(renewal != "Renewal") %>%
      group_by(user_id) %>%
      summarise(total_savings = sum(replacement_cost)) %>%
      drop_na(total_savings)
  })

  output$avg_savings <- renderText({
    paste0(
      "£",
      round(mean(savings()$total_savings), digits = 2)
    )
  })



  output$max_savings <- renderText({
    paste0(
      "£",
      max(savings()$total_savings)
    )
  })


#Finding top user
  top_user_df <- reactive({
    clean_loans() %>%
      group_by(user_id) %>%
      count() %>%
      ungroup() %>%
      slice_max(n, n = 1) %>%
      select(user_id)
  })
  
  # Finding 2nd top user
  top_user_2_df <- reactive({
    clean_loans() %>%
      group_by(user_id) %>%
      count() %>%
      ungroup() %>%
      slice_max(n, n = 2) %>%
      arrange(n) %>% 
      slice(1) %>% 
      select(user_id)
  })

  output$top_user_savings <- renderText({
    
    if (input$user_choice == "top") {
    top_user <- clean_loans() %>%
      inner_join(top_user_df(), by = "user_id") %>%
      filter(renewal != "Renewal") %>%
      drop_na(replacement_cost) %>% 
      summarise(total_savings = sum(replacement_cost))
    }
    else {
      top_user <- clean_loans() %>%
        inner_join(top_user_2_df(), by = "user_id") %>%
        filter(renewal != "Renewal") %>%
        drop_na(replacement_cost) %>% 
        summarise(total_savings = sum(replacement_cost))
    }
      
    
    paste0(
      "£",
      sum(top_user$total_savings)
    )
  })

  
  output$top_user <- renderPlotly({
    if (input$cat_or_tool == "category") {
      return(
        ggplotly(clean_loans() %>%
          inner_join(top_user_df(),
            by = "user_id"
          ) %>%
          filter(renewal != "Renewal") %>%
          group_by(checked_out) %>%
          drop_na(category) %>%
          drop_na(checked_out) %>%
          ggplot(aes(
            x = checked_out,
            fill = category
          )) +
          labs(
            x = "Date Checked Out",
            y = "Count",
            fill = "Category"
          ) +
          geom_histogram(bins = 12) +
          theme_classic() +
          scale_fill_viridis_d(option ="viridis")
        )
      )
    }

    else {
      return(
        ggplotly(clean_loans() %>%
          inner_join(top_user_df(),
            by = "user_id"
          ) %>%
          filter(renewal != "Renewal") %>%
          group_by(checked_out) %>%
          drop_na(item_name) %>%
          drop_na(checked_out) %>%
          ggplot(aes(
            x = checked_out,
            fill = item_name
          )) +
          labs(
            x = "Date Checked Out",
            y = "Count",
            fill = "Tool"
          ) +
          geom_histogram(bins = 12) +
          theme_classic() + 
          scale_fill_viridis_d(option ="viridis")
        )
      )
    }
  })
  
  output$location_plot <- renderPlot({
    clean_usage() %>% 
    filter(!(is.na(home_location))) %>% 
    group_by(home_location) %>% 
    summarise(total_loans = sum(loans)) %>% 
    ggplot(aes(x = home_location, y = total_loans)) +
    geom_col() +
    labs(x = "Location",
         y = "Total Loans") +
    theme_classic()
  })
}



# Run the app ----
shinyApp(ui, server)
