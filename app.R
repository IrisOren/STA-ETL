library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(lubridate)
library(janitor)

categories <- read_csv("raw_data/categories copy.csv") %>%
    clean_names()


# Define UI for data upload app ----
ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "Edinburgh Tool Library"),
    dashboardSidebar(
        
                                # This code is for custome skin colour
                                # tags$head(tags$style(HTML('.logo {
                                #                       background-color: #fbec3b !important;
                                #                       }
                                #                       .navbar {
                                #                       background-color: #fbec3b !important;
                                #                       }
                                #                       '))),
                                
        # The dynamically-generated user panel
        uiOutput("userpanel"),
        sidebarMenu(
            menuItem("Upload Files", tabName = "uploader", icon = icon("jedi-order")),
            menuItem("Data Viz", tabName = "viz", icon = icon("dashboard"))
        )
    ),
        
        # Sidebar panel for inputs ----
    dashboardBody(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
        
        
            
            # Output: Data file ----
            tableOutput("contents")
            
        )
        
    )


# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath) 
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    
    observeEvent(input$jumpToP2, {
        updateTabsetPanel(session, "inTabset",
                          selected = "panel2")
    })
    
    observeEvent(input$jumpToP1, {
        updateTabsetPanel(session, "inTabset",
                          selected = "panel1")
    })
    
    
    # duf %>% 
    #     clean_names() %>% 
    #     mutate(due_date = as.Date(due_date, format = "%d/%m/%Y"),
    #            checked_out = as.POSIXct(checked_out, format = "%d/%m/%Y %H:%M"),
    #            checked_in = as.POSIXct(checked_in, format = "%d/%m/%Y %H:%M")
    #     ) %>% 
    #     left_join(categories, by = "item_id") %>% 
    #     mutate(month = month(checked_out, label = T))
    
}
# Run the app ----
shinyApp(ui, server)