# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - LAYOUT -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyquant)
library(tidyverse)

source(file = "00_scripts/stock_analysis_functions.R")
stock_list_tbl <- get_stock_list()

# UI V1 
# ui <- fluidPage(
#     # FRONTEND
#     title = "Stock Analyzer", 
#     titlePanel("Stock Analyzer by Nicholas C Rivera"), 
#     p("This is the first mini-project completed in our Expert Shiny Applications Course (DS4B 202-R)"), 
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(inputId = "user_pick", label = "Stock List (Pick One to Analyze)", choices = stock_list), 
#             actionButton(inputId = "analyze", label = "Analyze", icon = icon("download"))
#         ), 
#         mainPanel(
#             plotlyOutput("mavg_plot")
#         )
#     ), 
#     fluidRow(
#         h4("Analyst Commentary"), 
#         # p("Output here")
#         textOutput(outputId = "comments")
#     )
# )

ui <- fluidPage(
    title = "Stock Analyzer", 
    
    # 1.0 HEADER ----
    div(
        h1("Stock Analyzer", "by Nich Rivera"), 
        p("This is the first mini-project completed in our Expert Shiny Applications Course (DS4B 202-R)")
    ), 
    
    # 2.0 APPLICATION UI ----
    div(
        column(
            width = 4, 
            wellPanel(
                pickerInput(
                    inputId = "user_pick", 
                    label   = "Stock List (Pick One to Analyze)", 
                    choices = stock_list_tbl$label, 
                    multiple = FALSE, 
                    selected = stock_list_tbl %>% filter(label %>% str_detect("AAPL")) %>% pull(label), 
                    options = pickerOptions(
                        actionsBox = FALSE, 
                        liveSearch = TRUE, 
                        size = 10
                    )
                ), 
                actionButton(inputId = "analyze", label = "Analyze", icon = icon("download"))
            )
        ), 
        column(
            width = 8, 
            div(
                div(h4("PLACEHOLDER")), 
                div(
                    plotlyOutput("mavg_plot")
                )
            )
        )
    ),
    # 3.0 ANALYST COMMENTARY ----
    div(
        column(width = 12, 
            div(
                div(h4("Analyst Commentary")), 
                div(textOutput(outputId = "comments"))
            )
        )
    )
)

# SERVER ----
server <- function(input, output, session){
    
}

# SERVER V1 ----
# server <- function(input, output, session) {
#     
#     # BACKEND
#     
#     stock_choice <- eventReactive(input$analyze, {input$user_pick})
#     data <- reactive({
#         get_symbol_from_user_input(user_input = stock_choice()) %>% 
#             get_stock_data(stock_symbol = .)
#     })
#     
#     output$mavg_plot <- renderPlotly({
#         data() %>% plot_stock_data(data = .)
#     })
#     
#     output$comments <- renderText({
#         data() %>% 
#             generate_commentary(data = ., user_input = stock_choice())
#     })
# }

# RUN APP ----
shinyApp(ui, server)