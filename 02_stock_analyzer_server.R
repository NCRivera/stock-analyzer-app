# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - SERVER -----
# Version 1

# APPLICATION DESCRIPTION ----
# - Connect the stock dropdown, interactive plot and commentary using shiny server operations


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyquant)
library(tidyverse)

source(file = "00_scripts/stock_analysis_functions.R")

stock_list_tbl <- get_stock_list("SP500")

# stock_data_tbl <- get_stock_data("AAPL", start = "2018-01-01", end = "2019-01-01")

# UI ----
ui <- fluidPage(
    title = "Stock Analyzer",
    
    # 1.0 HEADER ----
    div(
        h1("Stock Analyzer", "by Business Science"),
        p("This is the first mini-project completed in our", "Expert Shiny Applications Course (DS4B 202-R)")
    ),
    
    # 2.0 APPLICATION UI -----
    div(
        column(
            width = 4, 
            wellPanel(
                pickerInput(
                    inputId = "stock_selection", 
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
                actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")), 
                sliderInput(inputId = "short_avg", label = "Short Moving Average", min = 5, max = 40, value = 20), 
                sliderInput(inputId = "long_avg", label = "Long Moving Average", min = 50, max = 120, value = 50)
            )
        ),
        column(
            width = 8, 
            div(
                div(h4(textOutput("plot_header"))),
                div(
                    plotlyOutput("plotly_plot")
                )
            )
        )
    ),
    
    # 3.0 ANALYST COMMENTARY ----
    div(
        column(
            width = 12,
            div(
                div(h4("Analyst Commentary")),
                div(
                    textOutput("analyst_commentary")
                    # stock_data_tbl %>% generate_commentary(user_input = "Placeholder")
                )
            )
        )
    )
)

# SERVER ----
server <- function(input, output, session) {
    
    # Stock Symbol ---- 
    stock_symbol <- eventReactive(input$analyze, {
        get_symbol_from_user_input(input$stock_selection)
    }, ignoreNULL = TRUE)
    
    plot_header <- eventReactive(input$analyze, {
        input$stock_selection
        # stock_symbol()
    }, ignoreNULL = TRUE)

    short <- eventReactive(input$short_avg, {
        input$short_avg
    })

    long <- eventReactive(input$long_avg, {
        input$long_avg
    })
    
    output$plot_header <- renderText({
        plot_header()
    })
    
    # Get Stock Data ----
    stock_data_tbl <- reactive({
        stock_symbol() %>% 
            get_stock_data(
                start = today() - days(180), 
                end = today(), 
                # mavg_short = 20, 
                mavg_short = short(), 
                # mavg_long = 50
                mavg_long = long()
            )
    })
    
    # Plotly Plot
    output$plotly_plot <- renderPlotly({
        stock_data_tbl() %>%
            plot_stock_data()
    })
    
    output$analyst_commentary <- renderText({
        generate_commentary(data = stock_data_tbl(), user_input = plot_header())
    })
}

# RUN APP ----
shinyApp(ui = ui, server = server)