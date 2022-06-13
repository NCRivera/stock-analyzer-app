# BUSINESS SCIENCE ----
# DS4B 202-R ----
# STOCK ANALYZER APP - DATA ANALYSIS -----
# Version 1

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - The functionality is designed to pull the past 180 days of stock data
# - We will implement 2 moving averages - short (fast) and long (slow)
# - We will produce a timeseries visualization
# - We will produce automated commentary based on the moving averages

# REPRODUCIBILITY REQUIREMENTS
# - The functionality is designed to pull the past 180 days of stock data from today's date
# - Because of this, your analysis may differ from mine
# - To reproduce my analysis, replace today() with ymd("2019-08-20")

# LIBRARIES ----
library(plotly)
library(tidyquant)
library(tidyverse)
library(fs)



# 1.0 GET STOCK LIST ----
# stock_list_tbl <- tq_index("SP500") %>% 
#     select(symbol, company) %>% 
#     arrange(symbol) %>% 
#     mutate(label = str_c(symbol, company, sep = ", ")) %>% 
#     select(label)

get_stock_list <- function(stock_index = "SP500") {
    stock_list_tbl <- tq_index(x = stock_index) %>% 
        select(symbol, company) %>% 
        arrange(symbol) %>% 
        mutate(label = str_c(symbol, company, sep = ", ")) %>% 
        select(label)
    stock_list_tbl
}

# tq_index_options()
# get_stock_list()
# get_stock_list("DOW")
# get_stock_list("DOWGLOBAL")


# 2.0 EXTRACT SYMBOL BASED ON USER INPUT ----

# user_input <- "AAPL, Apple Inc"
# user_input %>% str_split(pattern = ", ") %>% pluck(1, 1)


get_symbol_from_user_input <- function(user_input){
    user_input %>% str_split(pattern = ", ") %>% pluck(1, 1)
}

# "A, Agilent Technologies Inc." %>% get_symbol_from_user_input()

# 3.0 GET STOCK DATA ----

start <- today() - days(180)
end <- today()

"AAPL" %>% 
    tq_get(get = "stock.prices", from = start, to = end) %>% 
    select(date, adjusted) %>% 
    mutate(
        mavg_short = rollmean(adjusted, k = 20, na.pad = TRUE, align = "right"), 
        mavg_long  = rollmean(adjusted, k = 50, na.pad = TRUE, align = "right")
    )

get_stock_data <- function(
        stock_symbol, 
        start = today() - days(180), 
        end   = today(), 
        mavg_short = 20, 
        mavg_long  = 50
        ) {
    stock_data <- stock_symbol %>% 
        tq_get(get = "stock.prices", from = start, to = end) %>% 
        select(date, adjusted) %>% 
        mutate(
            mavg_short = rollmean(adjusted, k = mavg_short, na.pad = TRUE, align = "right"), 
            mavg_long  = rollmean(adjusted, k = mavg_long,  na.pad = TRUE, align = "right")
        )
    stock_data
}

stock_data_tbl <- get_stock_data("AAPL", start = "2018-01-01", end = "2018-06-30", mavg_short = 5, mavg_long = 20)

# 4.0 PLOT STOCK DATA ----

g <- stock_data_tbl %>% 
    pivot_longer(cols = adjusted:mavg_long, names_to = "legend", values_to = "value") %>% 
    
    ggplot(mapping = aes(x = date, y = value, color = legend, group = legend)) + 
    geom_line(mapping = aes(linetype = legend)) + 
    theme_tq() + 
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
    scale_color_tq() + 
    labs(
        x = "", 
        y = "Adjusted Share Price"
    )

ggplotly(g)


plot_stock_data <- function(data) {
    # 
    g <- data %>% 
        pivot_longer(cols = adjusted:mavg_long, names_to = "legend", values_to = "value",  names_ptypes = list(legend = factor())) %>% 
        
        ggplot(mapping = aes(x = date, y = value, color = legend, group = legend)) + 
        geom_line(mapping = aes(linetype = legend)) + 
        theme_tq() + 
        scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10)) +
        scale_color_tq() + 
        labs(
            x = "", 
            y = "Adjusted Share Price"
        )
    
    ggplotly(g)
}

plot_stock_data(get_stock_data("AAPL"))

# 5.0 GENERATE COMMENTARY ----
warning_signal <- stock_data_tbl %>% 
    tail(1) %>% 
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>% 
    pull(mavg_warning_flag)

n_short <- stock_data_tbl %>% pull(mavg_short) %>% is.na() %>% sum() + 1
n_long <- stock_data_tbl %>% pull(mavg_long) %>% is.na() %>% sum() + 1


if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
} else {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
}


generate_commentary <- function(data, user_input){
    warning_signal <- data %>% 
        tail(1) %>% 
        mutate(mavg_warning_flag = mavg_short < mavg_long) %>% 
        pull(mavg_warning_flag)
    
    n_short <- data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
    n_long <- data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
    
    if (warning_signal) {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends.")
    } else {
        str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends.")
    }
}

generate_commentary(stock_data_tbl, "AAPL, Apple Inc")

# 6.0 TEST WORKFLOW ----

test <- get_stock_list() %>% 
    slice(3) %>% 
    pull() %>% 
    get_symbol_from_user_input() %>% 
    get_stock_data(star = "2018-01-01")

test %>% 
    plot_stock_data()

test %>% 
    generate_commentary(user_input = "AAP")
    

# 7.0 SAVE SCRIPTS ----
dir_create(path = "00_scripts")

dump(
    list = c("get_stock_list", "get_symbol_from_user_input", "get_stock_data", "plot_stock_data", "generate_commentary"), 
    file = "00_scripts/stock_analysis_functions.R", 
    append = FALSE
)
