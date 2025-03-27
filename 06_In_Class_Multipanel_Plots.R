library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)


stock_df <- read.csv("AAPL_stock_data.csv")


stock_df <- stock_df %>%
  mutate(
    SMA50 = zoo::rollmean(AAPL.Close, 50, fill = NA, align = "right"),
    SMA200 = zoo::rollmean(AAPL.Close, 200, fill = NA, align = "right")
  )


crossover_points <- stock_df %>%
  mutate(
    Prev_SMA50 = lag(SMA50),
    Prev_SMA200 = lag(SMA200),
    Signal = case_when(
      Prev_SMA50 < Prev_SMA200 & SMA50 >= SMA200 ~ "Bullish",
      Prev_SMA50 > Prev_SMA200 & SMA50 <= SMA200 ~ "Bearish",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Signal))

# Shiny UI
ui <- fluidPage(
  titlePanel("AAPL Stock Analysis"),
  
  tabsetPanel(
    tabPanel("SMA Analysis",
      plotOutput("sma_plot")
    ),
    tabPanel("Crossover Analysis",
      plotOutput("crossover_plot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  
  # SMA Plot
  output$sma_plot <- renderPlot({
    ggplot(stock_df, aes(x = Date, y = AAPL.Close)) +
      geom_line(color = "blue") +
      geom_line(aes(y = SMA50), color = "red") +
      geom_line(aes(y = SMA200), color = "gray") +
      geom_text(data = crossover_points, aes(label = round(AAPL.Close, 2)), 
                vjust = -0.5, size = 3.5) + 
      labs(title = "AAPL Stock Price with SMAs and Crossover Points", x = "Date", y = "Price") +
      theme_minimal()
  })
  
  output$crossover_plot <- renderPlot({
    ggplot(stock_df, aes(x = Date, y = AAPL.Close)) +
      geom_line(color = "blue") +
      geom_text(data = crossover_points %>% filter(Signal == "Bullish"), 
                aes(label = round(AAPL.Close, 2)), color = "green", vjust = -0.5, size = 4) +  
      geom_text(data = crossover_points %>% filter(Signal == "Bearish"), 
                aes(label = round(AAPL.Close, 2)), color = "red", vjust = 1.5, size = 4) +  
      labs(title = "AAPL Close Price and Crossover Signals", x = "Date", y = "Price") +
      theme_minimal()
  })
}

# Run the Shiny App
shinyApp(ui, server)




