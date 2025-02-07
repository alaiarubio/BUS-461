library(quantmod)
library(TTR)


# Question 1: Write a function get_stock_data() that takes a stock symbol and a date range as input ----
# and returns a data frame containing the stock data for that period. 
# The function should handle potential errors, such as invalid stock symbols.

df <- getSymbols(Symbols ="NKE", 
           env = parent.frame(),
           reload.Symbols = FALSE,
           verbose = FALSE,
           warnings = TRUE, 
           src = "yahoo",
           symbol.lookup = TRUE, 
           auto.assign = getOption('getSymbols.auto.assign',F)) 

 get_stock_data <- function (symbol = NULL, 
                            start_date = NULL, 
                            end_date = NULL) {
  df <- getSymbols(Symbols= symbol, 
                   auto.assign = getOption('getSymbols.auto.assign',F),
                   from=start_date, 
                   to=end_date
)
  names(df) <- c("Open", "High" , "Low" , "Close" , "Volume", "Adjusted")
  return(df)
  
}
 
df <- get_stock_data("TSLA",
                    "2024-01-01",
                    "2025-02-01")


# Question 2:  Write a function plot_stock_prices() that takes a stock data object (as returned by get_stock_data()) ----
# and a price type ("Close", "Adjusted", etc.) as input and creates a base plot of the specified price over time. 
# The function should handle customization of plot appearance (color, title, labels, etc.). 
plot_stock_prices <- function(df = NULL, column_name = NULL) {
  df <- as.data.frame(df)
  plot(x = as.Date(row.names(df)),
       y = df[[column_name]], type = "l",
       xlab = "",
       ylab = column_name)
}
df <- get_stock_data("TSLA",
                     "2024-01-01",
                     "2025-02-01")

plot_stock_prices(df, "Volume")
plot_stock_prices(df, "Open")

# Question 3: Fetch daily price data for one single stock of your choice for the past year. ----
# Hint: Reuse the function previously created 
# Create a multi-panel plot showing the "Open", "High", "Low", and "Close" prices in separate panels. 
# Use different colors for each price type and add appropriate titles and labels.

my_fav_stock <- get_stock_data("TSLA",
                               "2024-01-01",
                               "2025-02-01")

par(mfrow = c(2 ,2))
plot_stock_prices(my_fav_stock, "Open")
plot_stock_prices(my_fav_stock, "High")
plot_stock_prices(my_fav_stock, "Close")
plot_stock_prices(my_fav_stock, "Adjsuted")

# Question 4: Fetch weekly price data for a good stock, say Microsoft (MSFT) for the past two years. ----
# Note: By default daily data is returned. Use to.weekly()
# Create a single plot (with two panes) showing the "Adjusted" price as a line on one side, and the "Volume" on the other side. 

my_fav_stock_weekly <- to.weekly(my_fav_stock)
names(my_fav_stock_weekly) <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
par(mfrow = c(1,2))
plot_stock_prices(my_fav_stock_weekly, "Adjsuted")
plot_stock_prices(my_fav_stock_weekly, "Volume")


# Question 5: Implement a Golden Cross trading strategy for a chosen stock (e.g., Apple - AAPL) over the past 5 years. ----
# Fetch the daily stock price data using quantmod.
# Calculate the 50-day and 200-day moving averages.
# Identify the Golden Cross points (where the 50-day MA crosses above the 200-day MA). 
# Plot 50-day and 200-day lines in different colors.
# Generate a plot of the stock prices along with the moving averages.
my_fav_stock$SMA50 <- 0
my_fav_stock$SMA50 <- SMA(my_fav_stock$Close, 50)
my_fav_stock$SMA200 <- SMA(my_fav_stock$Close, 200)
par(mfrow=c (1,1))
plot(my_fav_stock$Close)
lines(my_fav_stock$SMA50, col="blue")
lines(my_fav_stock$SMA200, col="red")


