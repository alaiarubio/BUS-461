

# Define UI for application that draws a histogram
ui1 <- fluidPage(

    # Application title
    titlePanel("05_InClass_Activity - shiny"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "datasetName",
          label = "Please select a dataset from the dropdown",
          choices = c("AirPassengers", "cars", "iris"),
          selected = "AirPassengers")
           
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
          #dataTableOutput()
          #tableOutput("summary1")
          verbatimTextOutput("summary1")
        )
    )
)


server1 <- function(input, output) {

    
    output$summary1 <- renderPrint({
      print(input$datasetName)
      summary(get(input$datasetName))
    })
    
    
}

# Define UI for application that draws a histogram
ui2 <- fluidPage(
  titlePanel("Old Faithful Geyser Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "bins",
                  label = "select the # of bins you want!",
                  choices = c(10,20,30,40,50),
                  selected = 30)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server2 <- function(input, output) {
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = as.integer(input$bins) + 1)
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

ui3 <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "cutType",
                  label = "select a cut from dropdown!",
                  choices = c("Ideal", "Good","Very Good", "Premium"),
                  selected = "Ideal"),
      sliderInput("caratId", 
                  "Select the range of carat",
                  min = 0,
                  max = 10,
                  value = c(1,3)
                  )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2")
    )
  )
)

# Define server logic required to draw a histogram
server3 <- function(input, output) {
  
  output$plot1 <- renderPlot({
    print(input$caratId[1])
    diamonds %>%  # magrittr
      filter(carat >= input$caratId[1],
             carat <= input$caratId[2],
             cut == input$cutType) %>%
      ggplot() +
      geom_histogram(aes(carat))
    
  })
  output$plot2 <- renderPlot({
    print(input$caratId[1])
    diamonds %>%  # magrittr
      filter(carat >= input$caratId[1],
             carat <= input$caratId[2],
             cut == input$cutType) %>%
      ggplot() +
      geom_point(aes(carat, price, color = color))
  }) 
  
}


ui4 <- fluidPage(
  titlePanel("Stock RSI - Varying the average days for computing RSI"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("rsi_days", "Number of days to consider:", min = 1, max = 20, value = 14),
      textInput("stock_symbol", "Enter a stock symbol:", value = "AAPL"),
      actionButton("update", "Update Data")
    ),
    mainPanel(
      plotOutput("rsiPlot")
    )
  )
)

server4 <- function(input, output) {
  stockData <- eventReactive(input$update, {
    stock <- getSymbols(input$stock_symbol, auto.assign = FALSE, from = "2022-08-01", to = "2024-02-01")
    stock_df <- data.frame(Date = index(stock), coredata(stock))
    colnames(stock_df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    
  
    stock_df$RSI <- RSI(stock_df$Close, n = input$rsi_days)
    stock_df <- na.omit(stock_df)
    
    return(stock_df)
  })
  
  output$rsiPlot <- renderPlot({
    req(stockData())  
    
    ggplot(stockData(), aes(x = Date, y = RSI)) +
      geom_line(color = "black") +
      labs(title = paste(input$stock_symbol, "RSI"), y = "RSI", x = "Date")
      
  })
}


ui5 <- fluidPage(
  titlePanel("Stock Analysis: AAPL"),
  mainPanel(
    plotOutput("stockPlot")
  )
)

server5 <- function(input, output) {
  stockData <- reactive({
    stock <- getSymbols("AAPL", auto.assign = FALSE, from = "2020-01-01")
    names(stock) <- c("Open", "High", "Low", "c", "Vol", "Adj")
    stock$RSI <- RSI(stock$c)
    stock$MA50 <- SMA(stock$c, 50)
    stock$MA200 <- SMA(stock$c, 200)
    stock$InvestPoint <- ifelse((stock$MA50 > stock$MA200) & (lag(stock$MA50) < lag(stock$MA200)),
                                stock$Open, 
                                0)
    na.omit(stock)
  })
  
  output$stockPlot <- renderPlot({
    stock_df <- data.frame(date = index(stockData()), coredata(stockData()))
    stock_crossover <- stock_df[stock_df$InvestPoint > 0, ]
    ggplot(stock_df, aes(x = date)) + 
      geom_line(aes(y = c), col = "blue") +
      geom_line(aes(y = MA50), col= "red") +
      geom_line(aes(y = MA200), col= "grey") +
      geom_text(data = stock_crossover, aes(x= date, y= MA50, label = round(Open, 0)))
  })
}



# Run the application 
shinyApp(ui = ui3, server = server3)






