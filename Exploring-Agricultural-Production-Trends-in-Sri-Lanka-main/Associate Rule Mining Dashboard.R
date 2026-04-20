library(shiny) 
library(dplyr) 
library(arules) 
library(arulesViz) 
library(DT) 
library(plotly) 

ui <- fluidPage( 
  titlePanel("FAOSTAT Association Rule Mining Dashboard"), 
  sidebarLayout( 
    sidebarPanel( 
      fileInput("file", "Upload FAOSTAT CSV File", accept = ".csv"), 
      sliderInput("support", "Minimum Support:", min = 0.001, max = 0.2, value = 0.01), 
      sliderInput("confidence", "Minimum Confidence:", min = 0.1, max = 1, value = 0.5), 
      sliderInput("lift", "Minimum Lift:", min = 1, max = 5, value = 1) 
    ), 
    mainPanel( 
      tabsetPanel( 
        tabPanel("Rules Table", DTOutput("rules_table")), 
        tabPanel("Grouped Plot", plotOutput("grouped_plot")), 
        tabPanel("Graph Plot", plotOutput("graph_plot")), 
        tabPanel("Transactions Sample", DTOutput("sample_trans")), 
        tabPanel("Plotly Scatter", plotlyOutput("plotly_scatter")), 
        tabPanel("Plotly Parallel Coordinates", plotlyOutput("plotly_parallel")) 
      ) 
    ) 
  ) 
) 

server <- function(input, output, session) { 
  
  data_input <- reactive({ 
    req(input$file) 
    
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE) 
    
    required_cols <- c("Area", "Year", "Element", "Item", "Flag.Description") 
    if (!all(required_cols %in% colnames(df))) { 
      showNotification("Error: Required columns missing.", type = "error") 
      return(NULL) 
    } 
    
    df_clean <- df %>% 
      filter(Element == "Production") %>% 
      filter(Flag.Description != "Missing value (data cannot exist, not applicable)") 
    
    transactions <- df_clean %>% 
      group_by(Area, Year) %>% 
      summarise(Items = paste(unique(Item), collapse = ","), .groups = "drop") %>% 
      select(Items) 
    
    if (nrow(transactions) == 0) { 
      showNotification("No transactions found.", type = "error") 
      return(NULL) 
    } 
    
    tmpfile <- tempfile(fileext = ".csv") 
    write.csv(transactions, tmpfile, row.names = FALSE, quote = FALSE) 
    
    trans_data <- read.transactions(tmpfile, format = "basket", sep = ",") 
    print(summary(trans_data))  # Debugging 
    
    trans_data 
  }) 
  
  rules_sorted <- reactive({ 
    trans_data <- data_input() 
    req(trans_data) 
    
    rules <- apriori(trans_data, parameter = list( 
      support = input$support, 
      confidence = input$confidence, 
      minlen = 2, 
      maxlen = 3 
    )) 
    
    if (length(rules) == 0) { 
      showNotification("No rules generated. Try lower support/confidence.", type = 
                         "warning") 
      return(NULL) 
    } 
    
    filtered <- subset(rules, lift > input$lift) 
    if (length(filtered) == 0) { 
      showNotification("No rules remain after filtering by lift.", type = "warning") 
      return(NULL) 
    } 
    
    sort(filtered, by = c("confidence", "lift"), decreasing = TRUE) 
  }) 
  
  output$rules_table <- DT::renderDataTable({ 
    rules <- rules_sorted() 
    if (is.null(rules)) { 
      return(DT::datatable(data.frame(Message = "No rules found."), rownames = FALSE)) 
    } 
    DT::datatable( 
      as(rules, "data.frame"), 
      options = list(pageLength = 10, autoWidth = TRUE), 
      caption = "Association Rules Table" 
    ) 
  }) 
  
  output$grouped_plot <- renderPlot({ 
    rules <- rules_sorted() 
    req(rules) 
    plot(rules, method = "grouped", control = list(k = 20)) 
  }) 
  
  output$graph_plot <- renderPlot({ 
    rules <- rules_sorted() 
    req(rules) 
    plot(rules, method = "graph", control = list(type = "items", max = 100)) 
  }) 
  
  output$sample_trans <- DT::renderDataTable({ 
    trans_data <- data_input() 
    req(trans_data) 
    
    sample_trans <- as(trans_data, "list")[1:min(10, length(trans_data))] 
    df <- data.frame( 
      TransactionID = seq_along(sample_trans), 
      Items = sapply(sample_trans, paste, collapse = ", ") 
    ) 
    
    DT::datatable(df, options = list(pageLength = 5), caption = "Sample Transactions") 
  }) 
  
  output$plotly_scatter <- renderPlotly({ 
    rules <- rules_sorted() 
    req(rules) 
    
    rules_df <- as(rules, "data.frame") 
    
    plot_ly( 
      data = rules_df, 
      x = ~support, 
      y = ~confidence, 
      text = ~paste("Rule:", rules_df$rules, "<br>Lift:", round(lift, 2)), 
      type = 'scatter', 
      mode = 'markers', 
      marker = list(size = ~lift * 5, color = ~lift, colorscale = "Viridis", showscale = TRUE) 
    ) %>% 
      layout( 
        title = "Support vs Confidence (Bubble size = Lift)", 
        xaxis = list(title = "Support"), 
        yaxis = list(title = "Confidence") 
      ) 
  }) 
  
  output$plotly_parallel <- renderPlotly({ 
    rules <- rules_sorted() 
    req(rules) 
    
    rules_df <- as(rules, "data.frame") 
    
    plot_ly( 
      type = 'parcoords', 
      line = list(color = ~rules_df$lift, colorscale = 'Viridis', showscale = TRUE), 
      dimensions = list( 
        list(label = 'Support', values = ~rules_df$support), 
        list(label = 'Confidence', values = ~rules_df$confidence), 
        list(label = 'Lift', values = ~rules_df$lift) 
      ) 
    ) %>% 
      layout(title = "Parallel Coordinates: Rule Metrics") 
  }) 
} 

shinyApp(ui, server) 