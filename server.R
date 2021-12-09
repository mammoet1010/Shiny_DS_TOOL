library(shiny)
library(MASS)
library(kernlab)
library(shinydashboard)
library(DT)
data(spam)

server <- function(input, output, session){
  
  ### 可視化----
  output$distPlot_shiny <- renderPlot({
    x <- faithful[,2]
    bins <- seq(min(x), max(x), length.out = input$bin_shiny + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
    
  })
  
  data_for_plot <- reactive({

    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
    
    updateSelectInput(session, "select_input_data_for_hist", choices = colnames(df))
    
    updateSelectInput(session, "input_data_for_scatter_plotX",
                      choices = colnames(df))
    
    updateSelectInput(session, "input_data_for_scatter_plotY",
                      choices = colnames(df))
    
    return(df)
    
  })
  
  output$table_for_plot = DT::renderDataTable({
    req(input$file1)
    
    return(DT::datatable(data_for_plot(),  options = list(pageLength = 10, scrollX = TRUE), filter = c("top")))
    
  })
  
  
  output$histgram <- renderPlot({
    tmpData <- data_for_plot()[, input$select_input_data_for_hist]
    x <- na.omit(tmpData)
    bins <- seq(min(x), max(x), length.out = input$slider_input_data + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
  
  output$scatter_plot <- renderPlot({
    input$trigger_scatter_plot
    plot(isolate(data_for_plot()[, c(input$input_data_for_scatter_plotX,
                                     input$input_data_for_scatter_plotY)]))
  })
  
  output$plot_brushedPoints <- DT::renderDataTable({
    res <- brushedPoints(data_for_plot(),
                         input$plot_brush,
                         xvar = input$input_data_for_scatter_plotX,
                         yvar = input$input_data_for_scatter_plotY)
    
    if (nrow(res) == 0)
      return()
    res
  })

## 回帰----
  data_for_regression <- reactive({

    req(input$file2)
    df <- read.csv(input$file2$datapath, stringsAsFactors = TRUE)

    updateSelectInput(session, "data_for_regressionY", choices = colnames(df))

    return(df)

  })

  output$data_table_for_regression <- DT::renderDataTable({
    req(input$file2)
    
    return(DT::datatable(t(data_for_regression()[1:10, ]), 
                         selection = list(target = "row"),  
                         options = list(pageLength = 10, scrollX = TRUE)))
    
  })
  
  output$rows_selected <- renderPrint(
    input$data_table_for_regression_rows_selected
  )


  
  
  
  
}
