library(shiny)
library(MASS)
library(kernlab)
library(shinydashboard)
library(DT)


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
  
  data_train_and_test <- reactiveValues()
  
  regression_summary <- reactive({
    input$regression_button
    
    y <- data_for_regression()[, isolate(input$data_for_regressionY)]
    x <- data_for_regression()[, isolate(input$data_table_for_regression_rows_selected)]
    
    tmp_data <- cbind(na.omit(x), na.omit(y))
    colnames(tmp_data) <- c(colnames(x), "dependent_variable")
    train_index <- createDataPartition(tmp_data$"dependent_variable", p = .7,
                                       list = F,
                                       times = 1)
    data_train_and_test$train <- tmp_data[train_index,]
    data_train_and_test$test <- tmp_data[-train_index,]
    
    return(train(dependent_variable ~.,
                 data = data_train_and_test$train,
                 method = isolate(input$regression_type),
                 tuneLength = 4,
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv"),
                 linout = T))
    
  })
  
  output$summary_regression <- renderPrint({
    predict_result_residual <- predict(regression_summary(), data_train_and_test$test) - data_train_and_test$test$"dependent_variable"
    cat("MSE(平均二乗誤差")
    print(sqrt(sum(predict_result_residual ^ 2)/nrow(data_train_and_test$test)))
    summary(regression_summary())
  })

  output$plot_regression <- renderPlot({
    plot(predict(regression_summary(), data_train_and_test$test),
         data_train_and_test$test$"dependent_variable",
         xlab="prediction", ylab="real")
    abline(a=0, b=1, col="red", lwd=2)
  })
  
  ## 分類 ----
  data_for_classification <- reactive({

    req(input$file3)
    df <- read.csv(input$file3$datapath, stringsAsFactors = TRUE)

    updateSelectInput(session, "data_for_classificationY", choices = colnames(df))

    return(df)

  })

  output$data_table_for_classification <- DT::renderDataTable({
    req(input$file3)

    return(DT::datatable(t(data_for_classification()[1:10, ]),
                         selection = list(target = "row"),
                         options = list(pageLength = 10, scrollX = TRUE)))

  })
  
  output$rows_selected_classification <- renderPrint(
    input$data_table_for_classification_rows_selected
  )
  
  data_train_and_test_classification <- reactiveValues()
  
  classification_summary <- reactive({
    input$classification_button
    
    y <- data_for_classification()[, isolate(input$data_for_classificationY)]
    x <- data_for_classification()[, isolate(input$data_table_for_classification_rows_selected)]#
    
    tmp_data <- cbind(na.omit(x), na.omit(y))
    colnames(tmp_data) <- c(colnames(x), "dependent_variable")
    train_index <- createDataPartition(tmp_data$"dependent_variable", p = .7,
                                       list = F,
                                       times = 1)
    data_train_and_test_classification$train <- tmp_data[train_index,]
    data_train_and_test_classification$test <- tmp_data[-train_index,]
    
    return(train(dependent_variable ~.,
                 data = data_train_and_test_classification$train,
                 method = isolate(input$classification_type),
                 tuneLength = 4,
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv"),
                 linout = T))
    
  })
  
  output$summary_classification <- renderPrint({
    cat("Summary")
    print(confusionMatrix(data = predict(classification_summary(),
                                         data_train_and_test_classification$test),
                          data_train_and_test_classification$test$"dependent_variable"))
    summary(classification_summary())
  })
  
  #
  data_for_clustering <- reactive({
    
    req(input$file4)
    df <- read.csv(input$file4$datapath, stringsAsFactors = TRUE)
    return(na.omit(df))
    
  })
  
  output$data_table_for_clustering <- DT::renderDataTable(
    t(data_for_clustering()[1:10, ]), selection = list(target = "row")
  )
  
  output$rows_selected_clustering <- renderPrint(
    input$data_table_for_clustering_rows_selected
  )
  
  clustering_summary <- reactive({
    input$clustering_button
    
    clusters <- kmeans(isolate(data_for_clustering()[,
                                                     input$data_table_for_clustering_rows_selected]),
                       centers = isolate(input$cluster_number))
    
    return(clusters$cluster)
  })
  
  output$data_with_clustering_result <- DT::renderDataTable({
    cbind(clustering_summary(), data_for_clustering())
  })

}

























