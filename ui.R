library(shiny)
library(shinydashboard)
library(DT)
# UI ----
  ## サイドバー ----
ui <- dashboardPage(
  dashboardHeader(title = "DS_TOOL"),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("可視化", 
               tabName = "tab_A", icon = icon("dashboard")),
      menuItem("教師あり_回帰", 
               tabName = "tab_B", icon = icon("line-chart")),
      menuItem("教師あり_分類", 
               tabName = "tab_C", icon = icon("th")),
      menuItem("教師なし", 
               tabName = "tab_D", icon = icon("search"))
    )
  ),
  
  ## メインパネル ----
  dashboardBody(
    tabItems(
      ### 可視化タブ----
      tabItem(tabName = "tab_A",
              sidebarLayout(
                sidebarPanel(
                  
                  #### Input: Select a file ----
                  h3("データセットの読込み"),
                  fileInput("file1", "upload csv file here",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  
                  h3("ヒストグラム"),
                  selectInput("select_input_data_for_hist",
                              "ヒストグラムで表示する変数を選択",
                              choices = colnames(df)),

                  sliderInput("slider_input_data",
                              "Number of bins:",
                              min = 1,
                              max = 50,
                              value = 30),

                  h3("散布図"),
                  selectInput("input_data_for_scatter_plotX",
                              "x軸",
                              choices = colnames(df)),
                  
                  selectInput("input_data_for_scatter_plotY",
                              "y軸",
                              choices = colnames(df)),
                  actionButton("trigger_scatter_plot", "出力")

                ),
                
              mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Table", DT::dataTableOutput("table_for_plot")),
                            
                            tabPanel("ヒストグラム", plotOutput("histgram")),
                            
                            tabPanel("散布図", plotOutput("scatter_plot",
                                                          brush = brushOpts(id = "plot_brush")),
                                     DT::dataTableOutput("plot_brushedPoints")),

                            tabPanel("相関分析")
                            )
                  )
                )
              ), 
  ## 回帰タブ ----      
      tabItem(tabName = "tab_B",
              sidebarLayout(
                sidebarPanel(

                  ### Input: Select a file ----
                  h3("データセットの読込み"),
                  fileInput("file2", "upload csv file here",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),

                  h3("回帰を出力"),
                  selectInput("data_for_regressionY",
                              "目的変数を選択",
                              choices = colnames(df), selected = colnames(df)[1]),

                  h3("選択済みの説明変数"),
                  verbatimTextOutput("rows_selected"),
                  
                  selectInput("regression_type",
                              "回帰手法を選択",
                              choices = c("重回帰" = "lm",
                                          "ランダムフォレスト" = "rf",
                                          "3層ニューラルネット" = "nnet")),
                  
                  actionButton("regression_button", "実行")),

                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Table",
                                       h3("説明変数を選択"),
                                       DT::dataTableOutput("data_table_for_regression")),

                              tabPanel("回帰結果", verbatimTextOutput("summary_regression")),

                              tabPanel("Y-Y plot", plotOutput("plot_regression"))
                            )
                          )
                        )
                ),
  ## 分類タブ ----
        tabItem(tabName = "tab_C",
                sidebarLayout(
                  sidebarPanel(
                    
                    ### Input: Select a file ----
                    h3("データセットの読込み"),
                    fileInput("file3", "upload csv file here",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    h3("分類を出力"),
                    selectInput("data_for_classificationY",
                                "目的変数を選択",
                                choices = colnames(df), selected = colnames(df)[1]),
                    
                    h3("選択済みの説明変数"),
                    verbatimTextOutput("rows_selected_classification"),
                    
                    selectInput("classification_type",
                                "分類手法を選択",
                                choices = c("ランダムフォレスト" = "rf",
                                            "3層ニューラルネット" = "nnet")),
                    
                    actionButton("classification_button", "実行")
                    ),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Table",
                                         h3("説明変数を選択"),
                                         DT::dataTableOutput("data_table_for_classification")),
                                
                                tabPanel("分類結果", verbatimTextOutput("summary_classification")),
                                )
                    )
                  )
                ),
  
  ## クラスタリングタブ ----
        tabItem(tabName = "tab_D",
                sidebarLayout(
                  sidebarPanel(
                    
                    ### Input: Select a file ----
                    h3("データセットの読込み"),
                    fileInput("file4", "upload csv file here",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    verbatimTextOutput("rows_selected_clustering"),
                    numericInput("cluster_number", "クラスタ数を指定",
                                 min = 1, max = 10, value = 1),
                    actionButton("clustering_button", "クラスタリング")
                    ),
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Table",
                                         h3("説明変数を選択"),
                                         DT::dataTableOutput("data_table_for_clustering")),
                                tabPanel("クラスタリング結果",
                                         DT::dataTableOutput("data_with_clustering_result")))
                    )
                  )
                )

 
    )
  )
)
