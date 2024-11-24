#' Estimation Trends of age
#'
#' @export trend_age
#' @import shiny
#'
#' @importFrom tibble column_to_rownames
#' @importFrom tools file_ext
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' ## Run the App
#' trend_age()
#' }
#'


library(shiny)
trend_age <- function(...){

  #### UI for trend age ####
  ### 选择函数 Ui ###
  ChooseFunc_tabs1 <- tabsetPanel(
    id = "FormulaRW" , type = "hidden",

    tabPanel( "none", helpText( "Formula : No fun ")  ) ,## none
    tabPanel( "exp", helpText("$$ Ringwidth = \\alpha \\cdot e^{\\beta age } + c  $$")  ) ,## exp
    tabPanel( "log", helpText("$$ Ringwidth = \\alpha \\cdot \\log_{ \\beta} age + c  $$")  ) ,## log
    tabPanel( "pow", helpText("$$ Ringwidth = \\alpha \\cdot age^{ \\beta  } + c   $$") ) ## pow
  )
  ChooseFunc_tabs2 <- tabsetPanel(
    id = "FormulaLA" , type = "hidden",

    tabPanel( "none", helpText( "Formula : No fun ")  ) ,## none
    tabPanel( "exp", helpText("$$ MaxLA = \\alpha \\cdot e^{\\beta age } + c  $$")  ) ,## exp
    tabPanel( "log", helpText("$$ MaxLA = \\alpha \\cdot \\log_{ \\beta} age + c  $$")  ) ,## log
    tabPanel( "pow", helpText("$$ MaxLA = \\alpha \\cdot age^{ \\beta  } + c   $$") ) ## pow
  )


  ### Trend Age ###
  TrendAgeUI <- fluidPage(
    withMathJax(),
    # App title -
    titlePanel("Trend Age"),

    # Sidebar layout with input and output definitions -
    sidebarLayout(

      # Sidebar panel for inputs -
      sidebarPanel( width = 4,
                    ## title
                    "Fitting Age Trend (GAM & other function)",
                    # Input: Select a file -
                    fileInput("fileRW", "Updata data of TreeRing series(ring width & MaxLA )[xlsx or csv]",
                              multiple = FALSE,
                              accept = c(".xlsx", ".xls", ".csv")),

                    #
                    selectizeInput("xaxis", "colum of age", choices = NULL ),
                    selectizeInput("MRW", "colum of ring width", choices = NULL ),
                    selectizeInput("MaxLA", "colum of Max lumen area", choices = NULL ),
                    tags$hr(),
                    selectizeInput("TLRW", "Select ringwidth trend line for output", choices = c( 'GAM', "Fitting" , "Manual"   ) ),
                    selectizeInput("TLLA", "Select ringwidth trend line for output", choices = c( "None",'GAM', "Fitting" , "Manual"   ) ),

                    fluidRow( column( width = 6, actionButton("Sim", "Calculate") )  ,
                              column( width = 6,downloadButton("downloadData", "Download Trend_age") )    ) ,

                    ## 需要在server 里 updata
                    # updateSelectizeInput(session, 'x2', choices = list(
                    #   Eastern = list(`Rhode Island` = 'RI', `New Jersey` = 'NJ'),
                    #   Western = list(`Oregon` = 'OR', `Washington` = 'WA'),
                    #   Middle = list(Iowa = 'IA')
                    # ), selected = 'IA')
                    #
                    tags$hr(),
                    ### input Knots of gam , use slider
                    sliderInput("rwK",  "k value of ring width:",
                                min = 1, max = 12, step = 1, value = 5 ), ## end of sliderinput

                    sliderInput("laK",  "k value of max lumen area:",
                                min = 1, max = 12, step = 1, value = 5 ), ## end of sliderinput
                    tags$hr(),

                    # Input: Select function -
                    fluidRow(
                      column(  width = 6 ,radioButtons("sepRW", "Trend funtion of Ring wdith",
                                                       choices = c(None = "none",
                                                                   Exponential  = "exp",
                                                                   Logarithmic  = "log",
                                                                   Power  = "pow"),
                                                       selected = "none") ,ChooseFunc_tabs1 ) ,
                      column(  width = 6 ,radioButtons("sepLA", "Trend funtion of max LA",
                                                       choices = c(None = "none",
                                                                   Exponential  = "exp",
                                                                   Logarithmic  = "log",
                                                                   Power  = "pow"),
                                                       selected = "none") , ChooseFunc_tabs2 ) ,

                    ),

                    tags$hr(),
                    ## Whether the user sets the parameters manually
                    checkboxInput("headerRW", "Manual RW parameter setting", FALSE ),
                    conditionalPanel(
                      condition = "input.headerRW == true",  # Display the tabsetPanel when checkbox is checked
                      ### input param 1 & 2 :alpha_age, beta_age
                      fluidRow(
                        column(6,helpText( "Parameters of RW")) ,
                        column(6, numericInput("rwA", "Alpha", NA, min = -Inf , max = Inf )) ),
                      fluidRow(
                        column(6,numericInput("rwB", "Beta", NA, min = -Inf , max = Inf )),
                        column(6,numericInput("rwC", "C", NA, min = -Inf , max = Inf )) )

                    ),
                    checkboxInput("headerMaxLA", "Manual MAxLA parameter setting", FALSE ),
                    conditionalPanel(
                      condition = "input.headerMaxLA == true",  # Display the tabsetPanel when checkbox is checked
                      fluidRow(
                        column(6,helpText( "Parameters of MaxLA")) ,
                        column(6, numericInput("laA", "Alpha", NA, min = -Inf , max = Inf )) ),
                      fluidRow(
                        column(6,numericInput("laB", "Beta", NA, min = -Inf , max = Inf )),
                        column(6,numericInput("laC", "C", NA, min = -Inf , max = Inf )) )

                    )
      ), ## sidebarPanel end

      # Main panel for displaying outputs -
      mainPanel(

        # Output: Data file -
        tableOutput("contents"),
        tableOutput('SimParam'),
        plotOutput('PlotTrend'),
        tableOutput("TestTrend")
      ) ## mainPanel end

    )## sidebarLayout end
  )## fluidPage end
  #### UI trend_age end ----

  #### Trend age server ####
  TrendAgeServer <-  function( input, output, session ){
    ## 数据准备
    dataIn <- reactiveValues( 'ManualParam' = data.frame( 'names' = c('RW', 'MaxLA'), 'As' = c(NA,NA ) ,
                                                          'Bs'  = c(NA,NA ), 'Cs' =  c(NA,NA ) ) |>
                                tibble::column_to_rownames('names'),
                              'O_param' = data.frame( 'names' = c('RW', 'MaxLA'), 'As' = c(NA,NA ) ,
                                                      'Bs'  = c(NA,NA ), 'Cs' =  c(NA,NA ) , 'Ks' = c(NA,NA) ) |>
                                tibble::column_to_rownames('names')  )

    ## 更新公式UI
    observeEvent( input$sepRW,{
      updateTabsetPanel(  inputId = "FormulaRW", selected = input$sepRW ) }  )
    observeEvent( input$sepLA,{
      updateTabsetPanel(  inputId = "FormulaLA", selected = input$sepLA ) }  )

    ## 读取数据
    output$contents <- renderTable({

      req(input$fileRW)
      file <- input$fileRW$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          ifelse(ext == "csv",
                 df <- read.csv( file,  header = T ) ,
                 df <- openxlsx::read.xlsx( file  ) )
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      dataIn$dtRW <- df
      return( head(df,3) )

    }) ## output$contents end ------

    ## 添加列选项
    observeEvent( input$fileRW, {
      updateSelectizeInput(session, "xaxis", choices = colnames( dataIn$dtRW )    )
      updateSelectizeInput(session, "MRW", choices = colnames( dataIn$dtRW )    )
      updateSelectizeInput(session, "MaxLA", choices = colnames( dataIn$dtRW )    )
    })

    ## 计算趋势线
    observeEvent(input$Sim,{ ### 按键之后执行

      if ( any( c( is.null(input$xaxis) ,  is.null(  input$MRW) )   )     ) {
        stop(safeError(  "“colum of age” and “colum of max lumen area” cannot be null."  ))
      }


      ## 按列选择数据
      Cols <- c("Year", input$xaxis , input$MRW,input$MaxLA )
      dt <- dataIn$dtRW[,Cols] |> na.omit()

      colnames(  dt ) <- c("Year", "age", "RW","MaxLA"   )[  1: ncol( dt ) ]

      ## 更新参数列表

      dataIn$ManualParam$As <- c( input$rwA , input$laA   )
      dataIn$ManualParam$Bs <- c( input$rwB , input$laB   )
      dataIn$ManualParam$Cs <- c( input$rwC , input$laC   )
      dataIn$O_param$Ks <- c( input$rwK,input$laK    )
      dataIn$O_param$method <- c( input$sepRW,input$sepLA  )

      ## 数据输入画图和计算 Function
      ResTrendAge <- RegPlot(RegData = dt, Mparam = dataIn$ManualParam , Oparam = dataIn$O_param )

      ##
      dataIn$Trend <- ResTrendAge$DtTrend
      dataIn$ManualParam <-ResTrendAge$Mparam
      dataIn$O_param<-ResTrendAge$Oparam
      # req(TabParam )
      # req(ResTrendAge$Plot )
      # req(ResTrendAge$modelTest )

      TabParam <- ResTrendAge$Oparam
      colnames(TabParam  ) <- c( "α", "β" ,"c" ,"k" , 'Method')
      output$SimParam <- renderTable( TabParam )
      output$PlotTrend <- renderPlot(  ResTrendAge$Plot )
      output$TestTrend <- renderTable(  ResTrendAge$modelTest )
      CRW <- dplyr::case_when( input$TLRW == "GAM"~  'G_RW' , input$TLRW == "Fitting"~ "F_RW" ,
                               input$TLRW == "Manual"~ "M_RW"   )
      CLA <- dplyr::case_when( input$TLLA == "GAM"~  'G_MaxLA' ,
                               input$TLLA == "Fitting"~ "F_MaxLA" , input$TLLA == "Manual"~ "M_MaxLA"   )

      if ( !is.na(CLA) ) {
        File1 <- dataIn$Trend[,c( 'Year', 'age',CRW,CLA )   ]
        colnames(File1 ) <- c( 'Year', 'age','Tage','Lage'  )
      } else {
        File1 <- dataIn$Trend[,c( 'Year', 'age',CRW )   ]
        colnames(File1 ) <- c( 'Year', 'age','Tage'  )
        File1$Lage <- 1
      }
      dataIn$Out <- File1

    }) ## input$Sim end --------


    # Downloadable xlsx of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = 'TrendAge.xlsx',
      content = function(file) {
        openxlsx::write.xlsx( dataIn$Out , file)
      }
    )

  }
  #### Trend age server end ----

  shinyApp(TrendAgeUI, TrendAgeServer)
  #### UI for Trend age end ---------

}
