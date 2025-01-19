#' Calibration Cell Growth Parameters
#' @name Calibration_Cell_Growth_Parameters
#'
#' @export calibrate_BTR
#' @import shiny
#'
#' @importFrom tibble column_to_rownames
#' @importFrom rBTR btr_parallel nor
#' @importFrom tidyr gather separate
#' @importFrom tools file_ext
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom dplyr case_when filter select mutate bind_rows rename group_by summarise
#' @importFrom ggplot2 scale_y_continuous expansion geom_point geom_smooth geom_text geom_hline margin scale_x_continuous geom_bar
#' @importFrom ggplot2 ggplot theme_bw theme element_text geom_line aes facet_wrap labs geom_boxplot scale_y_sqrt facet_grid
#' @importFrom ggpubr ggarrange
#'
#' @examples
#' \dontrun{
#' ## Run the App
#' calibrate_BTR()
#' }
#'
  library(shiny)
calibrate_BTR <- function(....){

  ### UI for Sim BTR model ###
  SimBTRUI <- fluidPage( #### ui ####
    titlePanel( "Calibrate BTR model parameters"),
    fluidRow( column( numericInput( 'RTD', "Simuate zone width", value = 1000, min = 0, step = 50  ) ,width = 2  ),
              column( numericInput( 'Syear', "Start year", value = 2010, min = 0, step = 50  ),
                      numericInput( 'Eyear', "End year", value = 2015, min = 0, step = 50  ), width = 2  ),
              column( sliderInput( 'Cores', "Parallel core number",value = 6, min = 0,max = 24, step = 1   ),
                      actionButton( 'RunBTR', "RUN BTR"  ),width = 3   ),
              column( sliderInput( 'ShowY', "Show Years",value = c(2010,2015), min = 0,max = 2020, step = 1   ),
                      actionButton( 'Refresh', "Refresh sliderbar"  ),width = 2  ),
              column( downloadButton("downloadData", "Download parameter list") ,
                      downloadButton("downloadRes", "Download simulate result"),width = 3    )),
    ## pages
    tabsetPanel(
      tabPanel( "Read Datas", ## Tab input ###
                column(  width = 2 ,## Col 0
                         fileInput("fileClim", "Updata Climate data", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         fileInput("fileParam", "Updata Parameter list", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         fileInput("fileTrend", "Updata trend of rings", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         tags$hr(),
                         fileInput("fileRW", "Updata observe data of tree-ring", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         tags$hr(),
                         fileInput("fileFiber", "Updata observe data of fiber", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         tags$hr(),
                         fileInput("fileVessel", "Updata observe data of vessel" ,multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,

                ), ## Col 0.1 -
                column( width = 3, ##Col 0.2
                ), ##Col 0.2 -
                column( width =  7,  ## Col 0.3
                        tableOutput("TabClim"),
                        tags$hr(),
                        tableOutput("TabParam"),
                        tags$hr(),
                        tableOutput("TabTrend"),
                        tags$hr(),
                        tableOutput("TabRW"),
                        tags$hr(),
                        tableOutput("TabFiber"),
                        tags$hr(),
                        tableOutput("TabVessel")
                ) ## Col 0.3 -

      ), ## Tab input ---


      tabPanel( "Photoperiod ", ## Tab Photoperiod ###
                column( width = 4 ,## Col 1.1
                        fluidRow(
                          column(width = 6 ,
                                 helpText("BTR param"),
                                 lapply( c( 'a.fiber',  'b/c.fiber', 'c.fiber')  , function(sips) {
                                   sliderInput( inputId =sips, label =sips,
                                                min = 0, max = 2 , value = 0, step = 0.1 )  }),
                                 tags$hr(),actionButton( "CoverFiberLi", "Use New Parameters") ), ##col1
                          column(width = 6 ,
                                 helpText("BTR param"),
                                 lapply(c( 'a.vessel',  'b/c.vessel', 'c.vessel'  ) , function(sips) {
                                   sliderInput( inputId =sips, label = sips,
                                                min = 0, max = 2 , value = 0, step = 0.1 )  }),
                                 tags$hr(),
                                 actionButton( "CoverVesselLi", "Use New Parameters"),
                          ) )  ##col2
                ), ## Col 1.1

                column(  width = 8, ## Col 1.2 Res
                         fluidRow( plotOutput( 'PhotoOld' ,height = '200px' )  ),
                         fluidRow( plotOutput( 'PhotoNew',height = '500px')  ),
                         # fluidRow(  tableOutput("testTableF")),
                         # fluidRow(  tableOutput("testTableCB"))
                ), ## Col 1.2

      ), ## Tab Photoperiod ---
      tabPanel( "Grwth Rate", ## tab  gR ###
                column( width = 4 , ## Col 2.1

                        # uiOutput("slidersVessel")
                        fluidRow(
                          column(width = 6 ,
                                 # helpText("Set gT shape"),
                                 # sliderInput( inputId = "gTmid" , label = "Width of 90%gT",
                                 #              min = 0, max = 40 , value = c( 15, 20 ), step = 0.1  ),

                                 helpText("Growth Rate parameters"),
                                 sliderInput( inputId = "T1&4" , label = "Growth Tem threshold",
                                              min = 0, max = 45 , value = c( 0, 45), step = 0.1  ),
                                 lapply(c( 'deltaH_A_Da','deltaH_D','deltaS_D')  , function(sips) {
                                   sliderInput( inputId = sips , label = sips ,
                                                min = 0, max = 2 , value = 1, step = 0.1  )   })
                          ),
                          # column(width = 4 , ## Clim
                          #         ),
                          column(width = 6 , ## Cambial
                                 lapply(c( 'M1&2','M3&4', 'VPD1&2','VPD3&4' )  , function(sips) {
                                   sliderInput( inputId = sips , label = sips ,
                                                min = 0, max = 4 , value = c(1,2   ), step = 0.1  )   }),
                                 tags$hr(),
                                 actionButton( "CoverGrowthRate", "Use New Parameters"),
                          ), ##col1
                        )

                ), ## Col 2.1
                column(  width = 8, ## Col 2.2 Res
                         fluidRow( plotOutput( 'Growths',height = "720px" )  ),
                         # tableOutput(  'T1'),
                         # tableOutput(  'T2'),

                ), ## Col 2.2

      ), ## Tab gR  ---

      tabPanel( "Cambial Activaty" , ## tab  Cambial activaty ###
                column( width = 4 , ## Col 2.1

                        # uiOutput("slidersVessel")
                        fluidRow(
                          column(width = 6 ,
                                 lapply(c( 'Div_alpha','LAtoDiv','a1','a2','maxRCTA','RCTADivT')   , function(sips) {
                                   sliderInput( inputId = sips , label =  sips ,
                                                min = 0 ,max = 2 ,value = 1,step = 0.1)  }) ),
                          # column(width = 4 , ## Clim
                          #         ),
                          column(width = 6 , ## Cambial
                                 lapply(c( 'AAT','deltaD', 'va_cz','alpha_cz','beta_cz')   , function(sips) {
                                   sliderInput( inputId = sips , label =  sips ,
                                                min = 0 ,max = 2 ,value = 1,step = 0.1)  }),
                                 tags$hr(),
                                 actionButton( "CoverCbAct", "Use New Parameters"), ), ##col1
                        )

                ), ## Col 2.1
                column( 'CbActs', width = 4, ## Col 2.2 Res
                        plotOutput( 'CbActs' ,height = "720px" ) ,
                ), ## Col 2.2
                column( "Ring", width = 4, ## Col 2.3 Res
                        plotOutput( 'Rings' ,height = "700px" ) ,
                ) ## Col 2.2

      ), ## Tab gR  ---
      tabPanel( "Model Results",
                column( width  = 4 , ## Ring Results
                        'Rings',
                        # fluidRow(
                        plotOutput( 'Rings' ,height = "700px" ), # ),
                ),## Ring result-
                column( width = 8,## Cells Result-
                        plotOutput( 'Fibers' ,height = "400px"  ),
                        plotOutput( 'Vessels' ,height = "250px" ),

                ),## Cells Result-


      )

    ), ## tabsetPanel end -

  ) #### ui end -------
  #### UI for sim BTR -----

  SimBTRServer <- function(input, output, session){
    dataInBTR <- reactiveValues(   )
    dataResBTR <- reactiveValues(   )

    ## 读取数据 ##
    output$TabClim <- renderTable({  ## param
      req(input$fileClim)
      file <- input$fileClim$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      dataInBTR$dtClim <- df ## Total Param

      return( head(df,3) )
    }) ## output$dParam end ------
    ##
    output$TabParam <- renderTable({  ## param
      req(input$fileParam)
      file <- input$fileParam$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      dataInBTR$dtParam <- df ## Total Param

      return( head(df,3) )
    }) ## output$dParam end ------
    ##
    output$TabTrend <- renderTable({  ## param
      req(input$fileTrend)
      file <- input$fileTrend$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      dataInBTR$dtTrend <- df ## Total Param

      return( head(df,3) )
    }) ## output$dParam end ------
    ##
    output$TabRW <- renderTable({  ## param
      req(input$fileRW)
      file <- input$fileRW$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      dataInBTR$dtRW <- df |> dplyr::mutate(type= "Obs") ## Total Param

      return( head(df,3) )
    }) ## output$dParam end ------
    ##
    output$TabFiber <- renderTable({
      req(input$fileFiber)
      file <- input$fileFiber$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      cls <- intersect(colnames(df),c( 'Year','LA','RadDistR','RRadDistR','CWTall'))
      dataInBTR$dtFiber <- df <- df[ , cls   ] |> dplyr::mutate(type= "Obs")
      return( head(df,3) )
    }) ## output$dFiber end ------
    ##
    output$TabVessel <- renderTable({
      req(input$fileVessel)
      file <- input$fileVessel$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      cls <- intersect(colnames(df),c( 'Year','LA','RadDistR','RRadDistR'))
      dataInBTR$dtVessel <- df <- df[, cls   ] |> dplyr::mutate(type= "Obs")
      return( head(df,3) )
    }) ## output$dVessel end ------

    ## 更新数据 ##
    observeEvent( input$Refresh,{
      ## 更新show year
      minyear <- max( min(dataInBTR$dtClim$Year) ,min(dataInBTR$dtTrend$Year), input$Syear   )
      maxyear <- min( max(dataInBTR$dtClim$Year) ,max(dataInBTR$dtTrend$Year), input$Eyear   )

      updateSliderInput(inputId = 'ShowY', value = c( minyear, maxyear   ),min = minyear , max = maxyear    )

      ## 更新RTD
      updateNumericInput( inputId = 'RTD', value = dataInBTR$RingParam$Values[dataInBTR$RingParam$Parameter == "Twidth" ]   )
      ## 更新 CambialActivity
      dtp1 <- dataInBTR$dtParam[ dataInBTR$dtParam$Module == "CambialActivity",  ]
      lapply( dtp1$Parameter, function(sips) {
        updateSliderInput(session ,
                          inputId = sips ,
                          # label = sips ,
                          min = min(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ),
                          max = max(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ) ,
                          value = dtp1$Values[dtp1$Parameter == sips],
                          step = round( abs(dtp1$Values[dtp1$Parameter == sips] )/ 25 , 3  ) )
      })
      ## 更新 Photoperiod
      dtp1<- dataInBTR$dtParam[ dataInBTR$dtParam$Module == "GrowthRate" &
                                  dataInBTR$dtParam$Note %in% c( 'Fiber' , 'Vessel' ),  ]
      lapply( dtp1$Parameter|> setdiff(c("b.vessel" , "b.fiber" )   ), function(sips) {
        updateSliderInput(session ,
                          inputId = sips ,
                          # label = sips ,
                          min = min(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ),
                          max = max(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ) ,
                          value = dtp1$Values[dtp1$Parameter == sips],
                          step = round( abs(dtp1$Values[dtp1$Parameter == sips]) / 25 , 3  ) )
      })

      bcfiber = dtp1$Values[ dtp1$Parameter == "b.fiber"  ]/dtp1$Values[ dtp1$Parameter == "c.fiber"  ]
      bcvessel = dtp1$Values[ dtp1$Parameter == "b.vessel"  ]/dtp1$Values[ dtp1$Parameter == "c.vessel"  ]
      updateSliderInput(inputId = 'b/c.fiber',min = min(round( bcfiber *0.5 ,3  ) ,
                                                        round( bcfiber *1.5 ,3  ) ),
                        max = max(round( bcfiber *0.5 ,3  ) ,
                                  round( bcfiber *1.5 ,3  ) ) ,
                        value = round( bcfiber,3 ),
                        step = round( abs(bcfiber)  / 25 , 3  ))

      updateSliderInput(inputId = 'b/c.vessel',
                        min = min(round( bcvessel *0.5 ,3  ) , round( bcvessel *1.5 ,3  ) ),
                        max = max(round( bcvessel *0.5 ,3  ) , round( bcvessel *1.5 ,3  ) ) ,
                        value =round( bcvessel ,3 ) ,
                        step = round( abs(bcvessel)  / 25 , 3  ))





      ## 更新 GrowthRate
      dtp1 <- dataInBTR$dtParam[  dataInBTR$dtParam$Note %in% c( 'Growth'),  ]
      updateSliderInput(session ,"VPD3&4", value = c( dtp1$Values[dtp1$Parameter == "VPD3"],dtp1$Values[dtp1$Parameter == "VPD4"] ) )
      updateSliderInput(session ,"VPD1&2", value = c( dtp1$Values[dtp1$Parameter == "VPD1"],dtp1$Values[dtp1$Parameter == "VPD2"] ) ,
                        max = dtp1$Values[dtp1$Parameter == "VPD3"] )
      updateSliderInput(session ,"M3&4", value = c( dtp1$Values[dtp1$Parameter == "M3"],dtp1$Values[dtp1$Parameter == "M4"] ), max = 1,step = 0.02 )
      updateSliderInput(session ,"T1&4", value = c( dtp1$Values[dtp1$Parameter == "T1"],dtp1$Values[dtp1$Parameter == "T4"] ) )
      updateSliderInput(session ,"M1&2", value = c( dtp1$Values[dtp1$Parameter == "M1"],dtp1$Values[dtp1$Parameter == "M2"] ),
                        max = dtp1$Values[dtp1$Parameter == "M3"] ,step = 0.02 )



      lapply( c( 'deltaH_A_Da','deltaH_D','deltaS_D'  ), function(sips) {
        updateSliderInput(session ,
                          inputId = sips ,
                          # label = sips ,
                          min = min(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ),
                          max = max(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ) ,
                          value = dtp1$Values[dtp1$Parameter == sips],
                          step = round( dtp1$Values[dtp1$Parameter == sips]/ 25 , 3  ) )
      })



    }) ## Refresh -

    ## 保存参数和结果 ##
    # Downloadable xlsx of BTR modle result dataset
    output$downloadData <- downloadHandler(
      filename = 'Parameter.xlsx',
      content  = function(file) {
        openxlsx::write.xlsx( dataInBTR$dtParam , file)
      })

    # Downloadable xlsx of Params dataset
    output$downloadRes <- downloadHandler(
      filename = 'BtrResult.xlsx',
      content  = function(file) {
        openxlsx::write.xlsx( dataResBTR$ResBTR , file)
      })


    ## RUn BTR ##
    # 运行BTR 模型, 根据输入的Obs数据展示模型模拟效果
    observeEvent(input$RunBTR,{

      dataInBTR$dtParam$Values[ dataInBTR$dtParam$Parameter == "Twidth"  ] <- input$RTD

      dataResBTR$ResBTR <- rBTR::btr_parallel(clim = dataInBTR$dtClim, parameters = dataInBTR$dtParam,age = dataInBTR$dtTrend,
                                              syear = input$Syear, eyear = input$Eyear, Cores = input$Cores )
      #### 整理数据
      dataInBTR$ObsInput <- c( 0,0,0 )
      dataInBTR$ObsInput[1][ any( names(dataInBTR)  == "dtRW" ) ] <- 1
      dataInBTR$ObsInput[2][ any( names(dataInBTR)  == "dtFiber" ) ] <- 1
      dataInBTR$ObsInput[3][ any( names(dataInBTR)  == "dtVessel" ) ] <- 1

      ifelse( dataInBTR$ObsInput[2] == 1 , yC <- unique(dataInBTR$dtFiber$Year), yC <- NA  )
      ifelse( dataInBTR$ObsInput[3] == 1 , yV <- unique(dataInBTR$dtVessel$Year), yV <- NA)

      dataResBTR$ReRes <- BtrResRemake(  dtlist = dataResBTR$ResBTR ,dtin = dataInBTR, years = input$ShowY, yC, yV )

      RingFig <- list()
      if (dataInBTR$ObsInput[1] == 1 ) {
        for (i_p in intersect( unique(dataResBTR$ReRes$pdRW$Factor), c( 'MRW', 'CD', 'RCTA'))  ) {##
          RingFig[[i_p]] <- dataResBTR$ReRes$pdRW |> dplyr::filter(  Factor == i_p ) |>
            ggplot2::ggplot( ggplot2::aes(x = Year, y =val, color = type, linetype = type )   )+
            ggplot2::theme_bw()+
            ggplot2::theme(text = ggplot2::element_text(size = 14))+
            ggplot2::labs(y = i_p, subtitle = paste0( 'Cor: ', dataResBTR$ReRes$ModTestList[[i_p]]$COR,"   p: ",dataResBTR$ReRes$ModTestList[[i_p]]$pvalue,
                                                      '   RMSE: ', dataResBTR$ReRes$ModTestList[[i_p]]$RMSE, '\nNRMSD: ', dataResBTR$ReRes$ModTestList[[i_p]]$NRMSD,
                                                      '   MAE: ', dataResBTR$ReRes$ModTestList[[i_p]]$MAE,'   MAPE: ', dataResBTR$ReRes$ModTestList[[i_p]]$MAPE  ) )+
            ggplot2::geom_line(linewidth = 1.1)+
            ggplot2::facet_grid( Factor~. ,scales = 'free')
        }
        output$Rings <- renderPlot({ ggpubr::ggarrange( plotlist = RingFig, ncol = 1, align = 'hv',common.legend = T,legend = 'top') })
      }

      if (dataInBTR$ObsInput[2] == 1 ) {
        output$Fibers <- renderPlot({
          ggplot2::ggplot( )+
            ggplot2::theme_bw()+
            ggplot2::scale_y_continuous(expand =ggplot2::expansion(c(0,0.4 ) )  )+
            ggplot2::geom_point( data= dataResBTR$ReRes$pdFiber , ggplot2::aes( x = RRadDistR , y = val , color = "Sim"),size = 1,alpha= 0.5     )+
            ggplot2::geom_line( data = dataResBTR$ReRes$GamFiber , ggplot2::aes(x = RRadDistR, y = val, color= "ObsLine" ), linetype = 2,linewidth = 1.5   )+
            ggplot2::geom_smooth( data= dataResBTR$ReRes$pdFiber , ggplot2::aes( x = RRadDistR , y = val , color = "SimLine"), linewidth = 1 ,se= F,
                                  method = 'gam',formula = y~s(x, k = 5 )   )+
            ggplot2::geom_text(data =dataResBTR$ReRes$TestFiber, ggplot2::aes( x = Inf, y = Inf , label = ResC, hjust = 1.2, vjust = 1.2 ) )+
            ggplot2::facet_grid( factor(Fac,levels = c( 'LA' ,'CWTall') )  ~Year ,scale="free")
        })
      }
      if (dataInBTR$ObsInput[3] == 1 ) {
        output$Vessels <- renderPlot({
          ggplot2::ggplot( )+
            ggplot2::theme_bw()+
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0,0.4 ) )  )+
            ggplot2::geom_point( data= dataResBTR$ReRes$pdVessel , ggplot2::aes( x = RRadDistR , y = LA , color = "Sim"),size = 1,alpha= 0.5     )+
            ggplot2::geom_line( data = dataResBTR$ReRes$GamVessel , ggplot2::aes(x = RRadDistR, y = LA, color= "ObsLine" ), linetype = 2,linewidth = 1.5   )+
            ggplot2::geom_smooth( data= dataResBTR$ReRes$pdVessel , ggplot2::aes( x = RRadDistR , y = LA , color = "SimLine"), linewidth = 1 ,se= F,
                                  method = 'gam',formula = y~s(x, k = 4 )   )+
            ggplot2::geom_text(data =dataResBTR$ReRes$TestVessel, ggplot2::aes( x = Inf, y = Inf , label = ResC, hjust = 1.2, vjust = 1.2 ) )+
            ggplot2::facet_grid(.~Year ,scale="free")
        })
      }


      if (dataInBTR$ObsInput[2] == 1   ) {
        PshowF <- dataResBTR$ReRes$pdFiber[dataResBTR$ReRes$pdFiber$Fac == 'LA',] |>
          ggplot2::ggplot( ggplot2::aes(x = DOY , y = val, color = "Sim" ) )+ ##
          ggplot2::theme_bw()+
          ggplot2::labs(title = "Fiber"  )+
          ggplot2::geom_smooth(data = dataResBTR$ReRes$inFiber,se = F, method = 'gam', formula = y~s(x, k = 4),
                               ggplot2::aes(x = DOY , y = LA, group = Year ,color = "Obsline" )  )+
          ggplot2::geom_point(size = 3)+
          ggplot2::geom_smooth( ggplot2::aes(color= "SimLine"), method = 'gam', formula = y~s(x, k = 5),se = F ,linewidth = 1)

      }else{
        PshowF <- dataResBTR$ReRes$pdFiber[dataResBTR$ReRes$pdFiber$Fac == 'LA',] |>
          ggplot2::ggplot( ggplot2::aes(x = DOY , y = val, color = "Sim" ) )+ ##
          ggplot2::theme_bw()+
          ggplot2::labs(title = "Fiber"  )+
          ggplot2::geom_point(size = 3)+
          ggplot2::geom_smooth( ggplot2::aes(color= "SimLine"), method = 'gam', formula = y~s(x, k = 5),se = F ,linewidth = 1)

      }

      if (dataInBTR$ObsInput[3] == 1   ) {
        PshowV <- dataResBTR$ReRes$pdVessel |>
          ggplot2::ggplot( ggplot2::aes(x = DOY , y = LA, color = "Sim" ) )+ ##
          ggplot2::theme_bw()+
          ggplot2::labs(title = "Vessel"  )+
          ggplot2::geom_smooth(data = dataResBTR$ReRes$inVessel,se = F, method = 'gam', formula = y~s(x, k = 4),
                               ggplot2::aes(x = DOY , y = LA, group = Year ,color = "Obsline" )  )+
          ggplot2::geom_point(size = 3)+
          ggplot2::geom_smooth( ggplot2::aes(color= "SimLine"), method = 'gam', formula = y~s(x, k = 5),se = F ,linewidth = 1)

      }else{
        PshowV <- dataResBTR$ReRes$pdVessel |>
          ggplot2::ggplot( ggplot2::aes(x = DOY , y = LA, color = "Sim" ) )+ ##
          ggplot2::labs(title = "Vessel"  )+
          ggplot2::theme_bw()+
          ggplot2::geom_point(size = 3)+
          ggplot2::geom_smooth( ggplot2::aes(color= "SimLine"), method = 'gam', formula = y~s(x, k = 5),se = F ,linewidth = 1)

      }
      output$PhotoOld <- renderPlot({ ggpubr::ggarrange(PshowF, PshowV, ncol =2 ,align = 'hv', common.legend = T   )  })

      ## Other Calculate

      dataResBTR$ReRes$dLi <-
        dataResBTR$ResBTR$microclim[  dataResBTR$ResBTR$microclim$Year == min( dataResBTR$ResBTR$microclim$Year ), c( 'DOY','dL_i' ,'L_i.fiber', 'L_i.vessel' )   ]  |>
        # dplyr::filter( DOY %in% which.max(dataResBTR$ResBTR$microclim$dL_i):which.min(dataResBTR$ResBTR$microclim$dL_i) ) |>
        dplyr::rename( Old_fiber = L_i.fiber ,  Old_vessel = L_i.vessel  )

      dataResBTR$ReRes$dLi <- dataResBTR$ReRes$dLi |> dplyr::filter( DOY %in% which.max(dataResBTR$ReRes$dLi$dL_i):which.min(dataResBTR$ReRes$dLi$dL_i) )


      dataResBTR$ReRes$detrendRW <- detrendRW( dtTrend = dataInBTR$dtTrend, dtRW = dataInBTR$dtRW )

      dataResBTR$ReRes$MeanClim <- dataResBTR$ResBTR$microclim |> dplyr::group_by(DOY) |>
        dplyr::summarise( TEM = mean(TEM), soilM = mean(soilM) , VPD = mean(VPD)  ) |>
        tidyr::gather( Clim, val,-1)


    }) ## runBTR

    ## Tab Photoperiod
    output$PhotoNew <- renderPlot({
      req( input$RunBTR  )
      dataResBTR$ReRes$dLi$New_fiber <- input$a.fiber * exp(
        -exp( -input$c.fiber* (  dataResBTR$ReRes$dLi$dL_i - input$`b/c.fiber`) ) )
      dataResBTR$ReRes$dLi$New_vessel <- input$a.vessel * exp(
        -exp( -input$c.vessel* (  dataResBTR$ReRes$dLi$dL_i - input$`b/c.vessel`) ) )

      dt <- dataResBTR$ReRes$dLi |>
        tidyr::gather( key = type, value = L_i ,c(3:6  ) )|>
        tidyr::separate( type, c( 'Lis', 'type'),sep = "_" )
      dt2 <- data.frame( h1 = c( input$a.fiber ,input$a.vessel) , type= c( "fiber" , 'vessel'  )   )

      ggpubr::ggarrange(
        ggplot2::ggplot( dt, ggplot2::aes(x = dL_i , y =L_i , color = Lis ,linetype = Lis   ) )+
          ggplot2::theme_bw()+
          ggplot2::geom_hline( data = dt2 ,ggplot2::aes(yintercept = h1 ) ,color = 'red', linetype =2  )+
          ggplot2::geom_line(  linewidth = 1   ) +
          ggplot2::facet_wrap(. ~ type , scales = 'free'  ),
        ggplot2::ggplot( dt, ggplot2::aes(x = DOY , y =-L_i , color = Lis ,linetype = Lis   ) )+
          ggplot2::theme_bw()+
          ggplot2::geom_hline( data = dt2 ,ggplot2::aes(yintercept = -h1 ) ,color = 'red', linetype =2  )+
          ggplot2::geom_line(  linewidth = 1  ) +
          ggplot2::facet_wrap(. ~ type , scales = 'free'  )
        ,ncol = 1, align = 'hv', common.legend = T
      )


    })## PhotoNew-

    ## Set Photoperiod param
    observeEvent( input$CoverFiberLi ,{
      Prarms <- data.frame( Parameter = c( "a.fiber","b.fiber","c.fiber"    ),
                            Values = c ( input$a.fiber, input$`b/c.fiber`*input$c.fiber, input$c.fiber))
      dataInBTR$dtParam <- NewReplacesOld(  Prarms, dataInBTR$dtParam  )
    })
    observeEvent( input$CoverVesselLi ,{
      Prarms <- data.frame( Parameter = c( "a.vessel","b.vessel","c.vessel"    ),
                            Values = c ( input$a.vessel, input$`b/c.vessel`*input$c.vessel, input$c.vessel))
      dataInBTR$dtParam <- NewReplacesOld(Prarms , dataInBTR$dtParam  )
    })
    ### error catch
    observeEvent( input$`M3&4`[1],{
      ifelse ( input$`M1&2`[2] > input$`M3&4`[1], t2 <- input$`M3&4`[1] , t2 <-input$`M1&2`[2]    )
      updateSliderInput(inputId ='M1&2', max = input$`M3&4`[1] ,value = c( input$`M1&2`[1], t2)  )
    }  )
    observeEvent( input$`VPD3&4`[1],{
      ifelse ( input$`VPD1&2`[2] > input$`VPD3&4`[1], t2 <- input$`VPD3&4`[1] , t2 <-input$`VPD1&2`[2]    )
      updateSliderInput(inputId ='VPD1&2', max = input$`VPD3&4`[1] ,value = c( input$`VPD1&2`[1], t2)  )
    }  )

    ## Tab Growth Rate
    output$Growths <- renderPlot({
      GRParam <- data.frame( Parameter = c("T1","T4","deltaH_A_Da","deltaH_D","deltaS_D","M1","M2","M3","M4","VPD1","VPD2","VPD3","VPD4"),
                             Values = c( input$`T1&4`, input$deltaH_A_Da,input$deltaH_D,input$deltaS_D,
                                         input$`M1&2`,input$`M3&4`, input$`VPD1&2`,input$`VPD3&4`) )
      Pdata <- GRsim( GRParam = GRParam, dt = dataInBTR$dtClim , dtRW = dataResBTR$ReRes$detrendRW , ys = input$ShowY   )
      if( length(Pdata$LimY$degR) >=3   ){
        cors <- cor.test( Pdata$LimY$degR,Pdata$LimY$deRW )
      } else {
        cors <- cor.test( c(Pdata$LimY$degR,1,1  ) ,c(Pdata$LimY$deRW,1,1  )  )
      }

      # 三个曲线：
      df <- data.frame( Clim = 'TEM', Xval = seq( input$`T1&4`[1],input$`T1&4`[2],0.1  ) )
      Ta <- df$Xval + 273.15
      R <- 8.314
      df$Growth_rate <- ( Ta * exp( - input$deltaH_A_Da / ( R * Ta) ) /
                            (1 + exp( input$deltaS_D / R - input$deltaH_D /(R*Ta) ) ) ) |>
        rBTR::nor( Zeros = T)
      df2 <- data.frame( Clim = rep(c('SoilM',"VPD"),each =4 ) ,
                         Xval = c(  input$`M1&2`,input$`M3&4`, input$`VPD1&2`,input$`VPD3&4`     ),
                         Growth_rate = c( 0,1,1,0,0,1,1,0     )) |> dplyr::bind_rows(df)

      hlins <- data.frame( Clim = c('TEM','TEM','TEM', 'soilM', 'soilM','soilM','soilM','VPD','VPD','VPD','VPD' ) ,
                           Label = c( 'Val1', 'Val2','Val4','Val1', 'Val2', 'Val3', 'Val4','Val1', 'Val2', 'Val3', 'Val4'),
                           num = c( GRParam[1,2], df$Xval[which.max(df$Growth_rate)] ,GRParam[2,2], GRParam[c(6:13),2]      )
      )


      Plots <-
        ggpubr::ggarrange(
          ggpubr::ggarrange(
            ggplot2::ggplot(  df2,ggplot2::aes(x = Xval, y =Growth_rate )  )+
              ggplot2::labs( x = NULL   )+
              ggplot2::geom_line()+
              ggplot2::theme_bw()+
              ggplot2::facet_grid(.~ factor(Clim, levels = c('TEM', 'SoilM', 'VPD')) , scale = 'free'),
            ggpubr::ggarrange(
              ggplot2::ggplot( Pdata$LimY )+
                ggplot2::labs(y = "normalization value", x= NULL,
                              subtitle = paste('r = ', round(cors$estimate,3 ), "p = ",round(cors$p.value,2)  )   )+
                ggplot2::theme_bw()+
                ggplot2::theme( plot.margin = ggplot2::margin(0, 0, 0, 0, "cm") )+
                ggplot2::scale_x_continuous(expand = c(0,0), limits = c( input$ShowY[1]-1,input$ShowY[2]+1    )  )+
                ggplot2::geom_line(ggplot2::aes(x = Year , y = rBTR::nor(deRW  ) , color = "detrend RW" ) )+
                ggplot2::geom_line(ggplot2::aes(x = Year , y = rBTR::nor(degR), color = "detrend gR" ) ) ,
              ggplot2::ggplot(Pdata$Lims)+
                ggplot2::theme_bw()+
                ggplot2::theme( plot.margin = ggplot2::margin(0, 0, 0, 0, "cm") )+
                ggplot2::scale_x_continuous(expand = c(0,0), limits = c( input$ShowY[1]-1,input$ShowY[2]+1    )  )+
                ggplot2::geom_bar( ggplot2::aes(x = Year , y = gR , fill = LimFac ),stat = "identity", position ='stack'  ),
              ncol = 1, align = 'hv' ),

            ncol = 1, align = 'hv', heights = c(0.35,1 ) ), ##


          ggplot2::ggplot( dataResBTR$ReRes$MeanClim,ggplot2::aes(x = DOY , y = val ) )+
            ggplot2::theme_bw()+
            ggplot2::labs(y = NULL,title =  "Climate")+
            ggplot2::theme( plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),legend.position = "bottom" )+
            ggplot2::geom_line(,linewidth = 1.2 )+
            ggplot2::geom_hline(  data =hlins ,ggplot2::aes(yintercept =num, color =Label   ), linetype =2,linewidth = 1 )+
            ggplot2::facet_grid( Clim~., scale="free"  ),
          ncol = 2,widths = c( 1,0.3)
        )


      # ggplot2::ggplot( data.frame(x = seq( input$`T1&4`[1],input$`T1&4`[2],0.1  )+ 273.15 ), ggplot2::aes(x) )+
      # ggplot2::geom_function( fun = function(x) ( x * exp( - input$deltaH_A_Da / ( 8.314 * x) ) /
      #                            (1 + exp( input$deltaS_D / 8.314 - input$deltaH_D /(R*x) ) ) ) )

      return(Plots)
    })
    ## Set Growth Rate param
    observeEvent( input$CoverGrowthRate ,{
      GRParam <- data.frame( Parameter = c("T1","T4","deltaH_A_Da","deltaH_D","deltaS_D","M1","M2","M3","M4","VPD1","VPD2","VPD3","VPD4"),
                             Values = c( input$`T1&4`, input$deltaH_A_Da,input$deltaH_D,input$deltaS_D,
                                         input$`M1&2`,input$`M3&4`, input$`VPD1&2`,input$`VPD3&4`) )
      dataInBTR$dtParam <- NewReplacesOld(GRParam , dataInBTR$dtParam   )
    })

    ## Tab Cambial Activaty

    output$CbActs <- renderPlot({
      ResDiv <- data.frame(
        DivLimit = seq(0,1,0.05),
        deltaD_dD = input$deltaD * ( 1 +  input$Div_alpha-
                                       exp( log(1/input$Div_alpha) * - seq(0,1,0.05) )  ),
        waterDivLim_dLim = input$a1 * seq(0,1,0.05),
        RctaDivLim_dLim = input$a2 * seq(0,1,0.05)
      ) |> tidyr::gather(key, val , c(2:4)) |> tidyr::separate(key,c( "Limit", 'type' ))

      CbAct <- data.frame(  ClimLimit = seq(0,1,0.05),
                            czgR =  input$alpha_cz * exp(input$beta_cz * ( seq(0,1,0.05) )  ) ) |>
        dplyr::mutate(czgR = dplyr::case_when(czgR > ClimLimit ~ ClimLimit , czgR < ClimLimit ~ czgR  ) ,
                      V_cz = czgR * input$va_cz ) |>
        tidyr::gather( key, val , c(2:3) )


      Plots <-
        ggpubr::ggarrange( ## [ ResDiv$type == "dLim",]
          ggplot2::ggplot(ResDiv , ggplot2::aes( x =DivLimit, y = val ,color = Limit  ) )+
            ggplot2::theme_bw()+
            # ggplot2::theme( plot.margin = ggplot2::margin(0, 0, 0, 0, "cm") )+
            ggplot2::labs(title = "Vessel Division", y = "dD", x = 'limited value')+
            ggplot2::theme(text = ggplot2::element_text( size = 14), legend.position = 'bottom' )+
            ggplot2::geom_line(  linewidth = 1  )+
            ggplot2::facet_wrap( .~type , scales = 'free'),

          ggplot2::ggplot( CbAct ,ggplot2::aes(x =ClimLimit , y = val , color = key  )   )+
            ggplot2::theme_bw()+
            ggplot2::theme( plot.margin = ggplot2::margin(0, 0, 0, 0, "cm") )+
            ggplot2::labs(title = "Cambial Activaty", y = "V_cz", x = 'egR')+
            ggplot2::theme(text = ggplot2::element_text( size = 14), legend.position = 'bottom' )+
            ggplot2::geom_abline(slope = 1,linetype=3)+
            ggplot2::geom_line(  linewidth = 1  )+
            ggplot2::facet_wrap( .~ key , scales = 'free'),
          ncol= 1, align = 'hv'
        )
      return(Plots)
    })
    ## Set Cambial Activaty param
    observeEvent( input$CoverCbAct ,{
      CbParam <- data.frame( Parameter = c( 'Div_alpha','LAtoDiv','a1','a2','maxRCTA','RCTADivT', 'AAT','deltaD', 'va_cz','alpha_cz','beta_cz'),
                             Values = c(input$Div_alpha ,input$LAtoDiv,input$a1 ,input$a2,input$maxRCTA,input$RCTADivT,
                                        input$AAT,input$deltaD,input$va_cz,input$alpha_cz,input$beta_cz) )
      dataInBTR$dtParam <- NewReplacesOld(CbParam , dataInBTR$dtParam   )
    })
  }

  shinyApp(  SimBTRUI, SimBTRServer)

}












