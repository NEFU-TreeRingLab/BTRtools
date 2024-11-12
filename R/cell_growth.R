#' Calibration Cell Growth Parameters
#'
#' @export cell_growth
#' @import shiny
#'
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr gather
#' @importFrom tools file_ext
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom dplyr case_when filter select mutate bind_rows
#' @importFrom ggplot2 ggplot theme_bw theme element_text geom_line aes facet_wrap labs geom_boxplot scale_y_sqrt
#' @importFrom ggpubr ggarrange
#'
#' @examples
#' \dontrun{
#' ## Run the App
#' cell_growth()
#' }
#'
  library(shiny)
cell_growth <- function(...){

  #### UI for Sim Cell ####
  SimCellUI <- fluidPage( ### ui ###
    titlePanel( "Calibrate Fiber & Vessel Growth Parameters"),
    fluidRow(
      column( width = 2,
              sliderInput("dry", "Drought rate:",
                          min = 0, max = 1, step = 0.05,
                          value = c( 0.4, 0.6) )  ), ## col T.1
      column( width = 2, sliderInput("CBCV", "Cambial cell LA:",
                                     min = 0, max = 100, step = 1,
                                     value = 0 ) ) ,  ## col T.2
      column( width = 2, sliderInput("CBWT", "Cambial cell WT:",
                                     min = 0, max = 1, step = 0.05,
                                     value = 0 ) ), ## col T.3
      column( width = 2, sliderInput("CBCTD", "Cambial CTD:",
                                     min = 0, max = 50, step = 0.5,
                                     value = 0 )  ), ## col T.4
      column( width = 2, actionButton("RefreshSlider", "Refresh sliders")  ), ## col T.5
      column( width = 2,  downloadButton("downloadData", "Download parameter list")  ), ## col T.6
    ), ## fluidRow end -
    ## pages
    tabsetPanel(
      tabPanel( "Read Datas", ## Tab input
                column(  width = 5 ,## Col 0
                         fileInput("fileParam", "Updata Parameter list", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         tags$hr(),
                         fileInput("fileFiber", "Updata observe data of fiber", multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,
                         tags$hr(),
                         fileInput("fileVessel", "Updata observe data of vessel" ,multiple = FALSE,
                                   accept = c(".xlsx", ".xls", ".csv")) ,

                ), ## Col 0.1 -
                column( width =  7,  ## Col 0.2
                        tableOutput("dParam"),
                        tags$hr(),
                        tableOutput("dFiber"),
                        tags$hr(),
                        tableOutput("dVessel")
                ) ## Col 0.2 -

      ), ## Tab input-


      tabPanel( "Sim Fiber Cell", ## tab fiber
                column( width = 4 ,## Col 1.1
                        sliderInput("LiF", "Fiber_gRLi:",min = 0, max = 5, step = 0.02, value = 0  ) ,
                        tags$hr(),
                        # uiOutput("slidersFiber")

                        fluidRow(
                          column(width = 6 ,
                                 lapply( c( "va_c.fiber", "va_w.fiber", "va_l.fiber", "CAmax","WAmax","WTmax")  , function(sips) {
                                   sliderInput( inputId = paste0("F_", sips), label = paste("Fiber_", sips),
                                                min = 0, max = 2 , value = 0, step = 0.1 )  }) ), ##col1
                          column(width = 6 ,
                                 lapply(c( 'WTmin', 'WTa', 'ml',  'sl',  'mw', 'sw'  ) , function(sips) {
                                   sliderInput( inputId = paste0("F_", sips), label = paste("Fiber_", sips),
                                                min = 0, max = 2 , value = 0, step = 0.1 )  })  ) )  ##col2

                ), ## Col 1.1

                column(  width = 8, ## Col 1.2 Res
                         fluidRow( plotOutput( 'RegFigFiber',height = "750px" )  ),
                         # fluidRow(  tableOutput("testTableF")),
                         # fluidRow(  tableOutput("testTableCB"))
                ), ## Col 1.2

      ), ## Tab Fiber -
      tabPanel( "Sim Vessel", ## tab Vessel
                column( width = 4 , ## Col 2.1
                        sliderInput("LiV", "Vessel_gRLi:",min = 0, max = 5, step = 0.02, value = 1 ) ,
                        tags$hr(),
                        # uiOutput("slidersVessel")
                        fluidRow(
                          column(width = 6 ,
                                 lapply(c( "va_c.vessel", "va_w.vessel", "va_l.vessel", "CAmax","WAmax","WTmax")   , function(sips) {
                                   sliderInput( inputId = paste0("V_", sips), label = paste("Vessel_", sips),
                                                min = 0 ,max = 2 ,value = 1,step = 0.1)  }) ), ##col1
                          column(width = 6 ,
                                 lapply(c( 'WTmin', 'WTa', 'ml',  'sl',  'mw', 'sw'  )  , function(sips) {
                                   sliderInput( inputId = paste0("V_", sips), label = paste("Vessel_", sips),
                                                min = 0, max = 2 , value = 1, step = 0.1  )   }) ) )

                ), ## Col 2.1
                column(  width = 8, ## Col 2.2 Res
                         fluidRow( plotOutput( 'RegFigVessel',height = "750px" )  ),
                ), ## Col 2.2

      ) ## Tab Fiber -
    ) ## tabsetPanel end -
  ) #### ui end -------
  #### UI for Sim Cells end --------
  #### Sim Cells server ####

  SimCellServer <- function( input, output, session ){
    ## 数据集
    dataInCell <- reactiveValues(   )
    # timer <- reactiveTimer( 2000 )

    ## 读取数据##
    output$dParam <- renderTable({  ## param
      req(input$fileParam)
      file <- input$fileParam$datapath
      ext <- tools::file_ext(file)  # 获取文件扩展名
      tryCatch(
        {         ifelse(ext == "csv",
                         df <- read.csv( file,  header = T ) ,
                         df <- openxlsx::read.xlsx( file  ) )  },
        error = function(e) { stop(safeError(e)) }
      )
      dataInCell$dtParam <- df ## Total Param

      dataInCell$CBParam <- dataInCell$dtParam[ dataInCell$dtParam$Module == 'InitialCambialCell'&
                                                  dataInCell$dtParam$Parameter %in% c('CV','CTD' ,'WT' ),
                                                c( 'Parameter', 'ParamType',  'Module','Values' )  ]
      dataInCell$FParam <- dataInCell$dtParam[ dataInCell$dtParam$Note == 'Fiber' &
                                                 dataInCell$dtParam$Module %!in% c( 'GrowthRate' ) ,
                                               c( 'Parameter', 'ParamType','Module', 'Values' )  ]

      dataInCell$VParam <- dataInCell$dtParam[ dataInCell$dtParam$Note == 'Vessel'&
                                                 dataInCell$dtParam$Module %!in% c( 'GrowthRate'  ) ,
                                               c( 'Parameter', 'ParamType','Module', 'Values' )  ]
      return( head(df,3) )
    }) ## output$dParam end ------

    return( head(df,3) )



    ## 点击 RefreshSlider 时更新滑动条UI
    observeEvent( input$RefreshSlider ,{
      dtp1 <- dataInCell$FParam

      lapply( dtp1$Parameter, function(sips) {
        updateSliderInput(session ,
                          inputId = paste0("F_", sips),
                          label = paste("Fiber_", sips),
                          min = min(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ),
                          max = max(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ) ,
                          value = dtp1$Values[dtp1$Parameter == sips],
                          step = round( dtp1$Values[dtp1$Parameter == sips]/ 25 , 3  ) )
      })

      dtp1 <- dataInCell$VParam

      lapply(dtp1$Parameter , function(sips) {
        updateSliderInput(session ,
                          inputId = paste0("V_", sips),
                          label = paste("Vessel_", sips),
                          min = min(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ),
                          max = max(round( dtp1$Values[dtp1$Parameter == sips] *0.5 ,3  ) ,
                                    round( dtp1$Values[dtp1$Parameter == sips] *1.5 ,3  ) ) ,
                          value = dtp1$Values[dtp1$Parameter == sips],
                          step = round( dtp1$Values[dtp1$Parameter == sips]/ 25 , 3  )  )
      })

      dtp1 <- dataInCell$CBParam
      updateSliderInput(session, "CBCV",
                        min = round(dtp1$Values[dtp1$Parameter == "CV"] *0.5,2 ),
                        max = round(dtp1$Values[dtp1$Parameter == "CV"] *1.5,2 ),
                        value =  round(dtp1$Values[dtp1$Parameter == "CV"],2 ),
                        step = round(dtp1$Values[dtp1$Parameter == "CV"]/ 25,2 ) )
      updateSliderInput(session, "CBWT",
                        min = round(dtp1$Values[dtp1$Parameter == "WT"] *0.5,2 ),
                        max = round(dtp1$Values[dtp1$Parameter == "WT"] *1.5,2 ),
                        value =  round(dtp1$Values[dtp1$Parameter == "WT"],2 ),
                        step = round(dtp1$Values[dtp1$Parameter == "WT"]/ 25,2 ) )
      updateSliderInput(session, "CBCTD",
                        min = round(dtp1$Values[dtp1$Parameter == "CTD"] *0.5,2 ),
                        max = round(dtp1$Values[dtp1$Parameter == "CTD"] *1.5,2 ),
                        value =  round(dtp1$Values[dtp1$Parameter == "CTD"],2 ),
                        step = round(dtp1$Values[dtp1$Parameter == "CTD"]/ 25,2 ) )
    }) ## CBparam end -



    # 使用debounce处理每个sliderInput
    slidersCB <- lapply(paste0 ('CB',c("CTD", "CV",  "WT") ) , function(id) {
      debounce(reactive(input[[id]]), 1500) } )

    slidersF <- lapply(paste0 ('F_',c("va_c.fiber","va_w.fiber", "va_l.fiber","CAmax","ml","sl",
                                      "mw","sw","WAmax","WTmax","WTmin","WTa"  ) ) , function(id) {
                                        debounce(reactive(input[[id]]), 1500) } )

    slidersV <- lapply(paste0 ('V_',c("va_c.vessel","va_w.vessel", "va_l.vessel","CAmax","ml","sl",
                                      "mw","sw","WAmax","WTmax","WTmin","WTa"  ) ) , function(id) {
                                        debounce(reactive(input[[id]]), 1500) } )


    # # 渲染汇总表格
    # output$testTableCB <- renderTable({
    #
    #     req( input$fileParam  )
    #     # timer()
    #     NewCB <- data.frame(Parameter = c("CTD", "CV",  "WT")  , NewValues = sapply(slidersCB, function(slider) slider()))
    #     NewF  <- data.frame('Parameter' = c("va_c.fiber","va_w.fiber", "va_l.fiber","CAmax","ml","sl",
    #                                         "mw","sw","WAmax","WTmax","WTmin","WTa"  ) ,
    #                         'NewValues' = sapply(slidersF, function(slider) slider())   )
    #
    #     dataInCell$CBParam <- NewReplacesOld( NewCB, dataInCell$CBParam, ons = c( 'Parameter' ) )
    #     dataInCell$FParam  <- NewReplacesOld( NewF , dataInCell$FParam , ons = c( 'Parameter' ) )
    #
    # return( dataInCell$FParam  )
    # })

    ### 渲染 Fiber 模拟图 ####
    output$RegFigFiber <- renderPlot({


      # timer()
      NewCB <- data.frame(Parameter = c("CTD", "CV",  "WT")  , Values = sapply(slidersCB, function(slider) slider()))
      NewF  <- data.frame('Parameter' = c("va_c.fiber","va_w.fiber", "va_l.fiber","CAmax","ml","sl",
                                          "mw","sw","WAmax","WTmax","WTmin","WTa"  ) ,
                          'Values' = sapply(slidersF, function(slider) slider())   )

      dataInCell$CBParam <- NewReplacesOld( NewCB, dataInCell$CBParam, ons = c( 'Parameter' ) )
      dataInCell$FParam  <- NewReplacesOld( NewF , dataInCell$FParam , ons = c( 'Parameter' ) )
      req( input$LiF  )
      ## 计算新细胞大小
      ResCells <- CalculateCells( CBparam = dataInCell$CBParam, Cparam = dataInCell$FParam ,
                                  gRLi = input$LiF , DroughtIndex = input$dry )

      ResCell2 <- ResCells |> dplyr::filter(DDOY != 0 ) |> dplyr::select('Dry','EL','CV','WT') |>
        dplyr::mutate( type = 'Sim')

      ResCells <- ResCells[,c('Dry','Day','EL','CA','CV','WA','LWA' )   ] |>
        tidyr::gather( key = "factor", value = 'Area', c(-1:-3))

      ifelse( any( names(dataInCell) == "dtFiber") ,
              ResCell2 <- dplyr::bind_rows(ResCell2, PartELcells( dataInCell$dtFiber  ) ),
              ResCell2 <- ResCell2  )

      ResCell2 <- tidyr::gather(ResCell2,  key = "factor", value = 'Value', c( 3,4) )


      PlotFiber <-
        ggplot2::ggplot( ResCells  ) +
        ggplot2::theme_bw()+
        # ggplot2::theme( legend.position = "top")+
        ggplot2::theme( text = ggplot2::element_text( size = 18 )  )+
        ggplot2::geom_line( ggplot2::aes( x = Day , y = Area, color =factor , alpha = as.factor(Dry) ))+#
        ggplot2::facet_wrap( EL~. ,scales = 'free'  )
      PlotComp <-
        ggplot2::ggplot( ResCell2[ResCell2$Value>=0,  ], ggplot2::aes( x = EL , y = Value, color = type ) )+
        ggplot2::labs( x = NULL , y = NULL )+
        ggplot2::theme_bw()+
        ggplot2::theme( text = ggplot2::element_text( size = 18 )  )+
        ggplot2::geom_boxplot()+
        ggplot2::facet_wrap( factor ~. ,scales = 'free')

      ResFig<- ggpubr::ggarrange( PlotFiber ,PlotComp , ncol = 1 ,align = 'v' )

      return( ResFig  )
    })

    ### 渲染 Vessel 模拟图 ####
    output$RegFigVessel <- renderPlot({


      # timer()
      NewCB <- data.frame(Parameter = c("CTD", "CV",  "WT")  , Values = sapply(slidersCB, function(slider) slider()))
      NewV  <- data.frame('Parameter' = c("va_c.vessel","va_w.vessel", "va_l.vessel","CAmax","ml","sl",
                                          "mw","sw","WAmax","WTmax","WTmin","WTa"  ) ,
                          'Values' = sapply(slidersV, function(slider) slider())   )

      dataInCell$CBParam <- NewReplacesOld( NewCB, dataInCell$CBParam, ons = c( 'Parameter' ) )
      dataInCell$VParam  <- NewReplacesOld( NewV , dataInCell$VParam , ons = c( 'Parameter' ) )
      req( input$LiV  )
      ## 计算新细胞大小
      ResCells <- CalculateCells( CBparam = dataInCell$CBParam, Cparam = dataInCell$VParam ,
                                  gRLi = input$LiV , DroughtIndex = input$dry )

      ResCell2 <- ResCells |> dplyr::filter(DDOY != 0 ) |> dplyr::select('Dry','EL','CV','WT') |>
        dplyr::mutate( type = 'Sim')

      ResCells <- ResCells[,c('Dry','Day','EL','CA','CV','WA','LWA' )   ] |>
        tidyr::gather( key = "factor", value = 'Area', c(-1:-3))

      ifelse( any( names(dataInCell) == "dtVessel") ,
              ResCell2 <- dplyr::bind_rows(ResCell2, PartELcells( dataInCell$dtVessel  ) ),
              ResCell2 <- ResCell2  )

      ResCell2 <- ResCell2[,c('Dry','EL','CV','type'   )]


      PlotVessel <-
        ggplot2::ggplot( ResCells  ) +
        ggplot2::theme_bw()+
        ggplot2::scale_y_sqrt()+
        ggplot2::theme( text = ggplot2::element_text( size = 18 )  )+
        ggplot2::geom_line( ggplot2::aes( x = Day , y = Area, color =factor , alpha = as.factor(Dry) ))+#
        ggplot2::facet_wrap( EL~. ,scales = 'free'  )
      PlotComp <-
        ggplot2::ggplot( ResCell2[ResCell2$CV >=0,  ], ggplot2::aes( x = EL , y = CV, color = type ) )+
        ggplot2::labs( x = NULL , y = 'Vessel lumen area' )+
        ggplot2::scale_y_sqrt()+
        ggplot2::theme( text = ggplot2::element_text( size = 18 )  )+
        ggplot2::theme_bw()+
        ggplot2::geom_boxplot()

      ResFig<- ggpubr::ggarrange( PlotVessel ,PlotComp , ncol = 1 )

      return( ResFig  )
    })


    # Downloadable csv of Params dataset ----
    output$downloadData <- downloadHandler(
      filename = 'Parameter.xlsx',
      content = function(file) {
        Ndt <- dplyr::bind_rows( dataInCell$CBParam,dataInCell$FParam,dataInCell$VParam,  )
        dataInCell$dtParam <- NewReplacesOld( Ndt, dataInCell$dtParam, ons = c('Parameter','ParamType','Module' ) )
        openxlsx::write.xlsx( dataInCell$dtParam , file)
      })



  }

  #### Sim Cells server end ----


  shinyApp(  SimCellUI, SimCellServer)

}




