#' Simulate Fiber & Vessel cells
#' @export
#'
#' @import shiny
#'
#' @importFrom dplyr select mutate rename bind_rows group_by distinct case_when summarise filter
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot theme_bw labs theme geom_point geom_line facet_wrap aes geom_boxplot
#' @importFrom openxlsx write.xlsx
#' @importFrom ggpubr ggarrange
#'
#' @param param BTRmodels parameters data
#' @param ObsV Vessel anatomical trait measured by ROXAS
#' @param ObsF Fiber anatomical trait measured by ROXAS
#'

# library(shiny)

Sim_Cells <- function(ObsV =NULL ,ObsF=NULL , param,...) {

  ## 整理细胞参数
  Obs <- rbind( dplyr::mutate( ObsV, type = "Vessel" ) ,dplyr::mutate( ObsF, type = "Fiber" )  )
  Obst <- Obs[  Obs$RRadDistR<= 20| Obs$RRadDistR >= 20 ,c('Year','RRadDistR','type','TID','LA',"CWTall" ) ]|>
    dplyr::mutate( EL = dplyr::case_when(RRadDistR<= 20~ "Ew", RRadDistR > 20 ~"Lw" ) ,
                   CWTall = dplyr::case_when(CWTall <=0 ~ NA , CWTall > 0 ~ CWTall ) ) |>
    dplyr::group_by(Year,EL,type,TID) |> dplyr::summarise(LA = mean(LA,na.rm = T),CWT = mean(CWTall,na.rm = T)  )


  ui <- fluidPage( ### ui ####
    titlePanel( "Simulate Fiber & Vessel Growth Parameters"),
              column( ## col 2.1 细胞初始参数 ####
                2,
                wellPanel( ## 灰色底 panel
                  h4("Photoperiod promotion rate"),
                  sliderInput("fgRLi",
                              "Fiber rate:",
                              min = 0,
                              max = 5,
                              step = 0.05,
                              value = 1 ), ## end of sliderinput
                  sliderInput("vgRLi",
                              "Vessel rate:",
                              min = 0,
                              max = 5,
                              step = 0.05,
                              value = 1 ), ## end of sliderinput
                  sliderInput("dry",
                              "Drought rate:",
                              min = 0,
                              max = 1,
                              step = 0.05,
                              value = 1 ), ## end of sliderinput
                  tags$hr(),
                  h4("Initial cambial cell size"),
                  numericInput("CV", "Lunen Area",param$values[param$parameter == "CV" & param$modul == "C0" ],
                               min = -Inf , max = Inf,step = 0.5 ), ##
                  numericInput("CWT", "Cell wall thick", param$values[param$parameter == "WT"& param$modul == "C0" ],
                               min = -Inf , max = Inf ,step = 0.1), ##
                  numericInput("CTD", "CTD", param$values[param$parameter == "CTD"& param$modul == "C0" ],
                               min = -Inf , max = Inf ,step = 0.2), ##
                  tags$hr(),
                  # Button
                  downloadButton("downloadParam", "Download Parameters")


                  ) ## wellPanel end
              ), ## col 2.1 end ----

              column( ## col 2.2 细胞初始参数 ####
                2,
                wellPanel( ## 灰色底 panel
                  h4( "Specific fiber growth rate" ),
                  numericInput("va_c.fiber", "Fiber enlargement rate",param$values[param$parameter == "va_c.fiber" & param$modul == "division" ],
                               min = -Inf , max = Inf ), ##
                  numericInput("va_w.fiber", "Fiberwall deposition rate", param$values[param$parameter == "va_w.fiber"& param$modul == "division" ],
                               min = -Inf , max = Inf ), ##
                  numericInput("va_l.fiber", "Fiber wall lignin rate", param$values[param$parameter == "va_l.fiber"& param$modul == "division" ],
                               min = -Inf , max = Inf ), ##
                  tags$hr(),
                  h4("Fiber生长参数"),
                  numericInput("fCAmax", "Max cell Area", param$values[param$parameter == "CAmax"& param$modul == "growthC" ],
                               min = 0 , max = Inf, step = 20  ), ##
                  numericInput("fml", "p1 of lignified (ml)", param$values[param$parameter == "ml"& param$modul == "growthC" ],
                               min = 0 , max = Inf , step = 10), ##
                  numericInput("fsl", "p2 of lignified (sl)", param$values[param$parameter == "sl" & param$modul == "growthC"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("fmw", "p1 of deposition (mw)", param$values[param$parameter == "mw"& param$modul == "growthC" ],
                               min = 0 , max = Inf , step = 10), ##
                  numericInput("fsw", "p2 of deposition (sw)", param$values[param$parameter == "sw" & param$modul == "growthC"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("fWAmax", "Max cell wall area", param$values[param$parameter == "WAmax" & param$modul == "growthC"],
                               min = 0 , max = Inf , step = 10), ##
                  numericInput("fWTmax", "Max cell wall thick", param$values[param$parameter == "WTmax"& param$modul == "growthC" ],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("fWTmin", "Min cell wall thick", param$values[param$parameter == "WTmin" & param$modul == "growthC"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("fWTa", "Threshold thickness of cell wall", param$values[param$parameter == "WTa" & param$modul == "growthC"],
                               min = 0 , max = Inf , step = 0.1), ##

                ) ## wellPanel end
              ), ## col 2.2 end ---

              column( ## col 2.3 细胞初始参数
                2,
                wellPanel( ## 灰色底 panel
                  h4( "Specific vessel growth rate" ),
                  numericInput("va_c.vessel", "Vessel enlargement rate",param$values[param$parameter == "va_c.vessel"& param$modul == "division" ],
                               min = -Inf , max = Inf ), ##
                  numericInput("va_w.vessel", "Vessel wall deposition rate", param$values[param$parameter == "va_w.vessel"& param$modul == "division" ],
                               min = -Inf , max = Inf ), ##
                  numericInput("va_l.vessel", "Vessel wall lignin rate", param$values[param$parameter == "va_l.vessel"& param$modul == "division" ],
                               min = -Inf , max = Inf ), ##
                  h4("Vessel生长参数"),
                  numericInput("vCAmax", "Max cell Area", param$values[param$parameter == "CAmax"& param$modul == "growthV" ],
                               min = 0 , max = Inf, step = 20  ), ##
                  numericInput("vml", "p1 of lignified (ml)", param$values[param$parameter == "ml" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 10), ##
                  numericInput("vsl", "p2 of lignified (sl)", param$values[param$parameter == "sl" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("vmw", "p1 of deposition (mw)", param$values[param$parameter == "mw" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 10), ##
                  numericInput("vsw", "p2 of deposition (sw)", param$values[param$parameter == "sw" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("vWAmax", "Max cell wall area", param$values[param$parameter == "WAmax" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 10), ##
                  numericInput("vWTmax", "Max cell wall thick", param$values[param$parameter == "WTmax" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("vWTmin", "Min cell wall thick", param$values[param$parameter == "WTmin" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 0.1), ##
                  numericInput("vWTa", "Threshold thickness of cell wall", param$values[param$parameter == "WTa" & param$modul == "growthV"],
                               min = 0 , max = Inf , step = 0.1) ##

                ) ## wellPanel end
              ), ## col 2.3 end -----
              # 输出部分：显示图表
            column( ## Tab 2 output
              6,
                # tableOutput("result"),
                plotOutput("RegFigCell",height = "600px"),
                # tableOutput("result")
                plotOutput("supplot",height = "500px")
              ) ## mainPanel end --

    # )  ##
  ) #### ui end -------

  server <- function(input, output) {




    Adata <- reactiveValues(
      param = data.frame(param),
      Obst = Obst,
      Finall = NULL
    )

    output$RegFigCell  <- renderPlot( {
      ## 更新数据 ####
      CRD <- (input$CV / (input$CTD - 2 * input$CWT )) + 2 * input$CWT
      CA <- input$CTD * CRD
      WA <- CA - input$CV

      ###
      param2 <- Adata$param

      NewP  <- data.frame(   parameter = c('CV' , 'WT','CTD','CRD',  'WA',  'CA' ,
                                            'CV' , 'WT','CTD','CRD',  'WA',  'CA' ,
                                            'va_c.fiber' ,'va_w.fiber', 'va_l.fiber',
                                            'va_c.vessel' ,'va_w.vessel', 'va_l.vessel',
                                            'CAmax' , 'ml' , 'sl' , 'mw', 'sw', 'WAmax', 'WTmax', 'WTmin', 'WTa',
                                            'CAmax' , 'ml' , 'sl' , 'mw', 'sw', 'WAmax', 'WTmax', 'WTmin', 'WTa'  ),
                              modul = c( rep('C0',6), rep('V0',6), rep('division',6),rep('growthC',9) ,rep('growthV',9) ),
                              Nvalues = c( input$CV , input$CWT,input$CTD,CRD ,WA, CA,
                                           input$CV , input$CWT,input$CTD,CRD ,WA, CA,
                                           input$va_c.fiber ,input$va_w.fiber, input$va_l.fiber,
                                           input$va_c.vessel ,input$va_w.vessel, input$va_l.vessel,
                                           input$fCAmax , input$fml , input$fsl , input$fmw, input$fsw,
                                           input$fWAmax, input$fWTmax, input$fWTmin, input$fWTa ,
                                           input$vCAmax , input$vml , input$vsl , input$vmw, input$vsw,
                                           input$vWAmax, input$vWTmax, input$vWTmin, input$vWTa )   )

      param2 <- AupdatedB(DataA = NewP,DataB = param2,ons = c('parameter','modul'))

      Adata$param <- param2

      ## 模拟细胞大小 ####

      ELwCell <- rbind(  CellGrwothData(param2,wgR = 1, dry = input$dry, fgRLi = 0,vgRLi = 0)|>
        dplyr::mutate(EL = "Earlywood") ,
                  CellGrwothData(param2,wgR = 0.2, dry = input$dry, fgRLi = input$fgRLi,vgRLi = input$vgRLi)|>
        dplyr::mutate(EL = "Latewood")   ) |> dplyr::select(Day,CA, CV,  DDOY,  EDOY,LWA , TDOY,WA,WT,type,EL) |>
        dplyr::distinct(CA, CV,  DDOY,  EDOY,LWA , TDOY,WA,WT,type,EL, .keep_all = TRUE)
      ## 计算早晚材细胞大小 Ew = RRadDistR[5,20] , Lw = RRadDistR[75,90] ####
        Obs <- Adata$Obst


      simCells <- ELwCell|> dplyr::group_by( type,EL ) |>
        dplyr::filter(Day == max(Day)) |>
        dplyr::select(CV,WT,type,EL ) |>
        dplyr::rename(  LA = CV , CWT = WT)|>
        tidyr::gather(key,val,c(LA, CWT))

      simCells$val[ simCells$type == "Vessel" & simCells$key == "CWT" ] <- NA

      Adata$Finall <- NewP

      # 计算结果画图 ####
      ggpubr::ggarrange(
      ELwCell |>
        tidyr::gather(key ,val , c( -Day,-type,-EL )) |>
        dplyr::filter(key %in% c( 'CA','CV','WA','LWA' )  ) |>
      ggplot2::ggplot(    )+
        ggplot2::theme_bw()+
        ggplot2::theme(legend.position = "top")+
        ggplot2::labs(title = "Cell Growth", x = 'Day', y = 'cellsize')+
        ggplot2::geom_line( ggplot2::aes(x = Day ,y = val,color = key ) )+
        ggplot2::facet_wrap( EL ~ type,scale="free")
      ,
      Obs |> tidyr::gather(key ,val , c( LA  ,  CWT )) |> na.omit() |>
        dplyr::mutate(EL =  factor(EL,levels = c("Ew",'Lw'),labels = c('Earlywood',"Latewood"))) |>
        ggplot2::ggplot()+
        ggplot2::theme_bw()+
        ggplot2::labs(title = "Cell Size", x = 'ELw', y = 'cell size')+
        ggplot2::theme(legend.position = "bottom")+
        # ggplot2::geom_point( ggplot2::aes(x = EL , y = val ,color = "Obs" ,alpha = Year ,shape = EL ),position = 'jitter')+
        ggplot2::geom_boxplot( ggplot2::aes(x = EL , y = val ,color = "Obs"  ,shape = EL ))+
        ggplot2::geom_point( ggplot2::aes(x = EL , y = val ,color =  "Sim"  ,shape = EL ),
                             data = na.omit(simCells),size = 3)+

        ggplot2::facet_wrap(type   ~ key ,scale="free")
      ,ncol = 1 ,heights = c(1,0.6)

      ) ## ggarrange end

    }) # RegFigTab2 (Fiber) end

    output$supplot <-  renderPlot({

      lineF <- supline(vc =input$va_c.fiber ,vw = input$va_w.fiber, vl =  input$va_l.fiber,
                       ml = input$fml , sl = input$fsl,mw = input$fmw , sw = input$fsw,  CAmax = input$fCAmax )
      lineV <- supline(input$va_c.vessel ,input$va_w.vessel, input$va_l.vessel,
                       input$vml , input$vsl,input$vmw , input$vsw,  input$vCAmax )

      lineFs <- supline(vc =1 ,vw = 1, vl =  1,
                       ml = input$fml , sl = input$fsl,mw = input$fmw , sw = input$fsw,  CAmax = input$fCAmax,S =T )
      lineVs <- supline(1 ,1, 1,
                       input$vml , input$vsl,input$vmw , input$vsw,  input$vCAmax,S =T )


      ggpubr::ggarrange(
        ggplot2::ggplot(lineF)+
          ggplot2::labs(title = "Fiber")+
          ggplot2::geom_line( ggplot2::aes(x = x , y = val,color = key  ))
        ,
        ggplot2::ggplot( lineV )+
          ggplot2::scale_y_sqrt()+
          ggplot2::scale_x_sqrt()+
          ggplot2::labs(title = "Vessel")+
          ggplot2::geom_line( ggplot2::aes(x = x , y = val,color = key  ))
        ,
        ggplot2::ggplot(lineFs)+
          ggplot2::labs(title = "sFiber")+
          ggplot2::geom_line( ggplot2::aes(x = x , y = val,color = key  ))
        ,
        ggplot2::ggplot( lineVs)+
          ggplot2::scale_x_sqrt()+
          ggplot2::labs(title = "sVessel")+
          ggplot2::geom_line( ggplot2::aes(x = x , y = val,color = key  ))
        # ,ncol = 2
        ,common.legend = T, legend = "top"
      ) ## ggarrange end
    }) # plot sup end ---

    # Downloadable csv of selected dataset ----
    output$downloadParam <- downloadHandler(
      filename = "Parameters.xlsx",
      content = function(file) {
        openxlsx::write.xlsx( list( parameter =  Adata$param ,
                         LiLimt = data.frame( Vessel = input$vgRLi , Fiber = input$fgRLi   )) , file )
      }
    )



    # # 观察textInput的变化，并更新数据框
    # observe({
    #   req(input$fCV)  # 确保输入值存在
    #   data$param$values[param$parameter == "CV" & param$modul == "C0" ] <- input$fCV
    # })
    #
    # # 显示更新后的数据框
    # output$result <- renderTable({
    #   Adata$param
    # })
    # output$result <- renderTable({
    #   Adata$Finall
    # })

  }

  supline <- function( vc,vw,vl, ml,sl,mw,sw, CAmax,S =F   ){

    ifelse(S == T , CA <- 1 ,CA <- 1:CAmax  )
    y = data.frame(x = c(1:CAmax),
                   Fe =  vc*  CA * ( 1- 1:CAmax/CAmax),
                   Fw = vw * (1 - ( 1/(1+ 1:CAmax/mw  )^sw )),
                   Fl = vl * (1 - ( 1/(1+ 1:CAmax/ml  )^sl ))
                   ) |> tidyr::gather(key,val,c(-x))
    return(y)
  }

  shinyApp(ui, server)

}
# library(shiny)

Sim_Cells(ObsV = ObsV, ObsF = ObsF,  param = param)




