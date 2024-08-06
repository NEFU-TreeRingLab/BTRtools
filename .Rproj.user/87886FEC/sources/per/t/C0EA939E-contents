#' Sim gRs
#' use summary gR&gE and Standardization filtered RW sim threshold of clims
#'
#' @export
#'
#' @import shiny
#'
#' @param Tage trend of age
## #' @param sfRW  Standardization filtered RW
#' @param param parameters
#' @param clims climate
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot theme_bw labs theme geom_point geom_line aes scale_color_manual scale_x_continuous
#' @importFrom rBTRdev mod.test
#' @importFrom ggpubr ggarrange
#' @importFrom openxlsx write.xlsx
#'

## library(shiny)

Sim_gRs <- function(param,Tage,clims  ){ ## sfRW

  StartY <- max( min(Tage$Year), min(clims$Year)  )
  EndY <- min( max(Tage$Year), max(clims$Year) )

  Tage <- Tage[Tage$Year %in% c(StartY:EndY ),]
  clims <- clims[clims$Year %in% c(StartY:EndY ),]

  ui <- fluidPage( ## ui ####
    titlePanel("Simulate gRs"),
    column(width = 2,  ## c1
      h5("Climate limit parameters"),
      numericInput("AAT", "Accumulated temperature",param$values[param$parameter == 'AAT'  ],
                   min = 0 , max = Inf , step = 0.5), ##
      numericInput("T1", "Min cambial activaty temperature",param$values[param$parameter == 'T1'  ],
                   min = 0 , max = 20 , step = 0.1), ##
      numericInput("T4", "Max cambial activaty temperature",param$values[param$parameter == 'T4'  ],
                   min = 0 , max = 50 , step = 0.1), ##
      numericInput("deltaH_A_Da", "enthalpy of activation", param$values[param$parameter == 'deltaH_A_Da'  ] ,
                   min = 0 , max = Inf, step = 1  ), ##
      numericInput("deltaH_D", "ΔHd", param$values[param$parameter == 'deltaH_D'  ],
                   min = 0 , max = Inf , step = 1), ##
      numericInput("deltaS_D", "ΔSd", param$values[param$parameter == 'deltaS_D'  ],
                   min = 0 , max = Inf , step = 1), ##
      tags$hr(),
      # Button
      downloadButton("downloadParam", "Download Parameters")

      ),
    column( width = 2, ##  c2
            sliderInput("M1", "M1(v/v):", min = 0, max = 1, step = 0.01,
                        value = param$values[param$parameter == 'M1'  ] ), ## end of sliderinput
            sliderInput("M2", "M2(v/v):", min = 0, max = 1, step = 0.01,
                        value = param$values[param$parameter == 'M2'  ] ), ## end of sliderinput
            sliderInput("M3", "M3(v/v):", min = 0, max = 1, step = 0.01,
                        value = param$values[param$parameter == 'M3'  ] ), ## end of sliderinput
            sliderInput("M4", "M4(v/v):", min = 0, max = 1, step = 0.01,
                        value = param$values[param$parameter == 'M4'  ] ), ## end of sliderinput
            tags$hr(),

            sliderInput("VPD1", "VPD1(hPa):", min = -0.1, max = 0.5, step = 0.01,
                        value = param$values[param$parameter == 'VPD1'  ] ), ## end of sliderinput
            sliderInput("VPD2", "VPD2(hPa):", min = 0, max = 2, step = 0.01,
                        value = param$values[param$parameter == 'VPD2'  ] ), ## end of sliderinput
            sliderInput("VPD3", "VPD3(hPa):", min =  0, max = 4, step = 0.01,
                        value = param$values[param$parameter == 'VPD3'  ] ), ## end of sliderinput
            sliderInput("VPD4", "VPD4(hPa):", min =  0, max = 5, step = 0.01,
                        value = param$values[param$parameter == 'VPD4'  ]), ## end of sliderinput
    ),
    column( width = 8, ## output col
            # tableOutput("result"),
            plotOutput("RegFigGrs",height = "800px")

    ) ##

  ) ## end fluidPage -----

  server <- function(input, output ) {
    # observeEvent(input$M1, {
    #   updateSliderInput(session, "M2", min = input$M1)
    # })
    # observeEvent(input$M2, {
    #   updateSliderInput(session, "M3", min = input$M2)
    # })
    # observeEvent(input$M3, {
    #   updateSliderInput(session, "M4", min = input$M3)
    # })
    #
    # observeEvent(input$VPD1, {
    #   updateSliderInput(session, "VPD2", min = input$VPD1)
    # })
    # observeEvent(input$VPD2, {
    #   updateSliderInput(session, "VPD3", min = input$VPD2)
    # })
    # observeEvent(input$VPD3, {
    #   updateSliderInput(session, "VPD4", min = input$VPD3)
    # })



    Bdata <- reactiveValues(
      param = data.frame(param),
      clims = data.frame(clims),
      Tage = data.frame(Tage)
    )


    output$RegFigGrs  <- renderPlot( {
      ## 更新数据 ####
      clims <- Bdata$clims
      param2 <- Bdata$param
      NewP  <- data.frame(   parameter = c('AAT', "T1", "T4", "deltaH_A_Da","deltaH_D","deltaS_D",
                                           'M1','M2','M3','M4','VPD1' ,'VPD2','VPD3','VPD4'),
                             Nvalues = c( input$AAT , input$T1, input$T4  , input$deltaH_A_Da,input$deltaH_D,input$deltaS_D,
                                          input$M1 , input$M2,input$M3,input$M4 ,
                                          input$VPD1 , input$VPD2,input$VPD3,input$VPD4 )   )

      param2 <- AupdatedB(DataA = NewP,DataB = param2,ons = 'parameter' )
      Bdata$param <- param2

      ## line grs
      Ta <- 0:40 + 273.15
      R <- 8.314
      LgT <- ( Ta * exp( - param2$values[ param2$parameter == 'deltaH_A_Da' ] / ( R * Ta) ) /
                 (1 + exp( param2$values[ param2$parameter == 'deltaS_D' ] /
                             R - param2$values[ param2$parameter == 'deltaH_D' ] /(R*Ta) ) ) ) |>
        nors()
      LgT <- data.frame(TEM = 0:40 ,gT = LgT )

      LgT$LgT[LgT$TEM <= param2$values[ param2$parameter == 'T1' ]|
                LgT$TEM >= param2$values[ param2$parameter == 'T4' ] ] <- NA

      LgM<- data.frame( SoilM = c(input$M1,input$M2,input$M3,input$M4 ) ,gM = c(0,1,1,0)  )
      LgV<- data.frame( VPD = c(input$VPD1,input$VPD2,input$VPD3,input$VPD4 ) ,gV = c(0,1,1,0)  )

      ## 计算gRs

      gRs <- ComputeGrs( clims , param2 )

     gRs$Years <- dplyr::left_join(gRs$Years, Tage)

     cors <- rBTRdev::mod.test(gRs$Years$sFrw,gRs$Years$sgR)

     p4 <- ggpubr::ggarrange(
     ggplot2::ggplot(gRs$DOYs)+
       ggplot2::theme_bw()+
       ggplot2::labs(subtitle = "gR" )+
       ggplot2::scale_color_manual( name = "",values = c("royalblue","red","orange"))+
       ggplot2::geom_line( ggplot2::aes(x = DOY, y = val, color = gR ))
     ,
     ggplot2::ggplot(LgT)+
       ggplot2::theme_bw()+
       ggplot2::labs(subtitle = "gT" )+
       # ggplot2::scale_color_manual(values = c("royalblue","red","orange"))+
       ggplot2::geom_line( ggplot2::aes(x = TEM, y = gT ))
     ,
     ggplot2::ggplot(LgM)+
       ggplot2::theme_bw()+
       ggplot2::labs(subtitle = "gM" )+
       # ggplot2::scale_color_manual(values = c("royalblue","red","orange"))+
       ggplot2::geom_line( ggplot2::aes(x = SoilM, y = gM ))
     ,
     ggplot2::ggplot(LgV)+
       ggplot2::theme_bw()+
       ggplot2::labs(subtitle = "gVPD" )+
       # ggplot2::scale_color_manual(values = c("royalblue","red","orange"))+
       ggplot2::geom_line( ggplot2::aes(x = VPD, y = gV ))
     ,nrow = 1 #, align = "v"
     )

      ggpubr::ggarrange(
       ggplot2::ggplot(gRs$Years) + ##
         ggplot2::labs(subtitle = paste("r =",round(cors$COR,3)) )+
         ggplot2::theme_bw()+
         ggplot2::scale_x_continuous(limits = c(StartY,EndY))+
         ggplot2::geom_line(  ggplot2::aes(x = Year , y = sFrw,color = "sRW" ),linewidth = 0.3)+
         ggplot2::geom_line(  ggplot2::aes(x = Year , y = sgR,color = "sgR" ), linewidth = 0.5)+
         ggplot2::geom_point(  ggplot2::aes(x = Year , y = sgR,color = "sgR" ), size = 1.3)
       ,
       gRs$Years[,c(1,7:9)] |> tidyr::gather( key,val,-1 ) |> ##
         ggplot2::ggplot( )+
         ggplot2::theme_bw()+
         ggplot2::scale_x_continuous(limits = c(StartY,EndY))+
         ggplot2::geom_col( ggplot2::aes(x = Year, y = val , fill = key ),position = 'fill' )
       ,p4
      ,ncol = 1, align = "hv"
        )




    }) ## RegFigGrs end

    output$downloadParam <- downloadHandler(
      filename = "Parameters.xlsx",
      content = function(file) {
        openxlsx::write.xlsx( Bdata$param , file )
      }
    )

    # gRs <-
    # output$result <- renderTable({
    #   Bdata$param
    # })



  } # end server -----

  shinyApp(ui, server)

}## end fun

# Sim_gRs( param,Tage, clims )



