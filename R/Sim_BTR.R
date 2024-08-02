#' Sim BTR
#'
#' @export
#'
#' @import shiny
#'
#' @param Tage trend of age
## #' @param sfRW  Standardization filtered RW
#' @param param parameters
#' @param clims climate
#' @param syear start year
#' @param eyear end year
#' @param ObsF Observe Fiber
#' @param ObsV Observe vessel
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot theme_bw labs theme geom_point geom_line aes scale_color_manual scale_x_continuous
#' @importFrom rBTRdev mod.test
#' @importFrom ggpubr ggarrange
#' @importFrom openxlsx write.xlsx
#'
#' @import shiny
#'
# library(shiny)
sim_btr <- function( param, clims = NA, Tage =NA, ObsF =NA , ObsV =NA ){

  # StartY <- max( min(Tage$Year), min(clims$Year)  )
  # EndY <- min( max(Tage$Year), max(clims$Year) )
  #
  # Tage <- Tage[Tage$Year %in% c(StartY:EndY ),]
  # clims <- clims[clims$Year %in% c(StartY:EndY ),]
  #
  # obsF <- obsF[obsF$Year %in% c(StartY:EndY ),]
  # obsV <- obsF[obsF$Year %in% c(StartY:EndY ),]

  ui <- fluidPage( ## ui ####
     titlePanel( "Simulate BTR model"),
     column( ## col 1
       width = 2,
       h4( 'Cambial parameters:' ),
       h4("Vessel division"),
       numericInput("deltaD", "deltaD",param$values[param$parameter == "deltaD"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("a1", "a1",param$values[param$parameter == "a1"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("a2", "a2",param$values[param$parameter == "a2"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("Div_alpha", "Div_alpha",param$values[param$parameter == "Div_alpha"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("maxRCTA", "maxRCTA",param$values[param$parameter == "maxRCTA"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("RCTADivT", "RCTADivT",param$values[param$parameter == "RCTADivT"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       h4("Photoperiod 调整"),
       numericInput("MaxLi.fiber", "Fiber dLi ",0,
                    min = 0 , max = 5,step = 0.1 ), ##
       sliderInput("MaxLiDoy",
                   "The end DOY of early wood fiber:",
                   min = 100,
                   max = 200,
                   step = 1,
                   value = 176 ), ## end of sliderinput
       sliderInput("MinLiDoy",
                   "The start DOY of late wood fiber:",
                   min = 150,
                   max = 320,
                   step = 1,
                   value = 200 ), ## end of sliderinput
       tags$hr(),
       numericInput("MaxLi.vessel", "Vessel dLi",0,
                    min = 0 , max = 5,step = 0.1 ), ##
       sliderInput("MaxLiDoy",
                   "The end DOY of early wood vessel:",
                   min = 100,
                   max = 200,
                   step = 1,
                   value = 176 ), ## end of sliderinput
       sliderInput("MinLiDoy",
                   "The start DOY of late wood vessel:",
                   min = 150,
                   max = 320,
                   step = 1,
                   value = 200 ), ## end of sliderinput
       tags$hr(),
       sliderInput("Cores",
                   "Use CPU cores:",
                   min = 1,
                   max = 32,
                   step = 1,
                   value = 5 ),
       tags$hr(),
       actionButton("Sim", "Run BTR model")

     ), # end c1
     column( ## col 2
       width = 2,
       numericInput("syear", "Start year",2000,min = 1800 , max = Inf , step = 1), ##
       numericInput("eyear", "End year",2001,min = 1800 , max = Inf , step = 1), ##

       h4( 'Climate parameters:' ),
       numericInput("AAT", "Accumulated temperature",param$values[param$parameter == 'AAT'  ],
                    min = 0 , max = Inf , step = 0.5), ##
       numericInput("T1", "Min cambial activaty temperature",param$values[param$parameter == 'T1'  ],
                    min = 0 , max = 20 , step = 0.1), ##
       numericInput("T4", "Max cambial activaty temperature",param$values[param$parameter == 'T4'  ],
                    min = 0 , max = 50 , step = 0.1), ##
       tags$hr(),
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
       # tags$hr(),
       # Button
       # actionButton("Sim", "Run BTR model"),
       tags$hr(),
       downloadButton("downloadData", "Download Trend_age")
     ),# end c2

     column(
       width = 8,
       plotOutput("FigSim"),

     )


  )## ui end -----

  server <- function(input, output) {
    SimData <- reactiveValues(
      param = as.data.frame(param),
      clims = as.data.frame(clims),
      Tage = as.data.frame(Tage),
      ObsF = as.data.frame(ObsF),
      ObsV = as.data.frame(ObsV)

    )

    ResData <- reactiveValues(
      Sim = NA
    )

    output$FigSim <- renderPlot( {

      StartY <- max( min(Tage$Year), min(clims$Year) ,input$syear )
      EndY <- min( max(Tage$Year), max(clims$Year), input$eyear )
      Tage <- Tage[Tage$Year %in% c(StartY:EndY ),]
      clims <- clims[clims$Year %in% c(StartY:EndY ),]
      obsF <- obsF[obsF$Year %in% c(StartY:EndY ),]
      obsV <- obsF[obsF$Year %in% c(StartY:EndY ),]

      ## 修改参数
      # clims <- SimData$clims
      param2 <- SimData$param
      NewP  <- data.frame(   parameter = c('AAT', "T1", "T4", "deltaH_A_Da","deltaH_D","deltaS_D",
                                           'M1','M2','M3','M4','VPD1' ,'VPD2','VPD3','VPD4'),
                             Nvalues = c( input$AAT , input$T1, input$T4  , input$deltaH_A_Da,input$deltaH_D,input$deltaS_D,
                                          input$M1 , input$M2,input$M3,input$M4 ,
                                          input$VPD1 , input$VPD2,input$VPD3,input$VPD4 )   )

      param2 <- AupdatedB(DataA = NewP,DataB = param2,ons = 'parameter' )
      SimData$param <- param2

      LgM <- data.frame( SoilM = c(input$M1,input$M2,input$M3,input$M4 ) ,gM = c(0,1,1,0)  )
      LgV <- data.frame( VPD = c(input$VPD1,input$VPD2,input$VPD3,input$VPD4 ) ,gV = c(0,1,1,0)  )

      ## 气象限制图 和 Li 限制图
      ggpubr::ggarrange(
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




    }) ## Fig Sim end

    observeEvent(input$Sim, { ## 点击Run BTR
      # ResData$result <-  rBTRdev::btr_parallel( clim = SimData$clims ,parameters = SimData$param , age = SimData$Tage,
      #                          syear = input$syear  ,eyear = input$eyear , Cores = input$Cores, writeRes = F)

      # ResData$annaulRing <- ResSim$annaulRing
      # ResData$xylem_trait <- ResSim$xylem_trait
      # ResData$IntraAnnual <- ResSim$IntraAnnual
      # ResData$microclim <- ResSim$microclim
      # ResData$dailyParameters <- ResSim$dailyParameters



    })


  }

  shinyApp(ui, server)


}

sim_btr(param = param)
