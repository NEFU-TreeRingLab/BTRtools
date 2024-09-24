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
#' @param Obsline 取代ObsF V
#'
#' @importFrom dplyr left_join
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot theme_bw labs theme geom_point geom_line aes scale_color_manual scale_x_continuous
#' @importFrom rBTR mod.test
#' @importFrom ggpubr ggarrange
#' @importFrom openxlsx write.xlsx
#' @importFrom mgcv gam
#'
#' @import shiny
#'
# library(shiny)
sim_btr <- function( param, clims = NA, Tage =NA, ObsF =NA , ObsV =NA, Obsline = NA ){

  # StartY <- max( min(Tage$Year), min(clims$Year)  )
  # EndY <- min( max(Tage$Year), max(clims$Year) )
  #
  # Tage <- Tage[Tage$Year %in% c(StartY:EndY ),]
  # clims <- clims[clims$Year %in% c(StartY:EndY ),]
  #
  # obsF <- obsF[obsF$Year %in% c(StartY:EndY ),]
  # obsV <- obsF[obsF$Year %in% c(StartY:EndY ),]
  ifelse( all(is.na(Obsline)) ,
      CLines <- CellLine(ObsV = ObsV,ObsF = ObsF ),
      CLines <- Obsline
  )

  ui <- fluidPage( ## ui ####
     titlePanel( "Simulate BTR model"),
     column( ## col 1 ####
       width = 1,


       numericInput("syear", "Start year",2010,min = 1800 , max = Inf , step = 1), ##
       numericInput("eyear", "End year",2020,min = 1800 , max = Inf , step = 1), ##
       h4( 'Cambial parameters:' ),
       numericInput("va_cz", "V_cz",param$values[param$parameter == "va_cz"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("alpha_cz", "α_cz: st_gR",param$values[param$parameter == "alpha_cz"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("beta_cz", "β_cz: v_gR",param$values[param$parameter == "beta_cz"],
                    min = -Inf , max = Inf,step = 0.1 ), ##

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
       numericInput("RCTADivT", "RCTADivT minVF/maxVF",param$values[param$parameter == "RCTADivT"],
                    min = -Inf , max = Inf,step = 0.1 ), ##
       numericInput("LAtoDiv", "Trend LA to dD",param$values[param$parameter == "LAtoDiv"],
                    min = -Inf , max = Inf,step = 0.1 ), ##


       h4( 'Climate parameters:' ),
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



       # tags$hr(),
       # Button
       # actionButton("Sim", "Run BTR model"),
       tags$hr(),
       downloadButton("downloadParam", "Download Param"),

       tags$hr(),
       downloadButton("downloadResult", "Download Result")


     ), # end c1 -----

     # column( width = 2, ## C2 滑动条为主  ####
     #
     # ), # end c2 -----

     # column(width = 1 , ### c3.1#####
     #
     #
     # ), # c3.1 end -----


     column( ## col 3.2 ####
       width = 2,

       tags$hr(),
       h4( 'Climate parameters:' ),
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
       tags$hr(),
       h4("Photoperiod 调整"),
       checkboxInput( "ifRegLi","Sim Li?",value = T   ),
       numericInput("MaxLi.fiber", "Fiber dLi ",NA,
                    min = 0 , max = 5,step = 0.05  ), ##
       sliderInput("EwLiDoyF",
                   "The end DOY of early wood fiber:",
                   min = 60,
                   max = 240,
                   step = 1,
                   value = 60 ), ## end of sliderinput
       sliderInput("LwLiDoyF",
                   "The start DOY of late wood fiber:",
                   min = 120,
                   max = 270,
                   step = 1,
                   value = 270 ), ## end of sliderinput
       tags$hr(),
       numericInput("MaxLi.vessel", "Vessel dLi",NA,
                    min = 0 , max = 5,step = 0.05 ), ##
       sliderInput("EwLiDoyV",
                   "The end DOY of early wood vessel:",
                   min = 60,
                   max = 240,
                   step = 1,
                   value = 60 ), ## end of sliderinput
       sliderInput("LwLiDoyV",
                   "The start DOY of late wood vessel:",
                   min = 120,
                   max = 270,
                   step = 1,
                   value = 270 ), ## end of sliderinput
       tags$hr(),
       actionButton("SaveParam", "Save this parameters"),
       actionButton("ReturenParam", "Returen old parameters"),

       tags$hr(),
       sliderInput("Cores",
                   "Use CPU cores:",
                   min = 1,
                   max = 32,
                   step = 1,
                   value = 12 ),


       tags$hr(),
       actionButton("Sim", "Run BTR model"),

     ),# end c3.2 ----



     column(  #col output 1 ####
       width = 4,


       fluidRow(
        plotOutput("RegFigGrs",height = "700px"),
        plotOutput('RegFigDiv',height = "200px"),
        plotOutput("RegFigLis",height = "500px"),
       ),
       fluidRow(
        plotOutput("RegFigSimLi",height = "200px"),
        tableOutput("NOparamLi")
       ),
     ), # col output1 end ####
     column(  #col output 2 ####
              width = 4,
              tableOutput("TestRW"),
              plotOutput("PlotRW"),
              plotOutput("TestAR",height = "750px"),
     ), # col output2 end ####


  )## ui end -----

  server <- function(input, output, session) {

    if (all( is.na( ObsF)  )  ) {
      ObsF = ObsV
    }

    SimData <- reactiveValues(
      param = as.data.frame(param),
      Oldparam = as.data.frame(param),
      clims = as.data.frame(clims),
      Tage = as.data.frame(Tage),
      ObsF = as.data.frame(ObsF),
      ObsV = as.data.frame(ObsV),
      CLines = CLines
    )

    ResData <- reactiveValues( )

    output$RegFigGrs  <- renderPlot( { ## RegFigGrs ####

      StartY <- max( min(Tage$Year), min(clims$Year) ,input$syear )
      EndY <- min( max(Tage$Year), max(clims$Year)  ,input$eyear )

      ## 更新数据
      clims <- SimData$clims
      param2 <- SimData$param
      NewP  <- data.frame(   parameter = c('AAT', "T1", "T4", "deltaH_A_Da","deltaH_D","deltaS_D",
                                           'M1','M2','M3','M4','VPD1' ,'VPD2','VPD3','VPD4'),
                             Nvalues = c( input$AAT , input$T1, input$T4  , input$deltaH_A_Da,input$deltaH_D,input$deltaS_D,
                                          input$M1 , input$M2,input$M3,input$M4 ,
                                          input$VPD1 , input$VPD2,input$VPD3,input$VPD4 )   )

      param2 <- AupdatedB(DataA = NewP,DataB = param2,ons = 'parameter' )
      SimData$param <- param2

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

      cors <- rBTR::mod.test(gRs$Years$sFrw,gRs$Years$sgR)

      p4 <- ggpubr::ggarrange(
        ggplot2::ggplot(gRs$DOYs)+
          ggplot2::theme_bw()+
          # ggplot2::theme(legend.position = "top")+
          # ggplot2::labs(subtitle = "gR" )+
          ggplot2::scale_color_manual( name = "",values = c("royalblue","red","orange"))+
          ggplot2::geom_line( ggplot2::aes(x = DOY, y = val, color = gR ))
        ,
        ggplot2::ggplot(LgT)+
          ggplot2::theme_bw()+
          # ggplot2::labs(subtitle = "gT" )+
          # ggplot2::scale_color_manual(values = c("royalblue","red","orange"))+
          ggplot2::geom_line( ggplot2::aes(x = TEM, y = gT ))
        ,nrow = 1 ,widths = c(2,1)#, align = "v"
      )

      p5 <- ggpubr::ggarrange(
        ggplot2::ggplot(LgM)+
          ggplot2::theme_bw()+
          # ggplot2::labs(subtitle = "gM" )+
          # ggplot2::scale_color_manual(values = c("royalblue","red","orange"))+
          ggplot2::geom_line( ggplot2::aes(x = SoilM, y = gM ))
        ,
        ggplot2::ggplot(LgV)+
          ggplot2::theme_bw()+
          # ggplot2::labs(subtitle = "gVPD" )+
          # ggplot2::scale_color_manual(values = c("royalblue","red","orange"))+
          ggplot2::geom_line( ggplot2::aes(x = VPD, y = gV ))
        ,nrow = 1 #, align = "v"
      )

      ggpubr::ggarrange(
        ggplot2::ggplot(gRs$Years)+ ##
          ggplot2::labs(subtitle =  paste("r =",round(cors$COR,3)) )+ ##
          ggplot2::theme_bw()+
          ggplot2::scale_x_continuous(limits = c(StartY-1,EndY+1))+
          ggplot2::geom_line(  ggplot2::aes(x = Year , y = sFrw,color = "sRW" ),linewidth = 0.3)+
          ggplot2::geom_line(  ggplot2::aes(x = Year , y = sgR,color = "sgR" ), linewidth = 0.5)+
          ggplot2::geom_point(  ggplot2::aes(x = Year , y = sgR,color = "sgR" ), size = 1.3)
        ,
        gRs$Years[,c(1,7:9)] |> tidyr::gather( key,val,-1 ) |> ##
          ggplot2::ggplot( )+
          ggplot2::theme_bw()+
          ggplot2::scale_x_continuous(limits = c(StartY-1,EndY+1))+
          ggplot2::geom_col( ggplot2::aes(x = Year, y = val , fill = key ),position = 'fill' )
        ,p4, p5
        ,ncol = 1, align = "hv",heights = c(1,1,1.4,1)
      )




    }) ## RegFigGrs end

    output$RegFigDiv <- renderPlot( { ## RegFigDiv ####
      ## 更新数据
      param2 <- SimData$param
      NewP  <- data.frame(   parameter = c( "va_cz", "alpha_cz", "beta_cz",
                                            'deltaD', "a1", "a2", "Div_alpha","maxRCTA","RCTADivT",'LAtoDiv'),
                             Nvalues = c( input$va_cz,input$alpha_cz,input$beta_cz,input$deltaD,input$a1,input$a2,
                                          input$Div_alpha,input$maxRCTA,input$RCTADivT,input$LAtoDiv)   )

      param2 <- AupdatedB(DataA = NewP,DataB = param2,ons = 'parameter' )
      SimData$param <- param2

      ResCdiv <- C_Div( param2 )
      ggpubr::ggarrange(
        ggplot2::ggplot( ResCdiv$ttRCTA )+

          ggplot2::geom_point( ggplot2::aes(x = RCTAt, y = dD ,color = EL))+
          ggplot2::geom_hline( yintercept = SimData$param$values[ SimData$param$parameter == "deltaD" ] )+
          ggplot2::geom_vline( xintercept = SimData$param$values[ SimData$param$parameter == "maxRCTA" ] )+
          ggplot2::geom_vline( xintercept = SimData$param$values[ SimData$param$parameter == "maxRCTA" ] *
                                 SimData$param$values[ SimData$param$parameter == "RCTADivT"  ] )
        ,
        ggplot2::ggplot( ResCdiv$ttczgR )+
          ggplot2::geom_line( ggplot2::aes( x = gR, y = czgR ,color = "czgR" ) )+
          ggplot2::geom_line( ggplot2::aes( x = gR, y = Vcz ,color = "Vcz" ) )
        ,ncol = 2 , align = 'hv',legend = "top"
        )



    }) ## RegFigDiv end

    observeEvent(input$Sim, { ## 点击Run BTR ####

      StartY <- max( min(SimData$Tage$Year), min(SimData$clims$Year) ,input$syear )
      EndY <- min( max(SimData$Tage$Year), max(SimData$clims$Year)  ,input$eyear )

      Taget <- SimData$Tage[SimData$Tage$Year %in% c(StartY:EndY ),]
      climst <- SimData$clims[SimData$clims$Year %in% c(StartY:EndY ),]

      ObsFt <- SimData$ObsF[SimData$ObsF$Year %in% c(StartY:EndY ),]
      ObsVt <- SimData$ObsV[SimData$ObsV$Year %in% c(StartY:EndY ),]


      ResData$result <-  rBTR::btr_parallel( clim = climst ,parameters = SimData$param , age = Taget,
                               syear = StartY  ,eyear = EndY , Cores = input$Cores, writeRes = F,
                               intraannual = F, gTmethod = "Jonhson" , division = "limit",
                               testLim = F,CZgR = c(1,0,0,1 ), Pbar = T , testMod = F,Dcase = "min", Named = NULL )

      ResRings <- ResData$result$annaulRing |>
        dplyr::select(Year, RingWidth, MaxVesselLumenArea, VesselDensity, VesselFraction ) |>
        dplyr::rename( Sim_MRW = RingWidth, Sim_MaxLA = MaxVesselLumenArea , Sim_CD =  VesselDensity, Sim_RCTA =  VesselFraction ) |>
        dplyr::inner_join( dplyr::select(Tage,Year,MRW,MaxLA,CD,RCTA ) |>
        dplyr::rename( Obs_MRW = MRW, Obs_MaxLA = MaxLA , Obs_CD =  CD, Obs_RCTA = RCTA ) )


      ResData$ModTestTable <- rbind( rBTR::mod.test( y_actual = ResRings$Obs_MRW ,y_predicted =  ResRings$Sim_MRW   ) ,
                             rBTR::mod.test( y_actual = ResRings$Obs_CD ,y_predicted =  ResRings$Sim_CD   ) ,
                             rBTR::mod.test( y_actual = ResRings$Obs_RCTA ,y_predicted =  ResRings$Sim_RCTA   ) ,
                             rBTR::mod.test( y_actual = ResRings$Obs_MaxLA ,y_predicted =  ResRings$Sim_MaxLA   )
                             ) |> round(4)
      rownames(ResData$ModTestTable) <- c( "RingWidth", "CD", "VF", "MaxLA" )


      output$TestRW <- renderTable(  ResData$ModTestTable ,rownames = T)

      output$PlotRW <- renderPlot({

        ResRings|>
          tidyr::gather(key, value , -1) |>
          tidyr::separate(key, c("Type","Traits" ),sep = "_") |>
          ggplot2::ggplot(  )+
            ggplot2::geom_line(  ggplot2::aes(x = Year, y = value,  color = Type) )+
            ggplot2::facet_grid(  Traits~., scale = "free" )

      } )

      ## Show Annual Rings Res
      ## 随机抽5年
      InY <- base::intersect(c( StartY : EndY), base::intersect( unique(ObsV$Year), unique(ObsF$Year)  ))
      ifelse( length( InY ) <= 5, InY,  InY  <- sample(InY,size = 5, replace = FALSE) )

      # ResData$InY <- InY
      FLine2 <- SimData$CLines$Fline2[ SimData$CLines$Fline2$Year %in% InY, ]
      FLine3 <- SimData$CLines$Fline3[ SimData$CLines$Fline3$Year %in% InY, ]
      VLine2 <- SimData$CLines$Vline2[ SimData$CLines$Vline2$Year %in% InY, ]
      VLine3 <- SimData$CLines$Vline3[ SimData$CLines$Vline3$Year %in% InY, ]


      SimTrait <- dplyr::left_join( ResData$result$xylem_trait,  ResData$result$annaulRing[ ,c( 'Year',"RingWidth")  ]  ) |>
        dplyr::filter( Year %in% InY ) |>
        dplyr::mutate( RRadDistR = Raddist/RingWidth/10  ,RRadDistRV = (Raddist+0.5*VCRD) /RingWidth/10) ##
      SimTrait$VCV[SimTrait$VCV == 0 ] <- NA
      # ResData$SimTrait <- SimTrait



      Pva <- ggplot2::ggplot(    )+
        ggplot2::theme_bw()+
        ggplot2::labs( title = "Vessel lumen area")+
        ggplot2:: scale_color_manual( values = c( "gray80", 'gray40', "orange" ), breaks = c( "ObsTID", "Obs","Sim")  )+
        # ggplot2::geom_smooth(data =  SimData$ObsV[ SimData$ObsV$Year %in% InY , ],
        #                      ggplot2::aes(x = RRadDistR , y =LA , color = "ObsTID" , group = TID ),
        #                      method = "gam",se=F ,formula = y~s(x , k = 4)   )+
        # ggplot2::geom_smooth(data =  SimData$ObsV[ SimData$ObsV$Year %in% InY , ],
        #                      ggplot2::aes(x = RRadDistR , y =LA , color = "Obs" ),
        #                      method = "gam",se=F ,formula = y~s(x , k = 4)     )+
        ggplot2::geom_line(data = VLine3, ggplot2::aes(x = RRadDistR , y = TYva , color = "ObsTID", group = TID )   )+
        ggplot2::geom_line(data = VLine2, ggplot2::aes(x = RRadDistR , y = TYva , color = "Obs" )   )+
        ggplot2::geom_point(data =  na.omit(SimTrait ),
                             ggplot2::aes(x = RRadDistRV , y = VCV  , color = "Sim"  ) )+
        ggplot2::geom_smooth(data = na.omit(SimTrait ) ,method = 'gam',se=F, formula = y~s(x , k = 6 ),
                            ggplot2::aes(x = RRadDistRV , y = VCV  , color = "Sim"  ) )+
        ggplot2::facet_grid(.~Year ,scale= "free")
      # Pva

      Pfa <- ggplot2::ggplot(    )+
        ggplot2::theme_bw()+
        ggplot2::labs( title = "Fiber lumen area")+
        ggplot2:: scale_color_manual( values = c( "gray80", 'gray40', "orange" ), breaks = c( "ObsTID", "Obs","Sim")  )+
        # ggplot2::geom_smooth(data =  ObsFt[ ObsFt$Year %in% InY , ],
        #                      ggplot2::aes(x = RRadDistR , y = LA , color = "ObsTID" , group = TID ),
        #                      method = "gam",se=F ,formula = y~s(x , k = 4)   )+
        # ggplot2::geom_smooth(data =  ObsFt[ ObsFt$Year %in% InY , ],
        #                      ggplot2::aes(x = RRadDistR , y = LA , color = "Obs" ),
        #                      method = "gam",se=F ,formula = y~s(x , k = 4)     )+
        ggplot2::geom_line(data = FLine3, ggplot2::aes(x = RRadDistR , y = TYfa , color = "ObsTID", group = TID )   )+
        ggplot2::geom_line(data = FLine2, ggplot2::aes(x = RRadDistR , y = TYfa , color = "Obs" )   )+
        ggplot2::geom_point(data =  SimTrait ,
                            ggplot2::aes(x = RRadDistR , y = CV  , color = "Sim"  ) )+
        ggplot2::geom_smooth(data = SimTrait ,method = 'gam',se=F,formula = y~s(x , k = 6 ),
                           ggplot2::aes(x = RRadDistR , y = CV  , color = "Sim"  ) )+
        ggplot2::facet_grid(.~Year ,scale= "free")
      # Pfa

      Pfw <- ggplot2::ggplot(    )+
        ggplot2::theme_bw()+
        ggplot2::labs( title = "Fiber cell wall thickness")+
        ggplot2:: scale_color_manual( values = c( "gray80", 'gray40', "orange" ), breaks = c( "ObsTID", "Obs","Sim")  )+
        # ggplot2::geom_smooth(data =  ObsFt[ ObsFt$Year %in% InY , ],
        #                      ggplot2::aes(x = RRadDistR , y = CWTall , color = "ObsTID" , group = TID ),
        #                      method = "gam",se=F ,formula = y~s(x , k = 4)   )+
        # ggplot2::geom_smooth(data =  ObsFt[ ObsFt$Year %in% InY , ],
        #                      ggplot2::aes(x = RRadDistR , y = CWTall , color = "Obs" ),
        #                      method = "gam",se=F ,formula = y~s(x , k = 4)     )+
        ggplot2::geom_line(data = FLine3, ggplot2::aes(x = RRadDistR , y = TYfwt , color = "ObsTID", group = TID )   )+
        ggplot2::geom_line(data = FLine2, ggplot2::aes(x = RRadDistR , y = TYfwt , color = "Obs" )   )+
        ggplot2::geom_point(data =  SimTrait ,
                            ggplot2::aes(x = RRadDistR , y = WT  , color = "Sim"  ) )+
        ggplot2::geom_smooth(data = SimTrait ,method = 'gam',se=F,formula = y~s(x , k = 6 ),
                           ggplot2::aes(x = RRadDistR , y = WT , color = "Sim"  ) )+
        ggplot2::facet_grid(.~Year ,scale= "free")
      # Pfw

      Pvn <- ggplot2::ggplot( )+
        ggplot2::theme_bw()+
        ggplot2::labs( title = "Fiber cell wall thickness")+
        # ggplot2:: scale_color_manual( values = c( "gray80", 'gray40', "orange" ), breaks = c( "ObsTID", "Obs","Sim")  )+
        ggplot2::geom_point(data =  SimTrait ,
                            ggplot2::aes(x = RRadDistRV , y = rrVN  , color = "VN"  ) )+
        ggplot2::geom_abline(slope = 1)+
        ggplot2::facet_grid(.~Year ,scale= "free")



      output$TestAR <- renderPlot({
        ggpubr::ggarrange(
           Pfa, Pfw,Pva,Pvn, ncol = 1 , align = 'hv', common.legend = T, legend = "top" )
      } )

      ##
      tLiRes <- Litest( microclim = ResData$result$microclim ,
                        SimTrait = SimTrait ,
                        ObsV = ObsVt ,ObsF= ObsFt ,InY = InY )

      LineV <- tLiRes$SimClim[ c(60:273) , c('DOY',"L_i.vessel","dL_i"  )  ] |> dplyr::mutate(type = "TA")

      EWDOYV <- which.min( abs( LineV$L_i.vessel - (max(LineV$L_i.vessel) - min(LineV$L_i.vessel))*0.01 ) )+59
      aPV <- which.min( abs( LineV$L_i.vessel - (max(LineV$L_i.vessel) - min(LineV$L_i.vessel))*0.15 ) )+59
      bPV <- which.min( abs( LineV$L_i.vessel - (max(LineV$L_i.vessel) - min(LineV$L_i.vessel))*0.65 ) )+59
      LWDOYV <- which.min( abs( LineV$L_i.vessel - (max(LineV$L_i.vessel) - min(LineV$L_i.vessel))*0.99 ) )+59
      LineV$type <- cut(x = LineV$DOY,breaks =  c(0, EWDOYV,aPV,bPV,LWDOYV,366  ) ,labels = c( 'EA','a','TA','b','LA'  )  ) ##

      updateNumericInput(session, "MaxLi.Vessel", value = max(LineV$L_i.vessel) )
      updateNumericInput(session, "EwLiDoyV", value = EWDOYV)
      updateNumericInput(session, "LwLiDoyV", value = LWDOYV)

      LineF <- tLiRes$SimClim[ c(60:273) , c('DOY',"L_i.fiber","dL_i"  )  ] |> dplyr::mutate(type = "TA")
      EWDOYF <- which.min( abs( LineF$L_i.fiber - (max(LineF$L_i.fiber) - min(LineF$L_i.fiber))*0.01 ) )+59
      aPF <- which.min( abs( LineF$L_i.fiber - (max(LineF$L_i.fiber) - min(LineF$L_i.fiber))*0.15 ) )+59
      bPF <- which.min( abs( LineF$L_i.fiber - (max(LineF$L_i.fiber) - min(LineF$L_i.fiber))*0.65 ) )+59
      LWDOYF <- which.min( abs( LineF$L_i.fiber - (max(LineF$L_i.fiber) - min(LineF$L_i.fiber))*0.99 ) )+59
      LineF$type <- cut(x = LineF$DOY,breaks =  c(0, EWDOYF,aPF,bPF,LWDOYF,366  ) ,labels = c( 'EA','a','TA','b','LA'  )  ) ##

      # if ( all(  input$EwLiDoyF == 100 , input$LwLiDoyF == 320 ,
      #            input$EwLiDoyV == 100 , input$LwLiDoyV == 320  )  ) {

        updateNumericInput(session, "MaxLi.fiber", value = max(LineF$L_i.fiber) )
        updateNumericInput(session, "EwLiDoyF", value = EWDOYF)
        updateNumericInput(session, "LwLiDoyF", value = LWDOYF)



      # }

      tLiRes$LineV <- LineV
      tLiRes$LineF <- LineF

      ResData$tLiRes <- tLiRes

    }) #点击Run BTR end

    output$RegFigLis  <- renderPlot({

      if ( length(ResData) != 0  ) {

        LineF <- ResData$tLiRes$LineF |> dplyr::mutate(NewL_i.fiber = NA, Newtypr= 'EW')
        LineV <- ResData$tLiRes$LineV |> dplyr::mutate(NewL_i.vessel = NA, Newtypr= 'EW')


        ## old type
        # dFEA = input$EwLiDoyF - max(LineF$DOY[ LineF$type == "EA" ])
        # dFLA = input$LwLiDoyF - min(LineF$DOY[ LineF$type == "LA" ])
        # dFDOY = nrow(LineF[ LineF$type == "TA", ])
        # RRdliF <- ifelse( is.na(input$MaxLi.fiber),1, input$MaxLi.fiber/ max(LineF$L_i.fiber)    )
        # LineF$NewL_i.fiber <- LineF$L_i.fiber * RRdliF
        # LineF$DOYnew[ LineF$type == "EA" ] <- LineF$DOYnew[ LineF$type == "EA" ] + dFEA
        # LineF$DOYnew[ LineF$type == "LA" ] <- LineF$DOYnew[ LineF$type == "LA" ] + dFLA
        # LineF$DOYnew[ LineF$type == "TA" ] <- input$EwLiDoyF + ((input$LwLiDoyF -input$EwLiDoyF )/dFDOY) * c(1:dFDOY)

        # dVEA = input$EwLiDoyV - max(LineV$DOY[ LineV$type == "EA" ])
        # dVLA = input$LwLiDoyV - min(LineV$DOY[ LineV$type == "LA" ])
        # dVDOY = nrow(LineV[ LineV$type == "TA", ])
        # RRdliV <- ifelse( is.na(input$MaxLi.vessel),1, input$MaxLi.vessel/ max(LineV$L_i.vessel)  )
        # LineV$NewL_i.vessel <- LineV$L_i.vessel * RRdliV
        # LineV$DOYnew[ LineV$type == "EA" ] <- LineV$DOYnew[ LineV$type == "EA" ] + dVEA
        # LineV$DOYnew[ LineV$type == "LA" ] <- LineV$DOYnew[ LineV$type == "LA" ] + dVLA
        # LineV$DOYnew[ LineV$type == "TA" ] <- input$EwLiDoyV + ((input$LwLiDoyV -input$EwLiDoyV )/dVDOY) * c(1:dVDOY)

        ### new type
        ## 新区间
        R_ELF <- (input$LwLiDoyF - input$EwLiDoyF - 1)/ length( LineF$type[ LineF$type %in%c("a","TA", "b"   ) ]   )
        ## a的新时常
        daF <- round(length(LineF$type[ LineF$type == "a"]  ) * R_ELF )
        dTAF <- round(length(LineF$type[ LineF$type == "TA"]  ) * R_ELF )
        LineF$Newtypr <- cut( LineF$DOY, breaks = c(0,input$EwLiDoyF, input$EwLiDoyF+daF, input$EwLiDoyF+daF +dTAF ,input$LwLiDoyF,366 ),
                              labels = c('EA' ,'a','TA' , 'b', 'LA'  ) )

        # LineF$NewL_i.fiber <- NA
        LineF$NewL_i.fiber[ LineF$Newtypr == "EA"  ]  <- 0
        LineF$NewL_i.fiber[ LineF$Newtypr == "LA"   ]  <- input$MaxLi.fiber
        LineF$NewL_i.fiber[ LineF$Newtypr == "TA"   ]  <- input$MaxLi.fiber*0.5/dTAF *
                                                          c(1:dTAF ) +input$MaxLi.fiber*0.15
        # ggplot(LineF)+
        # geom_line( aes(x =DOY , y =  NewL_i.fiber, linetype= "New" ), linewidth = 1)+
        # geom_line( aes(x =DOY , y =  L_i.fiber, color = type ), linetype = 2 ,linewidth = 1 )


        R_ELV <- (input$LwLiDoyV - input$EwLiDoyV - 1)/ length( LineV$type[ LineV$type %in%c("a","TA", "b"   ) ]   )
        daV <- round(length(LineV$type[ LineV$type == "a"]  ) * R_ELV )
        dTAV <- round(length(LineV$type[ LineV$type == "TA"]  ) * R_ELV )
        LineV$Newtypr <- cut( LineV$DOY, breaks = c(0,input$EwLiDoyV, input$EwLiDoyV+daV, input$EwLiDoyV+daV +dTAV ,input$LwLiDoyV,366 ),
                              labels = c('EA' ,'a','TA' , 'b', 'LA'  ) )

        LineV$NewL_i.vessel[ LineV$Newtypr == "EA"  ]   <- 0
        LineV$NewL_i.vessel[ LineV$Newtypr == "LA"   ]  <- input$MaxLi.vessel
        LineV$NewL_i.vessel[ LineV$Newtypr == "TA"   ]  <- input$MaxLi.vessel*0.5/dTAV *
                                                              c(1:dTAV  )+input$MaxLi.vessel*0.15

        # ggplot(LineV)+
        #   geom_line( aes(x =DOY , y =  NewL_i.vessel, linetype= "New" ), linewidth = 1)+
        #   geom_line( aes(x =DOY , y =  L_i.vessel, color = type ), linetype = 2 ,linewidth = 1 )



        SimData$Line <- list(LineF=LineF, LineV =LineV)

          ## 更新数据
          param2 <- SimData$param


            NewLip <- RegLi( simclim = ResData$tLiRes$SimClim, LineF = SimData$Line$LineF , LineV = SimData$Line$LineV , param = param2  )

          if (input$ifRegLi == T) {
            SimData$NewLiP <- NewLip$NewP
            param2 <- AupdatedB(DataA = NewLip$NewP,DataB = param2,ons = 'parameter' )
            SimData$param <- param2
          }

        ggpubr::ggarrange(
        ggplot2::ggplot( ResData$tLiRes$SimTraitF )+
          ggplot2::scale_x_continuous(limits = c(60,300),breaks = c( 61,91,122,153,184,215,246,277      ),
                                      labels = c( 'Mar 61', 'Apr 91','May 122','Jun 153',"Jul 184","Aug 215",'Sep 246', 'Oct 277' )  )+
          ggplot2::geom_smooth( ggplot2::aes( x = DOY , y = LA,group = Year),
                                data = ResData$tLiRes$ObsF,alpha = 0.5,
                                method = 'gam',se=F,color = "#55B532"  )+
          ggplot2::geom_point( ggplot2::aes( x = DOY , y = CV ),color = "orange" ,shape=2,alpha=0.4  )+
          ggplot2::geom_smooth( ggplot2::aes( x = DOY , y = CV),method = 'gam',se=F,color = "orange" ,linewidth = 2)
        ,
        ggplot2::ggplot( NewLip$LineF )+
          # ggplot2::labs(subtitle = paste( "a=", NewLip$NewP[1,2],"b=",NewLip$NewP[2,2],'c=',NewLip$NewP[3,2]   ))+
          ggplot2::scale_x_continuous(limits = c(60,300),breaks = c( 61,91,122,153,184,215,246,277      ),
                                      labels = c( 'Mar 61', 'Apr 91','May 122','Jun 153',"Jul 184","Aug 215",'Sep 246', 'Oct 277' ))+
          ggplot2::geom_point( ggplot2::aes( x = DOY, y = - NewL_i.fiber  ),
                              color = "red" )+
          ggplot2::geom_line( ggplot2::aes( x = DOY, y = - NewLi    ),
                               color = "pink", linewidth = 1.5 )+
          ggplot2::geom_line( ggplot2::aes( x = DOY , y = -L_i.fiber    ), color = "#0047AB" )
        ,
        ggplot2::ggplot( ResData$tLiRes$SimTraitV )+
          ggplot2::scale_x_continuous(limits = c(60,300),breaks = c( 61,91,122,153,184,215,246,277      ),
                                      labels = c( 'Mar 61', 'Apr 91','May 122','Jun 153',"Jul 184","Aug 215",'Sep 246', 'Oct 277' ))+
          ggplot2::geom_smooth( ggplot2::aes( x = DOY , y = LA,group = Year),
                                data = ResData$tLiRes$ObsV,alpha = 0.5,
                                method = 'gam',se=F,color = "#55B532"  )+
          ggplot2::geom_point( ggplot2::aes( x = DOY , y = VCV ),color = "orange",shape= 2  )+
          ggplot2::geom_smooth( ggplot2::aes( x = DOY , y = VCV),method = 'gam',se=F,color = "orange" ,linewidth = 2 )
        ,
        ggplot2::ggplot( NewLip$LineV )+
          ggplot2::scale_x_continuous(limits = c(60,300),breaks = c( 61,91,122,153,184,215,246,277      ),
                                      labels = c( 'Mar 61', 'Apr 91','May 122','Jun 153',"Jul 184","Aug 215",'Sep 246', 'Oct 277' ))+
          # ggplot2::labs(subtitle = paste( "a=", NewLip$NewP[4,2],"b=",NewLip$NewP[5,2],'c=',NewLip$NewP[6,2]   ))+
          ggplot2::geom_point( ggplot2::aes( x = DOY, y = -NewL_i.vessel    ),
                               color = "red" )+
          ggplot2::geom_line( ggplot2::aes( x = DOY, y = -NewLi    ),
                              color = "pink", linewidth = 1.5 )+
          ggplot2::geom_line( ggplot2::aes( x = DOY , y = -L_i.vessel    ), color = "#0047AB" )
        ,ncol =1 ,align = 'hv'
        )


      } else{
        ggplot2::ggplot()+
          ggplot2::labs(title = "Waite Sim BTR")
      }## if end -------


    })

    output$NOparamLi <- renderTable({
        dplyr::left_join( SimData$param[ SimData$param$parameter %in% c(  'a.fiber','b.fiber','c.fiber','a.vessel', 'b.vessel', 'c.vessel' ),
                                            c('parameter', 'values'  )] ,
                          SimData$Oldparam[ SimData$Oldparam$parameter %in% c(  'a.fiber','b.fiber','c.fiber','a.vessel', 'b.vessel', 'c.vessel' ),
                                            c('parameter', 'values'  )] |> dplyr::rename(OldValue = values )      ) |>
        t() |> as.data.frame()   },rownames = T )

    observeEvent( input$SaveParam, {
      SimData$Oldparam <- SimData$param
    } )


    observeEvent( input$ReturenParam, {
      SimData$param <- SimData$Oldparam
    } )



    # Downloadable Param ----
    output$downloadParam <- downloadHandler(
      filename = "Parameters.xlsx",
      content = function(file) {
        openxlsx::write.xlsx( SimData$param , file )
      }
    )


    # Downloadable Result ----
    output$downloadResult <- downloadHandler(
      filename = "Outputs.xlsx",
      content = function(file) {
        openxlsx::write.xlsx( ResData$result , file )
      }
    )

  }

  shinyApp(ui, server)


}

# sim_btr(param = param,clims =  clims,Tage = Tage, ObsF = ObsF ,ObsV = ObsV )

