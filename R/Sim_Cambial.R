#' #' Sim Cambial
#' #' 用10年平均的气候模拟10年平均的纤维细胞和导管的年内生长趋势
#' #'
#' #' @export
#' #'
#' #' @import shiny
#' #'
#' #' @param syear start year
#' #' @param eyear end year
#' #' @param ObsF Fiber observe data
#' #' @param ObsV Vessel observe data
#' #' @param param parameters
#' #' @param clims climate
#' #'
#' #' @importFrom dplyr left_join
#' #' @importFrom tidyr gather
#' #' @importFrom ggplot2 ggplot theme_bw labs theme geom_point geom_line aes scale_color_manual scale_x_continuous
#' #' @importFrom rBTRdev mod.test
#' #' @importFrom ggpubr ggarrange
#' #' @importFrom openxlsx write.xlsx
#' #'
#' # library(shiny)
#' sim_cambial<- function(  param , clims =NA  , syear =NA , eyear=NA ,obsF=NA , ObsV=NA ){
#'
#'   StartY <- max( min(Tage$Year), min(clims$Year)  )
#'   EndY <- min( max(Tage$Year), max(clims$Year) )
#'
#'   clims <- clims[clims$Year %in% c(StartY:EndY ),]
#'
#'   obsF <- obsF[obsF$Year %in% c(StartY:EndY ),]
#'   obsV <- obsF[obsF$Year %in% c(StartY:EndY ),]
#'
#'
#'   ui <- fluidPage( ## ui ####
#'                    titlePanel( "Simulate Cambial"),
#'                    column( ## col 1
#'                      width = 2,
#'                      h4("Vessel division"),
#'                      numericInput("deltaD", "deltaD",param$values[param$parameter == "deltaD"],
#'                                   min = -Inf , max = Inf,step = 0.1 ), ##
#'                      numericInput("a1", "a1",param$values[param$parameter == "a1"],
#'                                   min = -Inf , max = Inf,step = 0.1 ), ##
#'                      numericInput("a2", "a2",param$values[param$parameter == "a2"],
#'                                   min = -Inf , max = Inf,step = 0.1 ), ##
#'                      numericInput("Div_alpha", "Div_alpha",param$values[param$parameter == "Div_alpha"],
#'                                   min = -Inf , max = Inf,step = 0.1 ), ##
#'                      numericInput("maxRCTA", "maxRCTA",param$values[param$parameter == "maxRCTA"],
#'                                   min = -Inf , max = Inf,step = 0.1 ), ##
#'                      numericInput("RCTADivT", "RCTADivT",param$values[param$parameter == "RCTADivT"],
#'                                   min = -Inf , max = Inf,step = 0.1 ), ##
#'                      # h4("photoperiod"),
#'                      # numericInput("a.fiber", "a.fiber",param$values[param$parameter == "a.fiber"],
#'                      #              min = -Inf , max = Inf,step = 0.1 ), ##
#'                      # numericInput("b.fiber", "b.fiber",param$values[param$parameter == "b.fiber"],
#'                      #              min = -Inf , max = Inf,step = 0.1 ), ##
#'                      # numericInput("c.fiber", "c.fiber",param$values[param$parameter == "c.fiber"],
#'                      #              min = -Inf , max = Inf,step = 0.1 ), ##
#'                      # numericInput("a.vessel", "a.vessel",param$values[param$parameter == "a.vessel"],
#'                      #              min = -Inf , max = Inf,step = 0.1 ), ##
#'                      # numericInput("b.vessel", "b.vessel",param$values[param$parameter == "b.vessel"],
#'                      #              min = -Inf , max = Inf,step = 0.1 ), ##
#'                      # numericInput("c.vessel", "c.vessel",param$values[param$parameter == "c.vessel"],
#'                      #              min = -Inf , max = Inf,step = 0.1 ) ##
#'                      tags$hr(),
#'                      # Button
#'                      downloadButton("downloadParam", "Download Parameters")
#'
#'
#'                    ), # end c1
#'
#'                    column( ## col 2
#'                      width = 2,
#'                      h4("Photoperiod 调整"),
#'                      numericInput("MaxLi.fiber", "Fiber dLi ",0,
#'                                   min = 0 , max = 5,step = 0.1 ), ##
#'                      sliderInput("MaxLiDoy",
#'                                  "The end DOY of early wood fiber:",
#'                                  min = 100,
#'                                  max = 200,
#'                                  step = 1,
#'                                  value = 176 ), ## end of sliderinput
#'                      sliderInput("MinLiDoy",
#'                                  "The start DOY of late wood fiber:",
#'                                  min = 150,
#'                                  max = 320,
#'                                  step = 1,
#'                                  value = 200 ), ## end of sliderinput
#'                      tags$hr(),
#'                      numericInput("MaxLi.vessel", "Vessel dLi",0,
#'                                   min = 0 , max = 5,step = 0.1 ), ##
#'                      sliderInput("MaxLiDoy",
#'                                  "The end DOY of early wood vessel:",
#'                                  min = 100,
#'                                  max = 200,
#'                                  step = 1,
#'                                  value = 176 ), ## end of sliderinput
#'                      sliderInput("MinLiDoy",
#'                                  "The start DOY of late wood vessel:",
#'                                  min = 150,
#'                                  max = 320,
#'                                  step = 1,
#'                                  value = 200 ), ## end of sliderinput
#'
#'                    ),# end c 2
#'
#'                    column( ## col output
#'                      width = 8,
#'
#'                    ),# end c output
#'
#'
#'   )## ui end -----
#'
#'   server <- function(input, output, session) {
#'
#'   }
#'
#'   shinyApp(ui, server)
#'
#'
#' }
#'
#' sim_cambial(param =param  )
