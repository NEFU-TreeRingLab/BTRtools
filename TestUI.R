## import Rpackages ####
library(shiny)
library(tidyverse)
library(rBTRdev)
###

##################
####    UI    ####
##################

ui<- tagList(  ##

  navbarPage(
    "Sim rBTR",

    tabPanel( # tab1  ####
              "Trend Age",
              sidebarPanel(width =3,
                           "三种方法拟合年龄趋势",
                           ## 读入 RW 和 MaxLA 序列拟合年龄趋势文件
                           fileInput("fileRW", "Updata csv file of TreeRing series",
                                     accept=c("text/csv", "text/comma-separated-values,text/plain")),
                           checkboxInput("headerRW", "Header", TRUE),
                           tags$hr(),
                           ## 指定行列名
                           textInput("xaxis", "colum of X-axis", "age"),
                           textInput("MRW", "colum of Ring Width", "MRW"),
                           textInput("MaxLA", "colum of Max Lumen area", "MAXLA"),

                           tags$hr(),
                           ## 输入参数
                           helpText( "Formula: y  = α exp( β * x ) + c"),
                           helpText( "Parameters of MRW"),
                           ### input param 1 & 2 :alpha_age, beta_age
                           numericInput("rwA", "Alpha", NULL, min = -Inf , max = Inf ),
                           numericInput("rwB", "Beta", NULL, min = -Inf , max = Inf ),
                           numericInput("rwC", "C", NULL, min = -Inf , max = Inf ),

                           ### input Knots of gam , use slider
                           sliderInput("rwK",
                                       "K of GAM:",
                                       min = 1,
                                       max = 12,
                                       step = 1,
                                       value = 5 ), ## end of sliderinput
                           tags$hr(),

                           helpText( "Parameters of MaxLA"),
                           ### input param 1 & 2 :alpha_age, beta_age
                           numericInput("laA", "Alpha", NULL, min = -Inf , max = Inf ),
                           numericInput("laB", "Beta",NULL, min = -Inf , max = Inf ),
                           numericInput("laC", "C", NULL, min = -Inf , max = Inf ),
                           ### input Knots of gam , use slider
                           sliderInput("laK",
                                       "K of GAM:",
                                       min = 1,
                                       max = 12,
                                       step = 1,
                                       value = 5 ), ## end of sliderinput

                           tags$hr(),
                           selectInput("Trw", "Choose RW trend line:",
                                       choices = c("EXP", "GAM", "Manual")),
                           selectInput("Tla", "Choose a dataset:",
                                       choices = c("EXP", "GAM", "Manual")),
                           tags$hr(),
                           # Button
                           actionButton("Sim", "Calculate"),
                           # Button
                           downloadButton("downloadData", "Download Trend_age")

              ), ##end of sidebarPanel


              # 输出部分：显示图表
              mainPanel(
                tableOutput("dataRW"),
                plotOutput("RegFig"),
                tableOutput("Params"),
                tableOutput("ModAs"),
                tableOutput('Trend')
              ) ## mainPanel end --


    ), ## tablanel 1 end -----

    tabPanel( ## tab 2 ####
              "Fiber cell",
              column( ## col 2.1 细胞初始参数
                2,
                h4("输入纤维细胞观察数据（ROXAS）"),
                fileInput("fileF", "Updata csv file of fiber cells anatomy ",
                          accept=c("text/csv", "text/comma-separated-values,text/plain")),
                checkboxInput("headerF", "Header", TRUE),
                tags$hr(),

                wellPanel( ## 灰色底 panel
                  h5("输入初始细胞参数 Initial cambial cell size"),
                  numericInput("LA", "Lunen Area", 5, min = -Inf , max = Inf ), ##
                  numericInput("CWT", "Cell wall thick", 5, min = -Inf , max = Inf ), ##
                  numericInput("CRD", "CRD", 5, min = -Inf , max = Inf ), ##
                  numericInput("CTD", "CTD", 5, min = -Inf , max = Inf ), ##
                  tags$hr(),
                  h5( "细胞初始生长速率 Specific cell growth rate" ),
                  numericInput("va_c.fiber", "V cell enlargement ", 5, min = -Inf , max = Inf ), ##
                  numericInput("va_w.fiber", "V cell wall deposition", 5, min = -Inf , max = Inf ), ##
                  numericInput("va_l.fiber", "V cell lignin", 5, min = -Inf , max = Inf ) ##
                  # checkboxInput("vessel", "Vessel", value = FALSE)
                ) ## wellPanel end
              ), ## col 2.1 end ---

              column( ## col 2.2 细胞初始参数
                2,
                wellPanel( ## 灰色底 panel
                  h4("细胞生长参数"),
                  numericInput("CAmax", "MAx cell Area", 400, min = 0 , max = Inf, step = 20  ), ##
                  numericInput("ml", "p1 of lignified (ml)", 400, min = 0 , max = Inf , step = 10), ##
                  numericInput("sl", "p2 of lignified (sl)", 8, min = 0 , max = Inf , step = 0.1), ##
                  numericInput("mw", "p1 of deposition (mw)", 400, min = 0 , max = Inf , step = 10), ##
                  numericInput("sw", "p2 of deposition (sw)", 3, min = 0 , max = Inf , step = 0.1), ##
                  numericInput("WAmax", "Max cell wall area", 200, min = 0 , max = Inf , step = 10), ##
                  numericInput("WTmax", "Max cell wall thick", 5, min = 0 , max = Inf , step = 0.1), ##
                  numericInput("WTmin", "Min cell wall thick", 1, min = 0 , max = Inf , step = 0.1), ##
                  numericInput("WTa", "Threshold thickness of cell wall", 3, min = 0 , max = Inf , step = 0.1) ##

                ) ## wellPanel end
              ), ## col 2.2 end ---

              # 输出部分：显示图表
              mainPanel( ## Tab 2 output
                # tableOutput("contents"),
                plotOutput("plot"),
                tableOutput("modTest")

              ) ## mainPanel end --

    ), ## tabPanel 2 end -------

    tabPanel( ## tab 3  ####
      "Vessel cell",
      column( ## col 3.1 细胞初始参数
        2,
        h4("输入导管细胞观察数据（ROXAS）"),
        fileInput("fileV", "Updata csv file of vessels anatomy ",
                  accept=c("text/csv", "text/comma-separated-values,text/plain")),
        checkboxInput("headerV", "Header", TRUE),
        tags$hr(),

        wellPanel( ## 灰色底 panel
          h4("输入初始细胞参数"),
          numericInput("LA", "Lunen Area", 5, min = -Inf , max = Inf ), ##
          numericInput("CWT", "Cell wall thick", 5, min = -Inf , max = Inf ), ##
          numericInput("CRD", "CRD", 5, min = -Inf , max = Inf ), ##
          numericInput("CTD", "CTD", 5, min = -Inf , max = Inf ), ##
          tags$hr(),
          h4( "细胞初始生长速率" ),
          numericInput("va_c.vessel", "V cell enlargement ", 5, min = -Inf , max = Inf ), ##
          numericInput("va_w.vessel", "V cell wall deposition", 5, min = -Inf , max = Inf ), ##
          numericInput("va_l.vessel", "V cell lignin", 5, min = -Inf , max = Inf ) ##
          # checkboxInput("vessel", "Vessel", value = FALSE)
        ) ## wellPanel end
      ), ## col 3.1 end ---

      column( ## col 3.2 细胞初始参数
        2,
        wellPanel( ## 灰色底 panel
          h4("细胞生长参数"),
          numericInput("CAmax", "MAx cell Area", 400, min = 0 , max = Inf, step = 20  ), ##
          numericInput("ml", "p1 of lignified (ml)", 400, min = 0 , max = Inf , step = 10), ##
          numericInput("sl", "p2 of lignified (sl)", 8, min = 0 , max = Inf , step = 0.1), ##
          numericInput("mw", "p1 of deposition (mw)", 400, min = 0 , max = Inf , step = 10), ##
          numericInput("sw", "p2 of deposition (sw)", 3, min = 0 , max = Inf , step = 0.1), ##
          numericInput("WAmax", "Max cell wall area", 200, min = 0 , max = Inf , step = 10), ##
          numericInput("WTmax", "Max cell wall thick", 5, min = 0 , max = Inf , step = 0.1), ##
          numericInput("WTmin", "Min cell wall thick", 1, min = 0 , max = Inf , step = 0.1), ##
          numericInput("WTa", "Threshold thickness of cell wall", 3, min = 0 , max = Inf , step = 0.1) ##

        ) ## wellPanel end
      ), ## col 3.2 end ---

      # 输出部分：显示图表
      mainPanel( ## Tab 2 output
        ## 导管生长图，gR1 dLi 0.02 & gR2 dLi -0.055
        ## 早（30）晚材（后30）实测和模拟 plot & table
        plotOutput("plot")

      ) ## mainPanel end --
    ), ## tabPanel 3 end -------

    tabPanel( ### Tab 4 ####
      "ClimLimite",
      column( ## col 1 气候参数
        2,
        wellPanel( ## 灰色底 panel
          h4("输入气象数据"),
          fileInput("fileClim", "Updata csv file of Climate",
                    accept=c("text/csv", "text/comma-separated-values,text/plain")),
          checkboxInput("headerClim", "Header", TRUE),
          tags$hr(),

          h5("Climate limit parameters"),
          numericInput("AAT", "Accumulated temperature", 0, min = 0 , max = Inf , step = 0.5), ##
          numericInput("deltaH_A_Da", "enthalpy of activation", 400, min = 0 , max = Inf, step = 1  ), ##
          numericInput("deltaH_D", "ΔHd", 400, min = 0 , max = Inf , step = 1), ##
          numericInput("deltaS_D", "ΔSd", 8, min = 0 , max = Inf , step = 1), ##
          numericInput("M1", "M1", 400, min = 0 , max = Inf , step = 0.02), ##
          numericInput("M2", "M2", 3, min = 0 , max = Inf , step = 0.02), ##
          numericInput("M3", "M3", 200, min = 0 , max = Inf , step = 0.02), ##
          numericInput("M4", "M4", 5, min = 0 , max = Inf , step = 0.05), ##
          numericInput("V1", "V1", 1, min = 0 , max = Inf , step = 0.05), ##
          numericInput("V2", "V2", 1, min = 0 , max = Inf , step = 0.05), ##
          numericInput("V3", "V3", 1, min = 0 , max = Inf , step = 0.05), ##
          numericInput("V4", "V4", 3, min = 0 , max = Inf , step = 0.01) ##
      )## wellPanel end

      ), ## colum 4.1 end --


      # 输出部分：显示图表
      mainPanel( ## Tab 2 output
        ## RW 和 LA的去趋势scale 图与sum gR图
        plotOutput("plot")

      ) ## mainPanel end --

    ),## tabPanel 4 end -------

    tabPanel( ### Tab 5 ####
      "Division & Differentiation",
      column( ## 5.1
        2,
        wellPanel(
          fileInput("fileTd", "Updata csv file of age trend",
                    accept=c("text/csv", "text/comma-separated-values,text/plain")),
          checkboxInput("headerTd", "Header", TRUE),
          tags$hr(),

          numericInput("maxRCTA", "maxRCTA", 1, min = 0 , max = Inf , step = 0.05), ##
          numericInput("RCTADivT", "RCTADivT", 1, min = 0 , max = Inf , step = 0.05), ##
          numericInput("a1", "a1", 3, min = 0 , max = Inf , step = 0.01), ##
          numericInput("a2", "a2", 3, min = 0 , max = Inf , step = 0.01), ##
          numericInput("deltaD", "ΔD", 3, min = 0 , max = Inf , step = 0.01), ##
          numericInput("Div_alpha", "ΔDmin", 3, min = 0 , max = Inf , step = 0.01), ##
          tags$hr(),
          h5( 'czgR = αcz *exp(βcz Lcz)' ),
          numericInput("va_cz", "Vcz", 3, min = 0 , max = Inf , step = 0.01), ##
          numericInput("alpha_cz", "αcz", 3, min = 0 , max = Inf , step = 0.01), ##
          numericInput("beta_cz", "βcz", 3, min = 0 , max = Inf , step = 0.01) ##

        )
      ),## col 5.1 end

      mainPanel( ## Tab 6 output
        ## 分裂分化趋势
        plotOutput("plot")

      ) ## mainPanel end --

    ),## tabPanel 5 end -------

    tabPanel( ### Tab 6 ####
      "photoperiod on growth",
      column( ## 6.1
        2,
        wellPanel(
          h5("photoperiod on growth"),
          h6("Fiber cell: a *exp(-exp( b - c ΔLi ))"),
          numericInput("a.fiber", "a", 1, min = -Inf , max = Inf , step = 0.05), ##
          numericInput("b.fiber", "b", 1, min = -Inf , max = Inf , step = 0.05), ##
          numericInput("c.fiber", "c", 1, min = -Inf , max = Inf , step = 0.01), ##
          tags$hr(),
          h6("Vessel cell"),
          numericInput("a.vessel", "a", 1, min = -Inf , max = Inf , step = 0.05), ##
          numericInput("b.vessel", "b", 1, min = -Inf , max = Inf , step = 0.05), ##
          numericInput("c.vessel", "c", 1, min = -Inf , max = Inf , step = 0.01), ##

        )
      ),## col 6.1 end

      # 输出部分：显示图表
      mainPanel( ## Tab 6 output
        ## 年内细胞趋势
        plotOutput("plot")

      ) ## mainPanel end --

    ),## tabPanel 6 end -------

    tabPanel(## Tab 7 ####
      "Simulate",
      column( 2, ## 7.1
        wellPanel(
          numericInput("syear", "Stat year", 2000, min = 1800 , max = 2100 , step = 1), ##
          numericInput("eyear", "End year", 2000, min =1800 , max = 2100 , step = 1), ##
          tags$hr(),
          checkboxInput("writeRes", "writeRes", TRUE),
          checkboxInput("intraannual", "intraannual", TRUE),
          checkboxInput("Pbar", "Pbar", TRUE),
          tags$hr(),
          radioButtons("Dcase", "Dcase",
                       choices = c(min = "min",
                                   mean = "mean",
                                   multiply = "multiply"),
                       selected = "min"),
          tags$hr(),
          actionButton("rBTR", "Run BTRmodel"),
          actionButton("downloadRes", "download Result")
        ),
      ),#7.1end

      # 输出部分：显示图表
      mainPanel( ## Tab 6 output
        ## 年内细胞趋势
        plotOutput("plot")

      ) ## mainPanel end --


    ),## tabPanel 7 end -------




  ), ## navbarPage end -----



)## taglist end -----



#################
#### servers ####
#################

server <- function(input, output) {

  #### read datas ####
  # 当文件被上传时，读取CSV数据
  dataRw <- reactive({
    req(input$fileRW)
    read.csv(input$fileRW$datapath, header = input$headerRW)
  }) ## Tab 1

  dataF <- reactive({
    req(input$fileF)
    read.csv(input$fileF$datapath, header = input$headerF)
  }) ## Tab 2

  dataV <- reactive({
    req(input$fileV)
    read.csv(input$fileV$datapath, header = input$headerV)
  }) ## Tab 3

  dataClim <- reactive({
    req(input$fileClim)
    read.csv(input$fileClim$datapath, header = input$headerClim)
  }) ## Tab4

  dataTd <-  reactive({
    req(input$fileTd)
    read.csv(input$fileTd$datapath, header = input$headerTd)
  }) ## Tab 5

  #### 逐Table 分析
  ## blank database Tab1  ####
  Tab1Val <- reactiveValues(   )
  dtTrend <- reactiveValues(   )

  #### Tab 1 Analysis ####
  ### show dataTable
  output$dataRW <-renderTable({  head( dataRw(),3 )  })

  observeEvent(input$Sim, {
      ResReg <- RegData( dt = dataRw() ,
                         Nage = input$xaxis , Nrw = input$MRW, Nla = input$MaxLA,
                         rwA = input$rwA, rwB = input$rwB, rwC = input$rwC, rwK = input$rwK,
                         laA = input$laA, laB = input$laB, laC = input$laC, laK = input$laK )

      Tab1Val$dtOri <- ResReg$dtOri
      Tab1Val$dtNor <- ResReg$dtNor
      Tab1Val$param <- ResReg$param
      Tab1Val$ModAs <- ResReg$ModAs

      ccrw <- case_when(
        input$Trw == "EXP" ~ "pExpRw",
        input$Trw == "GAM" ~ "pGamRw",
        input$Trw == "Manual" ~ "mExpRw")
      ccla <- case_when(
        input$Trw == "EXP" ~ "pExpLa",
        input$Trw == "GAM" ~ "pGamLa",
        input$Trw == "Manual" ~ "mExpLa")
      Tab1Val$Trend <- Tab1Val$dtNor |> select(all_of(  c('Year','Age',ccrw,ccla ))) |>
        dplyr::rename( setNames( c('Year','Age',ccrw,ccla ), c('Year','Age','Tage','Lage'  )))

    output$RegFig <- renderPlot({
      dtRegSum <- ResReg$dtOri |> select(Age,MRW,pGamRw,pExpRw,mExpRw) |> mutate(Name = "RW", type = "Regression", .after = Age ) |>
        rename(setNames( c('Age','MRW','pGamRw','pExpRw','mExpRw' ),c('Age','Obs','GAM','EXP','Manu' )) )
      dtRegSum <- ResReg$dtOri |> select(Age,MaxLA,pGamLa,pExpLa, mExpLa)|> mutate(Name = "MaxLA", type = "Regression", .after = Age) |>
        rename(setNames( c('Age','MaxLA','pGamLa','pExpLa','mExpLa' ),c('Age','Obs','GAM','EXP','Manu' )))  |>
        bind_rows(dtRegSum)
      dtRegSum <- ResReg$dtNor |> select(Age,pGamRw,pExpRw,mExpRw) |> mutate(Name = "RW", type = "Trend line", .after = Age ) |>
        rename(setNames( c('Age','pGamRw','pExpRw','mExpRw' ),c('Age','GAM','EXP','Manu' )) ) |>
        bind_rows(dtRegSum)
      dtRegSum <- ResReg$dtNor |> select(Age,pGamLa,pExpLa, mExpLa)|> mutate(Name = "MaxLA", type = "Trend line", .after = Age) |>
        rename(setNames( c('Age','pGamLa','pExpLa','mExpLa' ),c('Age','GAM','EXP','Manu' )))  |>
        bind_rows(dtRegSum) |> gather(key, val , c(-1:-3)) |> na.omit()

      ggplot(dtRegSum ) + ## x = "age1", y = 'MRW'
        theme_light()+
        labs(title = "sim",
             # subtitle = paste( 'k = ', input$knots, '| α = ', alphat, " β = " ,  betat, " c = ",ct, ExpRes ),
             x = 'Age', y = 'Value'
        )+theme(legend.position = "bottom")+
        # scale_y_continuous( limits = c(0 , 1.1*max(dt$y))    )+
        geom_point(aes(x= Age, y = val ,color = "Obs" ), data = dtRegSum[ dtRegSum$key == "Obs",])+
        geom_line(aes(x= Age, y = val , color = key))+
        facet_wrap(factor(Name,levels = c('RW', 'MaxLA') )  ~ type, scales = "free")

    }) # RegFig end

    output$Params <- renderTable( Tab1Val$param, digits = 5 )
    output$ModAs <- renderTable( Tab1Val$ModAs , rownames = T  )
    output$Trend <- renderTable( head( Tab1Val$Trend ) )

  }) ## Sim act end

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "TrendAge.csv",
    content = function(file) {
      write.csv(Tab1Val$Trend , file, row.names = FALSE)
    }
  )
  #### Tab 1 Analysis end ----


} ## Server end





shinyApp(ui ,server)
