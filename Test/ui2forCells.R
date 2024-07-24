library(shiny)
### 
#### fuctions ####
modelEXP <- function( dt,colx ,coly , SSalpha,SSbeta,SSC  ){
  # f1 <-  as.formula( paste( coly ,'~ a * exp(b *', colx,' )' ) )
  # model2 <- nls( y ~ a*exp( b*x  ) + c   , data = dt ,
  #                start = list( a = SSalpha , b = SSbeta , c = SSC  ))
  as = c(SSalpha*0.5,  SSalpha*1.5  ) 
  bs = c( SSbeta * 0.5 , SSbeta * 1.5   )
  cs = c( SSC * 0.5 , SSC * 1.5  )
  
  f1 <- as.formula( paste( coly, "  ~ a* exp( b* ", colx ,") + c "   )   )
  model2 <- nls_multstart( f1  , 
                           data = dt ,  
                           iter = 500,
                           start_lower = c( a= min( as ), b= min(bs) ,c =min(cs)  ),
                           start_upper = c( a= max( as ), b= max(bs) ,c =max(cs) ) ,
                           supp_errors = 'Y' )
  
  return( model2 )
}

nor <- function( x ){
  res <- x /max(x)
}

mod.test<-function(y_actual,y_predicted,method = "pearson",digits = 3,  ... ){
  avr_y_actual <- mean(y_actual)
  
  ## SSR
  ss_regression <- sum( (y_predicted - avr_y_actual)^2)
  
  ## SSE
  ss_residuals <- sum( (y_actual - y_predicted)^2 )
  
  ## SST
  ss_total <- sum( (y_actual - avr_y_actual)^2 )
  
  
  ####
  ss_total2 <- ss_regression + ss_residuals
  ss_total3 <-  ss_total + ss_residuals
  
  rsquare <-  1- ss_residuals / ss_total
  
  #return(rsquare)#当模型偏差过大，rsquare很小时，不采用rsquare统计
  n1<-length(y_actual)
  n2<-length(y_predicted)#要求n1 == n2
  meansquare <- ss_residuals/(n1-2)
  #参考王辰勇译《线性回归分析导论》12页
  #return(meansquare)#MS残
  COR <- cor.test(y_actual, y_predicted, method = method,... )$estimate
  pvalue <- cor.test(y_actual, y_predicted, method = method ,...)$p.value
  
  RMSE<-(ss_residuals/n1)^0.5
  NRMSD<-RMSE/avr_y_actual
  
  
  MAE <- mean( abs(y_predicted- y_actual)  )
  MAPE <- mean( abs( (y_predicted- y_actual)/y_actual )   ) *100
  # res <- data.frame(rsquare,rsquare1,rsquare2,rsquare3,meansquare,RMSE,NRMSD,COR,MAPE  )
  res <- data.frame(rsquare,meansquare, COR,pvalue,RMSE,NRMSD, MAE, MAPE ) %>% round( digits = digits )
  
  return(res)
}

### funend --------

ui<- tagList( 
  navbarPage(
      "Sim rBTR",
    tabPanel( # tab1  ####
      "Teand Age",
      sidebarPanel(width =3,
                   "title1",
                   fileInput("file1", "Updata csv file of TreeRing series",
                             accept=c("text/csv", "text/comma-separated-values,text/plain")),
                   tags$hr(),
                   checkboxInput("header", "Header", TRUE),
                   
                   textInput("xaxis", "colum of X-axis", "age"),
                   textInput("yaxis", "colum of Y-axis", "MRW"),
                   
                   tags$hr(),
                   
                   helpText( "Formula: y  = α exp( β * x ) + c"),
                   
                   ### input param 1 & 2 :alpha_age, beta_age
                   numericInput("AgeAlpha", "Alpha", 1, min = -Inf , max = Inf ),
                   numericInput("AgeBeta", "Beta", 1, min = -Inf , max = Inf ),
                   numericInput("AgeC", "C", 0, min = -Inf , max = Inf ),
                   
                   ### input Knots of gam , use slider
                   sliderInput("knots",
                               "K of GAM:",
                               min = 1,
                               max = 12,
                               step = 1,
                               value = 3 ), ## end of sliderinput
                   
                   tags$hr(),
                   # Button
                   downloadButton("downloadData", "Download Trend_age")
                   
      ), ##end of sidebarPanel
      
      
      # 输出部分：显示图表
      mainPanel(
        tableOutput("contents"),
        plotOutput("plot"),
        tableOutput("modTest")
        
      ) ## mainPanel end --
      
      
    ), ## tablanel 1 end -----
    
    tabPanel( ## tab 2 ####
      "理论纤维细胞生长过程图/敏感性测试",
      column( ## col 1 细胞初始参数
        3, 
        wellPanel( ## 灰色底 panel 
          h4("输入初始细胞参数"),
          numericInput("LA", "Lunen Area", 5, min = -Inf , max = Inf ), ## 
          numericInput("CWT", "Cell wall thick", 5, min = -Inf , max = Inf ), ## 
          numericInput("CRD", "CRD", 5, min = -Inf , max = Inf ), ## 
          numericInput("CTD", "CTD", 5, min = -Inf , max = Inf ), ## 
          tags$hr(),
          h4( "细胞初始生长速率" ),
          numericInput("cva", "V cell enlargement ", 5, min = -Inf , max = Inf ), ## 
          numericInput("wva", "V cell wall deposition", 5, min = -Inf , max = Inf ), ## 
          numericInput("lva", "V cell lignin", 5, min = -Inf , max = Inf ) ## 
          # checkboxInput("vessel", "Vessel", value = FALSE)
        ) ## wellPanel end 
      ), ## col 1 end ---
      
      column( ## col 2 细胞初始参数
        3, 
        wellPanel( ## 灰色底 panel 
          h4("细胞生长参数"),
          numericInput("CAmax", "MAx cell Area", 400, min = 0 , max = Inf, step = 20  ), ## 
          numericInput("ml", "p1 of lignified", 400, min = 0 , max = Inf , step = 10), ## 
          numericInput("sl", "p2 of lignified", 8, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("mw", "p1 of deposition", 400, min = 0 , max = Inf , step = 10), ## 
          numericInput("sw", "p2 of deposition", 3, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("WAmax", "Max cell wall area", 200, min = 0 , max = Inf , step = 10), ## 
          numericInput("WTmax", "Max cell wall thick", 5, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("WTmin", "Min cell wall thick", 1, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("WTa", "Threshold thickness of cell wall", 3, min = 0 , max = Inf , step = 0.1) ## 

        ) ## wellPanel end 
      ), ## col 2 end ---
      column( ## col 3 拟合生长用的生长速率
        3, 
        wellPanel( ## 灰色底 panel 
          h4("指定生长率"),
          numericInput("pGR", "Grwoth rate", 1, min = 0 , max = 1, step = 0.1  ), ## 
          numericInput("rEGRWGR", "Ratio of egR to wgR", 400, min = 0 , max = Inf , step = 10)

          
        ) ## wellPanel end 
      ), ## col 3 end ---
      
      
      
    ), ## tabPanel 2 end -------
    
    tabPanel( ## tab 3 
      "理论导管细胞生长过程图/敏感性测试",
      column( ## col 1 细胞初始参数
        3, 
        wellPanel( ## 灰色底 panel 
          h4("输入初始细胞参数"),
          numericInput("LA", "Lunen Area", 5, min = -Inf , max = Inf ), ## 
          numericInput("CWT", "Cell wall thick", 5, min = -Inf , max = Inf ), ## 
          numericInput("CRD", "CRD", 5, min = -Inf , max = Inf ), ## 
          numericInput("CTD", "CTD", 5, min = -Inf , max = Inf ), ## 
          tags$hr(),
          h4( "细胞初始生长速率" ),
          numericInput("cva", "V cell enlargement ", 5, min = -Inf , max = Inf ), ## 
          numericInput("wva", "V cell wall deposition", 5, min = -Inf , max = Inf ), ## 
          numericInput("lva", "V cell lignin", 5, min = -Inf , max = Inf ) ## 
          # checkboxInput("vessel", "Vessel", value = FALSE)
        ) ## wellPanel end 
      ), ## col 1 end ---
      
      column( ## col 2 细胞初始参数
        3, 
        wellPanel( ## 灰色底 panel 
          h4("细胞生长参数"),
          numericInput("CAmax", "MAx cell Area", 400, min = 0 , max = Inf, step = 20  ), ## 
          numericInput("ml", "p1 of lignified", 400, min = 0 , max = Inf , step = 10), ## 
          numericInput("sl", "p2 of lignified", 8, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("mw", "p1 of deposition", 400, min = 0 , max = Inf , step = 10), ## 
          numericInput("sw", "p2 of deposition", 3, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("WAmax", "Max cell wall area", 200, min = 0 , max = Inf , step = 10), ## 
          numericInput("WTmax", "Max cell wall thick", 5, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("WTmin", "Min cell wall thick", 1, min = 0 , max = Inf , step = 0.1), ## 
          numericInput("WTa", "Threshold thickness of cell wall", 3, min = 0 , max = Inf , step = 0.1) ## 
          
        ) ## wellPanel end 
      ), ## col 2 end ---
      column( ## col 3 拟合生长用的生长速率
        3, 
        wellPanel( ## 灰色底 panel 
          h4("指定生长率"),
          numericInput("pGR", "Grwoth rate", 1, min = 0 , max = 1, step = 0.1  ), ## 
          numericInput("rEGRWGR", "Ratio of egR to wgR", 400, min = 0 , max = Inf , step = 10)
          
          
        ) ## wellPanel end 
      ), ## col 3 end ---
    ), ## tabPanel 3 end -------
    
  ) ## navbarPage end -----
  
)## taglist end -----


server <- function(input, output) {
  
  #### part of Trend age ####  
  values <- reactiveValues(data1 = NULL,data2 = NULL)
  # 当文件被上传时，读取CSV数据
  dataInput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header)
  })
  
  output$contents <-renderTable({  head(dataInput())  }) 
  
  
  
  # 根据读取的数据生成图表
  output$plot <- renderPlot({

    ## data and calculate
    dt <- dt3 <- dataInput() |>
      dplyr::select(input$xaxis,input$yaxis ) |>
      na.omit()
    
    colnames(dt) <- colnames(dt3) <- c("x", 'y')
    
    dt3 <- dt3|> mutate( predGAM = 0 ,predEXP = 0,
                         ManuEXP = input$AgeAlpha * exp(input$AgeBeta * x )+input$AgeC  )
    
    dt2 <- data.frame( x = seq( min(dt[,1]), max(dt[,1]), (max(dt[,1]) -min(dt[,1]) )/100  ) ) |>
      mutate( predGAM = 0 ,predEXP = 0,nGAM = 0,nEXP = 0,
              ManuEXP = input$AgeAlpha * exp(input$AgeBeta * x )+input$AgeC , ManuEXP2 = nor( ManuEXP ) )
    
    ## 拟合EXPmodel 
    ExpRes <- tryCatch(
      {
        modelEXP(dt,"x", 'y' ,input$AgeAlpha,input$AgeBeta,input$AgeC )
        "Y"
      },
      error = function(e) {
        "N"
      },
      waring = function(w){
        "N"
      }   )## end trycatch
    
    if ( ExpRes == "Y" ) {
      modEXP <- modelEXP(dt,"x", 'y' ,input$AgeAlpha,input$AgeBeta,input$AgeC )
      dt2$predEXP <- predict( modEXP , dt2 )  
      dt3$predEXP <- predict( modEXP , dt3 ) 
      dt2$nEXP <- predict( modEXP , dt2 ) |> nor()  
      alphat <- coef(modEXP)[1] |>round(5)
      betat <- coef(modEXP)[2] |>round(5)
      ct <- coef(modEXP)[3] |>round(5)
    } else {
      alphat <- paste("error:", round(0.8*max(dt$y) ,5)  )
      betat <-  paste("error:", round((max(dt$y)-min(dt$y))/nrow(dt),5  )   )
      ct <-  paste("error:", round(min(dt$y)*0.5,5  )   )
    }
    
    modGAM <- mgcv::gam( y ~ s( x , k = input$knots) ,data = dt  )
    
    # dt$predEXP <- predict(modEXP , dt ) 
    # dt$predGAM <- predict( modGAM , dt  )
    
    dt2$predGAM <- predict( modGAM , dt2 )
    dt3$predGAM <- predict( modGAM , dt3 )
    
    dt2$nGAM <- predict( modGAM , dt2 ) |> nor()
    
    dtRes <- rbind( mod.test(dt3$y, dt3$predGAM) , 
                    mod.test(dt3$y , dt3$predEXP),
                    mod.test(dt3$y , dt3$ManuEXP) )
    
    rownames(dtRes ) <- c('GAM','EXP',"mEXP")
    values$data1 <- dt2
    values$data2 <- dtRes %>% tibble::rownames_to_column("mod")
    
    ggpubr::ggarrange(
      ## f1 ：
      ggplot(dt2   ) + ## x = "age1", y = 'MRW' 
        theme_light()+
        labs(title = "sim",
             subtitle = paste( 'k = ', input$knots, '| α = ', alphat, " β = " ,  betat, " c = ",ct, ExpRes ),
             x = input$xaxis, y = input$yaxis
        )+
        scale_y_continuous( limits = c(0 , 1.1*max(dt$y))    )+
        geom_point(aes(x= x, y = y ,color = "Obs" ), data = dt)+
        geom_line(aes(x= x, y = predGAM , color = "GAM"))+
        geom_line(aes(x= x, y = predEXP , color = "EXP"))+
        geom_line(aes(x= x, y = ManuEXP , color = "ManuEXP")),
      ## f2 :
      ggplot( dt2  )+
        theme_light()+
        labs(title = "T_age",x = input$xaxis, y = input$yaxis)+
        geom_line(aes(x= x, y = nGAM , color = "GAM"))+
        geom_line(aes(x= x, y = nEXP , color = "EXP"))+
        geom_line(aes(x= x, y = ManuEXP2 , color = "ManuEXP")),
      # ggtexttable(dtRes) ,nrow = 2,
      ncol = 2, align = 'hv',common.legend = T ,legend = 'top'
    ) ## ggarrange end 
    
  })
  
  ### modtest
  output$modTest <-renderTable({  values$data2  }) 
  
  ## downLoad
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "TrendAge.csv",
    content = function(file) {
      write.csv(values$data1 , file, row.names = FALSE)
    }
  )
  #### Trend age end ---------
  
  
  #### part of  理论细胞生长过程图 ####
  
  
  ####  理论细胞生长过程图 end --------
  
}

shinyApp(ui ,server)
