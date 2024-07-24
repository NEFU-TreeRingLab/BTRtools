server <- function(input, output) {
  
  #### New way to coding ####
  
  ###  part 1 合并输入数据为表格 ####
  ## Trend age 
  # ParamAge <- reactive(  data.frame( input$xaxis, input$MRW, input$MaxLA ) )
  
  
  cc <- renderTable(data.frame( input$xaxis, input$MRW, input$MaxLA ))
  # NameXY <- c( input$xaxis, input$MRW, input$MaxLA )
  
  # paramAge <- data.frame(  input )
  
  # output$tdata <- paramAge
  
  
  
  #### New way end ----------
  
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
