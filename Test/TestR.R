#### import package ####
library(shiny)
library(readr)
library(ggplot2)
library(dplyr )
library(mgcv)
library(nls.multstart)
library(ggpubr)
library(knitr)

#### import Rp end ----------- 

## my app #### 
#### global fun ####
### 拟合最优 NEG
modelEXP <- function( dt,colx ,coly , SSalpha,SSbeta,SSC  ){
  # f1 <-  as.formula( paste( coly ,'~ a * exp(b *', colx,' )' ) )
  # model2 <- nls( y ~ a*exp( b*x  ) + c   , data = dt ,
  #                start = list( a = SSalpha , b = SSbeta , c = SSC  ))
  
  model2 <- nls_multstart( y ~ a*exp( b*x  ) + c  , 
                           data = dt ,  
                           iter = 500,
                           start_lower = c( a= SSalpha*0.5, b= SSbeta *-1 ,c = SSC *-1  ),
                           start_upper = c( a= SSalpha*1.5, b= SSbeta  ,c = SSC    ) ,
                           supp_errors = 'Y' )
  
  return( model2 )
}

## 归一化
nor <- function( x ){
  res <- x /max(x)
}

## 计算拟合结果
simTrendAge <- function( dt, colx,coly, SSalpha, SSbeta, SSC, Ks  ){
  
  dt <- dt3 <- dt |>
    dplyr::select(colx,coly ) |>
    na.omit()
  
  colnames(dt) <- colnames(dt3) <- c("x", 'y')
  
  dt3 <- dt3|> mutate( predGAM = 0 ,predEXP = 0,
                       ManuEXP = SSalpha * exp(SSbeta * x )+SSC  )
  
  dt2 <- data.frame( x = seq( min(dt[,1]), max(dt[,1]), (max(dt[,1]) -min(dt[,1]) )/100  ) ) |>
    mutate( predGAM = 0 ,predEXP = 0,nGAM = 0,nEXP = 0,
            ManuEXP = SSalpha * exp(SSbeta * x )+SSC , ManuEXP2 = nor( ManuEXP ) )
  
  ## 拟合EXPmodel 
  tryCatch(
    {
      modEXP <<- modelEXP(dt,"x", 'y' ,SSalpha, SSbeta ,SSC )
      ExpRes <- "Y"
    },
    error = function(e) {
      ExpRes <<- "N"
    },
    waring = function(w){
      ExpRes <<- "N"
    }   )## end trycatch
  
  # modEXP <- modelEXP(dt,"x", 'y' ,SSalpha, SSbeta ,SSC )
  
  if ( ExpRes == "Y" ) {
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
  
  dt4 <-data.frame(alphat, betat,ct )
  
  dtRes <- rbind( mod.test(dt3$y, dt3$predGAM) , 
                  mod.test(dt3$y , dt3$predEXP),
                  mod.test(dt3$y , dt3$ManuEXP) )
  
  rownames(dtRes ) <- c('GAM','EXP',"mEXP")
  
  ResDt <-  list( dt2,dt3,dt4, dtRes  )
  return( ResDt )

}



## 拟合效果评价 
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

#### global fun end ----------



## 建立UI
ui <- fluidPage(
  # 应用程序标题
  titlePanel("CSV 数据读取与绘图"),
  
  # 输入部分：上传CSV文件
  sidebarLayout(
    sidebarPanel(
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
      tableOutput("Res"),
      tableOutput("modtest")
    )
  )
)

server <- function(input, output, session) {
  ## values 
  values <- reactiveValues(data1 = NULL, data2 = NULL,data3 = NULL,dtRes = NULL)
  
  ## 当文件被上传时，读取CSV数据
  dataInput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = input$header)
  })
  ## 输出head
  output$contents <-renderTable({  head(dataInput())  })
  
  ## 计算data1 ，data2
  # observe({
  #   
  #   dts <- simTrendAge( dataInput(), input$xaxis,input$yaxis, 
  #                       input$AgeAlpha, input$AgeBeta, input$AgeC, input$knots    )
  #   
  #   values$data1 <- dts[[1]]
  #   values$data2 <- dts[[2]]
  #   values$data3 <- dts[[3]]
  #   values$dtRes <- dts[[4]]
  #   
  # })
  
  output$plot <- renderPlot({
    
    coefs <- values$data3
    
    ggpubr::ggarrange(
      ## f1 ：
      ggplot( values$data1   ) + ## x = "age1", y = 'MRW' 
        theme_light()+
        labs(title = "sim",
             subtitle = paste( 'k = ', input$knots, '| α = ', coefs[1,1], " β = " ,  coefs[1,2], " c = ",coefs[1,3]  ),
             x = input$xaxis, y = input$yaxis
        )+
        scale_y_continuous( limits = c(0 , 1.1*max(dt$y))    )+
        geom_point(aes(x= x, y = y ,color = "Obs" ), data = dt)+
        geom_line(aes(x= x, y = predGAM , color = "GAM"))+
        geom_line(aes(x= x, y = predEXP , color = "EXP"))+
        geom_line(aes(x= x, y = ManuEXP , color = "ManuEXP")),
      ## f2 :
      ggplot( values$data1  )+
        theme_light()+
        labs(title = "T_age",x = input$xaxis, y = input$yaxis)+
        geom_line(aes(x= x, y = nGAM , color = "GAM"))+
        geom_line(aes(x= x, y = nEXP , color = "EXP"))+
        geom_line(aes(x= x, y = ManuEXP2 , color = "ManuEXP")),
      
      ncol = 2,nrow = 1, align = 'hv',common.legend = T ,legend = 'top'
    ) ## ggarrange end 
    
  })
  
  ## output mod.test
  output$modtest <-renderTable({  
    values$dtRes 
    })

}

shinyApp(ui = ui, server = server)